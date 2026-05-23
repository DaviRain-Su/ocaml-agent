(* The agent loop: holds the conversation, calls the model, and runs whatever
   tools the model requests until it produces a final text answer.

   Provider-agnostic: it deals only in Llm's normalized content/turn types.
   Adds project-context injection, a run_bash approval gate, optional JSONL
   session persistence, and streamed text output. *)

let base_prompt =
  "You are a capable coding agent operating in a user's terminal, in their \
   current working directory. You can read, write, and edit files, list \
   directories, and run bash commands to accomplish software engineering tasks.\n\n\
   Guidelines:\n\
   - Use the tools to inspect the project before making changes; do not guess at \
   file contents.\n\
   - Make focused, correct edits. Prefer edit_file for small changes and \
   write_file for new or fully-rewritten files.\n\
   - Run builds/tests with run_bash to verify your work when relevant.\n\
   - When the task is done, give a short summary of what you changed. Be concise."

(* Read context files from the cwd and fold them, plus cwd/date, into the prompt. *)
let build_system_prompt cfg =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf base_prompt;
  let provider = match cfg.Llm.provider with Llm.Anthropic -> "Anthropic" | Llm.Openai -> "OpenAI-compatible" in
  Buffer.add_string buf
    (Printf.sprintf
       "\n\nYour identity: you are served by the %s API (%s) running the model \"%s\". If \
        asked which model or provider you are, answer with exactly these values and do not \
        guess or claim to be a different model."
       provider cfg.Llm.base_url cfg.Llm.model);
  let context_files = [ "AGENTS.md"; "CLAUDE.md" ] in
  let present =
    List.filter_map
      (fun name ->
        if Sys.file_exists name && not (Sys.is_directory name) then
          try
            let ic = open_in_bin name in
            let content =
              Fun.protect
                ~finally:(fun () -> close_in_noerr ic)
                (fun () -> really_input_string ic (in_channel_length ic))
            in
            Some (name, content)
          with _ -> None
        else None)
      context_files
  in
  if present <> [] then begin
    Buffer.add_string buf "\n\n<project_context>\n";
    List.iter
      (fun (name, content) ->
        Buffer.add_string buf (Printf.sprintf "<file path=\"%s\">\n%s\n</file>\n" name content))
      present;
    Buffer.add_string buf "</project_context>\n"
  end;
  let tm = Unix.localtime (Unix.time ()) in
  Buffer.add_string buf
    (Printf.sprintf "\n\nCurrent date: %04d-%02d-%02d\nCurrent working directory: %s"
       (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday (Sys.getcwd ()));
  Buffer.contents buf

let truthy s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "y" | "all" -> true
  | _ -> false

let env_int name default =
  match Sys.getenv_opt name with Some s -> ( try int_of_string s with _ -> default) | None -> default

let env_float name default =
  match Sys.getenv_opt name with Some s -> ( try float_of_string s with _ -> default) | None -> default

type t =
  { mutable cfg : Llm.config;
    mutable system : string;
    mutable turns : Llm.turn list; (* chronological *)
    session : Session.t option;
    mutable auto_approve : bool;
    tools_enabled : bool;
    context_window : int;
    compact_threshold : float; (* fraction of the window that triggers compaction *)
    auto_compact : bool;
    mutable last_input_tokens : int;
    mutable last_output_tokens : int }

let create ?session ?(initial_turns = []) ?(tools_enabled = true) cfg =
  let auto_approve =
    match Sys.getenv_opt "AGENT_AUTO_APPROVE" with Some v -> truthy v | None -> false
  in
  let auto_compact =
    match Sys.getenv_opt "AGENT_AUTO_COMPACT" with Some v -> truthy v | None -> true
  in
  { cfg;
    system = build_system_prompt cfg;
    turns = initial_turns;
    session;
    auto_approve;
    tools_enabled;
    context_window = env_int "AGENT_CONTEXT_WINDOW" 128000;
    compact_threshold = env_float "AGENT_COMPACT_THRESHOLD" 0.75;
    auto_compact;
    last_input_tokens = 0;
    last_output_tokens = 0 }

(* Swap the active model/provider; rebuilds the system prompt's identity line. *)
let set_config t cfg =
  t.cfg <- cfg;
  t.system <- build_system_prompt cfg

(* Clear the in-memory conversation (does not truncate the session file). *)
let reset t = t.turns <- []

let config t = t.cfg
let turn_count t = List.length t.turns

(* Change the reasoning level live. *)
let set_thinking t level = t.cfg <- { t.cfg with Llm.thinking = level }

(* Append a turn to history and persist it if a session is open. *)
let add t turn =
  t.turns <- t.turns @ [ turn ];
  Option.iter (fun s -> Session.append s turn) t.session

(* ANSI colors. *)
let dim s = "\027[2m" ^ s ^ "\027[0m"
let cyan s = "\027[36m" ^ s ^ "\027[0m"
let green s = "\027[32m" ^ s ^ "\027[0m"
let yellow s = "\027[33m" ^ s ^ "\027[0m"

let preview_input input =
  let s = Yojson.Safe.to_string input in
  if String.length s > 120 then String.sub s 0 117 ^ "..." else s

(* Only run_bash is gated. Returns true if the call may proceed. *)
let approve t name input =
  if name <> "run_bash" then true
  else if t.auto_approve then true
  else begin
    let command =
      match input with `Assoc l -> ( match List.assoc_opt "command" l with Some (`String c) -> c | _ -> "") | _ -> ""
    in
    if not (Unix.isatty Unix.stdin) then begin
      Printf.printf "%s run_bash denied (no TTY to approve): %s\n%!" (yellow "✗") command;
      false
    end
    else begin
      Printf.printf "%s run %s\n  %s\n%s "
        (yellow "⚠") (green "bash") command (dim "approve? [y]es / [N]o / [a]lways:");
      flush stdout;
      match In_channel.input_line stdin with
      | None -> false
      | Some ans -> (
        match String.lowercase_ascii (String.trim ans) with
        | "a" | "always" ->
          t.auto_approve <- true;
          true
        | "y" | "yes" -> true
        | _ -> false)
    end
  end

let run_tool t id name input : Llm.content =
  Printf.printf "%s %s %s\n%!" (cyan "⚙") (green name) (dim (preview_input input));
  let result =
    if not (approve t name input) then "Error: command not approved by user"
    else
      match Tools.find name with
      | Some tool -> ( try tool.execute input with e -> "Error: " ^ Printexc.to_string e)
      | None -> Printf.sprintf "Error: unknown tool %s" name
  in
  Llm.Tool_result { id; content = result }

(* --- context accounting + compaction --- *)

let content_chars = function
  | Llm.Text s -> String.length s
  | Llm.Thinking { text; _ } -> String.length text
  | Llm.Tool_use { input; _ } -> String.length (Yojson.Safe.to_string input)
  | Llm.Tool_result { content; _ } -> String.length content

let estimate_tokens t =
  let chars =
    List.fold_left
      (fun a turn -> a + List.fold_left (fun b c -> b + content_chars c) 0 turn.Llm.content)
      0 t.turns
  in
  chars / 4

(* Best-known size of the current context: real input tokens if the API gave us
   any, else a chars/4 estimate. *)
let context_used t = if t.last_input_tokens > 0 then t.last_input_tokens else estimate_tokens t

let usage_info t =
  let used = context_used t in
  (used, t.context_window, float_of_int used /. float_of_int t.context_window)

let keep_recent = 6

let has_tool_result turn =
  List.exists (function Llm.Tool_result _ -> true | _ -> false) turn.Llm.content

let turn_to_text (turn : Llm.turn) =
  let role = match turn.Llm.role with User -> "User" | Assistant -> "Assistant" in
  let part = function
    | Llm.Text s -> s
    | Llm.Thinking _ -> "" (* thinking is not carried into summaries *)
    | Llm.Tool_use { name; input; _ } -> Printf.sprintf "[tool %s %s]" name (Yojson.Safe.to_string input)
    | Llm.Tool_result { content; _ } -> "[result] " ^ content
  in
  role ^ ": " ^ String.concat "\n" (List.map part turn.Llm.content)

(* Summarize all but the most recent turns into a single synthetic user turn. *)
let compact t =
  let n = List.length t.turns in
  if n <= keep_recent + 1 then "Nothing to compact."
  else begin
    let rec split i acc = function
      | x :: xs when i > 0 -> split (i - 1) (x :: acc) xs
      | rest -> (List.rev acc, rest)
    in
    let older, recent = split (n - keep_recent) [] t.turns in
    (* Don't strand a tool_result whose tool_use is being summarized away. *)
    let rec fix older recent =
      match recent with r :: rs when has_tool_result r -> fix (older @ [ r ]) rs | _ -> (older, recent)
    in
    let older, recent = fix older recent in
    let transcript = String.concat "\n\n" (List.map turn_to_text older) in
    let sys =
      "You are a summarizer for a coding agent. Produce a concise but complete summary of \
       the conversation so far, preserving key decisions, file changes made, important \
       facts learned, and any open tasks. Output only the summary."
    in
    let prompt = [ { Llm.role = User; content = [ Llm.Text ("Summarize this conversation:\n\n" ^ transcript) ] } ] in
    let blocks, _ = Llm.complete t.cfg ~system:sys ~tools_enabled:false prompt in
    let summary = String.concat "\n" (List.filter_map (function Llm.Text s -> Some s | _ -> None) blocks) in
    let summary_turn = { Llm.role = User; content = [ Llm.Text ("[Earlier conversation summary]\n" ^ summary) ] } in
    t.turns <- summary_turn :: recent;
    t.last_input_tokens <- 0;
    (* force re-estimate next turn *)
    Printf.sprintf "Compacted %d older turns into a summary." (List.length older)
  end

let should_compact t =
  t.auto_compact
  && float_of_int (context_used t) > t.compact_threshold *. float_of_int t.context_window
  && List.length t.turns > keep_recent + 1

let rec step t : string =
  let streamed = ref false in
  let on_text s =
    streamed := true;
    print_string s;
    flush stdout
  in
  let blocks, usage = Llm.complete t.cfg ~system:t.system ~on_text ~tools_enabled:t.tools_enabled t.turns in
  if usage.Llm.input_tokens > 0 then t.last_input_tokens <- usage.Llm.input_tokens;
  if usage.Llm.output_tokens > 0 then t.last_output_tokens <- usage.Llm.output_tokens;
  if !streamed then print_newline ();
  List.iter
    (function Llm.Thinking { text; _ } when text <> "" -> Printf.printf "%s\n%!" (dim ("\xf0\x9f\x92\xad " ^ text)) | _ -> ())
    blocks;
  add t { Llm.role = Assistant; content = blocks };
  let texts = List.filter_map (function Llm.Text s -> Some s | _ -> None) blocks in
  let tool_results =
    List.filter_map
      (function
        | Llm.Tool_use { id; name; input } -> Some (run_tool t id name input)
        | _ -> None)
      blocks
  in
  if tool_results <> [] then begin
    add t { Llm.role = User; content = tool_results };
    step t
  end
  else String.concat "\n" texts

let send t (user_input : string) : string =
  if should_compact t then begin
    Printf.printf "%s\n%!" (dim "Auto-compacting context...");
    ignore (compact t)
  end;
  add t { Llm.role = User; content = [ Llm.Text user_input ] };
  step t
