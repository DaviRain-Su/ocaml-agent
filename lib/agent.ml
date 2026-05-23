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

type t =
  { cfg : Llm.config;
    system : string;
    mutable turns : Llm.turn list; (* chronological *)
    session : Session.t option;
    mutable auto_approve : bool }

let create ?session ?(initial_turns = []) cfg =
  let auto_approve =
    match Sys.getenv_opt "AGENT_AUTO_APPROVE" with Some v -> truthy v | None -> false
  in
  { cfg; system = build_system_prompt cfg; turns = initial_turns; session; auto_approve }

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

let rec step t : string =
  let streamed = ref false in
  let on_text s =
    streamed := true;
    print_string s;
    flush stdout
  in
  let blocks = Llm.complete t.cfg ~system:t.system ~on_text t.turns in
  if !streamed then print_newline ();
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
  add t { Llm.role = User; content = [ Llm.Text user_input ] };
  step t
