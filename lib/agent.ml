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
   - Make focused, correct edits. Prefer edit for small changes and \
   write for new or fully-rewritten files.\n\
   - Run builds/tests with bash to verify your work when relevant.\n\
   - When the task is done, give a short summary of what you changed. Be concise."

let read_text_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let resolve_prompt_input input =
  if Sys.file_exists input && not (Sys.is_directory input) then
    try read_text_file input with _ -> input
  else input

let join_prompt_inputs inputs =
  inputs |> List.map resolve_prompt_input |> String.concat "\n\n"

let truthy s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "y" | "all" -> true
  | _ -> false

let env_truthy name =
  match Sys.getenv_opt name with Some s -> truthy s | None -> false

(* Read context files from the cwd and fold them, plus cwd/date, into the prompt. *)
let build_system_prompt cfg =
  let buf = Buffer.create 1024 in
  let base =
    match Sys.getenv_opt "AGENT_SYSTEM_PROMPT" with
    | Some s when String.trim s <> "" -> resolve_prompt_input s
    | _ -> base_prompt
  in
  Buffer.add_string buf base;
  let provider = match cfg.Llm.provider with Llm.Anthropic -> "Anthropic" | Llm.Openai -> "OpenAI-compatible" in
  Buffer.add_string buf
    (Printf.sprintf
       "\n\nYour identity: you are served by the %s API (%s) running the model \"%s\". If \
        asked which model or provider you are, answer with exactly these values and do not \
        guess or claim to be a different model."
       provider cfg.Llm.base_url cfg.Llm.model);
  let context_files = if env_truthy "AGENT_NO_CONTEXT_FILES" then [] else Config_paths.context_files () in
  let present =
    List.filter_map
      (fun path ->
        if Sys.file_exists path && not (Sys.is_directory path) then
          try
            let ic = open_in_bin path in
            let content =
              Fun.protect
                ~finally:(fun () -> close_in_noerr ic)
                (fun () -> really_input_string ic (in_channel_length ic))
            in
            Some (path, content)
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
  Buffer.add_string buf (Skills.format (Skills.discover ()));
  let tm = Unix.localtime (Unix.time ()) in
  Buffer.add_string buf
    (Printf.sprintf "\n\nCurrent date: %04d-%02d-%02d\nCurrent working directory: %s"
       (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday (Sys.getcwd ()));
  (match Sys.getenv_opt "AGENT_APPEND_SYSTEM_PROMPT" with
   | Some s when String.trim s <> "" -> Buffer.add_string buf ("\n\n" ^ resolve_prompt_input s)
   | _ -> ());
  Buffer.contents buf

let env_int name default =
  match Sys.getenv_opt name with Some s -> ( try int_of_string s with _ -> default) | None -> default

let env_float name default =
  match Sys.getenv_opt name with Some s -> ( try float_of_string s with _ -> default) | None -> default

(* ANSI colors (used by the plain stdout frontend). *)
let dim s = "\027[2m" ^ s ^ "\027[0m"
let cyan s = "\027[36m" ^ s ^ "\027[0m"
let green s = "\027[32m" ^ s ^ "\027[0m"
let yellow s = "\027[33m" ^ s ^ "\027[0m"

let preview_input input =
  let s = Yojson.Safe.to_string input in
  if String.length s > 120 then String.sub s 0 117 ^ "..." else s

type approval = Approve_once | Approve_always | Deny

(* The agent emits all user-visible output through a frontend, so the plain REPL
   and a full TUI can render the same events differently. *)
type frontend =
  { text_delta : string -> unit; (* streamed assistant text chunk *)
    text_done : unit -> unit; (* end of a streamed assistant message *)
    thinking : string -> unit; (* a completed thinking block *)
    tool_call : string -> string -> unit; (* tool name, input preview *)
    tool_result : string -> unit; (* tool result text *)
    notice : string -> unit; (* status line, e.g. compaction *)
    message_end : Llm.turn -> Llm.usage -> Llm.config -> string -> unit;
    tool_result_end : Llm.turn -> unit;
    confirm_bash : string -> approval (* approve a command-capable tool *) }

let stdout_frontend () : frontend =
  let r = ref None in
  let cur () = match !r with Some x -> x | None -> let x = Render.create () in r := Some x; x in
  { text_delta = (fun s -> Render.feed (cur ()) s);
    text_done = (fun () -> (match !r with Some x -> Render.finish x | None -> ()); r := None);
    thinking = (fun s -> if String.trim s <> "" then Printf.printf "%s\n%!" (dim ("\xf0\x9f\x92\xad " ^ s)));
    tool_call = (fun name prev -> Printf.printf "%s %s %s\n%!" (cyan "\xe2\x9a\x99") (green name) (dim prev));
    tool_result = (fun res -> if String.trim res <> "" then Printf.printf "%s\n%!" (Render.tool_result res));
    notice = (fun s -> Printf.printf "%s\n%!" (dim s));
    message_end = (fun _ _ _ _ -> ());
    tool_result_end = (fun _ -> ());
    confirm_bash =
      (fun command ->
        if not (Unix.isatty Unix.stdin) then begin
          Printf.printf "%s command denied (no TTY to approve): %s\n%!" (yellow "\xe2\x9c\x97") command;
          Deny
        end
        else begin
          Printf.printf "%s run %s\n  %s\n%s " (yellow "\xe2\x9a\xa0") (green "bash") command
            (dim "approve? [y]es / [N]o / [a]lways:");
          flush stdout;
          match In_channel.input_line stdin with
          | None -> Deny
          | Some ans -> (
            match String.lowercase_ascii (String.trim ans) with
            | "a" | "always" -> Approve_always
            | "y" | "yes" -> Approve_once
            | _ -> Deny)
        end) }

(* A frontend that produces no output (used by --mode json). *)
let null_frontend () : frontend =
  { text_delta = (fun _ -> ());
    text_done = (fun () -> ());
    thinking = (fun _ -> ());
    tool_call = (fun _ _ -> ());
    tool_result = (fun _ -> ());
    notice = (fun _ -> ());
    message_end = (fun _ _ _ _ -> ());
    tool_result_end = (fun _ -> ());
    confirm_bash = (fun _ -> Deny) }

type t =
  { mutable cfg : Llm.config;
    mutable system : string;
    mutable turns : Llm.turn list; (* chronological *)
    mutable session : Session.t option;
    mutable auto_approve : bool;
    tools_enabled : bool;
    allowed_tools : string list option;
    context_window : int;
    compact_threshold : float; (* fraction of the window that triggers compaction *)
    mutable auto_compact : bool;
    depth : int; (* sub-agent nesting depth; 0 for the top-level agent *)
    max_tool_rounds : int;
    mutable steering_mode : string;
    mutable follow_up_mode : string;
    mutable auto_retry : bool;
    mutable fe : frontend;
    mutable last_input_tokens : int;
    mutable last_output_tokens : int }

let max_depth = 2

let create ?session ?(initial_turns = []) ?(tools_enabled = true) ?allowed_tools ?(depth = 0) ?frontend cfg =
  (* Default to auto-approving tools (like pi). Set AGENT_AUTO_APPROVE=0 to be
     prompted before run_bash and subprocess extension tools. *)
  let auto_approve =
    match Sys.getenv_opt "AGENT_AUTO_APPROVE" with Some v -> truthy v | None -> true
  in
  let auto_compact =
    match Sys.getenv_opt "AGENT_AUTO_COMPACT" with
    | Some v -> truthy v
    | None -> Option.value (Config_paths.settings_bool [ "compaction"; "enabled" ]) ~default:true
  in
  { cfg;
    system = build_system_prompt cfg;
    turns = initial_turns;
    session;
    auto_approve;
    tools_enabled;
    allowed_tools = Option.map Tools.canonical_names allowed_tools;
    context_window =
      (match Sys.getenv_opt "AGENT_CONTEXT_WINDOW" with
       | Some s -> ( try max 1 (int_of_string s) with _ -> 128000)
       | None -> Option.value (Models.context_window cfg.model) ~default:128000);
    compact_threshold = env_float "AGENT_COMPACT_THRESHOLD" 0.75;
    auto_compact;
    depth;
    max_tool_rounds = max 1 (env_int "AGENT_MAX_TOOL_ROUNDS" 20);
    steering_mode = "all";
    follow_up_mode = "all";
    auto_retry = true;
    fe = (match frontend with Some f -> f | None -> stdout_frontend ());
    last_input_tokens = 0;
    last_output_tokens = 0 }

let set_frontend t fe = t.fe <- fe

(* Swap the active model/provider; rebuilds the system prompt's identity line. *)
let set_config ?(source = "set") t cfg =
  let previous = t.cfg in
  t.cfg <- cfg;
  t.system <- build_system_prompt cfg;
  Extensions.clear_active_model ();
  Extensions.set_active_thinking cfg.Llm.thinking;
  Extensions.emit_thinking_level_select ~previous_level:previous.Llm.thinking cfg.Llm.thinking;
  Extensions.emit_model_select ~source ~previous_model:(Some previous) cfg

let reload_system_prompt t =
  t.system <- build_system_prompt t.cfg

(* Clear the active conversation and persist that empty state when a session is open. *)
let reset t =
  t.turns <- [];
  t.last_input_tokens <- 0;
  t.last_output_tokens <- 0;
  Option.iter (fun s -> Session.save_all s []) t.session

let config t = t.cfg
let system_prompt t = t.system
let turn_count t = List.length t.turns
let turns t = t.turns
let context_turns t =
  match t.session with
  | None -> t.turns
  | Some session -> (
    match Session.load_context_turns session.Session.path with
    | [] when t.turns <> [] -> t.turns
    | context -> context)
let session t = t.session
let session_name t = Option.map (fun s -> s.Session.name) t.session

let set_session_name t name =
  match t.session with
  | Some s ->
    Session.set_name s name t.turns;
    true
  | None -> false

let append_extension_session_entry t entry =
  let member name = Yojson.Safe.Util.member name entry in
  match t.session with
  | None -> false
  | Some s -> (
    match member "type" with
    | `String "custom" -> (
      match member "customType" with
      | `String custom_type when String.trim custom_type <> "" ->
        (match member "id" with
         | `String id when String.trim id <> "" -> Session.append_entry s entry
         | _ -> Session.append_custom_entry s custom_type (member "data"));
        true
      | _ -> false)
    | `String "label" -> (
      match member "targetId" with
      | `String target_id when String.trim target_id <> "" ->
        (match member "id" with
         | `String id when String.trim id <> "" -> Session.append_entry s entry
         | _ ->
           let label =
             match member "label" with
             | `String value -> Some value
             | `Null -> None
             | _ -> None
           in
           Session.append_label_change s target_id label);
        true
      | _ -> false)
    | `String "leaf" ->
      let target_id =
        match member "targetId" with
        | `String id when String.trim id <> "" -> Some id
        | `Null -> None
        | _ -> None
      in
      (match member "id" with
       | `String id when String.trim id <> "" -> Session.append_entry s entry
       | _ ->
         let parent_id =
           match member "parentId" with
           | `String id when String.trim id <> "" -> Some id
           | _ -> None
         in
         ignore (Session.append_leaf s ?parent_id target_id));
      true
    | `String "session_info" -> (
      match member "name" with
      | `String name ->
        Session.set_name s name t.turns;
        Session.append_entry s entry;
        true
      | _ -> false)
    | `String "custom_message" ->
      Session.append_entry s entry;
      true
    | `String "message" -> (
      match member "message" with
      | `Assoc fields when List.assoc_opt "role" fields <> None ->
        Session.append_entry s entry;
        true
      | _ -> false)
    | `String "thinking_level_change" -> (
      match member "thinkingLevel" with
      | `String level when String.trim level <> "" ->
        Session.append_entry s entry;
        true
      | _ -> false)
    | `String "model_change" -> (
      match member "provider", member "modelId" with
      | `String provider, `String model_id when String.trim provider <> "" && String.trim model_id <> "" ->
        Session.append_entry s entry;
        true
      | _ -> false)
    | `String "compaction" -> (
      match member "summary" with
      | `String summary when String.trim summary <> "" ->
        Session.append_entry s entry;
        true
      | _ -> false)
    | `String "branch_summary" -> (
      match member "id", member "summary" with
      | `String id, `String summary when String.trim id <> "" && String.trim summary <> "" ->
        Session.append_entry s entry;
        true
      | _ -> false)
    | _ -> false)

let append_branch_summary t ?parent_id ?details ?from_hook ~from_id summary =
  match t.session with
  | Some s -> Some (Session.append_branch_summary s ?parent_id ?details ?from_hook ~from_id summary)
  | None -> None

let append_leaf t ?parent_id target_id =
  match t.session with
  | Some s -> Some (Session.append_leaf s ?parent_id target_id)
  | None -> None

(* Rewrite the whole session file to reflect the current (e.g. compacted) turns. *)
let persist_full t = Option.iter (fun s -> Session.save_all s t.turns) t.session

let replace_turns t turns =
  t.turns <- turns;
  persist_full t

(* Switch to a different session and its history (used by /resume, /clone, /new). *)
let adopt_session t ?(turns = []) session =
  Option.iter Session.close t.session;
  t.session <- session;
  t.turns <- turns;
  t.last_input_tokens <- 0;
  t.last_output_tokens <- 0

(* Change the reasoning level live. *)
let set_thinking t level =
  let level = Model_spec.normalize_thinking level in
  let previous = t.cfg.Llm.thinking in
  if previous <> level then begin
    t.cfg <- { t.cfg with Llm.thinking = level };
    Extensions.set_active_thinking level;
    Extensions.emit_thinking_level_select ~previous_level:previous level
  end

let config_for_model_choice t (choice : Extensions.model_choice) =
  try
    let parsed =
      match choice.model with
      | Some spec -> Model_spec.parse ?provider:choice.provider ?thinking:choice.thinking (Some spec)
      | None -> { Model_spec.provider = choice.provider; model = None; thinking = choice.thinking }
    in
    let cfg =
      match (parsed.provider, parsed.model) with
      | Some provider, model -> Llm.config_for ?model provider
      | None, Some model -> { t.cfg with Llm.model = model }
      | None, None -> t.cfg
    in
    let cfg =
      match parsed.thinking with
      | Some thinking -> { cfg with Llm.thinking = Model_spec.normalize_thinking thinking }
      | None -> { cfg with Llm.thinking = t.cfg.Llm.thinking }
    in
    Ok cfg
  with Llm.Config_error msg -> Error msg

let apply_extension_model ?(source = "set") t choice =
  match config_for_model_choice t choice with
  | Ok cfg ->
    set_config ~source t cfg;
    Ok cfg
  | Error _ as err -> err

let auto_approve t = t.auto_approve
let set_auto_approve t b = t.auto_approve <- b
let auto_compact t = t.auto_compact
let set_auto_compact t b = t.auto_compact <- b
let steering_mode t = t.steering_mode
let set_steering_mode t mode = t.steering_mode <- mode
let follow_up_mode t = t.follow_up_mode
let set_follow_up_mode t mode = t.follow_up_mode <- mode
let auto_retry t = t.auto_retry
let set_auto_retry t b = t.auto_retry <- b

let effective_tool_names t = Extensions.effective_tool_names t.allowed_tools

let effective_config t =
  let cfg =
    match Extensions.active_model () with
    | Some choice -> ( match config_for_model_choice t choice with Ok cfg -> cfg | Error _ -> t.cfg)
    | None -> t.cfg
  in
  let thinking = Extensions.effective_thinking cfg.Llm.thinking in
  if thinking = cfg.Llm.thinking then cfg else { cfg with Llm.thinking = thinking }

let tool_allowed t name =
  match effective_tool_names t with
  | None -> true
  | Some names -> List.mem (Tools.canonical_name name) names

(* Append a turn to history and persist it if a session is open. *)
let add t turn =
  t.turns <- t.turns @ [ turn ];
  Option.iter (fun s -> Session.append s turn) t.session

let bash_context_text command code output =
  let body =
    if output = "" then "(no output)"
    else Printf.sprintf "```\n%s\n```" output
  in
  Printf.sprintf "Ran `%s`\n%s%s" command body
    (if code = 0 then "" else Printf.sprintf "\n\nCommand exited with code %d" code)

let run_user_bash ?(exclude_from_context = false) t command =
  let code, output =
    match Extensions.emit_user_bash ~command ~exclude_from_context with
    | Some result -> (result.Extensions.exit_code, result.Extensions.output)
    | None -> Tools.run_process ~use_shell_settings:true command
  in
  if not exclude_from_context then
    add t { Llm.role = User; content = [ Llm.Text (bash_context_text command code output) ] };
  Printf.sprintf "(exit %d)\n%s" code output

let approval_text name input =
  if name = "run_bash" then
    match input with `Assoc l -> ( match List.assoc_opt "command" l with Some (`String c) -> c | _ -> "") | _ -> ""
  else Printf.sprintf "%s %s" name (Yojson.Safe.to_string input)

(* Command-capable tools are gated. Returns true if the call may proceed. *)
let approve t name input =
  let requires_approval =
    match Tools.find (Tools.canonical_name name) with Some tool -> tool.Tools.requires_approval | None -> false
  in
  if not requires_approval then true
  else if t.auto_approve then true
  else begin
    match t.fe.confirm_bash (approval_text name input) with
    | Approve_always ->
      t.auto_approve <- true;
      true
    | Approve_once -> true
    | Deny -> false
  end

(* --- context accounting + compaction --- *)

let content_chars = function
  | Llm.Text s -> String.length s
  | Llm.Image { mime_type; data } -> String.length mime_type + String.length data
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

let session_info t = Option.map Session.info_of t.session

let has_tool_result turn =
  List.exists (function Llm.Tool_result _ -> true | _ -> false) turn.Llm.content

let turn_to_text (turn : Llm.turn) =
  let role = match turn.Llm.role with User -> "User" | Assistant -> "Assistant" in
  let part = function
    | Llm.Text s -> s
    | Llm.Image { mime_type; _ } -> "[image " ^ mime_type ^ "]"
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
    let session = session_info t in
    match
      Extensions.emit_session_before_compact
        ?session_file:(Option.map (fun (s : Session.info) -> s.path) session)
        ?session_id:(Option.map (fun (s : Session.info) -> s.id) session)
        ?session_name:(Option.map (fun (s : Session.info) -> s.name) session)
        ~turn_count:n ()
    with
    | Extensions.Session_cancel reason -> "Compaction cancelled: " ^ reason
    | Extensions.Session_continue ->
    let old_turns = t.turns in
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
    try
      let blocks, _ = Llm.complete t.cfg ~system:sys ~tools_enabled:false prompt in
      let summary = String.concat "\n" (List.filter_map (function Llm.Text s -> Some s | _ -> None) blocks) in
      let summary_turn = { Llm.role = User; content = [ Llm.Text ("[Earlier conversation summary]\n" ^ summary) ] } in
      t.turns <- summary_turn :: recent;
      t.last_input_tokens <- 0;
      (* force re-estimate next turn *)
      Option.iter (fun s -> Session.save_all s t.turns) t.session;
      Extensions.emit_session_compact
        ?session_file:(Option.map (fun (s : Session.info) -> s.path) session)
        ?session_id:(Option.map (fun (s : Session.info) -> s.id) session)
        ?session_name:(Option.map (fun (s : Session.info) -> s.name) session)
        ~before_turn_count:n ~after_turn_count:(List.length t.turns) ();
      Printf.sprintf "Compacted %d older turns into a summary." (List.length older)
    with e ->
      t.turns <- old_turns;
      "Compaction failed: " ^ Printexc.to_string e
  end

let should_compact t =
  t.auto_compact
  && float_of_int (context_used t) > t.compact_threshold *. float_of_int t.context_window
  && List.length t.turns > keep_recent + 1

(* Run a sub-agent for the `task` tool: a fresh, session-less agent at one
   greater depth that inherits config/tools but starts with an empty history. *)
let rec run_sub_agent t input =
  if t.depth >= max_depth then "Error: maximum sub-agent depth reached"
  else
    let prompt = try Yojson.Safe.Util.(input |> member "prompt" |> to_string) with _ -> "" in
    if prompt = "" then "Error: task requires a prompt"
    else begin
      let sub =
        { t with turns = []; session = None; depth = t.depth + 1; last_input_tokens = 0; last_output_tokens = 0 }
      in
      try send sub prompt with
      | Sys.Break as e -> raise e
      | e -> "Error: " ^ Printexc.to_string e
    end

and run_tool t id name input : Llm.content =
  let input, blocked =
    match Extensions.emit_tool_call ~tool_call_id:id ~tool_name:name input with
    | Extensions.Tool_continue input -> (input, None)
    | Extensions.Tool_block reason -> (input, Some reason)
  in
  t.fe.tool_call name (preview_input input);
  let result =
    match blocked with
    | Some reason -> "Tool call blocked by extension: " ^ reason
    | None ->
      Extensions.emit_tool_execution_start ~tool_call_id:id ~tool_name:name ~input;
      if not (tool_allowed t name) then Printf.sprintf "Error: tool %s is not enabled" name
      else if Tools.canonical_name name = "task" then run_sub_agent t input
      else if not (approve t name input) then "Error: command not approved by user"
      else
        match Tools.find (Tools.canonical_name name) with
        | Some tool -> ( try tool.execute input with
        | Sys.Break as e -> raise e
        | e -> "Error: " ^ Printexc.to_string e)
        | None -> Printf.sprintf "Error: unknown tool %s" name
  in
  let result = Extensions.emit_tool_result ~tool_call_id:id ~tool_name:name ~input result in
  let is_error =
    Tools.string_starts_with result "Error:"
    || Tools.string_starts_with result "Tool call blocked by extension"
  in
  if blocked = None then begin
    Extensions.emit_tool_execution_update ~tool_call_id:id ~tool_name:name ~input
      (`Assoc [ ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String result) ] ]) ]);
    Extensions.emit_tool_execution_end ~tool_call_id:id ~tool_name:name ~result ~is_error
  end;
  t.fe.tool_result (Extensions.render_text ~kind:"tool_result" ~role:"tool" ~tool_name:name result);
  Llm.Tool_result { id; content = result }

and step t : string =
  let limit_message () =
    Printf.sprintf "Error: stopped after %d tool round%s to avoid an infinite loop."
      t.max_tool_rounds (if t.max_tool_rounds = 1 then "" else "s")
  in
  let rec loop rounds =
    if rounds >= t.max_tool_rounds then limit_message ()
    else begin
      Extensions.emit_turn_start ~turn_index:rounds;
      let streamed = ref false in
      let streamed_text = Buffer.create 256 in
      let use_message_renderer = Extensions.has_message_renderers () in
      Extensions.emit_message_start { Llm.role = Assistant; content = [] };
      let on_text s =
        streamed := true;
        Buffer.add_string streamed_text s;
        Extensions.emit_message_update ~delta:s
          { Llm.role = Assistant; content = [ Llm.Text (Buffer.contents streamed_text) ] };
        if not use_message_renderer then t.fe.text_delta s
      in
      let llm_turns = Extensions.emit_context (context_turns t) in
      let cfg = effective_config t in
      let blocks, usage =
        Llm.complete cfg ~system:t.system ~on_text ~tools_enabled:t.tools_enabled ?tool_names:(effective_tool_names t) llm_turns
      in
      if usage.Llm.input_tokens > 0 then t.last_input_tokens <- usage.Llm.input_tokens;
      if usage.Llm.output_tokens > 0 then t.last_output_tokens <- usage.Llm.output_tokens;
      List.iter (function Llm.Thinking { text; _ } -> t.fe.thinking text | _ -> ()) blocks;
      let assistant_turn = Extensions.emit_message_end { Llm.role = Assistant; content = blocks } in
      add t assistant_turn;
      let texts = List.filter_map (function Llm.Text s -> Some s | _ -> None) assistant_turn.content in
      if use_message_renderer then begin
        let rendered = Extensions.render_text ~kind:"message" ~role:"assistant" (String.concat "\n" texts) in
        if String.trim rendered <> "" then t.fe.text_delta rendered
      end;
      if !streamed || use_message_renderer then t.fe.text_done ();
      let tool_results =
        List.filter_map
          (function
            | Llm.Tool_use { id; name; input } -> Some (run_tool t id name input)
            | _ -> None)
          assistant_turn.content
      in
      t.fe.message_end assistant_turn usage cfg (if tool_results <> [] then "tool_use" else "end");
      if tool_results <> [] then begin
        let pending_result_turn = { Llm.role = User; content = tool_results } in
        Extensions.emit_message_start pending_result_turn;
        let result_turn = Extensions.emit_message_end pending_result_turn in
        add t result_turn;
        t.fe.tool_result_end result_turn;
        Extensions.emit_turn_end ~turn_index:rounds ~message:assistant_turn ~tool_results;
        loop (rounds + 1)
      end
      else begin
        Extensions.emit_turn_end ~turn_index:rounds ~message:assistant_turn ~tool_results;
        String.concat "\n" texts
      end
    end
  in
  loop 0

and send t (user_input : string) : string =
  match Extensions.emit_input user_input with
  | Extensions.Input_handled -> "Input handled by extension."
  | Extensions.Input_continue user_input ->
    let expanded = Mentions.expand_rich user_input in
    let images =
      List.map
        (fun (image : Mentions.image) -> Llm.Image { mime_type = image.mime_type; data = image.data })
        expanded.images
    in
    send_content ~emit_input:false t (Llm.Text expanded.text :: images)

and send_content ?(emit_input = true) t (content : Llm.content list) : string =
  let content =
    if emit_input then
      match content with
      | Llm.Text text :: rest -> (
        match Extensions.emit_input ~source:"rpc" text with
        | Extensions.Input_handled -> []
        | Extensions.Input_continue text -> Llm.Text text :: rest)
      | _ -> content
    else content
  in
  if content = [] then "Input handled by extension."
  else begin
    if should_compact t then begin
      t.fe.notice "Auto-compacting context...";
      ignore (compact t)
    end;
    let rec drop n xs =
      if n <= 0 then xs
      else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest
    in
    let start_index = List.length t.turns in
    let prompt_text =
      content
      |> List.filter_map (function Llm.Text s -> Some s | _ -> None)
      |> String.concat "\n"
    in
    let before_agent = Extensions.emit_before_agent_start ~prompt:prompt_text ~system_prompt:t.system in
    let old_system = t.system in
    Option.iter (fun system -> t.system <- system) before_agent.Extensions.system_prompt;
    Extensions.emit_agent_start ();
    List.iter
      (fun injected ->
        Extensions.emit_message_start injected;
        add t (Extensions.emit_message_end injected))
      before_agent.Extensions.injected_messages;
    let pending_user_turn = { Llm.role = User; content } in
    Extensions.emit_message_start pending_user_turn;
    let user_turn = Extensions.emit_message_end pending_user_turn in
    add t user_turn;
    Fun.protect
      ~finally:(fun () -> t.system <- old_system)
      (fun () ->
        let response = step t in
        Extensions.emit_agent_end ~messages:(drop start_index t.turns);
        response)
  end
