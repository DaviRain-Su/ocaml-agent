(* Interactive REPL for the OCaml code agent.

   Provider/model/key are configured via environment variables (see README):
     AGENT_PROVIDER     anthropic | openai       (default anthropic)
     AGENT_MODEL        model name
     AGENT_API_KEY      API key (or provider-specific *_API_KEY)
     AGENT_BASE_URL     API base URL override
     AGENT_AUTO_APPROVE skip run_bash approval prompts when truthy
     AGENT_SESSION_FILE JSONL file to persist to (and resume from if it exists)

   Usage:
     dune exec ocaml-agent                 # interactive REPL
     dune exec ocaml-agent -- "a prompt"   # one-shot, then exit *)

open Agent_lib

let bold s = "\027[1m" ^ s ^ "\027[0m"
let dim s = "\027[2m" ^ s ^ "\027[0m"
let red s = "\027[31m" ^ s ^ "\027[0m"
let green s = "\027[32m" ^ s ^ "\027[0m"

let banner cfg resumed =
  print_string (bold "OCaml Code Agent\n");
  print_string (dim "Config: " ^ Llm.describe cfg ^ "\n");
  if resumed > 0 then print_string (dim (Printf.sprintf "Resumed %d turns from session.\n" resumed));
  print_string (dim "Type your request. /exit or Ctrl-D to quit.\n\n");
  flush stdout

let run_turn agent input =
  match Agent.send agent input with
  | _ -> ()
  | exception Llm.Api_error msg -> Printf.eprintf "%s %s\n%!" (red "API error:") msg
  | exception e -> Printf.eprintf "%s %s\n%!" (red "Error:") (Printexc.to_string e)

let starts_with prefix s =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let handle_bang agent line =
  if not (starts_with "!" line) then false
  else
    let exclude = starts_with "!!" line in
    let off = if exclude then 2 else 1 in
    let command = String.trim (String.sub line off (String.length line - off)) in
    if command = "" then false
    else begin
      let result = Agent.run_user_bash ~exclude_from_context:exclude agent command in
      Printf.printf "%s\n%!" (Render.tool_result result);
      true
    end

let print_help () =
  List.iter
    (fun (c, d) -> Printf.printf "  %-22s %s\n" c d)
    [ ("/model [alias] [name]", "switch provider/model, or list providers");
      ("/session", "show current model, turn count, and context usage");
      ("/compact", "summarize older turns to free up context");
      ("/think <level>", "set reasoning level (off/low/medium/high)");
      ("/sessions", "list saved sessions");
      ("/resume <n|id>", "resume a saved session");
      ("/name <text>", "name the current session");
      ("/clone", "duplicate the current session");
      ("/export <file>", "export session (.html or .jsonl)");
      ("/copy", "copy last reply to the clipboard");
      ("/reload", "reload context files, skills, prompt templates, and extensions");
      ("/new", "clear the conversation");
      ("/help", "show this help");
      ("/exit, /quit", "quit") ];
  Printf.printf "  %-22s %s\n" "!cmd" "run shell and add output to model context";
  Printf.printf "  %-22s %s\n" "!!cmd" "run shell without adding output to model context";
  flush stdout

let list_providers agent =
  Printf.printf "Current: %s\n" (Llm.describe (Agent.config agent));
  List.iter
    (fun (name, has) -> Printf.printf "  %s %s\n" (if has then green "*" else " ") name)
    (Llm.provider_status ());
  print_string (dim "  (* = API key detected in env; use /model <name> [model])\n");
  flush stdout

let switch_model agent = function
  | [] -> list_providers agent
  | alias :: rest ->
    let model = match rest with [] -> None | m :: _ -> Some m in
    (match Llm.config_for ?model alias with
     | cfg ->
       Agent.set_config agent cfg;
       Printf.printf "%s %s\n%!" (dim "Switched:") (Llm.describe cfg)
     | exception Llm.Config_error e -> Printf.eprintf "%s %s\n%!" (red "Error:") e)

let handle_command agent line =
  let parts = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in
  match parts with
  | "/help" :: _ -> print_help ()
  | "/session" :: _ ->
    let used, window, pct = Agent.usage_info agent in
    let cfg = Agent.config agent in
    Printf.printf "%s | think:%s | %d turns | context ~%d/%d (%.0f%%)\n%!" (Llm.describe cfg)
      cfg.Llm.thinking (Agent.turn_count agent) used window (pct *. 100.)
  | "/think" :: rest ->
    let level = match rest with l :: _ -> l | [] -> "off" in
    Agent.set_thinking agent level;
    Printf.printf "%s reasoning level = %s\n%!" (dim "Set") level
  | "/compact" :: _ ->
    print_string (dim (Agent.compact agent ^ "\n"));
    flush stdout
  | "/sessions" :: _ -> Printf.printf "%s\n%!" (Commands.format_sessions ())
  | "/resume" :: a :: _ -> Printf.printf "%s\n%!" (dim (Commands.resume agent a))
  | "/name" :: rest when rest <> [] -> Printf.printf "%s\n%!" (dim (Commands.name agent (String.concat " " rest)))
  | "/clone" :: _ -> Printf.printf "%s\n%!" (dim (Commands.clone agent))
  | "/export" :: p :: _ -> Printf.printf "%s\n%!" (dim (Commands.export agent p))
  | "/copy" :: _ -> Printf.printf "%s\n%!" (dim (Commands.copy agent))
  | "/reload" :: _ ->
    ignore (Extensions.load ());
    Agent.reload_system_prompt agent;
    Printf.printf "%s\n%!" (dim "Reloaded resources.")
  | "/new" :: _ ->
    Agent.reset agent;
    print_string (dim "Conversation cleared.\n");
    flush stdout
  | "/model" :: rest -> switch_model agent rest
  | cmd :: _ -> (
    match Prompts.expand_command line with
    | Some prompt -> run_turn agent prompt
    | None -> Printf.printf "%s %s (try /help)\n%!" (red "Unknown command") cmd)
  | [] -> ()

let interactive agent cfg resumed =
  banner cfg resumed;
  let rec loop () =
    print_string (bold "you> ");
    flush stdout;
    match In_channel.input_line stdin with
    | None -> print_newline ()
    | Some line ->
      let line = String.trim line in
      if line = "/exit" || line = "/quit" then ()
      else begin
        if line <> "" then
          if handle_bang agent line then ()
          else if String.length line > 0 && line.[0] = '/' then handle_command agent line
          else run_turn agent line;
        print_newline ();
        loop ()
      end
  in
  loop ()

type opts =
  { mutable model : string option;
    mutable provider : string option;
    mutable api_key : string option;
    mutable thinking : string option;
    mutable cont : bool;
    mutable print : bool;
    mutable no_tools : bool;
    mutable no_tui : bool;
    mutable mode : string; (* text | json | rpc *)
    mutable list_models : string option;
    mutable export : string option;
    mutable resume : bool;
    mutable session_spec : string option;
    mutable fork_spec : string option;
    mutable session_dir : string option;
    mutable no_session : bool;
    mutable tools : string list option;
    mutable no_prompt_templates : bool;
    mutable prompt_templates : string list;
    mutable no_skills : bool;
    mutable skills : string list;
    mutable no_context_files : bool;
    mutable system_prompt : string option;
    mutable append_system_prompt : string list;
    mutable file_args : string list;
    mutable prompt : string list }

let usage =
  "Usage: ocaml-agent [options] [prompt]\n\n\
   Options:\n\
  \  -m, --model <name>       model to use\n\
  \      --provider <alias>   provider (anthropic, deepseek, kimi, zai, ...)\n\
  \      --api-key <key>      API key override\n\
  \      --thinking <level>   reasoning level (off/low/medium/high)\n\
  \      --system-prompt <s>  replace the base system prompt (or read file path)\n\
  \      --append-system-prompt <s>  append text or file contents (repeatable)\n\
  \  -c, --continue           resume the last session (.ocaml-agent/session.jsonl)\n\
  \  -r, --resume             resume the last session in non-TTY mode\n\
  \      --session <path|id>  use a specific session file or partial id\n\
  \      --fork <path|id>     fork a session into a new session\n\
  \      --session-dir <dir>  custom session storage directory\n\
  \  -p, --print              one-shot mode; prompt from args or stdin\n\
  \      --no-session         do not create or resume a session\n\
  \  -t, --tools <list>       comma-separated tool allowlist (Pi aliases accepted)\n\
  \      --no-tools           disable all tools for this run\n\
  \      --prompt-template <path> load a prompt template file or directory\n\
  \      --no-prompt-templates disable prompt template discovery\n\
  \      --skill <path>        load a skill file or directory\n\
  \      --no-skills           disable skill discovery\n\
  \      --no-context-files    disable AGENTS.md / CLAUDE.md injection\n\
  \      --no-tui             use the plain line REPL instead of the full-screen TUI\n\
  \      --mode <text|json|rpc>  output mode; rpc = JSON-RPC over stdin/stdout\n\
  \      --list-models [pat]  list known models and exit\n\
  \      --export <file>      export the (resumed) session to .html/.jsonl and exit\n\
  \  -h, --help               show this help\n\n\
   With no prompt and a TTY, starts an interactive REPL.\n\
   Configuration is otherwise via AGENT_* / *_API_KEY env vars (see README)."

(* Parse flags up to the first positional token; the rest is the prompt. *)
let parse_args argv =
  let o =
    { model = None;
      provider = None;
      api_key = None;
      thinking = None;
      cont = false;
      print = false;
      no_tools = false;
      no_tui = false;
      mode = "text";
      list_models = None;
      export = None;
      resume = false;
      session_spec = None;
      fork_spec = None;
      session_dir = None;
      no_session = false;
      tools = None;
      no_prompt_templates = false;
      prompt_templates = [];
      no_skills = false;
      skills = [];
      no_context_files = false;
      system_prompt = None;
      append_system_prompt = [];
      file_args = [];
      prompt = [] }
  in
  let rec go = function
    | [] -> ()
    | "--" :: rest -> o.prompt <- rest
    | ("-m" | "--model") :: v :: rest -> o.model <- Some v; go rest
    | "--provider" :: v :: rest -> o.provider <- Some v; go rest
    | "--api-key" :: v :: rest -> o.api_key <- Some v; go rest
    | "--thinking" :: v :: rest -> o.thinking <- Some v; go rest
    | "--system-prompt" :: v :: rest -> o.system_prompt <- Some v; go rest
    | "--append-system-prompt" :: v :: rest ->
      o.append_system_prompt <- o.append_system_prompt @ [ v ];
      go rest
    | ("-c" | "--continue") :: rest -> o.cont <- true; go rest
    | ("-r" | "--resume") :: rest -> o.resume <- true; go rest
    | "--session" :: v :: rest -> o.session_spec <- Some v; go rest
    | "--fork" :: v :: rest -> o.fork_spec <- Some v; go rest
    | "--session-dir" :: v :: rest -> o.session_dir <- Some v; go rest
    | ("-p" | "--print") :: rest -> o.print <- true; go rest
    | "--no-session" :: rest -> o.no_session <- true; go rest
    | ("--tools" | "-t") :: v :: rest ->
      o.tools <- Some (String.split_on_char ',' v |> List.map String.trim |> List.filter (fun s -> s <> ""));
      go rest
    | ("--no-tools" | "-nt") :: rest -> o.no_tools <- true; go rest
    | "--prompt-template" :: v :: rest ->
      o.prompt_templates <- o.prompt_templates @ [ v ];
      go rest
    | "--no-prompt-templates" :: rest -> o.no_prompt_templates <- true; go rest
    | "--skill" :: v :: rest ->
      o.skills <- o.skills @ [ v ];
      go rest
    | "--no-skills" :: rest -> o.no_skills <- true; go rest
    | ("--no-context-files" | "-nc") :: rest -> o.no_context_files <- true; go rest
    | "--no-tui" :: rest -> o.no_tui <- true; go rest
    | "--mode" :: v :: rest -> o.mode <- v; go rest
    | "--rpc" :: rest -> o.mode <- "rpc"; go rest
    | "--export" :: v :: rest -> o.export <- Some v; go rest
    | "--list-models" :: rest -> (
      match rest with
      | p :: tl when String.length p = 0 || p.[0] <> '-' -> o.list_models <- Some p; go tl
      | _ -> o.list_models <- Some ""; go rest)
    | ("-h" | "--help") :: _ -> print_string (usage ^ "\n"); exit 0
    | arg :: rest when String.length arg > 1 && arg.[0] = '@' ->
      o.file_args <- o.file_args @ [ String.sub arg 1 (String.length arg - 1) ];
      go rest
    | arg :: _ as all ->
      if String.length arg > 0 && arg.[0] = '-' then begin
        Printf.eprintf "%s unknown flag %s\n%s\n%!" (red "Error:") arg usage;
        exit 2
      end
      else o.prompt <- all
  in
  go argv;
  o

let read_stdin_all () =
  let b = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel b stdin 4096
     done
   with End_of_file -> ());
  Buffer.contents b

let list_models pat =
  let entries = Models.list ~pat () in
  if entries = [] then print_string "(no matching models)\n"
  else
    List.iter
      (fun (e : Models.entry) -> Printf.printf "%-12s %-26s ctx %d\n" e.Models.provider e.Models.id e.Models.context_window)
      entries

let emit_json (j : Yojson.Safe.t) =
  print_string (Yojson.Safe.to_string j);
  print_char '\n';
  flush stdout

let pi_content_json = function
  | Llm.Text s -> `Assoc [ ("type", `String "text"); ("text", `String s) ]
  | Llm.Thinking { text; signature } ->
    `Assoc [ ("type", `String "thinking"); ("text", `String text); ("signature", `String signature) ]
  | Llm.Tool_use { id; name; input } ->
    `Assoc
      [ ("type", `String "toolCall");
        ("id", `String id);
        ("name", `String name);
        ("arguments", input) ]
  | Llm.Tool_result { id; content } ->
    `Assoc
      [ ("type", `String "toolResult");
        ("toolCallId", `String id);
        ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String content) ] ]) ]

let pi_message_json ?usage ?cfg ?stop_reason (turn : Llm.turn) =
  let role = match turn.role with Llm.User -> "user" | Llm.Assistant -> "assistant" in
  let fields =
    [ ("role", `String role); ("content", `List (List.map pi_content_json turn.content)) ]
  in
  let fields =
    match usage with
    | None -> fields
    | Some u ->
      fields
      @
      [ ( "usage",
          `Assoc
            [ ("input", `Int u.Llm.input_tokens);
              ("output", `Int u.Llm.output_tokens);
              ("totalTokens", `Int (u.input_tokens + u.output_tokens)) ] ) ]
  in
  let fields = match cfg with Some c -> fields @ [ ("model", `String c.Llm.model) ] | None -> fields in
  let fields = match stop_reason with Some s -> fields @ [ ("stopReason", `String s) ] | None -> fields in
  `Assoc fields

let json_frontend () : Agent.frontend =
  { text_delta = (fun s -> emit_json (`Assoc [ ("type", `String "text_delta"); ("text", `String s) ]));
    text_done = (fun () -> emit_json (`Assoc [ ("type", `String "text_done") ]));
    thinking = (fun s -> if String.trim s <> "" then emit_json (`Assoc [ ("type", `String "thinking"); ("text", `String s) ]));
    tool_call =
      (fun name prev ->
        emit_json (`Assoc [ ("type", `String "tool_call"); ("name", `String name); ("input", `String prev) ]));
    tool_result = (fun res -> emit_json (`Assoc [ ("type", `String "tool_result"); ("content", `String res) ]));
    notice = (fun s -> emit_json (`Assoc [ ("type", `String "notice"); ("text", `String s) ]));
    message_end =
      (fun turn usage cfg stop_reason ->
        emit_json
          (`Assoc
            [ ("type", `String "message_end");
              ("message", pi_message_json ~usage ~cfg ~stop_reason turn) ]));
    tool_result_end =
      (fun turn ->
        emit_json (`Assoc [ ("type", `String "tool_result_end"); ("message", pi_message_json turn) ]));
    confirm_bash =
      (fun command ->
        emit_json (`Assoc [ ("type", `String "bash_denied"); ("command", `String command) ]);
        Agent.Deny) }

let file_arg_text paths =
  let missing = List.filter (fun p -> not (Sys.file_exists p) || Sys.is_directory p) paths in
  match missing with
  | p :: _ ->
    Printf.eprintf "%s File not found: %s\n%!" (red "Error:") p;
    exit 1
  | [] -> Mentions.expand_file_args paths

let () =
  Random.self_init ();
  let o = parse_args (Array.to_list Sys.argv |> List.tl) in
  Option.iter (fun k -> Unix.putenv "AGENT_API_KEY" k) o.api_key;
  Option.iter (fun d -> Unix.putenv "AGENT_SESSION_DIR" d) o.session_dir;
  Option.iter (fun t -> Unix.putenv "AGENT_THINKING" t) o.thinking;
  Option.iter (fun s -> Unix.putenv "AGENT_SYSTEM_PROMPT" s) o.system_prompt;
  if o.append_system_prompt <> [] then
    Unix.putenv "AGENT_APPEND_SYSTEM_PROMPT" (Agent.join_prompt_inputs o.append_system_prompt);
  if o.no_prompt_templates then Unix.putenv "AGENT_NO_PROMPT_TEMPLATES" "1";
  if o.prompt_templates <> [] then Unix.putenv "AGENT_PROMPT_TEMPLATE_PATHS" (String.concat "\n" o.prompt_templates);
  if o.no_skills then Unix.putenv "AGENT_NO_SKILLS" "1";
  if o.skills <> [] then Unix.putenv "AGENT_SKILL_PATHS" (String.concat "\n" o.skills);
  if o.no_context_files then Unix.putenv "AGENT_NO_CONTEXT_FILES" "1";
  (match o.list_models with
   | Some pat -> list_models pat; exit 0
   | None -> ());
  (match Extensions.load () with
   | [] -> ()
   | names -> if o.mode <> "rpc" && o.mode <> "json" then
       Printf.eprintf "%s %s\n%!" (dim "Loaded extension tools:") (String.concat ", " names));
  match
    match o.provider with
    | Some p -> Llm.config_for ?model:o.model p
    | None ->
      let c = Llm.config () in
      (match o.model with Some m -> { c with model = m } | None -> c)
  with
  | exception Llm.Config_error msg ->
    Printf.eprintf "%s %s\n%!" (red "Config error:") msg;
    exit 1
  | cfg ->
    let one_shot_mode = o.print || o.prompt <> [] || o.file_args <> [] in
    let open_resolved_session spec =
      match Session.resolve_path spec with
      | Some path -> (Some (Session.open_file path), Session.load_turns path)
      | None ->
        Printf.eprintf "%s no matching session: %s\n%!" (red "Error:") spec;
        exit 1
    in
    let session, initial_turns =
      match Sys.getenv_opt "AGENT_SESSION_FILE" with
      | Some p when (not o.no_session) && o.session_spec = None && o.fork_spec = None && String.trim p <> "" ->
        (Some (Session.open_file p), Session.load_turns p)
      | _ ->
        if o.no_session then (None, [])
        else
          match (o.fork_spec, o.session_spec) with
          | Some spec, _ -> (
            match Session.fork_from spec with
            | Some s -> (Some s, Session.load_turns s.Session.path)
            | None ->
              Printf.eprintf "%s no matching session to fork: %s\n%!" (red "Error:") spec;
              exit 1)
          | None, Some spec -> open_resolved_session spec
          | None, None when o.cont || o.resume ->
            (* Resume the most recent session in the sessions dir. *)
            (match Session.list () with
             | i :: _ -> (Some (Session.open_file i.Session.path), Session.load_turns i.Session.path)
             | [] -> (Some (Session.create_new ()), []))
          | None, None ->
            if one_shot_mode then (None, []) (* one-shot is ephemeral by default *)
            else (Some (Session.create_new ()), []) (* interactive runs are persisted + resumable *)
    in
    let allowed_tools = Option.map Tools.canonical_names o.tools in
    let agent = Agent.create ?session ~initial_turns ~tools_enabled:(not o.no_tools) ?allowed_tools cfg in
    (* --export: dump the (resumed) session and exit. *)
    (match o.export with
     | Some path -> print_string (Commands.export agent path ^ "\n"); exit 0
     | None -> ());
    let prompt () =
      let parts = ref [] in
      if not (Unix.isatty Unix.stdin) then (
        let s = read_stdin_all () in
        if String.trim s <> "" then parts := !parts @ [ s ]);
      let files = file_arg_text o.file_args in
      if files <> "" then parts := !parts @ [ files ];
      if o.prompt <> [] then parts := !parts @ [ String.concat " " o.prompt ];
      String.trim (String.concat "\n" !parts)
    in
    match o.mode with
    | "rpc" ->
      if o.file_args <> [] then begin
        Printf.eprintf "%s @file arguments are not supported in RPC mode\n%!" (red "Error:");
        exit 1
      end;
      Rpc.run agent
    | "json" ->
      Agent.set_frontend agent (json_frontend ());
      let p = prompt () in
      if p = "" then (emit_json (`Assoc [ ("type", `String "error"); ("message", `String "no prompt") ]); exit 1)
      else (
        try
          let text = Agent.send agent p in
          let used, window, _ = Agent.usage_info agent in
          emit_json
            (`Assoc
              [ ("type", `String "turn_done");
                ("text", `String text);
                ("usage", `Assoc [ ("context_used", `Int used); ("context_window", `Int window) ]) ])
        with Llm.Api_error e ->
          emit_json (`Assoc [ ("type", `String "error"); ("message", `String e) ]);
          exit 1)
    | _ ->
      if one_shot_mode then (let p = prompt () in if p <> "" then run_turn agent p)
      else if (not o.no_tui) && Unix.isatty Unix.stdin && Unix.isatty Unix.stdout then Tui.run agent
      else interactive agent cfg (List.length initial_turns)
