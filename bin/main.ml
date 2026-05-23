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

let print_help () =
  List.iter
    (fun (c, d) -> Printf.printf "  %-22s %s\n" c d)
    [ ("/model [alias] [name]", "switch provider/model, or list providers");
      ("/session", "show current model and turn count");
      ("/new", "clear the conversation");
      ("/help", "show this help");
      ("/exit, /quit", "quit") ];
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
    Printf.printf "%s | %d turns\n%!" (Llm.describe (Agent.config agent)) (Agent.turn_count agent)
  | "/new" :: _ ->
    Agent.reset agent;
    print_string (dim "Conversation cleared.\n");
    flush stdout
  | "/model" :: rest -> switch_model agent rest
  | cmd :: _ -> Printf.printf "%s %s (try /help)\n%!" (red "Unknown command") cmd
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
          if String.length line > 0 && line.[0] = '/' then handle_command agent line
          else run_turn agent line;
        print_newline ();
        loop ()
      end
  in
  loop ()

type opts =
  { mutable model : string option;
    mutable provider : string option;
    mutable thinking : string option;
    mutable cont : bool;
    mutable print : bool;
    mutable no_tools : bool;
    mutable prompt : string list }

let usage =
  "Usage: ocaml-agent [options] [prompt]\n\n\
   Options:\n\
  \  -m, --model <name>       model to use\n\
  \      --provider <alias>   provider (anthropic, deepseek, kimi, zai, ...)\n\
  \      --thinking <level>   reasoning level (off/low/medium/high)\n\
  \  -c, --continue           resume the last session (.ocaml-agent/session.jsonl)\n\
  \  -p, --print              one-shot mode; prompt from args or stdin\n\
  \      --no-tools           disable all tools for this run\n\
  \  -h, --help               show this help\n\n\
   With no prompt and a TTY, starts an interactive REPL.\n\
   Configuration is otherwise via AGENT_* / *_API_KEY env vars (see README)."

(* Parse flags up to the first positional token; the rest is the prompt. *)
let parse_args argv =
  let o = { model = None; provider = None; thinking = None; cont = false; print = false; no_tools = false; prompt = [] } in
  let rec go = function
    | [] -> ()
    | "--" :: rest -> o.prompt <- rest
    | ("-m" | "--model") :: v :: rest -> o.model <- Some v; go rest
    | "--provider" :: v :: rest -> o.provider <- Some v; go rest
    | "--thinking" :: v :: rest -> o.thinking <- Some v; go rest
    | ("-c" | "--continue") :: rest -> o.cont <- true; go rest
    | ("-p" | "--print") :: rest -> o.print <- true; go rest
    | ("--no-tools" | "-nt") :: rest -> o.no_tools <- true; go rest
    | ("-h" | "--help") :: _ -> print_string (usage ^ "\n"); exit 0
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

let () =
  let o = parse_args (Array.to_list Sys.argv |> List.tl) in
  Option.iter (fun t -> Unix.putenv "AGENT_THINKING" t) o.thinking;
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
    let session, initial_turns =
      let path =
        match Sys.getenv_opt "AGENT_SESSION_FILE" with
        | Some p when String.trim p <> "" -> Some p
        | _ -> if o.cont then Some ".ocaml-agent/session.jsonl" else None
      in
      match path with
      | Some path -> (Some (Session.create path), Session.load path)
      | None -> (None, [])
    in
    let agent = Agent.create ?session ~initial_turns ~tools_enabled:(not o.no_tools) cfg in
    let one_shot () =
      let prompt = if o.prompt = [] then String.trim (read_stdin_all ()) else String.concat " " o.prompt in
      if prompt <> "" then run_turn agent prompt
    in
    if o.print || o.prompt <> [] then one_shot ()
    else interactive agent cfg (List.length initial_turns)
