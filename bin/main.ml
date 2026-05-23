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
        if line <> "" then run_turn agent line;
        print_newline ();
        loop ()
      end
  in
  loop ()

let () =
  match Llm.config () with
  | exception Llm.Config_error msg ->
    Printf.eprintf "%s %s\n%!" (red "Config error:") msg;
    exit 1
  | cfg ->
    let session, initial_turns =
      match Sys.getenv_opt "AGENT_SESSION_FILE" with
      | Some path when String.trim path <> "" ->
        let prior = Session.load path in
        (Some (Session.create path), prior)
      | _ -> (None, [])
    in
    let agent = Agent.create ?session ~initial_turns cfg in
    (match Array.to_list Sys.argv |> List.tl with
     | [] -> interactive agent cfg (List.length initial_turns)
     | args -> run_turn agent (String.concat " " args))
