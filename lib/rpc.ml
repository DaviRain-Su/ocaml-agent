(* JSON-RPC-style driver over stdin/stdout (one JSON object per line), for
   embedding the agent in other programs. Requests on stdin, events on stdout.

   Requests:  {"method":"send","params":{"message":"..."}}
              {"method":"set_model","params":{"provider":"kimi","model":"..."}}
              {"method":"session"} | {"method":"new"} | {"method":"quit"}
   Events:    {"type":"ready"|"text_delta"|"text_done"|"thinking"|"tool_call"|
               "tool_result"|"notice"|"turn_done"|"ok"|"error"|"bash_denied", ...} *)

open Yojson.Safe.Util

let emit (j : Yojson.Safe.t) =
  print_string (Yojson.Safe.to_string j);
  print_char '\n';
  flush stdout

let event ty fields = emit (`Assoc (("type", `String ty) :: fields))
let error msg = event "error" [ ("message", `String msg) ]

let make_frontend () : Agent.frontend =
  { text_delta = (fun s -> event "text_delta" [ ("text", `String s) ]);
    text_done = (fun () -> event "text_done" []);
    thinking = (fun s -> if String.trim s <> "" then event "thinking" [ ("text", `String s) ]);
    tool_call = (fun name prev -> event "tool_call" [ ("name", `String name); ("input", `String prev) ]);
    tool_result = (fun res -> event "tool_result" [ ("content", `String res) ]);
    notice = (fun s -> event "notice" [ ("text", `String s) ]);
    message_end = (fun _ _ _ _ -> ());
    tool_result_end = (fun _ -> ());
    confirm_bash =
      (fun cmd ->
        event "bash_denied" [ ("command", `String cmd) ];
        Agent.Deny) }

let usage_json agent =
  let used, window, pct = Agent.usage_info agent in
  `Assoc [ ("context_used", `Int used); ("context_window", `Int window); ("percent", `Float pct) ]

let opt_str j k = match j |> member k with `String s -> Some s | _ -> None

let run agent =
  Agent.set_frontend agent (make_frontend ());
  let c = Agent.config agent in
  event "ready" [ ("config", `String (Llm.describe c)) ];
  let handle line =
    match Yojson.Safe.from_string line with
    | exception _ -> error "invalid JSON"
    | j -> (
      match j |> member "method" with
      | `String "send" -> (
        match opt_str (j |> member "params") "message" with
        | None -> error "send requires params.message"
        | Some msg -> (
          try
            let final = Agent.send agent msg in
            event "turn_done" [ ("text", `String final); ("usage", usage_json agent) ]
          with
          | Llm.Api_error e -> error e
          | e -> error (Printexc.to_string e)))
      | `String "set_model" -> (
        let params = j |> member "params" in
        try
          let c =
            match opt_str params "provider" with
            | Some p -> Llm.config_for ?model:(opt_str params "model") p
            | None -> (
              match opt_str params "model" with
              | Some m -> { (Agent.config agent) with Llm.model = m }
              | None -> Agent.config agent)
          in
          Agent.set_config agent c;
          event "ok" [ ("config", `String (Llm.describe c)) ]
        with Llm.Config_error e -> error e)
      | `String "session" ->
        let c = Agent.config agent in
        event "ok"
          [ ("config", `String (Llm.describe c));
            ("turns", `Int (Agent.turn_count agent));
            ("usage", usage_json agent) ]
      | `String "new" -> Agent.reset agent; event "ok" []
      | `String "quit" -> raise Exit
      | `String other -> error ("unknown method: " ^ other)
      | _ -> error "missing method")
  in
  let rec loop () =
    match In_channel.input_line stdin with
    | None -> ()
    | Some line ->
      if String.trim line <> "" then begin
        try handle line with e -> error (Printexc.to_string e)
      end;
      loop ()
  in
  (try loop () with Exit -> ())
