open Agent_lib

let j = Yojson.Safe.from_string
let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let run name input =
  match Tools.find name with
  | Some t -> t.execute (j input)
  | None -> failwith ("no tool " ^ name)

let contains0 hay needle =
  try ignore (Str.search_forward (Str.regexp_string needle) hay 0); true with Not_found -> false

let contains = contains0

let () =
  let dir = Filename.temp_dir "agent_core_session_rpc_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  (* --- Llm turn <-> JSON round-trip --- *)
  let turns =
    [ { Llm.role = User; content = [ Llm.Text "hello" ] };
      { Llm.role = Assistant;
        content =
          [ Llm.Thinking { text = "let me think"; signature = "sig123" };
            Llm.Text "thinking";
            Llm.Tool_use { id = "t1"; name = "read_file"; input = j {|{"path":"a.txt"}|} } ] };
      { Llm.role = User; content = [ Llm.Image { mime_type = "image/png"; data = "iVBORw0KGgo=" } ] };
      { Llm.role = User; content = [ Llm.Tool_result { id = "t1"; content = "file body" } ] } ]
  in
  let roundtrip = List.map (fun t -> Llm.turn_of_json (Llm.turn_to_json t)) turns in
  check "turn json round-trips"
    (List.map Llm.turn_to_json turns = List.map Llm.turn_to_json roundtrip);
  let anthropic_image_json = Yojson.Safe.to_string (`List (Llm.anthropic_messages [ List.nth turns 2 ])) in
  check "Anthropic messages preserve image blocks"
    (contains0 anthropic_image_json "\"type\":\"image\"" && contains0 anthropic_image_json "\"media_type\":\"image/png\"");
  let openai_image_json = Yojson.Safe.to_string (`List (Llm.openai_messages ~system:"sys" [ List.nth turns 2 ])) in
  check "OpenAI messages preserve image blocks"
    (contains0 openai_image_json "\"type\":\"image_url\""
     && contains0 openai_image_json "data:image/png;base64,iVBORw0KGgo=");

  (* --- Session save/load --- *)
  let spath = "session.jsonl" in
  let s = Session.open_file spath in
  List.iter (Session.append s) turns;
  Session.close s;
  let loaded = Session.load spath in
  check "session reloads same turn count" (List.length loaded = List.length turns);
  check "session preserves content"
    (List.map Llm.turn_to_json loaded = List.map Llm.turn_to_json turns);

  (* --- session manager: dir, headers, list, name, clone --- *)
  let ns = Session.create_new ~name:"alpha" () in
  List.iter (Session.append ns) turns;
  (match Session.read_header ns.Session.path with
   | Some i -> check "session header has name" (i.Session.name = "alpha")
   | None -> check "session header has name" false);
  check "session in listing" (List.exists (fun (i : Session.info) -> i.Session.id = ns.Session.id) (Session.list ()));
  Session.set_name ns "beta" turns;
  (match Session.read_header ns.Session.path with
   | Some i -> check "session rename persists" (i.Session.name = "beta")
   | None -> check "session rename persists" false);
  check "set_name keeps turns" (List.length (Session.load_turns ns.Session.path) = List.length turns);
  check "session resolves by path" (Session.resolve_path ns.Session.path = Some ns.Session.path);
  check "session resolves by id" (Session.resolve_path ns.Session.id = Some ns.Session.path);
  check "session resolves by partial id"
    (Session.resolve_path (String.sub ns.Session.id 0 (min 8 (String.length ns.Session.id))) = Some ns.Session.path);
  let forked =
    match Session.fork_from ns.Session.id with
    | Some s -> s
    | None -> failwith "fork failed"
  in
  check "session fork copies turns" (List.length (Session.load_turns forked.Session.path) = List.length turns);
  check "session fork creates new file" (forked.Session.id <> ns.Session.id);
  Session.close forked;
  let cfg_for_reset =
    { Llm.provider = Llm.Openai;
      base_url = "https://api.example.test";
      api_key = "sk-test";
      model = "test-model";
      max_tokens = 4096;
      extra_headers = [];
      runtime = None;
      thinking = "off" }
  in
  let reset_session = Session.create_new ~name:"reset" () in
  List.iter (Session.append reset_session) turns;
  let reset_agent = Agent.create ~session:reset_session ~initial_turns:turns cfg_for_reset in
  Agent.reset reset_agent;
  check "reset truncates active session" (Session.load_turns reset_session.Session.path = []);
  Session.close reset_session;
  let bang_agent = Agent.create cfg_for_reset in
  let bang_result = Agent.run_user_bash bang_agent "printf bang" in
  check "bang shell captures output" (contains bang_result "bang");
  check "bang shell records context" (Agent.turn_count bang_agent = 1);
  let bang_context =
    match Agent.turns bang_agent with
    | [ { Llm.content = [ Llm.Text s ]; _ } ] -> s
    | _ -> ""
  in
  check "bang shell context includes command output" (contains bang_context "Ran `printf bang`" && contains bang_context "bang");
  let hidden_count = Agent.turn_count bang_agent in
  let hidden_result = Agent.run_user_bash ~exclude_from_context:true bang_agent "printf hidden" in
  check "hidden bang shell captures output" (contains hidden_result "hidden");
  check "hidden bang shell does not record context" (Agent.turn_count bang_agent = hidden_count);

  (* --- RPC command compatibility --- *)
  let rpc_field json key =
    match json with `Assoc fields -> List.assoc_opt key fields | _ -> None
  in
  let rpc_last out = match List.rev out with last :: _ -> last | [] -> `Null in
  let rpc_success out command =
    let r = rpc_last out in
    rpc_field r "type" = Some (`String "response")
    && rpc_field r "command" = Some (`String command)
    && rpc_field r "success" = Some (`Bool true)
  in
  let rpc_content =
    Rpc.prompt_content ~prefix:[ Llm.Text "file context" ]
      (j {|{"images":[{"mimeType":"image/png","data":"abc"}]}|})
      "rpc prompt"
  in
  check "rpc prompt content preserves CLI prefix and images"
    (match rpc_content with
     | [ Llm.Text "file context"; Llm.Text "rpc prompt"; Llm.Image { mime_type = "image/png"; data = "abc" } ] -> true
     | _ -> false);
  let rpc_agent = Agent.create cfg_for_reset in
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"id":"state-1","type":"get_state"}|}) in
  let state_ok =
    match rpc_field (rpc_last out) "data" with
    | Some (`Assoc fields) ->
      List.assoc_opt "thinkingLevel" fields = Some (`String "off")
      && List.assoc_opt "messageCount" fields = Some (`Int 0)
    | _ -> false
  in
  check "rpc Pi get_state response" (rpc_success out "get_state" && state_ok);
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"set_auto_compaction","enabled":false}|}) in
  check "rpc Pi set_auto_compaction toggles agent" (rpc_success out "set_auto_compaction" && not (Agent.auto_compact rpc_agent));
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"set_auto_retry","enabled":false}|}) in
  check "rpc Pi set_auto_retry toggles agent" (rpc_success out "set_auto_retry" && not (Agent.auto_retry rpc_agent));
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"set_steering_mode","mode":"one-at-a-time"}|}) in
  check "rpc Pi set_steering_mode persists" (rpc_success out "set_steering_mode" && Agent.steering_mode rpc_agent = "one-at-a-time");
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"set_follow_up_mode","mode":"one-at-a-time"}|}) in
  check "rpc Pi set_follow_up_mode persists" (rpc_success out "set_follow_up_mode" && Agent.follow_up_mode rpc_agent = "one-at-a-time");
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"get_state"}|}) in
  let queue_state_ok =
    match rpc_field (rpc_last out) "data" with
    | Some (`Assoc fields) ->
      List.assoc_opt "steeringMode" fields = Some (`String "one-at-a-time")
      && List.assoc_opt "followUpMode" fields = Some (`String "one-at-a-time")
      && List.assoc_opt "autoRetryEnabled" fields = Some (`Bool false)
    | _ -> false
  in
  check "rpc Pi get_state reflects queue/retry controls" (rpc_success out "get_state" && queue_state_ok);
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"abort_retry"}|}) in
  check "rpc Pi abort_retry acks" (rpc_success out "abort_retry");
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"abort_bash"}|}) in
  check "rpc Pi abort_bash acks" (rpc_success out "abort_bash");
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"abort"}|}) in
  check "rpc Pi abort acks" (rpc_success out "abort");
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"set_thinking_level","level":"high"}|}) in
  check "rpc Pi set_thinking_level applies" (rpc_success out "set_thinking_level" && (Agent.config rpc_agent).Llm.thinking = "high");
  Agent.set_thinking rpc_agent "off";
  let before_cycle_model = (Agent.config rpc_agent).Llm.model in
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"cycle_model"}|}) in
  let cycle_model_ok =
    match rpc_field (rpc_last out) "data" with
    | Some (`Assoc fields) -> (
      match List.assoc_opt "model" fields, List.assoc_opt "thinkingLevel" fields, List.assoc_opt "isScoped" fields with
      | Some (`Assoc model_fields), Some (`String "off"), Some (`Bool false) ->
        List.assoc_opt "id" model_fields <> Some (`String before_cycle_model)
        && List.assoc_opt "provider" model_fields <> None
      | _ -> false)
    | _ -> false
  in
  check "rpc Pi cycle_model switches model and returns metadata"
    (rpc_success out "cycle_model" && cycle_model_ok && (Agent.config rpc_agent).Llm.model <> before_cycle_model);
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"get_available_models"}|}) in
  let models_ok =
    match rpc_field (rpc_last out) "data" with
    | Some (`Assoc fields) -> (
      match List.assoc_opt "models" fields with Some (`List (_ :: _)) -> true | _ -> false)
    | _ -> false
  in
  check "rpc Pi get_available_models lists catalog" (rpc_success out "get_available_models" && models_ok);
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"bash","command":"printf rpc && exit 7"}|}) in
  let bash_ok =
    match rpc_field (rpc_last out) "data" with
    | Some (`Assoc fields) ->
      List.assoc_opt "output" fields = Some (`String "rpc")
      && List.assoc_opt "exitCode" fields = Some (`Int 7)
    | _ -> false
  in
  check "rpc Pi bash returns BashResult shape" (rpc_success out "bash" && bash_ok && Agent.turn_count rpc_agent = 1);
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"new_session"}|}) in
  check "rpc Pi new_session attaches empty session"
    (rpc_success out "new_session" && Agent.turn_count rpc_agent = 0 && Agent.session rpc_agent <> None);
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"set_session_name","name":"rpc-name"}|}) in
  let named =
    match Agent.session rpc_agent with
    | Some s -> (Session.info_of s).Session.name = "rpc-name"
    | None -> false
  in
  check "rpc Pi set_session_name persists" (rpc_success out "set_session_name" && named);
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"type":"export_html","outputPath":"rpc-export.html"}|}) in
  check "rpc Pi export_html writes file" (rpc_success out "export_html" && Sys.file_exists "rpc-export.html");
  let switch_target = Session.create_new ~name:"rpc-switch" () in
  Session.append switch_target { Llm.role = Llm.User; content = [ Llm.Text "switch turn" ] };
  let switch_json =
    Printf.sprintf {|{"type":"switch_session","sessionPath":"%s"}|} switch_target.Session.id |> j
  in
  let out = Rpc.handle_command_for_test rpc_agent switch_json in
  let switched =
    match Agent.session rpc_agent with
    | Some s -> s.Session.id = switch_target.Session.id && Agent.turn_count rpc_agent = 1
    | None -> false
  in
  check "rpc Pi switch_session resolves id" (rpc_success out "switch_session" && switched);
  Session.close switch_target;
  let out =
    Rpc.handle_command_for_test rpc_agent
      (j {|{"id":"ui-1","type":"extension_ui_response","requestId":"ui-1","response":true}|})
  in
  check "rpc Pi extension_ui_response accepts fallback ack" (rpc_success out "extension_ui_response");
  let out = Rpc.handle_command_for_test rpc_agent (j {|{"method":"session"}|}) in
  check "rpc legacy method still emits ok" (rpc_field (rpc_last out) "type" = Some (`String "ok"));
  Option.iter Session.close (Agent.session rpc_agent);

  let cl = Session.clone_from turns in
  check "clone duplicates turns" (List.length (Session.load_turns cl.Session.path) = List.length turns);
  check "clone is a new id" (cl.Session.id <> ns.Session.id);
  let import_path = "import-source.jsonl" in
  Session.export_jsonl turns import_path;
  let import_agent = Agent.create cfg_for_reset in
  let import_msg = Commands.import_session import_agent import_path in
  check "import command reports imported session" (contains import_msg "Imported import-source.jsonl");
  check "import command loads turns into agent" (Agent.turn_count import_agent = List.length turns);
  let imported_session =
    match Agent.session import_agent with
    | Some s -> s
    | None -> failwith "import did not attach session"
  in
  check "import command creates managed session" (imported_session.Session.path <> import_path);
  check "imported managed session persists turns"
    (List.length (Session.load_turns imported_session.Session.path) = List.length turns);
  Session.close imported_session;
  let fork_msg = Commands.fork import_agent (Some ns.Session.id) in
  check "fork command forks named session" (contains fork_msg "Forked" && Agent.turn_count import_agent = List.length turns);
  let fork_current_msg = Commands.fork import_agent None in
  check "fork command defaults to current session" (contains fork_current_msg "Cloned to new session");
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  check "scoped-models reports all by default" (Commands.scoped_models None = "Scoped models: all");
  check "scoped-models sets patterns" (contains (Commands.scoped_models (Some "anthropic/*,gpt-4o")) "anthropic/*");
  check "scoped-models writes env"
    (match Sys.getenv_opt "AGENT_SCOPED_MODELS" with Some s -> contains s "gpt-4o" | None -> false);
  check "scoped-models clears patterns" (Commands.scoped_models (Some "clear") = "Scoped models cleared.");
  let _ = run "write_file" {|{"path":"CHANGELOG.md","content":"# Changelog\n\n- Added parity\n"}|} in
  check "changelog command reads file" (contains (Commands.changelog ()) "Added parity");
  check "hotkeys command mentions slash commands" (contains (Commands.hotkeys ()) "slash commands");
  Option.iter Session.close (Agent.session import_agent);
  Session.close ns;
  Session.close cl;

  Printf.printf "\n%s\n" (if !failures = 0 then "All core session/RPC tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
