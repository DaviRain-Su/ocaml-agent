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

let () =
  (* Work in an isolated temp directory. *)
  let dir = Filename.temp_dir "agent_test" "" in
  Sys.chdir dir;

  let r = run "write_file" {|{"path":"sub/a.txt","content":"hello\nworld\n"}|} in
  check "write_file reports bytes" (Str.string_match (Str.regexp "Wrote ") r 0);
  check "write_file created file" (Sys.file_exists "sub/a.txt");

  let r = run "read_file" {|{"path":"sub/a.txt"}|} in
  check "read_file roundtrip" (r = "hello\nworld\n");

  let _ = run "edit_file" {|{"path":"sub/a.txt","old_str":"world","new_str":"OCaml"}|} in
  let r = run "read_file" {|{"path":"sub/a.txt"}|} in
  check "edit_file replaced" (r = "hello\nOCaml\n");

  let r = run "edit_file" {|{"path":"sub/a.txt","old_str":"nope","new_str":"x"}|} in
  check "edit_file missing old_str" (Str.string_match (Str.regexp "Error:") r 0);

  let r = run "list_dir" {|{"path":"sub"}|} in
  check "list_dir lists file" (r = "a.txt");

  let r = run "read_file" {|{"path":"does_not_exist"}|} in
  check "read_file error" (Str.string_match (Str.regexp "Error:") r 0);

  let contains hay needle =
    try ignore (Str.search_forward (Str.regexp_string needle) hay 0); true
    with Not_found -> false
  in
  let r = run "run_bash" {|{"command":"echo hi && exit 3"}|} in
  check "run_bash captures output" (contains r "hi");
  check "run_bash reports exit code" (contains r "(exit 3)");

  (* --- Llm turn <-> JSON round-trip --- *)
  let turns =
    [ { Llm.role = User; content = [ Llm.Text "hello" ] };
      { Llm.role = Assistant;
        content =
          [ Llm.Text "thinking";
            Llm.Tool_use { id = "t1"; name = "read_file"; input = j {|{"path":"a.txt"}|} } ] };
      { Llm.role = User; content = [ Llm.Tool_result { id = "t1"; content = "file body" } ] } ]
  in
  let roundtrip = List.map (fun t -> Llm.turn_of_json (Llm.turn_to_json t)) turns in
  check "turn json round-trips"
    (List.map Llm.turn_to_json turns = List.map Llm.turn_to_json roundtrip);

  (* --- Session save/load --- *)
  let spath = "session.jsonl" in
  let s = Session.create spath in
  List.iter (Session.append s) turns;
  Session.close s;
  let loaded = Session.load spath in
  check "session reloads same turn count" (List.length loaded = List.length turns);
  check "session preserves content"
    (List.map Llm.turn_to_json loaded = List.map Llm.turn_to_json turns);

  (* --- system prompt context injection + identity --- *)
  let _ = run "write_file" {|{"path":"AGENTS.md","content":"PROJECT_RULE_XYZ"}|} in
  let cfg =
    { Llm.provider = Llm.Openai;
      base_url = "https://api.deepseek.com";
      api_key = "sk-test";
      model = "deepseek-chat";
      max_tokens = 4096;
      extra_headers = [] }
  in
  let prompt = Agent.build_system_prompt cfg in
  check "system prompt injects AGENTS.md" (contains prompt "PROJECT_RULE_XYZ");
  check "system prompt includes cwd" (contains prompt "Current working directory:");
  check "system prompt includes date" (contains prompt "Current date:");
  check "system prompt states model identity" (contains prompt "deepseek-chat");
  check "system prompt states provider identity" (contains prompt "OpenAI-compatible");

  Printf.printf "\n%s\n" (if !failures = 0 then "All tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
