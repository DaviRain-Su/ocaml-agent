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

  let contains0 hay needle =
    try ignore (Str.search_forward (Str.regexp_string needle) hay 0); true with Not_found -> false
  in
  let r = run "edit_file" {|{"path":"sub/a.txt","old_str":"hello","new_str":"hi"}|} in
  check "edit_file returns diff" (contains0 r "-hello" && contains0 r "+hi");
  let _ = run "write_file" {|{"path":"multi.txt","content":"a\nb\nc\n"}|} in
  let r = run "edit_file" {|{"path":"multi.txt","edits":[{"old_str":"a","new_str":"A"},{"old_str":"c","new_str":"C"}]}|} in
  check "edit_file multi reports 2 changes" (contains0 r "2 changes");
  let r2 = run "read_file" {|{"path":"multi.txt"}|} in
  check "edit_file multi applied both" (r2 = "A\nb\nC\n");

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

  (* --- grep --- *)
  let _ = run "write_file" {|{"path":"src/foo.ml","content":"let answer = 42\nlet x = 1\n"}|} in
  let _ = run "write_file" {|{"path":"src/bar.txt","content":"answer here\n"}|} in
  let r = run "grep" {|{"pattern":"answer"}|} in
  check "grep finds in .ml" (contains r "src/foo.ml:1:");
  check "grep finds in .txt" (contains r "src/bar.txt:1:");
  let r = run "grep" {|{"pattern":"answer","include":"*.ml"}|} in
  check "grep include filters" (contains r "foo.ml" && not (contains r "bar.txt"));
  let r = run "grep" {|{"pattern":"zzz_nomatch"}|} in
  check "grep no match" (r = "No matches.");

  (* --- find --- *)
  let r = run "find" {|{"pattern":"*.ml"}|} in
  check "find by basename glob" (contains r "src/foo.ml" && not (contains r "bar.txt"));
  let r = run "find" {|{"pattern":"src/**/*.txt"}|} in
  check "find by path glob" (contains r "src/bar.txt");
  let r = run "find" {|{"pattern":"*.nope"}|} in
  check "find no match" (r = "No files found.");

  (* --- Llm turn <-> JSON round-trip --- *)
  let turns =
    [ { Llm.role = User; content = [ Llm.Text "hello" ] };
      { Llm.role = Assistant;
        content =
          [ Llm.Thinking { text = "let me think"; signature = "sig123" };
            Llm.Text "thinking";
            Llm.Tool_use { id = "t1"; name = "read_file"; input = j {|{"path":"a.txt"}|} } ] };
      { Llm.role = User; content = [ Llm.Tool_result { id = "t1"; content = "file body" } ] } ]
  in
  let roundtrip = List.map (fun t -> Llm.turn_of_json (Llm.turn_to_json t)) turns in
  check "turn json round-trips"
    (List.map Llm.turn_to_json turns = List.map Llm.turn_to_json roundtrip);

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
  let cl = Session.clone_from turns in
  check "clone duplicates turns" (List.length (Session.load_turns cl.Session.path) = List.length turns);
  check "clone is a new id" (cl.Session.id <> ns.Session.id);
  Session.close ns;
  Session.close cl;

  (* --- system prompt context injection + identity --- *)
  let _ = run "write_file" {|{"path":"AGENTS.md","content":"PROJECT_RULE_XYZ"}|} in
  let cfg =
    { Llm.provider = Llm.Openai;
      base_url = "https://api.deepseek.com";
      api_key = "sk-test";
      model = "deepseek-chat";
      max_tokens = 4096;
      extra_headers = [];
      thinking = "off" }
  in
  let prompt = Agent.build_system_prompt cfg in
  check "system prompt injects AGENTS.md" (contains prompt "PROJECT_RULE_XYZ");
  check "system prompt includes cwd" (contains prompt "Current working directory:");
  check "system prompt includes date" (contains prompt "Current date:");
  check "system prompt states model identity" (contains prompt "deepseek-chat");
  check "system prompt states provider identity" (contains prompt "OpenAI-compatible");

  (* --- Render --- *)
  let has hay needle =
    try ignore (Str.search_forward (Str.regexp_string needle) hay 0); true with Not_found -> false
  in
  let styled, _ = Render.render_line ~in_code:false "# Title" in
  check "render header bold + no hash" (has styled "Title" && not (has styled "#"));
  let _, in_code = Render.render_line ~in_code:false "```ocaml" in
  check "render code fence toggles" in_code;
  check "render inline bold" (has (Render.style_inline "a **b** c") "\027[1m");
  check "render inline code" (has (Render.style_inline "use `x` here") "\027[36m");
  let tr = Render.tool_result "-old\n+new\n@@ x" in
  check "tool_result colors diff" (has tr "\027[31m" && has tr "\027[32m");

  (* --- skills --- *)
  let _ =
    run "write_file"
      {|{"path":".ocaml-agent/skills/deploy.md","content":"---\nname: deploy\ndescription: How to deploy the app\n---\nDetailed steps."}|}
  in
  let _ =
    run "write_file"
      {|{"path":".ocaml-agent/skills/hidden.md","content":"---\nname: hidden\ndescription: nope\ndisable-model-invocation: true\n---\nx"}|}
  in
  let skills = Skills.discover () in
  check "skills discovered" (List.exists (fun (s : Skills.t) -> s.name = "deploy") skills);
  check "skills honor disable flag" (not (List.exists (fun (s : Skills.t) -> s.name = "hidden") skills));
  let sf = Skills.format skills in
  check "skills format has name + location"
    (contains0 sf "deploy" && contains0 sf ".ocaml-agent/skills/deploy.md");

  (* --- task tool registered --- *)
  check "task tool present" (Tools.find "task" <> None);

  Printf.printf "\n%s\n" (if !failures = 0 then "All tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
