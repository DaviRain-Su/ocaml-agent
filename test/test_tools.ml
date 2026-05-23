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
  let r2 = run "read_file" {|{"path":"sub/a.txt"}|} in
  check "edit_file atomic: no partial change on failure" (r2 = "hello\nOCaml\n");

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

  (* Test atomicity: second edit invalid should leave file unchanged *)
  let _ = run "write_file" {|{"path":"atomic.txt","content":"x\ny\nz\n"}|} in
  let r = run "edit_file" {|{"path":"atomic.txt","edits":[{"old_str":"x","new_str":"X"},{"old_str":"invalid","new_str":"Y"}]}|} in
  check "edit_file atomic multi fails" (Str.string_match (Str.regexp "Error:") r 0);
  let r2 = run "read_file" {|{"path":"atomic.txt"}|} in
  check "edit_file atomic multi no partial" (r2 = "x\ny\nz\n");

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
  let code, out = Tools.run_process ~timeout_s:1 "sleep 2" in
  check "run_process enforces timeout" (code = 124 && contains out "timed out");
  let code, out = Tools.run_process ~stdin_data:{|{"x":1}|} "cat" in
  check "run_process passes stdin data" (code = 0 && out = {|{"x":1}|});

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
      model = "deepseek-v4-pro";
      max_tokens = 4096;
      extra_headers = [];
      thinking = "off" }
  in
  let prompt = Agent.build_system_prompt cfg in
  check "system prompt injects AGENTS.md" (contains prompt "PROJECT_RULE_XYZ");
  check "system prompt includes cwd" (contains prompt "Current working directory:");
  check "system prompt includes date" (contains prompt "Current date:");
  check "system prompt states model identity" (contains prompt "deepseek-v4-pro");
  check "system prompt states provider identity" (contains prompt "OpenAI-compatible");
  Unix.putenv "AGENT_NO_CONTEXT_FILES" "1";
  let no_context_prompt = Agent.build_system_prompt cfg in
  check "system prompt can disable context files" (not (contains no_context_prompt "PROJECT_RULE_XYZ"));
  Unix.putenv "AGENT_NO_CONTEXT_FILES" "";
  let _ = run "write_file" {|{"path":"base_prompt.md","content":"CUSTOM_BASE_PROMPT"}|} in
  let _ = run "write_file" {|{"path":"append_prompt.md","content":"APPENDED_PROMPT"}|} in
  Unix.putenv "AGENT_SYSTEM_PROMPT" "base_prompt.md";
  Unix.putenv "AGENT_APPEND_SYSTEM_PROMPT" (Agent.join_prompt_inputs [ "append_prompt.md"; "INLINE_APPEND" ]);
  let custom_prompt = Agent.build_system_prompt cfg in
  check "system prompt can be replaced from file" (contains custom_prompt "CUSTOM_BASE_PROMPT");
  check "system prompt append reads files and text"
    (contains custom_prompt "APPENDED_PROMPT" && contains custom_prompt "INLINE_APPEND");
  Unix.putenv "AGENT_SYSTEM_PROMPT" "";
  Unix.putenv "AGENT_APPEND_SYSTEM_PROMPT" "";
  let reload_agent = Agent.create cfg in
  let before_reload = Agent.system_prompt reload_agent in
  let _ = run "write_file" {|{"path":"AGENTS.md","content":"PROJECT_RULE_AFTER_RELOAD"}|} in
  Agent.reload_system_prompt reload_agent;
  check "reload command target can rebuild system prompt"
    (contains (Agent.system_prompt reload_agent) "PROJECT_RULE_AFTER_RELOAD"
     && not (before_reload = Agent.system_prompt reload_agent));

  (* --- @file mentions / CLI file args --- *)
  let _ = run "write_file" {|{"path":"mention.txt","content":"MENTION_BODY"}|} in
  let expanded = Mentions.expand "Review @mention.txt." in
  check "mentions expand file references" (contains expanded "<file name=\"mention.txt\">" && contains expanded "MENTION_BODY");
  check "file args expand as file blocks" (contains (Mentions.expand_file_args [ "mention.txt" ]) "MENTION_BODY");

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
  let _ =
    run "write_file"
      {|{"path":"extra_skill.md","content":"---\nname: extra\ndescription: Loaded explicitly\n---\nExtra skill."}|}
  in
  Unix.putenv "AGENT_NO_SKILLS" "1";
  Unix.putenv "AGENT_SKILL_PATHS" "extra_skill.md";
  let only_extra = Skills.discover () in
  check "skill CLI path works when discovery disabled"
    (List.exists (fun (s : Skills.t) -> s.name = "extra") only_extra
     && not (List.exists (fun (s : Skills.t) -> s.name = "deploy") only_extra));
  Unix.putenv "AGENT_NO_SKILLS" "";
  Unix.putenv "AGENT_SKILL_PATHS" "";

  (* --- prompt templates --- *)
  let _ =
    run "write_file"
      {|{"path":".ocaml-agent/prompts/component.md","content":"---\ndescription: Create a component\nargument-hint: <name> [features]\n---\nBuild component $1 with: ${@:2}\nAll: $ARGUMENTS"}|}
  in
  let prompts = Prompts.discover () in
  check "prompt templates discovered" (List.exists (fun (p : Prompts.t) -> p.name = "component") prompts);
  check "prompt templates appear in completion" (List.mem_assoc "/component" (Complete.menu "/co"));
  let expanded = Prompts.expand_command {|/component Button "click handler" disabled|} in
  check "prompt template expands positional and rest args"
    (expanded = Some "Build component Button with: click handler disabled\nAll: Button click handler disabled");
  let _ =
    run "write_file"
      {|{"path":"extra_prompt.md","content":"---\ndescription: Extra prompt\n---\nExtra ${@:2:1} / $@"}|}
  in
  Unix.putenv "AGENT_NO_PROMPT_TEMPLATES" "1";
  Unix.putenv "AGENT_PROMPT_TEMPLATE_PATHS" "extra_prompt.md";
  check "prompt template CLI path works when discovery disabled"
    (Prompts.expand_command "/extra_prompt first second third" = Some "Extra second / first second third");
  Unix.putenv "AGENT_NO_PROMPT_TEMPLATES" "";
  Unix.putenv "AGENT_PROMPT_TEMPLATE_PATHS" "";

  (* --- task tool registered --- *)
  check "task tool present" (Tools.find "task" <> None);
  check "Pi tool aliases resolve" (Tools.find "bash" <> None && Tools.canonical_name "ls" = "list_dir");
  let readonly_schema = Yojson.Safe.to_string (`List (Tools.openai_schemas ~allowed:[ "read"; "grep"; "ls" ] ())) in
  check "tool schemas honor Pi allowlist aliases"
    (contains0 readonly_schema "read_file" && contains0 readonly_schema "list_dir" && not (contains0 readonly_schema "write_file"));

  (* --- model catalog --- *)
  check "model context window lookup" (Models.context_window "deepseek-v4-pro" = Some 1000000);
  check "model unknown -> None" (Models.context_window "no-such-model" = None);
  check "model list filters" (List.for_all (fun (e : Models.entry) -> contains0 e.Models.id "glm" || contains0 e.Models.provider "zai") (Models.list ~pat:"zai" ()));
  check "model list nonempty" (Models.list () <> []);

  (* --- extensions: custom tool from manifest --- *)
  let oc = open_out ".ocaml-agent/tools.json" in
  output_string oc
    {|{"tools":[{"name":"echoizer","description":"echo back","parameters":{"type":"object","properties":{}},"command":"cat"}]}|};
  close_out oc;
  let names = Extensions.load () in
  check "extension registered" (List.mem "echoizer" names);
  check "extension findable" (Tools.find "echoizer" <> None);
  (match Tools.find "echoizer" with
   | Some t -> check "extension requires approval" t.Tools.requires_approval
   | None -> check "extension requires approval" false);
  (match Tools.find "echoizer" with
   | Some t -> check "extension executes via subprocess" (contains0 (t.execute (j {|{"x":1}|})) "\"x\":1")
   | None -> check "extension executes via subprocess" false);
  check "extension in schemas"
    (contains0 (Yojson.Safe.to_string (`List (Tools.openai_schemas ()))) "echoizer");
  let oc = open_out ".ocaml-agent/tools.json" in
  output_string oc
    {|{"tools":[{"name":"run_bash","description":"override","parameters":{"type":"object","properties":{}},"command":"cat"}]}|};
  close_out oc;
  let names = Extensions.load () in
  check "extension cannot override builtin" (not (List.mem "run_bash" names));
  (match Tools.find "run_bash" with
   | Some t -> check "builtin run_bash still requires approval" t.Tools.requires_approval
   | None -> check "builtin run_bash still requires approval" false);

  (* --- autocomplete --- *)
  check "complete common_prefix" (Complete.common_prefix [ "abc"; "abd"; "abx" ] = "ab");
  check "complete token slash whole" (Complete.token_of "/mo" = (0, "/mo"));
  check "complete token last word" (Complete.token_of "read foo" = (5, "foo"));
  check "complete slash command" (List.mem "/model" (Complete.candidates "/mo"));
  check "complete slash multi" (List.mem "/session" (Complete.candidates "/se") && List.mem "/sessions" (Complete.candidates "/se"));
  check "complete path token" (List.mem "src/foo.ml" (Complete.candidates "read src/f"));
  check "menu matches slash prefix" (List.mem_assoc "/session" (Complete.menu "/se") && List.mem_assoc "/sessions" (Complete.menu "/se"));
  check "menu empty for non-slash" (Complete.menu "hello" = []);
  check "menu empty after space" (Complete.menu "/model deepseek" = []);

  (* --- @file mentions --- *)
  let _ = run "write_file" {|{"path":"ment.txt","content":"MENTION_BODY_42"}|} in
  let expanded = Mentions.expand "please read @ment.txt now" in
  check "mention expands existing file" (contains0 expanded "MENTION_BODY_42" && contains0 expanded "@ment.txt");
  check "mention leaves text without files" (Mentions.expand "no files here @nope.xyz" = "no files here @nope.xyz");
  check "mention strips trailing punctuation" (contains0 (Mentions.expand "see @ment.txt.") "MENTION_BODY_42");

  (* --- keymap --- *)
  check "keymap parse ctrl" (Keymap.parse_spec "ctrl+p" = Some ('p', true));
  check "keymap parse plain" (Keymap.parse_spec "s" = Some ('s', false));
  check "keymap action name" (Keymap.action_of_string "quit" = Some Keymap.Quit);
  check "keymap default has picker" (Keymap.lookup Keymap.default ('p', true) = Some Keymap.Model_picker);

  (* --- veldt_init --- *)
  let veldt_dir = Filename.concat dir "veldt-test" in
  let r = run "veldt_init" (Printf.sprintf {|{"path":"%s","lang":"lua"}|} veldt_dir) in
  check "veldt_init succeeds" (Str.string_match (Str.regexp "Initialized Veldt project") r 0);
  check "veldt_init created bin/main.ml" (Sys.file_exists (Filename.concat veldt_dir "bin/main.ml"));
  check "veldt_init created lib/lang.ml" (Sys.file_exists (Filename.concat veldt_dir "lib/lang.ml"));
  check "veldt_init created compile.sh" (Sys.file_exists (Filename.concat veldt_dir "compile.sh"));
  let main_content = run "read_file" (Printf.sprintf {|{"path":"%s"}|} (Filename.concat veldt_dir "bin/main.ml")) in
  check "veldt_init stub mentions lua" (contains0 main_content "lua");

  (* --- TUI inline styling helpers --- *)
  let seg_text segs = String.concat "" (List.map snd segs) in
  check "style_inline strips markers" (seg_text (Tui.style_inline Notty.A.empty "a **b** `c`") = "a b c");
  let dls = Tui.wrap_segs 3 [ (Notty.A.empty, "abcdef") ] in
  check "wrap_segs splits to width" (List.length dls = 2);
  check "wrap_segs preserves text" (String.concat "" (List.map seg_text dls) = "abcdef");
  check "input_window keeps cursor visible" (Tui.input_window ~height:10 ~menu_rows:0 ~cursor_row:20 ~total_rows:30 = (14, 7));
  check "input_window leaves body room" (snd (Tui.input_window ~height:4 ~menu_rows:0 ~cursor_row:3 ~total_rows:10) = 1);

  Printf.printf "\n%s\n" (if !failures = 0 then "All tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
