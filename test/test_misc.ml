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

let () =
  let dir = Filename.temp_dir "agent_misc_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  (* --- autocomplete --- *)
  let _ = run "write_file" {|{"path":"src/foo.ml","content":""}|} in
  check "complete common_prefix" (Complete.common_prefix [ "abc"; "abd"; "abx" ] = "ab");
  check "complete token slash whole" (Complete.token_of "/mo" = (0, "/mo"));
  check "complete token last word" (Complete.token_of "read foo" = (5, "foo"));
  check "complete slash command" (List.mem "/model" (Complete.candidates "/mo"));
  check "complete slash multi" (List.mem "/session" (Complete.candidates "/se") && List.mem "/sessions" (Complete.candidates "/se"));
  check "complete import command" (List.mem "/import" (Complete.candidates "/im"));
  check "complete new builtin commands"
    (List.mem "/scoped-models" (Complete.candidates "/sc") && List.mem "/hotkeys" (Complete.candidates "/ho"));
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
  let surface_state = Tui.create_extension_surface_state () in
  Tui.apply_extension_surfaces surface_state
    [ j {|{"kind":"status","key":"sync","text":"ready"}|};
      j {|{"kind":"widget","key":"above","lines":["above widget"],"options":{"placement":"aboveEditor"}}|};
      j {|{"kind":"widget","key":"below","lines":["below widget"],"options":{"placement":"belowEditor"}}|};
      j {|{"kind":"header","lines":["native header"]}|};
      j {|{"kind":"footer","lines":["native footer"]}|};
      j {|{"kind":"editor_component","lines":["native editor"]}|} ];
  let top, bottom, editor = Tui.extension_surface_lines surface_state in
  check "extension surfaces render native TUI bands"
    (List.mem "native header" top && List.mem "above widget" top && List.mem "below widget" bottom
     && List.mem "native footer" bottom && editor = [ "native editor" ]);
  Tui.apply_extension_surfaces surface_state
    [ j {|{"kind":"widget","key":"above","action":"clear"}|};
      j {|{"kind":"footer","action":"clear"}|} ];
  let top, bottom, _ = Tui.extension_surface_lines surface_state in
  check "extension surfaces clear widgets and fall back to statuses"
    ((not (List.mem "above widget" top)) && List.mem "sync: ready" bottom);
  let working_state = Tui.create_extension_surface_state () in
  Tui.apply_extension_surfaces working_state
    [ j {|{"kind":"working_message","message":"custom work"}|};
      j {|{"kind":"working_indicator","options":{"frames":["A","B"],"intervalMs":1000}}|} ];
  check "extension working state customizes active status"
    (Tui.extension_working_status ~now:1.0 working_state ~spin:0 ~turn_start:0.0 = Some "B custom work 1s");
  Tui.apply_extension_surfaces working_state [ j {|{"kind":"working_visible","visible":false}|} ];
  check "extension working visible hides active status"
    (Tui.extension_working_status ~now:2.0 working_state ~spin:0 ~turn_start:0.0 = None);
  Tui.apply_extension_surfaces working_state
    [ j {|{"kind":"working_visible","visible":true}|};
      j {|{"kind":"working_indicator","options":{"frames":[]}}|};
      j {|{"kind":"hidden_thinking_label","label":"condensed"}|} ];
  check "extension working indicator can hide frame"
    (Tui.extension_working_status ~now:2.0 working_state ~spin:0 ~turn_start:0.0 = Some "custom work 2s"
     && working_state.Tui.hidden_thinking_label = Some "condensed");
  Tui.apply_extension_surfaces working_state
    [ j {|{"kind":"working_message","message":null}|}; j {|{"kind":"working_indicator","options":null}|};
      j {|{"kind":"hidden_thinking_label","label":null}|} ];
  check "extension working state resets to defaults"
    (working_state.Tui.working_message = None && working_state.Tui.working_indicator = None
     && working_state.Tui.hidden_thinking_label = None);
  check "input_window keeps cursor visible" (Tui.input_window ~height:10 ~menu_rows:0 ~cursor_row:20 ~total_rows:30 = (14, 7));
  check "input_window leaves body room" (snd (Tui.input_window ~height:4 ~menu_rows:0 ~cursor_row:3 ~total_rows:10) = 1);

  Printf.printf "\n%s\n" (if !failures = 0 then "All misc tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
