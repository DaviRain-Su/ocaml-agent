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
  let dir = Filename.temp_dir "agent_file_tools_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

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

  let _ = run "write" {|{"file_path":"alias/path.txt","content":"alias body"}|} in
  let r = run "read" {|{"file_path":"alias/path.txt"}|} in
  check "Pi file_path aliases work for read/write" (r = "alias body");

  let _ = run "write_file" {|{"path":"pi-edit.txt","content":"one\ntwo\nthree\n"}|} in
  let r =
    run "edit"
      {|{"path":"pi-edit.txt","edits":[{"oldText":"one\n","newText":"ONE\n"}],"oldText":"three\n","newText":"THREE\n"}|}
  in
  check "edit accepts Pi oldText/newText and appends legacy top-level edit"
    (contains0 r "Successfully replaced 2 blocks" && contains0 r "2 changes");
  let r = run "read_file" {|{"path":"pi-edit.txt"}|} in
  check "edit Pi fields applied" (r = "ONE\ntwo\nTHREE\n");

  let _ = run "write_file" {|{"path":"string-edits.txt","content":"a\nb\n"}|} in
  let _ = run "edit_file" {|{"path":"string-edits.txt","edits":"[{\"oldText\":\"a\\n\",\"newText\":\"A\\n\"}]"}|} in
  let r = run "read_file" {|{"path":"string-edits.txt"}|} in
  check "edit parses JSON-string edits" (r = "A\nb\n");

  let _ = run "write_file" {|{"path":"original-match.txt","content":"foo\nbar\nbaz\n"}|} in
  let _ =
    run "edit"
      {|{"path":"original-match.txt","edits":[{"oldText":"foo\n","newText":"foo bar\n"},{"oldText":"bar\n","newText":"BAR\n"}]}|}
  in
  let r = run "read_file" {|{"path":"original-match.txt"}|} in
  check "edit multi matches against original file" (r = "foo bar\nBAR\nbaz\n");

  let _ = run "write_file" {|{"path":"dups.txt","content":"foo foo foo"}|} in
  let r = run "edit" {|{"path":"dups.txt","edits":[{"oldText":"foo","newText":"bar"}]}|} in
  check "edit rejects duplicate oldText" (contains0 r "Found 3 occurrences");

  let _ = run "write_file" {|{"path":"overlap.txt","content":"one\ntwo\nthree\n"}|} in
  let r =
    run "edit"
      {|{"path":"overlap.txt","edits":[{"oldText":"one\ntwo\n","newText":"ONE\nTWO\n"},{"oldText":"two\nthree\n","newText":"TWO\nTHREE\n"}]}|}
  in
  check "edit rejects overlapping multi edits" (contains0 r "overlap");

  let bom = "\239\187\191" in
  Tools.write_file_contents "bom-crlf.txt" (bom ^ "first\r\nsecond\r\nthird\r\n");
  let _ = run "edit" {|{"path":"bom-crlf.txt","edits":[{"oldText":"second\n","newText":"SECOND\n"}]}|} in
  let r = Tools.read_file_contents "bom-crlf.txt" in
  check "edit preserves BOM and CRLF" (r = bom ^ "first\r\nSECOND\r\nthird\r\n");

  Tools.write_file_contents "fuzzy-ws.txt" "line one   \nline two  \nline three\n";
  let _ = run "edit" {|{"path":"fuzzy-ws.txt","edits":[{"oldText":"line one\nline two\n","newText":"replaced\n"}]}|} in
  let r = run "read_file" {|{"path":"fuzzy-ws.txt"}|} in
  check "edit fuzzy matches trailing whitespace" (r = "replaced\nline three\n");

  Tools.write_file_contents "fuzzy-unicode.txt"
    ("\239\188\161\239\188\162\239\188\163\239\188\145\239\188\146\239\188\147\n"
     ^ "console.log(\226\128\152hello\226\128\153);\n");
  let _ =
    run "edit"
      {|{"path":"fuzzy-unicode.txt","edits":[{"oldText":"ABC123\n","newText":"XYZ789\n"},{"oldText":"console.log('hello');\n","newText":"console.log('world');\n"}]}|}
  in
  let r = run "read_file" {|{"path":"fuzzy-unicode.txt"}|} in
  check "edit fuzzy matches Unicode compatibility variants" (r = "XYZ789\nconsole.log('world');\n");

  let r = run "list_dir" {|{"path":"sub"}|} in
  check "list_dir lists file" (r = "a.txt");
  let _ = run "write_file" {|{"path":"sub/.hidden","content":"secret"}|} in
  let _ = run "write_file" {|{"path":"sub/nested/file.txt","content":"nested"}|} in
  let r = run "ls" {|{"path":"sub","limit":2}|} in
  check "ls supports dotfiles, directories, and limit"
    (contains0 r ".hidden" && contains0 r "a.txt" && contains0 r "2 entries limit reached");

  let r = run "read_file" {|{"path":"does_not_exist"}|} in
  check "read_file error" (Str.string_match (Str.regexp "Error:") r 0);

  let contains hay needle =
    try ignore (Str.search_forward (Str.regexp_string needle) hay 0); true
    with Not_found -> false
  in
  let large_lines = List.init 2500 (fun i -> Printf.sprintf "Line %d" (i + 1)) in
  Tools.write_file_contents "large-read.txt" (String.concat "\n" large_lines);
  let r = run "read_file" {|{"path":"large-read.txt"}|} in
  check "read_file truncates large files by line"
    (contains r "Line 1" && contains r "Line 2000" && not (contains r "Line 2001")
     && contains r "[Showing lines 1-2000 of 2500. Use offset=2001 to continue.]");
  let r = run "read_file" {|{"path":"large-read.txt","offset":2001,"limit":3}|} in
  check "read_file supports offset and limit"
    (not (contains r "Line 2000") && contains r "Line 2001" && contains r "Line 2003"
     && not (contains r "Line 2004"));
  let r = run "read_file" {|{"path":"large-read.txt","limit":10}|} in
  check "read_file limit includes continuation"
    (contains r "Line 10" && not (contains r "Line 11") && contains r "[2490 more lines in file. Use offset=11 to continue.]");
  let r = run "read_file" {|{"path":"large-read.txt","offset":9999}|} in
  check "read_file reports offset beyond EOF" (contains r "Offset 9999 is beyond end of file");
  let byte_lines = List.init 500 (fun i -> Printf.sprintf "Line %d: %s" (i + 1) (String.make 200 'x')) in
  Tools.write_file_contents "large-bytes.txt" (String.concat "\n" byte_lines);
  let r = run "read_file" {|{"path":"large-bytes.txt"}|} in
  check "read_file truncates large files by bytes" (contains r "Line 1:" && contains r "50KB limit" && contains r "Use offset=");
  let r = run "run_bash" {|{"command":"echo hi && exit 3"}|} in
  check "run_bash captures output" (contains r "hi");
  check "run_bash reports exit code" (contains r "(exit 3)");
  let r = run "run_bash" {|{"command":"sleep 2","timeout":1}|} in
  check "run_bash honors timeout argument" (contains r "(exit 124)" && contains r "timed out");
  let code, out = Tools.run_process ~timeout_s:1 "sleep 2" in
  check "run_process enforces timeout" (code = 124 && contains out "timed out");
  let code, out = Tools.run_process ~stdin_data:{|{"x":1}|} "cat" in
  check "run_process passes stdin data" (code = 0 && out = {|{"x":1}|});
  Unix.putenv "AGENT_SHELL_COMMAND_PREFIX" "export OCAML_AGENT_PREFIXED=ok";
  let code, out = Tools.run_process {|printf "$OCAML_AGENT_PREFIXED"|} in
  check "run_process ignores shell prefix by default" (code = 0 && out = "");
  let code, out = Tools.run_process ~use_shell_settings:true {|printf "$OCAML_AGENT_PREFIXED"|} in
  check "run_process applies shellCommandPrefix for bash mode" (code = 0 && out = "ok");
  Unix.putenv "AGENT_SHELL_COMMAND_PREFIX" "";
  Unix.putenv "AGENT_SHELL_PATH" "/bin/echo";
  let code, out = Tools.run_process ~use_shell_settings:true "shell-path-marker" in
  check "run_process uses configured shellPath" (code = 0 && contains out "shell-path-marker");
  Unix.putenv "AGENT_SHELL_PATH" "";

  Printf.printf "\n%s\n" (if !failures = 0 then "All file tool tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
