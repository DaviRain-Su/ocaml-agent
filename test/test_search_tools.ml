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

let contains hay needle =
  try ignore (Str.search_forward (Str.regexp_string needle) hay 0); true with Not_found -> false

let () =
  let dir = Filename.temp_dir "agent_search_tools_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  (* --- grep --- *)
  let _ = run "write_file" {|{"path":"src/foo.ml","content":"let answer = 42\nlet x = 1\n"}|} in
  let _ = run "write_file" {|{"path":"src/bar.txt","content":"answer here\n"}|} in
  let r = run "grep" {|{"pattern":"answer"}|} in
  check "grep finds in .ml" (contains r "src/foo.ml:1:");
  check "grep finds in .txt" (contains r "src/bar.txt:1:");
  let r = run "grep" {|{"pattern":"answer","include":"*.ml"}|} in
  check "grep include filters" (contains r "foo.ml" && not (contains r "bar.txt"));
  let r = run "grep" {|{"pattern":"ANSWER","path":"src","glob":"*.txt","ignoreCase":true}|} in
  check "grep supports glob and ignoreCase" (contains r "bar.txt:1:" && not (contains r "foo.ml"));
  let _ = run "write_file" {|{"path":"context.txt","content":"before\nmatch one\nafter\nmiddle\nmatch two\nafter two\n"}|} in
  let r = run "grep" {|{"pattern":"match","path":"context.txt","context":1,"limit":1}|} in
  check "grep supports context and limit"
    (contains r "context.txt-1- before" && contains r "context.txt:2: match one"
     && contains r "1 matches limit reached" && not (contains r "match two"));
  let r = run "grep" {|{"pattern":"--pre=/tmp/should-not-run","path":"src","literal":true}|} in
  check "grep treats flag-like literal patterns as text" (r = "No matches found");
  let r = run "grep" {|{"pattern":"zzz_nomatch"}|} in
  check "grep no match" (r = "No matches found");

  (* --- find --- *)
  let _ = run "write_file" {|{"path":"src/.secret/hidden.txt","content":"hidden"}|} in
  let _ = run "write_file" {|{"path":"src/ignored.txt","content":"ignored"}|} in
  let _ = run "write_file" {|{"path":"src/.gitignore","content":"ignored.txt\n"}|} in
  let r = run "find" {|{"pattern":"*.ml"}|} in
  check "find by basename glob" (contains r "src/foo.ml" && not (contains r "bar.txt"));
  let r = run "find" {|{"pattern":"src/**/*.txt"}|} in
  check "find by path glob" (contains r "src/bar.txt");
  let r = run "find" {|{"pattern":"**/*.txt","path":"src"}|} in
  check "find includes hidden files and respects .gitignore"
    (contains r ".secret/hidden.txt" && not (contains r "ignored.txt"));
  let r = run "find" {|{"pattern":"**/*.txt","path":"src","limit":1}|} in
  check "find supports limit" (contains r "1 results limit reached");
  let r = run "find" {|{"pattern":"*.nope"}|} in
  check "find no match" (r = "No files found matching pattern");

  Printf.printf "\n%s\n" (if !failures = 0 then "All search tool tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
