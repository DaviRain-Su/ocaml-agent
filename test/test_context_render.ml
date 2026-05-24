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
  let dir = Filename.temp_dir "agent_context_render_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  (* --- system prompt context injection + identity --- *)
  let _ = run "write_file" {|{"path":"AGENTS.md","content":"PROJECT_RULE_XYZ"}|} in
  let _ = run "write_file" {|{"path":"CLAUDE.md","content":"PROJECT_CLAUDE_RULE_XYZ"}|} in
  let _ =
    run "write_file"
      (Printf.sprintf {|{"path":"%s","content":"GLOBAL_RULE_XYZ"}|}
         (Filename.concat (Sys.getenv "PI_CODING_AGENT_DIR") "AGENTS.md"))
  in
  let _ =
    run "write_file"
      (Printf.sprintf {|{"path":"%s","content":"GLOBAL_CLAUDE_RULE_XYZ"}|}
         (Filename.concat (Sys.getenv "PI_CODING_AGENT_DIR") "CLAUDE.md"))
  in
  let cfg =
    { Llm.provider = Llm.Openai;
      base_url = "https://api.deepseek.com";
      api_key = "sk-test";
      model = "deepseek-v4-pro";
      max_tokens = 4096;
      extra_headers = [];
      runtime = None;
      thinking = "off" }
  in
  let prompt = Agent.build_system_prompt cfg in
  check "system prompt injects AGENTS.md" (contains prompt "PROJECT_RULE_XYZ");
  check "system prompt injects CLAUDE.md" (contains prompt "PROJECT_CLAUDE_RULE_XYZ");
  check "system prompt injects global AGENTS.md" (contains prompt "GLOBAL_RULE_XYZ");
  check "system prompt injects global CLAUDE.md" (contains prompt "GLOBAL_CLAUDE_RULE_XYZ");
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
  Tools.write_file_contents "tiny.png" "\137PNG\r\n\026\n";
  let image_args = Mentions.expand_file_args_rich [ "tiny.png"; "mention.txt" ] in
  check "file args attach image blocks"
    (contains image_args.text "<file name=\"tiny.png\">[Image: image/png]</file>"
     && List.length image_args.images = 1
     && (List.hd image_args.images).Mentions.mime_type = "image/png"
     && (List.hd image_args.images).Mentions.data = "iVBORw0KGgo=");
  let image_mention = Mentions.expand_rich "Look at @tiny.png" in
  check "mentions attach image files"
    (contains image_mention.text "[Image: image/png]" && List.length image_mention.images = 1);

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

  Printf.printf "\n%s\n" (if !failures = 0 then "All context/render tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
