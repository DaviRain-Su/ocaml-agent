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

let write_file path content =
  ignore
    (run "write_file"
       (Yojson.Safe.to_string
          (`Assoc [ ("path", `String path); ("content", `String content) ])))

let contains hay needle =
  let hay_len = String.length hay and needle_len = String.length needle in
  let rec loop i =
    i + needle_len <= hay_len
    && (String.sub hay i needle_len = needle || loop (i + 1))
  in
  needle_len = 0 || loop 0

let () =
  let dir = Filename.temp_dir "agent_extension_io_hooks_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

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
  let node_available = Sys.command "command -v node >/dev/null 2>&1" = 0 in
  write_file ".pi/extensions/io-hooks.ts"
    {|import { Type } from "typebox";
import { createLocalBashOperations, createLocalFileOperations } from "@earendil-works/pi-coding-agent";

export default function(pi) {
  pi.registerTool({
    name: "ts_greet",
    label: "TS Greet",
    description: "Greet from a Pi TypeScript extension",
    parameters: Type.Object({ name: Type.String({ description: "Name" }) }),
    async execute(_id, params) {
      return { content: [{ type: "text", text: `Hello ${params.name}` }], details: {} };
    },
  });
  pi.on("input", async (event) => {
    if (event.text === "handled") return { action: "handled" };
    if (event.text.startsWith("brief:")) return { action: "transform", text: `Respond briefly: ${event.text.slice(6).trim()}` };
  });
  pi.on("tool_call", async (event) => {
    if (event.toolName === "ts_greet") event.input.name = `${event.input.name}!`;
    if (event.toolName === "bash" && event.input.command === "blocked") return { block: true, reason: "blocked by ts" };
  });
  pi.on("tool_result", async (event) => {
    if (event.toolName === "ts_greet") return { content: [{ type: "text", text: `${event.content[0].text} hooked` }] };
  });
  pi.on("user_bash", async (event) => {
    if (event.command === "virtual") return { result: { output: `virtual ${event.excludeFromContext}`, exitCode: 7 } };
    if (event.command === "ops") return { operations: { exec: async (command, _cwd, options) => {
      options.onData(Buffer.from(`ops ${command}`));
      return { exitCode: 9 };
    } } };
    if (event.command === "localops") {
      const local = createLocalBashOperations();
      return { operations: { exec: async (_command, cwd, options) => {
        options.onData(Buffer.from("wrapped\n"));
        return local.exec("printf localops", cwd, options);
      } } };
    }
    if (event.command === "fileops") {
      const files = createLocalFileOperations();
      await files.writeFile("ops-dir/local-fileops.txt", "fileops");
      return { result: { output: await files.readFile("ops-dir/local-fileops.txt"), exitCode: 0 } };
    }
  });
}
|};

  ignore (Extensions.load ());
  check "TypeScript extension tool_call mutates input"
    ((not node_available)
     ||
     match Extensions.emit_tool_call ~tool_call_id:"call-1" ~tool_name:"ts_greet" (`Assoc [ ("name", `String "Pi") ]) with
     | Extensions.Tool_continue (`Assoc fields) -> List.assoc_opt "name" fields = Some (`String "Pi!")
     | _ -> false);
  check "TypeScript extension tool_call can block"
    ((not node_available)
     ||
     match Extensions.emit_tool_call ~tool_call_id:"call-2" ~tool_name:"bash" (`Assoc [ ("command", `String "blocked") ]) with
     | Extensions.Tool_block reason -> contains reason "blocked by ts"
     | _ -> false);
  check "TypeScript extension tool_result can replace text"
    ((not node_available)
     ||
     contains
       (Extensions.emit_tool_result ~tool_call_id:"call-3" ~tool_name:"ts_greet"
          ~input:(`Assoc [ ("name", `String "Pi") ]) "Hello Pi")
       "hooked");
  check "TypeScript extension input event transforms text"
    ((not node_available)
     ||
     match Extensions.emit_input "brief: explain pi" with
     | Extensions.Input_continue text -> text = "Respond briefly: explain pi"
     | Extensions.Input_handled -> false);
  check "TypeScript extension input event can handle text"
    ((not node_available)
     ||
     match Extensions.emit_input "handled" with
     | Extensions.Input_handled -> true
     | Extensions.Input_continue _ -> false);
  let user_bash_replace_ok, user_bash_context_ok, user_bash_hidden_ok =
    if not node_available then (true, true, true)
    else
      let user_bash_agent = Agent.create cfg_for_reset in
      let intercepted = Agent.run_user_bash user_bash_agent "virtual" in
      let replace_ok = contains intercepted "(exit 7)" && contains intercepted "virtual false" in
      let context_ok = Agent.turn_count user_bash_agent = 1 in
      let before_hidden = Agent.turn_count user_bash_agent in
      let hidden = Agent.run_user_bash ~exclude_from_context:true user_bash_agent "virtual" in
      let hidden_ok =
        contains hidden "(exit 7)" && contains hidden "virtual true" && Agent.turn_count user_bash_agent = before_hidden
      in
      (replace_ok, context_ok, hidden_ok)
  in
  check "TypeScript extension user_bash can replace result" user_bash_replace_ok;
  check "TypeScript extension user_bash records replacement context" user_bash_context_ok;
  check "TypeScript extension user_bash honors excludeFromContext" user_bash_hidden_ok;
  check "TypeScript extension user_bash can provide BashOperations"
    ((not node_available)
     ||
     let ops_agent = Agent.create cfg_for_reset in
     let result = Agent.run_user_bash ops_agent "ops" in
     contains result "(exit 9)" && contains result "ops ops");
  check "TypeScript extension createLocalBashOperations is exported"
    ((not node_available)
     ||
     let ops_agent = Agent.create cfg_for_reset in
     let result = Agent.run_user_bash ops_agent "localops" in
     contains result "(exit 0)" && contains result "wrapped" && contains result "localops");
  check "TypeScript extension createLocalFileOperations is exported"
    ((not node_available)
     ||
     let ops_agent = Agent.create cfg_for_reset in
     let result = Agent.run_user_bash ops_agent "fileops" in
     contains result "(exit 0)" && contains result "fileops");

  if !failures > 0 then exit 1
