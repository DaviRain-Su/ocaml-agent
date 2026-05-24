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
  let dir = Filename.temp_dir "agent_extension_lifecycle_hooks_test" "" in
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
  write_file ".pi/extensions/startup.ts"
    {|import { Type } from "typebox";

export default function(pi) {
  pi.on("session_start", async (event) => {
    pi.registerTool({
      name: "session_dynamic",
      label: "Session Dynamic",
      description: "Tool registered from session_start",
      parameters: Type.Object({}),
      async execute() {
        return { content: [{ type: "text", text: `session ${event.reason}` }], details: {} };
      },
    });
    pi.registerCommand("sessioncmd", {
      description: "Command registered from session_start",
      handler: async () => ({ content: [{ type: "text", text: `session command ${event.reason}` }] }),
    });
  });
}
|};
  write_file ".pi/extensions/lifecycle.ts"
    {|const fs = require("node:fs");
export default function(pi) {
  pi.on("before_agent_start", async (event) => {
    fs.appendFileSync("lifecycle.log", `before_agent_start ${event.prompt}\n`);
    return {
      message: { role: "user", content: [{ type: "text", text: `injected ${event.prompt}` }] },
      systemPrompt: `${event.systemPrompt}\nBEFORE:${event.prompt}`,
    };
  });
  pi.on("context", async (event) => {
    fs.appendFileSync("lifecycle.log", `context ${event.messages.length}\n`);
    return { messages: event.messages.concat([{ role: "user", content: [{ type: "text", text: "context extra" }] }]) };
  });
  pi.on("agent_start", async () => {
    fs.appendFileSync("lifecycle.log", "agent_start\n");
  });
  pi.on("agent_end", async (event) => {
    fs.appendFileSync("lifecycle.log", `agent_end ${event.messages.length}\n`);
  });
  pi.on("model_select", async (event) => {
    fs.appendFileSync("lifecycle.log", `model_select ${event.source} ${event.previousModel ? event.previousModel.id : "none"} ${event.model.id}\n`);
  });
  pi.on("thinking_level_select", async (event) => {
    fs.appendFileSync("lifecycle.log", `thinking_level_select ${event.previousLevel} ${event.level}\n`);
  });
  pi.on("turn_start", async (event) => {
    fs.appendFileSync("lifecycle.log", `turn_start ${event.turnIndex}\n`);
  });
  pi.on("turn_end", async (event) => {
    fs.appendFileSync("lifecycle.log", `turn_end ${event.turnIndex} ${event.toolResults.length}\n`);
  });
  pi.on("message_start", async (event) => {
    fs.appendFileSync("lifecycle.log", `message_start ${event.message.role}\n`);
  });
  pi.on("message_update", async (event) => {
    fs.appendFileSync("lifecycle.log", `message_update ${event.assistantMessageEvent.text}\n`);
  });
  pi.on("message_end", async (event) => {
    fs.appendFileSync("lifecycle.log", `message_end ${event.message.role}\n`);
    if (event.message.role === "assistant") {
      return { message: { ...event.message, content: [{ type: "text", text: "rewritten assistant" }] } };
    }
  });
  pi.on("tool_execution_start", async (event) => {
    fs.appendFileSync("lifecycle.log", `tool_start ${event.toolName}\n`);
  });
  pi.on("tool_execution_update", async (event) => {
    fs.appendFileSync("lifecycle.log", `tool_update ${event.toolName}\n`);
  });
  pi.on("tool_execution_end", async (event) => {
    fs.appendFileSync("lifecycle.log", `tool_end ${event.toolName} ${event.isError}\n`);
  });
}
|};
  write_file ".pi/extensions/session-hooks.ts"
    {|const fs = require("node:fs");
export default function(pi) {
  pi.on("session_before_switch", async (event) => {
    fs.appendFileSync("lifecycle.log", `session_before_switch ${event.reason} ${event.targetSessionFile || ""}\n`);
    if (event.reason === "blocked") return { cancel: true, reason: "no switch" };
    if ((event.targetSessionFile || "").includes("cancel-target")) return { cancel: true, reason: "no switch" };
  });
  pi.on("session_before_fork", async (event) => {
    fs.appendFileSync("lifecycle.log", `session_before_fork ${event.reason} ${event.entryId || ""}\n`);
    if (event.entryId === "blocked-fork") return { cancelled: true, message: "no fork" };
  });
  pi.on("session_before_compact", async (event) => {
    fs.appendFileSync("lifecycle.log", `session_before_compact ${event.turnCount}\n`);
    if (event.turnCount === 99) return { cancel: true, reason: "no compact" };
  });
  pi.on("session_shutdown", async (event) => {
    fs.appendFileSync("lifecycle.log", `session_shutdown ${event.reason} ${event.sessionId || ""}\n`);
  });
  pi.on("session_start", async (event) => {
    fs.appendFileSync("lifecycle.log", `session_start ${event.reason} ${event.sessionId || ""}\n`);
  });
  pi.on("session_compact", async (event) => {
    fs.appendFileSync("lifecycle.log", `session_compact ${event.beforeTurnCount} ${event.afterTurnCount}\n`);
  });
}
|};

  ignore (Extensions.load ());
  check "TypeScript extension session_start can register tool"
    ((not node_available)
     ||
     match Tools.find "session_dynamic" with
     | Some t -> contains (t.Tools.execute (`Assoc [])) "session startup"
     | None -> false);
  check "TypeScript extension session_start can register command"
    ((not node_available)
     ||
     List.mem_assoc "/sessioncmd" (Complete.menu "/sessionc")
     &&
     match Extensions.execute_command "/sessioncmd" with
     | Some output -> contains output "session command startup"
     | None -> false);
  let lifecycle_ok, message_replace_ok, user_message_preserved_ok, turn_message_tool_ok, before_context_ok =
    if not node_available then (true, true, true, true, true)
    else begin
      let before_agent = Extensions.emit_before_agent_start ~prompt:"ask" ~system_prompt:"system base" in
      let context_messages =
        Extensions.emit_context [ { Llm.role = Llm.User; content = [ Llm.Text "context base" ] } ]
      in
      Extensions.emit_agent_start ();
      Extensions.emit_turn_start ~turn_index:3;
      Extensions.emit_message_start { Llm.role = Llm.User; content = [ Llm.Text "plain user" ] };
      Extensions.emit_message_update ~delta:"delta" { Llm.role = Llm.Assistant; content = [ Llm.Text "delta" ] };
      let user_message =
        Extensions.emit_message_end { Llm.role = Llm.User; content = [ Llm.Text "plain user" ] }
      in
      let assistant_message =
        Extensions.emit_message_end { Llm.role = Llm.Assistant; content = [ Llm.Text "raw assistant" ] }
      in
      Extensions.emit_tool_execution_start ~tool_call_id:"tool-1" ~tool_name:"ts_greet"
        ~input:(`Assoc [ ("name", `String "Pi") ]);
      Extensions.emit_tool_execution_update ~tool_call_id:"tool-1" ~tool_name:"ts_greet"
        ~input:(`Assoc [ ("name", `String "Pi") ])
        (`Assoc [ ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String "partial") ] ]) ]);
      Extensions.emit_tool_execution_end ~tool_call_id:"tool-1" ~tool_name:"ts_greet" ~result:"done" ~is_error:false;
      Extensions.emit_turn_end ~turn_index:3 ~message:assistant_message
        ~tool_results:[ Llm.Tool_result { id = "tool-1"; content = "done" } ];
      Extensions.emit_agent_end ~messages:[ user_message; assistant_message ];
      let log = Tools.read_file_contents "lifecycle.log" in
      let lifecycle_ok = contains log "agent_start" && contains log "agent_end 2" in
      let message_replace_ok =
        match assistant_message with
        | { Llm.content = [ Llm.Text text ]; _ } -> text = "rewritten assistant"
        | _ -> false
      in
      let user_message_preserved_ok =
        match user_message with
        | { Llm.content = [ Llm.Text text ]; _ } -> text = "plain user"
        | _ -> false
      in
      let turn_message_tool_ok =
        contains log "turn_start 3" && contains log "turn_end 3 1"
        && contains log "message_start user" && contains log "message_update delta"
        && contains log "tool_start ts_greet" && contains log "tool_update ts_greet"
        && contains log "tool_end ts_greet false"
      in
      let before_context_ok =
        contains log "before_agent_start ask" && contains log "context 1"
        && before_agent.Extensions.system_prompt = Some "system base\nBEFORE:ask"
        && (match before_agent.Extensions.injected_messages with
            | [ { Llm.content = [ Llm.Text text ]; _ } ] -> text = "injected ask"
            | _ -> false)
        && List.length context_messages = 2
        &&
        match List.rev context_messages with
        | { Llm.content = [ Llm.Text "context extra" ]; _ } :: _ -> true
        | _ -> false
      in
      (lifecycle_ok, message_replace_ok, user_message_preserved_ok, turn_message_tool_ok, before_context_ok)
    end
  in
  check "TypeScript extension agent_start/agent_end fire" lifecycle_ok;
  check "TypeScript extension message_end can replace assistant" message_replace_ok;
  check "TypeScript extension message_end preserves unchanged user" user_message_preserved_ok;
  check "TypeScript extension turn/message/tool lifecycle events fire" turn_message_tool_ok;
  check "TypeScript extension before_agent_start/context can mutate prompt context" before_context_ok;
  check "TypeScript extension model/thinking selection events fire"
    ((not node_available)
     ||
     let event_agent = Agent.create cfg_for_reset in
     Agent.set_config event_agent { cfg_for_reset with Llm.model = "event-model"; thinking = "high" };
     Agent.set_thinking event_agent "low";
     let log = Tools.read_file_contents "lifecycle.log" in
     contains log "thinking_level_select off high"
     && contains log "model_select set test-model event-model"
     && contains log "thinking_level_select high low");
  let session_events_ok, switch_cancel_ok, fork_cancel_ok, compact_cancel_ok, command_new_ok =
    if not node_available then (true, true, true, true, true)
    else begin
      let switch_ok =
        match Extensions.emit_session_before_switch ~reason:"manual" ~target_session_file:"next.jsonl" () with
        | Extensions.Session_continue -> true
        | Extensions.Session_cancel _ -> false
      in
      let switch_cancel =
        match Extensions.emit_session_before_switch ~reason:"blocked" () with
        | Extensions.Session_cancel reason -> contains reason "no switch"
        | Extensions.Session_continue -> false
      in
      let fork_cancel =
        match Extensions.emit_session_before_fork ~reason:"fork" ~entry_id:"blocked-fork" () with
        | Extensions.Session_cancel reason -> contains reason "no fork"
        | Extensions.Session_continue -> false
      in
      let compact_cancel =
        match Extensions.emit_session_before_compact ~turn_count:99 () with
        | Extensions.Session_cancel reason -> contains reason "no compact"
        | Extensions.Session_continue -> false
      in
      ignore (Extensions.emit_session_start ~reason:"manual" ~session_file:"manual.jsonl" ~session_id:"manual-id" ());
      Extensions.emit_session_shutdown ~reason:"manual" ~session_file:"manual.jsonl" ~session_id:"manual-id" ();
      Extensions.emit_session_compact ~session_file:"manual.jsonl" ~session_id:"manual-id" ~before_turn_count:12
        ~after_turn_count:7 ();
      let command_session = Session.create_new ~name:"command-new" () in
      let command_agent = Agent.create ~session:command_session cfg_for_reset in
      let command_msg = Commands.new_session command_agent in
      let command_new =
        contains command_msg "Started new session"
        &&
        match Agent.session command_agent with
        | Some session -> session.Session.path <> command_session.Session.path
        | None -> false
      in
      Option.iter Session.close (Agent.session command_agent);
      let log = Tools.read_file_contents "lifecycle.log" in
      let session_events =
        switch_ok && contains log "session_before_switch manual next.jsonl"
        && contains log "session_before_fork fork blocked-fork"
        && contains log "session_before_compact 99" && contains log "session_start manual manual-id"
        && contains log "session_shutdown manual manual-id" && contains log "session_compact 12 7"
        && contains log "session_before_switch new" && contains log "session_shutdown new"
        && contains log "session_start new"
      in
      (session_events, switch_cancel, fork_cancel, compact_cancel, command_new)
    end
  in
  check "TypeScript extension session lifecycle events fire" session_events_ok;
  check "TypeScript extension session_before_switch can cancel" switch_cancel_ok;
  check "TypeScript extension session_before_fork can cancel" fork_cancel_ok;
  check "TypeScript extension session_before_compact can cancel" compact_cancel_ok;
  check "slash new starts a new persisted session" command_new_ok;

  if !failures > 0 then exit 1
