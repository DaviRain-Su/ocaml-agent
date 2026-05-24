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

let () =
  let dir = Filename.temp_dir "agent_extension_sdk_runtime_programmatic_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

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
  write_file ".pi/extensions/session-actions.ts"
    {|export default function(pi) {
  pi.registerCommand("sessionaction", {
    description: "Request command session actions",
    handler: async (_args, ctx) => {
      await ctx.waitForIdle();
      const result = await ctx.newSession();
      return `new:${result.cancelled}`;
    },
  });
}
|};
  write_file ".pi/extensions/runtime-provider.ts"
    {|const fs = require("node:fs");
export default function(pi) {
  pi.registerProvider({
    name: "runtimeai",
    aliases: ["runtime"],
    defaultModel: "runtime-small",
    models: [{ id: "runtime-small", contextWindow: 9001 }],
    complete: async (request) => {
      fs.appendFileSync("runtime-request.log", JSON.stringify(request.messages) + "\n");
      return {
        content: [{ type: "text", text: `runtime ${request.model}:${request.system}:${request.messages.length}:${request.toolsEnabled}` }],
        usage: { inputTokens: 12, outputTokens: 3 },
      };
    },
  });
}
|};
  write_file ".pi/extensions/sdk-runtime.ts"
    {|const fs = require("node:fs");
const path = require("node:path");
const { AgentSession, AgentSessionRuntime, ArminComponent, AssistantMessageComponent, FooterComponent, InteractiveMode, RpcClient, ToolExecutionComponent, UserMessageComponent, createAgentSession, createAgentSessionFromServices, createAgentSessionRuntime, createAgentSessionServices, main, runPrintMode, truncateToVisualLines } = require("@earendil-works/pi-coding-agent");

export default function(pi) {
  pi.registerCommand("sdkruntime", {
    description: "Probe programmatic SDK and UI component exports",
    handler: async () => {
      const agentDir = path.resolve("sdk-runtime-agent");
      const services = await createAgentSessionServices({ cwd: process.cwd(), agentDir });
      const created = await createAgentSession({ cwd: process.cwd(), agentDir, customTools: [{ name: "custom_sdk", description: "custom", parameters: { type: "object" }, execute: async () => ({ content: [{ type: "text", text: "custom" }] }) }] });
      const session = created.session;
      let commandActionCalled = false;
      await session.bindExtensions({ uiContext: { notify: () => {} }, commandContextActions: { waitForIdle: async () => {}, newSession: async () => { commandActionCalled = true; return { cancelled: false }; }, fork: async () => ({ cancelled: false }), navigateTree: async () => ({ cancelled: false }), switchSession: async () => ({ cancelled: false }), reload: async () => {} } });
      const dynamicTool = session.getAllTools().some((tool) => tool.name === "session_dynamic");
      const sessionCommand = session.getCommands().some((command) => command.name === "sessioncmd");
      const actionCommand = session.extensionRunner.getCommand("sessionaction");
      const actionText = actionCommand ? await actionCommand.handler("new", session.extensionRunner.createCommandContext()) : "missing";
      const boundHasUi = session.extensionRunner.hasUI();
      session.setActiveToolsByName(["read", "custom_sdk"]);
      await session.prompt("hello sdk runtime");
      const stats = session.getSessionStats();
      const usage = session.getContextUsage();
      const jsonPath = session.exportToJsonl("sdk-runtime-session.jsonl");
      const htmlPath = await session.exportToHtml("sdk-runtime.html");
      const fromServices = await createAgentSessionFromServices(services, { customTools: [{ name: "svc_tool", description: "svc", parameters: { type: "object" } }] });
      const runtimeResult = await createAgentSessionRuntime({ cwd: process.cwd(), agentDir });
      await runtimeResult.runtime.newSession({ setup: async (manager) => manager.appendCustomMessageEntry("runtime-note", "body", true) });
      const mode = new InteractiveMode(runtimeResult.runtime, { headless: true });
      const modeCode = await mode.start();
      const printCode = await runPrintMode(runtimeResult.runtime, { prompt: "print prompt" });
      const mainCode = await main({ cwd: process.cwd(), agentDir, mode: "print", prompt: "main prompt" });
      const rpc = new RpcClient({ cliPath: "missing-cli.js" });
      const trunc = truncateToVisualLines("a\nb\nc", 2, 80, 0);
      const userLines = new UserMessageComponent({ content: "user component" }).render(80).join("|");
      const assistantLines = new AssistantMessageComponent({ message: { content: [{ type: "text", text: "assistant component" }] } }).render(80).join("|");
      const toolLines = new ToolExecutionComponent({ name: "tool component" }).render(80).join("|");
      const footerLines = new FooterComponent("footer component").render(80).join("|");
      const armin = new ArminComponent("armin component").render(80).join("|");
      return [
        typeof AgentSession,
        session instanceof AgentSession,
        created.extensionsResult && Array.isArray(created.extensionsResult.extensions),
        session.getActiveToolNames().join(","),
        session.getAllTools().some((tool) => tool.name === "read"),
        session.getAllTools().some((tool) => tool.name === "custom_sdk"),
        stats.totalMessages,
        usage.tokens > 0,
        fs.existsSync(jsonPath),
        fs.existsSync(htmlPath),
        fromServices.session instanceof AgentSession,
        fromServices.session.getAllTools().some((tool) => tool.name === "svc_tool"),
        runtimeResult.runtime instanceof AgentSessionRuntime,
        runtimeResult.runtime.session.getSessionStats().totalMessages,
        modeCode,
        mode.running,
        printCode,
        mainCode,
        typeof rpc.onEvent,
        trunc.visualLines.join(","),
        trunc.skippedCount,
        userLines.includes("user component"),
        assistantLines.includes("assistant component"),
        toolLines.includes("tool component"),
        footerLines.includes("footer component"),
        armin.includes("armin component"),
        dynamicTool,
        sessionCommand,
        actionText === "new:false",
        commandActionCalled,
        boundHasUi,
      ].join(":");
    },
  });
}
|};
  ignore (Extensions.load ());
  check "TypeScript extension programmatic SDK binds extension runtime"
    ((not node_available)
     ||
     match Extensions.execute_command "/sdkruntime" with
     | Some output ->
       let parts = String.split_on_char ':' output in
       List.length parts = 31
       && List.nth parts 0 = "function"
       && List.nth parts 1 = "true"
       && List.nth parts 3 = "read,custom_sdk"
       && List.nth parts 6 = "1"
       && List.nth parts 13 = "1"
       && List.nth parts 18 = "function"
       && List.nth parts 19 = "b,c"
       && List.nth parts 20 = "1"
       && List.for_all (fun index -> List.nth parts index = "true")
            [ 2; 4; 5; 7; 8; 9; 10; 11; 12; 15; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30 ]
     | None -> false);

  if !failures > 0 then exit 1
