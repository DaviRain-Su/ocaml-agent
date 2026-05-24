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
const { AgentSession, AgentSessionRuntime, ArminComponent, AssistantMessageComponent, BashExecutionComponent, BorderedLoader, CustomEditor, FooterComponent, InteractiveMode, RpcClient, ToolExecutionComponent, UserMessageComponent, createAgentSession, createAgentSessionFromServices, createAgentSessionRuntime, createAgentSessionServices, main, runPrintMode, runRpcMode, truncateToVisualLines } = require("@earendil-works/pi-coding-agent");

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
      await mode.init();
      const initialMessages = mode.renderInitialMessages();
      const inputPromise = mode.getUserInput();
      mode.onInputCallback("typed input");
      const typedInput = await inputPromise;
      mode.editorText = "dirty";
      mode.clearEditor();
      mode.showError("mode error");
      mode.showWarning("mode warning");
      mode.showNewVersionNotification({ version: "9.9.9", note: "note" });
      mode.showPackageUpdateNotification(["pkg-a"]);
      const modeCode = await mode.start();
      const printCode = await runPrintMode(runtimeResult.runtime, { prompt: "print prompt" });
      const mainCode = await main({ cwd: process.cwd(), agentDir, mode: "print", prompt: "main prompt" });
      const rpc = new RpcClient({ cliPath: "missing-cli.js" });
      const trunc = truncateToVisualLines("a\nb\nc", 2, 80, 0);
      const userLines = new UserMessageComponent({ content: "user component" }).render(80).join("|");
      const assistant = new AssistantMessageComponent({ content: [{ type: "text", text: "assistant component" }] });
      assistant.setHideThinkingBlock(true);
      assistant.setHiddenThinkingLabel("hidden thinking");
      assistant.updateContent({ content: [{ type: "text", text: "assistant updated" }] });
      const assistantLines = assistant.render(80).join("|");
      const tool = new ToolExecutionComponent("tool component", "call-1", { before: true });
      tool.updateArgs({ after: true });
      tool.markExecutionStarted();
      tool.setArgsComplete();
      tool.setExpanded(true);
      tool.setShowImages(false);
      tool.setImageWidthCells(3);
      tool.updateResult({ content: [{ type: "text", text: "tool result" }], isError: false });
      const toolLines = tool.render(80).join("|");
      const bash = new BashExecutionComponent("echo hi");
      bash.setExpanded(true);
      bash.appendOutput("hello");
      bash.appendOutput(" world");
      bash.setComplete(0, false);
      const footer = new FooterComponent("footer component");
      footer.setSession(session);
      footer.setAutoCompactEnabled(false);
      footer.setExtensionStatus("sync", "ready");
      const footerLines = footer.render(80).join("|");
      const editor = new CustomEditor();
      let editorAction = false;
      editor.onAction("app.test", () => { editorAction = true; });
      editor.actionHandlers.get("app.test")();
      const loader = new BorderedLoader(null, { fg: (_key, text) => text }, "loading");
      let aborted = false;
      loader.onAbort = () => { aborted = true; };
      loader.handleInput("\u001b");
      loader.dispose();
      const armin = new ArminComponent("armin component").render(80).join("|");
      const rpcRuntime = await createAgentSessionRuntime({ cwd: process.cwd(), agentDir: path.resolve("sdk-rpc-agent") });
      const captured = [];
      const originalWrite = process.stdout.write;
      process.stdout.write = function(chunk, ...args) {
        captured.push(Buffer.isBuffer(chunk) ? chunk.toString("utf8") : String(chunk));
        if (typeof args[args.length - 1] === "function") args[args.length - 1]();
        return true;
      };
      const rpcPromise = runRpcMode(rpcRuntime.runtime);
      await new Promise((resolve) => setTimeout(resolve, 20));
      for (const command of [
        { id: "rpc1", type: "get_state" },
        { id: "rpc2", type: "set_session_name", name: "rpc session" },
        { id: "rpc3", type: "get_state" },
        { id: "rpc4", type: "bash", command: "printf rpc-ok" },
        { id: "rpc5", type: "shutdown" },
      ]) {
        process.stdin.emit("data", `${JSON.stringify(command)}\n`);
      }
      await rpcPromise;
      process.stdout.write = originalWrite;
      const rpcResponses = captured.join("").split(/\r?\n/).filter(Boolean).map((line) => JSON.parse(line)).filter((line) => line.type === "response");
      const rpcById = Object.fromEntries(rpcResponses.map((response) => [response.id, response]));
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
        assistantLines.includes("assistant updated"),
        toolLines.includes("tool result"),
        footerLines.includes("footer component"),
        armin.includes("armin component"),
        dynamicTool,
        sessionCommand,
        actionText === "new:false",
        commandActionCalled,
        boundHasUi,
        bash.getCommand() === "echo hi",
        bash.getOutput() === "hello world",
        bash.status === "complete",
        tool.executionStarted,
        tool.argsComplete,
        tool.showImages === false,
        tool.imageWidthCells === 3,
        footer.extensionStatuses.sync === "ready",
        editorAction,
        typeof loader.signal === "object",
        aborted,
        typeof runRpcMode,
        rpcById.rpc1 && rpcById.rpc1.success && typeof rpcById.rpc1.data.sessionId === "string",
        rpcById.rpc3 && rpcById.rpc3.data.sessionName === "rpc session",
        rpcById.rpc4 && rpcById.rpc4.data.output === "rpc-ok",
        rpcById.rpc5 && rpcById.rpc5.success,
        mode.isInitialized,
        initialMessages.length === 1,
        typedInput === "typed input",
        mode.editorText === "",
        mode.errors.includes("mode error"),
        mode.warnings.includes("mode warning"),
        mode.notifications.some((item) => item.type === "new_version" && item.release.version === "9.9.9"),
        mode.notifications.some((item) => item.type === "package_updates" && item.packages[0] === "pkg-a"),
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
       List.length parts = 55
       && List.nth parts 0 = "function"
       && List.nth parts 1 = "true"
       && List.nth parts 3 = "read,custom_sdk"
       && List.nth parts 6 = "1"
       && List.nth parts 13 = "1"
       && List.nth parts 18 = "function"
       && List.nth parts 19 = "b,c"
       && List.nth parts 20 = "1"
       && List.nth parts 42 = "function"
       && List.for_all (fun index -> List.nth parts index = "true")
            [ 2; 4; 5; 7; 8; 9; 10; 11; 12; 15; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39; 40; 41; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54 ]
     | None -> false);

  if !failures > 0 then exit 1
