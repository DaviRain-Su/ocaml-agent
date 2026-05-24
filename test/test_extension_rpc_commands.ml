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
  let dir = Filename.temp_dir "agent_extension_rpc_commands_test" "" in
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
  let rpc_field json key =
    match json with `Assoc fields -> List.assoc_opt key fields | _ -> None
  in
  let rpc_last out = match List.rev out with last :: _ -> last | [] -> `Null in
  let rpc_success out command =
    let r = rpc_last out in
    rpc_field r "type" = Some (`String "response")
    && rpc_field r "command" = Some (`String command)
    && rpc_field r "success" = Some (`Bool true)
  in
  let node_available = Sys.command "command -v node >/dev/null 2>&1" = 0 in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/hello.ts","content":"import { Type } from \"typebox\";\nimport { createLocalBashOperations, createLocalFileOperations } from \"@earendil-works/pi-coding-agent\";\nexport default function(pi) {\n  pi.registerTool({\n    name: \"ts_greet\",\n    label: \"TS Greet\",\n    description: \"Greet from a Pi TypeScript extension\",\n    parameters: Type.Object({ name: Type.String({ description: \"Name\" }) }),\n    async execute(_id, params) {\n      return { content: [{ type: \"text\", text: `Hello ${params.name}` }], details: {} };\n    },\n  });\n  pi.registerTool({\n    name: \"ops_file\",\n    label: \"Operations File\",\n    description: \"Use Pi-style tool operations\",\n    parameters: Type.Object({}),\n    async execute(_id, _params, operations, _system, ctx) {\n      await operations.writeFile(\"ops-dir/out.txt\", \"ops body\");\n      const body = await operations.readFile(\"ops-dir/out.txt\");\n      const listing = await ctx.operations.listDir(\"ops-dir\");\n      const stat = await operations.stat(\"ops-dir/out.txt\");\n      return { content: [{ type: \"text\", text: `${body}:${listing.join(\",\")}:${stat.isFile}` }], details: {} };\n    },\n  });\n  pi.registerFlag(\"voice\", { description: \"Voice flag\", type: \"string\", defaultValue: \"calm\" });\n  pi.registerFlag({ name: \"dry-run\", description: \"Dry run\", type: \"boolean\", default: false });\n  pi.registerShortcut(\"ctrl+g\", { description: \"Show voice shortcut\", handler: async () => `shortcut ${pi.getFlag(\"voice\")}` });\n  pi.registerShortcut({ key: \"ctrl+h\", description: \"Run tshello shortcut\", command: \"/tshello Shortcut\" });\n  pi.registerMessageRenderer(\"tagger\", {\n    description: \"Tag assistant and tool text\",\n    target: \"all\",\n    render: async (event) => `[${event.kind}:${event.toolName || event.role}] ${event.text}`,\n  });\n  pi.registerCommand(\"tshello\", {\n    description: \"Say hello from TypeScript\",\n    handler: async (args, ctx) => {\n      ctx.ui.notify(`notified ${args || \"world\"}`);\n      return { content: [{ type: \"text\", text: `Command ${args || \"world\"}` }] };\n    },\n  });\n  pi.registerCommand(\"flagshow\", {\n    description: \"Show registered flags\",\n    handler: async () => `${pi.getFlag(\"voice\")}:${pi.getFlag(\"dry-run\")}`,\n  });\n  pi.registerCommand(\"toolscope\", {\n    description: \"Show and set active tools\",\n    handler: async () => {\n      const before = pi.getActiveTools();\n      const all = pi.getAllTools().map((tool) => tool.name);\n      pi.setActiveTools([\"read\", \"ts_greet\"]);\n      const after = pi.getActiveTools();\n      return `${before.includes(\"read\")}:${all.includes(\"read\")}:${all.includes(\"ts_greet\")}:${after.join(\",\")}`;\n    },\n  });\n  pi.registerCommand(\"thinkscope\", {\n    description: \"Show and set thinking level\",\n    handler: async () => {\n      const before = pi.getThinkingLevel();\n      pi.setThinkingLevel(\"high\");\n      return `${before}:${pi.getThinkingLevel()}`;\n    },\n  });\n  pi.on(\"input\", async (event) => {\n    if (event.text === \"handled\") return { action: \"handled\" };\n    if (event.text.startsWith(\"brief:\")) return { action: \"transform\", text: `Respond briefly: ${event.text.slice(6).trim()}` };\n  });\n  pi.on(\"tool_call\", async (event) => {\n    if (event.toolName === \"ts_greet\") event.input.name = `${event.input.name}!`;\n    if (event.toolName === \"bash\" && event.input.command === \"blocked\") return { block: true, reason: \"blocked by ts\" };\n  });\n  pi.on(\"tool_result\", async (event) => {\n    if (event.toolName === \"ts_greet\") return { content: [{ type: \"text\", text: `${event.content[0].text} hooked` }] };\n  });\n  pi.on(\"user_bash\", async (event) => {\n    if (event.command === \"virtual\") return { result: { output: `virtual ${event.excludeFromContext}`, exitCode: 7 } };\n    if (event.command === \"ops\") return { operations: { exec: async (command, _cwd, options) => {\n      options.onData(Buffer.from(`ops ${command}`));\n      return { exitCode: 9 };\n    } } };\n    if (event.command === \"localops\") {\n      const local = createLocalBashOperations();\n      return { operations: { exec: async (_command, cwd, options) => {\n        options.onData(Buffer.from(\"wrapped\\n\"));\n        return local.exec(\"printf localops\", cwd, options);\n      } } };\n    }\n    if (event.command === \"fileops\") {\n      const files = createLocalFileOperations();\n      await files.writeFile(\"ops-dir/local-fileops.txt\", \"fileops\");\n      return { result: { output: await files.readFile(\"ops-dir/local-fileops.txt\"), exitCode: 0 } };\n    }\n  });\n  pi.on(\"session_start\", async (event) => {\n    pi.registerTool({\n      name: \"session_dynamic\",\n      label: \"Session Dynamic\",\n      description: \"Tool registered from session_start\",\n      parameters: Type.Object({}),\n      async execute() {\n        return { content: [{ type: \"text\", text: `session ${event.reason}` }], details: {} };\n      },\n    });\n    pi.registerCommand(\"sessioncmd\", {\n      description: \"Command registered from session_start\",\n      handler: async () => ({ content: [{ type: \"text\", text: `session command ${event.reason}` }] }),\n    });\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/tool-signature.ts","content":"export default function(pi) {\n  pi.registerTool({\n    name: \"tool_signature\",\n    label: \"Tool Signature\",\n    description: \"Probe Pi tool execute signature\",\n    parameters: { type: \"object\", properties: {} },\n    async execute(_id, _params, signal, onUpdate, ctx) {\n      onUpdate?.({ content: [{ type: \"text\", text: \"partial update\" }], details: { phase: 1 } });\n      await ctx.operations.writeFile(\"ops-dir/signature.txt\", \"sig\");\n      const body = await signal.readFile(\"ops-dir/signature.txt\");\n      return { content: [{ type: \"text\", text: `sig:${signal.aborted === false}:${ctx.signal === signal}:${typeof onUpdate}:${body}` }], details: {} };\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/prepared-tool.ts","content":"export default function(pi) {\n  pi.registerTool({\n    name: \"prepared_tool\",\n    label: \"Prepared Tool\",\n    description: \"Probe prepareArguments support\",\n    parameters: { type: \"object\", properties: {} },\n    prepareArguments(args) {\n      return { text: `prepared:${args.raw || \"none\"}`, count: Number(args.count || 0) + 1 };\n    },\n    async execute(_id, params) {\n      return { content: [{ type: \"text\", text: `${params.text}:${params.count}` }], details: {} };\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/command-complete.ts","content":"export default function(pi) {\n  pi.registerCommand(\"argcomplete\", {\n    description: \"Probe command argument completion\",\n    argumentHint: \"<source>\",\n    getArgumentCompletions(prefix) {\n      return [\"extension\", \"prompt\", \"skill\"]\n        .filter((item) => item.startsWith(prefix))\n        .map((item) => ({ value: item, label: item, description: `source ${item}` }));\n    },\n    handler: async (args) => `arg ${args}`,\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/command-api.ts","content":"export default function(pi) {\n  pi.registerCommand(\"commandlist\", {\n    description: \"Probe pi.getCommands API\",\n    handler: async () => {\n      return pi.getCommands()\n        .map((command) => `${command.name}:${command.source || \"\"}:${command.sourceInfo ? command.sourceInfo.path || \"\" : \"\"}`)\n        .sort()\n        .join(\"|\");\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/event-bus.ts","content":"export default function(pi) {\n  const seen = [];\n  pi.events.on(\"probe\", (data) => {\n    seen.push(`${data.kind}:${data.value}`);\n  });\n  const off = pi.events.on(\"probe-off\", () => {\n    seen.push(\"off-called\");\n  });\n  off();\n  pi.registerCommand(\"eventbus\", {\n    description: \"Probe pi.events EventBus\",\n    handler: async () => {\n      pi.events.emit(\"probe\", { kind: \"cmd\", value: 1 });\n      pi.events.emit(\"probe-off\", { kind: \"cmd\", value: 2 });\n      pi.events.emit(\"probe\", { kind: \"cmd\", value: 3 });\n      return seen.join(\",\");\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/event-listener.ts","content":"const fs = require(\"node:fs\");\nexport default function(pi) {\n  pi.events.on(\"cross:probe\", (data) => {\n    fs.appendFileSync(\"event-bus.log\", `${data.from}:${data.value}\\n`);\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/event-emitter.ts","content":"export default function(pi) {\n  pi.registerCommand(\"eventemit\", {\n    description: \"Emit cross-extension event\",\n    handler: async () => {\n      pi.events.emit(\"cross:probe\", { from: \"emitter\", value: 42 });\n      return \"emitted\";\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/exec.ts","content":"export default function(pi) {\n  pi.registerCommand(\"execprobe\", {\n    description: \"Probe pi.exec result shape\",\n    handler: async () => {\n      const ok = await pi.exec(process.execPath, [\"-e\", \"process.stdout.write('out'); process.stderr.write('err')\"]);\n      const fail = await pi.exec(process.execPath, [\"-e\", \"process.exit(7)\"]);\n      return `${ok.stdout}:${ok.stderr}:${ok.code}:${ok.exitCode}:${ok.killed}:${fail.code}:${fail.exitCode}:${fail.killed}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/file-queue.ts","content":"const fs = require(\"node:fs/promises\");\nconst path = require(\"node:path\");\nconst { withFileMutationQueue } = require(\"@earendil-works/pi-coding-agent\");\n\nconst sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));\n\nexport default function(pi) {\n  pi.registerCommand(\"queueprobe\", {\n    description: \"Probe withFileMutationQueue serialization\",\n    handler: async () => {\n      const file = path.resolve(\"queue-probe.txt\");\n      await fs.writeFile(file, \"\", \"utf8\");\n      await Promise.all([\n        withFileMutationQueue(file, async () => {\n          const before = await fs.readFile(file, \"utf8\");\n          await sleep(30);\n          await fs.writeFile(file, `${before}A`, \"utf8\");\n          return \"A\";\n        }),\n        withFileMutationQueue(file, async () => {\n          const before = await fs.readFile(file, \"utf8\");\n          await fs.writeFile(file, `${before}B`, \"utf8\");\n          return \"B\";\n        }),\n      ]);\n      return await fs.readFile(file, \"utf8\");\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/type-guards.ts","content":"const { isToolCallEventType, isBashToolResult, isReadToolResult, isEditToolResult, isWriteToolResult, isGrepToolResult, isFindToolResult, isLsToolResult } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"guardprobe\", {\n    description: \"Probe tool event type guards\",\n    handler: async () => [\n      typeof isToolCallEventType,\n      isToolCallEventType(\"ts_greet\", { toolName: \"ts_greet\" }),\n      isToolCallEventType(\"bash\", { toolName: \"read\" }),\n      isBashToolResult({ toolName: \"bash\" }),\n      isReadToolResult({ toolName: \"read\" }),\n      isEditToolResult({ toolName: \"edit\" }),\n      isWriteToolResult({ toolName: \"write\" }),\n      isGrepToolResult({ toolName: \"grep\" }),\n      isFindToolResult({ toolName: \"find\" }),\n      isLsToolResult({ toolName: \"ls\" }),\n    ].join(\":\"),\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/wrapped-tool.ts","content":"const { wrapRegisteredTool, wrapRegisteredTools } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  const runner = { createContext: () => ({ wrapped: true }) };\n  const registered = {\n    definition: {\n      name: \"wrapped_tool\",\n      label: \"Wrapped Tool\",\n      description: \"Probe wrapRegisteredTool\",\n      parameters: { type: \"object\", properties: { value: { type: \"string\" } } },\n      async execute(_id, params, _signal, _onUpdate, ctx) {\n        return { content: [{ type: \"text\", text: `wrapped:${params.value}:${ctx.wrapped}` }], details: {} };\n      },\n    },\n    sourceInfo: { path: \"wrapped-tool.ts\", source: \"extension\", scope: \"project\", origin: \"top-level\" },\n  };\n  pi.registerTool(wrapRegisteredTool(registered, runner));\n  pi.registerCommand(\"wrapprobe\", {\n    description: \"Probe registered tool wrappers\",\n    handler: async () => {\n      const wrapped = wrapRegisteredTools([registered], runner);\n      return `${typeof wrapRegisteredTool}:${wrapped.length}:${wrapped[0].name}:${wrapped[0].description}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":"sdk-child.ts","content":"export default function(pi) {\n  pi.registerTool({\n    name: \"sdk_child_tool\",\n    label: \"SDK Child Tool\",\n    description: \"Loaded through loadExtensions\",\n    parameters: { type: \"object\", properties: { value: { type: \"string\" } } },\n    async execute(_id, params, _signal, _onUpdate, ctx) {\n      return { content: [{ type: \"text\", text: `child:${params.value}:${ctx.cwd.endsWith(\"ocaml-agent\")}` }], details: {} };\n    },\n  });\n  pi.registerCommand(\"sdkchild\", { description: \"SDK child command\", handler: async () => \"child\" });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":"sdk-ext-dir/index.ts","content":"export default function(pi) {\n  pi.registerCommand(\"sdkdiscovered\", { description: \"Discovered SDK command\", handler: async () => \"discovered\" });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/sdk-loader.ts","content":"const { createExtensionRuntime, loadExtensionFromFactory, loadExtensions, discoverAndLoadExtensions, ExtensionRunner, wrapRegisteredTool } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"sdkloader\", {\n    description: \"Probe Pi extension loader exports\",\n    handler: async () => {\n      const runtime = createExtensionRuntime();\n      const inline = await loadExtensionFromFactory((api) => {\n        api.registerTool({\n          name: \"sdk_inline_tool\",\n          label: \"SDK Inline Tool\",\n          description: \"Inline SDK tool\",\n          parameters: { type: \"object\", properties: { value: { type: \"string\" } } },\n          async execute(_id, params, _signal, _onUpdate, ctx) {\n            return { content: [{ type: \"text\", text: `inline:${params.value}:${ctx.hasUI}` }], details: {} };\n          },\n        });\n        api.registerCommand(\"sdkinline\", { description: \"Inline command\", handler: async () => \"inline\" });\n        api.on(\"turn_start\", async () => ({ seen: true }));\n        api.registerProvider(\"queuedai\", { baseUrl: \"https://queued.invalid/v1\", apiKeyEnvVar: \"QUEUEDAI_API_KEY\", defaultModel: \"queued-small\", models: [\"queued-small\"] });\n      }, process.cwd(), pi.events, runtime, \"inline-sdk.ts\");\n      const loaded = await loadExtensions([\"sdk-child.ts\"], process.cwd(), pi.events);\n      const discovered = await discoverAndLoadExtensions([\"sdk-ext-dir\"], process.cwd(), process.cwd(), pi.events);\n      const runner = new ExtensionRunner([inline, ...loaded.extensions, ...discovered.extensions], runtime, process.cwd(), { id: \"session\" }, { id: \"models\" });\n      runner.setUIContext({ notify: () => {} });\n      const wrapped = wrapRegisteredTool(inline.tools.get(\"sdk_inline_tool\"), runner);\n      const wrappedText = await wrapped.execute(\"sdk-call\", { value: \"ok\" });\n      const toolNames = runner.getAllRegisteredTools().map((tool) => tool.definition.name).sort().join(\",\");\n      const commands = runner.getRegisteredCommands().map((command) => command.invocationName).sort().join(\",\");\n      const eventResult = await runner.emit({ type: \"turn_start\" });\n      return [\n        typeof createExtensionRuntime,\n        loaded.errors.length,\n        discovered.errors.length,\n        runtime.pendingProviderRegistrations.map((item) => item.name).join(\",\"),\n        runner.hasHandlers(\"turn_start\"),\n        eventResult && eventResult.seen,\n        toolNames.includes(\"sdk_inline_tool\"),\n        toolNames.includes(\"sdk_child_tool\"),\n        commands.includes(\"sdkinline\"),\n        commands.includes(\"sdkchild\"),\n        commands.includes(\"sdkdiscovered\"),\n        wrappedText.content[0].text,\n      ].join(\":\");\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/tool-factories.ts","content":"const { createReadTool, createWriteTool, createEditTool, createBashTool, createGrepTool, createFindTool, createLsTool, createCodingTools, createReadOnlyTools, createReadToolDefinition, createBashToolDefinition, DEFAULT_MAX_BYTES, DEFAULT_MAX_LINES, truncateHead, truncateTail, truncateLine, formatSize } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  const cwd = process.cwd();\n  const rename = (tool, name) => ({ ...tool, name, description: `${name} via factory` });\n  pi.registerTool(rename(createReadTool(cwd), \"factory_read\"));\n  pi.registerTool(rename(createWriteTool(cwd), \"factory_write\"));\n  pi.registerTool(rename(createEditTool(cwd), \"factory_edit\"));\n  pi.registerTool(rename(createBashTool(cwd), \"factory_bash\"));\n  pi.registerTool(rename(createGrepTool(cwd), \"factory_grep\"));\n  pi.registerTool(rename(createFindTool(cwd), \"factory_find\"));\n  pi.registerTool(rename(createLsTool(cwd), \"factory_ls\"));\n  pi.registerCommand(\"factoryprobe\", {\n    description: \"Probe tool factory exports\",\n    handler: async () => {\n      const remoteRead = createReadToolDefinition(cwd, { operations: { readFile: async () => \"remote\\nbody\" } });\n      const remote = await remoteRead.execute(\"remote\", { path: \"ignored.txt\", limit: 1 });\n      const hookedBash = createBashToolDefinition(cwd, {\n        operations: { exec: async (command, _cwd, options) => { options.onData(Buffer.from(`hooked:${command}`)); return { exitCode: 0 }; } },\n        spawnHook: ({ command }) => ({ command: `${command}:spawned` }),\n      });\n      const bash = await hookedBash.execute(\"bash\", { command: \"cmd\" });\n      const head = truncateHead(\"a\\nb\\nc\", { maxLines: 2 }).content.replace(/\\n/g, \",\");\n      const tail = truncateTail(\"a\\nb\\nc\", { maxLines: 2 }).content.replace(/\\n/g, \",\");\n      const line = truncateLine(\"abcdef\", 3).content;\n      return [\n        createCodingTools(cwd).map((tool) => tool.name).join(\",\"),\n        createReadOnlyTools(cwd).map((tool) => tool.name).join(\",\"),\n        DEFAULT_MAX_LINES,\n        DEFAULT_MAX_BYTES,\n        formatSize(2048),\n        head,\n        tail,\n        line,\n        remote.content[0].text,\n        bash.content[0].text,\n      ].join(\":\");\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/session-list.ts","content":"const fs = require(\"node:fs\");\nconst path = require(\"node:path\");\nconst { SessionManager, getAgentDir, VERSION } = require(\"@earendil-works/pi-coding-agent\");\n\nfunction writeJsonl(file, rows) {\n  fs.mkdirSync(path.dirname(file), { recursive: true });\n  fs.writeFileSync(file, rows.map((row) => JSON.stringify(row)).join(\"\\n\") + \"\\n\", \"utf8\");\n}\n\nexport default function(pi) {\n  pi.registerCommand(\"sessionlist\", {\n    description: \"Probe SessionManager static listing\",\n    handler: async () => {\n      const dir = path.resolve(\"sdk-sessions\");\n      const nested = path.join(dir, \"nested\");\n      writeJsonl(path.join(dir, \"older.jsonl\"), [\n        { _session: 1, id: \"older\", name: \"Older\", created: 1000, cwd: process.cwd() },\n        { role: \"user\", content: [{ type: \"text\", text: \"old question\" }] },\n      ]);\n      writeJsonl(path.join(dir, \"newer.jsonl\"), [\n        { type: \"session\", version: 3, id: \"newer\", timestamp: \"2024-01-01T00:00:00.000Z\", cwd: process.cwd(), parentSession: \"older.jsonl\" },\n        { type: \"session_info\", id: \"info\", parentId: null, timestamp: \"2024-01-01T00:00:01.000Z\", name: \"Newer Name\" },\n        { type: \"message\", id: \"m1\", parentId: null, timestamp: \"2024-01-01T00:00:02.000Z\", message: { role: \"user\", content: [{ type: \"text\", text: \"new question\" }] } },\n        { type: \"message\", id: \"m2\", parentId: \"m1\", timestamp: \"2024-01-01T00:00:03.000Z\", message: { role: \"assistant\", content: [{ type: \"text\", text: \"new answer\" }] } },\n      ]);\n      writeJsonl(path.join(nested, \"nested.jsonl\"), [\n        { _session: 1, id: \"nested\", name: \"Nested\", created: 2000, cwd: process.cwd() },\n      ]);\n      const listed = await SessionManager.list(process.cwd(), dir);\n      const all = await SessionManager.listAll();\n      const listedText = listed.map((session) => `${session.id}:${session.name || \"\"}:${session.messageCount}:${session.firstMessage}:${!!session.file}:${session.parentSessionPath || \"\"}`).join(\"|\");\n      const hasNested = all.some((session) => session.id === \"nested\");\n      return `${typeof SessionManager}:${path.basename(getAgentDir())}:${typeof VERSION}:${listedText}:${hasNested}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/model.ts","content":"export default function(pi) {\n  pi.registerCommand(\"modelscope\", {\n    description: \"Set runtime model\",\n    handler: async () => `${await pi.setModel({ provider: \"runtime\", id: \"runtime-small\" })}`,\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/session.ts","content":"export default function(pi) {\n  pi.registerCommand(\"sessionmeta\", {\n    description: \"Get and set session metadata\",\n    handler: async () => {\n      const before = pi.getSessionName() || \"\";\n      pi.setSessionName(\"from-extension\");\n      return `${before}:${pi.getSessionName() || \"\"}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/session-entry.ts","content":"export default function(pi) {\n  pi.registerCommand(\"sessionentry\", {\n    description: \"Persist extension session entries\",\n    handler: async () => {\n      pi.appendEntry(\"state-note\", { ok: true });\n      pi.setLabel(\"entry-target\", \"checkpoint\");\n      return \"entries\";\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/theme.ts","content":"export default function(pi) {\n  pi.registerCommand(\"themectl\", {\n    description: \"Use extension theme API\",\n    handler: async (_args, ctx) => {\n      const names = ctx.ui.getAllThemes().map((theme) => theme.name).sort().join(\",\");\n      const before = ctx.ui.theme.name;\n      const found = ctx.ui.getTheme(\"light\");\n      const set = ctx.ui.setTheme({ name: \"light\" });\n      const missing = ctx.ui.setTheme(\"no-such-theme\");\n      return `${before}:${names}:${found ? found.name : \"none\"}:${set.success}:${missing.success}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/tools-expanded.ts","content":"export default function(pi) {\n  pi.registerCommand(\"toolsexpanded\", {\n    description: \"Toggle tools expanded UI state\",\n    handler: async (_args, ctx) => {\n      const before = ctx.ui.getToolsExpanded();\n      ctx.ui.setToolsExpanded(!before);\n      return `${before}:${ctx.ui.getToolsExpanded()}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/runtime-actions.ts","content":"export default function(pi) {\n  pi.registerCommand(\"runtimeactions\", {\n    description: \"Request runtime actions\",\n    handler: async (_args, ctx) => {\n      ctx.abort();\n      ctx.compact({ reason: \"test\" });\n      await ctx.reload();\n      ctx.shutdown();\n      return \"actions\";\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/context.ts","content":"export default function(pi) {\n  pi.registerCommand(\"ctxinfo\", {\n    description: \"Read extension context runtime state\",\n    handler: async (_args, ctx) => {\n      const usage = ctx.getContextUsage();\n      return `${ctx.hasUI}:${ctx.isIdle()}:${ctx.hasPendingMessages()}:${ctx.model ? ctx.model.id : \"none\"}:${usage ? usage.tokens : \"none\"}:${ctx.getSystemPrompt().includes(\"CTX-SYSTEM\")}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/session-manager.ts","content":"export default function(pi) {\n  pi.registerCommand(\"sessionview\", {\n    description: \"Read readonly session manager\",\n    handler: async (_args, ctx) => {\n      const sm = ctx.sessionManager;\n      const entries = sm.getEntries();\n      const leaf = sm.getLeafId() || \"none\";\n      const leafEntry = sm.getLeafEntry();\n      const turn = sm.getEntry(\"turn-0\");\n      const branch = sm.getBranch().map((entry) => entry.id).join(\",\");\n      const header = sm.getHeader();\n      const tree = sm.getTree();\n      const children = sm.getChildren(\"turn-0\").map((entry) => entry.id).join(\",\");\n      return `${sm.getSessionId() || \"none\"}:${sm.getSessionName() || \"\"}:${!!sm.getSessionFile()}:${!!sm.getSessionDir()}:${entries.length}:${leaf}:${leafEntry ? leafEntry.type : \"none\"}:${turn && turn.message ? turn.message.role : \"none\"}:${sm.getLabel(\"turn-0\") || \"\"}:${branch}:${header ? header.id : \"none\"}:${tree.length}:${children}:${sm.getCwd() === ctx.cwd}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/model-registry.ts","content":"export default function(pi) {\n  pi.registerCommand(\"modelregistry\", {\n    description: \"Read model registry\",\n    handler: async (_args, ctx) => {\n      const registry = ctx.modelRegistry;\n      const all = registry.getAll();\n      const available = registry.getAvailable();\n      const current = ctx.model;\n      const found = current ? registry.find(current.provider, current.id) : undefined;\n      const status = current ? registry.getProviderAuthStatus(current.provider) : { configured: false };\n      const auth = found ? await registry.getApiKeyAndHeaders(found) : { ok: false };\n      return `${all.length}:${available.length}:${found ? found.id : \"none\"}:${found ? registry.hasConfiguredAuth(found) : false}:${status.configured}:${current ? registry.getProviderDisplayName(current.provider) : \"none\"}:${auth.ok}:${registry.getError() || \"none\"}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/session-actions.ts","content":"export default function(pi) {\n  pi.registerCommand(\"sessionaction\", {\n    description: \"Request command session actions\",\n    handler: async (args, ctx) => {\n      await ctx.waitForIdle();\n      const [mode, value] = String(args || \"new\").trim().split(/\\s+/, 2);\n      if (mode === \"fork\") {\n        const result = await ctx.fork(value || \"turn-0\", { position: \"at\" });\n        return `fork:${result.cancelled}`;\n      }\n      if (mode === \"forkwith\") {\n        const result = await ctx.fork(value || \"turn-0\", {\n          position: \"at\",\n          withSession: async (next) => {\n            await next.sendMessage({ customType: \"fork-note\", content: \"body\", display: false, details: { ok: true } }, { triggerTurn: false });\n          },\n        });\n        return `forkwith:${result.cancelled}`;\n      }\n      if (mode === \"nav\") {\n        const result = await ctx.navigateTree(value || \"turn-0\", { label: \"from-extension\" });\n        return `nav:${result.cancelled}`;\n      }\n      if (mode === \"navhook\") {\n        const result = await ctx.navigateTree(value || \"turn-0\", { label: \"from-hook\" });\n        return `navhook:${result.cancelled}`;\n      }\n      if (mode === \"navsummary\") {\n        const result = await ctx.navigateTree(value || \"turn-0\", { summarize: true, label: \"summary-request\" });\n        return `navsummary:${result.cancelled}`;\n      }\n      if (mode === \"navcancel\") {\n        const result = await ctx.navigateTree(value || \"turn-0\", { label: \"cancel-tree\" });\n        return `navcancel:${result.cancelled}`;\n      }\n      if (mode === \"switch\") {\n        const result = await ctx.switchSession(value || \"\");\n        return `switch:${result.cancelled}`;\n      }\n      if (mode === \"with\") {\n        const result = await ctx.newSession({\n          setup: async (sm) => {\n            sm.appendCustomEntry(\"setup-note\", { ok: true });\n            sm.appendSessionInfo(\"with-session-name\");\n            const setupMessage = sm.appendMessage({ role: \"user\", content: [{ type: \"text\", text: \"setup user\" }] });\n            sm.appendLabelChange(setupMessage, \"setup-label\");\n            sm.appendCustomMessageEntry(\"setup-message\", \"setup body\", true, { setup: true });\n            sm.appendThinkingLevelChange(\"high\");\n            sm.appendModelChange(\"runtime\", \"runtime-small\");\n            sm.appendCompaction(\"setup compacted\", \"callback-custom-message-5\", 77, { compact: true }, true);\n          },\n          withSession: async (next) => {\n            await next.sendMessage({ customType: \"with-note\", content: \"body\", display: false, details: { ok: true } }, { triggerTurn: false });\n          },\n        });\n        return `with:${result.cancelled}`;\n      }\n      const result = await ctx.newSession(value ? { parentSession: value } : undefined);\n      return `new:${result.cancelled}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/session-branch-actions.ts","content":"export default function(pi) {\n  pi.registerCommand(\"sessionbranch\", {\n    description: \"Request setup-time session tree writes\",\n    handler: async (_args, ctx) => {\n      const result = await ctx.newSession({\n        setup: async (sm) => {\n          const root = sm.appendMessage({ role: \"user\", content: [{ type: \"text\", text: \"branch root\" }] });\n          sm.appendMessage({ role: \"assistant\", content: [{ type: \"text\", text: \"branch old\" }] });\n          sm.branch(root);\n          sm.appendMessage({ role: \"user\", content: [{ type: \"text\", text: \"branch child\" }] });\n          sm.branchWithSummary(root, \"setup branch summary\", { setup: true }, true);\n        },\n      });\n      return `branch:${result.cancelled}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/session-alias-actions.ts","content":"export default function(pi) {\n  pi.registerCommand(\"sessionalias\", {\n    description: \"Request harness-style session manager aliases\",\n    handler: async (_args, ctx) => {\n      const result = await ctx.newSession({\n        setup: async (sm) => {\n          const root = sm.appendMessage({ role: \"user\", content: [{ type: \"text\", text: \"alias root\" }] });\n          sm.appendSessionName(\"alias-session\");\n          sm.appendLabel(root, \"alias-label\");\n        },\n      });\n      return `alias:${result.cancelled}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/lifecycle.ts","content":"const fs = require(\"node:fs\");\nexport default function(pi) {\n  pi.on(\"before_agent_start\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `before_agent_start ${event.prompt}\\n`);\n    return {\n      message: { role: \"user\", content: [{ type: \"text\", text: `injected ${event.prompt}` }] },\n      systemPrompt: `${event.systemPrompt}\\nBEFORE:${event.prompt}`,\n    };\n  });\n  pi.on(\"context\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `context ${event.messages.length}\\n`);\n    return { messages: event.messages.concat([{ role: \"user\", content: [{ type: \"text\", text: \"context extra\" }] }]) };\n  });\n  pi.on(\"agent_start\", async () => {\n    fs.appendFileSync(\"lifecycle.log\", \"agent_start\\n\");\n  });\n  pi.on(\"agent_end\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `agent_end ${event.messages.length}\\n`);\n  });\n  pi.on(\"model_select\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `model_select ${event.source} ${event.previousModel ? event.previousModel.id : \"none\"} ${event.model.id}\\n`);\n  });\n  pi.on(\"thinking_level_select\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `thinking_level_select ${event.previousLevel} ${event.level}\\n`);\n  });\n  pi.on(\"turn_start\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `turn_start ${event.turnIndex}\\n`);\n  });\n  pi.on(\"turn_end\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `turn_end ${event.turnIndex} ${event.toolResults.length}\\n`);\n  });\n  pi.on(\"message_start\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `message_start ${event.message.role}\\n`);\n  });\n  pi.on(\"message_update\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `message_update ${event.assistantMessageEvent.text}\\n`);\n  });\n  pi.on(\"message_end\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `message_end ${event.message.role}\\n`);\n    if (event.message.role === \"assistant\") {\n      return { message: { ...event.message, content: [{ type: \"text\", text: \"rewritten assistant\" }] } };\n    }\n  });\n  pi.on(\"tool_execution_start\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `tool_start ${event.toolName}\\n`);\n  });\n  pi.on(\"tool_execution_update\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `tool_update ${event.toolName}\\n`);\n  });\n  pi.on(\"tool_execution_end\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `tool_end ${event.toolName} ${event.isError}\\n`);\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/session-hooks.ts","content":"const fs = require(\"node:fs\");\nexport default function(pi) {\n  pi.on(\"session_before_switch\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `session_before_switch ${event.reason} ${event.targetSessionFile || \"\"}\\n`);\n    if (event.reason === \"blocked\") return { cancel: true, reason: \"no switch\" };\n    if ((event.targetSessionFile || \"\").includes(\"cancel-target\")) return { cancel: true, reason: \"no switch\" };\n  });\n  pi.on(\"session_before_fork\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `session_before_fork ${event.reason} ${event.entryId || \"\"}\\n`);\n    if (event.entryId === \"blocked-fork\") return { cancelled: true, message: \"no fork\" };\n  });\n  pi.on(\"session_before_compact\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `session_before_compact ${event.turnCount}\\n`);\n    if (event.turnCount === 99) return { cancel: true, reason: \"no compact\" };\n  });\n  pi.on(\"session_before_tree\", async (event) => {\n    const prep = event.preparation || {};\n    fs.appendFileSync(\"lifecycle.log\", `session_before_tree ${prep.targetId || \"\"} ${prep.oldLeafId || \"\"} ${prep.userWantsSummary} ${prep.label || \"\"}\\n`);\n    if (prep.label === \"cancel-tree\") return { cancel: true, reason: \"no tree\" };\n    if (prep.userWantsSummary) return { summary: { summary: `summary from tree ${prep.entriesToSummarize.length}`, details: { source: \"hook\" } }, label: \"summary-hook-label\" };\n    if (prep.label === \"from-hook\") return { label: \"hook-label\" };\n    return { label: prep.label };\n  });\n  pi.on(\"session_tree\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `session_tree ${event.newLeafId || \"\"} ${event.oldLeafId || \"\"} ${event.summaryEntry ? event.summaryEntry.type : \"\"}\\n`);\n  });\n  pi.on(\"session_shutdown\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `session_shutdown ${event.reason} ${event.sessionId || \"\"}\\n`);\n  });\n  pi.on(\"session_start\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `session_start ${event.reason} ${event.sessionId || \"\"}\\n`);\n  });\n  pi.on(\"session_compact\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `session_compact ${event.beforeTurnCount} ${event.afterTurnCount}\\n`);\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/provider.ts","content":"export default function(pi) {\n  pi.registerProvider({\n    name: \"localai\",\n    aliases: [\"local\"],\n    protocol: \"openai\",\n    baseUrl: \"https://local.invalid/v1\",\n    apiKeyEnvVar: \"LOCALAI_API_KEY\",\n    defaultModel: \"local-large\",\n    headers: { \"X-Local\": \"1\" },\n    models: [{ id: \"local-large\", contextWindow: 4242 }, \"local-small\"],\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/provider-unregister.ts","content":"export default function(pi) {\n  pi.registerCommand(\"provideradd\", {\n    description: \"Register provider at runtime\",\n    handler: async () => {\n      pi.registerProvider({\n        name: \"lateai\",\n        protocol: \"openai\",\n        baseUrl: \"https://late.invalid/v1\",\n        apiKeyEnvVar: \"LATEAI_API_KEY\",\n        defaultModel: \"late-small\",\n        models: [{ id: \"late-small\", contextWindow: 777 }],\n      });\n      return \"added\";\n    },\n  });\n  pi.registerCommand(\"providerdrop\", {\n    description: \"Unregister provider by alias\",\n    handler: async () => {\n      pi.unregisterProvider(\"local\");\n      return \"dropped\";\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/provider-hooks.ts","content":"const fs = require(\"node:fs\");\nexport default function(pi) {\n  pi.on(\"before_provider_request\", async (event) => {\n    fs.appendFileSync(\"provider-hooks.log\", `before ${event.payload.model}\\n`);\n    return { ...event.payload, model: \"hook-model\", metadata: { hooked: true } };\n  });\n  pi.on(\"after_provider_response\", async (event) => {\n    fs.appendFileSync(\"provider-hooks.log\", `after ${event.status} ${event.headers[\"x-test\"] || \"\"}\\n`);\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":"dynamic-resources/skills/auto/SKILL.md","content":"---\nname: resource-skill\ndescription: from resources_discover\n---\nUse dynamic skill.\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":"dynamic-resources/prompts/dynamic-prompt.md","content":"---\ndescription: Dynamic prompt\nargument-hint: <topic>\n---\nDynamic prompt says $1 and $@.\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":"dynamic-resources/themes/resource-theme.json","content":"{\"name\":\"resource-theme\",\"colors\":{\"accent\":\"#123456\"}}"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/resources.ts","content":"const fs = require(\"node:fs\");\nexport default function(pi) {\n  pi.on(\"resources_discover\", async (event) => {\n    fs.appendFileSync(\"resources.log\", `resources_discover ${event.reason}\\n`);\n    return {\n      skillPaths: [\"dynamic-resources/skills\"],\n      promptPaths: [\"dynamic-resources/prompts\"],\n      themePaths: [\"dynamic-resources/themes\"],\n    };\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/ui.ts","content":"import { Text } from \"@earendil-works/pi-tui\";\n\nexport default function(pi) {\n  pi.registerMessageRenderer(\"notice\", (message, { expanded }, theme) => {\n    const text = message.content.filter((part) => part.type === \"text\").map((part) => part.text).join(\"\\n\");\n    return new Text(`${theme.fg(\"accent\", \"NOTICE\")} ${text}${expanded ? \" expanded\" : \"\"}`, 0, 0);\n  });\n  pi.registerTool({\n    name: \"ui_tool\",\n    label: \"UI Tool\",\n    description: \"Use extension UI fallback\",\n    parameters: { type: \"object\", properties: {} },\n    async execute(_id, _params, _operations, _system, ctx) {\n      ctx.ui.notify(\"tool notice\");\n      const ok = await ctx.ui.confirm(\"continue?\");\n      const name = await ctx.ui.input(\"name?\", { defaultValue: \"anon\" });\n      const pick = await ctx.ui.select(\"pick?\", [\"one\", \"two\"], { defaultIndex: 1 });\n      return { content: [{ type: \"text\", text: `tool ${ok}:${name}:${pick}` }], details: {} };\n    },\n  });\n  pi.registerCommand(\"uicmd\", {\n    description: \"Use extension UI fallback\",\n    handler: async (_args, ctx) => {\n      ctx.ui.notify(\"command notice\");\n      const ok = await ctx.ui.confirm(\"continue?\");\n      const name = await ctx.ui.input(\"name?\", { defaultValue: \"anon\" });\n      const pick = await ctx.ui.select(\"pick?\", [\"one\", \"two\"], { defaultIndex: 1 });\n      return `ui ${ok}:${name}:${pick}`;\n    },\n  });\n  pi.registerCommand(\"surfacecmd\", {\n    description: \"Use extension UI surfaces\",\n    handler: async (_args, ctx) => {\n      ctx.ui.setStatus(\"sync\", \"ready\");\n      ctx.ui.setWidget(\"hint\", [\"first\", \"second\"], { placement: \"belowEditor\" });\n      ctx.ui.setTitle(\"Surface Title\");\n      ctx.ui.setWorkingMessage(\"surface work\");\n      ctx.ui.setWorkingVisible(false);\n      ctx.ui.setHiddenThinkingLabel(\"surface hidden\");\n      ctx.ui.pasteToEditor(\" pasted\");\n      ctx.ui.setEditorText(\"editor body\");\n      const current = ctx.ui.getEditorText();\n      const edited = await ctx.ui.editor(\"Edit body\", \"prefill\");\n      return `surface ${current}:${edited}`;\n    },\n  });\n  pi.registerCommand(\"messagecmd\", {\n    description: \"Send custom messages\",\n    handler: async (_args, ctx) => {\n      await ctx.sendMessage({ customType: \"notice\", content: [{ type: \"text\", text: \"message body\" }], display: true, details: { count: 1 } }, { triggerTurn: false });\n      await ctx.sendUserMessage(\"queued user\", { deliverAs: \"followUp\" });\n      pi.sendMessage({ customType: \"api-note\", content: \"api body\", display: false });\n      pi.appendEntry(\"state-note\", { ok: true });\n      return \"message sent\";\n    },\n  });\n  pi.registerShortcut(\"ctrl+u\", {\n    description: \"Use UI fallback shortcut\",\n    handler: async (ctx) => {\n      ctx.ui.notify(\"shortcut notice\");\n      return await ctx.ui.input(\"label?\", { defaultValue: \"fallback\" });\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/component-ui.ts","content":"import { CustomEditor } from \"@earendil-works/pi-coding-agent\";\n\nexport default function(pi) {\n  pi.registerCommand(\"componentcmd\", {\n    description: \"Use component factories\",\n    handler: async (_args, ctx) => {\n      ctx.ui.setStatus(\"sync\", \"ready\");\n      ctx.ui.setWidget(\"factory\", () => ({ render: () => [\"factory widget\"] }), { placement: \"belowEditor\" });\n      ctx.ui.setHeader(() => ({ render: () => [\"header component\"] }));\n      ctx.ui.setFooter((_tui, _theme, footer) => ({ render: () => [`footer ${footer.getStatus(\"sync\") || \"none\"}`] }));\n      const result = await ctx.ui.custom((_tui, _theme, _keybindings, done) => {\n        done(\"done-value\");\n        return { render: () => [\"custom component\"] };\n      });\n      class MiniEditor extends CustomEditor {\n        render() { return [\"mini editor\"]; }\n      }\n      ctx.ui.setEditorComponent((tui, theme, keybindings) => new MiniEditor(tui, theme, keybindings));\n      const hasEditor = typeof ctx.ui.getEditorComponent() === \"function\";\n      return `component ${result}:${hasEditor}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/component-overlay.ts","content":"export default function(pi) {\n  pi.registerCommand(\"overlaycmd\", {\n    description: \"Use overlay component handles\",\n    handler: async (_args, ctx) => {\n      const result = await ctx.ui.custom((tui, _theme, _keybindings, done) => {\n        const direct = tui.showOverlay({ width: 33, render: () => [\"direct overlay\"] }, { width: 33, nonCapturing: true });\n        direct.setHidden(true);\n        direct.hide();\n        done(\"overlay-done\");\n        return { width: 41, render: () => [\"overlay component\"] };\n      }, {\n        overlay: true,\n        overlayOptions: () => ({ width: 42, nonCapturing: true }),\n        onHandle: (handle) => {\n          handle.focus();\n          handle.setHidden(true);\n          handle.unfocus();\n          handle.hide();\n        },\n      });\n      return `overlay ${result}`;\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/runtime-provider.ts","content":"const fs = require(\"node:fs\");\nexport default function(pi) {\n  pi.registerProvider({\n    name: \"runtimeai\",\n    aliases: [\"runtime\"],\n    defaultModel: \"runtime-small\",\n    models: [{ id: \"runtime-small\", contextWindow: 9001 }],\n    complete: async (request) => {\n      fs.appendFileSync(\"runtime-request.log\", JSON.stringify(request.messages) + \"\\n\");\n      return {\n        content: [{ type: \"text\", text: `runtime ${request.model}:${request.system}:${request.messages.length}:${request.toolsEnabled}` }],\n        usage: { inputTokens: 12, outputTokens: 3 },\n      };\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/rich-renderer.ts","content":"export default function(pi) {\n  pi.registerRenderer(\"richbox\", {\n    description: \"Rich renderer fallback\",\n    target: \"rich_component\",\n    render: async (event) => ({\n      type: \"panel\",\n      children: [\n        { type: \"markdown\", markdown: `**Rich** ${event.text}` },\n        { type: \"text\", text: \"tail\" },\n      ],\n    }),\n  });\n}\n"}|}
  in
  ignore (Extensions.load ());
  check "rpc Pi get_commands lists extension slash commands"
    ((not node_available)
     ||
     let out = Rpc.handle_command_for_test (Agent.create cfg_for_reset) (j {|{"type":"get_commands"}|}) in
     rpc_success out "get_commands"
     &&
     match rpc_field (rpc_last out) "data" with
     | Some (`Assoc fields) -> (
       match List.assoc_opt "commands" fields with
       | Some (`List commands) ->
         List.exists
           (function
             | `Assoc fields ->
               List.assoc_opt "name" fields = Some (`String "tshello")
               && List.assoc_opt "source" fields = Some (`String "extension_command")
             | _ -> false)
           commands
       | _ -> false)
     | _ -> false);
  check "rpc Pi execute_command runs extension slash command with UI metadata"
    ((not node_available)
     ||
     let out =
       Rpc.handle_command_for_test (Agent.create cfg_for_reset)
         (j {|{"type":"execute_command","command":"/uicmd"}|})
     in
     rpc_success out "execute_command"
     &&
     match rpc_field (rpc_last out) "data" with
     | Some (`Assoc fields) -> (
       match List.assoc_opt "text" fields, List.assoc_opt "ui" fields with
       | Some (`String text), Some (`Assoc ui_fields) -> (
         match List.assoc_opt "requests" ui_fields with
         | Some (`List requests) ->
           contains0 text "ui false:anon:two"
           && List.exists
                (fun request -> Yojson.Safe.Util.member "kind" request = `String "confirm")
                requests
         | _ -> false)
       | _ -> false)
     | _ -> false);
  check "rpc Pi execute_command applies extension thinking level"
    ((not node_available)
     ||
     let agent = Agent.create cfg_for_reset in
     Extensions.clear_active_thinking ();
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/thinkscope"}|}) in
     let data_ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) ->
         List.assoc_opt "text" fields = Some (`String "off:high")
         && List.assoc_opt "thinkingLevel" fields = Some (`String "high")
       | _ -> false
     in
     let ok = rpc_success out "execute_command" && data_ok && (Agent.config agent).Llm.thinking = "high" in
     Extensions.clear_active_thinking ();
     ok);
  check "rpc Pi execute_command applies extension model"
    ((not node_available)
     ||
     let agent = Agent.create cfg_for_reset in
     Extensions.clear_active_model ();
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/modelscope"}|}) in
     let data_ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) -> (
         match List.assoc_opt "model" fields with
         | Some (`Assoc model_fields) ->
           List.assoc_opt "text" fields = Some (`String "true")
           && List.assoc_opt "id" model_fields = Some (`String "runtime-small")
           && List.assoc_opt "provider" model_fields = Some (`String "runtimeai")
         | _ -> false)
       | _ -> false
     in
     let ok = rpc_success out "execute_command" && data_ok && (Agent.config agent).Llm.model = "runtime-small" in
     Extensions.clear_active_model ();
     ok);
  check "rpc Pi execute_command applies extension session name"
    ((not node_available)
     ||
     let session = Session.create_new ~name:"rpc-before" () in
     let agent = Agent.create ~session cfg_for_reset in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionmeta"}|}) in
     let data_ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) ->
         List.assoc_opt "text" fields = Some (`String "rpc-before:from-extension")
         && List.assoc_opt "sessionName" fields = Some (`String "from-extension")
       | _ -> false
     in
     let persisted =
       match Session.read_header session.Session.path with
       | Some info -> info.Session.name = "from-extension"
       | None -> false
     in
     rpc_success out "execute_command" && data_ok && Agent.session_name agent = Some "from-extension" && persisted);
  check "rpc Pi execute_command persists extension session entries"
    ((not node_available)
     ||
     let session = Session.create_new ~name:"entry-session" () in
     let agent = Agent.create ~session cfg_for_reset in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionentry"}|}) in
     let data_ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) -> (
         match List.assoc_opt "sessionEntries" fields with
         | Some (`List entries) -> List.length entries = 2
         | _ -> false)
       | _ -> false
     in
     let entries = Session.load_entries session.Session.path in
     let has_custom =
       List.exists
         (function
           | `Assoc fields ->
             List.assoc_opt "type" fields = Some (`String "custom")
             && List.assoc_opt "customType" fields = Some (`String "state-note")
           | _ -> false)
         entries
     in
     let has_label =
       List.exists
         (function
           | `Assoc fields ->
             List.assoc_opt "type" fields = Some (`String "label")
             && List.assoc_opt "targetId" fields = Some (`String "entry-target")
           | _ -> false)
         entries
     in
     Session.set_name session "entry-session-renamed" (Agent.turns agent);
     rpc_success out "execute_command" && data_ok && has_custom && has_label
     && Session.load_turns session.Session.path = [] && List.length (Session.load_entries session.Session.path) = 2);
  check "rpc Pi execute_command applies extension theme"
    ((not node_available)
     ||
     let _ = Themes.set_active_name ~persist:true "dark" in
     let out = Rpc.handle_command_for_test (Agent.create cfg_for_reset) (j {|{"type":"execute_command","command":"/themectl"}|}) in
     let data_ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) ->
         List.assoc_opt "themeName" fields = Some (`String "light")
         && (match List.assoc_opt "text" fields with
             | Some (`String text) -> contains0 text "dark:" && contains0 text ":light:true:false"
             | _ -> false)
       | _ -> false
     in
     rpc_success out "execute_command" && data_ok && (Themes.current_theme ()).Themes.name = "light");
  check "rpc Pi execute_command exposes extension tools expanded state"
    ((not node_available)
     ||
     let out = Rpc.handle_command_for_test (Agent.create cfg_for_reset) (j {|{"type":"execute_command","command":"/toolsexpanded"}|}) in
     match rpc_field (rpc_last out) "data" with
     | Some (`Assoc fields) ->
       rpc_success out "execute_command"
       && List.assoc_opt "text" fields = Some (`String "false:true")
       && List.assoc_opt "toolsExpanded" fields = Some (`Bool true)
     | _ -> false);
  check "rpc Pi execute_command exposes extension runtime action requests"
    ((not node_available)
     ||
     let out = Rpc.handle_command_for_test (Agent.create cfg_for_reset) (j {|{"type":"execute_command","command":"/runtimeactions"}|}) in
     match rpc_field (rpc_last out) "data" with
     | Some (`Assoc fields) ->
       let compact_ok =
         match List.assoc_opt "compactResults" fields with
         | Some (`List [ `Assoc result_fields ]) -> (
           match List.assoc_opt "request" result_fields, List.assoc_opt "text" result_fields with
           | Some (`Assoc request_fields), Some (`String text) ->
             List.assoc_opt "reason" request_fields = Some (`String "test")
             && contains0 text "Nothing to compact"
           | _ -> false)
         | _ -> false
       in
       rpc_success out "execute_command"
       && List.assoc_opt "text" fields = Some (`String "actions")
       && List.assoc_opt "abortRequested" fields = Some (`Bool true)
       && List.assoc_opt "shutdownRequested" fields = Some (`Bool true)
       && List.assoc_opt "reloadRequested" fields = Some (`Bool true)
       && compact_ok
     | _ -> false);
  check "rpc Pi execute_command exposes extension context runtime state"
    ((not node_available)
     ||
     let out = Rpc.handle_command_for_test (Agent.create cfg_for_reset) (j {|{"type":"execute_command","command":"/ctxinfo"}|}) in
     match rpc_field (rpc_last out) "data" with
     | Some (`Assoc fields) -> (
       match List.assoc_opt "text" fields with
       | Some (`String text) ->
         rpc_success out "execute_command"
         && contains0 text "false:true:false:test-model:"
         && contains0 text ":true"
       | _ -> false)
     | _ -> false);
  Printf.printf "\n%s\n" (if !failures = 0 then "All extension RPC command tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
