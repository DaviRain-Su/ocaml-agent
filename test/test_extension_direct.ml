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
  let dir = Filename.temp_dir "agent_extension_direct_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

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
  let names = Extensions.load () in
  check "TypeScript extension registers tool"
    ((not node_available) || List.mem "ts_greet" names);
  check "TypeScript extension resources_discover adds skills/prompts/themes"
    ((not node_available)
     ||
     let skill_ok =
       Skills.discover ()
       |> List.exists (fun (skill : Skills.t) -> skill.name = "resource-skill")
     in
     let prompt_ok =
       match Prompts.expand_command "/dynamic-prompt topic extra" with
       | Some body -> contains0 body "Dynamic prompt says topic and topic extra."
       | None -> false
     in
     let theme_ok =
       Themes.discover ()
       |> List.exists (fun (theme : Themes.t) -> theme.name = "resource-theme")
     in
     skill_ok && prompt_ok && theme_ok);
  check "TypeScript extension resources_discover receives reload reason"
    ((not node_available)
     ||
     let _ = Extensions.load ~reason:"reload" () in
     let log = Tools.read_file_contents "resources.log" in
     contains0 log "resources_discover startup" && contains0 log "resources_discover reload");
  check "TypeScript extension before_provider_request mutates provider payload"
    ((not node_available)
     ||
     let original = j {|{"model":"base-model","messages":[]}|} in
     let mutated = Llm.apply_provider_request_hooks original in
     let log = Tools.read_file_contents "provider-hooks.log" in
     Yojson.Safe.Util.member "model" mutated = `String "hook-model"
     && Yojson.Safe.Util.member "hooked" (Yojson.Safe.Util.member "metadata" mutated) = `Bool true
     && contains0 log "before base-model");
  check "TypeScript extension after_provider_response receives response metadata"
    ((not node_available)
     ||
     (Llm.emit_provider_response_hooks ~status:201 ~headers:[ ("x-test", "ok") ] ();
      let log = Tools.read_file_contents "provider-hooks.log" in
      contains0 log "after 201 ok"));
  check "TypeScript extension executes registered tool"
    ((not node_available)
     ||
     match Tools.find "ts_greet" with
     | Some t -> contains0 (t.Tools.execute (`Assoc [ ("name", `String "Pi") ])) "Hello Pi"
     | None -> false);
  check "TypeScript extension tool execute receives file operations"
    ((not node_available)
     ||
     match Tools.find "ops_file" with
     | Some t -> t.Tools.execute (`Assoc []) = "ops body:out.txt:true"
     | None -> false);
  check "TypeScript extension tool execute receives Pi signal/update/context signature"
    ((not node_available)
     ||
     match Tools.find "tool_signature" with
     | Some t -> t.Tools.execute (`Assoc []) = "sig:true:true:function:sig"
     | None -> false);
  check "TypeScript extension tool onUpdate captures surface metadata"
    ((not node_available)
     ||
     match
       Extensions.run_node_bridge
         (j
            {|{"mode":"execute","path":".pi/extensions/tool-signature.ts","tool":"tool_signature","toolCallId":"sig-call","input":{}}|})
     with
     | Ok json ->
       let ui = Yojson.Safe.Util.member "ui" json in
       let surfaces = Yojson.Safe.Util.member "surfaces" ui in
       (match surfaces with
        | `List items ->
          List.exists
            (fun surface ->
              Yojson.Safe.Util.member "kind" surface = `String "tool_update"
              && Yojson.Safe.Util.member "toolCallId" surface = `String "sig-call"
              &&
              match Yojson.Safe.Util.member "text" surface with
              | `String text -> contains0 text "partial update"
              | _ -> false)
            items
        | _ -> false)
     | Error _ -> false);
  check "TypeScript extension tool prepareArguments normalizes execute params"
    ((not node_available)
     ||
     match Tools.find "prepared_tool" with
     | Some t -> t.Tools.execute (`Assoc [ ("raw", `String "input"); ("count", `Int 4) ]) = "prepared:input:5"
     | None -> false);
  check "TypeScript extension command appears in completion"
    ((not node_available) || List.mem_assoc "/tshello" (Complete.menu "/tsh"));
  check "TypeScript extension command argument hint appears in completion"
    ((not node_available)
     ||
     match List.assoc_opt "/argcomplete" (Complete.menu "/argc") with
     | Some detail -> contains0 detail "<source>"
     | None -> false);
  check "TypeScript extension command argument completions feed Tab candidates"
    ((not node_available)
     ||
     let start, cands = Complete.completion "/argcomplete p" in
     start = String.length "/argcomplete " && cands = [ "prompt" ]);
  check "TypeScript extension command executes"
    ((not node_available)
     ||
     match Extensions.execute_command "/tshello Pi" with
     | Some output -> contains0 output "Command Pi"
     | None -> false);
  check "TypeScript extension pi.getCommands exposes sources"
    ((not node_available)
     ||
     let commands =
       [ j
           {|{"name":"promptcmd","description":"Prompt command","source":"prompt","sourceInfo":{"path":"prompt.md","source":"prompt","scope":"project","origin":"top-level"}}|};
         j
           {|{"name":"skill:review","description":"Skill command","source":"skill","sourceInfo":{"path":"skill/SKILL.md","source":"skill","scope":"project","origin":"top-level"}}|} ]
     in
     match Extensions.execute_command_response ~commands "/commandlist" with
     | Some response ->
       contains0 response.Extensions.text "commandlist:extension:<extension-command:commandlist>"
       && contains0 response.Extensions.text "tshello:extension:<extension-command:tshello>"
       && contains0 response.Extensions.text "promptcmd:prompt:prompt.md"
       && contains0 response.Extensions.text "skill:review:skill:skill/SKILL.md"
     | None -> false);
  check "TypeScript extension pi.events emits and unsubscribes handlers"
    ((not node_available)
     ||
     match Extensions.execute_command "/eventbus" with
     | Some output -> output = "cmd:1,cmd:3"
     | None -> false);
  check "TypeScript extension pi.events is shared across loaded extensions"
    ((not node_available)
     ||
     let _ = Tools.write_file_contents "event-bus.log" "" in
     match Extensions.execute_command "/eventemit" with
     | Some output ->
       output = "emitted" && contains0 (Tools.read_file_contents "event-bus.log") "emitter:42"
     | None -> false);
  check "TypeScript extension pi.exec returns stdout stderr and status"
    ((not node_available)
     ||
     match Extensions.execute_command "/execprobe" with
     | Some output -> output = "out:err:0:0:false:7:7:false"
     | None -> false);
  check "TypeScript extension withFileMutationQueue serializes same-file mutations"
    ((not node_available)
     ||
     match Extensions.execute_command "/queueprobe" with
     | Some output -> output = "AB"
     | None -> false);
  check "TypeScript extension exports tool event type guards"
    ((not node_available)
     ||
     match Extensions.execute_command "/guardprobe" with
     | Some output -> output = "function:true:false:true:true:true:true:true:true:true"
     | None -> false);
  check "TypeScript extension exports registered tool wrappers"
    ((not node_available)
     ||
     match Extensions.execute_command "/wrapprobe" with
     | Some output -> output = "function:1:wrapped_tool:Probe wrapRegisteredTool"
     | None -> false);
  check "TypeScript extension wrapped registered tool executes with runner context"
    ((not node_available)
     ||
     match Tools.find "wrapped_tool" with
     | Some t -> t.Tools.execute (`Assoc [ ("value", `String "ok") ]) = "wrapped:ok:true"
     | None -> false);
  check "TypeScript extension exports loader runtime and runner APIs"
    ((not node_available)
     ||
     match Extensions.execute_command "/sdkloader" with
     | Some output ->
       output = "function:0:0:queuedai:true:true:true:true:true:true:true:inline:ok:true"
     | None -> false);
  check "TypeScript extension exports tool factory APIs"
    ((not node_available)
     ||
     match Extensions.execute_command "/factoryprobe" with
     | Some output ->
       output = "read,bash,edit,write:read,grep,find,ls:2000:51200:2.0KB:a,b:b,c:abc:remote:hooked:cmd:spawned"
     | None -> false);
  check "TypeScript extension factory read/write/edit/bash tools execute"
    ((not node_available)
     ||
     let write_ok =
       match Tools.find "factory_write" with
       | Some t -> contains0 (t.Tools.execute (`Assoc [ ("path", `String "factory-tools.txt"); ("content", `String "alpha") ])) "Wrote 5 bytes"
       | None -> false
     in
     let edit_ok =
       match Tools.find "factory_edit" with
       | Some t -> contains0 (t.Tools.execute (`Assoc [ ("path", `String "factory-tools.txt"); ("old_str", `String "alpha"); ("new_str", `String "beta") ])) "Edited factory-tools.txt"
       | None -> false
     in
     let read_ok =
       match Tools.find "factory_read" with
       | Some t -> t.Tools.execute (`Assoc [ ("path", `String "factory-tools.txt") ]) = "beta"
       | None -> false
     in
     let bash_ok =
       match Tools.find "factory_bash" with
       | Some t -> t.Tools.execute (`Assoc [ ("command", `String "printf factory-bash") ]) = "factory-bash"
       | None -> false
     in
     write_ok && edit_ok && read_ok && bash_ok);
  check "TypeScript extension factory grep/find/ls tools execute"
    ((not node_available)
     ||
     let grep_ok =
       match Tools.find "factory_grep" with
       | Some t -> contains0 (t.Tools.execute (`Assoc [ ("pattern", `String "beta"); ("path", `String "."); ("include", `String "factory-tools.txt") ])) "factory-tools.txt:1:beta"
       | None -> false
     in
     let find_ok =
       match Tools.find "factory_find" with
       | Some t -> contains0 (t.Tools.execute (`Assoc [ ("pattern", `String "factory-tools.txt"); ("path", `String ".") ])) "factory-tools.txt"
       | None -> false
     in
     let ls_ok =
       match Tools.find "factory_ls" with
       | Some t -> contains0 (t.Tools.execute (`Assoc [ ("path", `String ".") ])) "factory-tools.txt"
       | None -> false
     in
     grep_ok && find_ok && ls_ok);
  check "TypeScript extension exports SessionManager list APIs"
    ((not node_available)
     ||
     match Extensions.execute_command "/sessionlist" with
     | Some output ->
       contains0 output "function:"
       && contains0 output ":string:"
       && contains0 output "newer:Newer Name:2:new question:true:older.jsonl"
       && contains0 output "older:Older:1:old question:true:"
       && contains0 output ":true"
     | None -> false);
  check "TypeScript extension get/setThinkingLevel updates runtime state"
    ((not node_available)
     ||
     let ok =
       Extensions.clear_active_thinking ();
       match Extensions.execute_command_response "/thinkscope" with
       | Some response ->
         response.Extensions.text = "off:high"
         && response.Extensions.thinking_level = Some "high"
         && Extensions.active_thinking () = Some "high"
       | None -> false
     in
     Extensions.clear_active_thinking ();
     ok);
  check "TypeScript extension setModel updates runtime state"
    ((not node_available)
     ||
     let ok =
       Extensions.clear_active_model ();
       match Extensions.execute_command_response "/modelscope" with
       | Some response -> (
         match response.Extensions.model_choice with
         | Some choice ->
           response.Extensions.text = "true"
           && choice.Extensions.provider = Some "runtime"
           && choice.Extensions.model = Some "runtime-small"
           && Extensions.active_model () = Some choice
         | None -> false)
       | None -> false
     in
     Extensions.clear_active_model ();
     ok);
  check "TypeScript extension get/setSessionName returns runtime state"
    ((not node_available)
     ||
     match Extensions.execute_command_response ~session_name:"before-name" "/sessionmeta" with
     | Some response ->
       response.Extensions.text = "before-name:from-extension"
       && response.Extensions.session_name = Some "from-extension"
     | None -> false);
  check "TypeScript extension appendEntry/setLabel returns session entries"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/sessionentry" with
     | Some response ->
       let has_custom =
         List.exists
           (function
             | `Assoc fields ->
               List.assoc_opt "type" fields = Some (`String "custom")
               && List.assoc_opt "customType" fields = Some (`String "state-note")
             | _ -> false)
           response.Extensions.session_entries
       in
       let has_label =
         List.exists
           (function
             | `Assoc fields ->
               List.assoc_opt "type" fields = Some (`String "label")
               && List.assoc_opt "targetId" fields = Some (`String "entry-target")
               && List.assoc_opt "label" fields = Some (`String "checkpoint")
             | _ -> false)
           response.Extensions.session_entries
       in
       response.Extensions.text = "entries" && has_custom && has_label
     | None -> false);
  check "TypeScript extension theme API returns and sets runtime theme"
    ((not node_available)
     ||
     let themes =
       [ `Assoc [ ("name", `String "dark"); ("path", `Null) ];
         `Assoc [ ("name", `String "light"); ("path", `Null) ] ]
     in
     match Extensions.execute_command_response ~themes ~theme_name:"dark" "/themectl" with
     | Some response ->
       response.Extensions.text = "dark:dark,light:light:true:false"
       && response.Extensions.theme_name = Some "light"
     | None -> false);
  check "TypeScript extension get/setToolsExpanded returns runtime UI state"
    ((not node_available)
     ||
     match Extensions.execute_command_response ~tools_expanded:true "/toolsexpanded" with
     | Some response ->
       response.Extensions.text = "true:false"
       && response.Extensions.tools_expanded = Some false
     | None -> false);
  check "TypeScript extension runtime actions return requests"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/runtimeactions" with
     | Some response ->
       let compact_ok =
         match response.Extensions.compact_requests with
         | [ `Assoc fields ] -> List.assoc_opt "reason" fields = Some (`String "test")
         | _ -> false
       in
       response.Extensions.text = "actions"
       && response.Extensions.abort_requested
       && response.Extensions.shutdown_requested
       && response.Extensions.reload_requested
       && compact_ok
     | None -> false);
  check "TypeScript extension command context exposes runtime state"
    ((not node_available)
     ||
     let model = `Assoc [ ("id", `String "ctx-model"); ("provider", `String "ctx-provider") ] in
     let usage = `Assoc [ ("tokens", `Int 42); ("contextWindow", `Int 100); ("percent", `Float 42.) ] in
     match
       Extensions.execute_command_response ~model ~context_usage:usage ~system_prompt:"CTX-SYSTEM"
         ~has_ui:true ~is_idle:false ~has_pending_messages:true "/ctxinfo"
     with
     | Some response -> response.Extensions.text = "true:false:true:ctx-model:42:true"
     | None -> false);
  check "TypeScript extension readonly sessionManager exposes session snapshot"
    ((not node_available)
     ||
     let info : Session.info =
       { id = "session-id"; path = "/tmp/dir/session.jsonl"; name = "Session Name"; created = 0.; cwd = Sys.getcwd () }
     in
     let turns = [ { Llm.role = Llm.User; content = [ Llm.Text "hello" ] } ] in
     let entries =
       [ `Assoc
           [ ("type", `String "label");
             ("id", `String "label-1");
             ("parentId", `String "turn-0");
             ("targetId", `String "turn-0");
             ("label", `String "mark") ] ]
     in
     let session_context = Extensions.session_context_json ~entries ~info turns in
     match Extensions.execute_command_response ~session_context "/sessionview" with
     | Some response ->
       response.Extensions.text
       = "session-id:Session Name:true:true:2:turn-0:message:user:mark:turn-0:session-id:1:label-1:true"
     | None -> false);
  check "TypeScript extension modelRegistry exposes current and catalog models"
    ((not node_available)
     ||
     let model = `Assoc [ ("id", `String "ctx-model"); ("provider", `String "ctx-provider") ] in
     let models =
       [ `Assoc [ ("id", `String "ctx-model"); ("provider", `String "ctx-provider") ];
         `Assoc [ ("id", `String "other-model"); ("provider", `String "other-provider") ] ]
     in
     match Extensions.execute_command_response ~model ~models "/modelregistry" with
     | Some response -> response.Extensions.text = "2:2:ctx-model:true:true:ctx-provider:true:none"
     | None -> false);
  check "TypeScript extension command session actions return requests"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/sessionaction fork turn-0" with
     | Some response -> (
       match response.Extensions.session_actions with
       | [ `Assoc fields ] ->
         response.Extensions.text = "fork:false"
         && List.assoc_opt "kind" fields = Some (`String "fork")
         && List.assoc_opt "entryId" fields = Some (`String "turn-0")
         && List.assoc_opt "position" fields = Some (`String "at")
       | _ -> false)
     | None -> false);
  check "TypeScript extension fork withSession uses fork leaf"
    ((not node_available)
     ||
     let info : Session.info =
       { id = "fork-session"; path = "/tmp/dir/fork.jsonl"; name = "Fork Session"; created = 0.; cwd = Sys.getcwd () }
     in
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session_context = Extensions.session_context_json ~info turns in
     match Extensions.execute_command_response ~session_context "/sessionaction forkwith turn-0" with
     | Some response -> (
       match response.Extensions.session_actions with
       | [ `Assoc fields ] ->
         let entries =
           match List.assoc_opt "sessionEntries" fields with
           | Some (`List values) -> values
           | _ -> []
         in
         let has_fork_note =
           List.exists
             (function
               | `Assoc entry_fields ->
                 List.assoc_opt "type" entry_fields = Some (`String "custom_message")
                 && List.assoc_opt "id" entry_fields = Some (`String "message-1")
                 && List.assoc_opt "parentId" entry_fields = Some (`String "turn-0")
                 && List.assoc_opt "customType" entry_fields = Some (`String "fork-note")
               | _ -> false)
             entries
         in
         response.Extensions.text = "forkwith:false"
         && List.assoc_opt "kind" fields = Some (`String "fork")
         && List.assoc_opt "entryId" fields = Some (`String "turn-0")
         && List.assoc_opt "position" fields = Some (`String "at")
         && has_fork_note
       | _ -> false)
     | None -> false);
  check "TypeScript extension session action callbacks capture side effects"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/sessionaction with" with
     | Some response -> (
       match response.Extensions.session_actions with
       | [ `Assoc fields ] ->
         let entries =
           match List.assoc_opt "sessionEntries" fields with
           | Some (`List values) -> values
           | _ -> []
         in
         let has_entry typ =
           List.exists
             (function
               | `Assoc entry_fields -> List.assoc_opt "type" entry_fields = Some (`String typ)
               | _ -> false)
             entries
         in
         let entry_has expected =
           List.exists
             (function
               | `Assoc entry_fields ->
                 List.for_all
                   (fun (key, value) -> List.assoc_opt key entry_fields = Some value)
                   expected
               | _ -> false)
             entries
         in
         let has_custom_message custom_type =
           List.exists
             (function
               | `Assoc entry_fields ->
                 List.assoc_opt "type" entry_fields = Some (`String "custom_message")
                 && List.assoc_opt "customType" entry_fields = Some (`String custom_type)
               | _ -> false)
             entries
         in
         response.Extensions.text = "with:false"
         && List.assoc_opt "kind" fields = Some (`String "new_session")
         && List.assoc_opt "sessionName" fields = Some (`String "with-session-name")
         && has_entry "custom" && has_entry "session_info" && has_entry "label"
         && entry_has
              [ ("type", `String "custom");
                ("id", `String "callback-entry-1");
                ("parentId", `Null);
                ("customType", `String "setup-note") ]
         && entry_has
              [ ("type", `String "label");
                ("id", `String "callback-label-4");
                ("parentId", `String "callback-message-3");
                ("targetId", `String "callback-message-3");
                ("label", `String "setup-label") ]
         && has_entry "message"
         && has_entry "thinking_level_change" && has_entry "model_change"
         && has_entry "compaction"
         && has_custom_message "setup-message" && has_custom_message "with-note"
       | _ -> false)
     | None -> false);
  check "TypeScript extension setup session manager supports branch writes"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/sessionbranch" with
     | Some response -> (
       match response.Extensions.session_actions with
       | [ `Assoc fields ] ->
         let entries =
           match List.assoc_opt "sessionEntries" fields with
           | Some (`List values) -> values
           | _ -> []
         in
         let entry_has expected =
           List.exists
             (function
               | `Assoc entry_fields ->
                 List.for_all
                   (fun (key, value) -> List.assoc_opt key entry_fields = Some value)
                   expected
               | _ -> false)
             entries
         in
         response.Extensions.text = "branch:false"
         && List.assoc_opt "kind" fields = Some (`String "new_session")
         && entry_has
              [ ("type", `String "leaf");
                ("id", `String "callback-leaf-3");
                ("parentId", `String "callback-message-2");
                ("targetId", `String "callback-message-1") ]
         && entry_has
              [ ("type", `String "message");
                ("id", `String "callback-message-4");
                ("parentId", `String "callback-message-1") ]
         && entry_has
              [ ("type", `String "branch_summary");
                ("id", `String "callback-branch-summary-5");
                ("parentId", `String "callback-message-1");
                ("fromId", `String "callback-message-1");
                ("summary", `String "setup branch summary");
                ("fromHook", `Bool true) ]
       | _ -> false)
     | None -> false);
  check "TypeScript extension setup session manager supports harness aliases"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/sessionalias" with
     | Some response -> (
       match response.Extensions.session_actions with
       | [ `Assoc fields ] ->
         let entries =
           match List.assoc_opt "sessionEntries" fields with
           | Some (`List values) -> values
           | _ -> []
         in
         let entry_has expected =
           List.exists
             (function
               | `Assoc entry_fields ->
                 List.for_all
                   (fun (key, value) -> List.assoc_opt key entry_fields = Some value)
                   expected
               | _ -> false)
             entries
         in
         response.Extensions.text = "alias:false"
         && List.assoc_opt "kind" fields = Some (`String "new_session")
         && List.assoc_opt "sessionName" fields = Some (`String "alias-session")
         && entry_has
              [ ("type", `String "session_info");
                ("id", `String "callback-session-info-2");
                ("parentId", `String "callback-message-1");
                ("name", `String "alias-session") ]
         && entry_has
              [ ("type", `String "label");
                ("id", `String "callback-label-3");
                ("parentId", `String "callback-session-info-2");
                ("targetId", `String "callback-message-1");
                ("label", `String "alias-label") ]
       | _ -> false)
     | None -> false);
  check "TypeScript extension setActiveTools scopes runtime tools"
    ((not node_available)
     ||
     let ok =
       match Extensions.execute_command "/toolscope" with
       | Some output ->
         let active = Extensions.active_tools () in
         let scoped_schema =
           Yojson.Safe.to_string (`List (Tools.openai_schemas ?allowed:(Extensions.effective_tool_names None) ()))
         in
         contains0 output "true:true:true:read,ts_greet"
         && active = Some [ "read_file"; "ts_greet" ]
         && contains0 scoped_schema "\"name\":\"read\""
         && contains0 scoped_schema "\"name\":\"ts_greet\""
         && not (contains0 scoped_schema "\"name\":\"bash\"")
       | None -> false
     in
     Extensions.clear_active_tools ();
     ok);
  check "TypeScript extension command ctx.ui fallback returns defaults"
    ((not node_available)
     ||
     match Extensions.execute_command "/uicmd" with
     | Some output -> contains0 output "command notice" && contains0 output "ui false:anon:two"
     | None -> false);
  check "TypeScript extension command exposes structured UI requests"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/uicmd" with
     | Some response ->
       let ui : Extensions.ui_capture = response.ui in
       let kinds =
         ui.requests
         |> List.filter_map (fun request ->
                match Yojson.Safe.Util.member "kind" request with
                | `String kind -> Some kind
                | _ -> None)
       in
       contains0 response.text "ui false:anon:two"
       && List.mem "notify" kinds && List.mem "confirm" kinds && List.mem "input" kinds
       && List.mem "select" kinds
     | None -> false);
  check "TypeScript extension command exposes custom UI surfaces"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/surfacecmd" with
     | Some response ->
       let ui : Extensions.ui_capture = response.ui in
       let kinds =
         ui.surfaces
         |> List.filter_map (fun surface ->
                match Yojson.Safe.Util.member "kind" surface with
                | `String kind -> Some kind
                | _ -> None)
       in
       contains0 response.text "surface editor body:prefill"
       && List.mem "status" kinds
       && List.mem "widget" kinds
       && List.mem "title" kinds
       && List.mem "working_message" kinds
       && List.mem "working_visible" kinds
       && List.mem "hidden_thinking_label" kinds
       && List.mem "paste" kinds
       && List.mem "editor_text" kinds
     | None -> false);
  check "TypeScript extension component factories expose rendered lines"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/componentcmd" with
     | Some response ->
       let ui : Extensions.ui_capture = response.ui in
       let lines_for kind =
         ui.surfaces
         |> List.find_map (fun surface ->
                match Yojson.Safe.Util.member "kind" surface with
                | `String got when got = kind -> (
                  match Yojson.Safe.Util.member "lines" surface with
                  | `List lines ->
                    Some
                      (List.filter_map
                         (function `String line -> Some line | _ -> None)
                         lines)
                  | _ -> Some [])
                | _ -> None)
         |> Option.value ~default:[]
       in
       contains0 response.text "component done-value:true"
       && List.mem "factory widget" (lines_for "widget")
       && List.mem "header component" (lines_for "header")
       && List.mem "footer ready" (lines_for "footer")
       && List.mem "custom component" (lines_for "custom")
       && List.mem "mini editor" (lines_for "editor_component")
     | None -> false);
  check "TypeScript extension custom overlay exposes handle surfaces"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/overlaycmd" with
     | Some response ->
       let ui : Extensions.ui_capture = response.ui in
       let lines_contain text surface =
         match Yojson.Safe.Util.member "lines" surface with
         | `List lines -> List.exists (function `String line -> line = text | _ -> false) lines
         | _ -> false
       in
       let custom_overlay =
         ui.surfaces
         |> List.exists (fun surface ->
                Yojson.Safe.Util.member "kind" surface = `String "custom"
                && Yojson.Safe.Util.member "overlay" surface = `Bool true
                && lines_contain "overlay component" surface
                &&
                match Yojson.Safe.Util.member "overlayOptions" surface with
                | `Assoc opts -> List.assoc_opt "width" opts = Some (`Int 42)
                | _ -> false)
       in
       let direct_overlay =
         ui.surfaces
         |> List.exists (fun surface ->
                Yojson.Safe.Util.member "kind" surface = `String "overlay"
                && lines_contain "direct overlay" surface
                &&
                match Yojson.Safe.Util.member "options" surface with
                | `Assoc opts -> List.assoc_opt "width" opts = Some (`Int 33)
                | _ -> false)
       in
       let custom_methods =
         ui.surfaces
         |> List.filter_map (fun surface ->
                match Yojson.Safe.Util.member "kind" surface, Yojson.Safe.Util.member "overlayId" surface with
                | `String "overlay_handle", `String "custom-overlay-1" -> (
                  match Yojson.Safe.Util.member "method" surface with
                  | `String method_ -> Some method_
                  | _ -> None)
                | _ -> None)
       in
       contains0 response.text "overlay overlay-done"
       && custom_overlay && direct_overlay
       && List.mem "focus" custom_methods
       && List.mem "setHidden" custom_methods
       && List.mem "hide" custom_methods
     | None -> false);
  check "TypeScript extension sendMessage captures custom messages"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/messagecmd" with
     | Some response ->
       let ui : Extensions.ui_capture = response.ui in
       let custom_types =
         ui.messages
         |> List.filter_map (fun message ->
                match Yojson.Safe.Util.member "customType" message with
                | `String custom_type -> Some custom_type
                | _ -> None)
       in
       contains0 response.text "message sent"
       && List.mem "notice" custom_types
       && List.mem "user" custom_types
       && List.mem "api-note" custom_types
       && List.mem "state-note" custom_types
     | None -> false);
  check "TypeScript extension registerMessageRenderer renders custom messages"
    ((not node_available)
     ||
     match Extensions.execute_command_response "/messagecmd" with
     | Some response ->
       let notice =
         response.ui.messages
         |> List.find_opt (fun message ->
                Yojson.Safe.Util.member "customType" message = `String "notice")
       in
       (match notice with
        | Some message ->
          let rendered =
            match Yojson.Safe.Util.member "rendered" message with
            | `String s -> s
            | _ -> ""
          in
          let lines =
            match Yojson.Safe.Util.member "lines" message with
            | `List xs -> List.filter_map (function `String s -> Some s | _ -> None) xs
            | _ -> []
          in
          contains0 rendered "NOTICE message body" && List.mem "NOTICE message body" lines
        | None -> false)
     | None -> false);
  Printf.printf "\n%s\n" (if !failures = 0 then "All direct extension tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
