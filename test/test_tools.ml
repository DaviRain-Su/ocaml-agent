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
  (* Work in an isolated temp directory. *)
  let dir = Filename.temp_dir "agent_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  let contains0 hay needle =
    try ignore (Str.search_forward (Str.regexp_string needle) hay 0); true with Not_found -> false
  in
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
      {|{"path":".pi/extensions/sdk-core-exports.ts","content":"const fs = require(\"node:fs\");\nconst path = require(\"node:path\");\nconst { AuthStorage, ModelRegistry, SettingsManager, createSyntheticSourceInfo, loadSkills, loadSkillsFromDir, formatSkillsForPrompt, FileAuthStorageBackend, InMemoryAuthStorageBackend } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"sdkcore\", {\n    description: \"Probe top-level SDK storage/settings/skills exports\",\n    handler: async () => {\n      const authDir = path.resolve(\"sdk-auth\");\n      fs.mkdirSync(authDir, { recursive: true });\n      const authPath = path.join(authDir, \"auth.json\");\n      const modelsPath = path.join(authDir, \"models.json\");\n      const auth = AuthStorage.create(authPath);\n      auth.set(\"anthropic\", { type: \"api_key\", key: \"stored-anthropic\" });\n      auth.setRuntimeApiKey(\"runtime\", \"runtime-key\");\n      auth.setFallbackResolver((provider) => provider === \"fallback\" ? \"fallback-key\" : undefined);\n      process.env.OPENAI_API_KEY = \"env-openai\";\n      process.env.CUSTOMAI_KEY = \"custom-key\";\n      fs.writeFileSync(modelsPath, JSON.stringify({ providers: { customai: { api: \"openai\", baseUrl: \"https://custom.invalid/v1\", apiKey: \"CUSTOMAI_KEY\", headers: { \"X-Provider\": \"yes\" }, models: [{ id: \"custom-small\", name: \"Custom Small\", contextWindow: 321, headers: { \"X-Model\": \"ok\" } }] } } }), \"utf8\");\n      const authReload = AuthStorage.create(authPath);\n      const memAuth = AuthStorage.inMemory({ mem: { type: \"api_key\", key: \"mem-key\" } });\n      const registry = ModelRegistry.create(auth, modelsPath);\n      const custom = registry.find(\"customai\", \"custom-small\");\n      const customAuth = custom ? await registry.getApiKeyAndHeaders(custom) : { ok: false, headers: {} };\n\n      const settingsAgent = path.resolve(\"sdk-settings-agent\");\n      const settingsCwd = path.resolve(\"sdk-settings-cwd\");\n      fs.mkdirSync(settingsAgent, { recursive: true });\n      fs.mkdirSync(path.join(settingsCwd, \".pi\"), { recursive: true });\n      fs.writeFileSync(path.join(settingsAgent, \"settings.json\"), JSON.stringify({ defaultProvider: \"openai\", defaultModel: \"global-model\", compaction: { enabled: false } }), \"utf8\");\n      fs.writeFileSync(path.join(settingsCwd, \".pi\", \"settings.json\"), JSON.stringify({ compaction: { reserveTokens: 123 } }), \"utf8\");\n      const settings = SettingsManager.create(settingsCwd, settingsAgent);\n      const before = `${settings.getDefaultProvider()}:${settings.getDefaultModel()}:${settings.getCompactionEnabled()}:${settings.getCompactionReserveTokens()}`;\n      settings.setDefaultModelAndProvider(\"zai\", \"glm-4.6\");\n      settings.setCompactionEnabled(true);\n      await settings.flush();\n      const settingsReload = SettingsManager.create(settingsCwd, settingsAgent);\n      const after = `${settingsReload.getDefaultProvider()}:${settingsReload.getDefaultModel()}:${settingsReload.getCompactionEnabled()}`;\n\n      const skillRoot = path.resolve(\"sdk-skills\");\n      fs.mkdirSync(path.join(skillRoot, \"sdk-skill\"), { recursive: true });\n      fs.mkdirSync(path.join(skillRoot, \"hidden-skill\"), { recursive: true });\n      fs.writeFileSync(path.join(skillRoot, \"sdk-skill\", \"SKILL.md\"), \"---\\nname: sdk-skill\\ndescription: Use <sdk> skill\\n---\\nbody\\n\", \"utf8\");\n      fs.writeFileSync(path.join(skillRoot, \"hidden-skill\", \"SKILL.md\"), \"---\\nname: hidden-skill\\ndescription: Hidden skill\\ndisable-model-invocation: true\\n---\\nbody\\n\", \"utf8\");\n      const loaded = loadSkills({ cwd: process.cwd(), agentDir: authDir, skillPaths: [skillRoot], includeDefaults: false });\n      const loadedDir = loadSkillsFromDir({ dir: skillRoot, source: \"path\" });\n      const prompt = formatSkillsForPrompt(loaded.skills);\n      const source = createSyntheticSourceInfo(\"virtual.ts\", { source: \"sdk\", scope: \"project\", baseDir: \"base\" });\n\n      return [\n        typeof AuthStorage,\n        typeof ModelRegistry,\n        typeof SettingsManager,\n        typeof FileAuthStorageBackend,\n        typeof InMemoryAuthStorageBackend,\n        await authReload.getApiKey(\"anthropic\"),\n        await auth.getApiKey(\"runtime\"),\n        await memAuth.getApiKey(\"mem\"),\n        await auth.getApiKey(\"openai\"),\n        await auth.getApiKey(\"fallback\"),\n        authReload.getAuthStatus(\"anthropic\").source,\n        registry.getAll().some((model) => model.provider === \"customai\" && model.id === \"custom-small\" && model.contextWindow === 321),\n        registry.getAvailable().some((model) => model.provider === \"customai\"),\n        registry.hasConfiguredAuth(custom),\n        registry.getProviderAuthStatus(\"customai\").source,\n        registry.getProviderDisplayName(\"openai\"),\n        customAuth.ok,\n        customAuth.apiKey,\n        customAuth.headers && customAuth.headers[\"X-Provider\"],\n        customAuth.headers && customAuth.headers[\"X-Model\"],\n        registry.getError() || \"none\",\n        before,\n        after,\n        source.source,\n        source.scope,\n        source.origin,\n        loaded.skills.length,\n        loadedDir.skills.length,\n        prompt.includes(\"&lt;sdk&gt;\"),\n        prompt.includes(\"hidden-skill\"),\n      ].join(\":\");\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/resource-loader.ts","content":"const fs = require(\"node:fs\");\nconst path = require(\"node:path\");\nconst { DefaultResourceLoader, DefaultPackageManager, loadProjectContextFiles, parseFrontmatter, stripFrontmatter } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"resourceloader\", {\n    description: \"Probe resource loader and package manager exports\",\n    handler: async () => {\n      const root = path.resolve(\"sdk-resource-pkg\");\n      fs.mkdirSync(path.join(root, \"extensions\"), { recursive: true });\n      fs.mkdirSync(path.join(root, \"skills\", \"pkg\"), { recursive: true });\n      fs.mkdirSync(path.join(root, \"prompts\"), { recursive: true });\n      fs.mkdirSync(path.join(root, \"themes\"), { recursive: true });\n      fs.writeFileSync(path.join(root, \"package.json\"), JSON.stringify({ pi: { extensions: [\"extensions/pkg-ext.ts\"], skills: [\"skills\"], prompts: [\"prompts\"], themes: [\"themes\"] } }), \"utf8\");\n      fs.writeFileSync(path.join(root, \"extensions\", \"pkg-ext.ts\"), \"export default function(pi) { pi.registerCommand(\\\"pkgext\\\", { description: \\\"pkg\\\", handler: async () => \\\"pkg\\\" }); }\\n\", \"utf8\");\n      fs.writeFileSync(path.join(root, \"skills\", \"pkg\", \"SKILL.md\"), \"---\\nname: pkg-skill\\ndescription: Package skill\\n---\\nbody\\n\", \"utf8\");\n      fs.writeFileSync(path.join(root, \"prompts\", \"pkg-prompt.md\"), \"---\\ndescription: Package prompt\\nargument-hint: <x>\\n---\\nPrompt body\\n\", \"utf8\");\n      fs.writeFileSync(path.join(root, \"themes\", \"pkg-theme.json\"), JSON.stringify({ name: \"pkg-theme\", colors: { accent: \"#abcdef\" } }), \"utf8\");\n\n      const agentDir = path.resolve(\"sdk-resource-agent\");\n      const cwd = path.resolve(\"sdk-resource-cwd\", \"project\");\n      fs.mkdirSync(agentDir, { recursive: true });\n      fs.mkdirSync(cwd, { recursive: true });\n      fs.writeFileSync(path.join(agentDir, \"AGENTS.md\"), \"GLOBAL_CTX\", \"utf8\");\n      fs.writeFileSync(path.join(cwd, \"AGENTS.md\"), \"PROJECT_CTX\", \"utf8\");\n\n      const pm = new DefaultPackageManager({ cwd, agentDir });\n      const resolved = await pm.resolveExtensionSources([root], { temporary: true });\n      const loader = new DefaultResourceLoader({ cwd, agentDir, additionalSkillPaths: [path.join(root, \"skills\")], additionalPromptTemplatePaths: [path.join(root, \"prompts\")], additionalThemePaths: [path.join(root, \"themes\")], noExtensions: true });\n      await loader.reload();\n      const extra = path.resolve(\"sdk-resource-extra\");\n      fs.mkdirSync(path.join(extra, \"extra-skill\"), { recursive: true });\n      fs.writeFileSync(path.join(extra, \"extra-skill\", \"SKILL.md\"), \"---\\nname: extra-skill\\ndescription: Extra skill\\n---\\nbody\\n\", \"utf8\");\n      loader.extendResources({ skillPaths: [{ path: extra, metadata: { source: \"extension\", scope: \"project\", origin: \"top-level\" } }] });\n      const contexts = loadProjectContextFiles({ cwd, agentDir }).map((entry) => entry.content).join(\"|\");\n      const front = parseFrontmatter(\"---\\nname: parsed\\n---\\nBody\");\n      return [\n        typeof DefaultResourceLoader,\n        typeof DefaultPackageManager,\n        typeof loadProjectContextFiles,\n        resolved.extensions.some((entry) => entry.path.endsWith(\"pkg-ext.ts\") && entry.enabled),\n        resolved.skills.some((entry) => entry.path.endsWith(\"SKILL.md\") && entry.enabled),\n        resolved.prompts.some((entry) => entry.path.endsWith(\"pkg-prompt.md\") && entry.enabled),\n        resolved.themes.some((entry) => entry.path.endsWith(\"pkg-theme.json\") && entry.enabled),\n        loader.getSkills().skills.some((skill) => skill.name === \"pkg-skill\"),\n        loader.getPrompts().prompts.some((prompt) => prompt.name === \"pkg-prompt\"),\n        loader.getThemes().themes.some((theme) => theme.name === \"pkg-theme\"),\n        loader.getSkills().skills.some((skill) => skill.name === \"extra-skill\"),\n        contexts.includes(\"GLOBAL_CTX\") && contexts.includes(\"PROJECT_CTX\"),\n        front.data.name,\n        stripFrontmatter(\"---\\nname: stripped\\n---\\nBody\").trim(),\n      ].join(\":\");\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/sdk-utilities.ts","content":"const { DEFAULT_COMPACTION_SETTINGS, calculateContextTokens, collectEntriesForBranchSummary, estimateTokens, findCutPoint, findTurnStartIndex, formatDimensionNote, generateBranchSummary, generateSummary, getLanguageFromPath, getLastAssistantUsage, getMarkdownTheme, getSelectListTheme, getSettingsListTheme, getShellConfig, highlightCode, initTheme, parseFrontmatter, prepareBranchEntries, resizeImage, serializeConversation, shouldCompact, Theme } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"sdkutilities\", {\n    description: \"Probe compaction and utility exports\",\n    handler: async () => {\n      const entries = [\n        { type: \"message\", id: \"u1\", parentId: null, message: { role: \"user\", content: \"hello world\" } },\n        { type: \"message\", id: \"a1\", parentId: \"u1\", message: { role: \"assistant\", content: [{ type: \"text\", text: \"answer\" }, { type: \"toolCall\", name: \"read\", arguments: { path: \"lib/a.ml\" } }], usage: { input: 10, output: 3, cacheRead: 2, cacheWrite: 1 } } },\n        { type: \"message\", id: \"tr1\", parentId: \"a1\", message: { role: \"toolResult\", content: [{ type: \"text\", text: \"tool result\" }] } },\n        { type: \"message\", id: \"u2\", parentId: \"tr1\", message: { role: \"user\", content: [{ type: \"text\", text: \"next task text long enough\" }] } },\n        { type: \"message\", id: \"a2\", parentId: \"u2\", message: { role: \"assistant\", content: [{ type: \"thinking\", thinking: \"think\" }, { type: \"text\", text: \"done\" }] } },\n      ];\n      const byId = new Map(entries.map((entry) => [entry.id, entry]));\n      const session = {\n        getEntry: (id) => byId.get(id),\n        getBranch: (id) => {\n          const branch = [];\n          let current = byId.get(id);\n          while (current) {\n            branch.unshift(current);\n            current = current.parentId ? byId.get(current.parentId) : undefined;\n          }\n          return branch;\n        },\n      };\n      const cut = findCutPoint(entries, 0, entries.length, 3);\n      const collected = collectEntriesForBranchSummary(session, \"a2\", \"u1\");\n      const prepared = prepareBranchEntries(entries, 100);\n      const summary = await generateSummary([{ role: \"user\", content: \"summarize me\" }], { maxTokens: 4096 }, 1024, undefined, undefined, undefined, undefined, undefined, undefined, async () => ({ stopReason: \"stop\", content: [{ type: \"text\", text: \"summary\" }] }));\n      const branch = await generateBranchSummary(entries, { model: { contextWindow: 128000, maxTokens: 4096 }, apiKey: \"test\", streamFn: async () => ({ stopReason: \"stop\", content: [{ type: \"text\", text: \"branch summary\" }] }) });\n      const theme = new Theme({ accent: \"#ffffff\" }, { selectedBg: \"#000000\" }, \"truecolor\", { name: \"probe\" });\n      initTheme(\"dark\");\n      const md = getMarkdownTheme();\n      const select = getSelectListTheme();\n      const settings = getSettingsListTheme();\n      const front = parseFrontmatter(\"---\\nname: parsed\\n---\\nBody\");\n      const resized = await resizeImage(new Uint8Array([1, 2, 3]), \"image/png\", { maxWidth: 1 });\n      return [\n        typeof DEFAULT_COMPACTION_SETTINGS,\n        DEFAULT_COMPACTION_SETTINGS.reserveTokens,\n        calculateContextTokens({ input: 10, output: 2, cacheRead: 3, cacheWrite: 4 }),\n        estimateTokens({ role: \"user\", content: \"12345\" }),\n        serializeConversation(entries.map((entry) => entry.message)).includes(\"[Assistant tool calls]: read(path=\\\"lib/a.ml\\\")\"),\n        shouldCompact(901, 1000, { enabled: true, reserveTokens: 100 }),\n        shouldCompact(901, 1000, { enabled: false, reserveTokens: 100 }),\n        getLastAssistantUsage(entries).input,\n        findTurnStartIndex(entries, 2, 0),\n        cut.firstKeptEntryIndex,\n        cut.turnStartIndex,\n        cut.isSplitTurn,\n        collected.entries.length,\n        collected.commonAncestorId,\n        prepared.messages.length,\n        prepared.fileOps.read.has(\"lib/a.ml\"),\n        summary,\n        branch.summary.includes(\"branch summary\"),\n        getShellConfig(\"/bin/sh\").shell.endsWith(\"/bin/sh\"),\n        formatDimensionNote({ wasResized: true, originalWidth: 4000, originalHeight: 2000, width: 1000, height: 500 }).includes(\"Multiply coordinates by 4.00\"),\n        resized === null,\n        theme.name,\n        theme.fg(\"accent\", \"x\").includes(\"x\"),\n        typeof md.heading,\n        typeof select.selectedText,\n        typeof settings.label,\n        Array.isArray(highlightCode(\"let x = 1\", \"ocaml\")),\n        getLanguageFromPath(\"lib/a.ml\"),\n        front.frontmatter.name,\n        front.data.name,\n      ].join(\":\");\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/sdk-session-core.ts","content":"const { CURRENT_SESSION_VERSION, buildSessionContext, convertToLlm, getLatestCompactionEntry, migrateSessionEntries, parseSessionEntries, parseSkillBlock } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"sdksessioncore\", {\n    description: \"Probe session/message helper exports\",\n    handler: async () => {\n      const skill = parseSkillBlock(\"<skill name=\\\"zig\\\" location=\\\"/tmp/SKILL.md\\\">\\nbody\\n</skill>\\n\\nuser msg\");\n      const raw = [\n        { type: \"session\", id: \"legacy\", timestamp: \"2024-01-01T00:00:00.000Z\", cwd: \"/tmp\" },\n        { type: \"message\", timestamp: \"2024-01-01T00:00:01.000Z\", message: { role: \"user\", content: \"old\" } },\n        { type: \"message\", timestamp: \"2024-01-01T00:00:02.000Z\", message: { role: \"hookMessage\", content: \"hook\" } },\n        { type: \"compaction\", timestamp: \"2024-01-01T00:00:03.000Z\", summary: \"legacy summary\", tokensBefore: 11, firstKeptEntryIndex: 1 },\n      ];\n      migrateSessionEntries(raw);\n\n      const entries = [\n        { type: \"message\", id: \"u1\", parentId: null, timestamp: \"2024-01-01T00:00:01.000Z\", message: { role: \"user\", content: \"first\" } },\n        { type: \"message\", id: \"a1\", parentId: \"u1\", timestamp: \"2024-01-01T00:00:02.000Z\", message: { role: \"assistant\", provider: \"p\", model: \"m\", content: [{ type: \"text\", text: \"answer\" }] } },\n        { type: \"branch_summary\", id: \"b1\", parentId: \"a1\", timestamp: \"2024-01-01T00:00:03.000Z\", fromId: \"old\", summary: \"branch\" },\n        { type: \"compaction\", id: \"c1\", parentId: \"b1\", timestamp: \"2024-01-01T00:00:04.000Z\", firstKeptEntryId: \"a1\", summary: \"compact\", tokensBefore: 123 },\n        { type: \"custom_message\", id: \"cm1\", parentId: \"c1\", timestamp: \"2024-01-01T00:00:05.000Z\", customType: \"note\", content: \"custom body\", display: true },\n        { type: \"thinking_level_change\", id: \"th1\", parentId: \"cm1\", timestamp: \"2024-01-01T00:00:06.000Z\", thinkingLevel: \"high\" },\n      ];\n      const ctx = buildSessionContext(entries, \"th1\");\n      const converted = convertToLlm([\n        { role: \"bashExecution\", command: \"echo hi\", output: \"hi\", exitCode: 0, cancelled: false, truncated: false, timestamp: 1 },\n        { role: \"bashExecution\", command: \"hidden\", output: \"secret\", excludeFromContext: true, timestamp: 2 },\n        { role: \"custom\", customType: \"note\", content: \"custom text\", display: true, timestamp: 3 },\n        ...ctx.messages,\n      ]);\n      const parsed = parseSessionEntries('{\"type\":\"session\",\"id\":\"s\"}\\nnot-json\\n{\"type\":\"message\",\"id\":\"m\"}\\n');\n      const latest = getLatestCompactionEntry(entries);\n      return [\n        CURRENT_SESSION_VERSION,\n        skill.name,\n        skill.location.endsWith(\"SKILL.md\"),\n        skill.content,\n        skill.userMessage,\n        raw[0].version,\n        !!raw[1].id,\n        raw[2].parentId === raw[1].id,\n        raw[2].message.role,\n        raw[3].firstKeptEntryId === raw[1].id,\n        ctx.messages.length,\n        ctx.messages[0].role,\n        ctx.messages[1].role,\n        ctx.messages[2].role,\n        ctx.messages[3].customType,\n        ctx.thinkingLevel,\n        ctx.model.provider,\n        ctx.model.modelId,\n        converted.length,\n        converted[0].content[0].text.includes(\"Ran `echo hi`\"),\n        converted.some((message) => message.content && JSON.stringify(message.content).includes(\"compact\")),\n        parsed.length,\n        latest.id,\n      ].join(\":\");\n    },\n  });\n}\n"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/extensions/sdk-runtime.ts","content":"const fs = require(\"node:fs\");\nconst path = require(\"node:path\");\nconst { AgentSession, AgentSessionRuntime, ArminComponent, AssistantMessageComponent, FooterComponent, InteractiveMode, RpcClient, ToolExecutionComponent, UserMessageComponent, createAgentSession, createAgentSessionFromServices, createAgentSessionRuntime, createAgentSessionServices, main, runPrintMode, truncateToVisualLines } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"sdkruntime\", {\n    description: \"Probe programmatic SDK and UI component exports\",\n    handler: async () => {\n      const agentDir = path.resolve(\"sdk-runtime-agent\");\n      const services = await createAgentSessionServices({ cwd: process.cwd(), agentDir });\n      const created = await createAgentSession({ cwd: process.cwd(), agentDir, customTools: [{ name: \"custom_sdk\", description: \"custom\", parameters: { type: \"object\" }, execute: async () => ({ content: [{ type: \"text\", text: \"custom\" }] }) }] });\n      const session = created.session;\n      let commandActionCalled = false;\n      await session.bindExtensions({ uiContext: { notify: () => {} }, commandContextActions: { waitForIdle: async () => {}, newSession: async () => { commandActionCalled = true; return { cancelled: false }; }, fork: async () => ({ cancelled: false }), navigateTree: async () => ({ cancelled: false }), switchSession: async () => ({ cancelled: false }), reload: async () => {} } });\n      const dynamicTool = session.getAllTools().some((tool) => tool.name === \"session_dynamic\");\n      const sessionCommand = session.getCommands().some((command) => command.name === \"sessioncmd\");\n      const actionCommand = session.extensionRunner.getCommand(\"sessionaction\");\n      const actionText = actionCommand ? await actionCommand.handler(\"new\", session.extensionRunner.createCommandContext()) : \"missing\";\n      const boundHasUi = session.extensionRunner.hasUI();\n      session.setActiveToolsByName([\"read\", \"custom_sdk\"]);\n      await session.prompt(\"hello sdk runtime\");\n      const stats = session.getSessionStats();\n      const usage = session.getContextUsage();\n      const jsonPath = session.exportToJsonl(\"sdk-runtime-session.jsonl\");\n      const htmlPath = await session.exportToHtml(\"sdk-runtime.html\");\n      const fromServices = await createAgentSessionFromServices(services, { customTools: [{ name: \"svc_tool\", description: \"svc\", parameters: { type: \"object\" } }] });\n      const runtimeResult = await createAgentSessionRuntime({ cwd: process.cwd(), agentDir });\n      await runtimeResult.runtime.newSession({ setup: async (manager) => manager.appendCustomMessageEntry(\"runtime-note\", \"body\", true) });\n      const mode = new InteractiveMode(runtimeResult.runtime, { headless: true });\n      const modeCode = await mode.start();\n      const printCode = await runPrintMode(runtimeResult.runtime, { prompt: \"print prompt\" });\n      const mainCode = await main({ cwd: process.cwd(), agentDir, mode: \"print\", prompt: \"main prompt\" });\n      const rpc = new RpcClient({ cliPath: \"missing-cli.js\" });\n      const trunc = truncateToVisualLines(\"a\\nb\\nc\", 2, 80, 0);\n      const userLines = new UserMessageComponent({ content: \"user component\" }).render(80).join(\"|\");\n      const assistantLines = new AssistantMessageComponent({ message: { content: [{ type: \"text\", text: \"assistant component\" }] } }).render(80).join(\"|\");\n      const toolLines = new ToolExecutionComponent({ name: \"tool component\" }).render(80).join(\"|\");\n      const footerLines = new FooterComponent(\"footer component\").render(80).join(\"|\");\n      const armin = new ArminComponent(\"armin component\").render(80).join(\"|\");\n      return [\n        typeof AgentSession,\n        session instanceof AgentSession,\n        created.extensionsResult && Array.isArray(created.extensionsResult.extensions),\n        session.getActiveToolNames().join(\",\"),\n        session.getAllTools().some((tool) => tool.name === \"read\"),\n        session.getAllTools().some((tool) => tool.name === \"custom_sdk\"),\n        stats.totalMessages,\n        usage.tokens > 0,\n        fs.existsSync(jsonPath),\n        fs.existsSync(htmlPath),\n        fromServices.session instanceof AgentSession,\n        fromServices.session.getAllTools().some((tool) => tool.name === \"svc_tool\"),\n        runtimeResult.runtime instanceof AgentSessionRuntime,\n        runtimeResult.runtime.session.getSessionStats().totalMessages,\n        modeCode,\n        mode.running,\n        printCode,\n        mainCode,\n        typeof rpc.onEvent,\n        trunc.visualLines.join(\",\"),\n        trunc.skippedCount,\n        userLines.includes(\"user component\"),\n        assistantLines.includes(\"assistant component\"),\n        toolLines.includes(\"tool component\"),\n        footerLines.includes(\"footer component\"),\n        armin.includes(\"armin component\"),\n        dynamicTool,\n        sessionCommand,\n        actionText === \"new:false\",\n        commandActionCalled,\n        boundHasUi,\n      ].join(\":\");\n    },\n  });\n}\n"}|}
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
       && List.for_all (fun index -> List.nth parts index = "true") [ 2; 4; 5; 7; 8; 9; 10; 11; 12; 15; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30 ]
     | None -> false);
  check "TypeScript extension tool ctx.ui fallback returns defaults"
    ((not node_available)
     ||
     match Tools.find "ui_tool" with
     | Some t ->
       let output = t.Tools.execute (`Assoc []) in
       contains0 output "tool notice" && contains0 output "tool false:anon:two"
     | None -> false);
  check "TypeScript extension registerProvider appears in provider status"
    ((not node_available) || List.mem_assoc "localai" (Llm.provider_status ()));
  check "TypeScript extension registerProvider adds models"
    ((not node_available)
     ||
     Models.context_window "local-large" = Some 4242
     && List.exists
          (fun (e : Models.entry) -> e.provider = "localai" && e.id = "local-small")
          (Models.list ~pat:"localai" ()));
  Unix.putenv "LOCALAI_API_KEY" "local-key";
  check "TypeScript extension registerProvider configures LLM provider"
    ((not node_available)
     ||
     let cfg = Llm.config_for "local" in
     cfg.Llm.provider = Llm.Openai && cfg.base_url = "https://local.invalid/v1"
     && cfg.api_key = "local-key" && cfg.model = "local-large"
     && List.mem "X-Local: 1" cfg.extra_headers);
  Unix.putenv "LOCALAI_API_KEY" "";
  check "TypeScript extension registerProvider runtime executes without API key"
    ((not node_available)
     ||
     let cfg = Llm.config_for "runtime" in
     let streamed = Buffer.create 32 in
     let blocks, usage =
       Llm.complete cfg ~system:"sys" ~tools_enabled:false
         ~on_text:(fun text -> Buffer.add_string streamed text)
         [ { Llm.role = Llm.User; content = [ Llm.Text "hi" ] } ]
     in
     cfg.Llm.runtime <> None && cfg.api_key = "" && usage.Llm.input_tokens = 12
     && usage.Llm.output_tokens = 3
     &&
     match blocks with
     | [ Llm.Text text ] -> text = "runtime runtime-small:sys:1:false" && Buffer.contents streamed = text
     | _ -> false);
  check "TypeScript extension runtime registerProvider updates provider registry"
    ((not node_available)
     ||
     match Extensions.execute_command "/provideradd" with
     | Some output ->
       output = "added"
       && List.mem_assoc "lateai" (Llm.provider_status ())
       && Models.context_window "late-small" = Some 777
     | None -> false);
  Unix.putenv "LATEAI_API_KEY" "late-key";
  check "TypeScript extension runtime registerProvider configures LLM provider"
    ((not node_available)
     ||
     let cfg = Llm.config_for "lateai" in
     cfg.Llm.provider = Llm.Openai && cfg.base_url = "https://late.invalid/v1"
     && cfg.api_key = "late-key" && cfg.model = "late-small");
  Unix.putenv "LATEAI_API_KEY" "";
  check "TypeScript extension unregisterProvider removes provider and models"
    ((not node_available)
     ||
     match Extensions.execute_command "/providerdrop" with
     | Some output ->
       output = "dropped"
       && not (List.mem_assoc "localai" (Llm.provider_status ()))
       && not
            (List.exists
               (fun (e : Models.entry) -> e.provider = "localai")
               (Models.list ~pat:"localai" ()))
     | None -> false);
  check "TypeScript extension registerFlag/getFlag uses default values"
    ((not node_available)
     ||
     match Extensions.execute_command "/flagshow" with
     | Some output -> output = "calm:false"
     | None -> false);
  Unix.putenv "PI_FLAG_VOICE" "loud";
  Unix.putenv "PI_FLAG_DRY_RUN" "true";
  check "TypeScript extension getFlag reads Pi env overrides"
    ((not node_available)
     ||
     match Extensions.execute_command "/flagshow" with
     | Some output -> output = "loud:true"
     | None -> false);
  Unix.putenv "PI_FLAG_VOICE" "";
  Unix.putenv "PI_FLAG_DRY_RUN" "";
  check "TypeScript extension registerShortcut exposes handler output"
    ((not node_available)
     ||
     match Extensions.execute_shortcut "ctrl+g" with
     | Some (Extensions.Shortcut_output output) -> output = "shortcut calm"
     | _ -> false);
  check "TypeScript extension registerShortcut exposes command action"
    ((not node_available)
     ||
     match Extensions.execute_shortcut "C-h" with
     | Some (Extensions.Shortcut_command command) -> command = "/tshello Shortcut"
     | _ -> false);
  check "TypeScript extension shortcut ctx.ui fallback returns defaults"
    ((not node_available)
     ||
     match Extensions.execute_shortcut "C-u" with
     | Some (Extensions.Shortcut_output output) -> contains0 output "shortcut notice" && contains0 output "fallback"
     | _ -> false);
  check "TypeScript extension shortcut exposes structured UI requests"
    ((not node_available)
     ||
     match Extensions.execute_shortcut_response "C-u" with
     | Some (Extensions.Shortcut_response_output response) ->
       let ui : Extensions.ui_capture = response.ui in
       let kinds =
         ui.requests
         |> List.filter_map (fun request ->
                match Yojson.Safe.Util.member "kind" request with
                | `String kind -> Some kind
                | _ -> None)
       in
       contains0 response.text "fallback" && List.mem "notify" kinds && List.mem "input" kinds
     | _ -> false);
  check "hotkeys include extension shortcuts"
    ((not node_available) || (contains0 (Commands.hotkeys ()) "ctrl+g" && contains0 (Commands.hotkeys ()) "Show voice shortcut"));
  check "TypeScript extension registerMessageRenderer transforms assistant display text"
    ((not node_available)
     ||
     Extensions.render_text ~kind:"message" ~role:"assistant" "hello" = "[message:assistant] hello");
  check "TypeScript extension registerMessageRenderer transforms tool display text"
    ((not node_available)
     ||
     Extensions.render_text ~kind:"tool_result" ~role:"tool" ~tool_name:"bash" "done" = "[tool_result:bash] done");
  check "TypeScript extension rich renderer object falls back to text"
    ((not node_available)
     ||
     let output = Extensions.render_text ~kind:"rich_component" ~role:"assistant" "hello" in
     contains0 output "Rich" && contains0 output "hello" && contains0 output "tail");
  check "TypeScript extension rich renderer exposes structured components"
    ((not node_available)
     ||
     let response = Extensions.render_response ~kind:"rich_component" ~role:"assistant" "hello" in
     response.components <> []
     && contains0 response.rendered "+--"
     && contains0 response.rendered "Rich"
     && contains0 response.rendered "tail");
  check "TypeScript extension session_start can register tool"
    ((not node_available)
     ||
     match Tools.find "session_dynamic" with
     | Some t -> contains0 (t.Tools.execute (`Assoc [])) "session startup"
     | None -> false);
  check "TypeScript extension session_start can register command"
    ((not node_available)
     ||
     List.mem_assoc "/sessioncmd" (Complete.menu "/sessionc")
     &&
     match Extensions.execute_command "/sessioncmd" with
     | Some output -> contains0 output "session command startup"
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
      let lifecycle_ok = contains0 log "agent_start" && contains0 log "agent_end 2" in
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
        contains0 log "turn_start 3" && contains0 log "turn_end 3 1"
        && contains0 log "message_start user" && contains0 log "message_update delta"
        && contains0 log "tool_start ts_greet" && contains0 log "tool_update ts_greet"
        && contains0 log "tool_end ts_greet false"
      in
      let before_context_ok =
        contains0 log "before_agent_start ask" && contains0 log "context 1"
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
     contains0 log "thinking_level_select off high"
     && contains0 log "model_select set test-model event-model"
     && contains0 log "thinking_level_select high low");
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
        | Extensions.Session_cancel reason -> contains0 reason "no switch"
        | Extensions.Session_continue -> false
      in
      let fork_cancel =
        match Extensions.emit_session_before_fork ~reason:"fork" ~entry_id:"blocked-fork" () with
        | Extensions.Session_cancel reason -> contains0 reason "no fork"
        | Extensions.Session_continue -> false
      in
      let compact_cancel =
        match Extensions.emit_session_before_compact ~turn_count:99 () with
        | Extensions.Session_cancel reason -> contains0 reason "no compact"
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
        contains0 command_msg "Started new session"
        &&
        match Agent.session command_agent with
        | Some session -> session.Session.path <> command_session.Session.path
        | None -> false
      in
      Option.iter Session.close (Agent.session command_agent);
      let log = Tools.read_file_contents "lifecycle.log" in
      let session_events =
        switch_ok && contains0 log "session_before_switch manual next.jsonl"
        && contains0 log "session_before_fork fork blocked-fork"
        && contains0 log "session_before_compact 99" && contains0 log "session_start manual manual-id"
        && contains0 log "session_shutdown manual manual-id" && contains0 log "session_compact 12 7"
        && contains0 log "session_before_switch new" && contains0 log "session_shutdown new"
        && contains0 log "session_start new"
      in
      (session_events, switch_cancel, fork_cancel, compact_cancel, command_new)
    end
  in
  check "TypeScript extension session lifecycle events fire" session_events_ok;
  check "TypeScript extension session_before_switch can cancel" switch_cancel_ok;
  check "TypeScript extension session_before_fork can cancel" fork_cancel_ok;
  check "TypeScript extension session_before_compact can cancel" compact_cancel_ok;
  check "slash new starts a new persisted session" command_new_ok;
  check "rpc Pi navigateTree uses default branch summarizer when hook omits summary"
    ((not node_available)
     ||
     let _ =
       run "write_file"
         {|{"path":".pi/extensions/session-hooks.ts","content":"const fs = require(\"node:fs\");\nexport default function(pi) {\n  pi.on(\"session_before_tree\", async (event) => {\n    const prep = event.preparation || {};\n    fs.appendFileSync(\"lifecycle.log\", `session_before_tree_default ${prep.targetId || \"\"} ${prep.userWantsSummary} ${prep.customInstructions || \"\"} ${prep.replaceInstructions}\\n`);\n    return { label: \"default-summary-label\", customInstructions: \"Hook focus\", replaceInstructions: false };\n  });\n  pi.on(\"session_tree\", async (event) => {\n    fs.appendFileSync(\"lifecycle.log\", `session_tree_default ${event.newLeafId || \"\"} ${event.summaryEntry ? event.summaryEntry.type : \"\"} ${event.fromExtension}\\n`);\n  });\n}\n"}|}
     in
     Tools.write_file_contents "runtime-request.log" "";
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session = Session.create_new ~name:"nav-default-summary" () in
     List.iter (Session.append session) turns;
     let agent = Agent.create ~session ~initial_turns:turns (Llm.config_for "runtime") in
     let results =
       Commands.apply_extension_session_actions agent
         [ j
             {|{"kind":"navigate_tree","targetId":"turn-0","options":{"summarize":true,"label":"default-summary","customInstructions":"Original focus","replaceInstructions":true}}|} ]
     in
     let summary_entry =
       session.Session.entries
       |> List.find_map (function
              | `Assoc fields when List.assoc_opt "type" fields = Some (`String "branch_summary") -> Some fields
              | _ -> None)
     in
     let summary_id =
       match summary_entry with
       | Some fields -> (
         match List.assoc_opt "id" fields with
         | Some (`String id) -> Some id
         | _ -> None)
       | None -> None
     in
     let has_summary =
       match summary_entry with
       | Some fields ->
         List.assoc_opt "parentId" fields = Some `Null
         && List.assoc_opt "fromId" fields = Some (`String "root")
         && List.assoc_opt "fromHook" fields = Some (`Bool false)
         &&
         (match List.assoc_opt "summary" fields with
          | Some (`String summary) -> contains0 summary "runtime runtime-small:"
          | _ -> false)
       | None -> false
     in
     let has_label =
       match summary_id with
       | Some id ->
         List.exists
           (function
             | `Assoc fields ->
               List.assoc_opt "type" fields = Some (`String "label")
               && List.assoc_opt "targetId" fields = Some (`String id)
               && List.assoc_opt "label" fields = Some (`String "default-summary-label")
             | _ -> false)
           session.Session.entries
       | None -> false
     in
     let log = Tools.read_file_contents "runtime-request.log" in
     let lifecycle = Tools.read_file_contents "lifecycle.log" in
     let result_ok =
       match results with
       | [ `Assoc fields ] ->
         List.assoc_opt "kind" fields = Some (`String "navigate_tree")
         && List.assoc_opt "cancelled" fields = Some (`Bool false)
       | _ -> false
     in
     let ok =
       result_ok && has_summary && has_label && Agent.turn_count agent = 0
       && contains0 log "Hook focus" && not (contains0 log "Original focus")
       && contains0 lifecycle "session_tree_default"
       && contains0 lifecycle "branch_summary false"
     in
     Option.iter Session.close (Agent.session agent);
     ok);
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
     | Extensions.Tool_block reason -> contains0 reason "blocked by ts"
     | _ -> false);
  check "TypeScript extension tool_result can replace text"
    ((not node_available)
     ||
     contains0
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
      let replace_ok = contains0 intercepted "(exit 7)" && contains0 intercepted "virtual false" in
      let context_ok = Agent.turn_count user_bash_agent = 1 in
      let before_hidden = Agent.turn_count user_bash_agent in
      let hidden = Agent.run_user_bash ~exclude_from_context:true user_bash_agent "virtual" in
      let hidden_ok =
        contains0 hidden "(exit 7)" && contains0 hidden "virtual true" && Agent.turn_count user_bash_agent = before_hidden
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
     contains0 result "(exit 9)" && contains0 result "ops ops");
  check "TypeScript extension createLocalBashOperations is exported"
    ((not node_available)
     ||
     let ops_agent = Agent.create cfg_for_reset in
     let result = Agent.run_user_bash ops_agent "localops" in
     contains0 result "(exit 0)" && contains0 result "wrapped" && contains0 result "localops");
  check "TypeScript extension createLocalFileOperations is exported"
    ((not node_available)
     ||
     let ops_agent = Agent.create cfg_for_reset in
     let result = Agent.run_user_bash ops_agent "fileops" in
     contains0 result "(exit 0)" && contains0 result "fileops");

  Printf.printf "\n%s\n" (if !failures = 0 then "All tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
