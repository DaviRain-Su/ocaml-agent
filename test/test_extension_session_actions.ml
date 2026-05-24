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
  let dir = Filename.temp_dir "agent_extension_session_actions_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  let session_context_leaf session turns =
    Extensions.session_context_json ?info:(Some (Session.info_of session)) ~entries:session.Session.entries turns
    |> Yojson.Safe.Util.member "leafId"
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
  check "rpc Pi execute_command exposes readonly sessionManager snapshot"
    ((not node_available)
     ||
     let session = Session.create_new ~name:"Rpc Session" () in
     let agent =
       Agent.create ~session ~initial_turns:[ { Llm.role = Llm.User; content = [ Llm.Text "hello" ] } ] cfg_for_reset
     in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionview"}|}) in
     let ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) -> (
         match List.assoc_opt "text" fields with
         | Some (`String text) ->
           let parts = String.split_on_char ':' text in
           rpc_success out "execute_command"
           && List.length parts = 14
           && List.nth parts 1 = "Rpc Session"
           && List.nth parts 2 = "true"
           && List.nth parts 3 = "true"
           && List.nth parts 4 = "1"
           && List.nth parts 5 = "turn-0"
           && List.nth parts 6 = "message"
           && List.nth parts 7 = "user"
           && List.nth parts 9 = "turn-0"
           && List.nth parts 11 = "1"
           && List.nth parts 12 = ""
           && List.nth parts 13 = "true"
         | _ -> false)
       | _ -> false
     in
     Session.close session;
     ok);
  check "rpc Pi execute_command exposes readonly modelRegistry snapshot"
    ((not node_available)
     ||
     let out = Rpc.handle_command_for_test (Agent.create cfg_for_reset) (j {|{"type":"execute_command","command":"/modelregistry"}|}) in
     match rpc_field (rpc_last out) "data" with
     | Some (`Assoc fields) -> (
       match List.assoc_opt "text" fields with
       | Some (`String text) ->
         let parts = String.split_on_char ':' text in
         rpc_success out "execute_command"
         && List.length parts = 8
         && Option.value (int_of_string_opt (List.nth parts 0)) ~default:0 > 0
         && Option.value (int_of_string_opt (List.nth parts 1)) ~default:0 > 0
         && List.nth parts 2 = "test-model"
         && List.nth parts 3 = "true"
         && List.nth parts 4 = "true"
         && List.nth parts 5 = "openai"
         && List.nth parts 6 = "true"
         && List.nth parts 7 = "none"
       | _ -> false)
     | _ -> false);
  check "rpc Pi execute_command applies extension newSession action"
    ((not node_available)
     ||
     let session = Session.create_new ~name:"before-action" () in
     let agent =
       Agent.create ~session ~initial_turns:[ { Llm.role = Llm.User; content = [ Llm.Text "hello" ] } ] cfg_for_reset
     in
     let before_id = session.Session.id in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionaction new"}|}) in
     let current_id = match Agent.session agent with Some s -> s.Session.id | None -> "" in
     let ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "new_session")
             &&
             (match List.assoc_opt "text" result_fields with
              | Some (`String text) -> contains0 text "Started new session"
              | _ -> false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "new:false")
         && result_ok
         && current_id <> "" && current_id <> before_id
         && Agent.turn_count agent = 0
       | _ -> false
     in
     Option.iter Session.close (Agent.session agent);
     ok);
  check "rpc Pi execute_command applies extension fork withSession action"
    ((not node_available)
     ||
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session = Session.create_new ~name:"before-fork-with" () in
     List.iter (Session.append session) turns;
     let agent = Agent.create ~session ~initial_turns:turns cfg_for_reset in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionaction forkwith turn-0"}|}) in
     let current = Agent.session agent in
     let entries = match current with Some s -> s.Session.entries | None -> [] in
     let has_fork_note =
       List.exists
         (function
           | `Assoc fields ->
             List.assoc_opt "type" fields = Some (`String "custom_message")
             && List.assoc_opt "id" fields = Some (`String "message-1")
             && List.assoc_opt "parentId" fields = Some (`String "turn-0")
             && List.assoc_opt "customType" fields = Some (`String "fork-note")
           | _ -> false)
         entries
     in
     let ok =
       match rpc_field (rpc_last out) "data", current with
       | Some (`Assoc fields), Some _ ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "fork")
             &&
             (match List.assoc_opt "text" result_fields with
              | Some (`String text) -> contains0 text "Forked turn-0"
              | _ -> false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "forkwith:false")
         && result_ok && Agent.turn_count agent = 1 && has_fork_note
       | _ -> false
     in
     Option.iter Session.close current;
     ok);
  check "rpc Pi execute_command applies extension navigateTree action"
    ((not node_available)
     ||
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session = Session.create_new ~name:"nav-action" () in
     List.iter (Session.append session) turns;
     let agent = Agent.create ~session ~initial_turns:turns cfg_for_reset in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionaction nav turn-0"}|}) in
     let has_leaf_reset =
       List.exists
         (function
           | `Assoc entry_fields ->
             List.assoc_opt "type" entry_fields = Some (`String "leaf")
             && List.assoc_opt "parentId" entry_fields = Some (`String "turn-1")
             && List.assoc_opt "targetId" entry_fields = Some (`Null)
           | _ -> false)
         session.Session.entries
     in
     let ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "navigate_tree")
             && List.assoc_opt "editorText" result_fields = Some (`String "one")
             &&
             (match List.assoc_opt "text" result_fields with
              | Some (`String text) -> contains0 text "Navigated to turn-0"
              | _ -> false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "nav:false")
         && result_ok && Agent.turn_count agent = 0
         && session_context_leaf session (Agent.turns agent) = `Null
         && has_leaf_reset
         && contains0 (Tools.read_file_contents "lifecycle.log") "session_tree  turn-1"
         &&
         List.exists
           (function
             | `Assoc entry_fields ->
               List.assoc_opt "type" entry_fields = Some (`String "label")
               && List.assoc_opt "targetId" entry_fields = Some (`String "turn-0")
               && List.assoc_opt "label" entry_fields = Some (`String "from-extension")
             | _ -> false)
           session.Session.entries
       | _ -> false
     in
     Option.iter Session.close (Agent.session agent);
     ok);
  check "rpc Pi execute_command keeps assistant navigateTree target in context"
    ((not node_available)
     ||
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session = Session.create_new ~name:"nav-assistant-action" () in
     List.iter (Session.append session) turns;
     let agent = Agent.create ~session ~initial_turns:turns cfg_for_reset in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionaction nav turn-1"}|}) in
     let ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "navigate_tree")
             && List.assoc_opt "editorText" result_fields = None
             &&
             (match List.assoc_opt "text" result_fields with
              | Some (`String text) -> contains0 text "Navigated to turn-1"
              | _ -> false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "nav:false")
         && result_ok && Agent.turn_count agent = 2
       | _ -> false
     in
     Option.iter Session.close (Agent.session agent);
     ok);
  check "rpc Pi navigateTree custom_message target returns editor text"
    (let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session = Session.create_new ~name:"nav-custom-message" () in
     List.iter (Session.append session) turns;
     Session.append_entry session
       (`Assoc
         [ ("type", `String "custom_message");
           ("id", `String "custom-1");
           ("parentId", `String "turn-0");
           ("timestamp", `String "");
           ("customType", `String "note");
           ("content", `String "custom body");
           ("display", `Bool true) ]);
     let agent = Agent.create ~session ~initial_turns:turns cfg_for_reset in
     let results =
       Commands.apply_extension_session_actions agent
         [ j {|{"kind":"navigate_tree","targetId":"custom-1","options":{"label":"custom-label"}}|} ]
     in
     let result_ok =
       match results with
       | [ `Assoc fields ] ->
         List.assoc_opt "kind" fields = Some (`String "navigate_tree")
         && List.assoc_opt "editorText" fields = Some (`String "custom body")
         && List.assoc_opt "cancelled" fields = Some (`Bool false)
       | _ -> false
     in
     let has_label =
       List.exists
         (function
           | `Assoc fields ->
             List.assoc_opt "type" fields = Some (`String "label")
             && List.assoc_opt "targetId" fields = Some (`String "custom-1")
             && List.assoc_opt "label" fields = Some (`String "custom-label")
           | _ -> false)
         session.Session.entries
     in
     let ok = result_ok && Agent.turn_count agent = 1 && has_label in
     Option.iter Session.close (Agent.session agent);
     ok);
  check "rpc Pi navigateTree branch_summary target becomes model context leaf"
    ((not node_available)
     ||
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] };
         { Llm.role = Llm.User; content = [ Llm.Text "three" ] } ]
     in
     let session = Session.create_new ~name:"nav-branch-summary-target" () in
     List.iter (Session.append session) turns;
     let summary =
       Session.append_branch_summary session ~parent_id:"turn-0" ~from_hook:false ~from_id:"turn-0"
         "branch leaf summary"
     in
     let summary_id =
       match summary with
       | `Assoc fields -> (
         match List.assoc_opt "id" fields with
         | Some (`String id) -> id
         | _ -> "missing")
       | _ -> "missing"
     in
     Tools.write_file_contents "runtime-request.log" "";
     let agent = Agent.create ~session ~initial_turns:turns (Llm.config_for "runtime") in
     let results =
       Commands.apply_extension_session_actions agent
         [ `Assoc
             [ ("kind", `String "navigate_tree");
               ("targetId", `String summary_id);
               ("options", `Assoc [ ("label", `String "summary-leaf") ]) ] ]
     in
     let leaf_after_nav = session_context_leaf session (Agent.turns agent) in
     let has_leaf_move =
       List.exists
         (function
           | `Assoc fields ->
             List.assoc_opt "type" fields = Some (`String "leaf")
             && List.assoc_opt "targetId" fields = Some (`String summary_id)
           | _ -> false)
         session.Session.entries
     in
     ignore (Agent.send agent "continue from branch summary");
     let log = Tools.read_file_contents "runtime-request.log" in
     let result_ok =
       match results with
       | [ `Assoc fields ] ->
         List.assoc_opt "kind" fields = Some (`String "navigate_tree")
         && List.assoc_opt "editorText" fields = None
         && List.assoc_opt "cancelled" fields = Some (`Bool false)
       | _ -> false
     in
     let has_label =
       List.exists
         (function
           | `Assoc fields ->
             List.assoc_opt "type" fields = Some (`String "label")
             && List.assoc_opt "targetId" fields = Some (`String summary_id)
             && List.assoc_opt "label" fields = Some (`String "summary-leaf")
           | _ -> false)
         session.Session.entries
     in
     let ok =
       result_ok && has_label && has_leaf_move
       && leaf_after_nav = `String summary_id
       && Agent.turn_count agent = 4
       && contains0 log "branch leaf summary"
       && contains0 log "continue from branch summary"
     in
     Option.iter Session.close (Agent.session agent);
     ok);
  check "rpc Pi execute_command applies extension session_before_tree label override"
    ((not node_available)
     ||
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session = Session.create_new ~name:"nav-hook-action" () in
     List.iter (Session.append session) turns;
     let agent = Agent.create ~session ~initial_turns:turns cfg_for_reset in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionaction navhook turn-0"}|}) in
     let ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "navigate_tree")
             && List.assoc_opt "cancelled" result_fields = Some (`Bool false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "navhook:false")
         && result_ok && Agent.turn_count agent = 0
         &&
         List.exists
           (function
             | `Assoc entry_fields ->
               List.assoc_opt "type" entry_fields = Some (`String "label")
               && List.assoc_opt "targetId" entry_fields = Some (`String "turn-0")
               && List.assoc_opt "label" entry_fields = Some (`String "hook-label")
             | _ -> false)
           session.Session.entries
       | _ -> false
     in
     Option.iter Session.close (Agent.session agent);
     ok);
  check "rpc Pi execute_command persists extension branch summary from navigateTree"
    ((not node_available)
     ||
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session = Session.create_new ~name:"nav-summary-action" () in
     List.iter (Session.append session) turns;
     let agent = Agent.create ~session ~initial_turns:turns cfg_for_reset in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionaction navsummary turn-0"}|}) in
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
         && List.assoc_opt "summary" fields = Some (`String "summary from tree 1")
         && List.assoc_opt "fromHook" fields = Some (`Bool true)
         &&
         (match List.assoc_opt "details" fields with
          | Some (`Assoc detail_fields) -> List.assoc_opt "source" detail_fields = Some (`String "hook")
          | _ -> false)
       | None -> false
     in
     let has_label =
       match summary_id with
       | None -> false
       | Some id ->
         List.exists
           (function
             | `Assoc entry_fields ->
               List.assoc_opt "type" entry_fields = Some (`String "label")
               && List.assoc_opt "targetId" entry_fields = Some (`String id)
               && List.assoc_opt "label" entry_fields = Some (`String "summary-hook-label")
             | _ -> false)
           session.Session.entries
     in
     let summary_is_leaf =
       match summary_id with
       | Some id -> session_context_leaf session (Agent.turns agent) = `String id
       | None -> false
     in
     let ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "navigate_tree")
             && List.assoc_opt "cancelled" result_fields = Some (`Bool false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "navsummary:false")
         && result_ok && Agent.turn_count agent = 0 && has_summary && has_label && summary_is_leaf
         && contains0 (Tools.read_file_contents "lifecycle.log") "turn-1 branch_summary"
       | _ -> false
     in
     Option.iter Session.close (Agent.session agent);
     ok);
  check "rpc Pi navigateTree branch summary feeds next model context"
    ((not node_available)
     ||
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session = Session.create_new ~name:"nav-summary-context" () in
     List.iter (Session.append session) turns;
     let agent = Agent.create ~session ~initial_turns:turns cfg_for_reset in
     let _ = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionaction navsummary turn-0"}|}) in
     Tools.write_file_contents "runtime-request.log" "";
     Agent.set_config agent (Llm.config_for "runtime");
     ignore (Agent.send agent "next question");
     let log = Tools.read_file_contents "runtime-request.log" in
     let ok =
       Agent.turn_count agent = 3
       && contains0 log "The following is a summary of a branch that this conversation came back from"
       && contains0 log "summary from tree 1"
       && contains0 log "next question"
     in
     Option.iter Session.close (Agent.session agent);
     ok);
  check "rpc Pi execute_command cancels extension navigateTree action via session_before_tree"
    ((not node_available)
     ||
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session = Session.create_new ~name:"nav-cancel-action" () in
     List.iter (Session.append session) turns;
     let agent = Agent.create ~session ~initial_turns:turns cfg_for_reset in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionaction navcancel turn-0"}|}) in
     let ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "navigate_tree")
             && List.assoc_opt "cancelled" result_fields = Some (`Bool true)
             &&
             (match List.assoc_opt "text" result_fields with
              | Some (`String text) -> contains0 text "no tree"
              | _ -> false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "navcancel:false")
         && result_ok && Agent.turn_count agent = 2
         && not
              (List.exists
                 (function
                   | `Assoc entry_fields ->
                     List.assoc_opt "type" entry_fields = Some (`String "label")
                     && List.assoc_opt "label" entry_fields = Some (`String "cancel-tree")
                   | _ -> false)
                 session.Session.entries)
       | _ -> false
     in
     Option.iter Session.close (Agent.session agent);
     ok);
  check "rpc Pi execute_command marks cancelled session switch action"
    ((not node_available)
     ||
     let target = Session.open_file (Filename.concat (Session.default_dir ()) "cancel-target.jsonl") in
     Session.close target;
     let session = Session.create_new ~name:"before-cancel-switch" () in
     let agent = Agent.create ~session cfg_for_reset in
     let before_id = session.Session.id in
     let out =
       Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionaction switch cancel-target"}|})
     in
     let current_id = match Agent.session agent with Some s -> s.Session.id | None -> "" in
     let ok =
       match rpc_field (rpc_last out) "data" with
       | Some (`Assoc fields) ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "switch_session")
             && List.assoc_opt "cancelled" result_fields = Some (`Bool true)
             &&
             (match List.assoc_opt "text" result_fields with
              | Some (`String text) -> contains0 text "Session switch cancelled"
              | _ -> false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "switch:false")
         && result_ok && current_id = before_id
       | _ -> false
     in
     Option.iter Session.close (Agent.session agent);
     ok);
  check "rpc Pi execute_command applies extension withSession side effects"
    ((not node_available)
     ||
     let session = Session.create_new ~name:"before-with" () in
     let agent =
       Agent.create ~session ~initial_turns:[ { Llm.role = Llm.User; content = [ Llm.Text "hello" ] } ] cfg_for_reset
     in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionaction with"}|}) in
     let current = Agent.session agent in
     let entries = match current with Some s -> s.Session.entries | None -> [] in
     let has_entry typ =
       List.exists
         (function
           | `Assoc fields -> List.assoc_opt "type" fields = Some (`String typ)
           | _ -> false)
         entries
     in
     let entry_has expected =
       List.exists
         (function
           | `Assoc fields ->
             List.for_all
               (fun (key, value) -> List.assoc_opt key fields = Some value)
               expected
           | _ -> false)
         entries
     in
     let has_custom_message custom_type =
       List.exists
         (function
           | `Assoc fields ->
             List.assoc_opt "type" fields = Some (`String "custom_message")
             && List.assoc_opt "customType" fields = Some (`String custom_type)
           | _ -> false)
         entries
     in
     let context_contains_setup_message =
       Agent.context_turns agent
       |> List.exists (fun turn ->
              List.exists
                (function
                  | Llm.Text text -> contains0 text "setup user"
                  | _ -> false)
                turn.Llm.content)
     in
     let ok =
       match rpc_field (rpc_last out) "data", current with
       | Some (`Assoc fields), Some s ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "new_session")
             &&
             (match List.assoc_opt "text" result_fields with
              | Some (`String text) -> contains0 text "Started new session"
              | _ -> false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "with:false")
         && result_ok && s.Session.name = "with-session-name"
         && Agent.turn_count agent = 0
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
         && has_entry "message" && context_contains_setup_message
         && has_entry "thinking_level_change" && has_entry "model_change"
         && has_entry "compaction"
         && has_custom_message "setup-message" && has_custom_message "with-note"
       | _ -> false
     in
     Option.iter Session.close current;
     ok);
  check "rpc Pi execute_command applies setup session tree branch writes"
    ((not node_available)
     ||
     let agent = Agent.create cfg_for_reset in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionbranch"}|}) in
     let current = Agent.session agent in
     let entries = match current with Some s -> s.Session.entries | None -> [] in
     let entry_has expected =
       List.exists
         (function
           | `Assoc fields ->
             List.for_all
               (fun (key, value) -> List.assoc_opt key fields = Some value)
               expected
           | _ -> false)
         entries
     in
     let context_contains text =
       Agent.context_turns agent
       |> List.exists (fun turn ->
              List.exists
                (function
                  | Llm.Text value -> contains0 value text
                  | _ -> false)
                turn.Llm.content)
     in
     let ok =
       match rpc_field (rpc_last out) "data", current with
       | Some (`Assoc fields), Some _ ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "new_session")
             &&
             (match List.assoc_opt "text" result_fields with
              | Some (`String text) -> contains0 text "Started new session"
              | _ -> false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "branch:false")
         && result_ok && Agent.turn_count agent = 0
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
         && context_contains "branch child" && context_contains "setup branch summary"
       | _ -> false
     in
     Option.iter Session.close current;
     ok);
  check "rpc Pi execute_command applies setup session manager aliases"
    ((not node_available)
     ||
     let agent = Agent.create cfg_for_reset in
     let out = Rpc.handle_command_for_test agent (j {|{"type":"execute_command","command":"/sessionalias"}|}) in
     let current = Agent.session agent in
     let entries = match current with Some s -> s.Session.entries | None -> [] in
     let entry_has expected =
       List.exists
         (function
           | `Assoc fields ->
             List.for_all
               (fun (key, value) -> List.assoc_opt key fields = Some value)
               expected
           | _ -> false)
         entries
     in
     let context_contains_alias_root =
       Agent.context_turns agent
       |> List.exists (fun turn ->
              List.exists
                (function
                  | Llm.Text value -> contains0 value "alias root"
                  | _ -> false)
                turn.Llm.content)
     in
     let ok =
       match rpc_field (rpc_last out) "data", current with
       | Some (`Assoc fields), Some s ->
         let result_ok =
           match List.assoc_opt "sessionActionResults" fields with
           | Some (`List [ `Assoc result_fields ]) ->
             List.assoc_opt "kind" result_fields = Some (`String "new_session")
             &&
             (match List.assoc_opt "text" result_fields with
              | Some (`String text) -> contains0 text "Started new session"
              | _ -> false)
           | _ -> false
         in
         rpc_success out "execute_command"
         && List.assoc_opt "text" fields = Some (`String "alias:false")
         && result_ok && s.Session.name = "alias-session"
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
         && context_contains_alias_root
       | _ -> false
     in
     Option.iter Session.close current;
     ok);
  check "rpc Pi execute_command emits extension UI request events"
    ((not node_available)
     ||
     let out =
       Rpc.handle_command_for_test (Agent.create cfg_for_reset)
         (j {|{"type":"execute_command","command":"/uicmd"}|})
     in
     let methods =
       out
       |> List.filter_map (fun event ->
              match rpc_field event "type", rpc_field event "method" with
              | Some (`String "extension_ui_request"), Some (`String method_) -> Some method_
              | _ -> None)
     in
     rpc_success out "execute_command"
     && List.mem "notify" methods && List.mem "confirm" methods
     && List.mem "input" methods && List.mem "select" methods);
  check "rpc Pi execute_command exposes extension UI surfaces"
    ((not node_available)
     ||
     let out =
       Rpc.handle_command_for_test (Agent.create cfg_for_reset)
         (j {|{"type":"execute_command","command":"/surfacecmd"}|})
     in
     rpc_success out "execute_command"
     &&
     match rpc_field (rpc_last out) "data" with
     | Some (`Assoc fields) -> (
       match List.assoc_opt "text" fields, List.assoc_opt "ui" fields with
       | Some (`String text), Some (`Assoc ui_fields) -> (
         match List.assoc_opt "surfaces" ui_fields with
         | Some (`List surfaces) ->
           let kinds =
             List.filter_map
               (fun surface ->
                 match Yojson.Safe.Util.member "kind" surface with
                 | `String kind -> Some kind
                 | _ -> None)
               surfaces
           in
           contains0 text "surface editor body:prefill"
           && List.mem "status" kinds && List.mem "widget" kinds
           && List.mem "title" kinds && List.mem "editor_text" kinds
         | _ -> false)
       | _ -> false)
     | _ -> false);
  check "rpc Pi execute_command emits extension UI surface events"
    ((not node_available)
     ||
     let out =
       Rpc.handle_command_for_test (Agent.create cfg_for_reset)
         (j {|{"type":"execute_command","command":"/surfacecmd"}|})
     in
     let methods =
       out
       |> List.filter_map (fun event ->
              match rpc_field event "type", rpc_field event "method" with
              | Some (`String "extension_ui_request"), Some (`String method_) -> Some method_
              | _ -> None)
     in
     rpc_success out "execute_command"
     && List.mem "setStatus" methods && List.mem "setWidget" methods
     && List.mem "setTitle" methods && List.mem "set_editor_text" methods);
  check "rpc Pi execute_command exposes component factory lines"
    ((not node_available)
     ||
     let out =
       Rpc.handle_command_for_test (Agent.create cfg_for_reset)
         (j {|{"type":"execute_command","command":"/componentcmd"}|})
     in
     rpc_success out "execute_command"
     &&
     match rpc_field (rpc_last out) "data" with
     | Some (`Assoc fields) -> (
       match List.assoc_opt "text" fields, List.assoc_opt "ui" fields with
       | Some (`String text), Some (`Assoc ui_fields) -> (
         match List.assoc_opt "surfaces" ui_fields with
         | Some (`List surfaces) ->
           let lines_for kind =
             surfaces
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
           contains0 text "component done-value:true"
           && List.mem "factory widget" (lines_for "widget")
           && List.mem "custom component" (lines_for "custom")
	       | _ -> false)
	     | _ -> false)
	 | _ -> false);
  check "rpc Pi execute_command exposes custom overlay handle surfaces"
    ((not node_available)
     ||
     let out =
       Rpc.handle_command_for_test (Agent.create cfg_for_reset)
         (j {|{"type":"execute_command","command":"/overlaycmd"}|})
     in
     rpc_success out "execute_command"
     &&
     match rpc_field (rpc_last out) "data" with
     | Some (`Assoc fields) -> (
       match List.assoc_opt "text" fields, List.assoc_opt "ui" fields with
       | Some (`String text), Some (`Assoc ui_fields) -> (
         match List.assoc_opt "surfaces" ui_fields with
         | Some (`List surfaces) ->
           let lines_contain expected surface =
             match Yojson.Safe.Util.member "lines" surface with
             | `List lines -> List.exists (function `String line -> line = expected | _ -> false) lines
             | _ -> false
           in
           let custom_overlay =
             surfaces
             |> List.exists (fun surface ->
                    Yojson.Safe.Util.member "kind" surface = `String "custom"
                    && Yojson.Safe.Util.member "overlay" surface = `Bool true
                    && lines_contain "overlay component" surface)
           in
           let custom_methods =
             surfaces
             |> List.filter_map (fun surface ->
                    match Yojson.Safe.Util.member "kind" surface, Yojson.Safe.Util.member "overlayId" surface with
                    | `String "overlay_handle", `String "custom-overlay-1" -> (
                      match Yojson.Safe.Util.member "method" surface with
                      | `String method_ -> Some method_
                      | _ -> None)
                    | _ -> None)
           in
           contains0 text "overlay overlay-done"
           && custom_overlay
           && List.mem "focus" custom_methods
           && List.mem "setHidden" custom_methods
           && List.mem "hide" custom_methods
         | _ -> false)
       | _ -> false)
     | _ -> false);
  check "rpc Pi execute_command emits component widget lines"
    ((not node_available)
     ||
     let out =
       Rpc.handle_command_for_test (Agent.create cfg_for_reset)
         (j {|{"type":"execute_command","command":"/componentcmd"}|})
     in
     let widget_lines =
       out
       |> List.find_map (fun event ->
              match rpc_field event "type", rpc_field event "method", rpc_field event "widgetKey" with
              | Some (`String "extension_ui_request"), Some (`String "setWidget"), Some (`String "factory") -> (
                match rpc_field event "widgetLines" with
                | Some (`List lines) ->
                  Some (List.filter_map (function `String line -> Some line | _ -> None) lines)
                | _ -> Some [])
              | _ -> None)
       |> Option.value ~default:[]
     in
     rpc_success out "execute_command" && List.mem "factory widget" widget_lines);
  check "rpc Pi execute_command emits custom message events"
    ((not node_available)
     ||
     let out =
       Rpc.handle_command_for_test (Agent.create cfg_for_reset)
         (j {|{"type":"execute_command","command":"/messagecmd"}|})
     in
     let custom_types =
       out
       |> List.filter_map (fun event ->
              match rpc_field event "type", rpc_field event "customType" with
              | Some (`String "custom_message"), Some (`String custom_type) -> Some custom_type
              | _ -> None)
     in
     let notice_rendered =
       out
       |> List.find_map (fun event ->
              match rpc_field event "type", rpc_field event "customType", rpc_field event "rendered" with
              | Some (`String "custom_message"), Some (`String "notice"), Some (`String rendered) -> Some rendered
              | _ -> None)
       |> Option.value ~default:""
     in
     rpc_success out "execute_command"
     && List.mem "notice" custom_types
     && contains0 notice_rendered "NOTICE message body"
     && List.mem "user" custom_types
     && List.mem "api-note" custom_types
     && List.mem "state-note" custom_types);
  Printf.printf "\n%s\n" (if !failures = 0 then "All extension session action tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
