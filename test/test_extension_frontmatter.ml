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
  let dir = Filename.temp_dir "agent_extension_frontmatter_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  let node_available = Sys.command "command -v node >/dev/null 2>&1" = 0 in
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/frontmatter.ts","content":"const { parseFrontmatter, stripFrontmatter } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"frontmatterprobe\", {\n    description: \"Probe exported frontmatter helpers\",\n    handler: async () => {\n      const quoted = parseFrontmatter(\"---\\r\\nname: \\\"skill-name\\\"\\r\\ndescription: 'A desc'\\r\\nfoo-bar: value\\r\\n---\\r\\n\\r\\nBody text\");\n      const multi = parseFrontmatter(\"---\\ndescription: |\\n  Line one\\n  Line two\\n---\\n\\nBody\");\n      const missingEnd = parseFrontmatter(\"---\\nname: test\\nBody without terminator\");\n      let invalid = false;\n      try {\n        parseFrontmatter(\"---\\nfoo: [bar\\n---\\nBody\");\n      } catch (error) {\n        invalid = /at line 1, column 10/.test(String(error && error.message || error));\n      }\n      const stripped = stripFrontmatter(\"---\\nkey: value\\n---\\n\\nBody\\n\");\n      return [\n        quoted.frontmatter.name,\n        quoted.frontmatter.description,\n        quoted.frontmatter[\"foo-bar\"],\n        quoted.body,\n        multi.frontmatter.description.replace(/\\n/g, \"\\\\n\"),\n        multi.body,\n        missingEnd.body.includes(\"Body without terminator\"),\n        invalid,\n        stripped,\n      ].join(\"|\");\n    },\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/skills.ts","content":"const { loadSkillsFromDir, loadSkills, formatSkillsForPrompt } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"skillsprobe\", {\n    description: \"Probe exported skills helpers\",\n    handler: async () => {\n      const root = loadSkillsFromDir({ dir: \"skills-fixtures/root\", source: \"test\" });\n      const direct = loadSkillsFromDir({ dir: \"skills-fixtures/direct\", source: \"test\" });\n      const ignored = loadSkillsFromDir({ dir: \"skills-fixtures/ignored\", source: \"test\" });\n      const missingDir = loadSkillsFromDir({ dir: \"skills-fixtures/nope\", source: \"test\" });\n      const explicitMissing = loadSkills({ cwd: process.cwd(), agentDir: \"agent-none\", skillPaths: [\"skills-fixtures/nope\"], includeDefaults: false });\n      const hidden = loadSkillsFromDir({ dir: \"skills-fixtures/hidden-skill\", source: \"test\" });\n      const formatted = formatSkillsForPrompt([...direct.skills, ...hidden.skills]);\n      return [\n        root.skills.map((s) => s.name).join(\",\"),\n        direct.skills.map((s) => s.name).sort().join(\",\"),\n        direct.diagnostics.some((d) => d.message.includes(\"description is required\")),\n        ignored.skills.map((s) => s.name).join(\",\"),\n        missingDir.diagnostics.length,\n        explicitMissing.diagnostics.some((d) => d.message.includes(\"does not exist\")),\n        hidden.skills[0].disableModelInvocation,\n        !formatted.includes(\"hidden-skill\"),\n        formatted.includes(\"Visible &lt;&amp; &quot;quoted&quot;\"),\n        root.skills[0].sourceInfo.source,\n        root.skills[0].sourceInfo.scope,\n      ].join(\"|\");\n    },\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/resource-loader.ts","content":"const { DefaultResourceLoader, createSyntheticSourceInfo } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"loaderprobe\", {\n    description: \"Probe DefaultResourceLoader SDK parity\",\n    handler: async () => {\n      const injectedSkill = {\n        name: \"injected-skill\",\n        description: \"Injected skill\",\n        filePath: \"/fake/skill/SKILL.md\",\n        baseDir: \"/fake/skill\",\n        sourceInfo: createSyntheticSourceInfo(\"/fake/skill/SKILL.md\", { source: \"custom\" }),\n        disableModelInvocation: false,\n      };\n      const loader = new DefaultResourceLoader({\n        cwd: process.cwd(),\n        agentDir: \"loader-agent\",\n        additionalExtensionPaths: [\"loader-explicit.ts\"],\n        extensionFactories: [(api) => {\n          api.registerCommand(\"inlinecmd\", { description: \"Inline command\", handler: async () => \"inline\" });\n        }],\n        skillsOverride: (base) => ({ skills: [...base.skills, injectedSkill], diagnostics: [...base.diagnostics, { type: \"warning\", message: \"skill override\" }] }),\n        promptsOverride: (base) => ({ prompts: [...base.prompts, { name: \"injected-prompt\", description: \"Injected prompt\", content: \"Prompt body\", body: \"Prompt body\", filePath: \"/fake/prompt.md\", sourceInfo: createSyntheticSourceInfo(\"/fake/prompt.md\", { source: \"custom\" }) }], diagnostics: [...base.diagnostics, { type: \"warning\", message: \"prompt override\" }] }),\n        themesOverride: (base) => ({ themes: [...base.themes, { name: \"injected-theme\", sourcePath: \"/fake/theme.json\" }], diagnostics: [...base.diagnostics, { type: \"warning\", message: \"theme override\" }] }),\n        agentsFilesOverride: (base) => ({ agentsFiles: [...base.agentsFiles, { path: \"OVERRIDE.md\", content: \"Override agents\" }] }),\n        systemPromptOverride: (base) => `${base}|override`,\n        appendSystemPromptOverride: (base) => [...base, \"append override\"],\n      });\n      await loader.reload();\n      const commands = loader.getExtensions().extensions.flatMap((extension) => Array.from(extension.commands.keys())).sort();\n      const noExtLoader = new DefaultResourceLoader({\n        cwd: process.cwd(),\n        agentDir: \"loader-agent\",\n        additionalExtensionPaths: [\"loader-explicit.ts\"],\n        noExtensions: true,\n      });\n      await noExtLoader.reload();\n      const noExtCommands = noExtLoader.getExtensions().extensions.flatMap((extension) => Array.from(extension.commands.keys())).sort();\n      return [\n        loader.getSystemPrompt(),\n        loader.getAppendSystemPrompt().join(\",\"),\n        commands.includes(\"inlinecmd\"),\n        commands.includes(\"explicitcmd\"),\n        commands.includes(\"defaultlocal\"),\n        loader.getSkills().skills.some((skill) => skill.name === \"injected-skill\"),\n        loader.getSkills().diagnostics.some((diagnostic) => diagnostic.message === \"skill override\"),\n        loader.getPrompts().prompts.some((prompt) => prompt.name === \"injected-prompt\"),\n        loader.getPrompts().diagnostics.some((diagnostic) => diagnostic.message === \"prompt override\"),\n        loader.getThemes().themes.some((theme) => theme.name === \"injected-theme\"),\n        loader.getThemes().diagnostics.some((diagnostic) => diagnostic.message === \"theme override\"),\n        loader.getAgentsFiles().agentsFiles.some((file) => file.path === \"OVERRIDE.md\"),\n        noExtCommands.includes(\"explicitcmd\"),\n        !noExtCommands.includes(\"defaultlocal\"),\n      ].join(\"|\");\n    },\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/SYSTEM.md","content":"Project system"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/APPEND_SYSTEM.md","content":"Project append"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/default-local.ts","content":"export default function(pi) {\n  pi.registerCommand(\"defaultlocal\", { description: \"Default local\", handler: async () => \"default\" });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/esm-syntax.ts","content":"import * as Agent from \"@earendil-works/pi-coding-agent\";\nimport DefaultHelper, { makeLabel as label, helperValue, aliasTarget } from \"./lib/helper\";\nimport { Type as T } from \"typebox\";\nimport \"node:path\";\n\nexport const exportedConstant = \"constant\";\nexport function exportedHelper(value) {\n  return `${value}!`;\n}\n\nexport default function(pi) {\n  pi.registerCommand(\"esmprobe\", {\n    description: \"Probe ESM import/export bridge parity\",\n    handler: async () => [\n      typeof Agent.DefaultResourceLoader,\n      T.Object({}).type,\n      label(\"x\"),\n      DefaultHelper(\"y\"),\n      exportedHelper(\"z\"),\n      exportedConstant,\n      helperValue,\n      aliasTarget(),\n    ].join(\"|\"),\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/lib/helper.ts","content":"export const helperValue = \"value\";\nexport function makeLabel(input) {\n  return `label:${input}`;\n}\nfunction target() {\n  return \"aliased\";\n}\nexport { target as aliasTarget };\nexport default function defaultHelper(input) {\n  return `default:${input}`;\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/typed-syntax.ts","content":"import { type ExtensionAPI } from \"@earendil-works/pi-coding-agent\";\n\ninterface Item {\n  name: string;\n  count?: number;\n}\n\ntype Result = {\n  value: string;\n  tags?: string[];\n};\n\nconst choices = [\"a\", \"b\"] as const;\nconst typedValue: Result = { value: \"ok\", tags: [\"typed\"] };\n\nfunction identity<T>(value: T): T {\n  return value;\n}\n\nfunction withTypes(input: string, optional?: number): string {\n  return `${input}:${optional ?? 0}`;\n}\n\nconst arrow = (item: Item): string => item.name;\n\nclass Box {\n  private label: string;\n  constructor(label: string) {\n    this.label = label;\n  }\n  render(width: number): string[] {\n    return [this.label, String(width)];\n  }\n}\n\nexport default function(pi: ExtensionAPI): void {\n  pi.registerCommand(\"typedprobe\", {\n    description: \"Probe TypeScript type erasure\",\n    handler: async (_args: string) => {\n      const box = new Box(\"box\");\n      const filtered = [{ type: \"text\" }, { other: true }].filter((block): block is { type: string } => block.type === \"text\");\n      const state = { enabled: true, todos: choices } as { enabled: boolean; todos: readonly string[] } | undefined;\n      return [\n        withTypes(\"value\", 7),\n        arrow({ name: \"item\" }),\n        identity<string>(\"generic\"),\n        box.render(3).join(\":\"),\n        filtered.length,\n        typedValue.value,\n        state && state.todos.length,\n      ].join(\"|\");\n    },\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/typebox-subpaths.ts","content":"import { Type } from \"typebox\";\nimport { Compile } from \"typebox/compile\";\nimport { TypeCompiler } from \"@sinclair/typebox/compiler\";\nimport { Value } from \"typebox/value\";\nimport { Value as SinclairValue } from \"@sinclair/typebox/value\";\nimport { Format } from \"typebox/error\";\n\nexport default function(pi) {\n  pi.registerCommand(\"typeboxprobe\", {\n    description: \"Probe TypeBox compile/value virtual modules\",\n    handler: async () => {\n      const schema = Type.Object({\n        name: Type.String(),\n        count: Type.Optional(Type.Integer({ default: 1 })),\n        enabled: Type.Boolean(),\n        tags: Type.Array(Type.String()),\n        mode: Type.Union([Type.Literal(\"fast\"), Type.Literal(\"safe\")]),\n      });\n      const input = { name: 123, count: \"7\", enabled: \"true\", tags: [1, \"b\"], mode: \"fast\" };\n      const converted = Value.Convert(schema, input);\n      const validator = Compile(schema);\n      const ok = validator.Check(input);\n      const bad = { name: \"x\", enabled: \"nope\", tags: [], mode: \"slow\" };\n      const errors = validator.Errors(bad).map((e) => `${e.path || e.instancePath}:${e.message}`).join(\",\");\n      const compilerOk = TypeCompiler.Compile(Type.Object({ x: Type.Number() })).Check({ x: 1 });\n      const sinclairConverted = SinclairValue.Convert(Type.Object({ x: Type.Number() }), { x: \"4\" });\n      return [\n        converted === input,\n        input.name,\n        input.count,\n        input.enabled,\n        input.tags.join(\",\"),\n        ok,\n        errors.includes(\"enabled\") && errors.includes(\"mode\"),\n        compilerOk,\n        sinclairConverted.x,\n        typeof Format(validator.Errors(bad)),\n      ].join(\"|\");\n    },\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/bundled-modules.ts","content":"import { StringEnum, Type, getModel } from \"@earendil-works/pi-ai\";\nimport { Container, Key, SelectList, SettingsList, Spacer, Text, CURSOR_MARKER, Input, matchesKey, truncateToWidth, visibleWidth } from \"@earendil-works/pi-tui\";\n\nexport default function(pi) {\n  pi.registerCommand(\"bundleprobe\", {\n    description: \"Probe bundled pi-ai and pi-tui module exports\",\n    handler: async () => {\n      const schema = StringEnum([\"add\", \"remove\"] as const, { description: \"Action\", default: \"add\" });\n      const object = Type.Object({ action: schema });\n      const model = getModel(\"openai/gpt-test\");\n      const container = new Container();\n      container.addChild(new Text(\"hello\"));\n      container.addChild(new Spacer(1));\n      container.addChild(new Text(\"world\"));\n      const select = new SelectList([{ value: \"add\", label: \"Add\" }, { value: \"remove\", label: \"Remove\" }], 2, {});\n      select.setSelectedIndex(1);\n      const settings = new SettingsList([{ key: \"mode\", label: \"Mode\", currentValue: \"fast\" }], 2, {});\n      const input = new Input(\"abc\");\n      input.handleInput(\"d\");\n      return [\n        schema.enum.join(\",\"),\n        object.required.includes(\"action\"),\n        model.provider,\n        model.id,\n        container.render(20).join(\"/\"),\n        select.getSelectedItem().value,\n        settings.render(20)[0].includes(\"Mode\"),\n        matchesKey(\"\\x03\", Key.ctrl(\"c\")),\n        matchesKey(\"\\x1b\", Key.escape),\n        truncateToWidth(\"abcdef\", 4, \"\"),\n        visibleWidth(`a\\x1b[31mb${CURSOR_MARKER}c`),\n        input.getValue(),\n      ].join(\"|\");\n    },\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/ai-registry.ts","content":"import { EventStream, appendAssistantMessageDiagnostic, calculateCost, clampThinkingLevel, cleanupSessionResources, completeSimple, createAssistantMessageDiagnostic, createAssistantMessageEventStream, fauxAssistantMessage, fauxText, fauxToolCall, findEnvKeys, generateImages, getApiProvider, getApiProviders, getEnvApiKey, getImageModels, getImageProviders, getImagesApiProvider, getModels, getProviders, getSupportedThinkingLevels, parseStreamingJson, registerApiProvider, registerImagesApiProvider, registerSessionResourceCleanup, resetApiProviders, streamSimple, unregisterApiProviders, validateToolCall } from \"@earendil-works/pi-ai\";\n\nexport default function(pi) {\n  pi.registerCommand(\"airegistryprobe\", {\n    description: \"Probe pi-ai registry and helper exports\",\n    handler: async () => {\n      process.env.OPENAI_API_KEY = \"env-key\";\n      resetApiProviders();\n      const builtIn = getApiProviders().some((provider) => provider.api === \"openai-responses\");\n      const unitModel = { api: \"unit-api\", provider: \"unit\", id: \"m\", name: \"m\", reasoning: true, thinkingLevelMap: { xhigh: \"max\" }, cost: { input: 2, output: 4, cacheRead: 1, cacheWrite: 3 } };\n      const makeStream = (model, context) => {\n        const stream = createAssistantMessageEventStream();\n        const text = `${model.id}:${context.messages[0].content[0].text}`;\n        stream.push({ type: \"done\", message: { role: \"assistant\", content: [{ type: \"text\", text }], stopReason: \"stop\" } });\n        return stream;\n      };\n      registerApiProvider({ api: \"unit-api\", stream: makeStream, streamSimple: makeStream }, \"probe-source\");\n      const providerApi = getApiProvider(\"unit-api\").api;\n      const response = await completeSimple(unitModel, { messages: [{ role: \"user\", content: [{ type: \"text\", text: \"hello\" }] }] });\n      const streamed = await streamSimple(unitModel, { messages: [{ role: \"user\", content: [{ type: \"text\", text: \"hello\" }] }] }).result();\n      const totalCost = calculateCost(unitModel, { input: 1000000, output: 500000, cacheRead: 1000000, cacheWrite: 0, cost: {} }).total;\n      unregisterApiProviders(\"probe-source\");\n      const removed = !getApiProvider(\"unit-api\");\n      registerImagesApiProvider({ api: \"unit-images\", generateImages: async () => ({ images: [{ type: \"image\", data: \"abc\" }] }) }, \"image-source\");\n      const imageApi = getImagesApiProvider(\"unit-images\").api;\n      const images = await generateImages({ api: \"unit-images\", provider: \"unit\", id: \"img\" }, { prompt: \"draw\" });\n      const events = new EventStream((event) => event.done, (event) => event.value);\n      const eventResult = events.result();\n      events.push({ done: true, value: \"event\" });\n      let cleaned = \"\";\n      const unregisterCleanup = registerSessionResourceCleanup((sessionId) => { cleaned = sessionId; });\n      cleanupSessionResources(\"s1\");\n      unregisterCleanup();\n      cleanupSessionResources(\"s2\");\n      const diagnosticMessage = appendAssistantMessageDiagnostic({ role: \"assistant\", content: [] }, createAssistantMessageDiagnostic(new Error(\"bad\")));\n      const validated = validateToolCall([{ name: \"ok\" }], { name: \"ok\", arguments: { x: 1 } });\n      return [\n        builtIn,\n        providerApi,\n        response.content[0].text,\n        streamed.content[0].text,\n        totalCost,\n        getSupportedThinkingLevels(unitModel).join(\",\"),\n        clampThinkingLevel(unitModel, \"xhigh\"),\n        getProviders().includes(\"openai\"),\n        getModels(\"openai\").length > 0,\n        removed,\n        imageApi,\n        images.images.length,\n        await eventResult,\n        cleaned === \"s1\",\n        findEnvKeys(\"openai\")[0],\n        getEnvApiKey(\"openai\"),\n        parseStreamingJson(\"{\\\"partial\\\":1\").partial,\n        diagnosticMessage.diagnostics.length,\n        validated.ok,\n        fauxAssistantMessage([fauxText(\"hi\"), fauxToolCall(\"ok\", {})]).content.length,\n        getImageProviders().includes(\"openrouter\"),\n        getImageModels(\"openrouter\").length > 0,\n      ].join(\"|\");\n    },\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/agent-core.ts","content":"import { Agent, AgentHarness, AgentHarnessError, BRANCH_SUMMARY_PREFIX, DEFAULT_MAX_BYTES, DEFAULT_MAX_LINES, ExecutionError, FileError, InMemorySessionRepo, SessionError, agentLoop, bashExecutionToText, buildSessionContext, computeFileLists, convertToLlm, createBranchSummaryMessage, createCompactionSummaryMessage, createCustomMessage, createFileOps, createSessionId, createTimestamp, err, estimateContextTokens, extractFileOpsFromMessage, formatFileOperations, formatPromptTemplateInvocation, formatSkillInvocation, formatSkillsForSystemPrompt, getOrThrow, getOrUndefined, ok, parseCommandArgs, sanitizeBinaryOutput, shouldCompact, streamProxy, substituteArgs, toError, truncateHead, truncateLine, truncateTail, uuidv7 } from \"@earendil-works/pi-agent-core\";\n\nexport default function(pi) {\n  pi.registerCommand(\"agentcoreprobe\", {\n    description: \"Probe bundled pi-agent-core module exports\",\n    handler: async () => {\n      const repo = new InMemorySessionRepo();\n      const session = await repo.create({ id: \"s-main\" });\n      await session.appendSessionName(\"Main\");\n      const messageId = await session.appendMessage({ role: \"user\", content: [{ type: \"text\", text: \"hello\" }], timestamp: 1 });\n      await session.appendThinkingLevelChange(\"high\");\n      await session.appendModelChange(\"openai\", \"gpt-4o\");\n      await session.appendLabel(messageId, \"greeting\");\n      const context = await session.buildContext();\n      const fork = await repo.fork(await session.getMetadata(), { entryId: messageId, position: \"at\", id: \"s-fork\" });\n      const forkMetadata = await fork.getMetadata();\n      const loop = agentLoop([{ role: \"user\", content: [{ type: \"text\", text: \"loop\" }] }], { systemPrompt: \"sys\", messages: [], tools: [] }, { model: { api: \"unit\", provider: \"unit\", id: \"m\" }, streamFn: async () => ({ role: \"assistant\", content: [{ type: \"text\", text: \"loop-reply\" }], stopReason: \"stop\" }) });\n      const loopMessages = await loop.result();\n      const agent = new Agent({ initialState: { systemPrompt: \"sys\", model: { api: \"unit\", provider: \"unit\", id: \"m\" }, messages: [], tools: [] }, streamFn: async (_model, ctx) => ({ role: \"assistant\", content: [{ type: \"text\", text: `reply:${ctx.messages.length}` }], stopReason: \"stop\" }) });\n      const agentEvents = [];\n      agent.subscribe((event) => agentEvents.push(event.type));\n      await agent.prompt(\"hi\");\n      const fileOps = createFileOps();\n      extractFileOpsFromMessage({ role: \"assistant\", content: [{ type: \"toolCall\", name: \"read\", arguments: { path: \"a.txt\" } }, { type: \"toolCall\", name: \"write\", arguments: { path: \"b.txt\" } }] }, fileOps);\n      const lists = computeFileLists(fileOps);\n      const custom = createCustomMessage(\"note\", \"custom body\", true, { a: 1 }, \"2024-01-01T00:00:00.000Z\");\n      const branch = createBranchSummaryMessage(\"branch\", \"from\", \"2024-01-01T00:00:00.000Z\");\n      const compaction = createCompactionSummaryMessage(\"compact\", 12, \"2024-01-01T00:00:00.000Z\");\n      const llm = convertToLlm([custom, branch, compaction, { role: \"user\", content: [{ type: \"text\", text: \"u\" }] }]);\n      const proxyResult = await streamProxy({ api: \"unit\", provider: \"unit\", id: \"proxy\" }, { messages: [{ role: \"user\", content: [{ type: \"text\", text: \"proxy\" }] }] }, { streamFn: async () => ({ role: \"assistant\", content: [{ type: \"text\", text: \"proxied\" }] }) }).result();\n      return [\n        getOrThrow(ok({ value: 7 })).value,\n        getOrUndefined(ok({ value: 8 })).value,\n        err(\"bad\").ok === false,\n        toError(\"oops\").message,\n        new FileError(\"not_found\", \"missing\", \"x\").name,\n        new ExecutionError(\"timeout\", \"slow\").code,\n        new SessionError(\"storage\", \"broken\").name,\n        new AgentHarnessError(\"invalid_options\", \"bad\").code,\n        formatSkillInvocation({ name: \"skill\", description: \"desc\", content: \"Body\", filePath: \"/tmp/SKILL.md\" }, \"extra\").includes(\"References are relative to /tmp\"),\n        formatSkillsForSystemPrompt([{ name: \"visible\", description: \"A < B\", filePath: \"/s\", content: \"\" }, { name: \"hidden\", description: \"H\", filePath: \"/h\", content: \"\", disableModelInvocation: true }]).includes(\"A &lt; B\") && !formatSkillsForSystemPrompt([{ name: \"hidden\", description: \"H\", filePath: \"/h\", content: \"\", disableModelInvocation: true }]).includes(\"hidden\"),\n        parseCommandArgs(\"one \\\"two three\\\" 'four five'\").join(\",\"),\n        substituteArgs(\"$1|$@|${@:2}\", [\"a\", \"b\", \"c\"]),\n        formatPromptTemplateInvocation({ content: \"$2 $1\" }, [\"x\", \"y\"]),\n        sanitizeBinaryOutput(\"a\\u0000b\\tc\"),\n        truncateHead(\"a\\nb\\nc\", { maxLines: 2 }).content,\n        truncateTail(\"a\\nb\\nc\", { maxLines: 2 }).content,\n        truncateLine(\"abcdef\", 3).content,\n        DEFAULT_MAX_LINES > 0 && DEFAULT_MAX_BYTES > 0,\n        typeof createSessionId() === \"string\" && createTimestamp().includes(\"T\") && typeof uuidv7() === \"string\",\n        await session.getSessionName(),\n        await session.getLabel(messageId),\n        context.messages.length,\n        context.thinkingLevel,\n        context.model.modelId,\n        (await repo.list()).length,\n        forkMetadata.id,\n        loopMessages[1].content[0].text,\n        agent.state.messages.map((m) => m.role).join(\",\"),\n        agentEvents.includes(\"agent_start\") && agentEvents.includes(\"agent_end\"),\n        lists.readFiles.join(\",\"),\n        lists.modifiedFiles.join(\",\"),\n        formatFileOperations(lists.readFiles, lists.modifiedFiles).includes(\"<modified-files>\"),\n        bashExecutionToText({ role: \"bashExecution\", command: \"ls\", output: \"out\", exitCode: 1, cancelled: false, truncated: false }).includes(\"code 1\"),\n        llm.length,\n        llm[1].content[0].text.startsWith(BRANCH_SUMMARY_PREFIX),\n        estimateContextTokens([{ role: \"user\", content: [{ type: \"text\", text: \"hello\" }] }]).tokens > 0,\n        shouldCompact(90, 100, { enabled: true, reserveTokens: 20, keepRecentTokens: 10 }),\n        proxyResult.content[0].text,\n        new AgentHarness().agent instanceof Agent,\n        buildSessionContext(await session.getBranch()).model.modelId,\n      ].join(\"|\");\n    },\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/subpath-modules.ts","content":"import { bedrockProviderModule } from \"@earendil-works/pi-ai/bedrock-provider\";\nimport { streamAnthropic } from \"@earendil-works/pi-ai/anthropic\";\nimport { getOpenAICodexWebSocketDebugStats, resetOpenAICodexWebSocketDebugStats, closeOpenAICodexWebSocketSessions, streamOpenAICodexResponses } from \"@earendil-works/pi-ai/openai-codex-responses\";\nimport { convertMessages } from \"@earendil-works/pi-ai/openai-completions\";\nimport { streamOpenAIResponses, streamSimpleOpenAIResponses } from \"@earendil-works/pi-ai/openai-responses\";\nimport { NodeExecutionEnv } from \"@earendil-works/pi-agent-core/node\";\nimport { defineTool } from \"@earendil-works/pi-coding-agent/hooks\";\n\nexport default function(pi) {\n  pi.registerCommand(\"subpathprobe\", {\n    description: \"Probe package export subpath virtual modules\",\n    handler: async () => {\n      const ctx = (text) => ({ messages: [{ role: \"user\", content: [{ type: \"text\", text }] }] });\n      const openai = await streamOpenAIResponses({ api: \"openai-responses\", provider: \"openai\", id: \"responses\" }, ctx(\"openai\")).result();\n      const openaiSimple = await streamSimpleOpenAIResponses({ api: \"openai-responses\", provider: \"openai\", id: \"simple\" }, ctx(\"simple\")).result();\n      const anthropic = await streamAnthropic({ api: \"anthropic-messages\", provider: \"anthropic\", id: \"claude\" }, ctx(\"anthropic\")).result();\n      const codex = await streamOpenAICodexResponses({ api: \"openai-codex-responses\", provider: \"openai\", id: \"codex\" }, ctx(\"codex\")).result();\n      const bedrock = await bedrockProviderModule.streamBedrock({ api: \"bedrock-converse-stream\", provider: \"amazon-bedrock\", id: \"bedrock\" }, ctx(\"bedrock\")).result();\n      const statsMissing = getOpenAICodexWebSocketDebugStats(\"none\") === undefined;\n      resetOpenAICodexWebSocketDebugStats(\"none\");\n      closeOpenAICodexWebSocketSessions(\"none\");\n      const env = new NodeExecutionEnv({ cwd: process.cwd() });\n      const write = await env.writeFile(\"subpath.txt\", \"hello\\nworld\");\n      const read = await env.readTextFile(\"subpath.txt\");\n      const lines = await env.readTextLines(\"subpath.txt\", { maxLines: 1 });\n      const info = await env.fileInfo(\"subpath.txt\");\n      const listed = await env.listDir(\".\");\n      const exists = await env.exists(\"subpath.txt\");\n      const abs = await env.absolutePath(\"subpath.txt\");\n      const exec = await env.exec(\"printf ok\");\n      return [\n        openai.content[0].text,\n        openaiSimple.content[0].text,\n        anthropic.content[0].text,\n        codex.content[0].text,\n        statsMissing,\n        convertMessages([{ role: \"user\" }]).length,\n        typeof bedrockProviderModule.streamBedrock === \"function\",\n        bedrock.content[0].text,\n        write.ok,\n        read.value,\n        lines.value[0],\n        info.value.kind,\n        listed.value.some((item) => item.name === \"subpath.txt\"),\n        exists.value,\n        abs.value.endsWith(\"subpath.txt\"),\n        exec.ok && exec.value.stdout,\n        typeof defineTool,\n        NodeExecutionEnv.name,\n      ].join(\"|\");\n    },\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":"loader-explicit.ts","content":"export default function(pi) {\n  pi.registerCommand(\"explicitcmd\", { description: \"Explicit command\", handler: async () => \"explicit\" });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":".pi/extensions/resource-loader-dynamic.ts","content":"const { DefaultResourceLoader } = require(\"@earendil-works/pi-coding-agent\");\n\nexport default function(pi) {\n  pi.registerCommand(\"loaderdynamic\", {\n    description: \"Probe dynamic ResourceLoader resources\",\n    handler: async () => {\n      const metadata = { source: \"dynamic-pkg\", scope: \"project\", origin: \"package\", baseDir: \"dynamic-resources\" };\n      const loader = new DefaultResourceLoader({\n        cwd: process.cwd(),\n        agentDir: \"loader-agent\",\n        noSkills: true,\n        noPromptTemplates: true,\n        noThemes: true,\n      });\n      await loader.reload();\n      loader.extendResources({\n        skillPaths: [{ path: \"dynamic-resources/skills\", metadata }],\n        promptPaths: [{ path: \"dynamic-resources/prompts-a\", metadata }, { path: \"dynamic-resources/prompts-b\", metadata }],\n        themePaths: [{ path: \"dynamic-resources/themes-a\", metadata }, { path: \"dynamic-resources/themes-b\", metadata }],\n      });\n      const skills = loader.getSkills();\n      const prompts = loader.getPrompts();\n      const themes = loader.getThemes();\n      loader.extendResources({\n        promptPaths: [{ path: \"dynamic-resources/prompts-c\", metadata }],\n        themePaths: [{ path: \"dynamic-resources/themes-c\", metadata }],\n      });\n      const prompts2 = loader.getPrompts();\n      const themes2 = loader.getThemes();\n      const skill = skills.skills.find((item) => item.name === \"dyn-skill\");\n      const prompt = prompts.prompts.find((item) => item.name === \"actual\");\n      const theme = themes.themes.find((item) => item.name === \"dyn-theme\");\n      return [\n        skill && skill.sourceInfo && skill.sourceInfo.source,\n        skill && skill.sourceInfo && skill.sourceInfo.scope,\n        prompt && prompt.name,\n        prompt && prompt.description.endsWith(\"...\"),\n        prompt && prompt.sourceInfo && prompt.sourceInfo.source,\n        prompts.prompts.filter((item) => item.name === \"dup\").length,\n        prompts.diagnostics.some((item) => item.collision && item.collision.resourceType === \"prompt\" && item.collision.name === \"dup\"),\n        theme && theme.sourceInfo && theme.sourceInfo.source,\n        themes.themes.filter((item) => item.name === \"shared-theme\").length,\n        themes.diagnostics.some((item) => item.collision && item.collision.resourceType === \"theme\" && item.collision.name === \"shared-theme\"),\n        prompts2.prompts.some((item) => item.name === \"later\"),\n        themes2.themes.some((item) => item.name === \"later-theme\"),\n      ].join(\"|\");\n    },\n  });\n}\n"}|});
  ignore
    (run "write_file"
       {|{"path":"dynamic-resources/skills/dyn/SKILL.md","content":"---\nname: dyn-skill\ndescription: Dynamic skill\n---\nDynamic body"}|});
  ignore
    (run "write_file"
       {|{"path":"dynamic-resources/prompts-a/actual.md","content":"---\nname: ignored-prompt\n---\nThis first line is deliberately longer than sixty characters for truncation.\n\nPrompt body"}|});
  ignore
    (run "write_file"
       {|{"path":"dynamic-resources/prompts-a/dup.md","content":"---\ndescription: First duplicate\n---\nFirst dup"}|});
  ignore
    (run "write_file"
       {|{"path":"dynamic-resources/prompts-b/dup.md","content":"---\ndescription: Second duplicate\n---\nSecond dup"}|});
  ignore
    (run "write_file"
       {|{"path":"dynamic-resources/prompts-c/later.md","content":"---\ndescription: Later prompt\n---\nLater body"}|});
  ignore
    (run "write_file"
       {|{"path":"dynamic-resources/themes-a/dyn.json","content":"{\"name\":\"dyn-theme\",\"accent\":\"cyan\"}"}|});
  ignore
    (run "write_file"
       {|{"path":"dynamic-resources/themes-a/shared-a.json","content":"{\"name\":\"shared-theme\",\"accent\":\"red\"}"}|});
  ignore
    (run "write_file"
       {|{"path":"dynamic-resources/themes-b/shared-b.json","content":"{\"name\":\"shared-theme\",\"accent\":\"blue\"}"}|});
  ignore
    (run "write_file"
       {|{"path":"dynamic-resources/themes-c/later.json","content":"{\"name\":\"later-theme\",\"accent\":\"green\"}"}|});
  ignore
    (run "write_file"
       {|{"path":"skills-fixtures/root/SKILL.md","content":"---\nname: root-skill\ndescription: Root skill should win.\n---\nRoot body"}|});
  ignore
    (run "write_file"
       {|{"path":"skills-fixtures/root/nested/SKILL.md","content":"---\nname: child-skill\ndescription: Child should not load.\n---\nChild body"}|});
  ignore
    (run "write_file"
       {|{"path":"skills-fixtures/direct/direct.md","content":"---\nname: direct-skill\ndescription: Visible <& \"quoted\"\n---\nDirect body"}|});
  ignore
    (run "write_file"
       {|{"path":"skills-fixtures/direct/missing.md","content":"---\nname: missing-description\n---\nMissing desc"}|});
  ignore
    (run "write_file"
       {|{"path":"skills-fixtures/direct/node_modules/dep/SKILL.md","content":"---\nname: dep-skill\ndescription: Dependency should not load.\n---\nDep"}|});
  ignore
    (run "write_file"
       {|{"path":"skills-fixtures/direct/.hidden/SKILL.md","content":"---\nname: hidden-dot\ndescription: Hidden should not load.\n---\nHidden"}|});
  ignore (run "write_file" {|{"path":"skills-fixtures/ignored/.gitignore","content":"skip/\n"}|});
  ignore
    (run "write_file"
       {|{"path":"skills-fixtures/ignored/skip/SKILL.md","content":"---\nname: skipped-skill\ndescription: Ignored should not load.\n---\nSkip"}|});
  ignore
    (run "write_file"
       {|{"path":"skills-fixtures/ignored/keep/SKILL.md","content":"---\nname: keep-skill\ndescription: Keep should load.\n---\nKeep"}|});
  ignore
    (run "write_file"
       {|{"path":"skills-fixtures/hidden-skill/SKILL.md","content":"---\nname: hidden-skill\ndescription: Hidden from prompt\ndisable-model-invocation: true\n---\nHidden"}|});
  ignore (Extensions.load ());
  check "TypeScript extension frontmatter helpers match Pi YAML behavior"
    ((not node_available)
     ||
     match Extensions.execute_command "/frontmatterprobe" with
     | Some output -> output = "skill-name|A desc|value|Body text|Line one\\nLine two\\n|Body|true|true|Body"
     | None -> false);
  check "TypeScript extension skills helpers match Pi discovery rules"
    ((not node_available)
     ||
     match Extensions.execute_command "/skillsprobe" with
     | Some output -> output = "root-skill|direct-skill|true|keep-skill|0|true|true|true|true|test|temporary"
     | None -> false);
  check "TypeScript DefaultResourceLoader applies overrides and explicit noExtensions paths"
    ((not node_available)
     ||
     match Extensions.execute_command "/loaderprobe" with
     | Some output ->
       output
       = "Project system|override|Project append,append override|true|true|true|true|true|true|true|true|true|true|true|true"
     | None -> false);
  check "TypeScript DefaultResourceLoader preserves dynamic resource source info and collisions"
    ((not node_available)
     ||
     match Extensions.execute_command "/loaderdynamic" with
     | Some output ->
       output = "dynamic-pkg|project|actual|true|dynamic-pkg|1|true|dynamic-pkg|1|true|true|true"
     | None -> false);
  check "TypeScript extension loader supports common ESM import/export syntax"
    ((not node_available)
     ||
     match Extensions.execute_command "/esmprobe" with
     | Some output -> output = "function|object|label:x|default:y|z!|constant|value|aliased"
     | None -> false);
  check "TypeScript extension loader erases common type syntax"
    ((not node_available)
     ||
     match Extensions.execute_command "/typedprobe" with
     | Some output -> output = "value:7|item|generic|box:3|1|ok|2"
     | None -> false);
  check "TypeScript extension loader exposes TypeBox compile/value subpaths"
    ((not node_available)
     ||
     match Extensions.execute_command "/typeboxprobe" with
     | Some output -> output = "true|123|7|true|1,b|true|true|true|4|string"
     | None -> false);
  check "TypeScript extension loader exposes bundled pi-ai and pi-tui modules"
    ((not node_available)
     ||
     match Extensions.execute_command "/bundleprobe" with
     | Some output ->
       output = "add,remove|true|openai|gpt-test|hello//world|remove|true|true|true|abcd|3|abcd"
     | None -> false);
  check "TypeScript extension loader exposes pi-ai registries and helpers"
    ((not node_available)
     ||
     match Extensions.execute_command "/airegistryprobe" with
     | Some output ->
       output
       = "true|unit-api|m:hello|m:hello|5|off,minimal,low,medium,high,xhigh|xhigh|true|true|true|unit-images|1|event|true|OPENAI_API_KEY|env-key|1|1|true|2|true|true"
     | None -> false);
  check "TypeScript extension loader exposes pi-agent-core helpers and session runtime"
    ((not node_available)
     ||
     match Extensions.execute_command "/agentcoreprobe" with
     | Some output ->
       output
       = "7|8|true|oops|FileError|timeout|SessionError|invalid_options|true|true|one,two three,four five|a|a b c|b c|y x|ab\tc|a\nb|b\nc|abc|true|true|Main|greeting|1|high|gpt-4o|2|s-fork|loop-reply|user,assistant|true|a.txt|b.txt|true|true|4|true|true|true|proxied|true|gpt-4o"
     | None -> false);
  check "TypeScript extension loader resolves exported package subpaths"
    ((not node_available)
     ||
     match Extensions.execute_command "/subpathprobe" with
     | Some output ->
       output
       = "openai|simple|anthropic|codex|true|1|true|bedrock|true|hello\nworld|hello|file|true|true|true|ok|function|NodeExecutionEnv"
     | None -> false);

  Printf.printf "\n%s\n" (if !failures = 0 then "All extension frontmatter tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
