let source =
  {js|
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const childProcess = require("node:child_process");
const { pathToFileURL } = require("node:url");

function readStdin() {
  return new Promise((resolve) => {
    let data = "";
    process.stdin.setEncoding("utf8");
    process.stdin.on("data", (chunk) => data += chunk);
    process.stdin.on("end", () => resolve(data));
  });
}

function schema(type, extra = {}) {
  return { type, ...extra };
}

const Type = {
  Object: (properties = {}, options = {}) => {
    const required = Object.entries(properties)
      .filter(([, value]) => !(value && value.__optional))
      .map(([key]) => key);
    const clean = {};
    for (const [key, value] of Object.entries(properties)) {
      if (value && value.__optional) {
        const { __optional, ...rest } = value;
        clean[key] = rest;
      } else {
        clean[key] = value;
      }
    }
    return { type: "object", properties: clean, ...(required.length ? { required } : {}), ...options };
  },
  String: (options = {}) => schema("string", options),
  Number: (options = {}) => schema("number", options),
  Integer: (options = {}) => schema("integer", options),
  Boolean: (options = {}) => schema("boolean", options),
  Array: (items, options = {}) => ({ type: "array", items, ...options }),
  Literal: (value, options = {}) => ({ const: value, ...options }),
  Union: (items, options = {}) => ({ anyOf: items, ...options }),
  Optional: (inner) => ({ ...inner, __optional: true }),
  Record: (_key, value, options = {}) => ({ type: "object", additionalProperties: value, ...options }),
  Any: (options = {}) => options,
  Unknown: (options = {}) => options,
  Null: (options = {}) => ({ type: "null", ...options }),
};

function defineTool(tool) {
  return tool;
}

function wrapToolDefinition(definition, ctxFactory) {
  if (!definition || typeof definition !== "object") return definition;
  const wrapped = {
    name: definition.name,
    label: definition.label,
    description: definition.description,
    parameters: definition.parameters,
    prepareArguments: definition.prepareArguments,
    executionMode: definition.executionMode,
  };
  if (definition.promptSnippet !== undefined) wrapped.promptSnippet = definition.promptSnippet;
  if (definition.promptGuidelines !== undefined) wrapped.promptGuidelines = definition.promptGuidelines;
  if (definition.renderShell !== undefined) wrapped.renderShell = definition.renderShell;
  if (definition.renderCall !== undefined) wrapped.renderCall = definition.renderCall;
  if (definition.renderResult !== undefined) wrapped.renderResult = definition.renderResult;
  if (typeof definition.execute === "function") {
    wrapped.execute = (toolCallId, params, signal, onUpdate) =>
      definition.execute(toolCallId, params, signal, onUpdate, ctxFactory ? ctxFactory() : undefined);
  }
  return wrapped;
}

function wrapRegisteredTool(registeredTool, runner) {
  const definition = registeredTool && registeredTool.definition ? registeredTool.definition : registeredTool;
  return wrapToolDefinition(definition, () =>
    runner && typeof runner.createContext === "function" ? runner.createContext() : undefined
  );
}

function wrapRegisteredTools(registeredTools, runner) {
  if (!Array.isArray(registeredTools)) return [];
  return registeredTools.map((registeredTool) => wrapRegisteredTool(registeredTool, runner));
}

function toolEventName(event) {
  return event && typeof event === "object" ? event.toolName : undefined;
}

function isToolCallEventType(toolName, event) {
  return toolEventName(event) === toolName;
}

const isBashToolResult = (event) => toolEventName(event) === "bash";
const isReadToolResult = (event) => toolEventName(event) === "read";
const isEditToolResult = (event) => toolEventName(event) === "edit";
const isWriteToolResult = (event) => toolEventName(event) === "write";
const isGrepToolResult = (event) => toolEventName(event) === "grep";
const isFindToolResult = (event) => toolEventName(event) === "find";
const isLsToolResult = (event) => toolEventName(event) === "ls";

function createEventBus() {
  const listeners = new Map();
  return {
    emit: (channel, data) => {
      const key = String(channel || "");
      const handlers = listeners.get(key);
      if (!handlers) return;
      for (const handler of [...handlers]) {
        try {
          const result = handler(data);
          if (result && typeof result.catch === "function") {
            result.catch((error) => console.error(`Event handler error (${key}):`, error));
          }
        } catch (error) {
          console.error(`Event handler error (${key}):`, error);
        }
      }
    },
    on: (channel, handler) => {
      const key = String(channel || "");
      if (typeof handler !== "function") return () => {};
      const handlers = listeners.get(key) || new Set();
      handlers.add(handler);
      listeners.set(key, handlers);
      return () => {
        const current = listeners.get(key);
        if (!current) return;
        current.delete(handler);
        if (current.size === 0) listeners.delete(key);
      };
    },
    clear: () => {
      listeners.clear();
    },
  };
}

function createExtensionRuntime() {
  const notInitialized = () => {
    throw new Error("Extension runtime not initialized. Action methods cannot be called during extension loading.");
  };
  const state = {};
  const assertActive = () => {
    if (state.staleMessage) throw new Error(state.staleMessage);
  };
  const runtime = {
    sendMessage: notInitialized,
    sendUserMessage: notInitialized,
    appendEntry: notInitialized,
    setSessionName: notInitialized,
    getSessionName: notInitialized,
    setLabel: notInitialized,
    getActiveTools: notInitialized,
    getAllTools: notInitialized,
    setActiveTools: notInitialized,
    refreshTools: () => {},
    getCommands: notInitialized,
    setModel: () => Promise.reject(new Error("Extension runtime not initialized")),
    getThinkingLevel: notInitialized,
    setThinkingLevel: notInitialized,
    flagValues: new Map(),
    pendingProviderRegistrations: [],
    assertActive,
    invalidate: (message) => {
      state.staleMessage ||= message ||
        "This extension ctx is stale after session replacement or reload. Do not use a captured pi or command ctx after ctx.newSession(), ctx.fork(), ctx.switchSession(), or ctx.reload().";
    },
    registerProvider: (name, config, extensionPath = "<unknown>") => {
      runtime.pendingProviderRegistrations.push({ name, config, extensionPath });
    },
    unregisterProvider: (name) => {
      const requested = String(name || "").trim();
      runtime.pendingProviderRegistrations = runtime.pendingProviderRegistrations.filter((item) => item.name !== requested);
    },
  };
  return runtime;
}

function defaultShellPath() {
  if (process.env.AGENT_SHELL_PATH) return process.env.AGENT_SHELL_PATH;
  if (process.env.PI_SHELL_PATH) return process.env.PI_SHELL_PATH;
  if (fs.existsSync("/bin/bash")) return "/bin/bash";
  return "/bin/sh";
}

function createLocalBashOperations(options = {}) {
  return {
    exec: async (command, cwd, execOptions = {}) => {
      const shell = options.shellPath || defaultShellPath();
      return new Promise((resolve, reject) => {
        const child = childProcess.spawn(shell, ["-c", command], {
          cwd,
          env: execOptions.env || process.env,
          stdio: ["ignore", "pipe", "pipe"],
          windowsHide: true,
        });
        let timeoutHandle;
        if (execOptions.timeout !== undefined && execOptions.timeout > 0) {
          timeoutHandle = setTimeout(() => {
            child.kill("SIGKILL");
            reject(new Error(`timeout:${execOptions.timeout}`));
          }, execOptions.timeout * 1000);
        }
        child.stdout?.on("data", execOptions.onData || (() => {}));
        child.stderr?.on("data", execOptions.onData || (() => {}));
        child.on("error", (error) => {
          if (timeoutHandle) clearTimeout(timeoutHandle);
          reject(error);
        });
        child.on("close", (code) => {
          if (timeoutHandle) clearTimeout(timeoutHandle);
          resolve({ exitCode: code });
        });
      });
    },
  };
}

function ensureParentDir(filePath) {
  const parent = path.dirname(filePath);
  if (parent && parent !== ".") fs.mkdirSync(parent, { recursive: true });
}

function createLocalFileOperations() {
  return {
    readFile: async (filePath, options = {}) => {
      const encoding = options.encoding === null ? null : (options.encoding || "utf8");
      return fs.promises.readFile(filePath, encoding);
    },
    readTextFile: async (filePath) => fs.promises.readFile(filePath, "utf8"),
    writeFile: async (filePath, content, options = {}) => {
      ensureParentDir(filePath);
      const data = typeof content === "string" || Buffer.isBuffer(content) ? content : String(content);
      await fs.promises.writeFile(filePath, data, options);
      return { path: filePath, bytes: Buffer.byteLength(data) };
    },
    writeTextFile: async (filePath, content) => {
      ensureParentDir(filePath);
      const data = String(content);
      await fs.promises.writeFile(filePath, data, "utf8");
      return { path: filePath, bytes: Buffer.byteLength(data) };
    },
    appendFile: async (filePath, content, options = {}) => {
      ensureParentDir(filePath);
      const data = typeof content === "string" || Buffer.isBuffer(content) ? content : String(content);
      await fs.promises.appendFile(filePath, data, options);
      return { path: filePath, bytes: Buffer.byteLength(data) };
    },
    listDir: async (dirPath = ".") => fs.promises.readdir(dirPath),
    readdir: async (dirPath = ".") => fs.promises.readdir(dirPath),
    exists: async (filePath) => fs.existsSync(filePath),
    stat: async (filePath) => {
      const s = await fs.promises.stat(filePath);
      return {
        path: filePath,
        size: s.size,
        isFile: s.isFile(),
        isDirectory: s.isDirectory(),
        mtimeMs: s.mtimeMs,
      };
    },
  };
}

function createLocalToolOperations(options = {}) {
  const bash = createLocalBashOperations(options.bash || {});
  const files = createLocalFileOperations();
  return {
    ...files,
    exec: bash.exec,
    bash,
    bashOperations: bash,
    file: files,
    files,
    fs: files,
  };
}

const DEFAULT_MAX_LINES = 2000;
const DEFAULT_MAX_BYTES = 50 * 1024;

function splitLinesForCounting(content) {
  if (content.length === 0) return [];
  const lines = content.split("\n");
  if (content.endsWith("\n")) lines.pop();
  return lines;
}

function formatSize(bytes) {
  const value = Number(bytes || 0);
  if (value < 1024) return `${value}B`;
  if (value < 1024 * 1024) return `${(value / 1024).toFixed(1)}KB`;
  return `${(value / (1024 * 1024)).toFixed(1)}MB`;
}

function truncationResult(content, truncated, truncatedBy, totalLines, totalBytes, maxLines, maxBytes, extra = {}) {
  return {
    content,
    truncated,
    truncatedBy,
    totalLines,
    totalBytes,
    outputLines: splitLinesForCounting(content).length,
    outputBytes: Buffer.byteLength(content, "utf8"),
    lastLinePartial: false,
    firstLineExceedsLimit: false,
    maxLines,
    maxBytes,
    ...extra,
  };
}

function truncateHead(content, options = {}) {
  const text = String(content || "");
  const maxLines = options.maxLines ?? DEFAULT_MAX_LINES;
  const maxBytes = options.maxBytes ?? DEFAULT_MAX_BYTES;
  const lines = splitLinesForCounting(text);
  const totalLines = lines.length;
  const totalBytes = Buffer.byteLength(text, "utf8");
  if (totalLines <= maxLines && totalBytes <= maxBytes) {
    return truncationResult(text, false, null, totalLines, totalBytes, maxLines, maxBytes);
  }
  if (lines.length && Buffer.byteLength(lines[0], "utf8") > maxBytes) {
    return truncationResult("", true, "bytes", totalLines, totalBytes, maxLines, maxBytes, { firstLineExceedsLimit: true });
  }
  const out = [];
  let bytes = 0;
  let truncatedBy = "lines";
  for (const line of lines) {
    if (out.length >= maxLines) {
      truncatedBy = "lines";
      break;
    }
    const candidate = out.length === 0 ? line : `\n${line}`;
    const candidateBytes = Buffer.byteLength(candidate, "utf8");
    if (bytes + candidateBytes > maxBytes) {
      truncatedBy = "bytes";
      break;
    }
    out.push(line);
    bytes += candidateBytes;
  }
  return truncationResult(out.join("\n"), true, truncatedBy, totalLines, totalBytes, maxLines, maxBytes);
}

function truncateTail(content, options = {}) {
  const text = String(content || "");
  const maxLines = options.maxLines ?? DEFAULT_MAX_LINES;
  const maxBytes = options.maxBytes ?? DEFAULT_MAX_BYTES;
  const lines = splitLinesForCounting(text);
  const totalLines = lines.length;
  const totalBytes = Buffer.byteLength(text, "utf8");
  if (totalLines <= maxLines && totalBytes <= maxBytes) {
    return truncationResult(text, false, null, totalLines, totalBytes, maxLines, maxBytes);
  }
  const out = [];
  let bytes = 0;
  let truncatedBy = "lines";
  for (let i = lines.length - 1; i >= 0; i--) {
    if (out.length >= maxLines) {
      truncatedBy = "lines";
      break;
    }
    const candidate = out.length === 0 ? lines[i] : `${lines[i]}\n`;
    const candidateBytes = Buffer.byteLength(candidate, "utf8");
    if (bytes + candidateBytes > maxBytes) {
      truncatedBy = "bytes";
      break;
    }
    out.unshift(lines[i]);
    bytes += candidateBytes;
  }
  return truncationResult(out.join("\n"), true, truncatedBy, totalLines, totalBytes, maxLines, maxBytes);
}

function truncateLine(line, maxBytes = DEFAULT_MAX_BYTES) {
  const text = String(line || "");
  if (Buffer.byteLength(text, "utf8") <= maxBytes) return { content: text, truncated: false };
  let out = "";
  for (const char of text) {
    if (Buffer.byteLength(out + char, "utf8") > maxBytes) break;
    out += char;
  }
  return { content: out, truncated: true };
}

function toolTextResult(text, details = {}) {
  return { content: [{ type: "text", text: String(text ?? "") }], details };
}

function toolPath(input) {
  return String(input && (input.path ?? input.filePath ?? input.file_path ?? input.target ?? "") || "");
}

function resolveToolPath(cwd, filePath) {
  const raw = String(filePath || "");
  return path.isAbsolute(raw) ? raw : path.resolve(cwd || process.cwd(), raw);
}

function createReadToolDefinition(cwd = process.cwd(), options = {}) {
  const operations = options.operations || createLocalFileOperations();
  return {
    name: "read",
    label: "Read",
    description: `Read the contents of a file. Supports text files. Output is truncated to ${DEFAULT_MAX_LINES} lines or ${DEFAULT_MAX_BYTES / 1024}KB.`,
    parameters: Type.Object({
      path: Type.String({ description: "File path to read" }),
      offset: Type.Optional(Type.Integer({ description: "1-based line offset" })),
      limit: Type.Optional(Type.Integer({ description: "Maximum number of lines" })),
    }),
    execute: async (_toolCallId, input = {}) => {
      const filePath = toolPath(input);
      const absolutePath = resolveToolPath(cwd, filePath);
      let text = await operations.readFile(absolutePath, { encoding: "utf8" });
      text = Buffer.isBuffer(text) ? text.toString("utf8") : String(text);
      const lines = text.split(/\r?\n/);
      if (input.offset !== undefined || input.limit !== undefined) {
        const offset = Math.max(1, Number(input.offset || 1));
        const limit = input.limit === undefined ? lines.length : Math.max(0, Number(input.limit));
        text = lines.slice(offset - 1, offset - 1 + limit).join("\n");
      }
      const truncated = truncateHead(text, options.truncation || {});
      return toolTextResult(truncated.content, { path: filePath, absolutePath, truncation: truncated });
    },
  };
}

function createWriteToolDefinition(cwd = process.cwd(), options = {}) {
  const operations = options.operations || createLocalFileOperations();
  return {
    name: "write",
    label: "Write",
    description: "Write content to a file, creating parent directories when needed.",
    parameters: Type.Object({
      path: Type.String({ description: "File path to write" }),
      content: Type.String({ description: "Content to write" }),
    }),
    execute: async (_toolCallId, input = {}) => {
      const filePath = toolPath(input);
      const absolutePath = resolveToolPath(cwd, filePath);
      const content = String(input.content ?? input.text ?? "");
      const result = await withFileMutationQueue(absolutePath, () => operations.writeFile(absolutePath, content, "utf8"));
      const bytes = result && typeof result.bytes === "number" ? result.bytes : Buffer.byteLength(content);
      return toolTextResult(`Wrote ${bytes} bytes to ${filePath}`, { path: filePath, absolutePath, bytes });
    },
  };
}

function createEditToolDefinition(cwd = process.cwd(), options = {}) {
  const operations = options.operations || createLocalFileOperations();
  return {
    name: "edit",
    label: "Edit",
    description: "Replace text in a file.",
    parameters: Type.Object({
      path: Type.String({ description: "File path to edit" }),
      old_str: Type.Optional(Type.String({ description: "Text to replace" })),
      new_str: Type.Optional(Type.String({ description: "Replacement text" })),
      oldText: Type.Optional(Type.String({ description: "Text to replace" })),
      newText: Type.Optional(Type.String({ description: "Replacement text" })),
    }),
    execute: async (_toolCallId, input = {}) => {
      const filePath = toolPath(input);
      const absolutePath = resolveToolPath(cwd, filePath);
      const oldText = String(input.old_str ?? input.oldText ?? "");
      const newText = String(input.new_str ?? input.newText ?? "");
      return withFileMutationQueue(absolutePath, async () => {
        const beforeRaw = await operations.readFile(absolutePath, { encoding: "utf8" });
        const before = Buffer.isBuffer(beforeRaw) ? beforeRaw.toString("utf8") : String(beforeRaw);
        if (!before.includes(oldText)) throw new Error("old text not found");
        const after = before.replace(oldText, newText);
        await operations.writeFile(absolutePath, after, "utf8");
        return toolTextResult(`Edited ${filePath}`, { path: filePath, absolutePath, replacements: 1 });
      });
    },
  };
}

function createBashToolDefinition(cwd = process.cwd(), options = {}) {
  const operations = options.operations || createLocalBashOperations(options);
  return {
    name: "bash",
    label: "Bash",
    description: `Execute a bash command in the current working directory. Output is truncated to last ${DEFAULT_MAX_LINES} lines or ${DEFAULT_MAX_BYTES / 1024}KB.`,
    parameters: Type.Object({
      command: Type.String({ description: "Command to execute" }),
      timeout: Type.Optional(Type.Number({ description: "Timeout in seconds" })),
      cwd: Type.Optional(Type.String({ description: "Working directory" })),
    }),
    execute: async (_toolCallId, input = {}) => {
      let command = String(input.command || "");
      let commandCwd = resolveToolPath(cwd, input.cwd || ".");
      let env = process.env;
      if (typeof options.spawnHook === "function") {
        const hooked = await options.spawnHook({ command, cwd: commandCwd, env });
        if (hooked && typeof hooked === "object") {
          if (hooked.command) command = String(hooked.command);
          if (hooked.cwd) commandCwd = String(hooked.cwd);
          if (hooked.env) env = hooked.env;
        }
      }
      const chunks = [];
      const result = await operations.exec(command, commandCwd, {
        timeout: input.timeout,
        env,
        onData: (data) => chunks.push(outputText(data)),
      });
      const output = truncateTail(chunks.join("").replace(/\r/g, ""), options.truncation || {});
      return toolTextResult(output.content, { command, cwd: commandCwd, exitCode: result.exitCode ?? 0, truncation: output });
    },
  };
}

function walkFiles(root, limit = 10000) {
  const out = [];
  const visit = (dir) => {
    if (out.length >= limit) return;
    let entries = [];
    try {
      entries = fs.readdirSync(dir, { withFileTypes: true });
    } catch {
      return;
    }
    for (const entry of entries) {
      if (entry.name === ".git" || entry.name === "node_modules" || entry.name === "_build") continue;
      const full = path.join(dir, entry.name);
      if (entry.isDirectory()) visit(full);
      else if (entry.isFile()) out.push(full);
      if (out.length >= limit) return;
    }
  };
  visit(root);
  return out;
}

function globToRegExp(glob) {
  const escaped = String(glob || "*").replace(/[.+^${}()|[\]\\]/g, "\\$&").replace(/\*/g, ".*").replace(/\?/g, ".");
  return new RegExp(`^${escaped}$`);
}

function createGrepToolDefinition(cwd = process.cwd(), options = {}) {
  return {
    name: "grep",
    label: "Grep",
    description: "Search file contents for a pattern.",
    parameters: Type.Object({
      pattern: Type.String({ description: "Pattern to search for" }),
      path: Type.Optional(Type.String({ description: "Directory to search" })),
      include: Type.Optional(Type.String({ description: "Filename glob" })),
    }),
    execute: async (_toolCallId, input = {}) => {
      const root = resolveToolPath(cwd, input.path || ".");
      const pattern = String(input.pattern ?? input.query ?? "");
      const re = input.regex === false ? null : new RegExp(pattern, input.ignoreCase ? "i" : "");
      const include = input.include ? globToRegExp(input.include) : null;
      const matches = [];
      for (const file of walkFiles(root, options.limit || 10000)) {
        const rel = path.relative(cwd, file);
        if (include && !include.test(path.basename(file)) && !include.test(rel)) continue;
        let text;
        try {
          text = fs.readFileSync(file, "utf8");
        } catch {
          continue;
        }
        text.split(/\r?\n/).forEach((line, index) => {
          const ok = re ? re.test(line) : line.includes(pattern);
          if (ok) matches.push(`${rel}:${index + 1}:${line}`);
        });
      }
      const truncated = truncateHead(matches.join("\n"), options.truncation || {});
      return toolTextResult(truncated.content, { matches: matches.length, truncation: truncated });
    },
  };
}

function createFindToolDefinition(cwd = process.cwd(), options = {}) {
  return {
    name: "find",
    label: "Find",
    description: "Search for files by glob pattern.",
    parameters: Type.Object({
      pattern: Type.String({ description: "Glob pattern" }),
      path: Type.Optional(Type.String({ description: "Directory to search" })),
    }),
    execute: async (_toolCallId, input = {}) => {
      const root = resolveToolPath(cwd, input.path || ".");
      const pattern = globToRegExp(input.pattern || input.glob || "*");
      const matches = walkFiles(root, options.limit || 10000)
        .map((file) => path.relative(cwd, file))
        .filter((rel) => pattern.test(path.basename(rel)) || pattern.test(rel))
        .sort();
      const truncated = truncateHead(matches.join("\n"), options.truncation || {});
      return toolTextResult(truncated.content, { matches: matches.length, truncation: truncated });
    },
  };
}

function createLsToolDefinition(cwd = process.cwd(), options = {}) {
  const operations = options.operations || createLocalFileOperations();
  return {
    name: "ls",
    label: "List",
    description: "List directory contents.",
    parameters: Type.Object({
      path: Type.Optional(Type.String({ description: "Directory path" })),
    }),
    execute: async (_toolCallId, input = {}) => {
      const dir = resolveToolPath(cwd, input.path || ".");
      const entries = await operations.listDir(dir);
      const rendered = [];
      for (const entry of entries.sort()) {
        const full = path.join(dir, entry);
        let suffix = "";
        try {
          suffix = fs.statSync(full).isDirectory() ? "/" : "";
        } catch {}
        rendered.push(entry + suffix);
      }
      const truncated = truncateHead(rendered.join("\n"), options.truncation || {});
      return toolTextResult(truncated.content, { path: input.path || ".", entries: entries.length, truncation: truncated });
    },
  };
}

function createReadTool(cwd, options) { return wrapToolDefinition(createReadToolDefinition(cwd, options)); }
function createWriteTool(cwd, options) { return wrapToolDefinition(createWriteToolDefinition(cwd, options)); }
function createEditTool(cwd, options) { return wrapToolDefinition(createEditToolDefinition(cwd, options)); }
function createBashTool(cwd, options) { return wrapToolDefinition(createBashToolDefinition(cwd, options)); }
function createGrepTool(cwd, options) { return wrapToolDefinition(createGrepToolDefinition(cwd, options)); }
function createFindTool(cwd, options) { return wrapToolDefinition(createFindToolDefinition(cwd, options)); }
function createLsTool(cwd, options) { return wrapToolDefinition(createLsToolDefinition(cwd, options)); }

function createCodingTools(cwd, options = {}) {
  return [
    createReadTool(cwd, options.read),
    createBashTool(cwd, options.bash),
    createEditTool(cwd, options.edit),
    createWriteTool(cwd, options.write),
  ];
}

function createReadOnlyTools(cwd, options = {}) {
  return [
    createReadTool(cwd, options.read),
    createGrepTool(cwd, options.grep),
    createFindTool(cwd, options.find),
    createLsTool(cwd, options.ls),
  ];
}

function createCodingToolDefinitions(cwd, options = {}) {
  return [
    createReadToolDefinition(cwd, options.read),
    createBashToolDefinition(cwd, options.bash),
    createEditToolDefinition(cwd, options.edit),
    createWriteToolDefinition(cwd, options.write),
  ];
}

function createReadOnlyToolDefinitions(cwd, options = {}) {
  return [
    createReadToolDefinition(cwd, options.read),
    createGrepToolDefinition(cwd, options.grep),
    createFindToolDefinition(cwd, options.find),
    createLsToolDefinition(cwd, options.ls),
  ];
}

function bridgeHomeDir() {
  return process.env.HOME || process.cwd();
}

function expandBridgeTilde(value) {
  const text = String(value || "");
  if (text === "~") return bridgeHomeDir();
  if (text.startsWith("~/")) return path.join(bridgeHomeDir(), text.slice(2));
  return text;
}

function getAgentDir() {
  return expandBridgeTilde(process.env.PI_CODING_AGENT_DIR || process.env.AGENT_CODING_AGENT_DIR || process.env.OCAML_AGENT_DIR || process.env.AGENT_DIR || path.join(bridgeHomeDir(), ".pi", "agent"));
}

const VERSION = "ocaml-agent";

function defaultSessionsDir() {
  return expandBridgeTilde(process.env.AGENT_SESSION_DIR || process.env.PI_CODING_AGENT_SESSION_DIR || path.join(getAgentDir(), "sessions"));
}

function readJsonLines(filePath) {
  try {
    return fs.readFileSync(filePath, "utf8")
      .split(/\r?\n/)
      .map((line) => line.trim())
      .filter(Boolean)
      .map((line) => {
        try {
          return JSON.parse(line);
        } catch {
          return null;
        }
      })
      .filter(Boolean);
  } catch {
    return [];
  }
}

function sessionDate(value, fallbackMs = 0) {
  if (typeof value === "number") return new Date(value > 100000000000 ? value : value * 1000);
  if (typeof value === "string" && value.trim()) {
    const parsed = Date.parse(value);
    if (!Number.isNaN(parsed)) return new Date(parsed);
  }
  return new Date(fallbackMs || 0);
}

function textFromSessionContent(content) {
  if (typeof content === "string") return content;
  if (Array.isArray(content)) {
    return content.map((item) => item && item.type === "text" ? item.text || "" : "").filter(Boolean).join(" ");
  }
  return "";
}

function sessionInfoFromFile(filePath) {
  const lines = readJsonLines(filePath);
  if (lines.length === 0) return null;
  const header = lines[0] || {};
  const isOcamlHeader = header._session !== undefined;
  const isPiHeader = header.type === "session";
  if (!isOcamlHeader && !isPiHeader) return null;
  let name = typeof header.name === "string" ? header.name : undefined;
  let messageCount = 0;
  let firstMessage = "";
  const allMessages = [];
  for (const entry of lines.slice(1)) {
    if (entry && entry.type === "session_info" && typeof entry.name === "string") name = entry.name || undefined;
    let message = null;
    if (entry && entry.type === "message" && entry.message) message = entry.message;
    else if (entry && typeof entry.role === "string") message = entry;
    if (!message) continue;
    messageCount += 1;
    if (message.role !== "user" && message.role !== "assistant") continue;
    const text = textFromSessionContent(message.content);
    if (!text) continue;
    allMessages.push(text);
    if (!firstMessage && message.role === "user") firstMessage = text;
  }
  let stats = null;
  try {
    stats = fs.statSync(filePath);
  } catch {}
  const created = isOcamlHeader ? sessionDate(header.created, stats ? stats.birthtimeMs : 0) : sessionDate(header.timestamp, stats ? stats.birthtimeMs : 0);
  const modified = stats ? stats.mtime : created;
  return {
    path: filePath,
    file: filePath,
    id: String(header.id || path.basename(filePath, ".jsonl")),
    cwd: String(header.cwd || ""),
    name,
    parentSessionPath: header.parentSession,
    created,
    modified,
    messageCount,
    firstMessage: firstMessage || "(no messages)",
    allMessagesText: allMessages.join(" "),
  };
}

function listSessionsFromDir(sessionDir) {
  let entries = [];
  try {
    entries = fs.readdirSync(sessionDir);
  } catch {
    return [];
  }
  return entries
    .filter((name) => name.endsWith(".jsonl"))
    .map((name) => sessionInfoFromFile(path.join(sessionDir, name)))
    .filter(Boolean)
    .sort((a, b) => b.modified.getTime() - a.modified.getTime());
}

class SessionManager {
  static async list(_cwd = process.cwd(), sessionDir = undefined, onProgress = undefined) {
    const dir = sessionDir ? expandBridgeTilde(sessionDir) : defaultSessionsDir();
    const sessions = listSessionsFromDir(dir);
    if (typeof onProgress === "function") onProgress(sessions.length, sessions.length);
    return sessions;
  }

  static async listAll(onProgress = undefined) {
    const root = defaultSessionsDir();
    const sessions = [];
    const addDir = (dir) => {
      for (const session of listSessionsFromDir(dir)) sessions.push(session);
    };
    addDir(root);
    try {
      for (const entry of fs.readdirSync(root, { withFileTypes: true })) {
        if (entry.isDirectory()) addDir(path.join(root, entry.name));
      }
    } catch {}
    sessions.sort((a, b) => b.modified.getTime() - a.modified.getTime());
    if (typeof onProgress === "function") onProgress(sessions.length, sessions.length);
    return sessions;
  }
}

const fileMutationQueues = new Map();
let fileMutationRegistrationQueue = Promise.resolve();

function isMissingPathError(error) {
  return error && typeof error === "object" && (error.code === "ENOENT" || error.code === "ENOTDIR");
}

async function fileMutationQueueKey(filePath) {
  const resolvedPath = path.resolve(String(filePath || ""));
  try {
    return await fs.promises.realpath(resolvedPath);
  } catch (error) {
    if (isMissingPathError(error)) return resolvedPath;
    throw error;
  }
}

async function withFileMutationQueue(filePath, fn) {
  if (typeof fn !== "function") throw new Error("withFileMutationQueue requires a function");
  const registration = fileMutationRegistrationQueue.then(async () => {
    const key = await fileMutationQueueKey(filePath);
    const currentQueue = fileMutationQueues.get(key) || Promise.resolve();
    let releaseNext = () => {};
    const nextQueue = new Promise((resolve) => {
      releaseNext = resolve;
    });
    const chainedQueue = currentQueue.then(() => nextQueue);
    fileMutationQueues.set(key, chainedQueue);
    return { key, currentQueue, chainedQueue, releaseNext };
  });
  fileMutationRegistrationQueue = registration.then(() => undefined, () => undefined);

  const { key, currentQueue, chainedQueue, releaseNext } = await registration;
  await currentQueue;
  try {
    return await fn();
  } finally {
    releaseNext();
    if (fileMutationQueues.get(key) === chainedQueue) fileMutationQueues.delete(key);
  }
}

function execCommand(command, args = [], options = {}) {
  return new Promise((resolve) => {
    const stdout = [];
    const stderr = [];
    let killed = false;
    let settled = false;
    let timeoutHandle;
    const finish = (result) => {
      if (settled) return;
      settled = true;
      if (timeoutHandle) clearTimeout(timeoutHandle);
      resolve(result);
    };
    const child = childProcess.spawn(String(command), Array.isArray(args) ? args.map(String) : [], {
      cwd: options.cwd || process.cwd(),
      env: options.env || process.env,
      stdio: ["ignore", "pipe", "pipe"],
      windowsHide: true,
    });
    const kill = () => {
      killed = true;
      try {
        child.kill("SIGKILL");
      } catch {}
    };
    if (options.timeout !== undefined && options.timeout > 0) {
      timeoutHandle = setTimeout(kill, options.timeout);
    }
    if (options.signal) {
      if (options.signal.aborted) kill();
      else options.signal.addEventListener("abort", kill, { once: true });
    }
    child.stdout?.on("data", (chunk) => stdout.push(Buffer.from(chunk)));
    child.stderr?.on("data", (chunk) => stderr.push(Buffer.from(chunk)));
    child.on("error", (error) => {
      finish({
        stdout: "",
        stderr: error && error.message ? error.message : String(error),
        output: error && error.message ? error.message : String(error),
        code: 1,
        exitCode: 1,
        killed,
      });
    });
    child.on("close", (code) => {
      const out = Buffer.concat(stdout).toString("utf8");
      const err = Buffer.concat(stderr).toString("utf8");
      const normalizedCode = typeof code === "number" ? code : killed ? 1 : 0;
      finish({
        stdout: out,
        stderr: err,
        output: out + err,
        code: normalizedCode,
        exitCode: normalizedCode,
        killed,
      });
    });
  });
}

class BridgeTextComponent {
  constructor(content = "") {
    this.content = content;
  }
  render() {
    if (Array.isArray(this.content)) return this.content.map(String);
    return String(this.content || "").split(/\r?\n/);
  }
}

class Container {
  constructor() {
    this.children = [];
  }
  addChild(child) {
    if (child !== undefined && child !== null) this.children.push(child);
  }
  clear() {
    this.children = [];
  }
  render(width = 80) {
    return this.children.flatMap((child) => componentLines(child, width));
  }
}

class Box extends Container {}

class CustomEditor extends BridgeTextComponent {
  constructor(_tui, _theme, _keybindings, options = {}) {
    super("");
    this.options = options;
    this.text = "";
  }
  handleInput(data) {
    this.text += String(data || "");
  }
  setText(text) {
    this.text = String(text || "");
  }
  getText() {
    return this.text;
  }
  render() {
    return this.text ? this.text.split(/\r?\n/) : [];
  }
}

function keyText(key) {
  return String(key || "");
}

function keyHint(key) {
  return String(key || "");
}

function rawKeyHint(key) {
  return String(key || "");
}

function renderDiff(value) {
  return String(value || "").split(/\r?\n/);
}

function piCodingAgentExports() {
  return {
    defineTool,
    createExtensionRuntime,
    loadExtensionFromFactory,
    loadExtensions,
    discoverAndLoadExtensions,
    ExtensionRunner,
    wrapRegisteredTool,
    wrapRegisteredTools,
    isToolCallEventType,
    isBashToolResult,
    isReadToolResult,
    isEditToolResult,
    isWriteToolResult,
    isGrepToolResult,
    isFindToolResult,
    isLsToolResult,
    createLocalBashOperations,
    createLocalFileOperations,
    createLocalToolOperations,
    createReadToolDefinition,
    createWriteToolDefinition,
    createEditToolDefinition,
    createBashToolDefinition,
    createGrepToolDefinition,
    createFindToolDefinition,
    createLsToolDefinition,
    createReadTool,
    createWriteTool,
    createEditTool,
    createBashTool,
    createGrepTool,
    createFindTool,
    createLsTool,
    createCodingTools,
    createReadOnlyTools,
    createCodingToolDefinitions,
    createReadOnlyToolDefinitions,
    DEFAULT_MAX_LINES,
    DEFAULT_MAX_BYTES,
    getAgentDir,
    VERSION,
    SessionManager,
    formatSize,
    truncateHead,
    truncateTail,
    truncateLine,
    withFileMutationQueue,
    createEventBus,
    CustomEditor,
    Container,
    Box,
    Text: BridgeTextComponent,
    Markdown: BridgeTextComponent,
    keyText,
    keyHint,
    rawKeyHint,
    renderDiff,
  };
}

function localRequire(baseDir) {
  return (specifier) => {
    if (specifier === "typebox" || specifier === "@sinclair/typebox") return { Type };
    if (specifier === "typebox/compile" || specifier === "@sinclair/typebox/compile") return {};
    if (specifier === "typebox/value" || specifier === "@sinclair/typebox/value") return {};
    if (specifier === "@earendil-works/pi-coding-agent" || specifier === "@mariozechner/pi-coding-agent") {
      return piCodingAgentExports();
    }
    if (specifier === "@earendil-works/pi-tui" || specifier === "@mariozechner/pi-tui") {
      return piCodingAgentExports();
    }
    if (specifier.startsWith(".")) return require(path.resolve(baseDir, specifier));
    return require(specifier);
  };
}

function transformModule(source) {
  let code = source;
  code = code.replace(/^\s*import\s+type\s+[^;]+;?\s*$/mg, "");
  code = code.replace(/^\s*import\s+\{([^}]+)\}\s+from\s+["']([^"']+)["'];?\s*$/mg,
    (_m, names, spec) => `const {${names}} = require(${JSON.stringify(spec)});`);
  code = code.replace(/^\s*import\s+([A-Za-z_$][\w$]*)\s+from\s+["']([^"']+)["'];?\s*$/mg,
    (_m, name, spec) => `const ${name} = require(${JSON.stringify(spec)}).default ?? require(${JSON.stringify(spec)});`);
  code = code.replace(/export\s+default\s+async\s+function\s*/g, "module.exports.default = async function ");
  code = code.replace(/export\s+default\s+function\s*/g, "module.exports.default = function ");
  code = code.replace(/export\s+default\s+async\s*\(/g, "module.exports.default = async (");
  code = code.replace(/export\s+default\s*\(/g, "module.exports.default = (");
  code = code.replace(/export\s+default\s+/g, "module.exports.default = ");
  code = code.replace(/export\s+\{[^}]+\};?\s*$/mg, "");
  return code;
}

async function loadFactory(extensionPath) {
  const ext = path.extname(extensionPath);
  if (ext === ".mjs") {
    const mod = await import(pathToFileURL(extensionPath).href);
    return mod.default ?? mod;
  }
  if (ext === ".cjs") {
    const mod = require(extensionPath);
    return mod.default ?? mod;
  }
  const source = fs.readFileSync(extensionPath, "utf8");
  const module = { exports: {} };
  const dirname = path.dirname(extensionPath);
  const sandbox = {
    module,
    exports: module.exports,
    require: localRequire(dirname),
    console,
    process,
    Buffer,
    setTimeout,
    clearTimeout,
    setInterval,
    clearInterval,
    fetch: globalThis.fetch,
    URL,
    AbortController,
  };
  vm.runInNewContext(transformModule(source), sandbox, { filename: extensionPath });
  return module.exports.default ?? module.exports;
}

function normalizeFlagName(name) {
  return String(name || "").trim();
}

function flagEnvName(prefix, name) {
  return `${prefix}${normalizeFlagName(name).replace(/[^A-Za-z0-9]/g, "_").toUpperCase()}`;
}

function parseFlagValue(raw, options = {}) {
  const type = options.type || options.kind || typeof (options.defaultValue ?? options.default);
  if (type === "boolean") return /^(1|true|yes|y|on)$/i.test(String(raw).trim());
  if (type === "number" || type === "integer") {
    const n = Number(raw);
    return Number.isFinite(n) ? n : raw;
  }
  return raw;
}

function normalizeThinkingLevel(level) {
  const value = String(level || "").trim().toLowerCase();
  if (value === "" || value === "none") return "off";
  if (value === "minimal") return "low";
  if (value === "xhigh") return "high";
  return value;
}

function modelField(value, names) {
  if (!value || typeof value !== "object") return "";
  for (const name of names) {
    const field = value[name];
    if (typeof field === "string" && field.trim()) return field.trim();
    if (field && typeof field === "object") {
      const nested = modelField(field, ["id", "name", "provider", "model"]);
      if (nested) return nested;
    }
  }
  return "";
}

function normalizeModelSpec(model) {
  if (typeof model === "string") {
    const spec = model.trim();
    return spec ? { model: spec } : null;
  }
  if (!model || typeof model !== "object") return null;
  const provider = modelField(model, ["provider", "providerId", "providerName"]);
  const id = modelField(model, ["id", "modelId", "model", "name"]);
  const thinking = modelField(model, ["thinkingLevel", "thinking"]);
  const out = {};
  if (provider) out.provider = provider;
  if (id) out.model = id;
  if (thinking) out.thinking = normalizeThinkingLevel(thinking);
  return out.provider || out.model || out.thinking ? out : null;
}

function normalizeThemeInfo(theme) {
  if (typeof theme === "string") {
    const name = theme.trim();
    return name ? { name } : null;
  }
  if (!theme || typeof theme !== "object") return null;
  const name = String(theme.name || theme.id || "").trim();
  if (!name) return null;
  const out = { ...safeSerializable(theme), name };
  if (theme.path && !out.path) out.path = String(theme.path);
  if (theme.location && !out.path) out.path = String(theme.location);
  return out;
}

function themeName(theme) {
  if (typeof theme === "string") return theme.trim();
  if (theme && typeof theme === "object") return String(theme.name || theme.id || "").trim();
  return "";
}

function defaultFlagValue(options = {}) {
  if (Object.prototype.hasOwnProperty.call(options, "defaultValue")) return options.defaultValue;
  if (Object.prototype.hasOwnProperty.call(options, "default")) return options.default;
  return undefined;
}

function getRegisteredFlagValue(flags, flagValues, name) {
  const key = normalizeFlagName(name);
  const options = flags.get(key) || {};
  if (Object.prototype.hasOwnProperty.call(flagValues, key)) return flagValues[key];
  for (const envName of [flagEnvName("PI_FLAG_", key), flagEnvName("AGENT_FLAG_", key)]) {
    if (Object.prototype.hasOwnProperty.call(process.env, envName) && process.env[envName] !== undefined && process.env[envName] !== "") {
      return parseFlagValue(process.env[envName], options);
    }
  }
  return defaultFlagValue(options);
}

function normalizeShortcutSpec(spec) {
  return String(spec || "")
    .trim()
    .toLowerCase()
    .replace(/^c-/, "ctrl+")
    .replace(/^control\+/, "ctrl+")
    .replace(/\s+/g, "");
}

function normalizeToolName(name) {
  const raw = String(name || "").trim();
  if (raw === "read_file") return "read";
  if (raw === "write_file") return "write";
  if (raw === "edit_file") return "edit";
  if (raw === "list_dir") return "ls";
  if (raw === "run_bash") return "bash";
  if (raw === "subagent") return "task";
  return raw;
}

function toolInfoName(tool) {
  return normalizeToolName(tool && tool.name);
}

function makeApi(registry, extensionPath = "<unknown>") {
  const { tools, commands, handlers, flags, flagValues, shortcuts, providers, renderers, messages } = registry;
  const noop = () => {};
  const registerRenderer = (name, options) => {
    let entry = {};
    if (name && typeof name === "object") {
      entry = { ...name };
    } else if (typeof name === "string") {
      entry = typeof options === "function" ? { render: options, name } : { ...(options || {}), name };
    }
    const rendererName = String(entry.name || entry.id || entry.type || "").trim();
    if (rendererName && (typeof entry.render === "function" || typeof entry.handler === "function")) {
      renderers.set(rendererName, entry);
    }
  };
  const registerMessageRenderer = (customType, renderer) => {
    if (typeof customType === "string" && customType.trim()) {
      if (typeof renderer === "function") {
        renderers.set(customType, {
          name: customType,
          customType,
          target: "custom_message",
          renderMessage: renderer,
        });
        return;
      }
      if (renderer && typeof renderer === "object" && !(renderer.target || renderer.kind || renderer.type)) {
        renderers.set(customType, {
          ...renderer,
          name: customType,
          customType,
          target: "custom_message",
          renderMessage: typeof renderer.render === "function" ? renderer.render : renderer.handler,
        });
        return;
      }
    }
    registerRenderer(customType, renderer);
  };
  return {
    on: (event, handler) => {
      if (typeof event !== "string" || typeof handler !== "function") return;
      const list = handlers.get(event) || [];
      list.push(handler);
      handlers.set(event, list);
    },
    registerCommand: (name, options) => {
      if (typeof name === "string" && name) commands.set(name, options || {});
    },
    registerShortcut: (shortcut, options) => {
      let spec = "";
      let entry = {};
      if (shortcut && typeof shortcut === "object") {
        spec = shortcut.key || shortcut.shortcut || shortcut.binding || shortcut.keybinding || shortcut.keys || "";
        entry = { ...shortcut };
      } else if (typeof shortcut === "string") {
        spec = shortcut;
        entry = typeof options === "function" ? { handler: options } : { ...(options || {}) };
      }
      spec = normalizeShortcutSpec(spec);
      if (!spec) return;
      shortcuts.set(spec, { ...entry, spec });
    },
    registerFlag: (name, options) => {
      if (typeof name === "object" && name && typeof name.name === "string") {
        flags.set(normalizeFlagName(name.name), { ...name });
      } else if (typeof name === "string" && normalizeFlagName(name)) {
        flags.set(normalizeFlagName(name), options || {});
      }
    },
    registerMessageRenderer,
    registerRenderer,
    registerComponentRenderer: registerRenderer,
    registerOutputRenderer: registerRenderer,
    registerProvider: (provider, options) => {
      let entry = {};
      if (provider && typeof provider === "object") {
        entry = { ...provider };
      } else if (typeof provider === "string") {
        entry = { ...(options || {}), name: provider };
      }
      const name = String(entry.name || entry.id || entry.provider || "").trim();
      if (name) {
        const provider = { ...entry, name, extensionPath };
        providers.set(name, provider);
        registry.providersChanged = true;
        for (const providerName of providerNames(provider)) registry.unregisteredProviders.delete(providerName);
        registry.unregisteredProviders.delete(name.toLowerCase());
      }
    },
    unregisterProvider: (name) => {
      const requested = String(name || "").trim();
      if (!requested) return;
      const normalized = requested.toLowerCase();
      for (const [key, provider] of providers.entries()) {
        const keyMatches = String(key || "").trim().toLowerCase() === normalized;
        if (keyMatches || providerNames(provider).includes(normalized)) providers.delete(key);
      }
      registry.providersChanged = true;
      registry.unregisteredProviders.add(normalized);
    },
    registerTool: (tool) => {
      if (tool && typeof tool.name === "string" && typeof tool.execute === "function") tools.set(tool.name, tool);
    },
    sendMessage: (message, options = {}) => pushCustomMessage(messages, message, options),
    sendUserMessage: (content, options = {}) => pushCustomMessage(messages, { customType: "user", content, display: true }, { ...options, user: true }),
    appendEntry: (customType, data) => {
      const entry = { type: "custom", customType: String(customType || ""), data: safeSerializable(data) };
      registry.sessionEntries.push(entry);
      pushCustomMessage(messages, { customType, content: "", display: false, details: data }, { entry: true });
    },
    setSessionName: (name) => {
      registry.sessionName = String(name || "");
      registry.sessionNameChanged = true;
    },
    getSessionName: () => {
      if (typeof registry.sessionName === "string" && registry.sessionName.trim()) return registry.sessionName;
      return undefined;
    },
    setLabel: (entryId, label) => {
      registry.sessionEntries.push({
        type: "label",
        targetId: String(entryId || ""),
        label: label === undefined ? null : String(label),
      });
    },
    exec: execCommand,
    getActiveTools: () => activeToolPayload(registry),
    getAllTools: () => allToolPayload(registry),
    setActiveTools: (names) => {
      registry.activeTools = Array.isArray(names) ? names.map(normalizeToolName).filter(Boolean) : [];
      registry.activeToolsChanged = true;
    },
    getCommands: () => commandPayload(registry),
    setModel: async (model) => {
      const spec = normalizeModelSpec(model);
      if (!spec || !(spec.provider || spec.model)) return false;
      registry.model = spec;
      registry.modelChanged = true;
      if (spec.thinking) {
        registry.thinkingLevel = spec.thinking;
        registry.thinkingLevelChanged = true;
      }
      return true;
    },
    getThinkingLevel: () => registry.thinkingLevel,
    setThinkingLevel: (level) => {
      registry.thinkingLevel = normalizeThinkingLevel(level);
      registry.thinkingLevelChanged = true;
    },
    getFlag: (name) => getRegisteredFlagValue(flags, flagValues, name),
    events: registry.eventBus,
  };
}

function createSourceInfo(extensionPath) {
  return { path: extensionPath, source: "extension", scope: "project", origin: "top-level" };
}

function createExtensionRecord(extensionPath) {
  return {
    path: extensionPath,
    sourceInfo: createSourceInfo(extensionPath),
    tools: new Map(),
    commands: new Map(),
    handlers: new Map(),
    flags: new Map(),
    shortcuts: new Map(),
    messageRenderers: new Map(),
    renderers: new Map(),
  };
}

function providerRegistration(provider, options = {}) {
  let entry = {};
  if (provider && typeof provider === "object") {
    entry = { ...provider };
  } else if (typeof provider === "string") {
    entry = { ...(options || {}), name: provider };
  }
  const name = String(entry.name || entry.id || entry.provider || "").trim();
  return name ? { name, config: { ...entry, name } } : null;
}

function makeExtensionLoaderApi(extension, runtime, cwd, eventBus) {
  const registerRenderer = (name, options) => {
    let entry = {};
    if (name && typeof name === "object") entry = { ...name };
    else if (typeof name === "string") entry = typeof options === "function" ? { render: options, name } : { ...(options || {}), name };
    const rendererName = String(entry.name || entry.id || entry.type || "").trim();
    if (rendererName) extension.renderers.set(rendererName, { ...entry, name: rendererName });
  };
  const registerMessageRenderer = (customType, renderer) => {
    if (typeof customType === "string" && customType.trim()) {
      const entry = typeof renderer === "function" ? { render: renderer } : { ...(renderer || {}) };
      extension.messageRenderers.set(customType, {
        ...entry,
        name: customType,
        customType,
        target: "custom_message",
      });
      return;
    }
    registerRenderer(customType, renderer);
  };
  return {
    on: (event, handler) => {
      runtime.assertActive();
      if (typeof event !== "string" || typeof handler !== "function") return;
      const list = extension.handlers.get(event) || [];
      list.push(handler);
      extension.handlers.set(event, list);
    },
    registerTool: (tool) => {
      runtime.assertActive();
      if (!tool || typeof tool.name !== "string") return;
      extension.tools.set(tool.name, { definition: tool, sourceInfo: extension.sourceInfo });
      runtime.refreshTools();
    },
    registerCommand: (name, options = {}) => {
      runtime.assertActive();
      if (typeof name !== "string" || !name.trim()) return;
      extension.commands.set(name, { name, sourceInfo: extension.sourceInfo, ...options });
    },
    registerShortcut: (shortcut, options = {}) => {
      runtime.assertActive();
      const spec = normalizeShortcutSpec(typeof shortcut === "string" ? shortcut : shortcut && (shortcut.key || shortcut.shortcut || shortcut.binding));
      if (spec) extension.shortcuts.set(spec, { shortcut: spec, spec, extensionPath: extension.path, ...(typeof options === "function" ? { handler: options } : options || {}) });
    },
    registerFlag: (name, options = {}) => {
      runtime.assertActive();
      const entry = name && typeof name === "object" ? { ...name } : { ...(options || {}), name };
      const flagName = normalizeFlagName(entry.name);
      if (flagName) extension.flags.set(flagName, { ...entry, name: flagName, extensionPath: extension.path });
    },
    registerMessageRenderer,
    registerRenderer,
    registerComponentRenderer: registerRenderer,
    registerOutputRenderer: registerRenderer,
    registerProvider: (provider, options) => {
      runtime.assertActive();
      const registration = providerRegistration(provider, options);
      if (registration) runtime.registerProvider(registration.name, registration.config, extension.path);
    },
    unregisterProvider: (name) => {
      runtime.assertActive();
      runtime.unregisterProvider(String(name || "").trim(), extension.path);
    },
    sendMessage: (...args) => runtime.sendMessage(...args),
    sendUserMessage: (...args) => runtime.sendUserMessage(...args),
    appendEntry: (...args) => runtime.appendEntry(...args),
    setSessionName: (...args) => runtime.setSessionName(...args),
    getSessionName: (...args) => runtime.getSessionName(...args),
    setLabel: (...args) => runtime.setLabel(...args),
    getActiveTools: (...args) => runtime.getActiveTools(...args),
    getAllTools: (...args) => runtime.getAllTools(...args),
    setActiveTools: (...args) => runtime.setActiveTools(...args),
    refreshTools: (...args) => runtime.refreshTools(...args),
    getCommands: (...args) => runtime.getCommands(...args),
    setModel: (...args) => runtime.setModel(...args),
    getThinkingLevel: (...args) => runtime.getThinkingLevel(...args),
    setThinkingLevel: (...args) => runtime.setThinkingLevel(...args),
    getFlag: (name) => runtime.flagValues.get(normalizeFlagName(name)),
    cwd,
    events: eventBus,
  };
}

async function loadExtensionFromFactory(factory, cwd = process.cwd(), eventBus = createEventBus(), runtime = createExtensionRuntime(), extensionPath = "<inline>") {
  if (typeof factory !== "function") throw new Error("loadExtensionFromFactory requires a function");
  const extension = createExtensionRecord(extensionPath);
  await factory(makeExtensionLoaderApi(extension, runtime, path.resolve(cwd || process.cwd()), eventBus));
  return extension;
}

async function loadExtensions(paths, cwd = process.cwd(), eventBus = createEventBus()) {
  const runtime = createExtensionRuntime();
  const extensions = [];
  const errors = [];
  for (const extPath of Array.isArray(paths) ? paths : []) {
    try {
      const resolved = path.resolve(cwd || process.cwd(), String(extPath || ""));
      const factory = await loadFactory(resolved);
      extensions.push(await loadExtensionFromFactory(factory, cwd, eventBus, runtime, resolved));
    } catch (error) {
      errors.push({ path: String(extPath || ""), error: error && error.message ? error.message : String(error) });
    }
  }
  return { extensions, errors, runtime };
}

function bridgeExtensionFile(name) {
  return [".ts", ".js", ".mjs", ".cjs"].some((suffix) => String(name || "").endsWith(suffix));
}

function discoverExtensionsInDir(dir) {
  try {
    return fs.readdirSync(dir)
      .filter((name) => bridgeExtensionFile(name))
      .sort()
      .map((name) => path.join(dir, name));
  } catch {
    return [];
  }
}

function resolveExtensionEntries(dir) {
  for (const name of ["index.ts", "index.js", "index.mjs", "index.cjs"]) {
    const candidate = path.join(dir, name);
    if (fs.existsSync(candidate)) return [candidate];
  }
  const packagePath = path.join(dir, "package.json");
  try {
    const manifest = JSON.parse(fs.readFileSync(packagePath, "utf8"));
    const entries = manifest && manifest.pi && Array.isArray(manifest.pi.extensions) ? manifest.pi.extensions : [];
    if (entries.length) return entries.map((entry) => path.resolve(dir, entry));
  } catch {}
  return null;
}

async function discoverAndLoadExtensions(configuredPaths = [], cwd = process.cwd(), agentDir = path.join(process.env.HOME || cwd, ".pi"), eventBus = createEventBus()) {
  const resolvedCwd = path.resolve(cwd || process.cwd());
  const resolvedAgentDir = path.resolve(agentDir || path.join(process.env.HOME || resolvedCwd, ".pi"));
  const allPaths = [];
  const seen = new Set();
  const addPath = (item) => {
    const resolved = path.resolve(resolvedCwd, String(item || ""));
    if (!seen.has(resolved)) {
      seen.add(resolved);
      allPaths.push(resolved);
    }
  };
  for (const item of discoverExtensionsInDir(path.join(resolvedCwd, ".pi", "extensions"))) addPath(item);
  for (const item of discoverExtensionsInDir(path.join(resolvedAgentDir, "extensions"))) addPath(item);
  for (const configured of Array.isArray(configuredPaths) ? configuredPaths : []) {
    const resolved = path.resolve(resolvedCwd, String(configured || ""));
    if (fs.existsSync(resolved) && fs.statSync(resolved).isDirectory()) {
      const entries = resolveExtensionEntries(resolved) || discoverExtensionsInDir(resolved);
      for (const entry of entries) addPath(entry);
    } else {
      addPath(resolved);
    }
  }
  return loadExtensions(allPaths, resolvedCwd, eventBus);
}

class ExtensionRunner {
  constructor(extensions = [], runtime = createExtensionRuntime(), cwd = process.cwd(), sessionManager = {}, modelRegistry = {}) {
    this.extensions = Array.isArray(extensions) ? extensions : [];
    this.runtime = runtime;
    this.cwd = path.resolve(cwd || process.cwd());
    this.sessionManager = sessionManager;
    this.modelRegistry = modelRegistry;
    this.uiContext = {};
    this.errorListeners = new Set();
    this.staleMessage = undefined;
  }

  bindCore(actions = {}, contextActions = {}, providerActions = {}) {
    for (const key of ["sendMessage", "sendUserMessage", "appendEntry", "setSessionName", "getSessionName", "setLabel", "getActiveTools", "getAllTools", "setActiveTools", "refreshTools", "getCommands", "setModel", "getThinkingLevel", "setThinkingLevel"]) {
      if (typeof actions[key] === "function") this.runtime[key] = actions[key];
    }
    this.contextActions = contextActions || {};
    for (const registration of this.runtime.pendingProviderRegistrations || []) {
      try {
        if (providerActions && typeof providerActions.registerProvider === "function") providerActions.registerProvider(registration.name, registration.config);
      } catch (error) {
        this.emitError({ extensionPath: registration.extensionPath, event: "register_provider", error: error && error.message ? error.message : String(error) });
      }
    }
    this.runtime.pendingProviderRegistrations = [];
    this.runtime.registerProvider = (name, config) => {
      if (providerActions && typeof providerActions.registerProvider === "function") providerActions.registerProvider(name, config);
    };
    this.runtime.unregisterProvider = (name) => {
      if (providerActions && typeof providerActions.unregisterProvider === "function") providerActions.unregisterProvider(name);
    };
  }

  bindCommandContext(actions = {}) { this.commandActions = actions || {}; }
  setUIContext(uiContext) { this.uiContext = uiContext || {}; }
  getUIContext() { return this.uiContext; }
  hasUI() { return Object.keys(this.uiContext || {}).length > 0; }
  getExtensionPaths() { return this.extensions.map((extension) => extension.path); }

  getAllRegisteredTools() {
    const byName = new Map();
    for (const extension of this.extensions) {
      for (const tool of extension.tools.values()) if (!byName.has(tool.definition.name)) byName.set(tool.definition.name, tool);
    }
    return [...byName.values()];
  }

  getToolDefinition(toolName) {
    for (const extension of this.extensions) {
      const tool = extension.tools.get(toolName);
      if (tool) return tool.definition;
    }
    return undefined;
  }

  getFlags() {
    const flags = new Map();
    for (const extension of this.extensions) for (const [name, flag] of extension.flags) if (!flags.has(name)) flags.set(name, flag);
    return flags;
  }

  setFlagValue(name, value) { this.runtime.flagValues.set(normalizeFlagName(name), value); }
  getFlagValues() { return new Map(this.runtime.flagValues); }
  getShortcuts() {
    const shortcuts = new Map();
    for (const extension of this.extensions) for (const [key, shortcut] of extension.shortcuts) shortcuts.set(key, shortcut);
    return shortcuts;
  }
  getShortcutDiagnostics() { return []; }

  invalidate(message) {
    this.staleMessage ||= message || "This extension ctx is stale after session replacement or reload.";
    this.runtime.invalidate(this.staleMessage);
  }
  assertActive() { if (this.staleMessage) throw new Error(this.staleMessage); }
  onError(listener) { this.errorListeners.add(listener); return () => this.errorListeners.delete(listener); }
  emitError(error) { for (const listener of this.errorListeners) listener(error); }
  hasHandlers(eventType) { return this.extensions.some((extension) => (extension.handlers.get(eventType) || []).length > 0); }
  getMessageRenderer(customType) {
    for (const extension of this.extensions) {
      const renderer = extension.messageRenderers.get(customType);
      if (renderer) return renderer;
    }
    return undefined;
  }

  getRegisteredCommands() {
    const commands = [];
    for (const extension of this.extensions) for (const command of extension.commands.values()) commands.push(command);
    const counts = new Map();
    for (const command of commands) counts.set(command.name, (counts.get(command.name) || 0) + 1);
    const seen = new Map();
    return commands.map((command) => {
      const occurrence = (seen.get(command.name) || 0) + 1;
      seen.set(command.name, occurrence);
      return { ...command, invocationName: counts.get(command.name) > 1 ? `${command.name}:${occurrence}` : command.name };
    });
  }
  getCommandDiagnostics() { return []; }
  getCommand(name) { return this.getRegisteredCommands().find((command) => command.invocationName === name); }
  shutdown() { if (this.contextActions && typeof this.contextActions.shutdown === "function") this.contextActions.shutdown(); }

  createContext() {
    const runner = this;
    return {
      get ui() { runner.assertActive(); return runner.uiContext; },
      get hasUI() { runner.assertActive(); return runner.hasUI(); },
      get cwd() { runner.assertActive(); return runner.cwd; },
      get sessionManager() { runner.assertActive(); return runner.sessionManager; },
      get modelRegistry() { runner.assertActive(); return runner.modelRegistry; },
      get model() { runner.assertActive(); return runner.contextActions && typeof runner.contextActions.getModel === "function" ? runner.contextActions.getModel() : undefined; },
      isIdle: () => !runner.contextActions || typeof runner.contextActions.isIdle !== "function" || runner.contextActions.isIdle(),
      get signal() { return runner.contextActions && typeof runner.contextActions.getSignal === "function" ? runner.contextActions.getSignal() : undefined; },
      abort: () => runner.contextActions && typeof runner.contextActions.abort === "function" && runner.contextActions.abort(),
      hasPendingMessages: () => !!(runner.contextActions && typeof runner.contextActions.hasPendingMessages === "function" && runner.contextActions.hasPendingMessages()),
      shutdown: () => runner.shutdown(),
      getContextUsage: () => runner.contextActions && typeof runner.contextActions.getContextUsage === "function" ? runner.contextActions.getContextUsage() : undefined,
      compact: (options) => runner.contextActions && typeof runner.contextActions.compact === "function" && runner.contextActions.compact(options),
      getSystemPrompt: () => runner.contextActions && typeof runner.contextActions.getSystemPrompt === "function" ? runner.contextActions.getSystemPrompt() : "",
    };
  }

  createCommandContext() {
    return { ...this.createContext(), waitForIdle: async () => {}, newSession: async () => ({ cancelled: false }), fork: async () => ({ cancelled: false }), navigateTree: async () => ({ cancelled: false }), switchSession: async () => ({ cancelled: false }), reload: async () => {} };
  }

  async emit(event) {
    this.assertActive();
    const handlers = [];
    for (const extension of this.extensions) handlers.push(...(extension.handlers.get(event && event.type) || []));
    let lastResult;
    for (const handler of handlers) {
      const result = await handler(event, this.createContext());
      if (result !== undefined) lastResult = result;
    }
    return lastResult;
  }
}

async function loadTools(extensionPath, flagValues = {}, context = {}) {
  const extensionPaths = (Array.isArray(extensionPath) ? extensionPath : [extensionPath])
    .map((item) => String(item || "").trim())
    .filter(Boolean);
  if (extensionPaths.length === 0) throw new Error("extension path is required");
  const registry = {
    tools: new Map(),
    commands: new Map(),
    handlers: new Map(),
    flags: new Map(),
    flagValues,
    shortcuts: new Map(),
    providers: new Map(),
    providersChanged: false,
    unregisteredProviders: new Set(),
    renderers: new Map(),
    messages: [],
    externalTools: Array.isArray(context.allTools) ? context.allTools : [],
    externalCommands: Array.isArray(context.commands) ? context.commands : [],
    activeTools: Array.isArray(context.activeTools) ? context.activeTools.map(normalizeToolName).filter(Boolean) : null,
    activeToolsChanged: false,
    model: normalizeModelSpec(context.model),
    modelChanged: false,
    contextModel: context.model && typeof context.model === "object" ? safeSerializable(context.model) : normalizeModelSpec(context.model),
    contextUsage: context.contextUsage || null,
    systemPrompt: typeof context.systemPrompt === "string" ? context.systemPrompt : "",
    hasUI: !!context.hasUI,
    isIdle: context.isIdle === undefined ? true : !!context.isIdle,
    hasPendingMessages: !!context.hasPendingMessages,
    abortRequested: false,
    shutdownRequested: false,
    reloadRequested: false,
    compactRequests: [],
    sessionActions: [],
    sessionName: typeof context.sessionName === "string" ? context.sessionName : undefined,
    sessionNameChanged: false,
    sessionEntries: [],
    themes: Array.isArray(context.themes) ? context.themes.map(normalizeThemeInfo).filter(Boolean) : [],
    themeName: typeof context.themeName === "string" ? context.themeName : undefined,
    themeChanged: false,
    session: context.session && typeof context.session === "object" ? context.session : null,
    models: Array.isArray(context.models) ? context.models.map((model) => safeSerializable(model)).filter(Boolean) : [],
    toolsExpanded: !!context.toolsExpanded,
    toolsExpandedChanged: false,
    thinkingLevel: normalizeThinkingLevel(context.thinkingLevel || context.thinking || "off"),
    thinkingLevelChanged: false,
    eventBus: createEventBus(),
  };
  for (const currentPath of extensionPaths) {
    const factory = await loadFactory(currentPath);
    if (typeof factory !== "function") throw new Error(`extension does not export a default factory function: ${currentPath}`);
    await factory(makeApi(registry, currentPath));
  }
  return registry;
}

function toolPayload(registry) {
  return [...registry.tools.values()].map((tool) => {
    const name = toolInfoName(tool);
    return {
      name,
      description: tool.description || tool.label || "",
      parameters: tool.parameters || { type: "object", properties: {} },
      sourceInfo: tool.sourceInfo || { path: `<extension:${name}>`, source: "extension", scope: "temporary", origin: "top-level" },
    };
  }).filter((tool) => tool.name);
}

function allToolPayload(registry) {
  const byName = new Map();
  for (const tool of registry.externalTools || []) {
    const name = toolInfoName(tool);
    if (name) byName.set(name, { ...tool, name });
  }
  for (const tool of registry.tools.values()) {
    const name = toolInfoName(tool);
    if (!name) continue;
    byName.set(name, {
      name,
      description: tool.description || tool.label || "",
      parameters: tool.parameters || { type: "object", properties: {} },
      sourceInfo: tool.sourceInfo || { path: `<extension:${name}>`, source: "extension", scope: "temporary", origin: "top-level" },
    });
  }
  return [...byName.values()].map((tool) => ({
    name: tool.name,
    description: tool.description || tool.label || "",
    parameters: tool.parameters || { type: "object", properties: {} },
    sourceInfo: tool.sourceInfo,
  }));
}

function activeToolPayload(registry) {
  if (Array.isArray(registry.activeTools)) return registry.activeTools;
  return allToolPayload(registry).map((tool) => tool.name);
}

function activeToolState(registry) {
  return {
    activeToolsChanged: !!registry.activeToolsChanged,
    activeTools: activeToolPayload(registry),
    providersChanged: !!registry.providersChanged,
    providers: registry.providersChanged ? providerPayload(registry) : undefined,
    unregisteredProviders: [...registry.unregisteredProviders],
    modelChanged: !!registry.modelChanged,
    model: registry.model || null,
    sessionNameChanged: !!registry.sessionNameChanged,
    sessionName: typeof registry.sessionName === "string" ? registry.sessionName : null,
    sessionEntries: Array.isArray(registry.sessionEntries) ? registry.sessionEntries : [],
    themeChanged: !!registry.themeChanged,
    themeName: typeof registry.themeName === "string" ? registry.themeName : null,
    toolsExpandedChanged: !!registry.toolsExpandedChanged,
    toolsExpanded: !!registry.toolsExpanded,
    abortRequested: !!registry.abortRequested,
    shutdownRequested: !!registry.shutdownRequested,
    reloadRequested: !!registry.reloadRequested,
    compactRequests: Array.isArray(registry.compactRequests) ? registry.compactRequests : [],
    sessionActions: Array.isArray(registry.sessionActions) ? registry.sessionActions : [],
    thinkingLevelChanged: !!registry.thinkingLevelChanged,
    thinkingLevel: registry.thinkingLevel,
  };
}

function normalizeCommandInfo(command) {
  if (!command || typeof command !== "object") return null;
  const name = String(command.name || command.command || command.slashCommand || "").replace(/^\//, "").trim();
  if (!name) return null;
  const source = String(command.source || "extension").trim() || "extension";
  const sourceInfo = command.sourceInfo && typeof command.sourceInfo === "object" ? safeSerializable(command.sourceInfo) : {};
  return {
    name,
    slashCommand: command.slashCommand || `/${name}`,
    description: command.description ? String(command.description) : "",
    source,
    sourceInfo: {
      path: command.path || sourceInfo.path || "",
      source: sourceInfo.source || source,
      scope: sourceInfo.scope || "temporary",
      origin: sourceInfo.origin || "top-level",
      ...sourceInfo,
    },
  };
}

function commandPayload(registry) {
  const extensionCommands = [...registry.commands.entries()].map(([name, command]) => ({
    name,
    slashCommand: `/${name}`,
    description: command.description || "",
    argumentHint: command.argumentHint || command.argument_hint || "",
    hasArgumentCompletions: typeof command.getArgumentCompletions === "function",
    source: command.source || "extension",
    sourceInfo: command.sourceInfo || { path: `<extension-command:${name}>`, source: "extension", scope: "temporary", origin: "top-level" },
  }));
  const externalCommands = Array.isArray(registry.externalCommands)
    ? registry.externalCommands.map(normalizeCommandInfo).filter(Boolean)
    : [];
  return [...extensionCommands, ...externalCommands];
}

function autocompleteItemsPayload(items) {
  if (!Array.isArray(items)) return [];
  return items.map((item) => {
    if (typeof item === "string") return { value: item, label: item };
    if (!item || typeof item !== "object") return null;
    const value = item.value ?? item.name ?? item.label;
    if (value === undefined || value === null) return null;
    const label = item.label ?? item.name ?? value;
    const out = { value: String(value), label: String(label) };
    if (item.description !== undefined && item.description !== null) out.description = String(item.description);
    return out;
  }).filter((item) => item && item.value);
}

function flagPayload(registry) {
  return [...registry.flags.entries()].map(([name, flag]) => {
    const payload = {
      name,
      description: flag.description || flag.label || "",
      type: flag.type || flag.kind || "",
    };
    const defaultValue = defaultFlagValue(flag);
    if (defaultValue !== undefined) payload.defaultValue = defaultValue;
    return payload;
  });
}

function shortcutPayload(registry) {
  return [...registry.shortcuts.entries()].map(([spec, shortcut]) => ({
    spec,
    description: shortcut.description || shortcut.label || "",
    command: typeof shortcut.command === "string" ? shortcut.command : typeof shortcut.action === "string" ? shortcut.action : undefined,
    hasHandler: typeof shortcut.handler === "function",
  }));
}

function arrayOfStrings(value) {
  if (Array.isArray(value)) return value.map(String).filter(Boolean);
  if (typeof value === "string" && value) return [value];
  return [];
}

function providerPayload(registry) {
  return [...registry.providers.values()].map((provider) => ({
    name: provider.name || provider.id || provider.provider || "",
    aliases: arrayOfStrings(provider.aliases || provider.names),
    protocol: provider.protocol || provider.wireProtocol || provider.api || provider.type || "openai",
    baseUrl: provider.baseUrl || provider.baseURL || provider.base_url || provider.url || "",
    envKeys: arrayOfStrings(provider.envKeys || provider.env_keys || provider.apiKeyEnvVars || provider.apiKeyEnvVar || provider.apiKeyEnv || provider.envKey),
    defaultModel: provider.defaultModel || provider.default_model || provider.model || "",
    headers: provider.headers || {},
    models: provider.models || [],
    hasRuntime: !!providerRuntimeHandler(provider),
    extensionPath: provider.extensionPath || "",
  }));
}

function providerRuntimeHandler(provider) {
  return provider && (
    (typeof provider.complete === "function" && provider.complete) ||
    (typeof provider.chat === "function" && provider.chat) ||
    (typeof provider.generate === "function" && provider.generate) ||
    (typeof provider.stream === "function" && provider.stream) ||
    (typeof provider.handler === "function" && provider.handler)
  );
}

function providerNames(provider) {
  return [
    provider.name,
    provider.id,
    provider.provider,
    ...(arrayOfStrings(provider.aliases || provider.names)),
  ].filter(Boolean).map((name) => String(name).trim().toLowerCase());
}

function findProvider(registry, requested) {
  const wanted = String(requested || "").trim().toLowerCase();
  for (const provider of registry.providers.values()) {
    if (providerNames(provider).includes(wanted)) return provider;
  }
  return undefined;
}

function resetProviderRuntimeState(registry) {
  registry.providersChanged = false;
  registry.unregisteredProviders.clear();
}

function rendererPayload(registry) {
  return [...registry.renderers.entries()].map(([name, renderer]) => ({
    name,
    description: renderer.description || renderer.label || "",
    target: renderer.target || renderer.kind || renderer.type || "all",
    customType: renderer.customType || undefined,
  }));
}

function uiMessage(value) {
  if (typeof value === "string") return value;
  if (value && typeof value.message === "string") return value.message;
  if (value && typeof value.text === "string") return value.text;
  return resultToText(value);
}

function responseValue(value) {
  if (value && typeof value === "object") {
    if (Object.prototype.hasOwnProperty.call(value, "value")) return value.value;
    if (Object.prototype.hasOwnProperty.call(value, "response")) return value.response;
    if (Object.prototype.hasOwnProperty.call(value, "result")) return value.result;
    if (Object.prototype.hasOwnProperty.call(value, "text")) return value.text;
    if (Object.prototype.hasOwnProperty.call(value, "confirmed")) return value.confirmed;
    if (Object.prototype.hasOwnProperty.call(value, "selected")) return value.selected;
  }
  return value;
}

function normalizeOptions(options) {
  const raw = Array.isArray(options) ? options : options && Array.isArray(options.options) ? options.options : [];
  return raw.map((option, index) => {
    if (option && typeof option === "object") {
      const label = option.label || option.name || option.title || option.text || String(option.value ?? option.id ?? index);
      const value = Object.prototype.hasOwnProperty.call(option, "value") ? option.value : (option.id ?? label);
      return { label: String(label), value };
    }
    return { label: String(option), value: option };
  });
}

function componentLines(value, width = 80) {
  if (value === undefined || value === null) return [];
  if (Array.isArray(value)) return value.flatMap((item) => componentLines(item, width));
  if (typeof value === "string") return value.split(/\r?\n/);
  if (typeof value.render === "function") {
    try {
      return componentLines(value.render(width), width);
    } catch (error) {
      return [`[component render error: ${error && error.message ? error.message : String(error)}]`];
    }
  }
  if (typeof value.text === "string") return value.text.split(/\r?\n/);
  if (typeof value.markdown === "string") return value.markdown.split(/\r?\n/);
  if (Array.isArray(value.children)) return value.children.flatMap((item) => componentLines(item, width));
  return [];
}

function safeSerializable(value, seen = new WeakSet()) {
  if (value === undefined || typeof value === "function") return undefined;
  if (value === null || typeof value !== "object") return value;
  if (Buffer.isBuffer(value) || value instanceof Uint8Array) return `[binary:${value.length}]`;
  if (seen.has(value)) return "[Circular]";
  seen.add(value);
  if (Array.isArray(value)) return value.map((item) => safeSerializable(item, seen)).filter((item) => item !== undefined);
  const out = {};
  for (const [key, item] of Object.entries(value)) {
    if (typeof item === "function") continue;
    const serialized = safeSerializable(item, seen);
    if (serialized !== undefined) out[key] = serialized;
  }
  return out;
}

function bridgeTheme(name = "ocaml-agent") {
  const passthrough = (...args) => String(args.length ? args[args.length - 1] ?? "" : "");
  return new Proxy({ fg: passthrough, bg: passthrough, accent: passthrough, muted: passthrough }, {
    get(target, prop) {
      if (prop in target) return target[prop];
      if (prop === "name") return name || "ocaml-agent";
      if (prop === "colors") return {};
      return passthrough;
    },
  });
}

function createOverlayHandle(pushSurface, overlayId, options = {}) {
  let hidden = false;
  let closed = false;
  let focused = !(options && options.nonCapturing);
  const emit = (method, extra = {}) => {
    if (typeof pushSurface === "function") {
      pushSurface("overlay_handle", { overlayId, method, ...extra });
    }
  };
  const hide = () => {
    if (closed) return;
    closed = true;
    focused = false;
    emit("hide");
  };
  const setHidden = (value) => {
    if (closed) return;
    hidden = !!value;
    if (hidden) focused = false;
    emit("setHidden", { hidden });
  };
  return {
    hide,
    close: hide,
    dispose: hide,
    show: () => setHidden(false),
    setHidden,
    isHidden: () => hidden,
    focus: () => {
      if (closed || hidden) return;
      focused = true;
      emit("focus");
    },
    unfocus: () => {
      if (closed) return;
      focused = false;
      emit("unfocus");
    },
    isFocused: () => focused && !hidden && !closed,
  };
}

function resolveOverlayOptions(options, rendered) {
  if (!options || !options.overlay) return undefined;
  let overlayOptions;
  if (Object.prototype.hasOwnProperty.call(options, "overlayOptions")) {
    overlayOptions = typeof options.overlayOptions === "function" ? options.overlayOptions() : options.overlayOptions;
  } else if (rendered && typeof rendered.width === "number") {
    overlayOptions = { width: rendered.width };
  }
  return safeSerializable(overlayOptions);
}

function bridgeTui(pushSurface) {
  let nextOverlayId = 0;
  const showOverlay = (component, options = {}) => {
    const overlayId = `overlay-${++nextOverlayId}`;
    const rendered = renderedComponentPayload(component);
    const serializableOptions = safeSerializable(options || {});
    if (typeof pushSurface === "function") {
      pushSurface("overlay", { overlayId, options: serializableOptions, ...rendered });
    }
    return createOverlayHandle(pushSurface, overlayId, serializableOptions || {});
  };
  return {
    width: 80,
    height: 24,
    requestRender: () => {
      if (typeof pushSurface === "function") pushSurface("render_request");
    },
    render: () => {},
    setTitle: (title) => {
      if (typeof pushSurface === "function") pushSurface("title", { title: String(title || "") });
    },
    setFocus: (component) => {
      if (typeof pushSurface === "function") {
        pushSurface("focus", component ? renderedComponentPayload(component) : { action: "clear", lines: [] });
      }
    },
    showOverlay,
    hideOverlay: () => {
      if (typeof pushSurface === "function") pushSurface("overlay_handle", { method: "hide_top" });
    },
    addOverlay: (component, options = {}) => showOverlay(component, options),
    addInputListener: () => {
      if (typeof pushSurface === "function") pushSurface("terminal_input_listener");
      return () => {
        if (typeof pushSurface === "function") pushSurface("terminal_input_unsubscribe");
      };
    },
  };
}

function bridgeKeybindings() {
  return {
    register: () => {},
    unregister: () => {},
    add: () => {},
    remove: () => {},
    get: () => undefined,
    list: () => [],
  };
}

function bridgeFooterData(statuses) {
  return {
    getCwd: () => process.cwd(),
    getBranch: () => undefined,
    getStatuses: () => statuses || {},
    getStatus: (key) => (statuses || {})[key],
  };
}

function createSessionManager(registry) {
  const session = registry && registry.session && typeof registry.session === "object" ? registry.session : {};
  const entries = Array.isArray(session.entries) ? session.entries : [];
  const entryById = () => new Map(entries.filter((entry) => typeof entry.id === "string").map((entry) => [entry.id, entry]));
  const leafIdAfterEntry = (current, entry) => {
    if (!entry || typeof entry !== "object") return current;
    if (entry.type === "leaf") {
      return typeof entry.targetId === "string" && entry.targetId ? entry.targetId : null;
    }
    if (
      (entry.type === "message" ||
        entry.type === "custom_message" ||
        entry.type === "branch_summary" ||
        entry.type === "compaction") &&
      typeof entry.id === "string" &&
      entry.id
    ) {
      return entry.id;
    }
    return current;
  };
  const labelById = () => {
    const labels = new Map();
    for (const entry of entries) {
      if (entry && entry.type === "label" && typeof entry.targetId === "string") {
        if (entry.label === undefined || entry.label === null || entry.label === "") labels.delete(entry.targetId);
        else labels.set(entry.targetId, String(entry.label));
      }
    }
    return labels;
  };
  const leafId = () => {
    if (Object.prototype.hasOwnProperty.call(session, "leafId")) {
      return typeof session.leafId === "string" && session.leafId ? session.leafId : null;
    }
    let current = null;
    for (const entry of entries) current = leafIdAfterEntry(current, entry);
    return current;
  };
  const sessionFile = () => typeof session.path === "string" ? session.path : undefined;
  const sessionDir = () => {
    if (typeof session.sessionDir === "string") return session.sessionDir;
    const file = sessionFile();
    return file ? path.dirname(file) : process.cwd();
  };
  const header = () => {
    if (!session.id && !session.path && !session.name) return null;
    return {
      type: "session",
      id: session.id || session.path || "session",
      sessionId: session.id || undefined,
      name: session.name || undefined,
      cwd: session.cwd || process.cwd(),
      sessionFile: sessionFile(),
    };
  };
  const getBranch = (fromId) => {
    const byId = entryById();
    const startId = typeof fromId === "string" && fromId ? fromId : leafId();
    const out = [];
    const seen = new Set();
    let current = startId ? byId.get(startId) : undefined;
    while (current && typeof current.id === "string" && !seen.has(current.id)) {
      seen.add(current.id);
      out.unshift(current);
      current = typeof current.parentId === "string" ? byId.get(current.parentId) : undefined;
    }
    return out;
  };
  const getChildren = (parentId) => {
    const id = String(parentId || "");
    return entries.filter((entry) => entry && entry.parentId === id);
  };
  const getTree = () => {
    const labels = labelById();
    const nodes = new Map();
    const roots = [];
    for (const entry of entries) {
      if (typeof entry.id !== "string") continue;
      nodes.set(entry.id, { entry, children: [], label: labels.get(entry.id) });
    }
    for (const entry of entries) {
      if (typeof entry.id !== "string") continue;
      const node = nodes.get(entry.id);
      const parent = typeof entry.parentId === "string" ? nodes.get(entry.parentId) : null;
      if (parent && parent !== node) parent.children.push(node);
      else roots.push(node);
    }
    return roots;
  };
  return {
    getCwd: () => session.cwd || process.cwd(),
    getSessionDir: sessionDir,
    getSessionId: () => session.id || undefined,
    getSessionFile: sessionFile,
    getLeafId: leafId,
    getLeafEntry: () => {
      const id = leafId();
      return id ? entryById().get(id) : undefined;
    },
    getEntry: (id) => entryById().get(String(id || "")),
    getLabel: (id) => labelById().get(String(id || "")),
    getBranch,
    getChildren,
    getHeader: header,
    getEntries: () => entries.slice(),
    getTree,
    getSessionName: () => session.name || undefined,
  };
}

function createModelRegistry(registry) {
  const seen = new Set();
  const addModel = (models, model) => {
    if (!model || typeof model !== "object") return;
    const provider = String(model.provider || model.api || "");
    const id = String(model.id || model.name || "");
    if (!provider || !id) return;
    const key = `${provider}:${id}`;
    if (seen.has(key)) return;
    seen.add(key);
    models.push({ ...safeSerializable(model), provider, id, name: model.name || id });
  };
  const models = [];
  if (registry && registry.contextModel) addModel(models, registry.contextModel);
  if (registry && Array.isArray(registry.models)) {
    for (const model of registry.models) addModel(models, model);
  }
  const providerDisplay = (provider) => {
    const found = models.find((model) => model.provider === provider && model.providerName);
    return found ? String(found.providerName) : String(provider || "");
  };
  return {
    refresh: () => {},
    getError: () => undefined,
    getAll: () => models.slice(),
    getAvailable: () => models.slice(),
    find: (provider, modelId) => models.find((model) => model.provider === provider && model.id === modelId),
    hasConfiguredAuth: (model) => !!model,
    getApiKeyAndHeaders: async (_model) => ({ ok: true }),
    getProviderAuthStatus: (provider) => ({ configured: true, source: "runtime", label: String(provider || "") }),
    getProviderDisplayName: providerDisplay,
  };
}

function sessionActionRecorder(registry) {
  const entries = [];
  const session = registry && registry.session && typeof registry.session === "object"
    ? { ...registry.session, entries: Array.isArray(registry.session.entries) ? registry.session.entries.slice() : [] }
    : { entries: [] };
  const nextId = (prefix) => `${prefix}-${entries.length + 1}`;
  const updateLeafId = (entry) => {
    if (entry.type === "leaf") {
      session.leafId = typeof entry.targetId === "string" && entry.targetId ? entry.targetId : null;
    } else if (entry && typeof entry.id === "string" && entry.id) {
      session.leafId = entry.id;
    }
  };
  const appendEntry = (entry) => {
    entries.push(entry);
    session.entries.push(entry);
    updateLeafId(entry);
    return entry.id;
  };
  const manager = createSessionManager({ ...registry, session });
  const assertEntry = (id) => {
    if (!id || !manager.getEntry(id)) throw new Error(`Entry ${id || ""} not found`);
    return id;
  };
  const appendLeafMove = (targetId) => {
    appendEntry({
      type: "leaf",
      id: nextId("callback-leaf"),
      parentId: manager.getLeafId(),
      timestamp: new Date().toISOString(),
      targetId: targetId === null ? null : String(targetId),
    });
  };
  const branchWithSummary = (branchFromId, summary, details, fromHook) => {
    const targetId = branchFromId === null || branchFromId === undefined ? null : assertEntry(String(branchFromId));
    session.leafId = targetId;
    return appendEntry({
      type: "branch_summary",
      id: nextId("callback-branch-summary"),
      parentId: targetId,
      timestamp: new Date().toISOString(),
      fromId: targetId || "root",
      summary: String(summary || ""),
      details: safeSerializable(details),
      fromHook: fromHook === undefined ? undefined : !!fromHook,
    });
  };
  const appendSessionInfo = (name) => {
    session.name = String(name || "").trim();
    return appendEntry({
      type: "session_info",
      id: nextId("callback-session-info"),
      parentId: manager.getLeafId(),
      timestamp: new Date().toISOString(),
      name: session.name,
    });
  };
  const appendLabelChange = (targetId, label) => {
    const target = assertEntry(String(targetId || ""));
    return appendEntry({
      type: "label",
      id: nextId("callback-label"),
      parentId: manager.getLeafId(),
      timestamp: new Date().toISOString(),
      targetId: target,
      label: label === undefined ? null : String(label),
    });
  };
  return {
    entries,
    appendEntry,
    manager: {
      ...manager,
      branch: (branchFromId) => {
        appendLeafMove(assertEntry(String(branchFromId || "")));
      },
      resetLeaf: () => {
        appendLeafMove(null);
      },
      branchWithSummary,
      moveTo: (entryId, summary) => {
        const targetId = entryId === null || entryId === undefined ? null : assertEntry(String(entryId));
        if (summary && typeof summary === "object") {
          return branchWithSummary(targetId, summary.summary, summary.details, summary.fromHook);
        }
        appendLeafMove(targetId);
        return undefined;
      },
      appendCustomEntry: (customType, data) =>
        appendEntry({
          type: "custom",
          id: nextId("callback-entry"),
          parentId: manager.getLeafId(),
          timestamp: new Date().toISOString(),
          customType: String(customType || ""),
          data: safeSerializable(data),
        }),
      appendCustomMessageEntry: (customType, content, display = true, details) =>
        appendEntry({
          type: "custom_message",
          id: nextId("callback-custom-message"),
          parentId: manager.getLeafId(),
          timestamp: new Date().toISOString(),
          customType: String(customType || "extension"),
          content: safeSerializable(content ?? ""),
          display: display === undefined ? true : !!display,
          details: safeSerializable(details),
        }),
      appendMessage: (message) =>
        appendEntry({
          type: "message",
          id: nextId("callback-message"),
          parentId: manager.getLeafId(),
          timestamp: new Date().toISOString(),
          message: safeSerializable(message || {}),
        }),
      appendThinkingLevelChange: (thinkingLevel) =>
        appendEntry({
          type: "thinking_level_change",
          id: nextId("callback-thinking"),
          parentId: manager.getLeafId(),
          timestamp: new Date().toISOString(),
          thinkingLevel: String(thinkingLevel || "off"),
        }),
      appendModelChange: (provider, modelId) =>
        appendEntry({
          type: "model_change",
          id: nextId("callback-model"),
          parentId: manager.getLeafId(),
          timestamp: new Date().toISOString(),
          provider: String(provider || ""),
          modelId: String(modelId || ""),
        }),
      appendCompaction: (summary, firstKeptEntryId, tokensBefore = 0, details, fromHook) =>
        appendEntry({
          type: "compaction",
          id: nextId("callback-compaction"),
          parentId: manager.getLeafId(),
          timestamp: new Date().toISOString(),
          summary: String(summary || ""),
          firstKeptEntryId: String(firstKeptEntryId || ""),
          tokensBefore: Number.isFinite(Number(tokensBefore)) ? Number(tokensBefore) : 0,
          details: safeSerializable(details),
          fromHook: fromHook === undefined ? undefined : !!fromHook,
        }),
      appendSessionInfo,
      appendSessionName: appendSessionInfo,
      appendLabelChange,
      appendLabel: appendLabelChange,
      getSessionName: () => session.name || manager.getSessionName(),
    },
  };
}

function replacementSessionRegistry(registry, action) {
  if (!registry || !action || (action.kind !== "new_session" && action.kind !== "fork")) return registry;
  const current = registry.session && typeof registry.session === "object" ? registry.session : {};
  if (action.kind === "fork") {
    const manager = createSessionManager(registry);
    const entryId = String(action.entryId || "");
    const selected = manager.getEntry(entryId);
    if (!selected) return registry;
    let leafId = entryId;
    if (action.position === "before") {
      if (selected.type !== "message" || !selected.message || selected.message.role !== "user") return registry;
      leafId = typeof selected.parentId === "string" && selected.parentId ? selected.parentId : null;
    }
    const entries = leafId ? manager.getBranch(leafId).map((entry) => safeSerializable(entry)) : [];
    return {
      ...registry,
      session: {
        ...current,
        entries,
        leafId,
        parentSession: current.path,
      },
    };
  }
  return {
    ...registry,
    session: {
      cwd: current.cwd || process.cwd(),
      sessionDir: current.sessionDir,
      parentSession: action.parentSession,
      entries: [],
      leafId: null,
    },
  };
}

async function runSessionActionCallbacks(action, options, registry) {
  if (!options || (typeof options.setup !== "function" && typeof options.withSession !== "function")) return action;
  const recorder = sessionActionRecorder(replacementSessionRegistry(registry, action));
  if (typeof options.setup === "function") {
    await options.setup(recorder.manager);
  }
  if (typeof options.withSession === "function") {
    const ctx = eventContext({}, null, registry);
    ctx.sessionManager = recorder.manager;
    await options.withSession(ctx);
    const messages = ctx.__uiState && Array.isArray(ctx.__uiState.messages) ? ctx.__uiState.messages : [];
    for (const message of messages) {
      recorder.appendEntry({
        ...message,
        id: message.id || `callback-message-${recorder.entries.length + 1}`,
        parentId: recorder.manager.getLeafId(),
        timestamp: new Date().toISOString(),
      });
    }
  }
  if (recorder.entries.length) action.sessionEntries = recorder.entries;
  const sessionName = recorder.manager.getSessionName();
  if (sessionName) action.sessionName = sessionName;
  return action;
}

function renderedComponentPayload(component) {
  const lines = componentLines(component);
  const payload = {
    componentClass: component && component.constructor && component.constructor.name ? component.constructor.name : undefined,
    lines,
  };
  if (component && typeof component.width === "number") payload.width = component.width;
  if (component && typeof component.height === "number") payload.height = component.height;
  const serializable = safeSerializable(component);
  if (serializable && typeof serializable === "object" && Object.keys(serializable).length > 0) {
    payload.component = serializable;
  }
  return payload;
}

function normalizeCustomMessage(message = {}, options = {}, id = "message-1") {
  const source = message && typeof message === "object" ? message : { content: message };
  return {
    id,
    type: "custom_message",
    customType: String(source.customType || source.type || "extension"),
    content: safeSerializable(source.content ?? ""),
    display: source.display === undefined ? true : !!source.display,
    details: safeSerializable(source.details),
    options: safeSerializable(options || {}),
  };
}

function pushCustomMessage(messages, message, options) {
  const entry = normalizeCustomMessage(message, options, `message-${messages.length + 1}`);
  messages.push(entry);
  return Promise.resolve();
}

function renderCustomMessage(message, registry) {
  if (!registry || !registry.renderers || !message || message.type !== "custom_message") return message;
  const customType = String(message.customType || "");
  const renderer = registry.renderers.get(customType);
  if (!renderer || renderer.target !== "custom_message") return message;
  const handler = renderer.renderMessage || renderer.render || renderer.handler;
  if (typeof handler !== "function") return message;
  try {
    const result = handler(message, { expanded: false }, bridgeTheme());
    const lines = componentLines(result);
    const text = resultToText(result);
    const component = renderComponentPayload(result);
    return {
      ...message,
      renderer: renderer.name || customType,
      rendered: text || lines.join("\n"),
      lines,
      components: component === undefined ? [] : (Array.isArray(component) ? component : [component]),
    };
  } catch (error) {
    return {
      ...message,
      renderError: error && error.message ? error.message : String(error),
    };
  }
}

function runComponentFactory(factory, args) {
  if (factory === undefined) return { action: "clear", component: false, lines: [] };
  if (typeof factory !== "function") {
    return { action: "set", component: false, lines: componentLines(factory) };
  }
  try {
    const component = factory(...args);
    if (component && typeof component.then === "function") {
      return { action: "set", component: true, async: true, lines: [] };
    }
    return { action: "set", component: true, ...renderedComponentPayload(component) };
  } catch (error) {
    return { action: "set", component: true, error: error && error.message ? error.message : String(error), lines: [] };
  }
}

async function runComponentFactoryAsync(factory, args) {
  if (factory === undefined) return { action: "clear", component: false, lines: [] };
  if (typeof factory !== "function") {
    return { action: "set", component: false, lines: componentLines(factory) };
  }
  try {
    const component = await factory(...args);
    return { action: "set", component: true, ...renderedComponentPayload(component) };
  } catch (error) {
    return { action: "set", component: true, error: error && error.message ? error.message : String(error), lines: [] };
  }
}

function createUiState(responses = {}, responder = null, registry = null) {
  const requests = [];
  const notifications = [];
  const surfaces = [];
  const messages = [];
  const statuses = {};
  const responseIndexes = {};
  let nextId = 0;
  let nextCustomOverlayId = 0;
  let editorText = typeof responses.editorText === "string" ? responses.editorText : "";
  let editorComponentFactory = undefined;
  const nextResponse = (kind) => {
    const read = (value) => {
      if (value === undefined) return undefined;
      if (Array.isArray(value)) {
        const index = responseIndexes[kind] || 0;
        responseIndexes[kind] = index + 1;
        return value[index];
      }
      return value;
    };
    if (Array.isArray(responses)) {
      const item = responses.find((entry) => entry && (entry.kind === kind || entry.type === kind));
      return responseValue(item);
    }
    if (!responses || typeof responses !== "object") return undefined;
    const direct = read(responses[kind]);
    if (direct !== undefined) return responseValue(direct);
    if (responses.responses) {
      const nested = Array.isArray(responses.responses)
        ? responses.responses.find((entry) => entry && (entry.kind === kind || entry.type === kind))
        : responses.responses[kind];
      if (nested !== undefined) return responseValue(nested);
    }
    return undefined;
  };
  const pushRequest = (kind, message, extra = {}) => {
    const request = { id: `ui-${++nextId}`, kind, message: uiMessage(message), ...extra };
    requests.push(request);
    return request;
  };
  const pushSurface = (kind, extra = {}) => {
    const surface = { id: `surface-${++nextId}`, kind, ...extra };
    surfaces.push(surface);
    return surface;
  };
  const tui = bridgeTui(pushSurface);
  const theme = bridgeTheme(registry && registry.themeName ? registry.themeName : undefined);
  const keybindings = bridgeKeybindings();
  const footerData = bridgeFooterData(statuses);
  const stringLines = (content) => {
    if (content === undefined || content === null) return undefined;
    if (Array.isArray(content)) return content.map((line) => String(line));
    if (typeof content === "string") return content.split(/\r?\n/);
    return undefined;
  };
  const selectDefault = (options, settings) => {
    if (!options.length) return undefined;
    if (settings && Object.prototype.hasOwnProperty.call(settings, "defaultValue")) {
      const match = options.find((option) => option.value === settings.defaultValue || option.label === settings.defaultValue);
      if (match) return match.value;
      return settings.defaultValue;
    }
    const index =
      settings && typeof settings.defaultIndex === "number"
        ? settings.defaultIndex
        : settings && typeof settings.index === "number"
          ? settings.index
          : 0;
    return options[Math.max(0, Math.min(index, options.length - 1))].value;
  };
  const coerceSelection = (value, options) => {
    if (typeof value === "number" && options[value]) return options[value].value;
    if (value && typeof value === "object" && Object.prototype.hasOwnProperty.call(value, "value")) return value.value;
    return value;
  };
  return {
    requests,
    notifications,
    surfaces,
    messages,
    pushSurface,
    sendMessage: (message, options = {}) => pushCustomMessage(messages, message, options),
    sendUserMessage: (content, options = {}) => pushCustomMessage(messages, { customType: "user", content, display: true }, { ...options, user: true }),
    ui: {
      notify: (message, type = "info") => {
        const text = uiMessage(message);
        notifications.push(text);
        const request = pushRequest("notify", text, { notifyType: type });
        if (responder) responder(request);
      },
      confirm: async (title, messageOrOptions = {}, maybeOptions = {}) => {
        const hasMessage = typeof messageOrOptions === "string";
        const message = hasMessage ? messageOrOptions : title;
        const options = hasMessage ? maybeOptions : messageOrOptions;
        const request = pushRequest("confirm", message, { title: String(title || ""), options });
        const supplied = responder ? await responder(request) : nextResponse("confirm");
        if (supplied !== undefined) return !!supplied;
        if (Object.prototype.hasOwnProperty.call(options || {}, "defaultValue")) return !!options.defaultValue;
        if (Object.prototype.hasOwnProperty.call(options || {}, "default")) return !!options.default;
        return false;
      },
      input: async (title, placeholderOrOptions = {}, maybeOptions = {}) => {
        const hasPlaceholder = typeof placeholderOrOptions === "string";
        const placeholder = hasPlaceholder ? placeholderOrOptions : "";
        const options = hasPlaceholder ? maybeOptions : placeholderOrOptions;
        const request = pushRequest("input", title, { title: String(title || ""), placeholder, options });
        const supplied = responder ? await responder(request) : nextResponse("input");
        if (supplied !== undefined) return supplied;
        if (Object.prototype.hasOwnProperty.call(options || {}, "defaultValue")) return options.defaultValue;
        if (Object.prototype.hasOwnProperty.call(options || {}, "default")) return options.default;
        if (Object.prototype.hasOwnProperty.call(options || {}, "value")) return options.value;
        return placeholder || undefined;
      },
      select: async (message, options = [], settings = {}) => {
        const normalizedOptions = normalizeOptions(options);
        const effectiveSettings = Array.isArray(options) ? settings : options || {};
        const request = pushRequest("select", message, { options: normalizedOptions, settings: effectiveSettings });
        const supplied = responder ? await responder(request) : nextResponse("select");
        if (supplied !== undefined) return coerceSelection(supplied, normalizedOptions);
        return selectDefault(normalizedOptions, effectiveSettings);
      },
      onTerminalInput: () => {
        pushSurface("terminal_input_listener");
        return () => pushSurface("terminal_input_unsubscribe");
      },
      setStatus: (key, text) => {
        const statusKey = String(key || "");
        if (text === undefined) delete statuses[statusKey];
        else statuses[statusKey] = String(text);
        pushSurface("status", { key: statusKey, text: text === undefined ? null : String(text) });
      },
      setWorkingMessage: (message) => pushSurface("working_message", { message: message === undefined ? null : String(message) }),
      setWorkingVisible: (visible) => pushSurface("working_visible", { visible: !!visible }),
      setWorkingIndicator: (options) => pushSurface("working_indicator", { options: options || null }),
      setHiddenThinkingLabel: (label) => pushSurface("hidden_thinking_label", { label: label === undefined ? null : String(label) }),
      setWidget: (key, content, options = {}) => {
        const lines = stringLines(content);
        const rendered = typeof content === "function" ? runComponentFactory(content, [tui, theme]) : {};
        pushSurface("widget", {
          key: String(key || ""),
          action: content === undefined ? "clear" : "set",
          lines: lines || rendered.lines || [],
          component: typeof content === "function",
          ...rendered,
          options,
        });
      },
      setFooter: (factory) => pushSurface("footer", { ...runComponentFactory(factory, [tui, theme, footerData]) }),
      setHeader: (factory) => pushSurface("header", { ...runComponentFactory(factory, [tui, theme]) }),
      setTitle: (title) => pushSurface("title", { title: String(title || "") }),
      custom: async (factory, options = {}) => {
        let doneCalled = false;
        let doneValue;
        const done = (value) => {
          doneCalled = true;
          doneValue = value;
        };
        const rendered = await runComponentFactoryAsync(factory, [tui, theme, keybindings, done]);
        const isOverlay = !!(options && options.overlay);
        const overlayOptions = resolveOverlayOptions(options, rendered);
        const overlayId = isOverlay ? `custom-overlay-${++nextCustomOverlayId}` : undefined;
        const safeOptions = safeSerializable(options || {});
        const payload = { options: safeOptions, overlay: isOverlay, overlayOptions, overlayId, ...rendered };
        if (isOverlay) {
          pushSurface("overlay", { overlayId, options: overlayOptions, ...rendered });
          if (options && typeof options.onHandle === "function") {
            try {
              options.onHandle(createOverlayHandle(pushSurface, overlayId, overlayOptions || {}));
            } catch (error) {
              pushSurface("overlay_handle", {
                overlayId,
                method: "onHandle_error",
                error: error && error.message ? error.message : String(error),
              });
            }
          }
        }
        const request = pushRequest("custom", "custom component", payload);
        pushSurface("custom", payload);
        const supplied = responder ? await responder(request) : nextResponse("custom");
        if (supplied !== undefined) return supplied;
        if (doneCalled) return doneValue;
        return undefined;
      },
      pasteToEditor: (text) => {
        const value = String(text || "");
        editorText += value;
        pushSurface("paste", { text: value });
      },
      setEditorText: (text) => {
        editorText = String(text || "");
        pushSurface("editor_text", { text: editorText });
      },
      getEditorText: () => editorText,
      editor: async (title, prefill = "") => {
        const request = pushRequest("editor", title, { title: String(title || ""), prefill: String(prefill || "") });
        const supplied = responder ? await responder(request) : nextResponse("editor");
        if (supplied !== undefined) return supplied;
        return String(prefill || "");
      },
      addAutocompleteProvider: () => pushSurface("autocomplete_provider"),
      setEditorComponent: (factory) => {
        editorComponentFactory = factory;
        pushSurface("editor_component", { ...runComponentFactory(factory, [tui, theme, keybindings]) });
      },
      getEditorComponent: () => editorComponentFactory,
      get theme() { return theme; },
      getAllThemes: () => registry && Array.isArray(registry.themes) ? registry.themes : [],
      getTheme: (name) => {
        const requested = String(name || "").trim();
        if (!registry || !Array.isArray(registry.themes) || !requested) return undefined;
        return registry.themes.find((candidate) => candidate && candidate.name === requested);
      },
      setTheme: (themeValue) => {
        const requested = themeName(themeValue);
        if (!requested) return { success: false, error: "theme name is required" };
        const known = !registry || !Array.isArray(registry.themes) || registry.themes.length === 0
          ? true
          : registry.themes.some((candidate) => candidate && candidate.name === requested);
        if (!known) return { success: false, error: `theme not found: ${requested}` };
        if (registry) {
          registry.themeName = requested;
          registry.themeChanged = true;
        }
        pushSurface("theme", { name: requested });
        return { success: true };
      },
      getToolsExpanded: () => registry ? !!registry.toolsExpanded : false,
      setToolsExpanded: (expanded) => {
        if (registry) {
          registry.toolsExpanded = !!expanded;
          registry.toolsExpandedChanged = true;
        }
        pushSurface("tools_expanded", { expanded: !!expanded });
      },
    },
  };
}

function eventContext(responses = {}, responder = null, registry = null) {
  const uiState = createUiState(responses, responder, registry);
  const pushSessionAction = (kind, payload = {}) => {
    if (!registry) return;
    registry.sessionActions.push({ kind, ...safeSerializable(payload) });
  };
  return {
    cwd: process.cwd(),
    ui: uiState.ui,
    hasUI: registry ? !!registry.hasUI : false,
    sessionManager: createSessionManager(registry),
    modelRegistry: createModelRegistry(registry),
    model: registry ? (registry.contextModel || null) : null,
    isIdle: () => registry ? !!registry.isIdle : true,
    signal: undefined,
    abort: () => {
      if (registry) registry.abortRequested = true;
    },
    hasPendingMessages: () => registry ? !!registry.hasPendingMessages : false,
    shutdown: () => {
      if (registry) registry.shutdownRequested = true;
    },
    getContextUsage: () => registry ? registry.contextUsage || undefined : undefined,
    compact: (options = {}) => {
      if (registry) registry.compactRequests.push(safeSerializable(options) || {});
    },
    reload: async () => {
      if (registry) registry.reloadRequested = true;
    },
    waitForIdle: async () => {},
    newSession: async (options = {}) => {
      const action = await runSessionActionCallbacks({
        kind: "new_session",
        parentSession: options && typeof options.parentSession === "string" ? options.parentSession : undefined,
        hasSetup: !!(options && typeof options.setup === "function"),
        hasWithSession: !!(options && typeof options.withSession === "function"),
      }, options, registry);
      pushSessionAction(action.kind, action);
      return { cancelled: false };
    },
    fork: async (entryId, options = {}) => {
      const action = await runSessionActionCallbacks({
        kind: "fork",
        entryId: String(entryId || ""),
        position: options && typeof options.position === "string" ? options.position : undefined,
        hasWithSession: !!(options && typeof options.withSession === "function"),
      }, options, registry);
      pushSessionAction(action.kind, action);
      return { cancelled: false };
    },
    navigateTree: async (targetId, options = {}) => {
      pushSessionAction("navigate_tree", { targetId: String(targetId || ""), options: safeSerializable(options) || {} });
      return { cancelled: false };
    },
    switchSession: async (sessionPath, options = {}) => {
      const action = await runSessionActionCallbacks({
        kind: "switch_session",
        sessionPath: String(sessionPath || ""),
        hasWithSession: !!(options && typeof options.withSession === "function"),
      }, options, registry);
      pushSessionAction(action.kind, action);
      return { cancelled: false };
    },
    getSystemPrompt: () => registry ? registry.systemPrompt || "" : "",
    sendMessage: uiState.sendMessage,
    sendUserMessage: uiState.sendUserMessage,
    __uiState: uiState,
  };
}

function uiPayload(ctx, registry = null) {
  const state = ctx && ctx.__uiState ? ctx.__uiState : ctx;
  const registryMessages = registry && registry.messages ? registry.messages : [];
  const stateMessages = state && state.messages ? state.messages : [];
  const messages = [...registryMessages, ...stateMessages].map((message) => renderCustomMessage(message, registry));
  return {
    notifications: state && state.notifications ? state.notifications : [],
    requests: state && state.requests ? state.requests : [],
    surfaces: state && state.surfaces ? state.surfaces : [],
    messages,
  };
}

function textWithUi(text, ctx) {
  const payload = uiPayload(ctx);
  const parts = [...payload.notifications];
  if (text) parts.push(text);
  return parts.join("\n");
}

async function replaySessionStart(registry) {
  const list = registry.handlers.get("session_start") || [];
  if (!list.length) return;
  const event = { type: "session_start", reason: "startup" };
  const ctx = eventContext();
  for (const handler of list) await handler(event, ctx);
}

function resultToText(result) {
  if (typeof result === "string") return result;
  if (result == null) return "";
  if (typeof result.text === "string") return result.text;
  if (typeof result.markdown === "string") return result.markdown;
  if (typeof result.value === "string") return result.value;
  const content = result.content;
  if (typeof content === "string") return content;
  if (Array.isArray(content)) {
    return content.map((item) => {
      if (typeof item === "string") return item;
      if (item && item.type === "text" && typeof item.text === "string") return item.text;
      if (item && item.type === "markdown" && typeof item.markdown === "string") return item.markdown;
      if (item && typeof item.text === "string") return item.text;
      if (item && typeof item.markdown === "string") return item.markdown;
      if (item && typeof item.content === "string") return item.content;
      if (item && typeof item.body === "string") return item.body;
      if (item && Array.isArray(item.children)) return resultToText({ content: item.children });
      return JSON.stringify(item);
    }).join("\n");
  }
  if (Array.isArray(result.children)) return resultToText({ content: result.children });
  if (typeof result.body === "string") return result.body;
  return JSON.stringify(result);
}

function createToolSignal(operations) {
  let signal;
  if (typeof AbortController !== "undefined") {
    signal = new AbortController().signal;
  } else {
    signal = {
      aborted: false,
      reason: undefined,
      addEventListener: () => {},
      removeEventListener: () => {},
      dispatchEvent: () => false,
    };
  }
  if (operations && typeof operations === "object") {
    for (const [key, value] of Object.entries(operations)) {
      if (!(key in signal)) {
        try {
          Object.defineProperty(signal, key, { value, enumerable: false, configurable: true });
        } catch {}
      }
    }
    try {
      Object.defineProperty(signal, "operations", { value: operations, enumerable: false, configurable: true });
    } catch {}
  }
  return signal;
}

function createToolUpdateCallback(ctx, toolCallId) {
  return (update) => {
    const serialized = safeSerializable(update) || {};
    const text = resultToText(update);
    if (ctx && ctx.__uiState && typeof ctx.__uiState.pushSurface === "function") {
      ctx.__uiState.pushSurface("tool_update", {
        toolCallId: String(toolCallId || ""),
        update: serialized,
        text,
      });
    }
  };
}

function renderComponentPayload(result) {
  if (!result || typeof result !== "object" || Buffer.isBuffer(result) || result instanceof Uint8Array) return undefined;
  if (Array.isArray(result)) return result;
  if (typeof result.text === "string" && Object.keys(result).length <= 2) return undefined;
  if (typeof result.markdown === "string" && Object.keys(result).length <= 2) return undefined;
  return result;
}

function parseMaybeJson(value) {
  if (value == null || typeof value !== "string") return value || {};
  try {
    return JSON.parse(value || "{}");
  } catch {
    return {};
  }
}

function providerContentBlocks(result) {
  const blocks = [];
  const pushText = (text) => {
    const value = String(text || "");
    if (value) blocks.push({ type: "text", text: value });
  };
  const pushBlock = (item) => {
    if (typeof item === "string") {
      pushText(item);
    } else if (item && item.type === "text" && typeof item.text === "string") {
      pushText(item.text);
    } else if (item && (item.type === "tool_use" || item.type === "tool_call" || item.name || item.function)) {
      const fn = item.function || {};
      const name = item.name || fn.name || item.toolName || "";
      if (name) {
        blocks.push({
          type: "tool_use",
          id: item.id || item.toolCallId || `tool-${blocks.length + 1}`,
          name,
          input: item.input || item.arguments || parseMaybeJson(fn.arguments),
        });
      }
    } else if (item) {
      pushText(resultToText(item));
    }
  };
  if (typeof result === "string") pushText(result);
  else if (result && Array.isArray(result.content)) result.content.forEach(pushBlock);
  else if (result && result.message && Array.isArray(result.message.content)) result.message.content.forEach(pushBlock);
  else if (result && typeof result.text === "string") pushText(result.text);
  else if (result && typeof result.output === "string") pushText(result.output);
  else if (result && typeof result.response === "string") pushText(result.response);
  else pushText(resultToText(result));
  if (result && Array.isArray(result.toolCalls)) result.toolCalls.forEach(pushBlock);
  if (result && Array.isArray(result.tool_calls)) result.tool_calls.forEach(pushBlock);
  return blocks;
}

function providerUsage(result) {
  const usage = result && result.usage ? result.usage : {};
  return {
    inputTokens: usage.inputTokens || usage.input_tokens || usage.prompt_tokens || usage.promptTokens || 0,
    outputTokens: usage.outputTokens || usage.output_tokens || usage.completion_tokens || usage.completionTokens || 0,
  };
}

function outputText(data) {
  if (Buffer.isBuffer(data)) return data.toString("utf8");
  if (data instanceof Uint8Array) return Buffer.from(data).toString("utf8");
  return String(data);
}

async function runBashOperations(operations, command, cwd) {
  const chunks = [];
  const onData = (data) => chunks.push(outputText(data).replace(/\r/g, ""));
  try {
    const result = await operations.exec(command, cwd, {
      onData,
      env: process.env,
    });
    const exitCode = result && typeof result.exitCode === "number" ? result.exitCode : 0;
    return { output: chunks.join(""), exitCode, cancelled: false, truncated: false };
  } catch (error) {
    const message = error && error.message ? error.message : String(error);
    const output = chunks.join("");
    return {
      output: output ? `${output}\nError: ${message}` : `Error: ${message}`,
      exitCode: 1,
      cancelled: false,
      truncated: false,
    };
  }
}

async function handleRequest(request) {
  try {
    const tools = await loadTools(request.paths || request.path, request.flags || {}, request);
    if (request.mode === "describe") {
      return { ok: true, tools: toolPayload(tools), commands: commandPayload(tools), flags: flagPayload(tools), shortcuts: shortcutPayload(tools), providers: providerPayload(tools), renderers: rendererPayload(tools), events: [...tools.handlers.keys()], ...activeToolState(tools) };
    }
    if (request.mode === "provider") {
      await replaySessionStart(tools);
      resetProviderRuntimeState(tools);
      const provider = findProvider(tools, request.provider);
      if (!provider) throw new Error(`provider not registered: ${request.provider}`);
      const handler = providerRuntimeHandler(provider);
      if (!handler) throw new Error(`provider has no runtime: ${request.provider}`);
      const ctx = eventContext(request.uiResponses || {}, null, tools);
      const result = await handler({
        type: "provider_runtime",
        provider: request.provider,
        model: request.model || provider.defaultModel || provider.default_model || provider.model || "",
        system: request.system || "",
        messages: request.messages || [],
        tools: request.tools || [],
        toolsEnabled: !!request.toolsEnabled,
        maxTokens: request.maxTokens || 0,
        thinking: request.thinking || "off",
      }, ctx);
      return { ok: true, content: providerContentBlocks(result), usage: providerUsage(result), ui: uiPayload(ctx, tools), ...activeToolState(tools) };
    }
    if (request.mode === "execute") {
      await replaySessionStart(tools);
      resetProviderRuntimeState(tools);
      const tool = tools.tools.get(request.tool);
      if (!tool) throw new Error(`tool not registered: ${request.tool}`);
      const operations = createLocalToolOperations();
      const ctx = eventContext(request.uiResponses || {}, null, tools);
      ctx.operations = operations;
      const toolCallId = request.toolCallId || "ocaml-agent";
      const signal = createToolSignal(operations);
      ctx.signal = signal;
      const onUpdate = createToolUpdateCallback(ctx, toolCallId);
      let params = request.input || {};
      if (typeof tool.prepareArguments === "function") {
        const prepared = await tool.prepareArguments(params);
        if (prepared !== undefined) params = prepared;
      }
      const result = await tool.execute(toolCallId, params, signal, onUpdate, ctx);
      return { ok: true, text: textWithUi(resultToText(result), ctx), ui: uiPayload(ctx, tools), ...activeToolState(tools) };
    }
    if (request.mode === "command") {
      await replaySessionStart(tools);
      resetProviderRuntimeState(tools);
      const command = tools.commands.get(request.command);
      if (!command || typeof command.handler !== "function") throw new Error(`command not registered: ${request.command}`);
      const ctx = eventContext(request.uiResponses || {}, null, tools);
      const result = await command.handler(request.args || "", ctx);
      const text = textWithUi(result == null ? "" : resultToText(result), ctx);
      return { ok: true, text: text || `Command /${request.command} completed.`, ui: uiPayload(ctx, tools), ...activeToolState(tools) };
    }
    if (request.mode === "command_completions") {
      await replaySessionStart(tools);
      resetProviderRuntimeState(tools);
      const command = tools.commands.get(request.command);
      if (!command || typeof command.getArgumentCompletions !== "function") {
        return { ok: true, items: [], ...activeToolState(tools) };
      }
      const items = await command.getArgumentCompletions(request.prefix || "");
      return { ok: true, items: autocompleteItemsPayload(items), ...activeToolState(tools) };
    }
    if (request.mode === "shortcut") {
      await replaySessionStart(tools);
      resetProviderRuntimeState(tools);
      const spec = normalizeShortcutSpec(request.shortcut || request.spec || "");
      const shortcut = tools.shortcuts.get(spec);
      if (!shortcut) throw new Error(`shortcut not registered: ${spec}`);
      if (typeof shortcut.command === "string" || typeof shortcut.action === "string") {
        return { ok: true, command: shortcut.command || shortcut.action, ...activeToolState(tools) };
      }
      if (typeof shortcut.handler !== "function") {
        return { ok: true, text: "", ...activeToolState(tools) };
      }
      const ctx = eventContext(request.uiResponses || {}, null, tools);
      const result = await shortcut.handler(ctx);
      return { ok: true, text: textWithUi(resultToText(result), ctx), ui: uiPayload(ctx, tools), ...activeToolState(tools) };
    }
    if (request.mode === "render") {
      const kind = request.kind || "message";
      let text = request.text || "";
      const components = [];
      const ctx = eventContext(request.uiResponses || {}, null, tools);
      for (const renderer of tools.renderers.values()) {
        const target = renderer.target || renderer.kind || renderer.type || "all";
        if (target !== "all" && target !== kind) continue;
        const handler = typeof renderer.render === "function" ? renderer.render : renderer.handler;
        if (typeof handler !== "function") continue;
        const result = await handler({
          type: "message_renderer",
          kind,
          role: request.role || "",
          toolName: request.toolName || "",
          text,
          content: [{ type: "text", text }],
          message: request.message || null,
        }, ctx);
        const component = renderComponentPayload(result);
        if (Array.isArray(component)) components.push(...component);
        else if (component !== undefined) components.push(component);
        const rendered = resultToText(result);
        if (rendered !== "") text = rendered;
      }
      return { ok: true, text, components, ui: uiPayload(ctx, tools), ...activeToolState(tools) };
    }
    if (request.mode === "event") {
      const list = tools.handlers.get(request.event) || [];
      const event = request.payload || {};
      const ctx = eventContext(request.uiResponses || {}, null, tools);
      resetProviderRuntimeState(tools);
      let lastResult;
      for (const handler of list) {
        const result = await handler(event, ctx);
        if (result) {
          if (request.event === "user_bash") {
            if (result.result) {
              lastResult = { result: result.result };
            } else if (result.operations && typeof result.operations.exec === "function") {
              const command = request.executionCommand || event.command || "";
              const cwd = event.cwd || process.cwd();
              lastResult = { result: await runBashOperations(result.operations, command, cwd) };
            } else {
              const { operations, ...serializable } = result;
              lastResult = serializable;
            }
            break;
          }
          lastResult = result;
          if (request.event === "context" && result.messages) {
            event.messages = result.messages;
            lastResult = { messages: event.messages };
          }
          if (request.event === "message_end" && result.message) {
            event.message = result.message;
            lastResult = { message: event.message };
          }
          if (request.event === "input" && result.action === "transform") {
            event.text = result.text;
            if (result.images !== undefined) event.images = result.images;
          }
          if (request.event === "input" && result.action === "handled") break;
          if (request.event === "tool_call" && result.block) break;
        }
      }
      return { ok: true, event, result: lastResult || null, tools: toolPayload(tools), commands: commandPayload(tools), flags: flagPayload(tools), shortcuts: shortcutPayload(tools), providers: providerPayload(tools), renderers: rendererPayload(tools), ui: uiPayload(ctx, tools), ...activeToolState(tools) };
    }
    throw new Error("unknown bridge mode");
  } catch (error) {
    return { ok: false, error: error && error.message ? error.message : String(error) };
  }
}

(async () => {
  const raw = await readStdin();
  const request = raw.trim() ? JSON.parse(raw) : {};
  process.stdout.write(JSON.stringify(await handleRequest(request)));
})();
|js}

let bridge_path = lazy (
  let path = Filename.temp_file "ocaml-agent-pi-extension-bridge-" ".cjs" in
  Tools.write_file_contents path source;
  at_exit (fun () -> try Sys.remove path with _ -> ());
  path)


let path () = Lazy.force bridge_path
