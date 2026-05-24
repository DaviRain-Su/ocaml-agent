let source =
  {js|
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const os = require("node:os");
const childProcess = require("node:child_process");
const { pathToFileURL, fileURLToPath } = require("node:url");

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
  Enum: (values = {}, options = {}) => ({ type: "string", enum: Array.isArray(values) ? values : Object.values(values), ...options }),
  Union: (items, options = {}) => ({ anyOf: items, ...options }),
  Optional: (inner) => ({ ...inner, __optional: true }),
  Record: (_key, value, options = {}) => ({ type: "object", additionalProperties: value, ...options }),
  Any: (options = {}) => options,
  Unknown: (options = {}) => options,
  Null: (options = {}) => ({ type: "null", ...options }),
  Undefined: (options = {}) => ({ type: "undefined", ...options }),
  Void: (options = {}) => ({ type: "void", ...options }),
  Never: (options = {}) => ({ not: {}, ...options }),
  Intersect: (items, options = {}) => ({ allOf: items, ...options }),
  Readonly: (inner) => ({ ...inner }),
  Unsafe: (definition = {}) => ({ ...definition }),
};

function typeboxSchemaTypes(schema) {
  if (!schema || typeof schema !== "object") return [];
  if (typeof schema.type === "string") return [schema.type];
  if (Array.isArray(schema.type)) return schema.type.filter((item) => typeof item === "string");
  return [];
}

function typeboxMatchesType(value, type) {
  switch (type) {
    case "string": return typeof value === "string";
    case "number": return typeof value === "number" && Number.isFinite(value);
    case "integer": return typeof value === "number" && Number.isInteger(value);
    case "boolean": return typeof value === "boolean";
    case "array": return Array.isArray(value);
    case "object": return value !== null && typeof value === "object" && !Array.isArray(value);
    case "null": return value === null;
    case "undefined": return value === undefined;
    case "void": return value === undefined;
    default: return true;
  }
}

function typeboxCoerce(value, schema) {
  if (!schema || typeof schema !== "object") return value;
  if (schema.const !== undefined) return value === schema.const ? value : value;
  if (Array.isArray(schema.anyOf)) {
    for (const option of schema.anyOf) {
      const converted = typeboxCoerce(value, option);
      if (typeboxCheck(option, converted)) return converted;
    }
    return value;
  }
  if (Array.isArray(schema.allOf)) {
    return schema.allOf.reduce((current, option) => typeboxCoerce(current, option), value);
  }
  const [type] = typeboxSchemaTypes(schema);
  if (type === "number" || type === "integer") {
    if (typeof value === "string" && value.trim() !== "") {
      const parsed = Number(value);
      if (Number.isFinite(parsed)) return type === "integer" ? Math.trunc(parsed) : parsed;
    }
    if (value === null) return 0;
  }
  if (type === "boolean") {
    if (value === "true") return true;
    if (value === "false") return false;
  }
  if (type === "string" && value !== undefined && value !== null && typeof value !== "object") return String(value);
  if (type === "array" && Array.isArray(value) && schema.items) return value.map((item) => typeboxCoerce(item, schema.items));
  if (type === "object" && value && typeof value === "object" && !Array.isArray(value)) {
    const out = { ...value };
    for (const [key, child] of Object.entries(schema.properties || {})) {
      if (Object.prototype.hasOwnProperty.call(out, key)) out[key] = typeboxCoerce(out[key], child);
      else if (child && typeof child === "object" && Object.prototype.hasOwnProperty.call(child, "default")) out[key] = child.default;
    }
    return out;
  }
  return value;
}

function typeboxCheck(schema, value, errors = [], path = "") {
  if (!schema || typeof schema !== "object") return true;
  if (schema.const !== undefined && value !== schema.const) {
    errors.push({ path, instancePath: path, message: `Expected ${JSON.stringify(schema.const)}` });
    return false;
  }
  if (Array.isArray(schema.enum) && !schema.enum.includes(value)) {
    errors.push({ path, instancePath: path, message: `Expected one of ${schema.enum.map(String).join(", ")}` });
    return false;
  }
  if (Array.isArray(schema.anyOf)) {
    if (schema.anyOf.some((option) => typeboxCheck(option, value, [], path))) return true;
    errors.push({ path, instancePath: path, message: "Expected union member" });
    return false;
  }
  if (Array.isArray(schema.allOf)) {
    let ok = true;
    for (const option of schema.allOf) ok = typeboxCheck(option, value, errors, path) && ok;
    return ok;
  }
  const types = typeboxSchemaTypes(schema);
  if (types.length > 0 && !types.some((type) => typeboxMatchesType(value, type))) {
    errors.push({ path, instancePath: path, message: `Expected ${types.join(" or ")}` });
    return false;
  }
  if (schema.type === "object" && value && typeof value === "object" && !Array.isArray(value)) {
    let ok = true;
    for (const key of schema.required || []) {
      if (!Object.prototype.hasOwnProperty.call(value, key)) {
        errors.push({ path: `${path}/${key}`, instancePath: `${path}/${key}`, message: `Required property ${key}` });
        ok = false;
      }
    }
    for (const [key, child] of Object.entries(schema.properties || {})) {
      if (Object.prototype.hasOwnProperty.call(value, key)) ok = typeboxCheck(child, value[key], errors, `${path}/${key}`) && ok;
    }
    return ok;
  }
  if (schema.type === "array" && Array.isArray(value) && schema.items) {
    let ok = true;
    value.forEach((item, index) => { ok = typeboxCheck(schema.items, item, errors, `${path}/${index}`) && ok; });
    return ok;
  }
  return true;
}

const Compile = (schema) => ({
  schema,
  Check: (value) => typeboxCheck(schema, value),
  Errors: (value) => {
    const errors = [];
    typeboxCheck(schema, value, errors);
    return errors;
  },
  Decode: (value) => typeboxCoerce(value, schema),
  Encode: (value) => value,
  Parse: (value) => Value.Parse(schema, value),
});

const TypeCompiler = { Compile };

const Value = {
  Convert: (schema, value) => {
    const converted = typeboxCoerce(value, schema);
    if (value && converted && typeof value === "object" && typeof converted === "object" && !Array.isArray(value) && !Array.isArray(converted)) {
      for (const key of Object.keys(value)) delete value[key];
      Object.assign(value, converted);
      return value;
    }
    return converted;
  },
  Check: (schema, value) => typeboxCheck(schema, value),
  Errors: (schema, value) => Compile(schema).Errors(value),
  Parse: (schema, value) => {
    const converted = typeboxCoerce(value, schema);
    if (!typeboxCheck(schema, converted)) throw new Error("Value does not match schema");
    return converted;
  },
  Clone: (value) => cloneJson(value),
  Default: (schema, value) => typeboxCoerce(value === undefined ? {} : value, schema),
  Clean: (_schema, value) => value,
  Create: (schema) => typeboxCoerce(undefined, schema),
};

function typeboxExports() { return { Type, Static: undefined, TSchema: undefined }; }
function typeboxCompileExports() { return { Compile, TypeCompiler }; }
function typeboxValueExports() { return { Value }; }
function typeboxErrorExports() {
  return {
    ValueErrorType: {},
    ValueErrorIterator: class ValueErrorIterator {},
    Format: (errors) => Array.from(errors || []).map((error) => `${error.instancePath || error.path || ""}: ${error.message || ""}`).join("\n"),
  };
}

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

function defaultSessionDirForCwd(cwd = process.cwd(), agentDir = getAgentDir()) {
  const resolvedCwd = path.resolve(cwd || process.cwd());
  const safePath = `--${resolvedCwd.replace(/^[/\\]/, "").replace(/[/\\:]/g, "-")}--`;
  const dir = path.join(expandBridgeTilde(agentDir || getAgentDir()), "sessions", safePath);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function sdkSessionFilePath(sessionDir, sessionId, timestamp = new Date().toISOString()) {
  const fileTimestamp = timestamp.replace(/[:.]/g, "-");
  return path.join(sessionDir, `${fileTimestamp}_${sessionId}.jsonl`);
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

function findMostRecentSessionFile(sessionDir) {
  const sessions = listSessionsFromDir(sessionDir);
  return sessions.length > 0 ? sessions[0].file : undefined;
}

function sdkSessionHeader(session) {
  return {
    type: "session",
    version: CURRENT_SESSION_VERSION,
    id: session.id || sdkNewEntryId("session"),
    timestamp: session.timestamp || new Date().toISOString(),
    cwd: session.cwd || process.cwd(),
    parentSession: session.parentSession,
    name: session.name || undefined,
  };
}

function persistSdkSession(session) {
  if (!session || session.persist === false || typeof session.path !== "string" || !session.path) return;
  ensureParentDir(session.path);
  const rows = [sdkSessionHeader(session), ...(Array.isArray(session.entries) ? session.entries : [])];
  fs.writeFileSync(session.path, `${rows.map((entry) => JSON.stringify(entry)).join("\n")}\n`, "utf8");
}

class SessionManager {
  constructor(session = undefined) {
    const source = session && session._session ? session._session : session;
    Object.assign(this, createSdkSessionManagerFromSession(source || {
      id: sdkNewEntryId("session"),
      cwd: process.cwd(),
      sessionDir: "",
      entries: [],
      leafId: null,
      path: undefined,
    }));
  }

  static create(cwd = process.cwd(), sessionDir = undefined) {
    const id = sdkNewEntryId("session");
    const timestamp = new Date().toISOString();
    const dir = sessionDir ? expandBridgeTilde(sessionDir) : defaultSessionDirForCwd(cwd);
    return new SessionManager({
      id,
      timestamp,
      cwd: path.resolve(cwd || process.cwd()),
      sessionDir: dir,
      entries: [],
      leafId: null,
      path: sdkSessionFilePath(dir, id, timestamp),
    });
  }

  static open(filePath, sessionDir = undefined, cwdOverride = undefined) {
    const resolvedPath = path.resolve(String(filePath || ""));
    return new SessionManager(createSdkSessionManagerFromJsonl(resolvedPath, sessionDir ? expandBridgeTilde(sessionDir) : path.dirname(resolvedPath), cwdOverride)._session);
  }

  static continueRecent(cwd = process.cwd(), sessionDir = undefined) {
    const dir = sessionDir ? expandBridgeTilde(sessionDir) : defaultSessionDirForCwd(cwd);
    const mostRecent = findMostRecentSessionFile(dir);
    return mostRecent ? SessionManager.open(mostRecent, dir) : SessionManager.create(cwd, dir);
  }

  static inMemory(cwd = process.cwd()) {
    return new SessionManager({
      id: sdkNewEntryId("session"),
      cwd: path.resolve(cwd || process.cwd()),
      sessionDir: "",
      entries: [],
      leafId: null,
      path: undefined,
      persist: false,
    });
  }

  static forkFrom(sourcePath, targetCwd, sessionDir = undefined) {
    const resolvedSourcePath = path.resolve(String(sourcePath || ""));
    const rows = readJsonLines(resolvedSourcePath);
    if (rows.length === 0) throw new Error(`Cannot fork: source session file is empty or invalid: ${resolvedSourcePath}`);
    const sourceHeader = rows.find((entry) => entry && entry.type === "session");
    if (!sourceHeader) throw new Error(`Cannot fork: source session has no header: ${resolvedSourcePath}`);
    const cwd = path.resolve(targetCwd || process.cwd());
    const dir = sessionDir ? expandBridgeTilde(sessionDir) : defaultSessionDirForCwd(cwd);
    fs.mkdirSync(dir, { recursive: true });
    const id = sdkNewEntryId("session");
    const timestamp = new Date().toISOString();
    const filePath = sdkSessionFilePath(dir, id, timestamp);
    const header = {
      type: "session",
      version: CURRENT_SESSION_VERSION,
      id,
      timestamp,
      cwd,
      parentSession: resolvedSourcePath,
    };
    const body = rows.filter((entry) => entry && entry.type !== "session");
    fs.writeFileSync(filePath, `${[header, ...body].map((entry) => JSON.stringify(entry)).join("\n")}\n`, "utf8");
    return SessionManager.open(filePath, dir, cwd);
  }

  static async list(_cwd = process.cwd(), sessionDir = undefined, onProgress = undefined) {
    const dir = sessionDir ? expandBridgeTilde(sessionDir) : defaultSessionDirForCwd(_cwd);
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

const CURRENT_SESSION_VERSION = 3;
const COMPACTION_SUMMARY_PREFIX = "The conversation history before this point was compacted into the following summary:\n\n<summary>\n";
const COMPACTION_SUMMARY_SUFFIX = "\n</summary>";
const BRANCH_SUMMARY_PREFIX = "The following is a summary of a branch that this conversation came back from:\n\n<summary>\n";
const BRANCH_SUMMARY_SUFFIX = "</summary>";

function parseSkillBlock(text) {
  const match = String(text || "").match(/^<skill name="([^"]+)" location="([^"]+)">\n([\s\S]*?)\n<\/skill>(?:\n\n([\s\S]+))?$/);
  if (!match) return null;
  return {
    name: match[1],
    location: match[2],
    content: match[3],
    userMessage: match[4] ? match[4].trim() || undefined : undefined,
  };
}

function bashExecutionToText(message) {
  let text = `Ran \`${message.command || ""}\`\n`;
  if (message.output) text += `\`\`\`\n${message.output}\n\`\`\``;
  else text += "(no output)";
  if (message.cancelled) text += "\n\n(command cancelled)";
  else if (message.exitCode !== null && message.exitCode !== undefined && message.exitCode !== 0) text += `\n\nCommand exited with code ${message.exitCode}`;
  if (message.truncated && message.fullOutputPath) text += `\n\n[Output truncated. Full output: ${message.fullOutputPath}]`;
  return text;
}

function createBranchSummaryMessage(summary, fromId, timestamp) {
  return { role: "branchSummary", summary: String(summary || ""), fromId: String(fromId || ""), timestamp: new Date(timestamp || Date.now()).getTime() };
}

function createCompactionSummaryMessage(summary, tokensBefore, timestamp) {
  return { role: "compactionSummary", summary: String(summary || ""), tokensBefore: Number(tokensBefore || 0), timestamp: new Date(timestamp || Date.now()).getTime() };
}

function createCustomMessage(customType, content, display, details, timestamp) {
  return {
    role: "custom",
    customType: String(customType || ""),
    content: Array.isArray(content) ? content : String(content || ""),
    display: !!display,
    details,
    timestamp: new Date(timestamp || Date.now()).getTime(),
  };
}

function convertToLlm(messages = []) {
  const out = [];
  for (const message of messages || []) {
    if (!message || typeof message !== "object") continue;
    switch (message.role) {
      case "bashExecution":
        if (!message.excludeFromContext) {
          out.push({ role: "user", content: [{ type: "text", text: bashExecutionToText(message) }], timestamp: message.timestamp });
        }
        break;
      case "custom":
        out.push({
          role: "user",
          content: typeof message.content === "string" ? [{ type: "text", text: message.content }] : message.content,
          timestamp: message.timestamp,
        });
        break;
      case "branchSummary":
        out.push({
          role: "user",
          content: [{ type: "text", text: BRANCH_SUMMARY_PREFIX + String(message.summary || "") + BRANCH_SUMMARY_SUFFIX }],
          timestamp: message.timestamp,
        });
        break;
      case "compactionSummary":
        out.push({
          role: "user",
          content: [{ type: "text", text: COMPACTION_SUMMARY_PREFIX + String(message.summary || "") + COMPACTION_SUMMARY_SUFFIX }],
          timestamp: message.timestamp,
        });
        break;
      case "user":
      case "assistant":
      case "toolResult":
      case "tool_result":
        out.push(message);
        break;
    }
  }
  return out;
}

function parseSessionEntries(content) {
  const entries = [];
  for (const line of String(content || "").trim().split(/\r?\n/)) {
    if (!line.trim()) continue;
    try {
      entries.push(JSON.parse(line));
    } catch {}
  }
  return entries;
}

function generatedSessionEntryId(used) {
  for (let i = 0; i < 100; i++) {
    const id = Math.floor(Math.random() * 0xffffffff).toString(16).padStart(8, "0");
    if (!used.has(id)) return id;
  }
  return `${Date.now().toString(16)}${Math.floor(Math.random() * 0xffffffff).toString(16)}`;
}

function migrateSessionEntries(entries = []) {
  const header = entries.find((entry) => entry && entry.type === "session");
  const version = header && typeof header.version === "number" ? header.version : 1;
  if (version < 2) {
    const used = new Set();
    let prevId = null;
    for (let i = 0; i < entries.length; i++) {
      const entry = entries[i];
      if (!entry || typeof entry !== "object") continue;
      if (entry.type === "session") {
        entry.version = 2;
        continue;
      }
      if (!entry.id) entry.id = generatedSessionEntryId(used);
      used.add(entry.id);
      if (!Object.prototype.hasOwnProperty.call(entry, "parentId")) entry.parentId = prevId;
      prevId = entry.id;
      if (entry.type === "compaction" && typeof entry.firstKeptEntryIndex === "number") {
        const target = entries[entry.firstKeptEntryIndex];
        if (target && target.type !== "session" && target.id) entry.firstKeptEntryId = target.id;
        delete entry.firstKeptEntryIndex;
      }
    }
  }
  if (version < 3) {
    for (const entry of entries) {
      if (!entry || typeof entry !== "object") continue;
      if (entry.type === "session") {
        entry.version = 3;
      } else if (entry.type === "message" && entry.message && entry.message.role === "hookMessage") {
        entry.message.role = "custom";
      }
    }
  }
}

function getLatestCompactionEntry(entries = []) {
  for (let i = entries.length - 1; i >= 0; i--) {
    if (entries[i] && entries[i].type === "compaction") return entries[i];
  }
  return null;
}

function buildSessionContext(entries = [], leafId = undefined, byId = undefined) {
  const index = byId instanceof Map ? byId : new Map((entries || []).filter((entry) => entry && typeof entry.id === "string").map((entry) => [entry.id, entry]));
  if (leafId === null) return { messages: [], thinkingLevel: "off", model: null };
  let leaf = leafId ? index.get(leafId) : undefined;
  if (!leaf) leaf = entries.length ? entries[entries.length - 1] : undefined;
  if (!leaf) return { messages: [], thinkingLevel: "off", model: null };

  const pathEntries = [];
  const seen = new Set();
  let current = leaf;
  while (current && typeof current === "object" && !seen.has(current.id)) {
    if (current.id) seen.add(current.id);
    pathEntries.unshift(current);
    current = current.parentId ? index.get(current.parentId) : undefined;
  }

  let thinkingLevel = "off";
  let model = null;
  let compaction = null;
  for (const entry of pathEntries) {
    if (!entry || typeof entry !== "object") continue;
    if (entry.type === "thinking_level_change") thinkingLevel = String(entry.thinkingLevel || "off");
    else if (entry.type === "model_change") model = { provider: String(entry.provider || ""), modelId: String(entry.modelId || "") };
    else if (entry.type === "message" && entry.message && entry.message.role === "assistant" && entry.message.provider && entry.message.model) {
      model = { provider: entry.message.provider, modelId: entry.message.model };
    } else if (entry.type === "compaction") {
      compaction = entry;
    }
  }

  const messages = [];
  const appendMessage = (entry) => {
    if (!entry || typeof entry !== "object") return;
    if (entry.type === "message" && entry.message) messages.push(entry.message);
    else if (entry.type === "custom_message") messages.push(createCustomMessage(entry.customType, entry.content, entry.display, entry.details, entry.timestamp));
    else if (entry.type === "branch_summary" && entry.summary) messages.push(createBranchSummaryMessage(entry.summary, entry.fromId, entry.timestamp));
  };

  if (compaction) {
    messages.push(createCompactionSummaryMessage(compaction.summary, compaction.tokensBefore, compaction.timestamp));
    const compactionIndex = pathEntries.findIndex((entry) => entry && entry.type === "compaction" && entry.id === compaction.id);
    let foundFirstKept = false;
    for (let i = 0; i < compactionIndex; i++) {
      const entry = pathEntries[i];
      if (entry && entry.id === compaction.firstKeptEntryId) foundFirstKept = true;
      if (foundFirstKept) appendMessage(entry);
    }
    for (let i = compactionIndex + 1; i < pathEntries.length; i++) appendMessage(pathEntries[i]);
  } else {
    for (const entry of pathEntries) appendMessage(entry);
  }

  return { messages, thinkingLevel, model };
}

function cloneJson(value) {
  if (value === undefined) return undefined;
  return JSON.parse(JSON.stringify(value));
}

function writeJsonFile(filePath, value) {
  ensureParentDir(filePath);
  fs.writeFileSync(filePath, JSON.stringify(value || {}, null, 2), "utf8");
}

function providerEnvKeys(provider) {
  const id = String(provider || "").trim();
  const upper = id.replace(/[^A-Za-z0-9]/g, "_").toUpperCase();
  const known = {
    anthropic: ["ANTHROPIC_API_KEY"],
    claude: ["ANTHROPIC_API_KEY"],
    openai: ["OPENAI_API_KEY"],
    deepseek: ["DEEPSEEK_API_KEY"],
    kimi: ["KIMI_API_KEY"],
    moonshot: ["MOONSHOT_API_KEY"],
    openrouter: ["OPENROUTER_API_KEY"],
    groq: ["GROQ_API_KEY"],
    xai: ["XAI_API_KEY"],
    grok: ["XAI_API_KEY"],
    mistral: ["MISTRAL_API_KEY"],
    zai: ["ZAI_API_KEY", "ZHIPU_API_KEY"],
    zhipu: ["ZAI_API_KEY", "ZHIPU_API_KEY"],
    glm: ["ZAI_API_KEY", "ZHIPU_API_KEY"],
    gemini: ["GEMINI_API_KEY", "GOOGLE_API_KEY"],
    google: ["GEMINI_API_KEY", "GOOGLE_API_KEY"],
  };
  return [...(known[id.toLowerCase()] || []), `${upper}_API_KEY`].filter((key, index, keys) => key && keys.indexOf(key) === index);
}

const BRIDGE_OAUTH_PROVIDERS = new Map();

function registerBridgeOAuthProvider(provider) {
  if (!provider || typeof provider !== "object") return;
  const id = String(provider.id || provider.name || "").trim();
  if (!id) return;
  BRIDGE_OAUTH_PROVIDERS.set(id, { ...provider, id });
}

function unregisterBridgeOAuthProvider(providerId) {
  BRIDGE_OAUTH_PROVIDERS.delete(String(providerId || "").trim());
}

function getBridgeOAuthProvider(providerId) {
  return BRIDGE_OAUTH_PROVIDERS.get(String(providerId || "").trim());
}

function bridgeOAuthApiKey(provider, credentials) {
  if (!credentials || typeof credentials !== "object") return undefined;
  if (provider && typeof provider.getApiKey === "function") return provider.getApiKey(credentials);
  if (typeof credentials.access === "string") return credentials.access;
  if (typeof credentials.accessToken === "string") return credentials.accessToken;
  if (typeof credentials.apiKey === "string") return credentials.apiKey;
  if (typeof credentials.key === "string") return credentials.key;
  return undefined;
}

class FileAuthStorageBackend {
  constructor(authPath = path.join(getAgentDir(), "auth.json")) {
    this.authPath = expandBridgeTilde(authPath);
  }

  withLock(fn) {
    const current = fs.existsSync(this.authPath) ? fs.readFileSync(this.authPath, "utf8") : undefined;
    const result = fn(current);
    if (result && Object.prototype.hasOwnProperty.call(result, "next")) {
      if (result.next === undefined) {
        try {
          fs.unlinkSync(this.authPath);
        } catch {}
      } else {
        writeJsonFile(this.authPath, JSON.parse(result.next || "{}"));
      }
    }
    return result ? result.result : undefined;
  }

  async withLockAsync(fn) {
    const current = fs.existsSync(this.authPath) ? fs.readFileSync(this.authPath, "utf8") : undefined;
    const result = await fn(current);
    if (result && Object.prototype.hasOwnProperty.call(result, "next")) {
      if (result.next === undefined) {
        try {
          fs.unlinkSync(this.authPath);
        } catch {}
      } else {
        writeJsonFile(this.authPath, JSON.parse(result.next || "{}"));
      }
    }
    return result ? result.result : undefined;
  }
}

class InMemoryAuthStorageBackend {
  constructor(initial = {}) {
    this.content = JSON.stringify(initial || {}, null, 2);
  }

  withLock(fn) {
    const result = fn(this.content);
    if (result && Object.prototype.hasOwnProperty.call(result, "next")) this.content = result.next;
    return result ? result.result : undefined;
  }

  async withLockAsync(fn) {
    const result = await fn(this.content);
    if (result && Object.prototype.hasOwnProperty.call(result, "next")) this.content = result.next;
    return result ? result.result : undefined;
  }
}

class AuthStorage {
  constructor(storage) {
    this.storage = storage;
    this.data = {};
    this.runtimeOverrides = new Map();
    this.errors = [];
    this.fallbackResolver = undefined;
    this.reload();
  }

  static create(authPath = undefined) {
    return new AuthStorage(new FileAuthStorageBackend(authPath || path.join(getAgentDir(), "auth.json")));
  }

  static fromStorage(storage) {
    return new AuthStorage(storage);
  }

  static inMemory(data = {}) {
    return new AuthStorage(new InMemoryAuthStorageBackend(data));
  }

  recordError(error) {
    this.errors.push(error instanceof Error ? error : new Error(String(error)));
  }

  parse(content) {
    if (!content || !String(content).trim()) return {};
    return JSON.parse(content);
  }

  reload() {
    try {
      this.storage.withLock((current) => {
        this.data = this.parse(current);
        return { result: undefined };
      });
    } catch (error) {
      this.data = {};
      this.recordError(error);
    }
  }

  persist(provider, credential) {
    try {
      this.storage.withLock((current) => {
        const next = this.parse(current);
        if (credential === undefined) delete next[provider];
        else next[provider] = credential;
        return { result: undefined, next: JSON.stringify(next, null, 2) };
      });
    } catch (error) {
      this.recordError(error);
    }
  }

  setRuntimeApiKey(provider, apiKey) {
    this.runtimeOverrides.set(String(provider), String(apiKey));
  }

  removeRuntimeApiKey(provider) {
    this.runtimeOverrides.delete(String(provider));
  }

  setFallbackResolver(resolver) {
    this.fallbackResolver = typeof resolver === "function" ? resolver : undefined;
  }

  get(provider) {
    return this.data[String(provider)];
  }

  set(provider, credential) {
    const key = String(provider);
    this.data[key] = credential;
    this.persist(key, credential);
  }

  remove(provider) {
    const key = String(provider);
    delete this.data[key];
    this.persist(key, undefined);
  }

  logout(provider) {
    this.remove(provider);
  }

  list() {
    return Object.keys(this.data);
  }

  has(provider) {
    return Object.prototype.hasOwnProperty.call(this.data, String(provider));
  }

  envApiKey(provider) {
    for (const key of providerEnvKeys(provider)) {
      if (process.env[key]) return { key, value: process.env[key] };
    }
    return null;
  }

  hasAuth(provider) {
    const key = String(provider);
    return this.runtimeOverrides.has(key) || this.has(key) || !!this.envApiKey(key) || !!(this.fallbackResolver && this.fallbackResolver(key));
  }

  getAuthStatus(provider) {
    const key = String(provider);
    if (this.runtimeOverrides.has(key)) return { configured: false, source: "runtime", label: "--api-key" };
    if (this.has(key)) return { configured: true, source: "stored" };
    const env = this.envApiKey(key);
    if (env) return { configured: false, source: "environment", label: env.key };
    if (this.fallbackResolver && this.fallbackResolver(key)) return { configured: false, source: "fallback", label: "custom provider config" };
    return { configured: false };
  }

  getAll() {
    return cloneJson(this.data);
  }

  drainErrors() {
    const drained = this.errors.slice();
    this.errors = [];
    return drained;
  }

  async login(providerId, callbacks = {}) {
    const key = String(providerId || "").trim();
    const provider = getBridgeOAuthProvider(key);
    if (!provider || typeof provider.login !== "function") throw new Error(`Unknown OAuth provider: ${key}`);
    const credentials = await provider.login(callbacks || {});
    this.set(key, { type: "oauth", ...(credentials || {}) });
  }

  async refreshOAuthCredentialWithLock(providerId, provider) {
    return this.storage.withLockAsync(async (current) => {
      const currentData = this.parse(current);
      this.data = currentData;
      const credential = currentData[providerId];
      if (!credential || credential.type !== "oauth") return { result: null };
      if (typeof credential.expires === "number" && Date.now() >= credential.expires) {
        if (typeof provider.refreshToken !== "function") return { result: null };
        const refreshed = await provider.refreshToken(credential);
        const nextCredential = { type: "oauth", ...(refreshed || {}) };
        const merged = { ...currentData, [providerId]: nextCredential };
        this.data = merged;
        return {
          result: { credential: nextCredential, apiKey: bridgeOAuthApiKey(provider, nextCredential) },
          next: JSON.stringify(merged, null, 2),
        };
      }
      return { result: { credential, apiKey: bridgeOAuthApiKey(provider, credential) } };
    });
  }

  async getApiKey(provider, options = {}) {
    const key = String(provider);
    if (this.runtimeOverrides.has(key)) return this.runtimeOverrides.get(key);
    const credential = this.data[key];
    if (credential && typeof credential === "object") {
      if (credential.type === "api_key") return resolveModelConfigValue(credential.key);
      if (credential.type === "oauth") {
        const oauthProvider = getBridgeOAuthProvider(key);
        if (!oauthProvider) return undefined;
        const needsRefresh = typeof credential.expires === "number" && Date.now() >= credential.expires;
        if (!needsRefresh) return bridgeOAuthApiKey(oauthProvider, credential);
        try {
          const refreshed = await this.refreshOAuthCredentialWithLock(key, oauthProvider);
          if (refreshed && refreshed.apiKey !== undefined) return refreshed.apiKey;
        } catch (error) {
          this.recordError(error);
          this.reload();
          const updated = this.data[key];
          if (updated && updated.type === "oauth" && !(typeof updated.expires === "number" && Date.now() >= updated.expires)) {
            return bridgeOAuthApiKey(oauthProvider, updated);
          }
        }
        return undefined;
      }
      if (typeof credential.apiKey === "string") return resolveModelConfigValue(credential.apiKey);
      if (typeof credential.key === "string") return resolveModelConfigValue(credential.key);
      if (typeof credential.accessToken === "string") return credential.accessToken;
    }
    if (typeof credential === "string") return credential;
    const env = this.envApiKey(key);
    if (env) return env.value;
    if (options.includeFallback !== false && this.fallbackResolver) return this.fallbackResolver(key);
    return undefined;
  }

  getOAuthProviders() {
    return [...BRIDGE_OAUTH_PROVIDERS.values()];
  }
}

function deepMergeSettings(base, override) {
  const out = { ...(base || {}) };
  for (const [key, value] of Object.entries(override || {})) {
    if (value && typeof value === "object" && !Array.isArray(value) && out[key] && typeof out[key] === "object" && !Array.isArray(out[key])) {
      out[key] = deepMergeSettings(out[key], value);
    } else {
      out[key] = cloneJson(value);
    }
  }
  return out;
}

class FileSettingsStorage {
  constructor(cwd = process.cwd(), agentDir = getAgentDir()) {
    this.cwd = cwd;
    this.agentDir = expandBridgeTilde(agentDir);
    this.paths = {
      global: path.join(this.agentDir, "settings.json"),
      project: path.join(cwd, ".pi", "settings.json"),
    };
  }

  withLock(scope, fn) {
    const filePath = this.paths[scope] || this.paths.global;
    const current = fs.existsSync(filePath) ? fs.readFileSync(filePath, "utf8") : undefined;
    const next = fn(current);
    if (next !== undefined) writeJsonFile(filePath, JSON.parse(next || "{}"));
    return next;
  }
}

class InMemorySettingsStorage {
  constructor(globalSettings = {}, projectSettings = {}) {
    this.content = {
      global: JSON.stringify(globalSettings || {}, null, 2),
      project: JSON.stringify(projectSettings || {}, null, 2),
    };
  }

  withLock(scope, fn) {
    const key = scope === "project" ? "project" : "global";
    const next = fn(this.content[key]);
    if (next !== undefined) this.content[key] = next;
    return next;
  }
}

class SettingsManager {
  constructor(storage) {
    this.storage = storage;
    this.errors = [];
    this.globalSettings = {};
    this.projectSettings = {};
    this.settings = {};
    this.reloadSync();
  }

  static create(cwd = process.cwd(), agentDir = getAgentDir()) {
    return new SettingsManager(new FileSettingsStorage(cwd, agentDir));
  }

  static fromStorage(storage) {
    return new SettingsManager(storage);
  }

  static inMemory(settings = {}) {
    return new SettingsManager(new InMemorySettingsStorage(settings, {}));
  }

  static migrateSettings(settings) {
    const out = cloneJson(settings || {});
    if (out.queueMode !== undefined && out.steeringMode === undefined) {
      out.steeringMode = out.queueMode;
      delete out.queueMode;
    }
    if (typeof out.websockets === "boolean" && out.transport === undefined) {
      out.transport = out.websockets ? "websocket" : "sse";
      delete out.websockets;
    }
    if (out.skills && typeof out.skills === "object" && !Array.isArray(out.skills)) {
      if (out.enableSkillCommands === undefined && out.skills.enableSkillCommands !== undefined) out.enableSkillCommands = out.skills.enableSkillCommands;
      out.skills = Array.isArray(out.skills.customDirectories) ? out.skills.customDirectories : undefined;
    }
    return out;
  }

  loadScope(scope) {
    let content;
    this.storage.withLock(scope, (current) => {
      content = current;
      return undefined;
    });
    return SettingsManager.migrateSettings(content && String(content).trim() ? JSON.parse(content) : {});
  }

  reloadSync() {
    try {
      this.globalSettings = this.loadScope("global");
    } catch (error) {
      this.globalSettings = {};
      this.errors.push({ scope: "global", error });
    }
    try {
      this.projectSettings = this.loadScope("project");
    } catch (error) {
      this.projectSettings = {};
      this.errors.push({ scope: "project", error });
    }
    this.settings = deepMergeSettings(this.globalSettings, this.projectSettings);
  }

  async reload() {
    this.reloadSync();
  }

  async flush() {}

  save(scope = "global") {
    const data = scope === "project" ? this.projectSettings : this.globalSettings;
    try {
      this.storage.withLock(scope, () => JSON.stringify(data || {}, null, 2));
      this.settings = deepMergeSettings(this.globalSettings, this.projectSettings);
    } catch (error) {
      this.errors.push({ scope, error });
    }
  }

  setGlobal(key, value) {
    this.globalSettings[key] = value;
    this.save("global");
  }

  setNestedGlobal(key, nestedKey, value) {
    if (!this.globalSettings[key] || typeof this.globalSettings[key] !== "object") this.globalSettings[key] = {};
    this.globalSettings[key][nestedKey] = value;
    this.save("global");
  }

  getGlobalSettings() { return cloneJson(this.globalSettings); }
  getProjectSettings() { return cloneJson(this.projectSettings); }
  applyOverrides(overrides) { this.settings = deepMergeSettings(this.settings, overrides || {}); }
  drainErrors() { const drained = this.errors.slice(); this.errors = []; return drained; }
  getLastChangelogVersion() { return this.settings.lastChangelogVersion; }
  setLastChangelogVersion(version) { this.setGlobal("lastChangelogVersion", version); }
  getSessionDir() { return this.settings.sessionDir; }
  getDefaultProvider() { return this.settings.defaultProvider; }
  getDefaultModel() { return this.settings.defaultModel; }
  setDefaultProvider(provider) { this.setGlobal("defaultProvider", provider); }
  setDefaultModel(model) { this.setGlobal("defaultModel", model); }
  setDefaultModelAndProvider(provider, model) { this.globalSettings.defaultProvider = provider; this.globalSettings.defaultModel = model; this.save("global"); }
  getSteeringMode() { return this.settings.steeringMode || "one-at-a-time"; }
  setSteeringMode(mode) { this.setGlobal("steeringMode", mode); }
  getFollowUpMode() { return this.settings.followUpMode || "one-at-a-time"; }
  setFollowUpMode(mode) { this.setGlobal("followUpMode", mode); }
  getTheme() { return this.settings.theme; }
  setTheme(theme) { this.setGlobal("theme", theme); }
  getDefaultThinkingLevel() { return this.settings.defaultThinkingLevel; }
  setDefaultThinkingLevel(level) { this.setGlobal("defaultThinkingLevel", level); }
  getTransport() { return this.settings.transport || "auto"; }
  setTransport(transport) { this.setGlobal("transport", transport); }
  getCompactionEnabled() { return this.settings.compaction?.enabled ?? true; }
  setCompactionEnabled(enabled) { this.setNestedGlobal("compaction", "enabled", enabled); }
  getCompactionReserveTokens() { return this.settings.compaction?.reserveTokens ?? 16384; }
  getCompactionKeepRecentTokens() { return this.settings.compaction?.keepRecentTokens ?? 20000; }
  getCompactionSettings() { return { enabled: this.getCompactionEnabled(), reserveTokens: this.getCompactionReserveTokens(), keepRecentTokens: this.getCompactionKeepRecentTokens() }; }
  getBranchSummarySettings() { return { reserveTokens: this.settings.branchSummary?.reserveTokens ?? 16384, skipPrompt: this.settings.branchSummary?.skipPrompt ?? false }; }
  getBranchSummarySkipPrompt() { return this.settings.branchSummary?.skipPrompt ?? false; }
  getRetryEnabled() { return this.settings.retry?.enabled ?? true; }
  setRetryEnabled(enabled) { this.setNestedGlobal("retry", "enabled", enabled); }
  getRetrySettings() { return { enabled: this.getRetryEnabled(), maxRetries: this.settings.retry?.maxRetries ?? 3, baseDelayMs: this.settings.retry?.baseDelayMs ?? 2000 }; }
  getHttpIdleTimeoutMs() { return this.settings.httpIdleTimeoutMs ?? 600000; }
  setHttpIdleTimeoutMs(timeoutMs) { this.setGlobal("httpIdleTimeoutMs", Math.floor(Number(timeoutMs))); }
  getProviderRetrySettings() { return { timeoutMs: this.settings.retry?.provider?.timeoutMs, maxRetries: this.settings.retry?.provider?.maxRetries, maxRetryDelayMs: this.settings.retry?.provider?.maxRetryDelayMs ?? 60000 }; }
  getHideThinkingBlock() { return this.settings.hideThinkingBlock ?? false; }
  setHideThinkingBlock(hide) { this.setGlobal("hideThinkingBlock", hide); }
  getNpmCommand() { return Array.isArray(this.settings.npmCommand) ? this.settings.npmCommand.slice() : undefined; }
  setNpmCommand(command) { this.globalSettings.npmCommand = Array.isArray(command) ? command.slice() : undefined; this.save("global"); }
  getPackages() { return Array.isArray(this.settings.packages) ? cloneJson(this.settings.packages) : []; }
  setPackages(packages) { this.globalSettings.packages = Array.isArray(packages) ? cloneJson(packages) : []; this.save("global"); }
  setProjectPackages(packages) { this.projectSettings.packages = Array.isArray(packages) ? cloneJson(packages) : []; this.save("project"); }
  getExtensionPaths() { return Array.isArray(this.settings.extensions) ? this.settings.extensions.slice() : []; }
  setExtensionPaths(paths) { this.globalSettings.extensions = Array.isArray(paths) ? paths.slice() : []; this.save("global"); }
  setProjectExtensionPaths(paths) { this.projectSettings.extensions = Array.isArray(paths) ? paths.slice() : []; this.save("project"); }
  getSkillPaths() { return Array.isArray(this.settings.skills) ? this.settings.skills.slice() : []; }
  setSkillPaths(paths) { this.globalSettings.skills = Array.isArray(paths) ? paths.slice() : []; this.save("global"); }
  setProjectSkillPaths(paths) { this.projectSettings.skills = Array.isArray(paths) ? paths.slice() : []; this.save("project"); }
  getPromptTemplatePaths() { return Array.isArray(this.settings.prompts) ? this.settings.prompts.slice() : []; }
  setPromptTemplatePaths(paths) { this.globalSettings.prompts = Array.isArray(paths) ? paths.slice() : []; this.save("global"); }
  setProjectPromptTemplatePaths(paths) { this.projectSettings.prompts = Array.isArray(paths) ? paths.slice() : []; this.save("project"); }
  getThemePaths() { return Array.isArray(this.settings.themes) ? this.settings.themes.slice() : []; }
  setThemePaths(paths) { this.globalSettings.themes = Array.isArray(paths) ? paths.slice() : []; this.save("global"); }
  setProjectThemePaths(paths) { this.projectSettings.themes = Array.isArray(paths) ? paths.slice() : []; this.save("project"); }
  getShellPath() { return this.settings.shellPath; }
  setShellPath(shellPath) { this.setGlobal("shellPath", shellPath); }
  getQuietStartup() { return this.settings.quietStartup ?? false; }
  setQuietStartup(quiet) { this.setGlobal("quietStartup", !!quiet); }
  getShellCommandPrefix() { return this.settings.shellCommandPrefix; }
  setShellCommandPrefix(prefix) { this.setGlobal("shellCommandPrefix", prefix); }
  getCollapseChangelog() { return this.settings.collapseChangelog ?? false; }
  setCollapseChangelog(collapse) { this.setGlobal("collapseChangelog", !!collapse); }
  getEnableInstallTelemetry() { return this.settings.enableInstallTelemetry ?? true; }
  setEnableInstallTelemetry(enabled) { this.setGlobal("enableInstallTelemetry", !!enabled); }
  getEnableSkillCommands() { return this.settings.enableSkillCommands ?? true; }
  setEnableSkillCommands(enabled) { this.setGlobal("enableSkillCommands", !!enabled); }
  getThinkingBudgets() { return this.settings.thinkingBudgets ? cloneJson(this.settings.thinkingBudgets) : undefined; }
  getShowImages() { return this.settings.terminal?.showImages ?? true; }
  setShowImages(show) { this.setNestedGlobal("terminal", "showImages", !!show); }
  getImageWidthCells() {
    const width = this.settings.terminal?.imageWidthCells;
    return typeof width === "number" && Number.isFinite(width) ? Math.max(1, Math.floor(width)) : 60;
  }
  setImageWidthCells(width) {
    const numeric = Number(width);
    this.setNestedGlobal("terminal", "imageWidthCells", Math.max(1, Math.floor(Number.isFinite(numeric) ? numeric : 1)));
  }
  getClearOnShrink() {
    if (this.settings.terminal?.clearOnShrink !== undefined) return !!this.settings.terminal.clearOnShrink;
    return process.env.PI_CLEAR_ON_SHRINK === "1";
  }
  setClearOnShrink(enabled) { this.setNestedGlobal("terminal", "clearOnShrink", !!enabled); }
  getShowTerminalProgress() { return this.settings.terminal?.showTerminalProgress ?? false; }
  setShowTerminalProgress(enabled) { this.setNestedGlobal("terminal", "showTerminalProgress", !!enabled); }
  getImageAutoResize() { return this.settings.images?.autoResize ?? true; }
  setImageAutoResize(enabled) { this.setNestedGlobal("images", "autoResize", !!enabled); }
  getBlockImages() { return this.settings.images?.blockImages ?? false; }
  setBlockImages(blocked) { this.setNestedGlobal("images", "blockImages", !!blocked); }
  getEnabledModels() { return Array.isArray(this.settings.enabledModels) ? this.settings.enabledModels.slice() : undefined; }
  setEnabledModels(patterns) { this.setGlobal("enabledModels", Array.isArray(patterns) ? patterns.slice() : undefined); }
  getDoubleEscapeAction() { return this.settings.doubleEscapeAction ?? "tree"; }
  setDoubleEscapeAction(action) { this.setGlobal("doubleEscapeAction", action); }
  getTreeFilterMode() {
    const mode = this.settings.treeFilterMode;
    return ["default", "no-tools", "user-only", "labeled-only", "all"].includes(mode) ? mode : "default";
  }
  setTreeFilterMode(mode) { this.setGlobal("treeFilterMode", mode); }
  getShowHardwareCursor() { return this.settings.showHardwareCursor ?? process.env.PI_HARDWARE_CURSOR === "1"; }
  setShowHardwareCursor(enabled) { this.setGlobal("showHardwareCursor", !!enabled); }
  getEditorPaddingX() {
    const padding = Number(this.settings.editorPaddingX ?? 0);
    return Math.max(0, Math.min(3, Math.floor(Number.isFinite(padding) ? padding : 0)));
  }
  setEditorPaddingX(padding) {
    const numeric = Number(padding);
    this.setGlobal("editorPaddingX", Math.max(0, Math.min(3, Math.floor(Number.isFinite(numeric) ? numeric : 0))));
  }
  getAutocompleteMaxVisible() {
    const maxVisible = Number(this.settings.autocompleteMaxVisible ?? 5);
    return Math.max(3, Math.min(20, Math.floor(Number.isFinite(maxVisible) ? maxVisible : 5)));
  }
  setAutocompleteMaxVisible(maxVisible) {
    const numeric = Number(maxVisible);
    this.setGlobal("autocompleteMaxVisible", Math.max(3, Math.min(20, Math.floor(Number.isFinite(numeric) ? numeric : 5))));
  }
  getCodeBlockIndent() { return this.settings.markdown?.codeBlockIndent ?? "  "; }
  getWarnings() { return this.settings.warnings ? cloneJson(this.settings.warnings) : {}; }
  setWarnings(warnings) { this.setGlobal("warnings", warnings && typeof warnings === "object" ? cloneJson(warnings) : {}); }
}

const BUILT_IN_PROVIDER_DISPLAY_NAMES = {
  anthropic: "Anthropic",
  claude: "Anthropic",
  openai: "OpenAI",
  deepseek: "DeepSeek",
  kimi: "Kimi",
  moonshot: "Moonshot",
  openrouter: "OpenRouter",
  groq: "Groq",
  xai: "xAI",
  grok: "xAI",
  mistral: "Mistral",
  zai: "Z.AI",
  zhipu: "Z.AI",
  glm: "Z.AI",
  gemini: "Google Gemini",
  google: "Google Gemini",
};

const BUILT_IN_MODELS = [
  { provider: "anthropic", id: "claude-opus-4-7", name: "Claude Opus 4.7", api: "anthropic", baseUrl: "https://api.anthropic.com", contextWindow: 200000, maxTokens: 4096, input: ["text", "image"], reasoning: true },
  { provider: "openai", id: "gpt-4o", name: "GPT-4o", api: "openai", baseUrl: "https://api.openai.com/v1", contextWindow: 128000, maxTokens: 4096, input: ["text", "image"], reasoning: false },
  { provider: "deepseek", id: "deepseek-v4-pro", name: "DeepSeek V4 Pro", api: "openai", baseUrl: "https://api.deepseek.com", contextWindow: 1000000, maxTokens: 8192, input: ["text"], reasoning: true },
  { provider: "kimi", id: "kimi-for-coding", name: "Kimi For Coding", api: "anthropic", baseUrl: "https://api.kimi.com/coding", contextWindow: 200000, maxTokens: 8192, input: ["text"], reasoning: true, headers: { "User-Agent": "KimiCLI/1.5" } },
  { provider: "zai", id: "glm-4.6", name: "GLM 4.6", api: "openai", baseUrl: "https://api.z.ai/api/coding/paas/v4", contextWindow: 128000, maxTokens: 8192, input: ["text"], reasoning: true },
  { provider: "gemini", id: "gemini-2.0-flash", name: "Gemini 2.0 Flash", api: "openai", baseUrl: "https://generativelanguage.googleapis.com/v1beta/openai", contextWindow: 1000000, maxTokens: 8192, input: ["text", "image"], reasoning: false },
];

const CONFIG_VALUE_CACHE = new Map();

function executeConfigCommand(commandConfig) {
  const command = String(commandConfig || "").slice(1);
  try {
    const output = childProcess.execSync(command, {
      encoding: "utf8",
      timeout: 10000,
      stdio: ["ignore", "pipe", "ignore"],
    });
    const value = String(output || "").trim();
    return value || undefined;
  } catch {
    return undefined;
  }
}

function resolveModelConfigValueUncached(value) {
  if (value === undefined || value === null) return undefined;
  const text = String(value);
  if (text.startsWith("!")) return executeConfigCommand(text);
  if (text.startsWith("$")) return process.env[text.slice(1)] || "";
  if (text.startsWith("env:")) return process.env[text.slice(4)] || "";
  return process.env[text] || text;
}

function resolveModelConfigValue(value) {
  if (value === undefined || value === null) return undefined;
  const text = String(value);
  if (!text.startsWith("!")) return resolveModelConfigValueUncached(text);
  if (CONFIG_VALUE_CACHE.has(text)) return CONFIG_VALUE_CACHE.get(text);
  const resolved = resolveModelConfigValueUncached(text);
  CONFIG_VALUE_CACHE.set(text, resolved);
  return resolved;
}

function normalizeHeaders(headers) {
  if (!headers || typeof headers !== "object" || Array.isArray(headers)) return undefined;
  const out = {};
  for (const [key, value] of Object.entries(headers)) if (value !== undefined && value !== null) out[String(key)] = String(resolveModelConfigValue(value));
  return Object.keys(out).length ? out : undefined;
}

function modelRequestKey(provider, modelId) {
  return `${provider}:${modelId}`;
}

function customModelEntry(provider, providerConfig = {}, modelDef = {}) {
  const id = String(modelDef.id || modelDef.name || providerConfig.defaultModel || provider).trim();
  if (!id) return null;
  const builtInDefault = BUILT_IN_MODELS.find((model) => model.provider === provider);
  const api = modelDef.api || providerConfig.api || builtInDefault?.api || "openai";
  const baseUrl = modelDef.baseUrl || providerConfig.baseUrl || builtInDefault?.baseUrl || "";
  return {
    id,
    name: String(modelDef.name || id),
    provider,
    api,
    baseUrl,
    reasoning: modelDef.reasoning ?? false,
    input: Array.isArray(modelDef.input) ? modelDef.input.slice() : ["text"],
    cost: modelDef.cost || { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
    contextWindow: Number(modelDef.contextWindow || modelDef.context_window || modelDef.maxContext || 128000),
    maxTokens: Number(modelDef.maxTokens || modelDef.max_tokens || 16384),
    headers: normalizeHeaders(modelDef.headers),
    compat: modelDef.compat || providerConfig.compat || {},
  };
}

class ModelRegistry {
  constructor(authStorage, modelsJsonPath = undefined) {
    this.authStorage = authStorage || AuthStorage.create();
    this.modelsJsonPath = modelsJsonPath ? expandBridgeTilde(modelsJsonPath) : undefined;
    this.models = [];
    this.providerRequestConfigs = new Map();
    this.modelRequestHeaders = new Map();
    this.registeredProviders = new Map();
    this.loadError = undefined;
    this.refresh();
  }

  static create(authStorage, modelsJsonPath = path.join(getAgentDir(), "models.json")) {
    return new ModelRegistry(authStorage, modelsJsonPath);
  }

  static inMemory(authStorage) {
    return new ModelRegistry(authStorage, undefined);
  }

  refresh() {
    this.providerRequestConfigs.clear();
    this.modelRequestHeaders.clear();
    this.loadError = undefined;
    this.models = BUILT_IN_MODELS.map((model) => ({ ...cloneJson(model) }));
    if (this.modelsJsonPath && fs.existsSync(this.modelsJsonPath)) this.loadModelsJson(this.modelsJsonPath);
    for (const [provider, config] of this.registeredProviders.entries()) this.applyProviderConfig(provider, config);
  }

  getError() {
    return this.loadError;
  }

  storeProviderRequestConfig(provider, config = {}) {
    const apiKey = config.apiKey || config.apiKeyEnvVar || config.apiKeyEnv || config.envKey;
    const headers = normalizeHeaders(config.headers);
    if (apiKey || headers || config.authHeader) {
      this.providerRequestConfigs.set(provider, { apiKey, headers, authHeader: !!config.authHeader });
    }
  }

  loadModelsJson(modelsJsonPath) {
    try {
      const text = fs.readFileSync(modelsJsonPath, "utf8");
      const parsed = text.trim() ? JSON.parse(text) : {};
      const providers = parsed.providers && typeof parsed.providers === "object" ? parsed.providers : {};
      for (const [provider, config] of Object.entries(providers)) {
        this.applyProviderConfig(String(provider), config || {});
      }
    } catch (error) {
      this.loadError = `Failed to load models.json: ${error && error.message ? error.message : String(error)}`;
    }
  }

  applyProviderConfig(provider, config = {}) {
    if (config.oauth && typeof config.oauth === "object") {
      registerBridgeOAuthProvider({ ...config.oauth, id: provider });
    }
    this.storeProviderRequestConfig(provider, config);
    const providerModels = Array.isArray(config.models) ? config.models : [];
    if (providerModels.length === 0) return;
    this.models = this.models.filter((model) => model.provider !== provider);
    for (const entry of providerModels) {
      const modelDef = typeof entry === "string" ? { id: entry } : entry || {};
      const model = customModelEntry(provider, config, modelDef);
      if (!model) continue;
      const modelHeaders = normalizeHeaders(modelDef.headers);
      if (modelHeaders) this.modelRequestHeaders.set(modelRequestKey(provider, model.id), modelHeaders);
      this.models.push(model);
    }
    if (config.oauth && typeof config.oauth.modifyModels === "function") {
      const credential = this.authStorage.get(provider);
      if (credential && credential.type === "oauth") this.models = config.oauth.modifyModels(this.models, credential);
    }
  }

  getAll() {
    return this.models.slice();
  }

  getAvailable() {
    return this.models.filter((model) => this.hasConfiguredAuth(model));
  }

  find(provider, modelId) {
    return this.models.find((model) => model.provider === provider && model.id === modelId);
  }

  hasConfiguredAuth(model) {
    return !!(model && (this.authStorage.hasAuth(model.provider) || this.providerRequestConfigs.get(model.provider)?.apiKey));
  }

  async getApiKeyForProvider(provider) {
    const authKey = await this.authStorage.getApiKey(provider, { includeFallback: false });
    if (authKey) return authKey;
    const configKey = this.providerRequestConfigs.get(provider)?.apiKey;
    return configKey ? resolveModelConfigValueUncached(configKey) : undefined;
  }

  async getApiKeyAndHeaders(model) {
    if (!model) return { ok: false, error: "model is required" };
    try {
      const providerConfig = this.providerRequestConfigs.get(model.provider) || {};
      const apiKey = await this.getApiKeyForProvider(model.provider);
      const providerHeaders = normalizeHeaders(providerConfig.headers);
      const modelHeaders = normalizeHeaders(this.modelRequestHeaders.get(modelRequestKey(model.provider, model.id)));
      let headers = { ...(model.headers || {}), ...(providerHeaders || {}), ...(modelHeaders || {}) };
      if (providerConfig.authHeader) {
        if (!apiKey) return { ok: false, error: `No API key found for "${model.provider}"` };
        headers = { ...headers, Authorization: `Bearer ${apiKey}` };
      }
      return { ok: true, apiKey, headers: Object.keys(headers).length ? headers : undefined };
    } catch (error) {
      return { ok: false, error: error && error.message ? error.message : String(error) };
    }
  }

  getProviderAuthStatus(provider) {
    const authStatus = this.authStorage.getAuthStatus(provider);
    if (authStatus.source) return authStatus;
    const apiKey = this.providerRequestConfigs.get(provider)?.apiKey;
    if (!apiKey) return authStatus;
    if (String(apiKey).startsWith("!")) return { configured: true, source: "models_json_command" };
    if (process.env[String(apiKey)]) return { configured: true, source: "environment", label: String(apiKey) };
    return { configured: true, source: "models_json_key" };
  }

  getProviderDisplayName(provider) {
    const registered = this.registeredProviders.get(provider);
    const oauthProvider = this.authStorage.getOAuthProviders().find((item) => item.id === provider);
    return String((registered && (registered.name || registered.displayName || (registered.oauth && registered.oauth.name))) || (oauthProvider && oauthProvider.name) || BUILT_IN_PROVIDER_DISPLAY_NAMES[provider] || provider || "");
  }

  isUsingOAuth(model) {
    const credential = model ? this.authStorage.get(model.provider) : undefined;
    return !!(credential && credential.type === "oauth");
  }

  getAuthCredential(model) {
    return model ? this.authStorage.get(model.provider) : undefined;
  }

  registerProvider(name, config = {}) {
    const provider = String(name || config.name || config.id || config.provider || "").trim();
    if (!provider) return;
    this.registeredProviders.set(provider, { ...config, name: config.displayName || config.name || provider });
    this.models = this.models.filter((model) => model.provider !== provider);
    this.applyProviderConfig(provider, {
      ...config,
      apiKey: config.apiKey || config.apiKeyEnvVar || config.apiKeyEnv || config.envKey,
      models: Array.isArray(config.models) && config.models.length ? config.models : [config.defaultModel || provider],
    });
  }

  unregisterProvider(provider) {
    const key = String(provider || "").trim();
    this.registeredProviders.delete(key);
    this.providerRequestConfigs.delete(key);
    this.models = this.models.filter((model) => model.provider !== key);
    unregisterBridgeOAuthProvider(key);
  }
}

function createSyntheticSourceInfo(filePath, options = {}) {
  return {
    path: String(filePath || ""),
    source: String(options.source || ""),
    scope: options.scope || "temporary",
    origin: options.origin || "top-level",
    baseDir: options.baseDir,
  };
}

function normalizeFrontmatterNewlines(value) {
  return String(value || "").replace(/\r\n/g, "\n").replace(/\r/g, "\n");
}

function parseYamlScalar(value, line, lineNumber) {
  const trimmed = String(value || "").trim();
  if ((trimmed.startsWith("[") && !trimmed.endsWith("]")) || (trimmed.startsWith("{") && !trimmed.endsWith("}"))) {
    throw new Error(`YAML parse error at line ${lineNumber}, column ${String(line || "").length + 1}`);
  }
  if ((trimmed.startsWith('"') && trimmed.endsWith('"')) || (trimmed.startsWith("'") && trimmed.endsWith("'"))) return trimmed.slice(1, -1);
  if (/^(true|false)$/i.test(trimmed)) return /^true$/i.test(trimmed);
  if (/^-?\d+$/.test(trimmed)) {
    const intValue = Number.parseInt(trimmed, 10);
    if (Number.isSafeInteger(intValue)) return intValue;
  }
  if (/^-?(?:\d+\.\d*|\d*\.\d+)$/.test(trimmed)) {
    const floatValue = Number.parseFloat(trimmed);
    if (Number.isFinite(floatValue)) return floatValue;
  }
  return trimmed;
}

function parseSimpleYamlFrontmatter(yamlString) {
  const data = {};
  const lines = normalizeFrontmatterNewlines(yamlString).split("\n");
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    const trimmed = line.trim();
    if (!trimmed || (trimmed.startsWith("#") && !trimmed.startsWith("\\#"))) continue;
    const match = /^([^:#][^:]*):(?:\s*(.*))?$/.exec(line);
    if (!match) throw new Error(`YAML parse error at line ${i + 1}, column ${line.length + 1}`);
    const key = match[1].trim();
    const value = match[2] === undefined ? "" : match[2];
    if (value.trim() === "|") {
      const block = [];
      while (i + 1 < lines.length) {
        const next = lines[i + 1];
        if (next.trim() !== "" && !/^\s/.test(next)) break;
        i += 1;
        block.push(next.startsWith("  ") ? next.slice(2) : next.replace(/^\s/, ""));
      }
      data[key] = `${block.join("\n")}\n`;
    } else {
      data[key] = parseYamlScalar(value, line, i + 1);
    }
  }
  return data;
}

function parseFrontmatter(markdown) {
  const text = normalizeFrontmatterNewlines(markdown);
  if (!text.startsWith("---")) return { data: {}, frontmatter: {}, body: text };
  const end = text.indexOf("\n---", 3);
  if (end < 0) return { data: {}, frontmatter: {}, body: text };
  const yamlString = text.slice(4, end);
  const data = parseSimpleYamlFrontmatter(yamlString);
  const body = text.slice(end + 4).trim();
  return { data, frontmatter: data, body };
}

function skillNameFromPath(filePath) {
  return path.basename(path.dirname(filePath));
}

function createSkillSourceInfo(filePath, baseDir, source) {
  if (source === "user") return createSyntheticSourceInfo(filePath, { source: "local", scope: "user", baseDir });
  if (source === "project") return createSyntheticSourceInfo(filePath, { source: "local", scope: "project", baseDir });
  if (source === "path") return createSyntheticSourceInfo(filePath, { source: "local", scope: "temporary", baseDir });
  return createSyntheticSourceInfo(filePath, { source, baseDir });
}

function validateSkillName(name) {
  const errors = [];
  if (String(name || "").length > 64) errors.push(`name exceeds 64 characters (${String(name || "").length})`);
  if (!/^[a-z0-9-]+$/.test(String(name || ""))) errors.push("name contains invalid characters (must be lowercase a-z, 0-9, hyphens only)");
  if (String(name || "").startsWith("-") || String(name || "").endsWith("-")) errors.push("name must not start or end with a hyphen");
  if (String(name || "").includes("--")) errors.push("name must not contain consecutive hyphens");
  return errors;
}

function loadSkillFileResult(filePath, source = "path", baseDir = path.dirname(filePath)) {
  const diagnostics = [];
  try {
    const markdown = fs.readFileSync(filePath, "utf8");
    const parsed = parseFrontmatter(markdown);
    const name = String(parsed.data.name || skillNameFromPath(filePath)).trim();
    const description = String(parsed.data.description || "").trim();
    if (!description) diagnostics.push({ type: "warning", message: "description is required", path: filePath });
    for (const error of validateSkillName(name)) diagnostics.push({ type: "warning", message: error, path: filePath });
    if (!name || !description) return { skill: null, diagnostics };
    return {
      skill: {
        name,
        description,
        filePath,
        baseDir,
        sourceInfo: createSkillSourceInfo(filePath, baseDir, source),
        disableModelInvocation: parsed.data["disable-model-invocation"] === true,
      },
      diagnostics,
    };
  } catch (error) {
    diagnostics.push({ type: "warning", message: error && error.message ? error.message : "failed to parse skill file", path: filePath });
    return { skill: null, diagnostics };
  }
}

function loadSkillFile(filePath, source = "path", baseDir = path.dirname(filePath)) {
  return loadSkillFileResult(filePath, source, baseDir).skill;
}

const SKILL_IGNORE_FILE_NAMES = [".gitignore", ".ignore", ".fdignore"];

function skillIgnorePatterns(dir, rootDir) {
  const patterns = [];
  const relativeDir = bridgePosix(path.relative(rootDir, dir));
  const prefix = relativeDir ? `${relativeDir}/` : "";
  for (const filename of SKILL_IGNORE_FILE_NAMES) {
    const ignorePath = path.join(dir, filename);
    if (!fs.existsSync(ignorePath)) continue;
    try {
      const content = fs.readFileSync(ignorePath, "utf8");
      for (const rawLine of content.split(/\r?\n/)) {
        const trimmed = rawLine.trim();
        if (!trimmed || (trimmed.startsWith("#") && !trimmed.startsWith("\\#"))) continue;
        let pattern = trimmed.startsWith("\\!") ? trimmed.slice(1) : trimmed;
        if (pattern.startsWith("!")) continue;
        if (pattern.startsWith("/")) pattern = pattern.slice(1);
        patterns.push(`${prefix}${pattern}`);
      }
    } catch {}
  }
  return patterns;
}

function skillPatternMatches(pattern, relativePath) {
  const normalizedPattern = bridgePosix(pattern).replace(/^\.\//, "");
  const normalizedPath = bridgePosix(relativePath).replace(/^\.\//, "");
  if (!normalizedPattern) return false;
  if (normalizedPattern.endsWith("/")) return normalizedPath.startsWith(normalizedPattern);
  if (!/[*?]/.test(normalizedPattern)) return normalizedPath === normalizedPattern || normalizedPath.startsWith(`${normalizedPattern}/`);
  return packageGlobToRegExp(normalizedPattern).test(normalizedPath);
}

function skillPathIgnored(patterns, rootDir, filePath, isDirectory) {
  const relativePath = bridgePosix(path.relative(rootDir, filePath)) + (isDirectory ? "/" : "");
  return patterns.some((pattern) => skillPatternMatches(pattern, relativePath));
}

function loadSkillsFromDir(options = {}) {
  const dir = expandBridgeTilde(options.dir || "");
  const source = options.source || "path";
  const rootDir = dir;
  const visit = (current, includeRootFiles, inheritedPatterns = []) => {
    const skills = [];
    const diagnostics = [];
    if (!current || !fs.existsSync(current)) return { skills, diagnostics };
    const localPatterns = [...inheritedPatterns, ...skillIgnorePatterns(current, rootDir)];
    let entries = [];
    try {
      entries = fs.readdirSync(current, { withFileTypes: true });
    } catch (error) {
      diagnostics.push({ type: "warning", message: error.message, path: current });
      return { skills, diagnostics };
    }
    const rootSkill = entries.find((entry) => entry.name === "SKILL.md");
    if (rootSkill) {
      const filePath = path.join(current, rootSkill.name);
      let isFile = rootSkill.isFile();
      if (rootSkill.isSymbolicLink()) {
        try { isFile = fs.statSync(filePath).isFile(); } catch { isFile = false; }
      }
      if (isFile && !skillPathIgnored(localPatterns, rootDir, filePath, false)) {
        const result = loadSkillFileResult(filePath, source, path.dirname(filePath));
        if (result.skill) skills.push(result.skill);
        diagnostics.push(...result.diagnostics);
      }
      return { skills, diagnostics };
    }
    for (const entry of entries) {
      const filePath = path.join(current, entry.name);
      if (entry.name.startsWith(".") || entry.name === "node_modules") continue;
      let isDirectory = entry.isDirectory();
      let isFile = entry.isFile();
      if (entry.isSymbolicLink()) {
        try {
          const stats = fs.statSync(filePath);
          isDirectory = stats.isDirectory();
          isFile = stats.isFile();
        } catch {
          continue;
        }
      }
      if (skillPathIgnored(localPatterns, rootDir, filePath, isDirectory)) continue;
      if (isDirectory) {
        const result = visit(filePath, false, localPatterns);
        skills.push(...result.skills);
        diagnostics.push(...result.diagnostics);
      } else if (isFile && includeRootFiles && entry.name.endsWith(".md")) {
        const result = loadSkillFileResult(filePath, source, path.dirname(filePath));
        if (result.skill) skills.push(result.skill);
        diagnostics.push(...result.diagnostics);
      }
    }
    return { skills, diagnostics };
  };
  return visit(dir, true, []);
}

function loadSkills(options = {}) {
  const cwd = options.cwd || process.cwd();
  const agentDir = options.agentDir || getAgentDir();
  const includeDefaults = options.includeDefaults !== false;
  const skillPaths = Array.isArray(options.skillPaths) ? options.skillPaths : [];
  const byName = new Map();
  const realPaths = new Set();
  const diagnostics = [];
  const canonical = (filePath) => {
    try { return fs.realpathSync(filePath); } catch { return path.resolve(filePath); }
  };
  const add = (result) => {
    diagnostics.push(...(result.diagnostics || []));
    for (const skill of result.skills || []) {
      const realPath = canonical(skill.filePath);
      if (realPaths.has(realPath)) continue;
      const existing = byName.get(skill.name);
      if (existing) {
        diagnostics.push({
          type: "collision",
          message: `name "${skill.name}" collision`,
          path: skill.filePath,
          collision: { resourceType: "skill", name: skill.name, winnerPath: existing.filePath, loserPath: skill.filePath },
        });
      } else {
        byName.set(skill.name, skill);
        realPaths.add(realPath);
      }
    }
  };
  if (includeDefaults) {
    add(loadSkillsFromDir({ dir: path.join(agentDir, "skills"), source: "user" }));
    add(loadSkillsFromDir({ dir: path.join(cwd, ".pi", "skills"), source: "project" }));
  }
  for (const raw of skillPaths) {
    const resolved = resolveBridgePath(cwd, String(raw));
    if (!fs.existsSync(resolved)) {
      diagnostics.push({ type: "warning", message: "skill path does not exist", path: resolved });
      continue;
    }
    const stats = fs.statSync(resolved);
    if (stats.isDirectory()) add(loadSkillsFromDir({ dir: resolved, source: "path" }));
    else if (stats.isFile()) {
      const result = loadSkillFileResult(resolved, "path", path.dirname(resolved));
      add({ skills: result.skill ? [result.skill] : [], diagnostics: result.diagnostics });
    }
  }
  return { skills: Array.from(byName.values()), diagnostics };
}

function escapeXml(text) {
  return String(text || "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&apos;");
}

function formatSkillsForPrompt(skills = []) {
  const visible = (skills || []).filter((skill) => !skill.disableModelInvocation);
  if (visible.length === 0) return "";
  const lines = [
    "",
    "",
    "The following skills provide specialized instructions for specific tasks.",
    "Use the read tool to load a skill's file when the task matches its description.",
    "When a skill file references a relative path, resolve it against the skill directory (parent of SKILL.md / dirname of the path) and use that absolute path in tool commands.",
    "",
    "<available_skills>",
  ];
  for (const skill of visible) {
    lines.push("  <skill>");
    lines.push(`    <name>${escapeXml(skill.name)}</name>`);
    lines.push(`    <description>${escapeXml(skill.description)}</description>`);
    lines.push(`    <location>${escapeXml(skill.filePath)}</location>`);
    lines.push("  </skill>");
  }
  lines.push("</available_skills>");
  return lines.join("\n");
}

function stripFrontmatter(markdown) {
  return parseFrontmatter(markdown).body;
}

const DEFAULT_COMPACTION_SETTINGS = Object.freeze({
  enabled: true,
  reserveTokens: 16384,
  keepRecentTokens: 20000,
});

const TOOL_RESULT_MAX_CHARS = 2000;
const SUMMARIZATION_SYSTEM_PROMPT = "You are a context summarization assistant. Your task is to read a conversation between a user and an AI coding assistant, then produce a structured summary following the exact format specified.\n\nDo NOT continue the conversation. Do NOT respond to any questions in the conversation. ONLY output the structured summary.";
const SUMMARIZATION_PROMPT = "The messages above are a conversation to summarize. Create a structured context checkpoint summary that another LLM will use to continue the work.\n\nUse this EXACT format:\n\n## Goal\n[What is the user trying to accomplish? Can be multiple items if the session covers different tasks.]\n\n## Constraints & Preferences\n- [Any constraints, preferences, or requirements mentioned by user]\n- [Or \"(none)\" if none were mentioned]\n\n## Progress\n### Done\n- [x] [Completed tasks/changes]\n\n### In Progress\n- [ ] [Current work]\n\n### Blocked\n- [Issues preventing progress, if any]\n\n## Key Decisions\n- **[Decision]**: [Brief rationale]\n\n## Next Steps\n1. [Ordered list of what should happen next]\n\n## Critical Context\n- [Any data, examples, or references needed to continue]\n- [Or \"(none)\" if not applicable]\n\nKeep each section concise. Preserve exact file paths, function names, and error messages.";
const UPDATE_SUMMARIZATION_PROMPT = "The messages above are NEW conversation messages to incorporate into the existing summary provided in <previous-summary> tags.\n\nUpdate the existing structured summary with new information. RULES:\n- PRESERVE all existing information from the previous summary\n- ADD new progress, decisions, and context from the new messages\n- UPDATE the Progress section: move items from \"In Progress\" to \"Done\" when completed\n- UPDATE \"Next Steps\" based on what was accomplished\n- PRESERVE exact file paths, function names, and error messages\n- If something is no longer relevant, you may remove it\n\nUse this EXACT format:\n\n## Goal\n[Preserve existing goals, add new ones if the task expanded]\n\n## Constraints & Preferences\n- [Preserve existing, add new ones discovered]\n\n## Progress\n### Done\n- [x] [Include previously done items AND newly completed items]\n\n### In Progress\n- [ ] [Current work - update based on progress]\n\n### Blocked\n- [Current blockers - remove if resolved]\n\n## Key Decisions\n- **[Decision]**: [Brief rationale] (preserve all previous, add new)\n\n## Next Steps\n1. [Update based on current state]\n\n## Critical Context\n- [Preserve important context, add new if needed]\n\nKeep each section concise. Preserve exact file paths, function names, and error messages.";
const TURN_PREFIX_SUMMARIZATION_PROMPT = "This is the PREFIX of a turn that was too large to keep. The SUFFIX (recent work) is retained.\n\nSummarize the prefix to provide context for the retained suffix:\n\n## Original Request\n[What did the user ask for in this turn?]\n\n## Early Progress\n- [Key decisions and work done in the prefix]\n\n## Context for Suffix\n- [Information needed to understand the retained recent work]\n\nBe concise. Focus on what's needed to understand the kept suffix.";
const BRANCH_SUMMARY_PREAMBLE = "The user explored a different conversation branch before returning here.\nSummary of that exploration:\n\n";
const BRANCH_SUMMARY_PROMPT = "Create a structured summary of this conversation branch for context when returning later.\n\nUse this EXACT format:\n\n## Goal\n[What was the user trying to accomplish in this branch?]\n\n## Constraints & Preferences\n- [Any constraints, preferences, or requirements mentioned]\n- [Or \"(none)\" if none were mentioned]\n\n## Progress\n### Done\n- [x] [Completed tasks/changes]\n\n### In Progress\n- [ ] [Work that was started but not finished]\n\n### Blocked\n- [Issues preventing progress, if any]\n\n## Key Decisions\n- **[Decision]**: [Brief rationale]\n\n## Next Steps\n1. [What should happen next to continue this work]\n\nKeep each section concise. Preserve exact file paths, function names, and error messages.";

function usageNumber(usage, keys) {
  if (!usage || typeof usage !== "object") return 0;
  for (const key of keys) {
    const value = usage[key];
    if (typeof value === "number" && Number.isFinite(value)) return value;
  }
  return 0;
}

function calculateContextTokens(usage = {}) {
  const total = usageNumber(usage, ["totalTokens", "total_tokens", "total"]);
  if (total > 0) return total;
  return usageNumber(usage, ["input", "inputTokens", "input_tokens", "promptTokens", "prompt_tokens"])
    + usageNumber(usage, ["output", "outputTokens", "output_tokens", "completionTokens", "completion_tokens"])
    + usageNumber(usage, ["cacheRead", "cacheReadTokens", "cache_read", "cache_read_tokens"])
    + usageNumber(usage, ["cacheWrite", "cacheWriteTokens", "cache_write", "cache_write_tokens"]);
}

function entryMessage(entry) {
  if (!entry || typeof entry !== "object") return undefined;
  if (entry.type === "message" && entry.message) return entry.message;
  if (entry.role) return entry;
  return undefined;
}

function assistantUsage(message) {
  if (!message || message.role !== "assistant" || !message.usage) return undefined;
  if (message.stopReason === "aborted" || message.stopReason === "error") return undefined;
  return message.usage;
}

function getLastAssistantUsage(entries = []) {
  for (let i = entries.length - 1; i >= 0; i--) {
    const usage = assistantUsage(entryMessage(entries[i]));
    if (usage) return usage;
  }
  return undefined;
}

function blocksOf(content) {
  if (Array.isArray(content)) return content;
  if (typeof content === "string") return [{ type: "text", text: content }];
  return [];
}

function contentTextLength(content, includeImages = false) {
  if (typeof content === "string") return content.length;
  let chars = 0;
  for (const block of blocksOf(content)) {
    if (!block || typeof block !== "object") continue;
    if (block.type === "text" && block.text) chars += String(block.text).length;
    else if (block.type === "thinking" && block.thinking) chars += String(block.thinking).length;
    else if (block.type === "image" && includeImages) chars += 4800;
  }
  return chars;
}

function estimateTokens(message = {}) {
  const msg = entryMessage(message) || message || {};
  let chars = 0;
  switch (msg.role) {
    case "user":
      chars = contentTextLength(msg.content);
      break;
    case "assistant":
      for (const block of blocksOf(msg.content)) {
        if (!block || typeof block !== "object") continue;
        if (block.type === "text" && block.text) chars += String(block.text).length;
        else if (block.type === "thinking" && block.thinking) chars += String(block.thinking).length;
        else if (block.type === "toolCall" || block.type === "tool_use") {
          const name = block.name || block.toolName || "";
          const args = block.arguments !== undefined ? block.arguments : block.input;
          chars += String(name).length + JSON.stringify(args || {}).length;
        }
      }
      break;
    case "custom":
    case "toolResult":
    case "tool_result":
      chars = contentTextLength(msg.content, true);
      break;
    case "bashExecution":
      chars = String(msg.command || "").length + String(msg.output || "").length;
      break;
    case "branchSummary":
    case "compactionSummary":
      chars = String(msg.summary || "").length;
      break;
    default:
      chars = contentTextLength(msg.content, true);
      break;
  }
  return Math.ceil(chars / 4);
}

function estimateContextTokens(messages = []) {
  let usageInfo;
  for (let i = messages.length - 1; i >= 0; i--) {
    const usage = assistantUsage(messages[i]);
    if (usage) {
      usageInfo = { usage, index: i };
      break;
    }
  }
  if (!usageInfo) {
    const estimated = messages.reduce((sum, message) => sum + estimateTokens(message), 0);
    return { tokens: estimated, usageTokens: 0, trailingTokens: estimated, lastUsageIndex: null };
  }
  const usageTokens = calculateContextTokens(usageInfo.usage);
  let trailingTokens = 0;
  for (let i = usageInfo.index + 1; i < messages.length; i++) trailingTokens += estimateTokens(messages[i]);
  return { tokens: usageTokens + trailingTokens, usageTokens, trailingTokens, lastUsageIndex: usageInfo.index };
}

function shouldCompact(contextTokens, contextWindow, settings = DEFAULT_COMPACTION_SETTINGS) {
  const merged = { ...DEFAULT_COMPACTION_SETTINGS, ...(settings || {}) };
  if (!merged.enabled) return false;
  return Number(contextTokens || 0) > Number(contextWindow || 0) - Number(merged.reserveTokens || 0);
}

function truncateForSummary(text, maxChars) {
  const value = String(text || "");
  if (value.length <= maxChars) return value;
  return `${value.slice(0, maxChars)}\n\n[... ${value.length - maxChars} more characters truncated]`;
}

function serializeConversation(messages = []) {
  const parts = [];
  for (const msg of messages) {
    if (!msg || typeof msg !== "object") continue;
    if (msg.role === "user") {
      const content = blocksOf(msg.content).filter((block) => block.type === "text").map((block) => block.text || "").join("");
      if (content) parts.push(`[User]: ${content}`);
    } else if (msg.role === "assistant") {
      const textParts = [];
      const thinkingParts = [];
      const toolCalls = [];
      for (const block of blocksOf(msg.content)) {
        if (!block || typeof block !== "object") continue;
        if (block.type === "text" && block.text) textParts.push(String(block.text));
        else if (block.type === "thinking" && block.thinking) thinkingParts.push(String(block.thinking));
        else if (block.type === "toolCall" || block.type === "tool_use") {
          const args = block.arguments !== undefined ? block.arguments : block.input || {};
          const argsText = Object.entries(args || {}).map(([key, value]) => `${key}=${JSON.stringify(value)}`).join(", ");
          toolCalls.push(`${block.name || block.toolName || ""}(${argsText})`);
        }
      }
      if (thinkingParts.length) parts.push(`[Assistant thinking]: ${thinkingParts.join("\n")}`);
      if (textParts.length) parts.push(`[Assistant]: ${textParts.join("\n")}`);
      if (toolCalls.length) parts.push(`[Assistant tool calls]: ${toolCalls.join("; ")}`);
    } else if (msg.role === "toolResult" || msg.role === "tool_result") {
      const content = blocksOf(msg.content).filter((block) => block.type === "text").map((block) => block.text || "").join("");
      if (content) parts.push(`[Tool result]: ${truncateForSummary(content, TOOL_RESULT_MAX_CHARS)}`);
    } else if (msg.role === "custom" && msg.content) {
      parts.push(`[Custom]: ${typeof msg.content === "string" ? msg.content : JSON.stringify(msg.content)}`);
    } else if (msg.role === "bashExecution") {
      parts.push(`[Bash]: ${msg.command || ""}\n${truncateForSummary(msg.output || "", TOOL_RESULT_MAX_CHARS)}`);
    } else if ((msg.role === "branchSummary" || msg.role === "compactionSummary") && msg.summary) {
      parts.push(`[Summary]: ${msg.summary}`);
    }
  }
  return parts.join("\n\n");
}

function entryToMessage(entry, includeToolResults = true) {
  if (!entry || typeof entry !== "object") return undefined;
  if (entry.type === "message") {
    if (!includeToolResults && entry.message && (entry.message.role === "toolResult" || entry.message.role === "tool_result")) return undefined;
    return entry.message;
  }
  if (entry.type === "custom_message") return { role: "custom", content: entry.content, timestamp: entry.timestamp };
  if (entry.type === "branch_summary") return { role: "branchSummary", summary: entry.summary || "", timestamp: entry.timestamp };
  if (entry.type === "compaction") return { role: "compactionSummary", summary: entry.summary || "", timestamp: entry.timestamp };
  return undefined;
}

function extractFileOpsFromMessage(message, fileOps) {
  if (!message || message.role !== "assistant") return;
  for (const block of blocksOf(message.content)) {
    if (!block || typeof block !== "object") continue;
    if (block.type !== "toolCall" && block.type !== "tool_use") continue;
    const args = block.arguments !== undefined ? block.arguments : block.input || {};
    const filePath = typeof args.path === "string" ? args.path : undefined;
    if (!filePath) continue;
    const name = block.name || block.toolName;
    if (name === "read") fileOps.read.add(filePath);
    else if (name === "write") fileOps.written.add(filePath);
    else if (name === "edit") fileOps.edited.add(filePath);
  }
}

function createFileOps() {
  return { read: new Set(), written: new Set(), edited: new Set() };
}

function computeFileLists(fileOps) {
  const modified = new Set([...(fileOps.edited || []), ...(fileOps.written || [])]);
  return {
    readFiles: [...(fileOps.read || [])].filter((filePath) => !modified.has(filePath)).sort(),
    modifiedFiles: [...modified].sort(),
  };
}

function formatFileOperations(readFiles = [], modifiedFiles = []) {
  const sections = [];
  if (readFiles.length) sections.push(`<read-files>\n${readFiles.join("\n")}\n</read-files>`);
  if (modifiedFiles.length) sections.push(`<modified-files>\n${modifiedFiles.join("\n")}\n</modified-files>`);
  return sections.length ? `\n\n${sections.join("\n\n")}` : "";
}

function validCutPoints(entries, startIndex, endIndex) {
  const cutPoints = [];
  for (let i = startIndex; i < endIndex; i++) {
    const entry = entries[i];
    if (!entry || typeof entry !== "object") continue;
    if (entry.type === "branch_summary" || entry.type === "custom_message") {
      cutPoints.push(i);
      continue;
    }
    if (entry.type !== "message" || !entry.message) continue;
    const role = entry.message.role;
    if (role === "toolResult" || role === "tool_result") continue;
    if (["bashExecution", "custom", "branchSummary", "compactionSummary", "user", "assistant"].includes(role)) {
      cutPoints.push(i);
    }
  }
  return cutPoints;
}

function findTurnStartIndex(entries = [], entryIndex, startIndex = 0) {
  for (let i = Number(entryIndex); i >= Number(startIndex || 0); i--) {
    const entry = entries[i];
    if (!entry || typeof entry !== "object") continue;
    if (entry.type === "branch_summary" || entry.type === "custom_message") return i;
    if (entry.type === "message" && entry.message && (entry.message.role === "user" || entry.message.role === "bashExecution")) return i;
  }
  return -1;
}

function findCutPoint(entries = [], startIndex = 0, endIndex = entries.length, keepRecentTokens = DEFAULT_COMPACTION_SETTINGS.keepRecentTokens) {
  const cutPoints = validCutPoints(entries, startIndex, endIndex);
  if (!cutPoints.length) return { firstKeptEntryIndex: startIndex, turnStartIndex: -1, isSplitTurn: false };
  let accumulatedTokens = 0;
  let cutIndex = cutPoints[0];
  for (let i = endIndex - 1; i >= startIndex; i--) {
    const entry = entries[i];
    if (!entry || entry.type !== "message") continue;
    accumulatedTokens += estimateTokens(entry.message);
    if (accumulatedTokens >= keepRecentTokens) {
      const point = cutPoints.find((candidate) => candidate >= i);
      cutIndex = point === undefined ? cutIndex : point;
      break;
    }
  }
  while (cutIndex > startIndex) {
    const prev = entries[cutIndex - 1];
    if (!prev || prev.type === "compaction" || prev.type === "message") break;
    cutIndex--;
  }
  const cutEntry = entries[cutIndex];
  const isUserMessage = cutEntry && cutEntry.type === "message" && cutEntry.message && cutEntry.message.role === "user";
  const turnStartIndex = isUserMessage ? -1 : findTurnStartIndex(entries, cutIndex, startIndex);
  return { firstKeptEntryIndex: cutIndex, turnStartIndex, isSplitTurn: !isUserMessage && turnStartIndex !== -1 };
}

function collectEntriesForBranchSummary(session, oldLeafId, targetId) {
  if (!oldLeafId) return { entries: [], commonAncestorId: null };
  const oldBranch = typeof session.getBranch === "function" ? session.getBranch(oldLeafId) : [];
  const targetBranch = typeof session.getBranch === "function" ? session.getBranch(targetId) : [];
  const oldIds = new Set(oldBranch.map((entry) => entry && entry.id).filter(Boolean));
  let commonAncestorId = null;
  for (let i = targetBranch.length - 1; i >= 0; i--) {
    if (targetBranch[i] && oldIds.has(targetBranch[i].id)) {
      commonAncestorId = targetBranch[i].id;
      break;
    }
  }
  const entries = [];
  let current = oldLeafId;
  while (current && current !== commonAncestorId) {
    const entry = typeof session.getEntry === "function" ? session.getEntry(current) : undefined;
    if (!entry) break;
    entries.push(entry);
    current = entry.parentId;
  }
  entries.reverse();
  return { entries, commonAncestorId };
}

function prepareBranchEntries(entries = [], tokenBudget = 0) {
  const messages = [];
  const fileOps = createFileOps();
  let totalTokens = 0;
  for (const entry of entries) {
    if (entry && entry.type === "branch_summary" && !entry.fromHook && entry.details) {
      if (Array.isArray(entry.details.readFiles)) for (const filePath of entry.details.readFiles) fileOps.read.add(filePath);
      if (Array.isArray(entry.details.modifiedFiles)) for (const filePath of entry.details.modifiedFiles) fileOps.edited.add(filePath);
    }
  }
  for (let i = entries.length - 1; i >= 0; i--) {
    const entry = entries[i];
    const message = entryToMessage(entry, false);
    if (!message) continue;
    extractFileOpsFromMessage(message, fileOps);
    const tokens = estimateTokens(message);
    if (tokenBudget > 0 && totalTokens + tokens > tokenBudget) {
      if ((entry.type === "compaction" || entry.type === "branch_summary") && totalTokens < tokenBudget * 0.9) {
        messages.unshift(message);
        totalTokens += tokens;
      }
      break;
    }
    messages.unshift(message);
    totalTokens += tokens;
  }
  return { messages, fileOps, totalTokens };
}

function buildSummaryMessages(conversationText, promptText, previousSummary) {
  let text = `<conversation>\n${conversationText}\n</conversation>\n\n`;
  if (previousSummary) text += `<previous-summary>\n${previousSummary}\n</previous-summary>\n\n`;
  text += promptText;
  return [{ role: "user", content: [{ type: "text", text }], timestamp: Date.now() }];
}

async function completeBridgeSummary(model, context, options, streamFn) {
  if (typeof streamFn === "function") {
    const stream = await streamFn(model, context, options);
    if (stream && typeof stream.result === "function") return await stream.result();
    return stream;
  }
  if (model && typeof model.completeSimple === "function") return await model.completeSimple(context, options);
  if (model && typeof model.complete === "function") return await model.complete(context, options);
  throw new Error("Summarization requires a stream function or model completion callback");
}

function responseText(response) {
  if (typeof response === "string") return response;
  if (!response || typeof response !== "object") return "";
  if (response.stopReason === "error") throw new Error(`Summarization failed: ${response.errorMessage || "Unknown error"}`);
  return blocksOf(response.content).filter((block) => block.type === "text").map((block) => block.text || "").join("\n");
}

async function generateSummary(currentMessages, model, reserveTokens, apiKey, headers, signal, customInstructions, previousSummary, thinkingLevel, streamFn) {
  const maxTokens = Math.min(Math.floor(0.8 * Number(reserveTokens || DEFAULT_COMPACTION_SETTINGS.reserveTokens)), model && model.maxTokens > 0 ? model.maxTokens : Number.POSITIVE_INFINITY);
  let basePrompt = previousSummary ? UPDATE_SUMMARIZATION_PROMPT : SUMMARIZATION_PROMPT;
  if (customInstructions) basePrompt = `${basePrompt}\n\nAdditional focus: ${customInstructions}`;
  const context = {
    systemPrompt: SUMMARIZATION_SYSTEM_PROMPT,
    messages: buildSummaryMessages(serializeConversation(currentMessages || []), basePrompt, previousSummary),
  };
  const options = { maxTokens, apiKey, headers, signal };
  if (model && model.reasoning && thinkingLevel && thinkingLevel !== "off") options.reasoning = thinkingLevel;
  return responseText(await completeBridgeSummary(model, context, options, streamFn));
}

async function generateTurnPrefixSummary(messages, model, reserveTokens, apiKey, headers, signal, thinkingLevel, streamFn) {
  const maxTokens = Math.min(Math.floor(0.5 * Number(reserveTokens || DEFAULT_COMPACTION_SETTINGS.reserveTokens)), model && model.maxTokens > 0 ? model.maxTokens : Number.POSITIVE_INFINITY);
  const context = {
    systemPrompt: SUMMARIZATION_SYSTEM_PROMPT,
    messages: buildSummaryMessages(serializeConversation(messages || []), TURN_PREFIX_SUMMARIZATION_PROMPT),
  };
  const options = { maxTokens, apiKey, headers, signal };
  if (model && model.reasoning && thinkingLevel && thinkingLevel !== "off") options.reasoning = thinkingLevel;
  return responseText(await completeBridgeSummary(model, context, options, streamFn));
}

async function compact(preparation, model, apiKey, headers, customInstructions, signal, thinkingLevel, streamFn) {
  if (!preparation || typeof preparation !== "object") throw new Error("Compaction preparation is required");
  const settings = { ...DEFAULT_COMPACTION_SETTINGS, ...(preparation.settings || {}) };
  let summary;
  if (preparation.isSplitTurn && Array.isArray(preparation.turnPrefixMessages) && preparation.turnPrefixMessages.length) {
    const historyPromise = Array.isArray(preparation.messagesToSummarize) && preparation.messagesToSummarize.length
      ? generateSummary(preparation.messagesToSummarize, model, settings.reserveTokens, apiKey, headers, signal, customInstructions, preparation.previousSummary, thinkingLevel, streamFn)
      : Promise.resolve("No prior history.");
    const [historyResult, turnPrefixResult] = await Promise.all([
      historyPromise,
      generateTurnPrefixSummary(preparation.turnPrefixMessages, model, settings.reserveTokens, apiKey, headers, signal, thinkingLevel, streamFn),
    ]);
    summary = `${historyResult}\n\n---\n\n**Turn Context (split turn):**\n\n${turnPrefixResult}`;
  } else {
    summary = await generateSummary(preparation.messagesToSummarize || [], model, settings.reserveTokens, apiKey, headers, signal, customInstructions, preparation.previousSummary, thinkingLevel, streamFn);
  }
  const lists = computeFileLists(preparation.fileOps || createFileOps());
  summary += formatFileOperations(lists.readFiles, lists.modifiedFiles);
  if (!preparation.firstKeptEntryId) throw new Error("First kept entry has no UUID - session may need migration");
  return { summary, firstKeptEntryId: preparation.firstKeptEntryId, tokensBefore: preparation.tokensBefore || 0, details: lists };
}

async function generateBranchSummary(entries, options = {}) {
  const model = options.model;
  const reserveTokens = options.reserveTokens || DEFAULT_COMPACTION_SETTINGS.reserveTokens;
  const contextWindow = model && model.contextWindow ? model.contextWindow : 128000;
  const prepared = prepareBranchEntries(entries || [], contextWindow - reserveTokens);
  if (!prepared.messages.length) return { summary: "No content to summarize" };
  let instructions = BRANCH_SUMMARY_PROMPT;
  if (options.replaceInstructions && options.customInstructions) instructions = options.customInstructions;
  else if (options.customInstructions) instructions = `${BRANCH_SUMMARY_PROMPT}\n\nAdditional focus: ${options.customInstructions}`;
  try {
    const context = {
      systemPrompt: SUMMARIZATION_SYSTEM_PROMPT,
      messages: buildSummaryMessages(serializeConversation(prepared.messages), instructions),
    };
    const response = await completeBridgeSummary(model, context, { apiKey: options.apiKey, headers: options.headers, signal: options.signal, maxTokens: 2048 }, options.streamFn);
    if (response && response.stopReason === "aborted") return { aborted: true };
    if (response && response.stopReason === "error") return { error: response.errorMessage || "Summarization failed" };
    const lists = computeFileLists(prepared.fileOps);
    return {
      summary: `${BRANCH_SUMMARY_PREAMBLE}${responseText(response) || "No summary generated"}${formatFileOperations(lists.readFiles, lists.modifiedFiles)}`,
      readFiles: lists.readFiles,
      modifiedFiles: lists.modifiedFiles,
    };
  } catch (error) {
    return { error: error && error.message ? error.message : String(error) };
  }
}

function getShellConfig(customShellPath) {
  if (customShellPath) {
    if (fs.existsSync(customShellPath)) return { shell: customShellPath, args: ["-c"] };
    throw new Error(`Custom shell path not found: ${customShellPath}`);
  }
  if (process.platform === "win32") {
    const paths = [process.env.ProgramFiles && `${process.env.ProgramFiles}\\Git\\bin\\bash.exe`, process.env["ProgramFiles(x86)"] && `${process.env["ProgramFiles(x86)"]}\\Git\\bin\\bash.exe`].filter(Boolean);
    for (const candidate of paths) if (fs.existsSync(candidate)) return { shell: candidate, args: ["-c"] };
    const where = childProcess.spawnSync("where", ["bash.exe"], { encoding: "utf8", timeout: 5000, windowsHide: true });
    const found = where.status === 0 && where.stdout ? where.stdout.trim().split(/\r?\n/)[0] : "";
    if (found && fs.existsSync(found)) return { shell: found, args: ["-c"] };
    throw new Error("No bash shell found. Install Git for Windows, add bash to PATH, or set shellPath in settings.json");
  }
  if (fs.existsSync("/bin/bash")) return { shell: "/bin/bash", args: ["-c"] };
  const which = childProcess.spawnSync("which", ["bash"], { encoding: "utf8", timeout: 5000 });
  const bash = which.status === 0 && which.stdout ? which.stdout.trim().split(/\r?\n/)[0] : "";
  if (bash) return { shell: bash, args: ["-c"] };
  return { shell: "sh", args: ["-c"] };
}

async function copyToClipboard(text) {
  const input = String(text || "");
  const options = { input, timeout: 5000, stdio: ["pipe", "ignore", "ignore"] };
  let copied = false;
  try {
    if (process.platform === "darwin") {
      childProcess.execFileSync("pbcopy", [], options);
      copied = true;
    } else if (process.platform === "win32") {
      childProcess.execFileSync("clip", [], options);
      copied = true;
    } else if (process.env.TERMUX_VERSION) {
      childProcess.execFileSync("termux-clipboard-set", [], options);
      copied = true;
    } else if (process.env.WAYLAND_DISPLAY) {
      const proc = childProcess.spawn("wl-copy", [], { stdio: ["pipe", "ignore", "ignore"] });
      proc.stdin.write(input);
      proc.stdin.end();
      proc.unref();
      copied = true;
    } else if (process.env.DISPLAY) {
      try {
        childProcess.execFileSync("xclip", ["-selection", "clipboard"], options);
        copied = true;
      } catch {
        childProcess.execFileSync("xsel", ["--clipboard", "--input"], options);
        copied = true;
      }
    }
  } catch {
    copied = false;
  }
  if (!copied || process.env.SSH_CONNECTION || process.env.SSH_CLIENT || process.env.MOSH_CONNECTION) {
    const encoded = Buffer.from(input).toString("base64");
    if (encoded.length <= 100000) {
      process.stdout.write(`\x1b]52;c;${encoded}\x07`);
      copied = true;
    }
  }
  if (!copied) throw new Error("Failed to copy to clipboard");
}

async function resizeImage(_inputBytes, _mimeType, _options) {
  return null;
}

function formatDimensionNote(result) {
  if (!result || !result.wasResized) return undefined;
  const scale = Number(result.originalWidth || 0) / Number(result.width || 1);
  return `[Image: original ${result.originalWidth}x${result.originalHeight}, displayed at ${result.width}x${result.height}. Multiply coordinates by ${scale.toFixed(2)} to map to original image.]`;
}

function ansiColor(value, background = false, mode = "truecolor") {
  if (typeof value === "number" && Number.isFinite(value)) return `\x1b[${background ? "48" : "38"};5;${Math.max(0, Math.min(255, Math.trunc(value)))}m`;
  const text = String(value || "");
  const hex = /^#?([0-9a-f]{6})$/i.exec(text);
  if (hex && mode === "truecolor") {
    const raw = hex[1];
    const r = parseInt(raw.slice(0, 2), 16);
    const g = parseInt(raw.slice(2, 4), 16);
    const b = parseInt(raw.slice(4, 6), 16);
    return `\x1b[${background ? "48" : "38"};2;${r};${g};${b}m`;
  }
  return "";
}

const DEFAULT_FG_THEME_COLORS = {
  accent: "#5fd7ff",
  border: "#808080",
  borderAccent: "#5fd7ff",
  borderMuted: "#606060",
  success: "#00af5f",
  error: "#d70000",
  warning: "#d75f00",
  muted: "#808080",
  dim: "#606060",
  text: "#d0d0d0",
  thinkingText: "#a0a0a0",
  userMessageText: "#ffffff",
  customMessageText: "#ffffff",
  customMessageLabel: "#5fd7ff",
  toolTitle: "#5fd7ff",
  toolOutput: "#d0d0d0",
  mdHeading: "#5fd7ff",
  mdLink: "#00afff",
  mdLinkUrl: "#808080",
  mdCode: "#afd75f",
  mdCodeBlock: "#d0d0d0",
  mdCodeBlockBorder: "#606060",
  mdQuote: "#a0a0a0",
  mdQuoteBorder: "#606060",
  mdHr: "#606060",
  mdListBullet: "#5fd7ff",
  toolDiffAdded: "#00af5f",
  toolDiffRemoved: "#d70000",
  toolDiffContext: "#808080",
  syntaxComment: "#808080",
  syntaxKeyword: "#ff5faf",
  syntaxFunction: "#5fd7ff",
  syntaxVariable: "#d0d0d0",
  syntaxString: "#afd75f",
  syntaxNumber: "#d75f00",
  syntaxType: "#5fafdf",
  syntaxOperator: "#d0d0d0",
  syntaxPunctuation: "#808080",
  thinkingOff: "#606060",
  thinkingMinimal: "#808080",
  thinkingLow: "#5fafdf",
  thinkingMedium: "#5fd7ff",
  thinkingHigh: "#ffaf00",
  thinkingXhigh: "#ff5faf",
  bashMode: "#5fd7ff",
};

const DEFAULT_BG_THEME_COLORS = {
  selectedBg: "#303030",
  userMessageBg: "#005f87",
  customMessageBg: "#303030",
  toolPendingBg: "#303030",
  toolSuccessBg: "#003f2f",
  toolErrorBg: "#3f0000",
};

class Theme {
  constructor(fgColors = {}, bgColors = {}, mode = "truecolor", options = {}) {
    this.name = options.name;
    this.sourcePath = options.sourcePath;
    this.sourceInfo = options.sourceInfo;
    this.mode = mode || "truecolor";
    this.fgColors = new Map();
    this.bgColors = new Map();
    for (const [key, value] of Object.entries({ ...DEFAULT_FG_THEME_COLORS, ...(fgColors || {}) })) this.fgColors.set(key, ansiColor(value, false, this.mode));
    for (const [key, value] of Object.entries({ ...DEFAULT_BG_THEME_COLORS, ...(bgColors || {}) })) this.bgColors.set(key, ansiColor(value, true, this.mode));
  }
  fg(color, text) {
    if (!this.fgColors.has(color)) throw new Error(`Unknown theme color: ${color}`);
    return `${this.fgColors.get(color)}${text}\x1b[39m`;
  }
  bg(color, text) {
    if (!this.bgColors.has(color)) throw new Error(`Unknown theme background color: ${color}`);
    return `${this.bgColors.get(color)}${text}\x1b[49m`;
  }
  bold(text) { return `\x1b[1m${text}\x1b[22m`; }
  italic(text) { return `\x1b[3m${text}\x1b[23m`; }
  underline(text) { return `\x1b[4m${text}\x1b[24m`; }
  inverse(text) { return `\x1b[7m${text}\x1b[27m`; }
  strikethrough(text) { return `\x1b[9m${text}\x1b[29m`; }
  getFgAnsi(color) {
    if (!this.fgColors.has(color)) throw new Error(`Unknown theme color: ${color}`);
    return this.fgColors.get(color);
  }
  getBgAnsi(color) {
    if (!this.bgColors.has(color)) throw new Error(`Unknown theme background color: ${color}`);
    return this.bgColors.get(color);
  }
  getColorMode() { return this.mode; }
  getThinkingBorderColor(level) {
    const key = { off: "thinkingOff", minimal: "thinkingMinimal", low: "thinkingLow", medium: "thinkingMedium", high: "thinkingHigh", xhigh: "thinkingXhigh" }[level] || "thinkingMedium";
    return (text) => this.fg(key, text);
  }
}

let activeTheme = new Theme(DEFAULT_FG_THEME_COLORS, DEFAULT_BG_THEME_COLORS, "truecolor", { name: "dark" });

function initTheme(themeName = "dark", _enableWatcher = false) {
  activeTheme = new Theme(DEFAULT_FG_THEME_COLORS, DEFAULT_BG_THEME_COLORS, "truecolor", { name: themeName || "dark" });
  return activeTheme;
}

function highlightCode(code, _lang) {
  return String(code || "").split("\n").map((line) => activeTheme.fg("mdCodeBlock", line));
}

function getLanguageFromPath(filePath) {
  const base = path.basename(String(filePath || "")).toLowerCase();
  if (base === "dockerfile") return "dockerfile";
  if (base === "makefile") return "makefile";
  const ext = String(filePath || "").split(".").pop().toLowerCase();
  return ({
    ts: "typescript", tsx: "typescript", js: "javascript", jsx: "javascript", mjs: "javascript", cjs: "javascript",
    py: "python", rb: "ruby", rs: "rust", go: "go", java: "java", kt: "kotlin", swift: "swift",
    c: "c", h: "c", cpp: "cpp", cc: "cpp", cxx: "cpp", hpp: "cpp", cs: "csharp", php: "php",
    sh: "bash", bash: "bash", zsh: "bash", fish: "fish", ps1: "powershell", sql: "sql",
    html: "html", htm: "html", css: "css", scss: "scss", sass: "sass", less: "less",
    json: "json", yaml: "yaml", yml: "yaml", toml: "toml", xml: "xml", md: "markdown", markdown: "markdown",
    cmake: "cmake", lua: "lua", perl: "perl", r: "r", scala: "scala", clj: "clojure",
    ex: "elixir", exs: "elixir", erl: "erlang", hs: "haskell", ml: "ocaml", vim: "vim",
    graphql: "graphql", proto: "protobuf", tf: "hcl", hcl: "hcl",
  })[ext];
}

function getMarkdownTheme() {
  return {
    heading: (text) => activeTheme.fg("mdHeading", text),
    link: (text) => activeTheme.fg("mdLink", text),
    linkUrl: (text) => activeTheme.fg("mdLinkUrl", text),
    code: (text) => activeTheme.fg("mdCode", text),
    codeBlock: (text) => activeTheme.fg("mdCodeBlock", text),
    codeBlockBorder: (text) => activeTheme.fg("mdCodeBlockBorder", text),
    quote: (text) => activeTheme.fg("mdQuote", text),
    quoteBorder: (text) => activeTheme.fg("mdQuoteBorder", text),
    hr: (text) => activeTheme.fg("mdHr", text),
    listBullet: (text) => activeTheme.fg("mdListBullet", text),
    bold: (text) => activeTheme.bold(text),
    italic: (text) => activeTheme.italic(text),
    underline: (text) => activeTheme.underline(text),
    strikethrough: (text) => activeTheme.strikethrough(text),
    highlightCode,
  };
}

function getSelectListTheme() {
  return {
    selectedPrefix: (text) => activeTheme.fg("accent", text),
    selectedText: (text) => activeTheme.fg("accent", text),
    description: (text) => activeTheme.fg("muted", text),
    scrollInfo: (text) => activeTheme.fg("muted", text),
    noMatch: (text) => activeTheme.fg("muted", text),
  };
}

function getSettingsListTheme() {
  return {
    label: (text, selected) => selected ? activeTheme.fg("accent", text) : text,
    value: (text, selected) => selected ? activeTheme.fg("accent", text) : activeTheme.fg("muted", text),
    description: (text) => activeTheme.fg("dim", text),
    cursor: activeTheme.fg("accent", "-> "),
    hint: (text) => activeTheme.fg("dim", text),
  };
}

function uniqueBridgePaths(values = []) {
  const seen = new Set();
  const out = [];
  for (const value of values) {
    if (!value) continue;
    const key = path.normalize(String(value));
    if (seen.has(key)) continue;
    seen.add(key);
    out.push(key);
  }
  return out;
}

function resolveBridgePath(baseDir, value) {
  const raw = String(value || "").trim();
  if (/^file:\/\//.test(raw)) {
    try {
      return path.normalize(fileURLToPath(raw));
    } catch {}
  }
  const expanded = expandBridgeTilde(raw);
  return path.isAbsolute(expanded) ? path.normalize(expanded) : path.resolve(baseDir || process.cwd(), expanded);
}

function jsonFile(filePath, fallback = {}) {
  try {
    if (!fs.existsSync(filePath)) return cloneJson(fallback);
    const text = fs.readFileSync(filePath, "utf8");
    return text.trim() ? JSON.parse(text) : cloneJson(fallback);
  } catch {
    return cloneJson(fallback);
  }
}

function directFilesWithExtensions(dir, extensions) {
  try {
    return fs.readdirSync(dir, { withFileTypes: true })
      .filter((entry) => entry.isFile() && extensions.some((suffix) => entry.name.endsWith(suffix)))
      .map((entry) => path.join(dir, entry.name))
      .sort();
  } catch {
    return [];
  }
}

function recursiveFilesWithExtensions(dir, extensions) {
  const out = [];
  const visit = (current) => {
    let entries = [];
    try {
      entries = fs.readdirSync(current, { withFileTypes: true });
    } catch {
      return;
    }
    for (const entry of entries.sort((a, b) => a.name.localeCompare(b.name))) {
      if (entry.name === "node_modules" || entry.name === ".git" || entry.name === "_build") continue;
      const filePath = path.join(current, entry.name);
      if (entry.isDirectory()) {
        visit(filePath);
      } else if (entry.isFile() && extensions.some((suffix) => entry.name.endsWith(suffix))) {
        out.push(filePath);
      }
    }
  };
  if (dir && fs.existsSync(dir)) visit(dir);
  return out;
}

function bridgeExtensionFiles(dir) {
  const direct = directFilesWithExtensions(dir, [".json", ".ts", ".js", ".mjs", ".cjs"]);
  let nested = [];
  try {
    nested = fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
      if (!entry.isDirectory()) return [];
      return ["index.ts", "index.js", "index.mjs", "index.cjs"]
        .map((name) => path.join(dir, entry.name, name))
        .filter((filePath) => fs.existsSync(filePath));
    });
  } catch {}
  return uniqueBridgePaths([...direct, ...nested]);
}

function collectResourceFiles(resourcePath, kind) {
  if (!resourcePath || !fs.existsSync(resourcePath)) return [];
  const stats = fs.statSync(resourcePath);
  if (!stats.isDirectory()) {
    const ok =
      kind === "extensions" ? /\.(json|ts|js|mjs|cjs)$/.test(resourcePath)
      : kind === "themes" ? resourcePath.endsWith(".json")
      : resourcePath.endsWith(".md");
    return ok ? [resourcePath] : [];
  }
  if (kind === "extensions") return bridgeExtensionFiles(resourcePath);
  if (kind === "skills") return recursiveFilesWithExtensions(resourcePath, [".md"]);
  if (kind === "prompts") return directFilesWithExtensions(resourcePath, [".md"]);
  if (kind === "themes") return directFilesWithExtensions(resourcePath, [".json"]);
  return [];
}

function bridgePosix(value) {
  return String(value || "").replace(/\\/g, "/");
}

function hasResourceGlobPattern(pattern) {
  return /[*?]/.test(String(pattern || ""));
}

function isResourceOverridePattern(pattern) {
  return /^[!+-]/.test(String(pattern || ""));
}

function packageGlobToRegExp(pattern) {
  const input = bridgePosix(pattern);
  let out = "";
  for (let i = 0; i < input.length; i += 1) {
    const ch = input[i];
    if (ch === "*") {
      if (input[i + 1] === "*") {
        out += ".*";
        i += 1;
      } else {
        out += "[^/]*";
      }
    } else if (ch === "?") {
      out += "[^/]";
    } else if ("\\^$+?.()|{}[]".includes(ch)) {
      out += `\\${ch}`;
    } else {
      out += ch;
    }
  }
  return new RegExp(`^${out}$`);
}

function packageRelativePath(baseDir, filePath) {
  return bridgePosix(path.relative(baseDir, filePath));
}

function normalizePackageExactPattern(pattern) {
  const normalized = bridgePosix(pattern);
  return normalized.startsWith("./") ? normalized.slice(2) : normalized;
}

function packagePatternCandidates(baseDir, filePath, includeBasename = true) {
  const candidates = [packageRelativePath(baseDir, filePath), bridgePosix(filePath)];
  if (includeBasename) candidates.push(path.basename(filePath));
  if (path.basename(filePath) === "SKILL.md") {
    const parent = path.dirname(filePath);
    candidates.push(packageRelativePath(baseDir, parent), bridgePosix(parent), path.basename(parent));
  }
  return uniqueBridgePaths(candidates.map(normalizePackageExactPattern));
}

function packagePatternMatches(baseDir, pattern, filePath) {
  const re = packageGlobToRegExp(pattern);
  return packagePatternCandidates(baseDir, filePath, true).some((candidate) => re.test(candidate));
}

function packageExactPatternMatches(baseDir, pattern, filePath) {
  const normalized = normalizePackageExactPattern(pattern);
  return packagePatternCandidates(baseDir, filePath, true).some((candidate) => candidate === normalized);
}

function applyPackageResourcePatterns(allPaths, patterns = [], baseDir) {
  const includes = [];
  const excludes = [];
  const forceIncludes = [];
  const forceExcludes = [];
  for (const pattern of patterns.map(String)) {
    if (pattern.startsWith("+")) forceIncludes.push(pattern.slice(1));
    else if (pattern.startsWith("-")) forceExcludes.push(pattern.slice(1));
    else if (pattern.startsWith("!")) excludes.push(pattern.slice(1));
    else includes.push(pattern);
  }

  let selected = includes.length === 0
    ? allPaths.slice()
    : allPaths.filter((filePath) => includes.some((pattern) => packagePatternMatches(baseDir, pattern, filePath)));
  if (excludes.length > 0) {
    selected = selected.filter((filePath) => !excludes.some((pattern) => packagePatternMatches(baseDir, pattern, filePath)));
  }
  for (const pattern of forceIncludes) {
    for (const filePath of allPaths) {
      if (!selected.includes(filePath) && packageExactPatternMatches(baseDir, pattern, filePath)) selected.push(filePath);
    }
  }
  if (forceExcludes.length > 0) {
    selected = selected.filter((filePath) => !forceExcludes.some((pattern) => packageExactPatternMatches(baseDir, pattern, filePath)));
  }
  return new Set(selected);
}

function allResourceFilesUnder(root, kind) {
  if (kind === "extensions") return recursiveFilesWithExtensions(root, [".json", ".ts", ".js", ".mjs", ".cjs"]);
  if (kind === "themes") return recursiveFilesWithExtensions(root, [".json"]);
  return recursiveFilesWithExtensions(root, [".md"]);
}

function packageManifestEntries(root, kind) {
  const manifest = jsonFile(path.join(root, "package.json"), {});
  const pi = manifest && typeof manifest.pi === "object" ? manifest.pi : {};
  const entries = pi && Array.isArray(pi[kind]) ? pi[kind] : [];
  return entries.map(String).map((entry) => entry.trim()).filter(Boolean);
}

function collectManifestResourceFiles(root, kind, entries) {
  return uniqueBridgePaths(entries.filter((entry) => !isResourceOverridePattern(entry)).flatMap((entry) => {
    if (hasResourceGlobPattern(entry)) {
      return allResourceFilesUnder(root, kind).filter((filePath) => packagePatternMatches(root, entry, filePath));
    }
    return collectResourceFiles(resolveBridgePath(root, entry), kind);
  }));
}

function packageResourceFiles(root, kind) {
  if (fs.existsSync(root) && fs.statSync(root).isFile()) return collectResourceFiles(root, kind);
  const entries = packageManifestEntries(root, kind);
  if (entries.length > 0) {
    const files = collectManifestResourceFiles(root, kind, entries);
    const overrides = entries.filter(isResourceOverridePattern);
    if (overrides.length === 0) return files;
    const enabled = applyPackageResourcePatterns(files, overrides, root);
    return files.filter((filePath) => enabled.has(filePath));
  }
  const conventional = { extensions: "extensions", skills: "skills", prompts: "prompts", themes: "themes" }[kind];
  return collectResourceFiles(path.join(root, conventional), kind);
}

function packageMetadata(source, scope = "temporary", origin = "package", baseDir = undefined) {
  return { source: String(source || ""), scope, origin, baseDir };
}

function resourceEntriesFromRoot(root, source, scope = "temporary", origin = "package", filter = undefined) {
  const resolvedRoot = resolveBridgePath(process.cwd(), root);
  const metadata = packageMetadata(source || resolvedRoot, scope, origin, resolvedRoot);
  const result = { extensions: [], skills: [], prompts: [], themes: [] };
  for (const kind of Object.keys(result)) {
    const files = packageResourceFiles(resolvedRoot, kind);
    if (filter && Object.prototype.hasOwnProperty.call(filter, kind) && Array.isArray(filter[kind])) {
      const enabled = applyPackageResourcePatterns(files, filter[kind], resolvedRoot);
      result[kind] = files.map((filePath) => ({ path: filePath, enabled: enabled.has(filePath), metadata }));
    } else {
      result[kind] = files.map((filePath) => ({ path: filePath, enabled: true, metadata }));
    }
  }
  return result;
}

function mergeResolvedResources(...sets) {
  const merged = { extensions: [], skills: [], prompts: [], themes: [] };
  for (const set of sets) {
    for (const kind of Object.keys(merged)) {
      merged[kind].push(...((set && set[kind]) || []));
    }
  }
  for (const kind of Object.keys(merged)) {
    const seen = new Set();
    merged[kind] = merged[kind].filter((entry) => {
      const key = entry.path;
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    });
  }
  return merged;
}

function parsePackageEntries(settings, scope) {
  const entries = Array.isArray(settings && settings.packages) ? settings.packages : [];
  return entries.flatMap((entry) => {
    if (typeof entry === "string") return [{ source: entry, scope, filtered: false }];
    if (!entry || typeof entry !== "object") return [];
    const source = String(entry.source || entry.package || entry.path || "").trim();
    if (!source) return [];
    const filters = {};
    for (const key of ["extensions", "skills", "prompts", "themes"]) {
      if (Array.isArray(entry[key])) filters[key] = entry[key].map(String);
    }
    const filtered = Object.keys(filters).length > 0;
    return [{ source, scope, filtered, filters: filtered ? filters : undefined }];
  });
}

function dedupePackageEntries(entries, cwd) {
  const byIdentity = new Map();
  for (const entry of entries) {
    const identity = packageSourceIdentity(entry.source, cwd, entry.scope);
    const existing = byIdentity.get(identity);
    if (!existing || (entry.scope === "project" && existing.scope !== "project")) {
      byIdentity.set(identity, entry);
    }
  }
  return [...byIdentity.values()];
}

function npmPackageName(source) {
  let spec = String(source || "").replace(/^npm:/, "");
  if (spec.startsWith("@")) {
    const index = spec.indexOf("@", 1);
    return index > 0 ? spec.slice(0, index) : spec;
  }
  const index = spec.indexOf("@");
  return index > 0 ? spec.slice(0, index) : spec;
}

function parseNpmSpec(spec) {
  const match = String(spec || "").match(/^(@?[^@]+(?:\/[^@]+)?)(?:@(.+))?$/);
  return {
    name: match && match[1] ? match[1] : String(spec || ""),
    version: match && match[2] ? match[2] : undefined,
  };
}

function isLocalPackageSource(source) {
  const text = String(source || "").trim();
  return !(
    text.startsWith("npm:") ||
    text.startsWith("git:") ||
    text.startsWith("github:") ||
    text.startsWith("http:") ||
    text.startsWith("https:") ||
    text.startsWith("ssh:")
  );
}

function splitGitRef(source) {
  const text = String(source || "");
  const scp = text.match(/^git@([^:]+):(.+)$/);
  if (scp) {
    const rest = scp[2] || "";
    const index = rest.indexOf("@");
    if (index > 0 && index < rest.length - 1) {
      return { repo: `git@${scp[1]}:${rest.slice(0, index)}`, ref: rest.slice(index + 1) };
    }
    return { repo: text };
  }
  if (text.includes("://")) {
    try {
      const parsed = new URL(text);
      const body = parsed.pathname.replace(/^\/+/, "");
      const index = body.indexOf("@");
      if (index > 0 && index < body.length - 1) {
        parsed.pathname = `/${body.slice(0, index)}`;
        return { repo: parsed.toString().replace(/\/$/, ""), ref: body.slice(index + 1) };
      }
    } catch {}
    return { repo: text };
  }
  const slash = text.indexOf("/");
  if (slash < 0) return { repo: text };
  const host = text.slice(0, slash);
  const body = text.slice(slash + 1);
  const index = body.indexOf("@");
  if (index > 0 && index < body.length - 1) return { repo: `${host}/${body.slice(0, index)}`, ref: body.slice(index + 1) };
  return { repo: text };
}

function parseGitSource(source) {
  const original = String(source || "").trim();
  const hasPrefix = original.startsWith("git:");
  const text = hasPrefix ? original.slice(4).trim() : original;
  if (!hasPrefix && !/^(https?|ssh|git):\/\//i.test(text)) return null;
  const split = splitGitRef(text);
  let repo = split.repo;
  let host = "";
  let repoPath = "";
  const scp = repo.match(/^git@([^:]+):(.+)$/);
  if (scp) {
    host = scp[1] || "";
    repoPath = scp[2] || "";
  } else if (/^(https?|ssh|git):\/\//i.test(repo)) {
    try {
      const parsed = new URL(repo);
      host = parsed.hostname;
      repoPath = parsed.pathname.replace(/^\/+/, "");
    } catch {
      return null;
    }
  } else {
    const slash = repo.indexOf("/");
    if (slash < 0) return null;
    host = repo.slice(0, slash);
    repoPath = repo.slice(slash + 1);
    if (!host.includes(".") && host !== "localhost") return null;
    repo = `https://${repo}`;
  }
  repoPath = repoPath.replace(/\.git$/, "").replace(/^\/+/, "");
  if (!host || !repoPath || repoPath.split("/").length < 2) return null;
  return { type: "git", repo, host, path: repoPath, ref: split.ref, pinned: !!split.ref };
}

function parsePackageSource(source) {
  const text = String(source || "").trim();
  if (text.startsWith("npm:")) {
    const spec = text.slice(4).trim();
    const parsed = parseNpmSpec(spec);
    return { type: "npm", spec, name: parsed.name, pinned: !!parsed.version };
  }
  if (isLocalPackageSource(text)) return { type: "local", path: text };
  return parseGitSource(text) || { type: "local", path: text };
}

function packageSourceIdentity(source, cwd, scope = undefined) {
  const parsed = parsePackageSource(source);
  if (parsed.type === "npm") return `npm:${parsed.name}`;
  if (parsed.type === "git") return `git:${parsed.host}/${parsed.path}`;
  return `local:${resolveBridgePath(cwd || process.cwd(), parsed.path)}`;
}

function packageInstallBase(agentDir, cwd, scope) {
  if (scope === "temporary") return path.join(os.tmpdir(), "pi-extensions");
  return scope === "project" ? path.join(cwd, ".pi") : agentDir;
}

function isBridgeOfflineMode() {
  return ["AGENT_OFFLINE", "PI_OFFLINE", "NO_NETWORK"].some((key) => /^(1|true|yes)$/i.test(String(process.env[key] || "")));
}

class DefaultPackageManager {
  constructor(options = {}) {
    this.cwd = resolveBridgePath(process.cwd(), options.cwd || process.cwd());
    this.agentDir = resolveBridgePath(process.cwd(), options.agentDir || getAgentDir());
    this.settingsManager = options.settingsManager || SettingsManager.create(this.cwd, this.agentDir);
    this.progressCallback = undefined;
  }

  setProgressCallback(callback) {
    this.progressCallback = typeof callback === "function" ? callback : undefined;
  }

  emitProgress(event) {
    if (this.progressCallback) this.progressCallback(event);
  }

  configuredEntries() {
    return [
      ...parsePackageEntries(this.settingsManager.getGlobalSettings ? this.settingsManager.getGlobalSettings() : {}, "user"),
      ...parsePackageEntries(this.settingsManager.getProjectSettings ? this.settingsManager.getProjectSettings() : {}, "project"),
    ];
  }

  listConfiguredPackages() {
    return this.configuredEntries().map((entry) => ({
      source: entry.source,
      scope: entry.scope,
      filtered: entry.filtered,
      installedPath: this.getInstalledPath(entry.source, entry.scope),
    }));
  }

  expectedInstalledPath(source, scope = "user") {
    const text = String(source || "");
    if (!text) return undefined;
    const parsed = parsePackageSource(text);
    if (parsed.type === "local") {
      return resolveBridgePath(this.cwd, text);
    }
    const base = packageInstallBase(this.agentDir, this.cwd, scope);
    if (parsed.type === "npm") {
      return path.join(base, "npm", "node_modules", parsed.name);
    }
    return path.join(base, "git", parsed.host, parsed.path);
  }

  getInstalledPath(source, scope = "user") {
    const installedPath = this.expectedInstalledPath(source, scope);
    return installedPath && fs.existsSync(installedPath) ? installedPath : undefined;
  }

  getNpmCommand() {
    const configured = this.settingsManager && this.settingsManager.settings ? this.settingsManager.settings.npmCommand : undefined;
    if (Array.isArray(configured) && configured.length > 0) {
      const [command, ...args] = configured.map(String);
      if (!command) throw new Error("Invalid npmCommand: first array entry must be a non-empty command");
      return { command, args };
    }
    return { command: "npm", args: [] };
  }

  packageManagerName() {
    const npmCommand = this.getNpmCommand();
    const parts = [npmCommand.command, ...npmCommand.args];
    const separator = parts.lastIndexOf("--");
    const command = separator >= 0 ? parts[separator + 1] : npmCommand.command;
    return path.basename(command || "").replace(/\.(cmd|exe)$/i, "");
  }

  async runPackageCommand(command, args, options = {}) {
    const result = await execCommand(command, args, { cwd: options.cwd || this.cwd, env: { ...process.env, ...(options.env || {}) }, timeout: options.timeout || 600000 });
    if (result.exitCode !== 0) {
      const detail = (result.stderr || result.stdout || result.output || "").trim();
      throw new Error(`Command failed: ${[command, ...args].join(" ")}${detail ? `\n${detail}` : ""}`);
    }
    return result.stdout || "";
  }

  ensureNpmProject(root) {
    fs.mkdirSync(root, { recursive: true });
    const ignorePath = path.join(root, ".gitignore");
    if (!fs.existsSync(ignorePath)) fs.writeFileSync(ignorePath, "*\n!.gitignore\n", "utf8");
    const packageJson = path.join(root, "package.json");
    if (!fs.existsSync(packageJson)) fs.writeFileSync(packageJson, JSON.stringify({ name: "pi-extensions", private: true }, null, 2), "utf8");
  }

  npmInstallRoot(scope, temporary = false) {
    if (temporary || scope === "temporary") return path.join(os.tmpdir(), "pi-extensions", "npm");
    return path.join(packageInstallBase(this.agentDir, this.cwd, scope), "npm");
  }

  npmInstallArgs(specs, root) {
    const manager = this.packageManagerName();
    if (manager === "bun") return ["install", ...specs, "--cwd", root, "--omit=peer"];
    if (manager === "pnpm") {
      return ["install", ...specs, "--prefix", root, "--config.auto-install-peers=false", "--config.strict-peer-dependencies=false", "--config.strict-dep-builds=false"];
    }
    return ["install", ...specs, "--prefix", root, "--legacy-peer-deps"];
  }

  async runNpm(args, options = {}) {
    const npmCommand = this.getNpmCommand();
    return this.runPackageCommand(npmCommand.command, [...npmCommand.args, ...args], options);
  }

  async installNpm(parsed, scope) {
    const root = this.npmInstallRoot(scope, scope === "temporary");
    this.ensureNpmProject(root);
    await this.runNpm(this.npmInstallArgs([parsed.spec], root));
  }

  async uninstallNpm(parsed, scope) {
    const root = this.npmInstallRoot(scope, false);
    if (!fs.existsSync(root)) return;
    const args = this.packageManagerName() === "bun" ? ["uninstall", parsed.name, "--cwd", root] : ["uninstall", parsed.name, "--prefix", root];
    await this.runNpm(args);
  }

  getInstalledNpmVersion(installedPath) {
    try {
      const packageJson = JSON.parse(fs.readFileSync(path.join(installedPath, "package.json"), "utf8"));
      return packageJson && packageJson.version ? String(packageJson.version) : undefined;
    } catch {
      return undefined;
    }
  }

  async getLatestNpmVersion(packageName) {
    const raw = (await this.runNpm(["view", packageName, "version", "--json"])).trim();
    if (!raw) throw new Error("Empty response from npm view");
    try {
      return String(JSON.parse(raw));
    } catch {
      return raw.replace(/^"|"$/g, "");
    }
  }

  async npmHasAvailableUpdate(parsed, installedPath) {
    if (isBridgeOfflineMode()) return false;
    const installedVersion = this.getInstalledNpmVersion(installedPath);
    if (!installedVersion) return false;
    try {
      return (await this.getLatestNpmVersion(parsed.name)) !== installedVersion;
    } catch {
      return false;
    }
  }

  async installGit(parsed, scope) {
    const target = this.expectedInstalledPath(`git:${parsed.repo}${parsed.ref ? `@${parsed.ref}` : ""}`, scope);
    if (fs.existsSync(target)) {
      await this.runPackageCommand("git", ["pull", "--ff-only"], { cwd: target, env: { GIT_TERMINAL_PROMPT: "0" } });
    } else {
      fs.mkdirSync(path.dirname(target), { recursive: true });
      await this.runPackageCommand("git", ["clone", parsed.repo, target], { env: { GIT_TERMINAL_PROMPT: "0" } });
    }
    if (parsed.ref) await this.runPackageCommand("git", ["checkout", parsed.ref], { cwd: target });
    if (fs.existsSync(path.join(target, "package.json"))) await this.runNpm(["install"], { cwd: target });
  }

  async gitHasAvailableUpdate(installedPath) {
    if (isBridgeOfflineMode()) return false;
    try {
      const local = (await this.runPackageCommand("git", ["rev-parse", "HEAD"], { cwd: installedPath, timeout: 60000 })).trim();
      const remoteOutput = await this.runPackageCommand("git", ["ls-remote", "origin", "HEAD"], {
        cwd: installedPath,
        env: { GIT_TERMINAL_PROMPT: "0" },
        timeout: 60000,
      });
      const match = remoteOutput.match(/^([0-9a-f]{40})\s+HEAD$/m) || remoteOutput.match(/^([0-9a-f]{40})\s+/m);
      return !!(match && match[1] && match[1] !== local);
    } catch {
      return false;
    }
  }

  removeGit(parsed, scope) {
    const target = this.expectedInstalledPath(`git:${parsed.repo}${parsed.ref ? `@${parsed.ref}` : ""}`, scope);
    if (target && fs.existsSync(target)) fs.rmSync(target, { recursive: true, force: true });
  }

  async installParsedSource(parsed, source, scope) {
    if (parsed.type === "npm") {
      await this.installNpm(parsed, scope);
      return;
    }
    if (parsed.type === "git") {
      await this.installGit(parsed, scope);
    }
  }

  async resolvePackageSource(source, scope = "user", onMissing = undefined, origin = "package", filter = undefined) {
    const parsed = parsePackageSource(source);
    if (parsed.type === "local") {
      const base = scope === "project" ? path.join(this.cwd, ".pi") : scope === "user" ? this.agentDir : this.cwd;
      const root = resolveBridgePath(base, parsed.path);
      return resourceEntriesFromRoot(root, source, scope, origin, filter);
    }

    let installedPath = this.expectedInstalledPath(source, scope);
    if (!installedPath || !fs.existsSync(installedPath)) {
      let action = "install";
      if (typeof onMissing === "function") action = await onMissing(source);
      if (action === "skip") return { extensions: [], skills: [], prompts: [], themes: [] };
      if (action === "error") throw new Error(`Missing source: ${source}`);
      await this.installParsedSource(parsed, source, scope);
      installedPath = this.expectedInstalledPath(source, scope);
    } else if (parsed.type === "git" && scope === "temporary" && !parsed.pinned) {
      try {
        await this.installGit(parsed, scope);
      } catch {}
    }
    return resourceEntriesFromRoot(installedPath, source, scope, origin, filter);
  }

  async resolve(onMissing = undefined) {
    const resolved = [];
    for (const entry of dedupePackageEntries(this.configuredEntries(), this.cwd)) {
      resolved.push(await this.resolvePackageSource(entry.source, entry.scope, onMissing, "package", entry.filters));
    }
    return mergeResolvedResources(...resolved);
  }

  async resolveExtensionSources(sources = [], options = {}) {
    const scope = options.local ? "project" : options.temporary ? "temporary" : "user";
    const resolved = [];
    for (const source of sources) {
      resolved.push(await this.resolvePackageSource(source, scope, undefined, options.temporary ? "top-level" : "package"));
    }
    return mergeResolvedResources(...resolved);
  }

  addSourceToSettings(source, options = {}) {
    const scope = options.local ? "project" : "global";
    const data = scope === "project" ? this.settingsManager.projectSettings : this.settingsManager.globalSettings;
    if (!data) return false;
    const current = Array.isArray(data.packages) ? data.packages.slice() : [];
    if (current.some((entry) => (typeof entry === "string" ? entry : entry && entry.source) === source)) return false;
    data.packages = [...current, source];
    this.settingsManager.save(scope);
    return true;
  }

  removeSourceFromSettings(source, options = {}) {
    const scope = options.local ? "project" : "global";
    const data = scope === "project" ? this.settingsManager.projectSettings : this.settingsManager.globalSettings;
    if (!data || !Array.isArray(data.packages)) return false;
    const before = data.packages.length;
    const wanted = packageSourceIdentity(source, this.cwd, options.local ? "project" : "user");
    data.packages = data.packages.filter((entry) => {
      const current = typeof entry === "string" ? entry : entry && entry.source;
      if (!current) return true;
      return packageSourceIdentity(current, this.cwd, options.local ? "project" : "user") !== wanted;
    });
    if (data.packages.length === before) return false;
    this.settingsManager.save(scope);
    return true;
  }

  async install(source, options = {}) {
    const scope = options.local ? "project" : "user";
    const parsed = parsePackageSource(source);
    this.emitProgress({ type: "start", action: "install", source, message: `Installing ${source}...` });
    try {
      if (parsed.type === "local") {
        const resolved = resolveBridgePath(this.cwd, parsed.path);
        if (!fs.existsSync(resolved)) throw new Error(`Path does not exist: ${resolved}`);
      } else if (parsed.type === "npm") {
        await this.installNpm(parsed, scope);
      } else if (parsed.type === "git") {
        await this.installGit(parsed, scope);
      }
      this.emitProgress({ type: "complete", action: "install", source });
    } catch (error) {
      this.emitProgress({ type: "error", action: "install", source, message: error && error.message ? error.message : String(error) });
      throw error;
    }
  }

  async installAndPersist(source, options = {}) {
    await this.install(source, options);
    this.addSourceToSettings(source, options);
  }

  async remove(source, options = {}) {
    const scope = options.local ? "project" : "user";
    const parsed = parsePackageSource(source);
    if (parsed.type === "npm") await this.uninstallNpm(parsed, scope);
    else if (parsed.type === "git") this.removeGit(parsed, scope);
    this.emitProgress({ type: "complete", action: "remove", source });
  }

  async removeAndPersist(source, options = {}) {
    const removed = this.removeSourceFromSettings(source, options);
    await this.remove(source, options);
    return removed;
  }

  async update(source = undefined) {
    const entries = this.configuredEntries();
    const wanted = source ? packageSourceIdentity(source, this.cwd) : undefined;
    let matched = false;
    for (const entry of entries) {
      if (wanted && packageSourceIdentity(entry.source, this.cwd, entry.scope) !== wanted) continue;
      matched = true;
      const parsed = parsePackageSource(entry.source);
      if (parsed.type === "local") continue;
      if (parsed.pinned) continue;
      await this.install(entry.source, { local: entry.scope === "project" });
    }
    if (source && !matched) throw new Error(`No matching package found for ${source}`);
    this.emitProgress({ type: "complete", action: "update", source: source || "*" });
  }

  async checkForAvailableUpdates() {
    if (isBridgeOfflineMode()) return [];
    const updates = [];
    for (const entry of dedupePackageEntries(this.configuredEntries(), this.cwd)) {
      const parsed = parsePackageSource(entry.source);
      if (parsed.type === "local" || parsed.pinned) continue;
      const installedPath = this.getInstalledPath(entry.source, entry.scope);
      if (!installedPath) continue;
      if (parsed.type === "npm") {
        if (await this.npmHasAvailableUpdate(parsed, installedPath)) {
          updates.push({ source: entry.source, displayName: parsed.name, type: "npm", scope: entry.scope });
        }
      } else if (parsed.type === "git") {
        if (await this.gitHasAvailableUpdate(installedPath)) {
          updates.push({ source: entry.source, displayName: `${parsed.host}/${parsed.path}`, type: "git", scope: entry.scope });
        }
      }
    }
    return updates;
  }
}

function contextFileFromDir(dir) {
  for (const name of ["AGENTS.md", "AGENTS.MD", "CLAUDE.md", "CLAUDE.MD"]) {
    const filePath = path.join(dir, name);
    if (fs.existsSync(filePath) && fs.statSync(filePath).isFile()) {
      try {
        return { path: filePath, content: fs.readFileSync(filePath, "utf8") };
      } catch {}
    }
  }
  return null;
}

function loadProjectContextFiles(options = {}) {
  const cwd = resolveBridgePath(process.cwd(), options.cwd || process.cwd());
  const agentDir = resolveBridgePath(process.cwd(), options.agentDir || getAgentDir());
  const seen = new Set();
  const files = [];
  const add = (entry) => {
    if (!entry || seen.has(entry.path)) return;
    seen.add(entry.path);
    files.push(entry);
  };
  add(contextFileFromDir(agentDir));
  const ancestors = [];
  let current = cwd;
  while (true) {
    ancestors.unshift(current);
    const parent = path.dirname(current);
    if (parent === current) break;
    current = parent;
  }
  for (const dir of ancestors) add(contextFileFromDir(dir));
  return files;
}

function loadPromptFile(filePath, source = "path", baseDir = path.dirname(filePath)) {
  const markdown = fs.readFileSync(filePath, "utf8");
  const parsed = parseFrontmatter(markdown);
  const name = String(path.basename(filePath, path.extname(filePath))).trim();
  const firstLine = parsed.body.split(/\r?\n/).find((line) => line.trim());
  let description = String(parsed.data.description || firstLine || "").trim();
  if (!parsed.data.description && description.length > 60) description = `${description.slice(0, 60)}...`;
  return {
    name,
    description,
    argumentHint: parsed.data["argument-hint"],
    argument_hint: parsed.data["argument-hint"],
    body: parsed.body,
    content: parsed.body,
    filePath,
    location: filePath,
    sourceInfo: createSyntheticSourceInfo(filePath, { source, baseDir, scope: source === "project" ? "project" : source === "user" ? "user" : "temporary" }),
  };
}

function loadPromptTemplates(options = {}) {
  const cwd = options.cwd || process.cwd();
  const agentDir = options.agentDir || getAgentDir();
  const includeDefaults = options.includeDefaults !== false;
  const promptPaths = Array.isArray(options.promptPaths) ? options.promptPaths : [];
  const prompts = [];
  const diagnostics = [];
  const addPath = (raw, source = "path") => {
    const resolved = path.isAbsolute(String(raw)) ? String(raw) : path.resolve(cwd, String(raw));
    if (!fs.existsSync(resolved)) {
      diagnostics.push({ type: "warning", message: "prompt path does not exist", path: resolved });
      return;
    }
    const files = fs.statSync(resolved).isDirectory() ? directFilesWithExtensions(resolved, [".md"]) : [resolved];
    for (const filePath of files) {
      if (!filePath.endsWith(".md")) continue;
      try {
        prompts.push(loadPromptFile(filePath, source, path.dirname(filePath)));
      } catch (error) {
        diagnostics.push({ type: "warning", message: error.message, path: filePath });
      }
    }
  };
  if (includeDefaults) {
    addPath(path.join(agentDir, "prompts"), "user");
    addPath(path.join(cwd, ".pi", "prompts"), "project");
  }
  for (const promptPath of promptPaths) addPath(promptPath, "path");
  return { prompts, diagnostics };
}

function loadThemeFile(filePath, source = "path", baseDir = path.dirname(filePath)) {
  const json = jsonFile(filePath, {});
  const name = String(json.name || path.basename(filePath, path.extname(filePath))).trim();
  return {
    ...json,
    name,
    sourcePath: filePath,
    path: filePath,
    location: filePath,
    sourceInfo: createSyntheticSourceInfo(filePath, { source, baseDir, scope: source === "project" ? "project" : source === "user" ? "user" : "temporary" }),
  };
}

function loadThemesFromPaths(themePaths = [], options = {}) {
  const cwd = options.cwd || process.cwd();
  const themes = [];
  const diagnostics = [];
  for (const raw of themePaths) {
    const resolved = path.isAbsolute(String(raw)) ? String(raw) : path.resolve(cwd, String(raw));
    if (!fs.existsSync(resolved)) {
      diagnostics.push({ type: "warning", message: "theme path does not exist", path: resolved });
      continue;
    }
    const files = fs.statSync(resolved).isDirectory() ? directFilesWithExtensions(resolved, [".json"]) : [resolved];
    for (const filePath of files) {
      if (!filePath.endsWith(".json")) continue;
      try {
        themes.push(loadThemeFile(filePath));
      } catch (error) {
        diagnostics.push({ type: "warning", message: error.message, path: filePath });
      }
    }
  }
  return { themes, diagnostics };
}

class DefaultResourceLoader {
  constructor(options = {}) {
    this.cwd = resolveBridgePath(process.cwd(), options.cwd || process.cwd());
    this.agentDir = resolveBridgePath(process.cwd(), options.agentDir || getAgentDir());
    this.settingsManager = options.settingsManager || SettingsManager.create(this.cwd, this.agentDir);
    this.eventBus = options.eventBus || createEventBus();
    this.packageManager = options.packageManager || new DefaultPackageManager({ cwd: this.cwd, agentDir: this.agentDir, settingsManager: this.settingsManager });
    this.additionalExtensionPaths = options.additionalExtensionPaths || [];
    this.additionalSkillPaths = options.additionalSkillPaths || [];
    this.additionalPromptTemplatePaths = options.additionalPromptTemplatePaths || [];
    this.additionalThemePaths = options.additionalThemePaths || [];
    this.extensionFactories = Array.isArray(options.extensionFactories) ? options.extensionFactories.slice() : [];
    this.noExtensions = !!options.noExtensions;
    this.noSkills = !!options.noSkills;
    this.noPromptTemplates = !!options.noPromptTemplates;
    this.noThemes = !!options.noThemes;
    this.noContextFiles = !!options.noContextFiles;
    this.systemPromptSource = options.systemPrompt;
    this.appendSystemPromptSource = options.appendSystemPrompt;
    this.extensionsOverride = typeof options.extensionsOverride === "function" ? options.extensionsOverride : undefined;
    this.skillsOverride = typeof options.skillsOverride === "function" ? options.skillsOverride : undefined;
    this.promptsOverride = typeof options.promptsOverride === "function" ? options.promptsOverride : undefined;
    this.themesOverride = typeof options.themesOverride === "function" ? options.themesOverride : undefined;
    this.agentsFilesOverride = typeof options.agentsFilesOverride === "function" ? options.agentsFilesOverride : undefined;
    this.systemPromptOverride = typeof options.systemPromptOverride === "function" ? options.systemPromptOverride : undefined;
    this.appendSystemPromptOverride = typeof options.appendSystemPromptOverride === "function" ? options.appendSystemPromptOverride : undefined;
    this.extensionsResult = { extensions: [], errors: [], runtime: createExtensionRuntime() };
    this.skills = [];
    this.skillDiagnostics = [];
    this.prompts = [];
    this.promptDiagnostics = [];
    this.themes = [];
    this.themeDiagnostics = [];
    this.agentsFiles = [];
    this.systemPrompt = undefined;
    this.appendSystemPrompt = [];
    this.lastSkillPaths = [];
    this.lastPromptPaths = [];
    this.lastThemePaths = [];
    this.extensionSkillSourceInfos = new Map();
    this.extensionPromptSourceInfos = new Map();
    this.extensionThemeSourceInfos = new Map();
  }

  getExtensions() { return this.extensionsResult; }
  getSkills() { return { skills: this.skills, diagnostics: this.skillDiagnostics }; }
  getPrompts() { return { prompts: this.prompts, diagnostics: this.promptDiagnostics }; }
  getThemes() { return { themes: this.themes, diagnostics: this.themeDiagnostics }; }
  getAgentsFiles() { return { agentsFiles: this.agentsFiles }; }
  getSystemPrompt() { return this.systemPrompt; }
  getAppendSystemPrompt() { return this.appendSystemPrompt.slice(); }

  resolvePromptInput(input) {
    if (!input) return undefined;
    const resolved = resolveBridgePath(this.cwd, input);
    if (fs.existsSync(resolved) && fs.statSync(resolved).isFile()) {
      try {
        return fs.readFileSync(resolved, "utf8");
      } catch {}
    }
    return String(input);
  }

  discoverSystemPromptFile() {
    for (const candidate of [path.join(this.cwd, ".pi", "SYSTEM.md"), path.join(this.agentDir, "SYSTEM.md")]) {
      if (fs.existsSync(candidate) && fs.statSync(candidate).isFile()) return candidate;
    }
    return undefined;
  }

  discoverAppendSystemPromptFile() {
    for (const candidate of [path.join(this.cwd, ".pi", "APPEND_SYSTEM.md"), path.join(this.agentDir, "APPEND_SYSTEM.md")]) {
      if (fs.existsSync(candidate) && fs.statSync(candidate).isFile()) return candidate;
    }
    return undefined;
  }

  async loadExtensionFactories(runtime) {
    const extensions = [];
    const errors = [];
    for (const [index, factory] of this.extensionFactories.entries()) {
      const extensionPath = `<inline:${index + 1}>`;
      try {
        extensions.push(await loadExtensionFromFactory(factory, this.cwd, this.eventBus, runtime, extensionPath));
      } catch (error) {
        errors.push({ path: extensionPath, error: error && error.message ? error.message : String(error) });
      }
    }
    return { extensions, errors };
  }

  resolveResourcePath(value) {
    return resolveBridgePath(this.cwd, value);
  }

  canonicalResourcePath(value) {
    try {
      return fs.realpathSync(value);
    } catch {
      return path.resolve(String(value || ""));
    }
  }

  mergePaths(primary = [], additional = []) {
    const merged = [];
    const seen = new Set();
    for (const value of [...primary, ...additional]) {
      if (!value) continue;
      const resolved = this.resolveResourcePath(value);
      const key = this.canonicalResourcePath(resolved);
      if (seen.has(key)) continue;
      seen.add(key);
      merged.push(resolved);
    }
    return merged;
  }

  normalizeExtensionPaths(entries = []) {
    return entries.map((entry) => {
      const rawPath = typeof entry === "string" ? entry : entry && entry.path;
      const metadata = typeof entry === "object" && entry && entry.metadata ? { ...entry.metadata } : undefined;
      if (metadata && metadata.baseDir) metadata.baseDir = this.resolveResourcePath(metadata.baseDir);
      return { path: this.resolveResourcePath(rawPath), metadata };
    }).filter((entry) => entry.path);
  }

  isUnderPath(target, root) {
    if (!target || !root) return false;
    const normalizedTarget = path.resolve(target);
    const normalizedRoot = path.resolve(root);
    return normalizedTarget === normalizedRoot || normalizedTarget.startsWith(`${normalizedRoot}${path.sep}`);
  }

  sourceInfoFromMetadata(resourcePath, metadata) {
    if (!metadata) return undefined;
    return createSyntheticSourceInfo(resourcePath, {
      source: metadata.source,
      scope: metadata.scope,
      origin: metadata.origin,
      baseDir: metadata.baseDir,
    });
  }

  findSourceInfoForPath(resourcePath, extraSourceInfos = undefined, metadataByPath = undefined) {
    if (!resourcePath) return undefined;
    if (String(resourcePath).startsWith("<")) return this.getDefaultSourceInfoForPath(resourcePath);
    const normalizedResourcePath = path.resolve(resourcePath);
    if (extraSourceInfos) {
      for (const [sourcePath, sourceInfo] of extraSourceInfos.entries()) {
        const normalizedSourcePath = path.resolve(sourcePath);
        if (normalizedResourcePath === normalizedSourcePath || normalizedResourcePath.startsWith(`${normalizedSourcePath}${path.sep}`)) {
          return { ...sourceInfo, path: resourcePath };
        }
      }
    }
    if (metadataByPath) {
      const exact = metadataByPath.get(normalizedResourcePath) || metadataByPath.get(resourcePath);
      if (exact) return this.sourceInfoFromMetadata(resourcePath, exact);
      for (const [sourcePath, metadata] of metadataByPath.entries()) {
        const normalizedSourcePath = path.resolve(sourcePath);
        if (normalizedResourcePath === normalizedSourcePath || normalizedResourcePath.startsWith(`${normalizedSourcePath}${path.sep}`)) {
          return this.sourceInfoFromMetadata(resourcePath, metadata);
        }
      }
    }
    return undefined;
  }

  getDefaultSourceInfoForPath(filePath) {
    const text = String(filePath || "");
    if (text.startsWith("<") && text.endsWith(">")) {
      return {
        path: text,
        source: text.slice(1, -1).split(":")[0] || "temporary",
        scope: "temporary",
        origin: "top-level",
      };
    }
    const normalized = path.resolve(text);
    const roots = [
      { root: path.join(this.agentDir, "skills"), scope: "user" },
      { root: path.join(this.agentDir, "prompts"), scope: "user" },
      { root: path.join(this.agentDir, "themes"), scope: "user" },
      { root: path.join(this.agentDir, "extensions"), scope: "user" },
      { root: path.join(this.cwd, ".pi", "skills"), scope: "project" },
      { root: path.join(this.cwd, ".pi", "prompts"), scope: "project" },
      { root: path.join(this.cwd, ".pi", "themes"), scope: "project" },
      { root: path.join(this.cwd, ".pi", "extensions"), scope: "project" },
    ];
    for (const entry of roots) {
      if (this.isUnderPath(normalized, entry.root)) {
        return createSyntheticSourceInfo(text, { source: "local", scope: entry.scope, baseDir: entry.root });
      }
    }
    let isDirectory = false;
    try {
      isDirectory = fs.statSync(normalized).isDirectory();
    } catch {}
    return createSyntheticSourceInfo(text, {
      source: "local",
      scope: "temporary",
      baseDir: isDirectory ? normalized : path.dirname(normalized),
    });
  }

  applyExtensionSourceInfo(extensions, metadataByPath) {
    for (const extension of extensions || []) {
      extension.sourceInfo =
        this.findSourceInfoForPath(extension.path, undefined, metadataByPath) ||
        extension.sourceInfo ||
        this.getDefaultSourceInfoForPath(extension.path);
      for (const command of extension.commands ? extension.commands.values() : []) command.sourceInfo = extension.sourceInfo;
      for (const tool of extension.tools ? extension.tools.values() : []) tool.sourceInfo = extension.sourceInfo;
    }
  }

  dedupePrompts(prompts = []) {
    const byName = new Map();
    const diagnostics = [];
    for (const prompt of prompts) {
      const existing = byName.get(prompt.name);
      if (existing) {
        diagnostics.push({
          type: "collision",
          message: `name "/${prompt.name}" collision`,
          path: prompt.filePath,
          collision: {
            resourceType: "prompt",
            name: prompt.name,
            winnerPath: existing.filePath,
            loserPath: prompt.filePath,
          },
        });
      } else {
        byName.set(prompt.name, prompt);
      }
    }
    return { prompts: [...byName.values()], diagnostics };
  }

  dedupeThemes(themes = []) {
    const byName = new Map();
    const diagnostics = [];
    for (const theme of themes) {
      const name = theme && theme.name ? theme.name : "unnamed";
      const existing = byName.get(name);
      if (existing) {
        diagnostics.push({
          type: "collision",
          message: `name "${name}" collision`,
          path: theme.sourcePath || theme.path,
          collision: {
            resourceType: "theme",
            name,
            winnerPath: existing.sourcePath || existing.path || "<builtin>",
            loserPath: theme.sourcePath || theme.path || "<builtin>",
          },
        });
      } else {
        byName.set(name, theme);
      }
    }
    return { themes: [...byName.values()], diagnostics };
  }

  updateSkillsFromPaths(skillPaths, metadataByPath = undefined, includeDefaults = false) {
    const base = this.noSkills && skillPaths.length === 0
      ? { skills: [], diagnostics: [] }
      : loadSkills({ cwd: this.cwd, agentDir: this.agentDir, skillPaths, includeDefaults });
    const resolved = this.skillsOverride ? this.skillsOverride(base) : base;
    this.skills = (resolved.skills || []).map((skill) => ({
      ...skill,
      sourceInfo:
        this.findSourceInfoForPath(skill.filePath, this.extensionSkillSourceInfos, metadataByPath) ||
        skill.sourceInfo ||
        this.getDefaultSourceInfoForPath(skill.filePath),
    }));
    this.skillDiagnostics = resolved.diagnostics || [];
  }

  updatePromptsFromPaths(promptPaths, metadataByPath = undefined, includeDefaults = false) {
    let base;
    if (this.noPromptTemplates && promptPaths.length === 0) {
      base = { prompts: [], diagnostics: [] };
    } else {
      const loaded = loadPromptTemplates({ cwd: this.cwd, agentDir: this.agentDir, promptPaths, includeDefaults });
      const deduped = this.dedupePrompts(loaded.prompts || []);
      base = { prompts: deduped.prompts, diagnostics: [...(loaded.diagnostics || []), ...deduped.diagnostics] };
    }
    const resolved = this.promptsOverride ? this.promptsOverride(base) : base;
    this.prompts = (resolved.prompts || []).map((prompt) => ({
      ...prompt,
      sourceInfo:
        this.findSourceInfoForPath(prompt.filePath, this.extensionPromptSourceInfos, metadataByPath) ||
        prompt.sourceInfo ||
        this.getDefaultSourceInfoForPath(prompt.filePath),
    }));
    this.promptDiagnostics = resolved.diagnostics || [];
  }

  updateThemesFromPaths(themePaths, metadataByPath = undefined, includeDefaults = false) {
    let base;
    if (this.noThemes && themePaths.length === 0) {
      base = { themes: [], diagnostics: [] };
    } else {
      const loaded = loadThemesFromPaths(themePaths, { cwd: this.cwd });
      const rawThemes = includeDefaults ? [{ name: "dark" }, { name: "light" }, ...loaded.themes] : loaded.themes;
      const deduped = this.dedupeThemes(rawThemes);
      base = { themes: deduped.themes, diagnostics: [...(loaded.diagnostics || []), ...deduped.diagnostics] };
    }
    const resolved = this.themesOverride ? this.themesOverride(base) : base;
    this.themes = (resolved.themes || []).map((theme) => {
      const sourcePath = theme.sourcePath || theme.path;
      return sourcePath
        ? {
            ...theme,
            sourceInfo:
              this.findSourceInfoForPath(sourcePath, this.extensionThemeSourceInfos, metadataByPath) ||
              theme.sourceInfo ||
              this.getDefaultSourceInfoForPath(sourcePath),
          }
        : theme;
    });
    this.themeDiagnostics = resolved.diagnostics || [];
  }

  async reload() {
    if (this.settingsManager && typeof this.settingsManager.reload === "function") await this.settingsManager.reload();
    const packageResources = await this.packageManager.resolve();
    const cliResources = await this.packageManager.resolveExtensionSources(this.additionalExtensionPaths, { temporary: true });
    const metadataByPath = new Map();
    const addMetadata = (entries = []) => {
      for (const entry of entries) {
        if (entry && entry.path && entry.metadata && !metadataByPath.has(path.resolve(entry.path))) {
          metadataByPath.set(path.resolve(entry.path), entry.metadata);
        }
      }
    };
    for (const kind of ["extensions", "skills", "prompts", "themes"]) {
      addMetadata(packageResources[kind]);
      addMetadata(cliResources[kind]);
    }
    this.extensionSkillSourceInfos = new Map();
    this.extensionPromptSourceInfos = new Map();
    this.extensionThemeSourceInfos = new Map();
    const cliExtensionPaths = cliResources.extensions.filter((entry) => entry.enabled).map((entry) => entry.path);
    const packageExtensionPaths = packageResources.extensions.filter((entry) => entry.enabled).map((entry) => entry.path);
    const extensionPaths = this.noExtensions ? this.mergePaths(cliExtensionPaths, []) : this.mergePaths(cliExtensionPaths, packageExtensionPaths);
    const extensionsResult = this.noExtensions
      ? await loadExtensions(extensionPaths, this.cwd, this.eventBus)
      : await discoverAndLoadExtensions(extensionPaths, this.cwd, this.agentDir, this.eventBus);
    const inlineExtensions = await this.loadExtensionFactories(extensionsResult.runtime);
    extensionsResult.extensions.push(...inlineExtensions.extensions);
    extensionsResult.errors.push(...inlineExtensions.errors);
    this.extensionsResult = this.extensionsOverride ? this.extensionsOverride(extensionsResult) : extensionsResult;
    this.applyExtensionSourceInfo(this.extensionsResult.extensions, metadataByPath);

    const cliSkillPaths = cliResources.skills.filter((entry) => entry.enabled).map((entry) => entry.path);
    const packageSkillPaths = packageResources.skills.filter((entry) => entry.enabled).map((entry) => entry.path);
    const skillPaths = this.noSkills
      ? this.mergePaths(cliSkillPaths, this.additionalSkillPaths)
      : this.mergePaths([...cliSkillPaths, ...packageSkillPaths], this.additionalSkillPaths);
    this.lastSkillPaths = skillPaths;
    this.updateSkillsFromPaths(skillPaths, metadataByPath, !this.noSkills);

    const cliPromptPaths = cliResources.prompts.filter((entry) => entry.enabled).map((entry) => entry.path);
    const packagePromptPaths = packageResources.prompts.filter((entry) => entry.enabled).map((entry) => entry.path);
    const promptPaths = this.noPromptTemplates
      ? this.mergePaths(cliPromptPaths, this.additionalPromptTemplatePaths)
      : this.mergePaths([...cliPromptPaths, ...packagePromptPaths], this.additionalPromptTemplatePaths);
    this.lastPromptPaths = promptPaths;
    this.updatePromptsFromPaths(promptPaths, metadataByPath, !this.noPromptTemplates);

    const cliThemePaths = cliResources.themes.filter((entry) => entry.enabled).map((entry) => entry.path);
    const packageThemePaths = packageResources.themes.filter((entry) => entry.enabled).map((entry) => entry.path);
    const themePaths = this.noThemes
      ? this.mergePaths(cliThemePaths, this.additionalThemePaths)
      : this.mergePaths([...cliThemePaths, ...packageThemePaths], this.additionalThemePaths);
    this.lastThemePaths = themePaths;
    this.updateThemesFromPaths(themePaths, metadataByPath, !this.noThemes);

    const baseAgentsFiles = { agentsFiles: this.noContextFiles ? [] : loadProjectContextFiles({ cwd: this.cwd, agentDir: this.agentDir }) };
    const resolvedAgentsFiles = this.agentsFilesOverride ? this.agentsFilesOverride(baseAgentsFiles) : baseAgentsFiles;
    this.agentsFiles = Array.isArray(resolvedAgentsFiles && resolvedAgentsFiles.agentsFiles) ? resolvedAgentsFiles.agentsFiles : [];
    const baseSystemPrompt = this.resolvePromptInput(this.systemPromptSource || this.discoverSystemPromptFile());
    this.systemPrompt = this.systemPromptOverride ? this.systemPromptOverride(baseSystemPrompt) : baseSystemPrompt;
    const discoveredAppend = this.discoverAppendSystemPromptFile();
    const appendSources =
      this.appendSystemPromptSource !== undefined
        ? (Array.isArray(this.appendSystemPromptSource) ? this.appendSystemPromptSource : [this.appendSystemPromptSource])
        : (discoveredAppend ? [discoveredAppend] : []);
    const baseAppend = appendSources.map((source) => this.resolvePromptInput(source)).filter((value) => value !== undefined);
    this.appendSystemPrompt = this.appendSystemPromptOverride ? this.appendSystemPromptOverride(baseAppend) : baseAppend;
  }

  extendResources(paths = {}) {
    const skillEntries = this.normalizeExtensionPaths(paths.skillPaths || []);
    const promptEntries = this.normalizeExtensionPaths(paths.promptPaths || []);
    const themeEntries = this.normalizeExtensionPaths(paths.themePaths || []);
    const skillPaths = skillEntries.map((entry) => entry.path);
    const promptPaths = promptEntries.map((entry) => entry.path);
    const themePaths = themeEntries.map((entry) => entry.path);
    for (const entry of skillEntries) {
      if (entry.metadata) this.extensionSkillSourceInfos.set(entry.path, this.sourceInfoFromMetadata(entry.path, entry.metadata));
    }
    for (const entry of promptEntries) {
      if (entry.metadata) this.extensionPromptSourceInfos.set(entry.path, this.sourceInfoFromMetadata(entry.path, entry.metadata));
    }
    for (const entry of themeEntries) {
      if (entry.metadata) this.extensionThemeSourceInfos.set(entry.path, this.sourceInfoFromMetadata(entry.path, entry.metadata));
    }
    if (skillPaths.length > 0) {
      this.lastSkillPaths = this.mergePaths(this.lastSkillPaths, skillPaths);
      this.updateSkillsFromPaths(this.lastSkillPaths, undefined, !this.noSkills);
    }
    if (promptPaths.length > 0) {
      this.lastPromptPaths = this.mergePaths(this.lastPromptPaths, promptPaths);
      this.updatePromptsFromPaths(this.lastPromptPaths, undefined, !this.noPromptTemplates);
    }
    if (themePaths.length > 0) {
      this.lastThemePaths = this.mergePaths(this.lastThemePaths, themePaths);
      this.updateThemesFromPaths(this.lastThemePaths, undefined, !this.noThemes);
    }
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
  setText(content) { this.content = content; }
  invalidate() {}
  dispose() {}
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
  removeChild(child) {
    const index = this.children.indexOf(child);
    if (index >= 0) this.children.splice(index, 1);
  }
  clear() {
    this.children = [];
  }
  invalidate() {
    for (const child of this.children) if (child && typeof child.invalidate === "function") child.invalidate();
  }
  dispose() {
    for (const child of this.children) if (child && typeof child.dispose === "function") child.dispose();
  }
  render(width = 80) {
    return this.children.flatMap((child) => componentLines(child, width));
  }
}

class Box extends Container {}

class Spacer {
  constructor(lines = 1) {
    this.lines = Math.max(0, Number(lines) || 0);
  }
  setLines(lines) { this.lines = Math.max(0, Number(lines) || 0); }
  invalidate() {}
  dispose() {}
  render() { return Array.from({ length: this.lines }, () => ""); }
}

class GenericSelectList {
  constructor(items = [], maxVisible = 5, theme = undefined, layout = {}) {
    this.items = Array.isArray(items) ? items.slice() : [];
    this.filteredItems = this.items.slice();
    this.selectedIndex = 0;
    this.maxVisible = Math.max(1, Number(maxVisible) || 5);
    this.theme = theme || {};
    this.layout = layout || {};
  }
  setFilter(filter = "") {
    const query = String(filter || "").toLowerCase();
    this.filteredItems = query
      ? this.items.filter((item) => String(item.label || item.value || "").toLowerCase().startsWith(query))
      : this.items.slice();
    this.selectedIndex = 0;
  }
  setSelectedIndex(index) {
    this.selectedIndex = Math.max(0, Math.min(Number(index) || 0, Math.max(0, this.filteredItems.length - 1)));
    if (this.onSelectionChange && this.filteredItems[this.selectedIndex]) this.onSelectionChange(this.filteredItems[this.selectedIndex]);
  }
  getSelectedItem() {
    return this.filteredItems[this.selectedIndex];
  }
  handleInput(data) {
    if (matchesKey(data, "up")) this.setSelectedIndex(this.selectedIndex - 1);
    else if (matchesKey(data, "down")) this.setSelectedIndex(this.selectedIndex + 1);
    else if (matchesKey(data, "enter") && this.onSelect && this.getSelectedItem()) this.onSelect(this.getSelectedItem());
    else if (matchesKey(data, "escape") && this.onCancel) this.onCancel();
  }
  invalidate() {}
  dispose() {}
  render(width = 80) {
    const visible = this.filteredItems.slice(0, this.maxVisible);
    if (visible.length === 0) return ["No matching items"];
    return visible.map((item, index) => {
      const selected = index === this.selectedIndex ? "> " : "  ";
      const label = String(item.label || item.value || "");
      const value = item.value !== undefined && String(item.value) !== label ? ` ${item.value}` : "";
      const description = item.description ? ` - ${item.description}` : "";
      return truncateToWidth(`${selected}${label}${value}${description}`, width, "");
    });
  }
}

class SelectList extends GenericSelectList {}

class SettingsList extends GenericSelectList {
  render(width = 80) {
    const visible = this.filteredItems.slice(0, this.maxVisible);
    if (visible.length === 0) return ["No matching settings"];
    return visible.map((item, index) => {
      const selected = index === this.selectedIndex ? "> " : "  ";
      const label = String(item.label || item.key || item.value || "");
      const currentValue = item.currentValue !== undefined ? ` = ${item.currentValue}` : "";
      const description = item.description ? ` - ${item.description}` : "";
      return truncateToWidth(`${selected}${label}${currentValue}${description}`, width, "");
    });
  }
}

class Input extends BridgeTextComponent {
  constructor(value = "", placeholder = "") {
    super(value);
    this.value = String(value || "");
    this.placeholder = String(placeholder || "");
    this.cursor = this.value.length;
    this.focused = false;
  }
  setValue(value) {
    this.value = String(value || "");
    this.content = this.value;
    this.cursor = Math.min(this.cursor, this.value.length);
  }
  getValue() { return this.value; }
  handleInput(data) {
    if (matchesKey(data, "backspace")) {
      this.value = this.value.slice(0, Math.max(0, this.value.length - 1));
    } else if (typeof data === "string" && data.length === 1 && data >= " ") {
      this.value += data;
    }
    this.content = this.value;
    this.cursor = this.value.length;
  }
  render(width = 80) {
    const text = this.value || this.placeholder;
    const marker = this.focused ? CURSOR_MARKER : "";
    return [truncateToWidth(`${text}${marker}`, width, "")];
  }
}

class Editor extends BridgeTextComponent {
  constructor(options = {}) {
    super("");
    this.options = options;
    this.value = "";
    this.cursor = 0;
    this.focused = false;
  }
  setText(text) {
    this.value = String(text || "");
    this.content = this.value;
    this.cursor = this.value.length;
  }
  getText() { return this.value; }
  handleInput(data) {
    if (matchesKey(data, "backspace")) this.setText(this.value.slice(0, -1));
    else if (typeof data === "string" && data.length === 1 && data >= " ") this.setText(this.value + data);
  }
}

class ImageComponent extends BridgeTextComponent {
  constructor(_data = undefined, options = {}) {
    super(options.alt || options.label || "[image]");
    this.options = options;
  }
}

class Loader extends BridgeTextComponent {
  constructor(text = "Loading") {
    super(text);
  }
}

class CancellableLoader extends Loader {}

class TruncatedText extends BridgeTextComponent {
  render(width = 80) {
    return [truncateToWidth(String(this.content || ""), width)];
  }
}

class TUI {}
class ProcessTerminal {}

const CURSOR_MARKER = "\x1b_pi:c\x07";

const Key = {
  escape: "escape",
  esc: "esc",
  enter: "enter",
  return: "return",
  tab: "tab",
  space: "space",
  backspace: "backspace",
  delete: "delete",
  insert: "insert",
  clear: "clear",
  home: "home",
  end: "end",
  pageUp: "pageUp",
  pageDown: "pageDown",
  up: "up",
  down: "down",
  left: "left",
  right: "right",
  ctrl: (key) => `ctrl+${key}`,
  alt: (key) => `alt+${key}`,
  shift: (key) => `shift+${key}`,
  super: (key) => `super+${key}`,
  ctrlShift: (key) => `ctrl+shift+${key}`,
  ctrlAlt: (key) => `ctrl+alt+${key}`,
  ctrlSuper: (key) => `ctrl+super+${key}`,
};

function normalizeKeyId(keyId) {
  return String(keyId || "").toLowerCase().replace(/^esc$/, "escape").replace(/^return$/, "enter");
}

function keySequenceName(data) {
  if (data === "\x1b") return "escape";
  if (data === "\n" || data === "\r") return "enter";
  if (data === "\t") return "tab";
  if (data === " " || data === "\x00") return "space";
  if (data === "\x7f" || data === "\b") return "backspace";
  if (data === "\x1b[A" || data === "\x1bOA") return "up";
  if (data === "\x1b[B" || data === "\x1bOB") return "down";
  if (data === "\x1b[D" || data === "\x1bOD") return "left";
  if (data === "\x1b[C" || data === "\x1bOC") return "right";
  return "";
}

function matchesKey(data, keyId) {
  const wanted = normalizeKeyId(keyId);
  const text = String(data || "");
  if (!wanted) return false;
  if (wanted.length === 1) return text.toLowerCase() === wanted;
  const named = keySequenceName(text);
  if (named && named === wanted) return true;
  const ctrl = wanted.match(/^ctrl\+(.+)$/);
  if (ctrl && ctrl[1].length === 1) {
    const code = ctrl[1].toLowerCase().charCodeAt(0);
    return text === String.fromCharCode(code - 96);
  }
  const shift = wanted.match(/^shift\+(.+)$/);
  if (shift && shift[1].length === 1) return text === shift[1].toUpperCase();
  const alt = wanted.match(/^alt\+(.+)$/);
  if (alt && alt[1].length === 1) return text === `\x1b${alt[1]}`;
  return false;
}

function parseKey(data) {
  return keySequenceName(String(data || "")) || (String(data || "").length === 1 ? String(data) : undefined);
}

function isKeyRelease(data) {
  return /[;:]3[~u]$/.test(String(data || ""));
}

function isKeyRepeat(data) {
  return /[;:]2[~u]$/.test(String(data || ""));
}

function visibleWidth(text) {
  return String(text || "")
    .replace(/\x1b\[[0-?]*[ -/]*[@-~]/g, "")
    .replace(/\x1b_pi:c\x07/g, "")
    .length;
}

function truncateToWidth(text, width = 80, suffix = "…") {
  const value = String(text || "");
  const limit = Math.max(0, Number(width) || 0);
  if (visibleWidth(value) <= limit) return value;
  const tail = String(suffix === undefined ? "…" : suffix);
  const tailWidth = visibleWidth(tail);
  const keep = Math.max(0, limit - tailWidth);
  return Array.from(value).slice(0, keep).join("") + tail;
}

function wrapTextWithAnsi(text, width = 80) {
  const limit = Math.max(1, Number(width) || 80);
  const lines = [];
  for (const rawLine of String(text || "").split(/\r?\n/)) {
    let current = "";
    for (const char of Array.from(rawLine)) {
      if (visibleWidth(current + char) > limit) {
        lines.push(current);
        current = char;
      } else {
        current += char;
      }
    }
    lines.push(current);
  }
  return lines;
}

class CustomEditor extends BridgeTextComponent {
  constructor(_tui, _theme, _keybindings, options = {}) {
    super("");
    this.options = options;
    this.text = "";
    this.actionHandlers = new Map();
  }
  onAction(action, handler) {
    if (typeof handler === "function") this.actionHandlers.set(String(action || ""), handler);
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

function sdkMessageText(content) {
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return "";
  return content
    .filter((block) => block && typeof block === "object" && block.type === "text")
    .map((block) => String(block.text || ""))
    .join("");
}

function sdkNewEntryId(prefix = "entry") {
  return `${prefix}-${Date.now().toString(36)}-${Math.floor(Math.random() * 0xffffff).toString(36)}`;
}

function sdkEntryMovesLeaf(entry) {
  return entry && entry.type && entry.type !== "session" && entry.type !== "leaf" && typeof entry.id === "string" && entry.id;
}

function inferSdkLeafId(entries = []) {
  let leafId = null;
  for (const entry of entries) {
    if (!entry || typeof entry !== "object") continue;
    if (entry.type === "leaf") leafId = entry.targetId || null;
    else if (sdkEntryMovesLeaf(entry) && entry.id) leafId = entry.id;
  }
  return leafId;
}

function normalizeSdkSessionEntries(rows = []) {
  const entries = [];
  let previousLeaf = null;
  for (const row of rows) {
    if (!row || typeof row !== "object") continue;
    let entry = row;
    if (typeof row.role === "string") entry = { type: "message", message: row };
    if (!entry.type || entry.type === "session") continue;
    const full = {
      id: entry.id || sdkNewEntryId(entry.type || "entry"),
      parentId: Object.prototype.hasOwnProperty.call(entry, "parentId") ? entry.parentId : previousLeaf,
      timestamp: entry.timestamp || new Date().toISOString(),
      ...entry,
    };
    entries.push(full);
    if (sdkEntryMovesLeaf(full)) previousLeaf = full.id;
  }
  return entries;
}

function createSdkSessionManagerFromSession(session) {
  const append = (entry) => {
    const previousLeaf = session.leafId;
    const full = {
      id: entry.id || sdkNewEntryId(entry.type || "entry"),
      parentId: Object.prototype.hasOwnProperty.call(entry, "parentId") ? entry.parentId : previousLeaf,
      timestamp: entry.timestamp || new Date().toISOString(),
      ...entry,
    };
    session.entries.push(full);
    if (sdkEntryMovesLeaf(full)) session.leafId = full.id;
    persistSdkSession(session);
    return full;
  };
  const manager = createSessionManager({ session });
  const appendId = (entry) => append(entry).id;
  const appendSessionInfo = (name) => {
    const trimmed = String(name || "").trim();
    session.name = trimmed || undefined;
    return appendId({ type: "session_info", name: trimmed });
  };
  const resetSessionStateFromFile = (filePath, sessionDir = undefined, cwdOverride = undefined) => {
    const loaded = loadSdkSessionStateFromJsonl(filePath, sessionDir, cwdOverride);
    session.id = loaded.id;
    session.timestamp = loaded.timestamp;
    session.cwd = loaded.cwd;
    session.sessionDir = loaded.sessionDir;
    session.name = loaded.name;
    session.parentSession = loaded.parentSession;
    session.entries = loaded.entries;
    session.leafId = loaded.leafId;
    session.path = loaded.path;
  };
  return {
    setSessionFile: (filePath) => {
      const resolvedPath = path.resolve(String(filePath || ""));
      if (fs.existsSync(resolvedPath)) {
        resetSessionStateFromFile(resolvedPath, session.sessionDir || path.dirname(resolvedPath));
      } else {
        const timestamp = new Date().toISOString();
        session.id = sdkNewEntryId("session");
        session.timestamp = timestamp;
        session.entries = [];
        session.leafId = null;
        session.name = undefined;
        session.parentSession = undefined;
        session.sessionDir = session.sessionDir || path.dirname(resolvedPath);
        session.path = resolvedPath;
      }
    },
    getCwd: manager.getCwd,
    getSessionDir: manager.getSessionDir,
    getSessionId: manager.getSessionId,
    getSessionFile: manager.getSessionFile,
    getLeafId: manager.getLeafId,
    getLeafEntry: manager.getLeafEntry,
    getEntry: manager.getEntry,
    getLabel: manager.getLabel,
    getBranch: manager.getBranch,
    getChildren: manager.getChildren,
    getHeader: manager.getHeader,
    getEntries: () => session.entries.slice(),
    getTree: manager.getTree,
    getSessionName: () => session.name,
    setSessionName: appendSessionInfo,
    appendSessionInfo,
    appendSessionName: appendSessionInfo,
    isPersisted: () => session.persist !== false && !!session.path,
    _persist: () => persistSdkSession(session),
    buildSessionContext: (leafId = undefined) => buildSessionContext(session.entries, leafId === undefined ? session.leafId : leafId),
    appendMessage: (message) => appendId({ type: "message", message }),
    appendThinkingLevelChange: (thinkingLevel) => appendId({ type: "thinking_level_change", thinkingLevel }),
    appendModelChange: (provider, modelId) => appendId({ type: "model_change", provider, modelId }),
    appendCustomEntry: (customType, data) => appendId({ type: "custom", customType, data }),
    appendCustomMessageEntry: (customType, content, display = true, details = undefined) => appendId({ type: "custom_message", customType, content, display, details }),
    appendLabelChange: (targetId, label) => {
      const id = String(targetId || "");
      if (!manager.getEntry(id)) throw new Error(`Entry ${id} not found`);
      return appendId({ type: "label", targetId: id, label });
    },
    appendLabel: (targetId, label) => {
      const id = String(targetId || "");
      if (!manager.getEntry(id)) throw new Error(`Entry ${id} not found`);
      return appendId({ type: "label", targetId: id, label });
    },
    appendCompaction: (summary, firstKeptEntryId, tokensBefore, details = undefined, fromHook = false) => appendId({ type: "compaction", summary, firstKeptEntryId, tokensBefore, details, fromHook }),
    branch: (targetId) => {
      if (targetId && !manager.getEntry(targetId)) throw new Error(`Entry ${targetId} not found`);
      session.leafId = targetId || null;
    },
    resetLeaf: () => {
      session.leafId = null;
    },
    branchWithSummary: (branchFromId, summary, details = undefined, fromHook = undefined) => {
      const targetId = branchFromId === null || branchFromId === undefined ? null : String(branchFromId);
      if (targetId !== null && !manager.getEntry(targetId)) throw new Error(`Entry ${targetId} not found`);
      session.leafId = targetId;
      return appendId({
        type: "branch_summary",
        parentId: targetId,
        fromId: targetId || "root",
        summary: String(summary || ""),
        details,
        fromHook,
      });
    },
    createBranchedSession: (leafId) => {
      const branch = manager.getBranch(leafId);
      if (branch.length === 0) throw new Error(`Entry ${leafId} not found`);
      const pathEntries = branch.filter((entry) => entry && entry.type !== "label");
      const pathIds = new Set(pathEntries.map((entry) => entry.id).filter(Boolean));
      const labelEntries = session.entries.filter((entry) => entry && entry.type === "label" && pathIds.has(entry.targetId));
      const previousSessionFile = session.path;
      const id = sdkNewEntryId("session");
      const timestamp = new Date().toISOString();
      session.id = id;
      session.timestamp = timestamp;
      session.entries = [...pathEntries, ...labelEntries].map((entry) => ({ ...entry }));
      session.leafId = inferSdkLeafId(session.entries);
      session.parentSession = previousSessionFile;
      session.path = session.persist === false || !session.sessionDir ? undefined : sdkSessionFilePath(session.sessionDir, id, timestamp);
      persistSdkSession(session);
      return session.path;
    },
    newSession: (options = {}) => {
      const timestamp = new Date().toISOString();
      session.id = options.id || sdkNewEntryId("session");
      session.timestamp = timestamp;
      session.entries = [];
      session.leafId = null;
      session.parentSession = options.parentSession;
      session.path = session.persist === false || !session.sessionDir ? undefined : sdkSessionFilePath(session.sessionDir, session.id, timestamp);
      return session.path;
    },
    _session: session,
  };
}

function createSdkSessionManager(cwd = process.cwd(), sessionDir = path.join(getAgentDir(), "sessions", "--sdk--")) {
  const id = sdkNewEntryId("session");
  const timestamp = new Date().toISOString();
  return createSdkSessionManagerFromSession({
    id,
    timestamp,
    cwd: path.resolve(cwd || process.cwd()),
    sessionDir,
    name: undefined,
    entries: [],
    leafId: null,
    path: sessionDir ? sdkSessionFilePath(sessionDir, id, timestamp) : undefined,
  });
}

function loadSdkSessionStateFromJsonl(filePath, sessionDir = undefined, cwdOverride = undefined) {
  const rows = readJsonLines(filePath);
  if (rows.length === 0) throw new Error(`Session file is empty or invalid: ${filePath}`);
  const hasHeader = rows[0] && rows[0].type === "session";
  const header = hasHeader ? rows[0] : {};
  const allRows = rows.slice();
  migrateSessionEntries(allRows);
  const entries = normalizeSdkSessionEntries(hasHeader ? allRows.slice(1) : allRows);
  const cwd = path.resolve(cwdOverride || header.cwd || process.cwd());
  const session = {
    id: String(header.id || header.sessionId || path.basename(filePath, ".jsonl")),
    timestamp: header.timestamp || new Date().toISOString(),
    cwd,
    sessionDir: sessionDir || path.dirname(filePath),
    name: typeof header.name === "string" && header.name ? header.name : undefined,
    parentSession: header.parentSession,
    entries,
    leafId: typeof header.leafId === "string" && header.leafId ? header.leafId : inferSdkLeafId(entries),
    path: filePath,
  };
  for (const entry of entries) {
    if (entry && entry.type === "session_info" && typeof entry.name === "string") session.name = entry.name || undefined;
  }
  return session;
}

function createSdkSessionManagerFromJsonl(filePath, sessionDir = undefined, cwdOverride = undefined) {
  const session = loadSdkSessionStateFromJsonl(filePath, sessionDir, cwdOverride);
  return createSdkSessionManagerFromSession(session);
}

function sdkToolInfo(tool) {
  return {
    name: tool.name,
    label: tool.label || tool.name,
    description: tool.description || "",
    source: tool.source || "sdk",
    sourceInfo: tool.sourceInfo,
  };
}

class AgentSession {
  constructor(config = {}) {
    this.agent = config.agent || { state: { messages: [], model: config.model, thinkingLevel: config.thinkingLevel || "off" } };
    this.sessionManager = config.sessionManager || createSdkSessionManager(config.cwd || process.cwd());
    if (!config.agent && this.sessionManager.buildSessionContext) {
      const existingSession = this.sessionManager.buildSessionContext();
      this.agent.state.messages = existingSession.messages || [];
      if (config.thinkingLevel === undefined && existingSession.thinkingLevel) this.agent.state.thinkingLevel = existingSession.thinkingLevel;
      if (!config.model && existingSession.model) {
        this.agent.state.model = {
          provider: existingSession.model.provider,
          id: existingSession.model.modelId,
          modelId: existingSession.model.modelId,
        };
      }
    }
    this.settingsManager = config.settingsManager || SettingsManager.create(config.cwd || process.cwd(), getAgentDir());
    this.cwd = path.resolve(config.cwd || this.sessionManager.getCwd?.() || process.cwd());
    this.resourceLoader = config.resourceLoader || new DefaultResourceLoader({ cwd: this.cwd, agentDir: getAgentDir(), settingsManager: this.settingsManager });
    this.modelRegistry = config.modelRegistry || ModelRegistry.create(AuthStorage.create());
    this.listeners = new Set();
    this.disposed = false;
    this.steering = [];
    this.followUpQueue = [];
    this.autoCompactionEnabled = this.settingsManager.getCompactionEnabled ? this.settingsManager.getCompactionEnabled() : true;
    this.autoRetryEnabled = this.settingsManager.getRetryEnabled ? this.settingsManager.getRetryEnabled() : true;
    this.scopedModels = config.scopedModels || [];
    this.activeToolNames = new Set(config.initialActiveToolNames || config.tools || ["read", "bash", "edit", "write"]);
    this.allowedToolNames = Array.isArray(config.allowedToolNames) ? new Set(config.allowedToolNames) : undefined;
    this.customTools = Array.isArray(config.customTools) ? config.customTools : [];
    this.sessionStartEvent = config.sessionStartEvent || { type: "session_start", reason: "startup" };
    this.extensionBindings = {};
    this.extensionErrorUnsubscriber = undefined;
    const extensionsResult = this.resourceLoader.getExtensions ? this.resourceLoader.getExtensions() : { extensions: [], runtime: createExtensionRuntime() };
    const extensionRuntime = extensionsResult.runtime || createExtensionRuntime();
    this.extensionRunner = new ExtensionRunner(extensionsResult.extensions || [], extensionRuntime, this.cwd, this.sessionManager, this.modelRegistry);
    this.bindExtensionCore();
    this.refreshToolRegistry({ includeAllExtensionTools: true });
  }

  get sessionId() { return this.sessionManager.getSessionId ? this.sessionManager.getSessionId() : undefined; }
  get sessionFile() { return this.sessionManager.getSessionFile ? this.sessionManager.getSessionFile() : undefined; }
  get sessionName() { return this.sessionManager.getSessionName ? this.sessionManager.getSessionName() : undefined; }
  get state() { return this.agent.state; }
  get model() { return this.agent.state.model; }
  get thinkingLevel() { return this.agent.state.thinkingLevel || "off"; }
  get isStreaming() { return !!this.agent.state.isStreaming; }
  get systemPrompt() { return this.agent.state.systemPrompt || ""; }
  get retryAttempt() { return 0; }
  get isCompacting() { return false; }
  get messages() { return this.agent.state.messages || []; }
  get steeringMode() { return this.agent.steeringMode || "one-at-a-time"; }
  get followUpMode() { return this.agent.followUpMode || "one-at-a-time"; }
  get pendingMessageCount() { return this.steering.length + this.followUpQueue.length; }
  get isRetrying() { return false; }
  get isBashRunning() { return false; }
  get hasPendingBashMessages() { return false; }

  subscribe(listener) {
    if (typeof listener !== "function") return () => {};
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  }

  emit(event) {
    for (const listener of [...this.listeners]) {
      try { listener(event); } catch {}
    }
  }

  dispose() {
    this.disposed = true;
    if (this.extensionRunner && typeof this.extensionRunner.invalidate === "function") this.extensionRunner.invalidate("AgentSession disposed.");
    this.listeners.clear();
  }

  allToolDefinitions() {
    const builtins = createCodingTools(this.cwd);
    const extensions = this.extensionRunner.getAllRegisteredTools().map((registered) => registered.definition);
    return [...builtins, ...this.customTools, ...extensions].filter((tool) => tool && tool.name && (!this.allowedToolNames || this.allowedToolNames.has(tool.name)));
  }

  refreshToolRegistry(options = {}) {
    const known = new Set(this.allToolDefinitions().map((tool) => tool.name));
    const next = new Set([...this.activeToolNames].filter((name) => known.has(name)));
    for (const name of options.activeToolNames || []) if (known.has(name)) next.add(name);
    if (options.includeAllExtensionTools) {
      for (const registered of this.extensionRunner.getAllRegisteredTools()) {
        const name = registered && registered.definition && registered.definition.name;
        if (name && known.has(name)) next.add(name);
      }
    }
    this.activeToolNames = next;
  }

  getCommands() {
    const extensionCommands = this.extensionRunner.getRegisteredCommands().map((command) => ({
      name: command.invocationName || command.name,
      slashCommand: `/${command.invocationName || command.name}`,
      description: command.description || "",
      source: "extension",
      sourceInfo: command.sourceInfo,
    }));
    const prompts = this.resourceLoader.getPrompts ? this.resourceLoader.getPrompts().prompts.map((prompt) => ({
      name: prompt.name,
      slashCommand: `/${prompt.name}`,
      description: prompt.description || "",
      source: "prompt",
      sourceInfo: prompt.sourceInfo,
    })) : [];
    const skills = this.resourceLoader.getSkills ? this.resourceLoader.getSkills().skills.map((skill) => ({
      name: `skill:${skill.name}`,
      slashCommand: `/skill:${skill.name}`,
      description: skill.description || "",
      source: "skill",
      sourceInfo: skill.sourceInfo,
    })) : [];
    return [...extensionCommands, ...prompts, ...skills];
  }

  bindExtensionCore() {
    const runner = this.extensionRunner;
    runner.bindCore(
      {
        sendMessage: (message, options = {}) => {
          try { this.sendCustomMessage(message, options); }
          catch (error) { runner.emitError({ extensionPath: "<runtime>", event: "send_message", error: error && error.message ? error.message : String(error) }); }
        },
        sendUserMessage: (content, options = {}) => {
          this.sendUserMessage(content, { triggerTurn: !!options.triggerTurn }).catch((error) => {
            runner.emitError({ extensionPath: "<runtime>", event: "send_user_message", error: error && error.message ? error.message : String(error) });
          });
        },
        appendEntry: (customType, data) => {
          if (this.sessionManager.appendCustomEntry) this.sessionManager.appendCustomEntry(customType, data);
        },
        setSessionName: (name) => this.setSessionName(name),
        getSessionName: () => this.sessionName,
        setLabel: (entryId, label) => {
          if (this.sessionManager.appendLabelChange) this.sessionManager.appendLabelChange(entryId, label);
        },
        getActiveTools: () => this.getActiveToolNames(),
        getAllTools: () => this.getAllTools(),
        setActiveTools: (toolNames) => this.setActiveToolsByName(toolNames),
        refreshTools: () => this.refreshToolRegistry({ includeAllExtensionTools: true }),
        getCommands: () => this.getCommands(),
        setModel: async (model) => {
          const spec = normalizeModelSpec(model) || {};
          const provider = spec.provider || (model && model.provider);
          const id = spec.model || (model && (model.id || model.modelId || model.model));
          if (!provider || !id) return false;
          const found = this.modelRegistry.find ? this.modelRegistry.find(provider, id) : undefined;
          await this.setModel(found || { provider, id, name: id });
          if (spec.thinking) this.setThinkingLevel(spec.thinking);
          return true;
        },
        getThinkingLevel: () => this.thinkingLevel,
        setThinkingLevel: (level) => this.setThinkingLevel(level),
      },
      {
        getModel: () => this.model,
        isIdle: () => !this.isStreaming,
        getSignal: () => this.agent.signal,
        abort: () => { void this.abort(); },
        hasPendingMessages: () => this.pendingMessageCount > 0,
        shutdown: () => {
          const handler = this.extensionBindings && this.extensionBindings.shutdownHandler;
          if (typeof handler === "function") handler();
        },
        getContextUsage: () => this.getContextUsage(),
        compact: (options = {}) => {
          void this.compact(options.customInstructions).then((result) => {
            if (typeof options.onComplete === "function") options.onComplete(result);
          }, (error) => {
            if (typeof options.onError === "function") options.onError(error);
          });
        },
        getSystemPrompt: () => this.systemPrompt,
      },
      {
        registerProvider: (name, config) => {
          if (this.modelRegistry.registerProvider) this.modelRegistry.registerProvider(name, config);
        },
        unregisterProvider: (name) => {
          if (this.modelRegistry.unregisterProvider) this.modelRegistry.unregisterProvider(name);
        },
      },
    );
  }

  getActiveToolNames() { return [...this.activeToolNames]; }
  getAllTools() { return this.allToolDefinitions().map(sdkToolInfo); }
  getToolDefinition(name) { return this.allToolDefinitions().find((tool) => tool.name === name); }
  setActiveToolsByName(toolNames) {
    this.activeToolNames = new Set((toolNames || []).map(String));
    this.emit({ type: "tools_changed", tools: this.getActiveToolNames() });
  }
  setScopedModels(scopedModels) { this.scopedModels = Array.isArray(scopedModels) ? scopedModels : []; }

  async prompt(text, options = {}) {
    return this.sendUserMessage({ role: "user", content: [{ type: "text", text: String(text || "") }, ...(options.images || [])], timestamp: Date.now() }, { triggerTurn: true });
  }

  async steer(text, images = []) {
    this.steering.push(String(text || ""));
    this.emit({ type: "queue_update", steering: this.steering.slice(), followUp: this.followUpQueue.slice() });
    if (text) await this.sendUserMessage({ role: "user", content: [{ type: "text", text: String(text) }, ...images], timestamp: Date.now() }, { triggerTurn: false });
  }

  async followUp(text, images = []) {
    this.followUpQueue.push(String(text || ""));
    this.emit({ type: "queue_update", steering: this.steering.slice(), followUp: this.followUpQueue.slice() });
    if (text) await this.sendUserMessage({ role: "user", content: [{ type: "text", text: String(text) }, ...images], timestamp: Date.now() }, { triggerTurn: false });
  }

  async sendUserMessage(message, options = {}) {
    const msg = message && message.role ? message : { role: "user", content: String(message || ""), timestamp: Date.now() };
    this.agent.state.messages.push(msg);
    if (this.sessionManager.appendMessage) this.sessionManager.appendMessage(msg);
    this.emit({ type: "message", message: msg });
    if (options.triggerTurn) {
      this.emit({ type: "turn_start", message: msg });
      this.emit({ type: "agent_end", messages: this.agent.state.messages.slice(), willRetry: false });
    }
  }

  sendCustomMessage(message, options = {}) {
    const raw = message && typeof message === "object" ? message : { content: String(message || "") };
    const msg = {
      role: "custom",
      customType: raw.customType || raw.type || "custom",
      content: raw.content !== undefined ? raw.content : (raw.text !== undefined ? raw.text : ""),
      display: raw.display !== false,
      details: raw.details,
      timestamp: Date.now(),
    };
    this.agent.state.messages.push(msg);
    if (this.sessionManager.appendCustomMessageEntry) {
      this.sessionManager.appendCustomMessageEntry(msg.customType, msg.content, msg.display, msg.details);
    }
    this.emit({ type: "message_start", message: msg });
    this.emit({ type: "message_end", message: msg });
    if (options.triggerTurn) this.emit({ type: "agent_end", messages: this.agent.state.messages.slice(), willRetry: false });
  }

  clearQueue() {
    const queues = { steering: this.steering.slice(), followUp: this.followUpQueue.slice() };
    this.steering = [];
    this.followUpQueue = [];
    this.emit({ type: "queue_update", steering: [], followUp: [] });
    return queues;
  }
  getSteeringMessages() { return this.steering.slice(); }
  getFollowUpMessages() { return this.followUpQueue.slice(); }
  async abort() { this.emit({ type: "abort" }); }

  async setModel(model) {
    this.agent.state.model = model;
    if (model && this.sessionManager.appendModelChange) this.sessionManager.appendModelChange(model.provider, model.id || model.modelId);
    this.emit({ type: "model_changed", model });
  }

  async cycleModel(direction = "forward") {
    const models = this.scopedModels.length ? this.scopedModels : this.modelRegistry.getAvailable().map((model) => ({ model }));
    if (!models.length) return undefined;
    const current = this.agent.state.model;
    const idx = models.findIndex((entry) => current && entry.model.provider === current.provider && entry.model.id === current.id);
    const nextIdx = direction === "backward" ? (idx <= 0 ? models.length - 1 : idx - 1) : ((idx + 1) % models.length);
    const selected = models[nextIdx];
    await this.setModel(selected.model);
    if (selected.thinkingLevel) this.setThinkingLevel(selected.thinkingLevel);
    return { model: selected.model, thinkingLevel: this.thinkingLevel, isScoped: this.scopedModels.length > 0 };
  }

  setThinkingLevel(level) {
    this.agent.state.thinkingLevel = String(level || "off");
    if (this.sessionManager.appendThinkingLevelChange) this.sessionManager.appendThinkingLevelChange(this.agent.state.thinkingLevel);
    this.emit({ type: "thinking_level_changed", level: this.agent.state.thinkingLevel });
  }
  cycleThinkingLevel() {
    const levels = ["off", "minimal", "low", "medium", "high", "xhigh"];
    const next = levels[(levels.indexOf(this.thinkingLevel) + 1) % levels.length];
    this.setThinkingLevel(next);
    return next;
  }
  getAvailableThinkingLevels() { return ["off", "minimal", "low", "medium", "high", "xhigh"]; }
  supportsThinking() { return !!(this.agent.state.model && this.agent.state.model.reasoning); }
  setSteeringMode(mode) { this.agent.steeringMode = mode; }
  setFollowUpMode(mode) { this.agent.followUpMode = mode; }
  async compact(customInstructions) {
    const messages = this.agent.state.messages.slice();
    const summary = customInstructions ? `Manual compaction: ${customInstructions}` : serializeConversation(convertToLlm(messages));
    const result = { summary, firstKeptEntryId: this.sessionManager.getLeafId?.() || "", tokensBefore: estimateContextTokens(messages).tokens };
    if (this.sessionManager.appendCompaction) this.sessionManager.appendCompaction(result.summary, result.firstKeptEntryId, result.tokensBefore);
    return result;
  }
  abortCompaction() {}
  abortBranchSummary() {}
  setAutoCompactionEnabled(enabled) { this.autoCompactionEnabled = !!enabled; }
  applyExtensionBindings(bindings = {}) {
    this.extensionBindings = bindings || {};
    if (this.extensionRunner.setUIContext) this.extensionRunner.setUIContext(this.extensionBindings.uiContext);
    if (this.extensionRunner.bindCommandContext) this.extensionRunner.bindCommandContext(this.extensionBindings.commandContextActions);
    if (typeof this.extensionErrorUnsubscriber === "function") this.extensionErrorUnsubscriber();
    this.extensionErrorUnsubscriber = typeof this.extensionBindings.onError === "function"
      ? this.extensionRunner.onError(this.extensionBindings.onError)
      : undefined;
  }
  async bindExtensions(bindings = {}) {
    this.applyExtensionBindings(bindings);
    await this.extensionRunner.emit({
      ...this.sessionStartEvent,
      sessionId: this.sessionId,
      sessionFile: this.sessionFile,
    });
    this.refreshToolRegistry({ includeAllExtensionTools: true });
  }
  async reload() {
    if (this.resourceLoader.reload) await this.resourceLoader.reload();
    const extensionsResult = this.resourceLoader.getExtensions ? this.resourceLoader.getExtensions() : { extensions: [], runtime: createExtensionRuntime() };
    if (this.extensionRunner && this.extensionRunner.invalidate) this.extensionRunner.invalidate();
    this.extensionRunner = new ExtensionRunner(extensionsResult.extensions || [], extensionsResult.runtime || createExtensionRuntime(), this.cwd, this.sessionManager, this.modelRegistry);
    this.bindExtensionCore();
    this.applyExtensionBindings(this.extensionBindings);
    this.refreshToolRegistry({ includeAllExtensionTools: true });
    await this.extensionRunner.emit({ type: "session_start", reason: "reload", sessionId: this.sessionId, sessionFile: this.sessionFile });
  }
  abortRetry() {}
  setAutoRetryEnabled(enabled) { this.autoRetryEnabled = !!enabled; }
  async executeBash(command, options = {}) {
    const operations = createLocalBashOperations();
    let output = "";
    const result = await operations.exec(String(command || ""), this.cwd, { onData: (chunk) => { output += Buffer.from(chunk).toString("utf8"); } });
    const bashResult = { output, exitCode: result.exitCode, code: result.exitCode, killed: false };
    this.recordBashResult(command, bashResult, options);
    return bashResult;
  }
  recordBashResult(command, result, options = {}) {
    const message = {
      role: "bashExecution",
      command: String(command || ""),
      output: result.output || "",
      exitCode: result.exitCode,
      cancelled: false,
      truncated: false,
      timestamp: Date.now(),
      excludeFromContext: !!options.excludeFromContext,
    };
    if (!message.excludeFromContext) {
      this.agent.state.messages.push(message);
      if (this.sessionManager.appendMessage) this.sessionManager.appendMessage(message);
    }
  }
  abortBash() {}
  setSessionName(name) { if (this.sessionManager.setSessionName) this.sessionManager.setSessionName(name); }
  async navigateTree(entryId) {
    if (this.sessionManager.branch) this.sessionManager.branch(entryId);
    this.agent.state.messages = this.sessionManager.buildSessionContext ? this.sessionManager.buildSessionContext(entryId).messages : this.agent.state.messages;
    return { cancelled: false };
  }
  getUserMessagesForForking() {
    return (this.sessionManager.getEntries ? this.sessionManager.getEntries() : [])
      .filter((entry) => entry.type === "message" && entry.message && entry.message.role === "user")
      .map((entry) => ({ entryId: entry.id, text: sdkMessageText(entry.message.content) }));
  }
  getSessionStats() {
    const messages = this.agent.state.messages || [];
    return {
      sessionFile: this.sessionFile,
      sessionId: this.sessionId,
      userMessages: messages.filter((message) => message.role === "user").length,
      assistantMessages: messages.filter((message) => message.role === "assistant").length,
      totalMessages: messages.length,
      contextTokens: this.getContextUsage().tokens,
    };
  }
  getContextUsage() { return estimateContextTokens(this.agent.state.messages || []); }
  async exportToHtml(outputPath = undefined) {
    const target = outputPath || path.join(this.cwd, "session.html");
    ensureParentDir(target);
    fs.writeFileSync(target, `<html><body><pre>${escapeXml(serializeConversation(convertToLlm(this.agent.state.messages || [])))}</pre></body></html>`, "utf8");
    return target;
  }
  exportToJsonl(outputPath = undefined) {
    const target = outputPath || path.join(this.cwd, "session.jsonl");
    ensureParentDir(target);
    const header = { type: "session", version: CURRENT_SESSION_VERSION, id: this.sessionId || sdkNewEntryId("session"), timestamp: new Date().toISOString(), cwd: this.cwd };
    const lines = [header, ...(this.sessionManager.getEntries ? this.sessionManager.getEntries() : [])].map((entry) => JSON.stringify(entry));
    fs.writeFileSync(target, `${lines.join("\n")}\n`, "utf8");
    return target;
  }
  getLastAssistantText() {
    for (let i = this.agent.state.messages.length - 1; i >= 0; i--) {
      const message = this.agent.state.messages[i];
      if (message.role === "assistant") return sdkMessageText(message.content);
    }
    return undefined;
  }
  createReplacedSessionContext() {
    return {
      sendMessage: (message, options) => this.sendCustomMessage(message, options),
      sendUserMessage: (content, options) => this.sendUserMessage(content, options),
      sessionManager: this.sessionManager,
      getSessionName: () => this.sessionManager.getSessionName?.(),
      setSessionName: (name) => this.setSessionName(name),
    };
  }
  hasExtensionHandlers(eventType) { return this.extensionRunner && this.extensionRunner.hasHandlers(eventType); }
}

async function createAgentSessionServices(options = {}) {
  const cwd = path.resolve(options.cwd || process.cwd());
  const agentDir = expandBridgeTilde(options.agentDir || getAgentDir());
  const settingsManager = options.settingsManager || SettingsManager.create(cwd, agentDir);
  const authStorage = options.authStorage || AuthStorage.create(path.join(agentDir, "auth.json"));
  const modelRegistry = options.modelRegistry || ModelRegistry.create(authStorage, path.join(agentDir, "models.json"));
  const sessionManager = options.sessionManager || createSdkSessionManager(cwd, path.join(agentDir, "sessions"));
  const resourceLoader = options.resourceLoader || new DefaultResourceLoader({ cwd, agentDir, settingsManager });
  if (resourceLoader.reload) await resourceLoader.reload();
  return {
    cwd,
    agentDir,
    settingsManager,
    authStorage,
    modelRegistry,
    sessionManager,
    resourceLoader,
    diagnostics: [],
  };
}

async function createAgentSessionFromServices(services, options = {}) {
  const session = new AgentSession({
    ...options,
    cwd: services.cwd,
    sessionManager: options.sessionManager || services.sessionManager,
    settingsManager: options.settingsManager || services.settingsManager,
    resourceLoader: options.resourceLoader || services.resourceLoader,
    modelRegistry: options.modelRegistry || services.modelRegistry,
    model: options.model,
    thinkingLevel: options.thinkingLevel,
  });
  const extensionsResult = services.resourceLoader && services.resourceLoader.getExtensions ? services.resourceLoader.getExtensions() : { extensions: [], errors: [] };
  return { session, extensionsResult, modelFallbackMessage: undefined };
}

async function createAgentSession(options = {}) {
  const services = await createAgentSessionServices(options);
  return createAgentSessionFromServices(services, options);
}

class AgentSessionRuntime {
  constructor(session, services, createRuntime, diagnostics = [], modelFallbackMessage = undefined) {
    this._session = session;
    this._services = services;
    this.createRuntime = createRuntime;
    this._diagnostics = diagnostics;
    this._modelFallbackMessage = modelFallbackMessage;
    this.rebindSession = undefined;
    this.beforeSessionInvalidate = undefined;
  }
  get services() { return this._services; }
  get session() { return this._session; }
  get cwd() { return this._services.cwd; }
  get diagnostics() { return this._diagnostics; }
  get modelFallbackMessage() { return this._modelFallbackMessage; }
  setRebindSession(fn) { this.rebindSession = fn; }
  setBeforeSessionInvalidate(fn) { this.beforeSessionInvalidate = fn; }
  async emitBeforeSwitch(reason, targetSessionFile = undefined) {
    const runner = this.session && this.session.extensionRunner;
    if (!runner || !runner.hasHandlers || !runner.hasHandlers("session_before_switch")) return { cancelled: false };
    const result = await runner.emit({ type: "session_before_switch", reason, targetSessionFile });
    return { cancelled: !!(result && (result.cancel || result.cancelled)) };
  }
  async emitBeforeFork(entryId, options = {}) {
    const runner = this.session && this.session.extensionRunner;
    if (!runner || !runner.hasHandlers || !runner.hasHandlers("session_before_fork")) return { cancelled: false };
    const result = await runner.emit({ type: "session_before_fork", entryId, position: options.position || "before" });
    return { cancelled: !!(result && (result.cancel || result.cancelled)) };
  }
  async emitShutdown(reason, targetSessionFile = undefined) {
    const runner = this.session && this.session.extensionRunner;
    if (runner && runner.hasHandlers && runner.hasHandlers("session_shutdown")) {
      await runner.emit({
        type: "session_shutdown",
        reason,
        targetSessionFile,
        sessionId: this.session.sessionId,
        sessionFile: this.session.sessionFile,
      });
    }
  }
  async applyResult(result, options = {}) {
    await this.emitShutdown(result.shutdownReason || "resume", result.session && result.session.sessionFile);
    if (this.beforeSessionInvalidate) this.beforeSessionInvalidate();
    if (this._session && typeof this._session.dispose === "function") this._session.dispose();
    this._session = result.session;
    this._services = result.services || this._services;
    this._diagnostics = result.diagnostics || [];
    this._modelFallbackMessage = result.modelFallbackMessage;
    if (options.rebind !== false && this.rebindSession) await this.rebindSession(this._session);
  }
  async newSession(options = {}) {
    const before = await this.emitBeforeSwitch("new");
    if (before.cancelled) return before;
    const sessionManager = createSdkSessionManager(this.cwd, this._services.sessionManager.getSessionDir?.());
    if (options.parentSession && sessionManager.newSession) sessionManager.newSession({ parentSession: options.parentSession });
    const previousSessionFile = this.session.sessionFile;
    await this.applyResult({
      ...(await this.createRuntime({
        cwd: this.cwd,
        agentDir: this.services.agentDir,
        sessionManager,
        sessionStartEvent: { type: "session_start", reason: "new", previousSessionFile },
      })),
      shutdownReason: "new",
    }, { rebind: false });
    if (options.setup) {
      await options.setup(this.session.sessionManager);
      if (this.session.sessionManager.buildSessionContext) {
        this.session.agent.state.messages = this.session.sessionManager.buildSessionContext().messages;
      }
    }
    if (this.rebindSession) await this.rebindSession(this._session);
    if (options.withSession) await options.withSession(this.session.createReplacedSessionContext());
    return { cancelled: false };
  }
  async switchSession(sessionPath, options = {}) {
    const resolvedPath = path.resolve(String(sessionPath || ""));
    if (!fs.existsSync(resolvedPath)) throw new Error(`Session file not found: ${resolvedPath}`);
    const before = await this.emitBeforeSwitch("resume", resolvedPath);
    if (before.cancelled) return before;
    const previousSessionFile = this.session.sessionFile;
    const sessionManager = createSdkSessionManagerFromJsonl(resolvedPath, path.dirname(resolvedPath), options.cwdOverride);
    await this.applyResult({
      ...(await this.createRuntime({
        cwd: sessionManager.getCwd(),
        agentDir: this.services.agentDir,
        sessionManager,
        sessionStartEvent: { type: "session_start", reason: "resume", previousSessionFile },
      })),
      shutdownReason: "resume",
    });
    if (options.withSession) await options.withSession(this.session.createReplacedSessionContext());
    return { cancelled: false };
  }
  async fork(entryId, options = {}) {
    const position = options.position || "before";
    const before = await this.emitBeforeFork(entryId, { position });
    if (before.cancelled) return before;
    const selectedEntry = this.session.sessionManager.getEntry ? this.session.sessionManager.getEntry(entryId) : undefined;
    if (!selectedEntry) throw new Error("Invalid entry ID for forking");
    let targetLeafId = null;
    let selectedText = undefined;
    if (position === "at") {
      targetLeafId = selectedEntry.id;
    } else {
      if (selectedEntry.type !== "message" || !selectedEntry.message || selectedEntry.message.role !== "user") {
        throw new Error("Invalid entry ID for forking");
      }
      targetLeafId = selectedEntry.parentId || null;
      selectedText = sdkMessageText(selectedEntry.message.content);
    }
    const previousSessionFile = this.session.sessionFile;
    const sessionDir = this.session.sessionManager.getSessionDir ? this.session.sessionManager.getSessionDir() : path.join(this.services.agentDir || getAgentDir(), "sessions");
    let sessionManager;
    if (this.session.sessionManager.isPersisted && this.session.sessionManager.isPersisted()) {
      const currentSessionFile = this.session.sessionFile;
      if (!currentSessionFile) throw new Error("Persisted session is missing a session file");
      if (!targetLeafId) {
        sessionManager = SessionManager.create(this.cwd, sessionDir);
        sessionManager.newSession({ parentSession: currentSessionFile });
      } else {
        sessionManager = SessionManager.open(currentSessionFile, sessionDir);
        const forkedSessionPath = sessionManager.createBranchedSession(targetLeafId);
        if (!forkedSessionPath) throw new Error("Failed to create forked session");
      }
    } else {
      sessionManager = this.session.sessionManager;
      if (!targetLeafId) sessionManager.newSession({ parentSession: this.session.sessionFile });
      else if (sessionManager.createBranchedSession) sessionManager.createBranchedSession(targetLeafId);
      else if (this.session.navigateTree) await this.session.navigateTree(targetLeafId);
    }
    await this.applyResult({
      ...(await this.createRuntime({
        cwd: sessionManager.getCwd ? sessionManager.getCwd() : this.cwd,
        agentDir: this.services.agentDir,
        sessionManager,
        sessionStartEvent: { type: "session_start", reason: "fork", previousSessionFile },
      })),
      shutdownReason: "fork",
    });
    if (options.withSession) await options.withSession(this.session.createReplacedSessionContext());
    return { cancelled: false, selectedText };
  }
  async navigateTree(entryId, options = {}) {
    await this.session.navigateTree(entryId);
    if (options.withSession) await options.withSession(this.session.createReplacedSessionContext());
    return { cancelled: false };
  }
  async importFromJsonl(inputPath, cwdOverride = undefined) {
    const resolvedPath = path.resolve(String(inputPath || ""));
    if (!fs.existsSync(resolvedPath)) throw new Error(`Session file not found: ${resolvedPath}`);
    const sessionDir = this.session.sessionManager.getSessionDir ? this.session.sessionManager.getSessionDir() : path.join(this.services.agentDir || getAgentDir(), "sessions");
    fs.mkdirSync(sessionDir, { recursive: true });
    const destinationPath = path.join(sessionDir, path.basename(resolvedPath));
    const before = await this.emitBeforeSwitch("resume", destinationPath);
    if (before.cancelled) return before;
    if (path.resolve(destinationPath) !== resolvedPath) fs.copyFileSync(resolvedPath, destinationPath);
    const previousSessionFile = this.session.sessionFile;
    const sessionManager = createSdkSessionManagerFromJsonl(destinationPath, sessionDir, cwdOverride);
    await this.applyResult({
      ...(await this.createRuntime({
        cwd: sessionManager.getCwd(),
        agentDir: this.services.agentDir,
        sessionManager,
        sessionStartEvent: { type: "session_start", reason: "resume", previousSessionFile },
      })),
      shutdownReason: "resume",
    });
    return { cancelled: false };
  }
  async dispose() {
    await this.emitShutdown("quit");
    if (this.beforeSessionInvalidate) this.beforeSessionInvalidate();
    if (this._session && typeof this._session.dispose === "function") this._session.dispose();
  }
}

async function createAgentSessionRuntime(options = {}) {
  const services = await createAgentSessionServices(options);
  const result = await createAgentSessionFromServices(services, options);
  const createRuntime = async (nextOptions) => {
    const nextServices = await createAgentSessionServices({ ...options, ...nextOptions });
    const nextResult = await createAgentSessionFromServices(nextServices, { ...options, ...nextOptions });
    return { ...nextResult, services: nextServices, diagnostics: nextServices.diagnostics || [] };
  };
  return {
    ...result,
    services,
    diagnostics: services.diagnostics || [],
    runtime: new AgentSessionRuntime(result.session, services, createRuntime, services.diagnostics || [], result.modelFallbackMessage),
  };
}

class InteractiveMode {
  constructor(runtimeHost, options = {}) {
    this.runtimeHost = runtimeHost;
    this.options = options;
    this.running = false;
    this.isInitialized = false;
    this.editorText = "";
    this.errors = [];
    this.warnings = [];
    this.notifications = [];
    this.initialMessages = [];
    this.onInputCallback = undefined;
  }
  async init() {
    if (this.isInitialized) return;
    this.isInitialized = true;
    if (this.runtimeHost && this.runtimeHost.session && typeof this.runtimeHost.session.bindExtensions === "function") {
      await this.runtimeHost.session.bindExtensions({
        uiContext: {
          notify: (message, type = "info") => this.notifications.push({ type, message: String(message || "") }),
        },
      });
    }
  }
  renderInitialMessages() {
    const manager = this.runtimeHost && this.runtimeHost.session && this.runtimeHost.session.sessionManager;
    const context = manager && typeof manager.buildSessionContext === "function" ? manager.buildSessionContext() : { messages: [] };
    this.initialMessages = Array.isArray(context.messages) ? context.messages.slice() : [];
    return this.initialMessages;
  }
  getUserInput() {
    return new Promise((resolve) => {
      this.onInputCallback = (text) => {
        this.onInputCallback = undefined;
        resolve(String(text || ""));
      };
    });
  }
  clearEditor() {
    this.editorText = "";
  }
  showError(errorMessage) {
    this.errors.push(String(errorMessage || ""));
  }
  showWarning(warningMessage) {
    this.warnings.push(String(warningMessage || ""));
  }
  showNewVersionNotification(release) {
    this.notifications.push({ type: "new_version", release: safeSerializable(release || {}) });
  }
  showPackageUpdateNotification(packages) {
    this.notifications.push({ type: "package_updates", packages: Array.isArray(packages) ? packages.map(String) : [] });
  }
  async start() {
    await this.init();
    this.renderInitialMessages();
    this.running = true;
    return 0;
  }
  async stop() { this.running = false; }
  async run() { return this.start(); }
}

async function runPrintMode(runtimeHost, options = {}) {
  const initialMessage = options.initialMessage || options.message || options.input;
  const messages = Array.isArray(options.messages) ? options.messages : [];
  if (initialMessage && runtimeHost && runtimeHost.session && typeof runtimeHost.session.prompt === "function") {
    await runtimeHost.session.prompt(initialMessage, { images: options.initialImages || options.images || [] });
  }
  for (const message of messages) {
    if (message && runtimeHost && runtimeHost.session && typeof runtimeHost.session.prompt === "function") {
      await runtimeHost.session.prompt(message);
    }
  }
  return 0;
}

async function runRpcMode(runtimeHost) {
  if (!runtimeHost || !runtimeHost.session) throw new Error("runRpcMode requires an AgentSessionRuntime");
  let session = runtimeHost.session;
  let unsubscribe = undefined;
  let detached = false;
  const pendingUi = new Map();
  const output = (value) => {
    process.stdout.write(`${JSON.stringify(value)}\n`);
  };
  const success = (id, command, data = undefined) => {
    const response = { id, type: "response", command, success: true };
    if (data !== undefined) response.data = data;
    return response;
  };
  const failure = (id, command, message) => ({ id, type: "response", command, success: false, error: String(message || "Unknown error") });
  const sessionState = () => ({
    model: session.model,
    thinkingLevel: session.thinkingLevel,
    isStreaming: session.isStreaming,
    isCompacting: session.isCompacting,
    steeringMode: session.steeringMode,
    followUpMode: session.followUpMode,
    sessionFile: session.sessionFile,
    sessionId: session.sessionId,
    sessionName: session.sessionName,
    autoCompactionEnabled: session.autoCompactionEnabled,
    messageCount: session.messages.length,
    pendingMessageCount: session.pendingMessageCount,
  });
  const nextUiId = () => `ui_${Date.now().toString(36)}_${Math.floor(Math.random() * 0xffffff).toString(36)}`;
  const requestUi = (request, defaultValue, parse) => {
    const id = nextUiId();
    return new Promise((resolve) => {
      pendingUi.set(id, { resolve: (response) => resolve(parse(response)), defaultValue });
      output({ type: "extension_ui_request", id, ...request });
    });
  };
  const uiContext = () => ({
    select: (title, options, opts = {}) => requestUi({ method: "select", title, options, timeout: opts.timeout }, undefined, (response) => response.cancelled ? undefined : response.value),
    confirm: (title, message, opts = {}) => requestUi({ method: "confirm", title, message, timeout: opts.timeout }, false, (response) => response.cancelled ? false : !!response.confirmed),
    input: (title, placeholder, opts = {}) => requestUi({ method: "input", title, placeholder, timeout: opts.timeout }, undefined, (response) => response.cancelled ? undefined : response.value),
    editor: (title, prefill) => requestUi({ method: "editor", title, prefill }, undefined, (response) => response.cancelled ? undefined : response.value),
    notify: (message, type = "info") => output({ type: "extension_ui_request", id: nextUiId(), method: "notify", message, notifyType: type }),
    setStatus: (key, text) => output({ type: "extension_ui_request", id: nextUiId(), method: "setStatus", statusKey: key, statusText: text }),
    setWidget: (key, content, options = {}) => {
      if (content === undefined || Array.isArray(content)) {
        output({ type: "extension_ui_request", id: nextUiId(), method: "setWidget", widgetKey: key, widgetLines: content, widgetPlacement: options.placement });
      }
    },
    setTitle: (title) => output({ type: "extension_ui_request", id: nextUiId(), method: "setTitle", title }),
    pasteToEditor(text) { this.setEditorText(text); },
    setEditorText: (text) => output({ type: "extension_ui_request", id: nextUiId(), method: "set_editor_text", text }),
    getEditorText: () => "",
    custom: async () => undefined,
    onTerminalInput: () => () => {},
    setWorkingMessage: () => {},
    setWorkingVisible: () => {},
    setWorkingIndicator: () => {},
    setHiddenThinkingLabel: () => {},
    setFooter: () => {},
    setHeader: () => {},
    addAutocompleteProvider: () => {},
    setEditorComponent: () => {},
    getEditorComponent: () => undefined,
    get theme() { return new Theme(); },
    getAllThemes: () => [],
    getTheme: () => undefined,
    setTheme: () => ({ success: false, error: "Theme switching not supported in RPC mode" }),
    getToolsExpanded: () => false,
    setToolsExpanded: () => {},
  });
  const rebindSession = async () => {
    session = runtimeHost.session;
    await session.bindExtensions({
      uiContext: uiContext(),
      commandContextActions: {
        waitForIdle: async () => {},
        newSession: (options) => runtimeHost.newSession(options),
        fork: async (entryId, options) => {
          const result = await runtimeHost.fork(entryId, options);
          return { cancelled: result.cancelled };
        },
        navigateTree: (targetId, options) => session.navigateTree(targetId, options || {}),
        switchSession: (sessionPath, options) => runtimeHost.switchSession(sessionPath, options),
        reload: () => session.reload(),
      },
      shutdownHandler: () => {
        detached = true;
      },
      onError: (error) => output({ type: "extension_error", extensionPath: error.extensionPath, event: error.event, error: error.error }),
    });
    if (typeof unsubscribe === "function") unsubscribe();
    unsubscribe = session.subscribe((event) => output(event));
  };
  runtimeHost.setRebindSession(rebindSession);
  await rebindSession();

  const handleCommand = async (command) => {
    const id = command && command.id;
    switch (command && command.type) {
      case "prompt":
        await session.prompt(command.message, { images: command.images, streamingBehavior: command.streamingBehavior });
        return success(id, "prompt");
      case "steer":
        await session.steer(command.message, command.images);
        return success(id, "steer");
      case "follow_up":
        await session.followUp(command.message, command.images);
        return success(id, "follow_up");
      case "abort":
        await session.abort();
        return success(id, "abort");
      case "new_session":
        return success(id, "new_session", await runtimeHost.newSession(command.parentSession ? { parentSession: command.parentSession } : undefined));
      case "get_state":
        return success(id, "get_state", sessionState());
      case "set_model": {
        const model = session.modelRegistry.getAvailable().find((item) => item.provider === command.provider && item.id === command.modelId)
          || { provider: command.provider, id: command.modelId, name: command.modelId };
        await session.setModel(model);
        return success(id, "set_model", model);
      }
      case "cycle_model":
        return success(id, "cycle_model", await session.cycleModel() || null);
      case "get_available_models":
        return success(id, "get_available_models", { models: session.modelRegistry.getAvailable() });
      case "set_thinking_level":
        session.setThinkingLevel(command.level);
        return success(id, "set_thinking_level");
      case "cycle_thinking_level": {
        const level = session.cycleThinkingLevel();
        return success(id, "cycle_thinking_level", level ? { level } : null);
      }
      case "set_steering_mode":
        session.setSteeringMode(command.mode);
        return success(id, "set_steering_mode");
      case "set_follow_up_mode":
        session.setFollowUpMode(command.mode);
        return success(id, "set_follow_up_mode");
      case "compact":
        return success(id, "compact", await session.compact(command.customInstructions));
      case "set_auto_compaction":
        session.setAutoCompactionEnabled(command.enabled);
        return success(id, "set_auto_compaction");
      case "set_auto_retry":
        session.setAutoRetryEnabled(command.enabled);
        return success(id, "set_auto_retry");
      case "abort_retry":
        session.abortRetry();
        return success(id, "abort_retry");
      case "bash":
        return success(id, "bash", await session.executeBash(command.command));
      case "abort_bash":
        session.abortBash();
        return success(id, "abort_bash");
      case "get_session_stats":
        return success(id, "get_session_stats", session.getSessionStats());
      case "export_html":
        return success(id, "export_html", { path: await session.exportToHtml(command.outputPath) });
      case "switch_session":
        return success(id, "switch_session", await runtimeHost.switchSession(command.sessionPath));
      case "fork": {
        const result = await runtimeHost.fork(command.entryId);
        return success(id, "fork", { text: result.selectedText, cancelled: result.cancelled });
      }
      case "clone": {
        const leafId = session.sessionManager.getLeafId();
        if (!leafId) return failure(id, "clone", "Cannot clone session: no current entry selected");
        const result = await runtimeHost.fork(leafId, { position: "at" });
        return success(id, "clone", { cancelled: result.cancelled });
      }
      case "get_fork_messages":
        return success(id, "get_fork_messages", { messages: session.getUserMessagesForForking() });
      case "get_last_assistant_text":
        return success(id, "get_last_assistant_text", { text: session.getLastAssistantText() || null });
      case "set_session_name":
        session.setSessionName(String(command.name || "").trim());
        return success(id, "set_session_name");
      case "get_messages":
        return success(id, "get_messages", { messages: session.messages });
      case "get_commands":
        return success(id, "get_commands", { commands: session.getCommands().map((command) => ({
          name: command.name,
          description: command.description,
          source: command.source || "extension",
          sourceInfo: command.sourceInfo || { path: "", source: command.source || "extension", scope: "project", origin: "runtime" },
        })) });
      case "shutdown":
        detached = true;
        return success(id, "shutdown");
      default:
        return failure(id, command && command.type || "unknown", `Unknown command: ${command && command.type}`);
    }
  };

  return await new Promise((resolve) => {
    let buffer = "";
    let cleaned = false;
    let commandQueue = Promise.resolve();
    const cleanup = async () => {
      if (cleaned) return;
      cleaned = true;
      process.stdin.off("data", onData);
      process.stdin.off("end", onEnd);
      if (typeof unsubscribe === "function") unsubscribe();
      await runtimeHost.dispose();
      resolve(0);
    };
    const processLine = (line) => {
      if (!line.trim()) return;
      commandQueue = commandQueue.then(async () => {
        let parsed;
        try {
          parsed = JSON.parse(line);
        } catch (error) {
          output(failure(undefined, "parse", error && error.message ? error.message : String(error)));
          return;
        }
        if (parsed && parsed.type === "extension_ui_response") {
          const pending = pendingUi.get(parsed.id);
          if (pending) {
            pendingUi.delete(parsed.id);
            pending.resolve(parsed);
          }
          return;
        }
        try {
          const response = await handleCommand(parsed);
          if (response) output(response);
        } catch (error) {
          output(failure(parsed && parsed.id, parsed && parsed.type || "unknown", error && error.message ? error.message : String(error)));
        }
        if (detached) await cleanup();
      });
    };
    const onData = (data) => {
      buffer += Buffer.from(data).toString("utf8");
      const lines = buffer.split(/\r?\n/);
      buffer = lines.pop() || "";
      for (const line of lines) processLine(line);
    };
    const onEnd = () => {
      if (buffer.trim()) processLine(buffer);
      commandQueue = commandQueue.then(() => cleanup());
    };
    process.stdin.on("data", onData);
    process.stdin.on("end", onEnd);
  });
}

async function main(options = {}) {
  const result = await createAgentSessionRuntime(options);
  const mode = options.mode || (options.print || options.prompt ? "print" : "interactive");
  if (mode === "print") {
    return runPrintMode(result.runtime, {
      ...options,
      initialMessage: options.initialMessage || options.prompt || options.message || options.input,
    });
  }
  if (mode === "rpc") {
    return runRpcMode(result.runtime);
  }
  const interactive = new InteractiveMode(result.runtime, options);
  return interactive.start();
}

class RpcClient {
  constructor(options = {}) {
    this.options = options;
    this.process = null;
    this.eventListeners = [];
    this.pendingRequests = new Map();
    this.requestId = 0;
    this.stderr = "";
  }
  async start() {
    if (this.process) throw new Error("Client already started");
    const cliPath = this.options.cliPath || "dist/cli.js";
    const args = ["--mode", "rpc"];
    if (this.options.provider) args.push("--provider", this.options.provider);
    if (this.options.model) args.push("--model", this.options.model);
    if (Array.isArray(this.options.args)) args.push(...this.options.args);
    this.process = childProcess.spawn("node", [cliPath, ...args], { cwd: this.options.cwd, env: { ...process.env, ...(this.options.env || {}) }, stdio: ["pipe", "pipe", "pipe"] });
    this.process.stderr.on("data", (data) => { this.stderr += data.toString(); });
    let buffer = "";
    this.process.stdout.on("data", (data) => {
      buffer += data.toString();
      const lines = buffer.split(/\r?\n/);
      buffer = lines.pop() || "";
      for (const line of lines) if (line.trim()) this.handleLine(line);
    });
    await new Promise((resolve) => setTimeout(resolve, 100));
    if (this.process.exitCode !== null) throw new Error(`Agent process exited immediately with code ${this.process.exitCode}. Stderr: ${this.stderr}`);
  }
  async stop() {
    if (!this.process) return;
    this.process.kill("SIGTERM");
    this.process = null;
    this.pendingRequests.clear();
  }
  onEvent(listener) {
    this.eventListeners.push(listener);
    return () => { this.eventListeners = this.eventListeners.filter((item) => item !== listener); };
  }
  getStderr() { return this.stderr; }
  handleLine(line) {
    try {
      const data = JSON.parse(line);
      if (data.type === "response" && data.id && this.pendingRequests.has(data.id)) {
        const pending = this.pendingRequests.get(data.id);
        this.pendingRequests.delete(data.id);
        pending.resolve(data);
        return;
      }
      for (const listener of this.eventListeners) listener(data);
    } catch {}
  }
  send(command) {
    if (!this.process || !this.process.stdin) return Promise.reject(new Error("Client not started"));
    const id = `req_${++this.requestId}`;
    const full = { ...command, id };
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pendingRequests.delete(id);
        reject(new Error(`Timeout waiting for response to ${command.type}. Stderr: ${this.stderr}`));
      }, 30000);
      this.pendingRequests.set(id, {
        resolve: (response) => { clearTimeout(timer); resolve(response); },
        reject: (error) => { clearTimeout(timer); reject(error); },
      });
      this.process.stdin.write(`${JSON.stringify(full)}\n`);
    });
  }
  getData(response) {
    if (!response.success) throw new Error(response.error || "RPC command failed");
    return response.data;
  }
  async prompt(message, images) { await this.send({ type: "prompt", message, images }); }
  async steer(message, images) { await this.send({ type: "steer", message, images }); }
  async followUp(message, images) { await this.send({ type: "follow_up", message, images }); }
  async abort() { await this.send({ type: "abort" }); }
  async newSession(parentSession) { return this.getData(await this.send({ type: "new_session", parentSession })); }
  async getState() { return this.getData(await this.send({ type: "get_state" })); }
  async setModel(provider, modelId) { return this.getData(await this.send({ type: "set_model", provider, modelId })); }
  async cycleModel() { return this.getData(await this.send({ type: "cycle_model" })); }
  async getAvailableModels() { return this.getData(await this.send({ type: "get_available_models" })).models; }
  async setThinkingLevel(level) { await this.send({ type: "set_thinking_level", level }); }
  async cycleThinkingLevel() { return this.getData(await this.send({ type: "cycle_thinking_level" })); }
  async setSteeringMode(mode) { await this.send({ type: "set_steering_mode", mode }); }
  async setFollowUpMode(mode) { await this.send({ type: "set_follow_up_mode", mode }); }
  async compact(customInstructions) { return this.getData(await this.send({ type: "compact", customInstructions })); }
  async setAutoCompaction(enabled) { await this.send({ type: "set_auto_compaction", enabled }); }
  async setAutoRetry(enabled) { await this.send({ type: "set_auto_retry", enabled }); }
  async abortRetry() { await this.send({ type: "abort_retry" }); }
  async bash(command) { return this.getData(await this.send({ type: "bash", command })); }
  async abortBash() { await this.send({ type: "abort_bash" }); }
  async getSessionStats() { return this.getData(await this.send({ type: "get_session_stats" })); }
  async exportHtml(outputPath) { return this.getData(await this.send({ type: "export_html", outputPath })); }
  async switchSession(sessionPath) { return this.getData(await this.send({ type: "switch_session", sessionPath })); }
  async fork(entryId) { return this.getData(await this.send({ type: "fork", entryId })); }
  async clone() { return this.getData(await this.send({ type: "clone" })); }
  async getForkMessages() { return this.getData(await this.send({ type: "get_fork_messages" })).messages; }
  async getLastAssistantText() { return this.getData(await this.send({ type: "get_last_assistant_text" })).text; }
  async setSessionName(name) { await this.send({ type: "set_session_name", name }); }
  async getMessages() { return this.getData(await this.send({ type: "get_messages" })).messages; }
  async getCommands() { return this.getData(await this.send({ type: "get_commands" })).commands; }
  waitForIdle(timeout = 60000) {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => { unsubscribe(); reject(new Error(`Timeout waiting for agent to become idle. Stderr: ${this.stderr}`)); }, timeout);
      const unsubscribe = this.onEvent((event) => {
        if (event.type === "agent_end") {
          clearTimeout(timer);
          unsubscribe();
          resolve();
        }
      });
    });
  }
  collectEvents(timeout = 60000) {
    return new Promise((resolve, reject) => {
      const events = [];
      const timer = setTimeout(() => { unsubscribe(); reject(new Error(`Timeout collecting events. Stderr: ${this.stderr}`)); }, timeout);
      const unsubscribe = this.onEvent((event) => {
        events.push(event);
        if (event.type === "agent_end") {
          clearTimeout(timer);
          unsubscribe();
          resolve(events);
        }
      });
    });
  }
  async promptAndWait(message, images, timeout = 60000) {
    const events = this.collectEvents(timeout);
    await this.prompt(message, images);
    return events;
  }
}

function truncateToVisualLines(text, maxVisualLines, width, paddingX = 0) {
  const rendered = new BridgeTextComponent(String(text || "")).render(Math.max(1, Number(width || 80) - paddingX * 2));
  if (rendered.length <= maxVisualLines) return { visualLines: rendered, skippedCount: 0 };
  return { visualLines: rendered.slice(-maxVisualLines), skippedCount: rendered.length - maxVisualLines };
}

class GenericComponent extends Container {
  constructor(...args) {
    super();
    this.args = args;
    this.expanded = false;
    this.showImages = true;
    this.imageWidthCells = 60;
    this.executionStarted = false;
    this.argsComplete = false;
    this.output = "";
    this.status = "running";
    this.abortController = new AbortController();
    this.focused = false;
    for (const arg of args) {
      if (arg instanceof BridgeTextComponent || arg instanceof Container || typeof arg === "string" || Array.isArray(arg)) this.addChild(arg);
      else if (arg && typeof arg === "object") {
        if (arg.message) this.addChild(sdkMessageText(arg.message.content || arg.message));
        else if (arg.content) this.addChild(sdkMessageText(arg.content));
        else if (arg.text) this.addChild(arg.text);
        else if (arg.summary) this.addChild(arg.summary);
        else if (arg.name) this.addChild(arg.name);
      }
    }
  }

  setExpanded(expanded) { this.expanded = !!expanded; }
  setHideThinkingBlock(hide) { this.hideThinkingBlock = !!hide; }
  setHiddenThinkingLabel(label) { this.hiddenThinkingLabel = label === undefined ? undefined : String(label); }
  updateContent(message) {
    this.lastMessage = message;
    this.clear();
    if (message && typeof message === "object") this.addChild(sdkMessageText(message.content || message.message || message));
    else this.addChild(String(message || ""));
  }
  updateArgs(args) { this.toolArgs = args; }
  markExecutionStarted() { this.executionStarted = true; }
  setArgsComplete() { this.argsComplete = true; }
  updateResult(result, isPartial = false) {
    this.result = result;
    this.isPartial = !!isPartial;
    if (result && Array.isArray(result.content)) {
      const text = result.content
        .map((part) => part && part.type === "text" ? String(part.text || "") : "")
        .filter(Boolean)
        .join("\n");
      if (text) {
        this.clear();
        this.addChild(text);
      }
    }
  }
  setShowImages(show) { this.showImages = !!show; }
  setImageWidthCells(width) {
    const numeric = Number(width);
    this.imageWidthCells = Math.max(1, Math.floor(Number.isFinite(numeric) ? numeric : 1));
  }
  appendOutput(chunk) {
    this.output += String(chunk || "");
    this.clear();
    this.addChild(this.output);
  }
  setComplete(exitCode = undefined, cancelled = false, truncationResult = undefined, fullOutputPath = undefined) {
    this.exitCode = exitCode;
    this.cancelled = !!cancelled;
    this.truncationResult = truncationResult;
    this.fullOutputPath = fullOutputPath;
    this.status = this.cancelled ? "cancelled" : exitCode !== 0 && exitCode !== undefined && exitCode !== null ? "error" : "complete";
  }
  getOutput() { return this.output; }
  getCommand() { return String(this.args[0] || this.command || ""); }
  get signal() { return this.abortController.signal; }
  set onAbort(fn) { this.abortHandler = typeof fn === "function" ? fn : undefined; }
  handleInput(data) {
    if ((data === "\u0003" || data === "\u001b") && this.abortHandler) this.abortHandler();
  }
  setSession(session) { this.session = session; }
  setAutoCompactEnabled(enabled) { this.autoCompactEnabled = !!enabled; }
  setAvailableProviderCount(count) { this.availableProviderCount = Number(count) || 0; }
  setCwd(cwd) { this.cwd = String(cwd || ""); }
  getAvailableProviderCount() { return this.availableProviderCount || 0; }
  getGitBranch() { return this.gitBranch; }
  getExtensionStatuses() { return this.extensionStatuses || {}; }
  setExtensionStatus(key, value) {
    if (!this.extensionStatuses) this.extensionStatuses = {};
    this.extensionStatuses[String(key || "")] = value;
  }
  clearExtensionStatuses() { this.extensionStatuses = {}; }
  onBranchChange() { return () => {}; }
  getSelectList() { return this.items || this.args[0] || []; }
  getSettingsList() { return this.getSelectList(); }
  getResourceList() { return this.getSelectList(); }
  getSessionList() { return this.getSelectList(); }
  getTreeList() { return this.getSelectList(); }
  getMessageList() { return this.getSelectList(); }
  getSearchInput() { return this.searchInput || ""; }
}

class ArminComponent extends BridgeTextComponent {}
class AssistantMessageComponent extends GenericComponent {}
class BashExecutionComponent extends GenericComponent {}
class BorderedLoader extends GenericComponent {}
class BranchSummaryMessageComponent extends GenericComponent {}
class CompactionSummaryMessageComponent extends GenericComponent {}
class CustomMessageComponent extends GenericComponent {}
class DynamicBorder extends GenericComponent {}
class ExtensionEditorComponent extends CustomEditor {}
class ExtensionInputComponent extends CustomEditor {}
class ExtensionSelectorComponent extends GenericComponent {}
class FooterComponent extends GenericComponent {}
class LoginDialogComponent extends GenericComponent {}
class ModelSelectorComponent extends GenericComponent {}
class OAuthSelectorComponent extends GenericComponent {}
class SessionSelectorComponent extends GenericComponent {}
class SettingsSelectorComponent extends GenericComponent {}
class ShowImagesSelectorComponent extends GenericComponent {}
class SkillInvocationMessageComponent extends GenericComponent {}
class ThemeSelectorComponent extends GenericComponent {}
class ThinkingSelectorComponent extends GenericComponent {}
class ToolExecutionComponent extends GenericComponent {}
class TreeSelectorComponent extends GenericComponent {}
class UserMessageComponent extends GenericComponent {}
class UserMessageSelectorComponent extends GenericComponent {}

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
    AgentSession,
    AgentSessionRuntime,
    createAgentSession,
    createAgentSessionFromServices,
    createAgentSessionRuntime,
    createAgentSessionServices,
    InteractiveMode,
    RpcClient,
    main,
    runPrintMode,
    runRpcMode,
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
    CURRENT_SESSION_VERSION,
    parseSkillBlock,
    convertToLlm,
    buildSessionContext,
    getLatestCompactionEntry,
    migrateSessionEntries,
    parseSessionEntries,
    SessionManager,
    AuthStorage,
    FileAuthStorageBackend,
    InMemoryAuthStorageBackend,
    ModelRegistry,
    SettingsManager,
    FileSettingsStorage,
    InMemorySettingsStorage,
    calculateContextTokens,
    compact,
    collectEntriesForBranchSummary,
    DEFAULT_COMPACTION_SETTINGS,
    estimateTokens,
    findCutPoint,
    findTurnStartIndex,
    generateBranchSummary,
    generateSummary,
    getLastAssistantUsage,
    prepareBranchEntries,
    serializeConversation,
    shouldCompact,
    createSyntheticSourceInfo,
    DefaultPackageManager,
    DefaultResourceLoader,
    loadProjectContextFiles,
    loadSkills,
    loadSkillsFromDir,
    loadPromptTemplates,
    formatSkillsForPrompt,
    parseFrontmatter,
    stripFrontmatter,
    formatSize,
    copyToClipboard,
    formatDimensionNote,
    getLanguageFromPath,
    getMarkdownTheme,
    getSelectListTheme,
    getSettingsListTheme,
    getShellConfig,
    highlightCode,
    initTheme,
    resizeImage,
    Theme,
    truncateHead,
    truncateTail,
    truncateLine,
    withFileMutationQueue,
    createEventBus,
    ArminComponent,
    AssistantMessageComponent,
    BashExecutionComponent,
    BorderedLoader,
    BranchSummaryMessageComponent,
    CompactionSummaryMessageComponent,
    CustomMessageComponent,
    CustomEditor,
    DynamicBorder,
    ExtensionEditorComponent,
    ExtensionInputComponent,
    ExtensionSelectorComponent,
    FooterComponent,
    LoginDialogComponent,
    ModelSelectorComponent,
    OAuthSelectorComponent,
    SessionSelectorComponent,
    SettingsSelectorComponent,
    ShowImagesSelectorComponent,
    SkillInvocationMessageComponent,
    ThemeSelectorComponent,
    ThinkingSelectorComponent,
    ToolExecutionComponent,
    TreeSelectorComponent,
    truncateToVisualLines,
    UserMessageComponent,
    UserMessageSelectorComponent,
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

function StringEnum(values = [], options = {}) {
  return {
    type: "string",
    enum: Array.isArray(values) ? values.slice() : [],
    ...(options && options.description ? { description: options.description } : {}),
    ...(options && options.default !== undefined ? { default: options.default } : {}),
  };
}

function normalizeModelReference(providerOrModel, modelId = undefined) {
  if (providerOrModel && typeof providerOrModel === "object") return { ...providerOrModel };
  const text = String(providerOrModel || "");
  if (modelId !== undefined) return { provider: text, id: String(modelId), name: String(modelId) };
  const slash = text.indexOf("/");
  if (slash > 0) return { provider: text.slice(0, slash), id: text.slice(slash + 1), name: text.slice(slash + 1) };
  return { provider: "custom", id: text || "model", name: text || "model" };
}

function getModel(providerOrModel, modelId = undefined) {
  if (modelId !== undefined) {
    const provider = String(providerOrModel || "");
    const found = BUILT_IN_MODELS.find((item) => item.provider === provider && item.id === String(modelId));
    if (found) return { cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 }, ...cloneJson(found) };
  }
  const model = normalizeModelReference(providerOrModel, modelId);
  const found = BUILT_IN_MODELS.find((item) => item.provider === model.provider && item.id === model.id);
  if (found) return { cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 }, ...cloneJson(found) };
  return {
    api: model.api || "openai",
    provider: model.provider || "custom",
    id: model.id || model.model || model.name || "model",
    name: model.name || model.id || model.model || "model",
    baseUrl: model.baseUrl || "",
    contextWindow: model.contextWindow || model.context_window || 128000,
    maxTokens: model.maxTokens || model.max_tokens || 4096,
    reasoning: !!model.reasoning,
    cost: model.cost || { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
    ...model,
  };
}

function getProviders() {
  return [...new Set(BUILT_IN_MODELS.map((model) => model.provider))];
}

function getModels(provider) {
  return BUILT_IN_MODELS
    .filter((model) => model.provider === provider)
    .map((model) => ({ cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 }, ...cloneJson(model) }));
}

function calculateCost(model, usage) {
  const cost = usage.cost || {};
  const rates = model.cost || {};
  cost.input = ((Number(rates.input) || 0) / 1000000) * (Number(usage.input) || 0);
  cost.output = ((Number(rates.output) || 0) / 1000000) * (Number(usage.output) || 0);
  cost.cacheRead = ((Number(rates.cacheRead) || 0) / 1000000) * (Number(usage.cacheRead) || 0);
  cost.cacheWrite = ((Number(rates.cacheWrite) || 0) / 1000000) * (Number(usage.cacheWrite) || 0);
  cost.total = cost.input + cost.output + cost.cacheRead + cost.cacheWrite;
  usage.cost = cost;
  return cost;
}

function modelsAreEqual(a, b) {
  return !!a && !!b && a.provider === b.provider && a.id === b.id;
}

const EXTENDED_THINKING_LEVELS = ["off", "minimal", "low", "medium", "high", "xhigh"];

function normalizeAiThinkingLevel(level) {
  const value = String(level || "").trim().toLowerCase();
  if (value === "" || value === "none") return "off";
  return value;
}

function getSupportedThinkingLevels(model) {
  if (!model || !model.reasoning) return ["off"];
  const map = model.thinkingLevelMap || {};
  return EXTENDED_THINKING_LEVELS.filter((level) => {
    if (map[level] === null) return false;
    if (level === "xhigh") return map[level] !== undefined;
    return true;
  });
}

function clampThinkingLevel(modelOrLevel, maybeLevel = undefined) {
  if (maybeLevel === undefined) {
    const value = normalizeAiThinkingLevel(modelOrLevel);
    return ["off", "minimal", "low", "medium", "high", "xhigh"].includes(value) ? value : "medium";
  }
  const availableLevels = getSupportedThinkingLevels(modelOrLevel);
  const requested = normalizeAiThinkingLevel(maybeLevel);
  if (availableLevels.includes(requested)) return requested;
  const requestedIndex = EXTENDED_THINKING_LEVELS.indexOf(requested);
  if (requestedIndex < 0) return availableLevels[0] || "off";
  for (let i = requestedIndex; i < EXTENDED_THINKING_LEVELS.length; i++) {
    if (availableLevels.includes(EXTENDED_THINKING_LEVELS[i])) return EXTENDED_THINKING_LEVELS[i];
  }
  for (let i = requestedIndex - 1; i >= 0; i--) {
    if (availableLevels.includes(EXTENDED_THINKING_LEVELS[i])) return EXTENDED_THINKING_LEVELS[i];
  }
  return availableLevels[0] || "off";
}

class EventStream {
  constructor(isComplete = () => false, extractResult = (event) => event) {
    this.queue = [];
    this.waiting = [];
    this.done = false;
    this.isComplete = isComplete;
    this.extractResult = extractResult;
    this.finalResultPromise = new Promise((resolve) => {
      this.resolveFinalResult = resolve;
    });
  }
  push(event) {
    if (this.done) return;
    if (this.isComplete(event)) {
      this.done = true;
      this.resolveFinalResult(this.extractResult(event));
    }
    const waiter = this.waiting.shift();
    if (waiter) waiter({ value: event, done: false });
    else this.queue.push(event);
  }
  end(result = undefined) {
    this.done = true;
    if (result !== undefined) this.resolveFinalResult(result);
    while (this.waiting.length > 0) this.waiting.shift()({ value: undefined, done: true });
  }
  async *[Symbol.asyncIterator]() {
    while (true) {
      if (this.queue.length > 0) yield this.queue.shift();
      else if (this.done) return;
      else {
        const result = await new Promise((resolve) => this.waiting.push(resolve));
        if (result.done) return;
        yield result.value;
      }
    }
  }
  result() {
    return this.finalResultPromise;
  }
}

class AssistantMessageEventStream extends EventStream {
  constructor() {
    super(
      (event) => event && (event.type === "done" || event.type === "error"),
      (event) => event.type === "done" ? event.message : event.error,
    );
  }
}

function createAssistantMessageEventStream() {
  return new AssistantMessageEventStream();
}

function normalizeContextMessages(context) {
  if (Array.isArray(context)) return context;
  if (context && Array.isArray(context.messages)) return context.messages;
  if (context && Array.isArray(context.content)) return [{ role: "user", content: context.content }];
  return [];
}

function assistantMessageFromContext(context = {}) {
  const messages = normalizeContextMessages(context);
  const last = messages.length ? messages[messages.length - 1] : undefined;
  const text = last && typeof last === "object" ? sdkMessageText(last.content || last.message || last) : "";
  return {
    role: "assistant",
    content: [{ type: "text", text }],
    stopReason: "stop",
    usage: { inputTokens: 0, outputTokens: 0 },
  };
}

function completedAssistantStream(message) {
  const eventStream = createAssistantMessageEventStream();
  eventStream.push({ type: "done", message });
  return eventStream;
}

function normalizeAssistantStream(value, context = {}) {
  if (value && typeof value.result === "function" && typeof value[Symbol.asyncIterator] === "function") return value;
  if (value && typeof value.result === "function") return value;
  if (value && typeof value.then === "function") {
    return {
      async result() { return value; },
      async *[Symbol.asyncIterator]() { yield { type: "done", message: await value }; },
    };
  }
  return completedAssistantStream(value && value.role ? value : assistantMessageFromContext(context));
}

const apiProviderRegistry = new Map();

function wrapApiStream(api, fn) {
  return (model, context, options) => {
    if (model && model.api && model.api !== api) throw new Error(`Mismatched api: ${model.api} expected ${api}`);
    return normalizeAssistantStream(fn(model, context, options), context);
  };
}

function registerApiProvider(provider, sourceId = undefined) {
  if (!provider || !provider.api) return;
  const api = String(provider.api);
  const streamFn = typeof provider.stream === "function" ? provider.stream : (_model, context) => assistantMessageFromContext(context);
  const streamSimpleFn = typeof provider.streamSimple === "function" ? provider.streamSimple : streamFn;
  apiProviderRegistry.set(api, {
    sourceId,
    provider: {
      api,
      stream: wrapApiStream(api, streamFn),
      streamSimple: wrapApiStream(api, streamSimpleFn),
    },
  });
}

function getApiProvider(api) {
  return apiProviderRegistry.get(String(api || ""))?.provider;
}

function getApiProviders() {
  return Array.from(apiProviderRegistry.values(), (entry) => entry.provider);
}

function unregisterApiProviders(sourceId) {
  for (const [api, entry] of apiProviderRegistry.entries()) if (entry.sourceId === sourceId) apiProviderRegistry.delete(api);
}

function clearApiProviders() {
  apiProviderRegistry.clear();
}

function builtInStream(model, context) {
  return completedAssistantStream(assistantMessageFromContext(context || { messages: [] }));
}

const streamAnthropic = builtInStream;
const streamSimpleAnthropic = builtInStream;
const streamAzureOpenAIResponses = builtInStream;
const streamSimpleAzureOpenAIResponses = builtInStream;
const streamGoogle = builtInStream;
const streamSimpleGoogle = builtInStream;
const streamGoogleVertex = builtInStream;
const streamSimpleGoogleVertex = builtInStream;
const streamMistral = builtInStream;
const streamSimpleMistral = builtInStream;
const streamOpenAICodexResponses = builtInStream;
const streamSimpleOpenAICodexResponses = builtInStream;
const streamOpenAICompletions = builtInStream;
const streamSimpleOpenAICompletions = builtInStream;
const streamOpenAIResponses = builtInStream;
const streamSimpleOpenAIResponses = builtInStream;
const streamBedrock = builtInStream;
const streamSimpleBedrock = builtInStream;
const bedrockProviderModule = { streamBedrock, streamSimpleBedrock };

function registerBuiltInApiProviders() {
  registerApiProvider({ api: "anthropic-messages", stream: streamAnthropic, streamSimple: streamSimpleAnthropic });
  registerApiProvider({ api: "openai-completions", stream: streamOpenAICompletions, streamSimple: streamSimpleOpenAICompletions });
  registerApiProvider({ api: "mistral-conversations", stream: streamMistral, streamSimple: streamSimpleMistral });
  registerApiProvider({ api: "openai-responses", stream: streamOpenAIResponses, streamSimple: streamSimpleOpenAIResponses });
  registerApiProvider({ api: "azure-openai-responses", stream: streamAzureOpenAIResponses, streamSimple: streamSimpleAzureOpenAIResponses });
  registerApiProvider({ api: "openai-codex-responses", stream: streamOpenAICodexResponses, streamSimple: streamSimpleOpenAICodexResponses });
  registerApiProvider({ api: "google-generative-ai", stream: streamGoogle, streamSimple: streamSimpleGoogle });
  registerApiProvider({ api: "google-vertex", stream: streamGoogleVertex, streamSimple: streamSimpleGoogleVertex });
  registerApiProvider({ api: "bedrock-converse-stream", stream: builtInStream, streamSimple: builtInStream });
}

function resetApiProviders() {
  clearApiProviders();
  registerBuiltInApiProviders();
}

function resolveApiProvider(api) {
  return getApiProvider(api) || {
    api: String(api || "openai"),
    stream: (_model, context) => completedAssistantStream(assistantMessageFromContext(context)),
    streamSimple: (_model, context) => completedAssistantStream(assistantMessageFromContext(context)),
  };
}

function stream(model, context = {}, options = {}) {
  return resolveApiProvider((model && model.api) || "openai").stream(model || getModel("custom/model"), context, options);
}

async function complete(model, context = {}, options = {}) {
  return stream(model, context, options).result();
}

function streamSimple(model, context = {}, options = {}) {
  return resolveApiProvider((model && model.api) || "openai").streamSimple(model || getModel("custom/model"), context, options);
}

async function completeSimple(model, context = {}, options = {}) {
  return streamSimple(model, context, options).result();
}

const imagesApiProviderRegistry = new Map();
const BUILT_IN_IMAGE_MODELS = [
  { provider: "openrouter", id: "openai/gpt-image-1", name: "GPT Image 1", api: "openrouter-images" },
];

function getImageProviders() {
  return [...new Set(BUILT_IN_IMAGE_MODELS.map((model) => model.provider))];
}

function getImageModels(provider) {
  return BUILT_IN_IMAGE_MODELS.filter((model) => model.provider === provider).map((model) => cloneJson(model));
}

function getImageModel(provider, modelId) {
  return getImageModels(provider).find((model) => model.id === modelId) || { provider, id: modelId, name: modelId, api: provider };
}

function registerImagesApiProvider(provider, sourceId = undefined) {
  if (!provider || !provider.api) return;
  const api = String(provider.api);
  const generateImages = typeof provider.generateImages === "function"
    ? provider.generateImages
    : async () => ({ images: [] });
  imagesApiProviderRegistry.set(api, {
    sourceId,
    provider: {
      api,
      generateImages: (model, context, options) => {
        if (model && model.api && model.api !== api) throw new Error(`Mismatched api: ${model.api} expected ${api}`);
        return generateImages(model, context, options);
      },
    },
  });
}

function getImagesApiProvider(api) {
  return imagesApiProviderRegistry.get(String(api || ""))?.provider;
}

const generateImagesOpenRouter = async () => ({ images: [] });

function registerBuiltInImagesApiProviders() {
  registerImagesApiProvider({ api: "openrouter-images", generateImages: generateImagesOpenRouter });
}

async function generateImages(model, context = {}, options = {}) {
  const provider = getImagesApiProvider((model && model.api) || "openrouter-images");
  if (!provider) return { images: [] };
  return provider.generateImages(model, context, options);
}

function findEnvKeys(provider) {
  const map = {
    openai: ["OPENAI_API_KEY"],
    anthropic: ["ANTHROPIC_OAUTH_TOKEN", "ANTHROPIC_API_KEY"],
    deepseek: ["DEEPSEEK_API_KEY"],
    google: ["GEMINI_API_KEY"],
    gemini: ["GEMINI_API_KEY"],
    "google-vertex": ["GOOGLE_CLOUD_API_KEY"],
    groq: ["GROQ_API_KEY"],
    xai: ["XAI_API_KEY"],
    openrouter: ["OPENROUTER_API_KEY"],
    mistral: ["MISTRAL_API_KEY"],
    "github-copilot": ["COPILOT_GITHUB_TOKEN"],
    "amazon-bedrock": ["AWS_PROFILE", "AWS_ACCESS_KEY_ID", "AWS_BEARER_TOKEN_BEDROCK"],
  };
  const keys = map[String(provider || "")] || [];
  const found = keys.filter((key) => !!process.env[key]);
  return found.length ? found : undefined;
}

function getEnvApiKey(provider) {
  const keys = findEnvKeys(provider);
  return keys && keys[0] ? process.env[keys[0]] : undefined;
}

const oauthProviderRegistry = new Map();
function registerOAuthProvider(provider) { if (provider && provider.id) oauthProviderRegistry.set(String(provider.id), provider); }
function unregisterOAuthProvider(id) { oauthProviderRegistry.delete(String(id || "")); }
function resetOAuthProviders() { oauthProviderRegistry.clear(); }
function getOAuthProviders() { return Array.from(oauthProviderRegistry.values()); }
function getOAuthProvider(id) { return oauthProviderRegistry.get(String(id || "")); }
async function getOAuthApiKey(_provider, credential) { return credential && (credential.accessToken || credential.access); }

const openAICodexWebSocketDebugStats = new Map();
function getOpenAICodexWebSocketDebugStats(sessionId) { return openAICodexWebSocketDebugStats.get(String(sessionId || "")); }
function resetOpenAICodexWebSocketDebugStats(sessionId = undefined) {
  if (sessionId === undefined) openAICodexWebSocketDebugStats.clear();
  else openAICodexWebSocketDebugStats.delete(String(sessionId || ""));
}
function closeOpenAICodexWebSocketSessions(sessionId = undefined) {
  resetOpenAICodexWebSocketDebugStats(sessionId);
}

function convertOpenAICompletionsMessages(messages = []) {
  return Array.isArray(messages) ? messages.slice() : [];
}

const sessionResourceCleanups = new Set();
function registerSessionResourceCleanup(cleanup) {
  if (typeof cleanup !== "function") return () => {};
  sessionResourceCleanups.add(cleanup);
  return () => sessionResourceCleanups.delete(cleanup);
}

function cleanupSessionResources(sessionId = undefined) {
  for (const cleanup of [...sessionResourceCleanups]) cleanup(sessionId);
}

function repairJson(json) {
  let repaired = "";
  let inString = false;
  for (let index = 0; index < String(json || "").length; index++) {
    const char = String(json || "")[index];
    if (!inString) {
      repaired += char;
      if (char === '"') inString = true;
      continue;
    }
    if (char === '"') {
      repaired += char;
      inString = false;
      continue;
    }
    if (char === "\\") {
      const next = String(json || "")[index + 1];
      if (next === undefined) {
        repaired += "\\\\";
      } else if (['"', "\\", "/", "b", "f", "n", "r", "t"].includes(next) || (next === "u" && /^[0-9a-fA-F]{4}$/.test(String(json || "").slice(index + 2, index + 6)))) {
        repaired += `\\${next}`;
        index += 1;
      } else {
        repaired += "\\\\";
      }
      continue;
    }
    const code = char.codePointAt(0);
    repaired += code !== undefined && code <= 0x1f ? `\\u${code.toString(16).padStart(4, "0")}` : char;
  }
  return repaired;
}

function parseJsonWithRepair(json) {
  try {
    return JSON.parse(json);
  } catch (error) {
    const repaired = repairJson(json);
    if (repaired !== json) return JSON.parse(repaired);
    throw error;
  }
}

function parseStreamingJson(partialJson) {
  if (!partialJson || !String(partialJson).trim()) return {};
  try {
    return parseJsonWithRepair(partialJson);
  } catch {
    const text = String(partialJson).trim();
    const pairs = {};
    for (const match of text.matchAll(/"([^"]+)"\s*:\s*("[^"]*"|-?\d+(?:\.\d+)?|true|false|null)/g)) {
      try {
        pairs[match[1]] = JSON.parse(match[2]);
      } catch {}
    }
    return pairs;
  }
}

function formatThrownValue(value) {
  if (value && value.stack) return String(value.stack);
  if (value && value.message) return String(value.message);
  if (typeof value === "string") return value;
  try { return JSON.stringify(value); } catch { return String(value); }
}

function extractDiagnosticError(error) {
  return { message: error && error.message ? String(error.message) : formatThrownValue(error), stack: error && error.stack ? String(error.stack) : undefined };
}

function createAssistantMessageDiagnostic(error, title = "Provider error") {
  return { type: "error", title, ...extractDiagnosticError(error) };
}

function appendAssistantMessageDiagnostic(message, diagnostic) {
  message.diagnostics = [...(message.diagnostics || []), diagnostic];
  return message;
}

function getOverflowPatterns() {
  return [/context length/i, /maximum context/i, /too many tokens/i, /context window/i];
}

function isContextOverflow(message, contextWindow = undefined) {
  const text = sdkMessageText(message && message.content ? message.content : message);
  return getOverflowPatterns().some((pattern) => pattern.test(text)) || (contextWindow !== undefined && message && message.usage && Number(message.usage.inputTokens || message.usage.input || 0) > Number(contextWindow));
}

function validateToolCall(tools, toolCall) {
  const name = toolCall && (toolCall.name || toolCall.toolName);
  const tool = (tools || []).find((item) => item && item.name === name);
  if (!tool) return { ok: false, error: `Unknown tool: ${name}` };
  return validateToolArguments(tool, toolCall);
}

function validateToolArguments(_tool, toolCall) {
  return toolCall && typeof (toolCall.arguments || toolCall.input || {}) === "object"
    ? { ok: true, value: toolCall.arguments || toolCall.input || {} }
    : { ok: false, error: "Tool arguments must be an object" };
}

function fauxText(text) { return { type: "text", text: String(text || "") }; }
function fauxThinking(thinking) { return { type: "thinking", thinking: String(thinking || "") }; }
function fauxToolCall(name, arguments_ = {}, options = {}) { return { type: "toolCall", id: options.id || sdkNewEntryId("tool"), name, arguments: arguments_ }; }
function fauxAssistantMessage(content = [], options = {}) {
  return { role: "assistant", content: Array.isArray(content) ? content : [fauxText(content)], stopReason: options.stopReason || "stop", usage: options.usage || { inputTokens: 0, outputTokens: 0 } };
}
function registerFauxProvider(options = {}) {
  const api = options.api || "faux";
  registerApiProvider({
    api,
    stream: (_model, context) => completedAssistantStream(options.message || assistantMessageFromContext(context)),
    streamSimple: (_model, context) => completedAssistantStream(options.message || assistantMessageFromContext(context)),
  }, options.sourceId);
  return { api, unregister: () => unregisterApiProviders(options.sourceId) };
}

resetApiProviders();
registerBuiltInImagesApiProviders();

function piAiExports() {
  return {
    Type,
    StringEnum,
    EventStream,
    AssistantMessageEventStream,
    createAssistantMessageEventStream,
    getModel,
    getProviders,
    getModels,
    calculateCost,
    getSupportedThinkingLevels,
    modelsAreEqual,
    clampThinkingLevel,
    stream,
    complete,
    completeSimple,
    streamSimple,
    registerApiProvider,
    getApiProvider,
    getApiProviders,
    unregisterApiProviders,
    clearApiProviders,
    registerBuiltInApiProviders,
    resetApiProviders,
    streamAnthropic,
    streamSimpleAnthropic,
    streamAzureOpenAIResponses,
    streamSimpleAzureOpenAIResponses,
    streamGoogle,
    streamSimpleGoogle,
    streamGoogleVertex,
    streamSimpleGoogleVertex,
    streamMistral,
    streamSimpleMistral,
    streamOpenAICodexResponses,
    streamSimpleOpenAICodexResponses,
    streamOpenAICompletions,
    streamSimpleOpenAICompletions,
    streamOpenAIResponses,
    streamSimpleOpenAIResponses,
    getImageModel,
    getImageProviders,
    getImageModels,
    registerImagesApiProvider,
    getImagesApiProvider,
    generateImagesOpenRouter,
    registerBuiltInImagesApiProviders,
    generateImages,
    findEnvKeys,
    getEnvApiKey,
    registerSessionResourceCleanup,
    cleanupSessionResources,
    repairJson,
    parseJsonWithRepair,
    parseStreamingJson,
    formatThrownValue,
    extractDiagnosticError,
    createAssistantMessageDiagnostic,
    appendAssistantMessageDiagnostic,
    getOverflowPatterns,
    isContextOverflow,
    validateToolCall,
    validateToolArguments,
    fauxText,
    fauxThinking,
    fauxToolCall,
    fauxAssistantMessage,
    registerFauxProvider,
  };
}

function piAiOauthExports() {
  return {
    registerOAuthProvider,
    unregisterOAuthProvider,
    resetOAuthProviders,
    getOAuthProviders,
    getOAuthProvider,
    getOAuthProviderInfoList: () => getOAuthProviders().map((provider) => ({ id: provider.id, name: provider.name || provider.id })),
    getOAuthApiKey,
  };
}

function piAiSubpathExports(subpath) {
  switch (String(subpath || "")) {
    case "anthropic":
      return { streamAnthropic, streamSimpleAnthropic };
    case "azure-openai-responses":
      return { streamAzureOpenAIResponses, streamSimpleAzureOpenAIResponses };
    case "google":
      return { streamGoogle, streamSimpleGoogle };
    case "google-vertex":
      return { streamGoogleVertex, streamSimpleGoogleVertex };
    case "mistral":
      return { streamMistral, streamSimpleMistral };
    case "openai-codex-responses":
      return {
        streamOpenAICodexResponses,
        streamSimpleOpenAICodexResponses,
        getOpenAICodexWebSocketDebugStats,
        resetOpenAICodexWebSocketDebugStats,
        closeOpenAICodexWebSocketSessions,
      };
    case "openai-completions":
      return {
        streamOpenAICompletions,
        streamSimpleOpenAICompletions,
        convertMessages: convertOpenAICompletionsMessages,
      };
    case "openai-responses":
      return { streamOpenAIResponses, streamSimpleOpenAIResponses };
    case "bedrock-provider":
      return { bedrockProviderModule, streamBedrock, streamSimpleBedrock };
    case "oauth":
      return piAiOauthExports();
    default:
      return null;
  }
}

function fuzzyMatch(query, text) {
  const q = String(query || "").toLowerCase();
  const value = String(text || "").toLowerCase();
  if (!q) return { score: 0, indexes: [] };
  let position = 0;
  const indexes = [];
  for (const char of q) {
    const found = value.indexOf(char, position);
    if (found < 0) return null;
    indexes.push(found);
    position = found + 1;
  }
  return { score: indexes.length, indexes };
}

function fuzzyFilter(items = [], query = "", getText = (item) => String(item || "")) {
  return (Array.isArray(items) ? items : [])
    .map((item) => ({ item, match: fuzzyMatch(query, getText(item)) }))
    .filter((entry) => entry.match)
    .map((entry) => entry.item);
}

class KeybindingsManager {
  constructor(config = {}) { this.config = config; }
  matches(data, keyId) { return matchesKey(data, keyId); }
  get(key) { return this.config[key]; }
}

let activeKeybindings = new KeybindingsManager();
function setKeybindings(keybindings) { activeKeybindings = keybindings || new KeybindingsManager(); }
function getKeybindings() { return activeKeybindings; }

function isFocusable(component) {
  return component !== null && component !== undefined && Object.prototype.hasOwnProperty.call(component, "focused");
}

function getCapabilities() { return {}; }
function getImageDimensions() { return undefined; }
function imageFallback() { return "[image]"; }

function piTuiExports() {
  return {
    Box,
    CancellableLoader,
    Container,
    CURSOR_MARKER,
    Editor,
    Image: ImageComponent,
    Input,
    Key,
    KeybindingsManager,
    Loader,
    Markdown: BridgeTextComponent,
    ProcessTerminal,
    SelectList,
    SettingsList,
    Spacer,
    Text: BridgeTextComponent,
    TruncatedText,
    TUI,
    fuzzyFilter,
    fuzzyMatch,
    getCapabilities,
    getImageDimensions,
    getKeybindings,
    imageFallback,
    isFocusable,
    isKeyRelease,
    isKeyRepeat,
    matchesKey,
    parseKey,
    setKeybindings,
    truncateToWidth,
    visibleWidth,
    wrapTextWithAnsi,
  };
}

function ok(value) { return { ok: true, value }; }
function err(error) { return { ok: false, error }; }
function getOrThrow(result) { if (!result || !result.ok) throw (result && result.error ? result.error : new Error("Result is not ok")); return result.value; }
function getOrUndefined(result) { return result && result.ok ? result.value : undefined; }
function toError(error) {
  if (error instanceof Error) return error;
  if (typeof error === "string") return new Error(error);
  try { return new Error(JSON.stringify(error)); } catch { return new Error(String(error)); }
}

class FileError extends Error {
  constructor(code, message, filePath = undefined, cause = undefined) {
    super(String(message || code || "File error"), cause === undefined ? undefined : { cause });
    this.name = "FileError";
    this.code = code;
    this.path = filePath;
  }
}

class ExecutionError extends Error {
  constructor(code, message, cause = undefined) {
    super(String(message || code || "Execution error"), cause === undefined ? undefined : { cause });
    this.name = "ExecutionError";
    this.code = code;
  }
}

class CompactionError extends Error {
  constructor(code, message, cause = undefined) {
    super(String(message || code || "Compaction error"), cause === undefined ? undefined : { cause });
    this.name = "CompactionError";
    this.code = code;
  }
}

class BranchSummaryError extends Error {
  constructor(code, message, cause = undefined) {
    super(String(message || code || "Branch summary error"), cause === undefined ? undefined : { cause });
    this.name = "BranchSummaryError";
    this.code = code;
  }
}

class SessionError extends Error {
  constructor(code, message, cause = undefined) {
    super(String(message || code || "Session error"), cause === undefined ? undefined : { cause });
    this.name = "SessionError";
    this.code = code;
  }
}

class AgentHarnessError extends Error {
  constructor(code, message, cause = undefined) {
    super(String(message || code || "Agent harness error"), cause === undefined ? undefined : { cause });
    this.name = "AgentHarnessError";
    this.code = code;
  }
}

function createSessionId() { return sdkNewEntryId("session"); }
function createTimestamp() { return new Date().toISOString(); }

function formatSkillInvocation(skill, additionalInstructions = undefined) {
  const filePath = String(skill && skill.filePath || "");
  const skillBlock = `<skill name="${skill && skill.name || ""}" location="${filePath}">\nReferences are relative to ${path.dirname(filePath)}.\n\n${skill && skill.content || ""}\n</skill>`;
  return additionalInstructions ? `${skillBlock}\n\n${additionalInstructions}` : skillBlock;
}

function escapeXml(value) {
  return String(value || "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&apos;");
}

function formatSkillsForSystemPrompt(skills = []) {
  const visibleSkills = (Array.isArray(skills) ? skills : []).filter((skill) => !skill.disableModelInvocation);
  if (!visibleSkills.length) return "";
  const lines = [
    "The following skills provide specialized instructions for specific tasks.",
    "Read the full skill file when the task matches its description.",
    "When a skill file references a relative path, resolve it against the skill directory (parent of SKILL.md / dirname of the path) and use that absolute path in tool commands.",
    "",
    "<available_skills>",
  ];
  for (const skill of visibleSkills) {
    lines.push("  <skill>");
    lines.push(`    <name>${escapeXml(skill.name)}</name>`);
    lines.push(`    <description>${escapeXml(skill.description)}</description>`);
    lines.push(`    <location>${escapeXml(skill.filePath)}</location>`);
    lines.push("  </skill>");
  }
  lines.push("</available_skills>");
  return lines.join("\n");
}

function parseCommandArgs(argsString = "") {
  const args = [];
  let current = "";
  let inQuote = null;
  for (const char of String(argsString || "")) {
    if (inQuote) {
      if (char === inQuote) inQuote = null;
      else current += char;
    } else if (char === '"' || char === "'") {
      inQuote = char;
    } else if (char === " " || char === "\t") {
      if (current) {
        args.push(current);
        current = "";
      }
    } else {
      current += char;
    }
  }
  if (current) args.push(current);
  return args;
}

function substituteArgs(content, args = []) {
  const values = Array.isArray(args) ? args : [];
  let result = String(content || "");
  result = result.replace(/\$(\d+)/g, (_match, num) => values[parseInt(num, 10) - 1] ?? "");
  result = result.replace(/\$\{@:(\d+)(?::(\d+))?\}/g, (_match, startText, lengthText) => {
    const start = Math.max(0, parseInt(startText, 10) - 1);
    return lengthText ? values.slice(start, start + parseInt(lengthText, 10)).join(" ") : values.slice(start).join(" ");
  });
  const allArgs = values.join(" ");
  return result.replace(/\$ARGUMENTS/g, allArgs).replace(/\$@/g, allArgs);
}

function formatPromptTemplateInvocation(template, args = []) {
  return substituteArgs(template && template.content || "", args);
}

function sanitizeBinaryOutput(value) {
  return Array.from(String(value || ""))
    .filter((char) => {
      const code = char.codePointAt(0);
      if (code === undefined) return false;
      if (code === 0x09 || code === 0x0a || code === 0x0d) return true;
      if (code <= 0x1f) return false;
      if (code >= 0xfff9 && code <= 0xfffb) return false;
      return true;
    })
    .join("");
}

class MemorySessionStorage {
  constructor(options = {}) {
    this.metadata = options.metadata || { id: createSessionId(), createdAt: createTimestamp() };
    this.entries = (options.entries || []).map((entry) => ({ ...entry }));
    this.leafId = this.entries.length ? this.entries[this.entries.length - 1].id : null;
  }
  async getMetadata() { return this.metadata; }
  async getLeafId() { return this.leafId; }
  async setLeafId(id) { this.leafId = id; }
  async createEntryId() { return sdkNewEntryId("entry"); }
  async getEntry(id) { return this.entries.find((entry) => entry.id === id); }
  async getEntries() { return this.entries.slice(); }
  async appendEntry(entry) { this.entries.push(entry); this.leafId = entry.id; }
  async findEntries(type) { return this.entries.filter((entry) => entry.type === type); }
  async getLabel(id) {
    for (let i = this.entries.length - 1; i >= 0; i--) {
      const entry = this.entries[i];
      if (entry.type === "label" && entry.targetId === id) return entry.label;
    }
    return undefined;
  }
  async getPathToRoot(leafId = this.leafId) {
    if (leafId === null) return [];
    const byId = new Map(this.entries.map((entry) => [entry.id, entry]));
    let current = leafId ? byId.get(leafId) : undefined;
    if (!current && this.entries.length) current = this.entries[this.entries.length - 1];
    const pathEntries = [];
    const seen = new Set();
    while (current && !seen.has(current.id)) {
      seen.add(current.id);
      pathEntries.unshift(current);
      current = current.parentId ? byId.get(current.parentId) : undefined;
    }
    return pathEntries;
  }
}

class Session {
  constructor(storage) { this.storage = storage; }
  getMetadata() { return this.storage.getMetadata(); }
  getStorage() { return this.storage; }
  getLeafId() { return this.storage.getLeafId(); }
  getEntry(id) { return this.storage.getEntry(id); }
  getEntries() { return this.storage.getEntries(); }
  async getBranch(fromId = undefined) { return this.storage.getPathToRoot(fromId ?? await this.storage.getLeafId()); }
  async buildContext() { return buildSessionContext(await this.getBranch()); }
  getLabel(id) { return this.storage.getLabel(id); }
  async getSessionName() {
    const entries = await this.storage.findEntries("session_info");
    return entries.length ? String(entries[entries.length - 1].name || "").trim() || undefined : undefined;
  }
  async appendTypedEntry(entry) { await this.storage.appendEntry(entry); return entry.id; }
  async appendMessage(message) {
    return this.appendTypedEntry({ type: "message", id: await this.storage.createEntryId(), parentId: await this.storage.getLeafId(), timestamp: createTimestamp(), message });
  }
  async appendThinkingLevelChange(thinkingLevel) {
    return this.appendTypedEntry({ type: "thinking_level_change", id: await this.storage.createEntryId(), parentId: await this.storage.getLeafId(), timestamp: createTimestamp(), thinkingLevel });
  }
  async appendModelChange(provider, modelId) {
    return this.appendTypedEntry({ type: "model_change", id: await this.storage.createEntryId(), parentId: await this.storage.getLeafId(), timestamp: createTimestamp(), provider, modelId });
  }
  async appendCompaction(summary, firstKeptEntryId, tokensBefore, details = undefined, fromHook = undefined) {
    return this.appendTypedEntry({ type: "compaction", id: await this.storage.createEntryId(), parentId: await this.storage.getLeafId(), timestamp: createTimestamp(), summary, firstKeptEntryId, tokensBefore, details, fromHook });
  }
  async appendCustomEntry(customType, data = undefined) {
    return this.appendTypedEntry({ type: "custom", id: await this.storage.createEntryId(), parentId: await this.storage.getLeafId(), timestamp: createTimestamp(), customType, data });
  }
  async appendCustomMessageEntry(customType, content, display, details = undefined) {
    return this.appendTypedEntry({ type: "custom_message", id: await this.storage.createEntryId(), parentId: await this.storage.getLeafId(), timestamp: createTimestamp(), customType, content, display, details });
  }
  async appendLabel(targetId, label) {
    if (!(await this.storage.getEntry(targetId))) throw new SessionError("not_found", `Entry ${targetId} not found`);
    return this.appendTypedEntry({ type: "label", id: await this.storage.createEntryId(), parentId: await this.storage.getLeafId(), timestamp: createTimestamp(), targetId, label });
  }
  async appendSessionName(name) {
    return this.appendTypedEntry({ type: "session_info", id: await this.storage.createEntryId(), parentId: await this.storage.getLeafId(), timestamp: createTimestamp(), name: String(name || "").trim() });
  }
  async moveTo(entryId, summary = undefined) {
    if (entryId !== null && !(await this.storage.getEntry(entryId))) throw new SessionError("not_found", `Entry ${entryId} not found`);
    await this.storage.setLeafId(entryId);
    if (!summary) return undefined;
    return this.appendTypedEntry({ type: "branch_summary", id: await this.storage.createEntryId(), parentId: entryId, timestamp: createTimestamp(), fromId: entryId || "root", summary: summary.summary, details: summary.details, fromHook: summary.fromHook });
  }
}

function toSession(storage) { return new Session(storage); }

async function getEntriesToFork(storage, options = {}) {
  if (!options.entryId) return storage.getEntries();
  const target = await storage.getEntry(options.entryId);
  if (!target) throw new SessionError("invalid_fork_target", `Entry ${options.entryId} not found`);
  const effectiveLeafId = (options.position || "before") === "at" ? target.id : target.parentId;
  return storage.getPathToRoot(effectiveLeafId || null);
}

class InMemorySessionRepo {
  constructor() { this.sessions = new Map(); }
  async create(options = {}) {
    const metadata = { id: options.id || createSessionId(), createdAt: createTimestamp() };
    const session = toSession(new MemorySessionStorage({ metadata }));
    this.sessions.set(metadata.id, session);
    return session;
  }
  async open(metadata) {
    const id = typeof metadata === "string" ? metadata : metadata && metadata.id;
    const session = this.sessions.get(id);
    if (!session) throw new SessionError("not_found", `Session not found: ${id}`);
    return session;
  }
  async list() { return Promise.all([...this.sessions.values()].map((session) => session.getMetadata())); }
  async delete(metadata) { this.sessions.delete(typeof metadata === "string" ? metadata : metadata && metadata.id); }
  async fork(sourceMetadata, options = {}) {
    const source = await this.open(sourceMetadata);
    const entries = await getEntriesToFork(source.getStorage(), options);
    const metadata = { id: options.id || createSessionId(), createdAt: createTimestamp() };
    const session = toSession(new MemorySessionStorage({ metadata, entries }));
    this.sessions.set(metadata.id, session);
    return session;
  }
}

class InMemorySessionStorage extends MemorySessionStorage {}

class JsonlSessionRepo extends InMemorySessionRepo {}
class JsonlSessionStorage extends MemorySessionStorage {}

function defaultAgentState(initialState = {}) {
  return {
    systemPrompt: initialState.systemPrompt || "",
    model: initialState.model || { id: "unknown", name: "unknown", api: "unknown", provider: "unknown", baseUrl: "", reasoning: false, input: [], cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 }, contextWindow: 0, maxTokens: 0 },
    thinkingLevel: initialState.thinkingLevel || "off",
    tools: Array.isArray(initialState.tools) ? initialState.tools.slice() : [],
    messages: Array.isArray(initialState.messages) ? initialState.messages.slice() : [],
    isStreaming: false,
    pendingToolCalls: new Set(),
  };
}

function createAgentStream() {
  return new EventStream((event) => event && event.type === "done", (event) => event.messages || []);
}

function normalizeAgentPromptInput(input, images = undefined) {
  if (Array.isArray(input)) return input;
  if (input && typeof input === "object") return [input];
  const content = [{ type: "text", text: String(input || "") }];
  if (Array.isArray(images)) content.push(...images);
  return [{ role: "user", content, timestamp: Date.now() }];
}

async function runAgentCoreTurn(prompts, context, config = {}, emit = async () => {}, signal = undefined, streamFn = undefined) {
  const newMessages = [...prompts];
  await emit({ type: "agent_start" });
  await emit({ type: "turn_start" });
  for (const prompt of prompts) {
    await emit({ type: "message_start", message: prompt });
    await emit({ type: "message_end", message: prompt });
  }
  const currentContext = { ...context, messages: [...(context.messages || []), ...prompts] };
  const resultStream = (streamFn || config.streamFn || streamSimple)(config.model || (context && context.model) || getModel("custom/model"), currentContext, config);
  const assistant = await normalizeAssistantStream(resultStream, currentContext).result();
  if (assistant) {
    newMessages.push(assistant);
    await emit({ type: "message_start", message: assistant });
    await emit({ type: "message_end", message: assistant });
  }
  await emit({ type: "turn_end", messages: newMessages });
  await emit({ type: "agent_end", messages: newMessages });
  return newMessages;
}

function agentLoop(prompts, context, config = {}, signal = undefined, streamFn = undefined) {
  const eventStream = createAgentStream();
  void runAgentCoreTurn(prompts || [], context || { messages: [], tools: [], systemPrompt: "" }, config, (event) => eventStream.push(event), signal, streamFn)
    .then((messages) => eventStream.end(messages))
    .catch((error) => eventStream.end([{ role: "assistant", content: [{ type: "text", text: formatThrownValue(error) }], stopReason: "error" }]));
  return eventStream;
}

function agentLoopContinue(context, config = {}, signal = undefined, streamFn = undefined) {
  if (!context || !Array.isArray(context.messages) || context.messages.length === 0) throw new Error("Cannot continue: no messages in context");
  if (context.messages[context.messages.length - 1].role === "assistant") throw new Error("Cannot continue from message role: assistant");
  return agentLoop([], context, config, signal, streamFn);
}

const runAgentLoop = runAgentCoreTurn;
const runAgentLoopContinue = (context, config = {}, emit = async () => {}, signal = undefined, streamFn = undefined) =>
  runAgentCoreTurn([], context, config, emit, signal, streamFn);

class Agent {
  constructor(options = {}) {
    this._state = defaultAgentState(options.initialState || {});
    this.listeners = new Set();
    this.streamFn = options.streamFn || streamSimple;
    this.convertToLlm = options.convertToLlm || ((messages) => messages.filter((message) => ["user", "assistant", "toolResult"].includes(message.role)));
    this.steeringQueue = [];
    this.followUpQueue = [];
    this.steeringMode = options.steeringMode || "one-at-a-time";
    this.followUpMode = options.followUpMode || "one-at-a-time";
    this.transport = options.transport || "auto";
    this.toolExecution = options.toolExecution || "parallel";
    this.sessionId = options.sessionId;
  }
  subscribe(listener) { this.listeners.add(listener); return () => this.listeners.delete(listener); }
  get state() { return this._state; }
  steer(message) { this.steeringQueue.push(message); }
  followUp(message) { this.followUpQueue.push(message); }
  clearSteeringQueue() { this.steeringQueue = []; }
  clearFollowUpQueue() { this.followUpQueue = []; }
  clearAllQueues() { this.clearSteeringQueue(); this.clearFollowUpQueue(); }
  hasQueuedMessages() { return this.steeringQueue.length > 0 || this.followUpQueue.length > 0; }
  abort() { if (this.abortController) this.abortController.abort(); }
  waitForIdle() { return this.activeRun || Promise.resolve(); }
  reset() { this._state.messages = []; this._state.isStreaming = false; this._state.pendingToolCalls = new Set(); this.clearAllQueues(); }
  async emit(event, signal = undefined) { for (const listener of [...this.listeners]) await listener(event, signal || new AbortController().signal); }
  async prompt(input, images = undefined) {
    if (this.activeRun) throw new Error("Agent is already processing a prompt. Use steer() or followUp() to queue messages, or wait for completion.");
    const prompts = normalizeAgentPromptInput(input, images);
    this.abortController = new AbortController();
    this.activeRun = (async () => {
      this._state.isStreaming = true;
      const messages = await runAgentCoreTurn(
        prompts,
        { systemPrompt: this._state.systemPrompt, messages: this._state.messages.slice(), tools: this._state.tools.slice() },
        { model: this._state.model, streamFn: this.streamFn, transport: this.transport, toolExecution: this.toolExecution },
        async (event) => this.emit(event, this.abortController.signal),
        this.abortController.signal,
        this.streamFn,
      );
      this._state.messages.push(...messages);
      this._state.isStreaming = false;
    })().finally(() => { this.activeRun = undefined; });
    return this.activeRun;
  }
  async continue() {
    if (this.activeRun) throw new Error("Agent is already processing. Wait for completion before continuing.");
    const last = this._state.messages[this._state.messages.length - 1];
    if (!last) throw new Error("No messages to continue from");
    if (last.role === "assistant") throw new Error("Cannot continue from message role: assistant");
    return this.prompt([]);
  }
}

class AgentHarness {
  constructor(options = {}) { this.options = options; this.agent = options.agent || new Agent(options); }
}

function streamProxy(model, context, options = {}) {
  return normalizeAssistantStream((options.streamFn || streamSimple)(model, context, options), context);
}

function nodeFileError(error, filePath = undefined) {
  if (error && error.code === "ENOENT") return new FileError("not_found", error.message, filePath || error.path, error);
  if (error && error.code === "EACCES") return new FileError("permission_denied", error.message, filePath || error.path, error);
  if (error && error.code === "ENOTDIR") return new FileError("not_directory", error.message, filePath || error.path, error);
  if (error && error.code === "EISDIR") return new FileError("is_directory", error.message, filePath || error.path, error);
  return new FileError("unknown", error && error.message ? error.message : String(error), filePath || (error && error.path), error instanceof Error ? error : undefined);
}

function nodeFileKind(stat) {
  if (stat.isSymbolicLink()) return "symlink";
  if (stat.isDirectory()) return "directory";
  return "file";
}

class NodeExecutionEnv {
  constructor(options = {}) {
    this.cwd = options.cwd || process.cwd();
    this.shellPath = options.shellPath;
    this.shellEnv = options.shellEnv;
  }
  resolve(filePath) { return path.isAbsolute(String(filePath || "")) ? String(filePath || "") : path.resolve(this.cwd, String(filePath || "")); }
  async absolutePath(filePath) { return ok(this.resolve(filePath)); }
  async joinPath(parts) { return ok(path.join(...(Array.isArray(parts) ? parts : []))); }
  async readTextFile(filePath) {
    const resolved = this.resolve(filePath);
    try { return ok(fs.readFileSync(resolved, "utf8")); } catch (error) { return err(nodeFileError(error, resolved)); }
  }
  async readTextLines(filePath, options = {}) {
    const result = await this.readTextFile(filePath);
    if (!result.ok) return result;
    const lines = String(result.value || "").split(/\r?\n/);
    if (lines.length && lines[lines.length - 1] === "") lines.pop();
    return ok(options.maxLines ? lines.slice(0, options.maxLines) : lines);
  }
  async readBinaryFile(filePath) {
    const resolved = this.resolve(filePath);
    try { return ok(new Uint8Array(fs.readFileSync(resolved))); } catch (error) { return err(nodeFileError(error, resolved)); }
  }
  async writeFile(filePath, content) {
    const resolved = this.resolve(filePath);
    try {
      ensureParentDir(resolved);
      fs.writeFileSync(resolved, content);
      return ok(undefined);
    } catch (error) {
      return err(nodeFileError(error, resolved));
    }
  }
  async appendFile(filePath, content) {
    const resolved = this.resolve(filePath);
    try {
      ensureParentDir(resolved);
      fs.appendFileSync(resolved, content);
      return ok(undefined);
    } catch (error) {
      return err(nodeFileError(error, resolved));
    }
  }
  fileInfoFromPath(filePath) {
    const resolved = this.resolve(filePath);
    const stat = fs.lstatSync(resolved);
    return { name: path.basename(resolved), path: resolved, kind: nodeFileKind(stat), size: stat.size, mtimeMs: stat.mtimeMs };
  }
  async fileInfo(filePath) {
    const resolved = this.resolve(filePath);
    try { return ok(this.fileInfoFromPath(resolved)); } catch (error) { return err(nodeFileError(error, resolved)); }
  }
  async listDir(filePath) {
    const resolved = this.resolve(filePath);
    try { return ok(fs.readdirSync(resolved).map((name) => this.fileInfoFromPath(path.join(resolved, name)))); } catch (error) { return err(nodeFileError(error, resolved)); }
  }
  async canonicalPath(filePath) {
    const resolved = this.resolve(filePath);
    try { return ok(fs.realpathSync(resolved)); } catch (error) { return err(nodeFileError(error, resolved)); }
  }
  async exists(filePath) {
    const resolved = this.resolve(filePath);
    try { return ok(fs.existsSync(resolved)); } catch (error) { return err(nodeFileError(error, resolved)); }
  }
  async createDir(filePath, options = {}) {
    const resolved = this.resolve(filePath);
    try { fs.mkdirSync(resolved, { recursive: options.recursive !== false }); return ok(undefined); } catch (error) { return err(nodeFileError(error, resolved)); }
  }
  async remove(filePath, options = {}) {
    const resolved = this.resolve(filePath);
    try { fs.rmSync(resolved, { recursive: !!options.recursive, force: !!options.force }); return ok(undefined); } catch (error) { return err(nodeFileError(error, resolved)); }
  }
  async createTempDir(prefix = "tmp-") {
    try { return ok(fs.mkdtempSync(path.join(os.tmpdir(), String(prefix || "tmp-")))); } catch (error) { return err(nodeFileError(error)); }
  }
  async createTempFile(options = {}) {
    try {
      const dir = fs.mkdtempSync(path.join(os.tmpdir(), String(options.prefix || "tmp-")));
      const filePath = path.join(dir, `${Date.now().toString(36)}${options.suffix || ""}`);
      fs.writeFileSync(filePath, "");
      return ok(filePath);
    } catch (error) {
      return err(nodeFileError(error));
    }
  }
  async exec(command, options = {}) {
    return new Promise((resolve) => {
      const cwd = options.cwd ? this.resolve(options.cwd) : this.cwd;
      const child = childProcess.exec(String(command || ""), {
        cwd,
        env: { ...process.env, ...(this.shellEnv || {}), ...(options.env || {}) },
        timeout: options.timeout ? Number(options.timeout) * 1000 : undefined,
        shell: this.shellPath || undefined,
      }, (error, stdout, stderr) => {
        if (error && error.killed) {
          resolve(err(new ExecutionError("timeout", error.message, error)));
          return;
        }
        resolve(ok({ stdout: String(stdout || ""), stderr: String(stderr || ""), exitCode: error && typeof error.code === "number" ? error.code : 0 }));
      });
      if (options.abortSignal) {
        if (options.abortSignal.aborted) child.kill();
        else options.abortSignal.addEventListener("abort", () => child.kill(), { once: true });
      }
      if (child.stdout && typeof options.onStdout === "function") child.stdout.on("data", (chunk) => options.onStdout(String(chunk)));
      if (child.stderr && typeof options.onStderr === "function") child.stderr.on("data", (chunk) => options.onStderr(String(chunk)));
    });
  }
  async cleanup() {}
}

function piAgentCoreNodeExports() {
  return { ...piAgentCoreExports(), NodeExecutionEnv };
}

function piAgentCoreExports() {
  return {
    Agent,
    agentLoop,
    agentLoopContinue,
    runAgentLoop,
    runAgentLoopContinue,
    AgentHarness,
    calculateContextTokens,
    collectEntriesForBranchSummary,
    compact,
    DEFAULT_COMPACTION_SETTINGS,
    estimateContextTokens,
    estimateTokens,
    findCutPoint,
    findTurnStartIndex,
    generateBranchSummary,
    generateSummary,
    getLastAssistantUsage,
    prepareBranchEntries,
    serializeConversation,
    shouldCompact,
    buildSessionContext,
    COMPACTION_SUMMARY_PREFIX,
    COMPACTION_SUMMARY_SUFFIX,
    BRANCH_SUMMARY_PREFIX,
    BRANCH_SUMMARY_SUFFIX,
    bashExecutionToText,
    createBranchSummaryMessage,
    createCompactionSummaryMessage,
    createCustomMessage,
    convertToLlm,
    parseCommandArgs,
    substituteArgs,
    formatPromptTemplateInvocation,
    JsonlSessionRepo,
    JsonlSessionStorage,
    InMemorySessionRepo,
    InMemorySessionStorage,
    createSessionId,
    createTimestamp,
    toSession,
    getEntriesToFork,
    uuidv7: () => sdkNewEntryId("uuid"),
    formatSkillInvocation,
    formatSkillsForSystemPrompt,
    ok,
    err,
    getOrThrow,
    getOrUndefined,
    toError,
    FileError,
    ExecutionError,
    CompactionError,
    BranchSummaryError,
    SessionError,
    AgentHarnessError,
    Session,
    DEFAULT_MAX_LINES,
    DEFAULT_MAX_BYTES,
    GREP_MAX_LINE_LENGTH: 500,
    formatSize,
    truncateHead,
    truncateTail,
    truncateLine,
    sanitizeBinaryOutput,
    createFileOps,
    extractFileOpsFromMessage,
    computeFileLists,
    formatFileOperations,
    streamProxy,
  };
}

const bridgeModuleCache = new Map();

function resolveLocalModule(baseDir, specifier) {
  const raw = path.resolve(baseDir, specifier);
  const candidates = [raw];
  if (!path.extname(raw)) {
    candidates.push(`${raw}.ts`, `${raw}.js`, `${raw}.cjs`, `${raw}.mjs`, `${raw}.json`);
  }
  for (const candidate of candidates) {
    if (fs.existsSync(candidate) && fs.statSync(candidate).isFile()) return candidate;
  }
  if (fs.existsSync(raw) && fs.statSync(raw).isDirectory()) {
    const manifest = jsonFile(path.join(raw, "package.json"), {});
    for (const field of ["main", "module"]) {
      if (manifest[field]) {
        const entry = resolveLocalModule(raw, String(manifest[field]));
        if (entry) return entry;
      }
    }
    for (const name of ["index.ts", "index.js", "index.cjs", "index.mjs", "index.json"]) {
      const candidate = path.join(raw, name);
      if (fs.existsSync(candidate) && fs.statSync(candidate).isFile()) return candidate;
    }
  }
  return raw;
}

function localRequire(baseDir) {
  return (specifier) => {
    if (specifier === "typebox" || specifier === "@sinclair/typebox") return typeboxExports();
    if (specifier === "typebox/compile" || specifier === "typebox/compiler" || specifier === "@sinclair/typebox/compile" || specifier === "@sinclair/typebox/compiler") return typeboxCompileExports();
    if (specifier === "typebox/value" || specifier === "@sinclair/typebox/value") return typeboxValueExports();
    if (specifier === "typebox/error" || specifier === "@sinclair/typebox/error") return typeboxErrorExports();
    if (specifier === "@earendil-works/pi-coding-agent/hooks" || specifier === "@mariozechner/pi-coding-agent/hooks") {
      return piCodingAgentExports();
    }
    if (specifier === "@earendil-works/pi-coding-agent" || specifier === "@mariozechner/pi-coding-agent") {
      return piCodingAgentExports();
    }
    if (specifier === "@earendil-works/pi-tui" || specifier === "@mariozechner/pi-tui") {
      return piTuiExports();
    }
    if (specifier === "@earendil-works/pi-ai" || specifier === "@mariozechner/pi-ai") {
      return piAiExports();
    }
    if (specifier === "@earendil-works/pi-ai/oauth" || specifier === "@mariozechner/pi-ai/oauth") {
      return piAiOauthExports();
    }
    if (specifier.startsWith("@earendil-works/pi-ai/") || specifier.startsWith("@mariozechner/pi-ai/")) {
      const subpath = specifier.replace(/^@(earendil-works|mariozechner)\/pi-ai\//, "");
      const exports = piAiSubpathExports(subpath);
      if (exports) return exports;
    }
    if (specifier === "@earendil-works/pi-agent-core/node" || specifier === "@mariozechner/pi-agent-core/node") {
      return piAgentCoreNodeExports();
    }
    if (specifier === "@earendil-works/pi-agent-core" || specifier === "@mariozechner/pi-agent-core") {
      return piAgentCoreExports();
    }
    if (specifier.startsWith(".")) {
      const resolved = resolveLocalModule(baseDir, specifier);
      if (/\.(ts|js)$/i.test(resolved)) return loadBridgeModule(resolved);
      return require(resolved);
    }
    return require(specifier);
  };
}

function transformImportBindings(bindings) {
  return String(bindings || "")
    .split(",")
    .map((part) => part.trim())
    .filter((part) => part && !part.startsWith("type "))
    .map((part) => part.replace(/\s+as\s+/g, ": "))
    .join(", ");
}

function transformExportBindings(bindings) {
  return String(bindings || "")
    .split(",")
    .map((part) => part.trim())
    .filter(Boolean)
    .map((part) => {
      const match = part.match(/^([A-Za-z_$][\w$]*)\s+as\s+([A-Za-z_$][\w$]*|default)$/);
      if (match) return { local: match[1], exported: match[2] };
      return { local: part, exported: part };
    });
}

function stripTypeOnlyDeclarations(code) {
  const lines = String(code || "").split("\n");
  const out = [];
  let skipping = false;
  let depth = 0;
  let untilSemicolon = false;
  const updateDepth = (line) => {
    for (const char of line) {
      if (char === "{" || char === "(" || char === "[") depth += 1;
      else if (char === "}" || char === ")" || char === "]") depth = Math.max(0, depth - 1);
    }
  };
  for (const line of lines) {
    const trimmed = line.trim();
    if (!skipping && /^(export\s+)?interface\s+[A-Za-z_$][\w$]*/.test(trimmed)) {
      skipping = true;
      untilSemicolon = false;
      depth = 0;
      updateDepth(line);
      if (depth === 0 && trimmed.endsWith("}")) skipping = false;
      continue;
    }
    if (!skipping && /^(export\s+)?type\s+[A-Za-z_$][\w$]*(?:<[^>]+>)?\s*=/.test(trimmed)) {
      skipping = true;
      untilSemicolon = true;
      depth = 0;
      updateDepth(line);
      if (trimmed.endsWith(";") && depth === 0) skipping = false;
      continue;
    }
    if (skipping) {
      updateDepth(line);
      if (untilSemicolon ? trimmed.endsWith(";") && depth === 0 : depth === 0 && trimmed.endsWith("}")) {
        skipping = false;
      }
      continue;
    }
    out.push(line);
  }
  return out.join("\n");
}

function stripParamTypes(params) {
  return String(params || "")
    .replace(/(\.\.\.)?([A-Za-z_$][\w$]*)\??\s*:\s*[^,)=]+(?=\s*(?:,|=|$))/g, (_m, rest, name) => `${rest || ""}${name}`)
    .replace(/(\{[^{}]*\}|\[[^\[\]]*\])\s*:\s*[^,)=]+(?=\s*(?:,|=|$))/g, "$1");
}

function stripTypeScriptSyntax(code) {
  let out = stripTypeOnlyDeclarations(code);
  out = out.replace(/\b(private|public|protected|readonly)\s+/g, "");
  out = out.replace(/(function\s+[A-Za-z_$][\w$]*)<[^()\n]+>\s*\(/g, "$1(");
  out = out.replace(/([A-Za-z_$][\w$]*(?:\.[A-Za-z_$][\w$]*)*)<[^<>\n]+>\s*\(/g, "$1(");
  out = out.replace(/(function(?:\s+[A-Za-z_$][\w$]*)?\s*)\(([^()\n]*)\)/g,
    (_m, prefix, params) => `${prefix}(${stripParamTypes(params)})`);
  out = out.replace(/(constructor\s*)\(([^()\n]*)\)/g,
    (_m, prefix, params) => `${prefix}(${stripParamTypes(params)})`);
  out = out.replace(/^(\s*[A-Za-z_$][\w$]*\s*)\(([^()\n]*)\)(\s*:\s*[^({=>;\n]+(?:\[\])?)?\s*\{/mg,
    (_m, prefix, params) => `${prefix}(${stripParamTypes(params)}) {`);
  out = out.replace(/\(([^()\n]*)\)\s*:\s*[^=\n]+?\s*=>/g,
    (_m, params) => `(${stripParamTypes(params)}) =>`);
  out = out.replace(/\(([^()\n]*)\)\s*=>/g,
    (_m, params) => `(${stripParamTypes(params)}) =>`);
  out = out.replace(/(function(?:\s+[A-Za-z_$][\w$]*)?\s*\([^()\n]*\))\s*:\s*[^({=>;\n]+(?:\[\])?\s*(?=\{)/g, "$1 ");
  out = out.replace(/\b(const|let|var)\s+([A-Za-z_$][\w$]*)\s*:\s*[^=;\n]+(?=\s*[=;])/g, "$1 $2");
  out = out.replace(/^(\s*[A-Za-z_$][\w$]*)\s*:\s*[^=;\n]+;/mg, "$1;");
  const preservedExportLists = [];
  out = out.replace(/^\s*export\s+\{[^}]+\};?\s*$/mg, (match) => {
    const token = `__PI_EXPORT_LIST_${preservedExportLists.length}__`;
    preservedExportLists.push(match);
    return token;
  });
  out = out.replace(/\s+as\s+const\b/g, "");
  out = out.replace(/\s+as\s+\{[\s\S]*?\}(?:\s*\|\s*[A-Za-z_$][\w$]*)?(?=\s*[;\),\]\}])/g, "");
  out = out.replace(/\s+as\s+[A-Za-z_$][\w$]*(?:<[^>\n]+>)?(?:\[\])?(?=\s*[,;\)\]\}])/g, "");
  out = out.replace(/\s+satisfies\s+[A-Za-z_$][\w$]*(?:<[^>\n]+>)?(?=\s*[,;\)\]\}])/g, "");
  out = out.replace(/__PI_EXPORT_LIST_(\d+)__/g, (_m, index) => preservedExportLists[Number(index)] || "");
  return out;
}

function transformModule(source) {
  let code = source;
  const namedExports = [];
  let importCounter = 0;
  code = stripTypeOnlyDeclarations(code);
  code = code.replace(/^\s*import\s+type\s+[\s\S]*?\s+from\s+["'][^"']+["'];?\s*$/mg, "");
  code = code.replace(/^\s*import\s+([A-Za-z_$][\w$]*)\s*,\s*\{([\s\S]*?)\}\s+from\s+["']([^"']+)["'];?\s*$/mg,
    (_m, defaultName, names, spec) => {
      const mod = `__import_${++importCounter}`;
      const bindings = transformImportBindings(names);
      return [
        `const ${mod} = require(${JSON.stringify(spec)});`,
        `const ${defaultName} = ${mod}.default ?? ${mod};`,
        bindings ? `const {${bindings}} = ${mod};` : "",
      ].filter(Boolean).join("\n");
    });
  code = code.replace(/^\s*import\s+([A-Za-z_$][\w$]*)\s*,\s*\*\s+as\s+([A-Za-z_$][\w$]*)\s+from\s+["']([^"']+)["'];?\s*$/mg,
    (_m, defaultName, namespaceName, spec) => {
      const mod = `__import_${++importCounter}`;
      return `const ${mod} = require(${JSON.stringify(spec)});\nconst ${defaultName} = ${mod}.default ?? ${mod};\nconst ${namespaceName} = ${mod};`;
    });
  code = code.replace(/^\s*import\s+\*\s+as\s+([A-Za-z_$][\w$]*)\s+from\s+["']([^"']+)["'];?\s*$/mg,
    (_m, name, spec) => `const ${name} = require(${JSON.stringify(spec)});`);
  code = code.replace(/^\s*import\s+\{([^}]+)\}\s+from\s+["']([^"']+)["'];?\s*$/mg,
    (_m, names, spec) => {
      const bindings = transformImportBindings(names);
      return bindings ? `const {${bindings}} = require(${JSON.stringify(spec)});` : "";
    });
  code = code.replace(/^\s*import\s+([A-Za-z_$][\w$]*)\s+from\s+["']([^"']+)["'];?\s*$/mg,
    (_m, name, spec) => `const ${name} = require(${JSON.stringify(spec)}).default ?? require(${JSON.stringify(spec)});`);
  code = code.replace(/^\s*import\s+["']([^"']+)["'];?\s*$/mg,
    (_m, spec) => `require(${JSON.stringify(spec)});`);
  code = stripTypeScriptSyntax(code);
  code = code.replace(/export\s+default\s+async\s+function\s*/g, "module.exports.default = async function ");
  code = code.replace(/export\s+default\s+function\s*/g, "module.exports.default = function ");
  code = code.replace(/export\s+default\s+async\s*\(/g, "module.exports.default = async (");
  code = code.replace(/export\s+default\s*\(/g, "module.exports.default = (");
  code = code.replace(/export\s+default\s+/g, "module.exports.default = ");
  code = code.replace(/export\s+async\s+function\s+([A-Za-z_$][\w$]*)/g, (_m, name) => {
    namedExports.push({ local: name, exported: name });
    return `async function ${name}`;
  });
  code = code.replace(/export\s+function\s+([A-Za-z_$][\w$]*)/g, (_m, name) => {
    namedExports.push({ local: name, exported: name });
    return `function ${name}`;
  });
  code = code.replace(/export\s+class\s+([A-Za-z_$][\w$]*)/g, (_m, name) => {
    namedExports.push({ local: name, exported: name });
    return `class ${name}`;
  });
  code = code.replace(/export\s+(const|let|var)\s+([A-Za-z_$][\w$]*)\s*=/g, (_m, kind, name) => {
    namedExports.push({ local: name, exported: name });
    return `${kind} ${name} =`;
  });
  code = code.replace(/^\s*export\s+\{([^}]+)\};?\s*$/mg, (_m, names) =>
    transformExportBindings(names)
      .map((entry) => entry.exported === "default"
        ? `module.exports.default = ${entry.local};`
        : `module.exports.${entry.exported} = ${entry.local};`)
      .join("\n"));
  if (namedExports.length > 0) {
    code += "\n" + namedExports
      .map((entry) => `module.exports.${entry.exported} = ${entry.local};`)
      .join("\n");
  }
  return code;
}

function loadBridgeModule(filePath) {
  const resolved = path.resolve(filePath);
  const ext = path.extname(resolved);
  if (ext === ".json" || ext === ".cjs") return require(resolved);
  if (ext === ".mjs") {
    throw new Error(`Cannot synchronously require ESM module: ${resolved}`);
  }
  if (bridgeModuleCache.has(resolved)) return bridgeModuleCache.get(resolved).exports;
  const source = fs.readFileSync(resolved, "utf8");
  const module = { exports: {} };
  bridgeModuleCache.set(resolved, module);
  const dirname = path.dirname(resolved);
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
    __dirname: dirname,
    __filename: resolved,
  };
  vm.runInNewContext(transformModule(source), sandbox, { filename: resolved });
  return module.exports;
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
    __dirname: dirname,
    __filename: extensionPath,
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
    getFlag: (name) => {
      const key = normalizeFlagName(name);
      if (runtime.flagValues.has(key)) return runtime.flagValues.get(key);
      return defaultFlagValue(extension.flags.get(key) || {});
    },
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
    const context = Object.defineProperties({}, Object.getOwnPropertyDescriptors(this.createContext()));
    const actions = this.commandActions || {};
    context.waitForIdle = () => typeof actions.waitForIdle === "function" ? actions.waitForIdle() : Promise.resolve();
    context.newSession = (options) => typeof actions.newSession === "function" ? actions.newSession(options) : Promise.resolve({ cancelled: false });
    context.fork = (entryId, options) => typeof actions.fork === "function" ? actions.fork(entryId, options) : Promise.resolve({ cancelled: false });
    context.navigateTree = (targetId, options) => typeof actions.navigateTree === "function" ? actions.navigateTree(targetId, options) : Promise.resolve({ cancelled: false });
    context.switchSession = (sessionPath, options) => typeof actions.switchSession === "function" ? actions.switchSession(sessionPath, options) : Promise.resolve({ cancelled: false });
    context.reload = () => typeof actions.reload === "function" ? actions.reload() : Promise.resolve();
    return context;
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
  const entries = () => Array.isArray(session.entries) ? session.entries : [];
  const entryById = () => new Map(entries().filter((entry) => typeof entry.id === "string").map((entry) => [entry.id, entry]));
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
    for (const entry of entries()) {
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
    for (const entry of entries()) current = leafIdAfterEntry(current, entry);
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
      version: CURRENT_SESSION_VERSION,
      id: session.id || session.path || "session",
      sessionId: session.id || undefined,
      name: session.name || undefined,
      cwd: session.cwd || process.cwd(),
      timestamp: session.timestamp || undefined,
      parentSession: session.parentSession || undefined,
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
    return entries().filter((entry) => entry && entry.parentId === id);
  };
  const getTree = () => {
    const labels = labelById();
    const nodes = new Map();
    const roots = [];
    for (const entry of entries()) {
      if (typeof entry.id !== "string") continue;
      nodes.set(entry.id, { entry, children: [], label: labels.get(entry.id) });
    }
    for (const entry of entries()) {
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
    getEntries: () => entries().slice(),
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
    isUsingOAuth: (model) => {
      const authStorage = registry && registry.authStorage;
      const credential = model && authStorage && typeof authStorage.get === "function" ? authStorage.get(model.provider) : undefined;
      return !!(credential && credential.type === "oauth");
    },
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
  at_exit (fun () -> try Sys.remove path with Sys.Break as e -> raise e | _ -> ());
  path)


let path () = Lazy.force bridge_path
