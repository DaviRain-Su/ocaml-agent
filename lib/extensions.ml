(* Extension loading: register custom tools declared in a JSON manifest or in a
   Pi-style TypeScript/JavaScript extension. JSON tools run an external command,
   receiving the tool input as JSON on stdin and returning stdout/stderr. TS/JS
   extensions are loaded through a small Node bridge that supports the core
   pi.registerTool() path.

   Manifest (default .ocaml-agent/tools.json, or AGENT_TOOLS_FILE):
     { "tools": [
         { "name": "weather",
           "description": "Get weather for a city.",
           "parameters": { "type":"object",
                           "properties": { "city": {"type":"string"} },
                           "required": ["city"] },
           "command": "python3 ./ext/weather.py" } ] } *)

open Yojson.Safe.Util

type command =
  { name : string;
    description : string;
    argument_hint : string option;
    has_argument_completions : bool;
    path : string;
    runtime : extension_runtime }

and extension_runtime =
  | Node
  | Ocaml_sdk

type shortcut =
  { spec : string;
    description : string;
    path : string;
    command : string option;
    has_handler : bool }

type shortcut_result =
  | Shortcut_output of string
  | Shortcut_command of string

type ui_capture =
  { notifications : string list;
    requests : Yojson.Safe.t list;
    surfaces : Yojson.Safe.t list;
    messages : Yojson.Safe.t list }

type model_choice =
  { provider : string option;
    model : string option;
    thinking : string option }

type command_response =
  { text : string;
    ui : ui_capture;
    thinking_level : string option;
    model_choice : model_choice option;
    session_name : string option;
    session_entries : Yojson.Safe.t list;
    theme_name : string option;
    tools_expanded : bool option;
    abort_requested : bool;
    shutdown_requested : bool;
    compact_requests : Yojson.Safe.t list;
    reload_requested : bool;
    session_actions : Yojson.Safe.t list }

type shortcut_response =
  | Shortcut_response_output of command_response
  | Shortcut_response_command of string

type render_response =
  { rendered : string;
    components : Yojson.Safe.t list;
    render_ui : ui_capture }

type message_renderer =
  { name : string;
    description : string;
    target : string;
    path : string }

type tool_call_result =
  | Tool_continue of Yojson.Safe.t
  | Tool_block of string

type input_result =
  | Input_continue of string
  | Input_handled

type user_bash_result =
  { exit_code : int;
    output : string }

let command_registry : command list ref = ref []
let shortcut_registry : shortcut list ref = ref []
let message_renderer_registry : message_renderer list ref = ref []
let event_paths : (string * string list) list ref = ref []
let ocaml_event_paths : (string * string list) list ref = ref []
let js_extension_paths : string list ref = ref []
let discovered_skill_paths : string list ref = ref []
let discovered_prompt_paths : string list ref = ref []
let discovered_theme_paths : string list ref = ref []
let active_tool_names : string list option ref = ref None
let active_thinking_level : string option ref = ref None
let active_model_choice : model_choice option ref = ref None

let skill_paths () = !discovered_skill_paths
let prompt_paths () = !discovered_prompt_paths
let theme_paths () = !discovered_theme_paths
let active_tools () = !active_tool_names
let clear_active_tools () = active_tool_names := None
let active_thinking () = !active_thinking_level
let clear_active_thinking () = active_thinking_level := None
let active_model () = !active_model_choice
let clear_active_model () = active_model_choice := None

let set_active_tools names =
  active_tool_names := Some (Tools.canonical_names names)

let set_active_thinking level =
  active_thinking_level := Some (Model_spec.normalize_thinking level)

let set_active_model choice =
  active_model_choice := Some choice

let effective_tool_names base =
  match (!active_tool_names, base) with
  | None, names -> names
  | Some active, None -> Some active
  | Some active, Some names -> Some (List.filter (fun name -> List.mem name names) active)

let effective_thinking base =
  match !active_thinking_level with Some level -> level | None -> base

let env_nonempty k =
  match Sys.getenv_opt k with Some s when String.trim s <> "" -> Some s | _ -> None

let truthy s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "y" | "all" -> true
  | _ -> false

let split_paths s =
  s |> String.split_on_char '\n' |> List.map String.trim |> List.filter (fun p -> p <> "")

let user_extensions_dir () = Filename.concat (Config_paths.agent_dir ()) "extensions"

let manifest_paths () =
  let explicit =
    (match env_nonempty "AGENT_TOOLS_FILE" with Some p -> [ p ] | None -> [])
    @ (match env_nonempty "AGENT_EXTENSION_PATHS" with Some s -> split_paths s | None -> [])
  in
  let defaults =
    match Sys.getenv_opt "AGENT_NO_EXTENSIONS" with
    | Some s when truthy s -> []
    | _ ->
      [ Config_paths.user_tools_manifest ();
        Config_paths.user_tools_dir ();
        user_extensions_dir ();
        ".ocaml-agent/extensions";
        ".ocaml-agent/tools.json";
        ".pi/extensions";
        ".pi/tools.json" ]
      @ Packages.paths Packages.Extension
      @ Settings.string_list "extensions"
  in
  Config_paths.uniq (defaults @ explicit)

let is_json_file path = Filename.check_suffix path ".json"

let is_js_extension_file path =
  List.exists (Filename.check_suffix path) [ ".ts"; ".js"; ".mjs"; ".cjs" ]

let is_ocaml_sdk_extension_file path =
  List.exists (Filename.check_suffix path) [ ".ocamlext"; ".ocaml-extension" ]

let files_in_dir path =
  match Sys.readdir path with
  | exception _ -> []
  | entries ->
    Array.to_list entries
    |> List.filter (fun name -> is_json_file name || is_js_extension_file name || is_ocaml_sdk_extension_file name)
    |> List.sort compare
    |> List.map (Filename.concat path)

let index_files path =
  [ "index.ts"; "index.js"; "index.mjs"; "index.cjs"; "index.ocamlext"; "index.ocaml-extension" ]
  |> List.map (Filename.concat path)
  |> List.filter Sys.file_exists

let expand_manifest_path path =
  if Sys.file_exists path && Sys.is_directory path then
    let direct = files_in_dir path in
    let nested =
      match Sys.readdir path with
      | exception _ -> []
      | entries ->
        Array.to_list entries
        |> List.concat_map (fun name ->
               let full = Filename.concat path name in
               if Sys.file_exists full && Sys.is_directory full then index_files full else [])
    in
    direct @ nested |> Config_paths.uniq
  else if is_json_file path || is_js_extension_file path || is_ocaml_sdk_extension_file path then [ path ]
  else []

(* Run [command], feeding [input] on stdin, returning combined stdout+stderr. *)
let run_command command input =
  let code, body = Tools.run_process ~stdin_data:input command in
  if code = 0 then body else Printf.sprintf "(exit %d)\n%s" code body

let tool_of_json (j : Yojson.Safe.t) : Tools.tool option =
  match (j |> member "name", j |> member "command") with
  | `String name, `String command when name <> "" && command <> "" ->
    let description = match j |> member "description" with `String s -> s | _ -> "" in
    let parameters =
      match j |> member "parameters" with
      | `Null -> `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ]
      | p -> p
    in
    Some
      { Tools.name;
        description;
        parameters;
        requires_approval = true;
        execute = (fun input -> try run_command command (Yojson.Safe.to_string input) with
        | Sys.Break as e -> raise e
        | e -> "Error: " ^ Printexc.to_string e) }
  | _ -> None

let load_manifest path =
  if not (Sys.file_exists path) then []
  else
    match
      let ic = open_in path in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> Yojson.Safe.from_channel ic)
    with
    | exception Yojson.Json_error msg ->
      Printf.eprintf "[warning] extension manifest %s has invalid JSON: %s\n%!" path msg;
      []
    | exception e ->
      Printf.eprintf "[warning] failed to read extension manifest %s: %s\n%!" path (Printexc.to_string e);
      []
    | json ->
      let entries = match json |> member "tools" with `List l -> l | _ -> [] in
      if entries = [] then
        Printf.eprintf "[warning] extension manifest %s has no tools array or it is empty\n%!" path;
      List.filter_map
        (fun j ->
          match tool_of_json j with
          | Some t when Tools.register t -> Some t.Tools.name
          | Some _ -> None
          | None ->
            let name =
              match j |> member "name" with `String s -> s | _ -> "(unnamed)"
            in
            Printf.eprintf "[warning] extension tool %s in %s is missing a required field (name or command)\n%!" name path;
            None)
        entries

let node_bridge_source =
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
  Tools.write_file_contents path node_bridge_source;
  at_exit (fun () -> try Sys.remove path with _ -> ());
  path)

let bridge_json_result body json =
  match json |> member "ok" with
  | `Bool true -> Ok json
  | _ ->
    let msg = match json |> member "error" with `String s -> s | _ -> body in
    Error msg

let model_choice_json (choice : model_choice) =
  `Assoc
    ((match choice.provider with Some provider -> [ ("provider", `String provider) ] | None -> [])
    @ (match choice.model with Some model -> [ ("model", `String model) ] | None -> [])
    @
    match choice.thinking with
    | Some thinking -> [ ("thinking", `String thinking) ]
    | None -> [])

let rec json_string_member json names =
  List.find_map
    (fun name ->
      match json |> member name with
      | `String s when String.trim s <> "" -> Some (String.trim s)
      | `Assoc _ as nested -> (
        match json_string_member nested [ "id"; "name"; "provider"; "model" ] with
        | Some _ as found -> found
        | None -> None)
      | _ -> None)
    names

let model_choice_of_json json =
  match json with
  | `String s when String.trim s <> "" -> Some { provider = None; model = Some (String.trim s); thinking = None }
  | `Assoc _ ->
    let provider = json_string_member json [ "provider"; "providerId"; "providerName" ] in
    let model = json_string_member json [ "id"; "modelId"; "model"; "name" ] in
    let thinking =
      Option.map Model_spec.normalize_thinking (json_string_member json [ "thinkingLevel"; "thinking" ])
    in
    if provider = None && model = None && thinking = None then None else Some { provider; model; thinking }
  | _ -> None

let bridge_request_context request =
  let active =
    match !active_tool_names with
    | None -> []
    | Some names -> [ ("activeTools", `List (List.map (fun name -> `String (Tools.wire_name name)) names)) ]
  in
  let thinking =
    match !active_thinking_level with
    | None -> []
    | Some level -> [ ("thinkingLevel", `String level) ]
  in
  let model =
    match !active_model_choice with
    | None -> []
    | Some choice -> [ ("model", model_choice_json choice) ]
  in
  match request with
  | `Assoc fields -> `Assoc (fields @ [ ("allTools", `List (Tools.tool_infos ())) ] @ active @ model @ thinking)
  | other -> other

let should_share_js_runtime mode =
  List.mem mode [ "command"; "command_completions"; "execute"; "provider"; "render"; "shortcut" ]

let apply_provider_registrations : (Yojson.Safe.t -> unit) ref = ref (fun _ -> ())

let add_js_runtime_paths request =
  let paths = !js_extension_paths in
  match request with
  | `Assoc fields -> (
    let has_paths = List.exists (fun (key, _) -> key = "paths") fields in
    match List.assoc_opt "mode" fields with
    | Some (`String mode) when (not has_paths) && should_share_js_runtime mode && paths <> [] ->
      `Assoc (fields @ [ ("paths", `List (List.map (fun path -> `String path) paths)) ])
    | _ -> request)
  | _ -> request

let apply_runtime_state_from_json json =
  match json |> member "activeToolsChanged", json |> member "activeTools" with
  | `Bool true, `List names ->
    names
    |> List.filter_map (function `String s when String.trim s <> "" -> Some s | _ -> None)
    |> set_active_tools
  | _ -> ();
  (match json |> member "unregisteredProviders" with
   | `List names ->
     names
     |> List.filter_map (function `String s when String.trim s <> "" -> Some s | _ -> None)
     |> List.iter (fun name ->
            let removed_names = Llm.unregister_provider name in
            let removed_names =
              match removed_names with
              | [] -> [ String.lowercase_ascii (String.trim name) ]
              | names -> names
            in
            List.iter Models.unregister_extension_provider removed_names)
   | _ -> ());
  (match json |> member "providersChanged" with
   | `Bool true -> !apply_provider_registrations json
   | _ -> ());
  (match json |> member "modelChanged", json |> member "model" with
   | `Bool true, model_json -> Option.iter set_active_model (model_choice_of_json model_json)
   | _ -> ());
  match json |> member "thinkingLevelChanged", json |> member "thinkingLevel" with
  | `Bool true, `String level -> set_active_thinking level
  | _ -> ()

let run_node_bridge request =
  let request = request |> add_js_runtime_paths |> bridge_request_context in
  let command = Printf.sprintf "node %s" (Filename.quote (Lazy.force bridge_path)) in
  let code, body = Tools.run_process ~stdin_data:(Yojson.Safe.to_string request) command in
  if code <> 0 then Error (Printf.sprintf "node bridge exited %d: %s" code body)
  else
    match Yojson.Safe.from_string body with
    | `Assoc _ as json -> (
      match bridge_json_result body json with
      | Ok json ->
        apply_runtime_state_from_json json;
        Ok json
      | Error _ as error -> error)
    | _ -> Error body
    | exception e -> Error (Printexc.to_string e ^ ": " ^ body)

let ocaml_sdk_command path =
  let direct () = Filename.quote path in
  match Yojson.Safe.from_file path with
  | `Assoc _ as json -> (
    match json |> member "command" with
    | `String command when String.trim command <> "" ->
      let cwd =
        match json |> member "cwd" with
        | `String dir when String.trim dir <> "" ->
          let dir = Config_paths.expand_tilde (String.trim dir) in
          if Filename.is_relative dir then Filename.concat (Filename.dirname path) dir else dir
        | _ -> Filename.dirname path
      in
      Printf.sprintf "cd %s && %s" (Filename.quote cwd) command
    | _ -> direct ())
  | _ -> direct ()
  | exception _ -> direct ()

let run_ocaml_sdk_bridge path request =
  let request = bridge_request_context request in
  let command = ocaml_sdk_command path in
  let code, body = Tools.run_process ~stdin_data:(Yojson.Safe.to_string request) command in
  if code <> 0 then Error (Printf.sprintf "OCaml extension exited %d: %s" code body)
  else
    match Yojson.Safe.from_string body with
    | `Assoc _ as json -> (
      match bridge_json_result body json with
      | Ok json ->
        apply_runtime_state_from_json json;
        Ok json
      | Error _ as error -> error)
    | _ -> Error body
    | exception e -> Error (Printexc.to_string e ^ ": " ^ body)

let run_extension_bridge runtime path request =
  match runtime with
  | Node -> run_node_bridge request
  | Ocaml_sdk -> run_ocaml_sdk_bridge path request

let js_tool_of_json path (j : Yojson.Safe.t) : Tools.tool option =
  match j |> member "name" with
  | `String name when name <> "" ->
    let description =
      match j |> member "description" with
      | `String s -> s
      | _ -> ""
    in
    let parameters =
      match j |> member "parameters" with
      | `Null -> `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ]
      | p -> p
    in
    Some
      { Tools.name;
        description;
        parameters;
        requires_approval = true;
        execute =
          (fun input ->
            let request =
              `Assoc
                [ ("mode", `String "execute");
                  ("path", `String path);
                  ("tool", `String name);
                  ("input", input) ]
            in
            match run_node_bridge request with
            | Ok json -> (
              match json |> member "text" with
              | `String s -> s
              | value -> Yojson.Safe.to_string value)
            | Error msg -> "Error: " ^ msg) }
  | _ -> None

let ocaml_sdk_tool_of_json path (j : Yojson.Safe.t) : Tools.tool option =
  match j |> member "name" with
  | `String name when String.trim name <> "" ->
    let name = String.trim name in
    let description =
      match j |> member "description" with
      | `String s -> s
      | _ -> ""
    in
    let parameters =
      match j |> member "parameters" with
      | `Null -> `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ]
      | p -> p
    in
    Some
      { Tools.name;
        description;
        parameters;
        requires_approval = true;
        execute =
          (fun input ->
            let request =
              `Assoc
                [ ("mode", `String "execute");
                  ("path", `String path);
                  ("tool", `String name);
                  ("input", input) ]
            in
            match run_ocaml_sdk_bridge path request with
            | Ok json -> (
              match json |> member "text" with
              | `String s -> s
              | value -> Yojson.Safe.to_string value)
            | Error msg -> "Error: " ^ msg) }
  | _ -> None

let js_command_of_json path (j : Yojson.Safe.t) : command option =
  match j |> member "name" with
  | `String name when String.trim name <> "" ->
    let name =
      let name = String.trim name in
      if String.length name > 0 && name.[0] = '/' then String.sub name 1 (String.length name - 1) else name
    in
    let description =
      match j |> member "description" with
      | `String s -> s
      | _ -> ""
    in
    let argument_hint =
      match j |> member "argumentHint" with
      | `String s when String.trim s <> "" -> Some (String.trim s)
      | _ -> None
    in
    let has_argument_completions =
      match j |> member "hasArgumentCompletions" with
      | `Bool b -> b
      | _ -> false
    in
    Some { name; description; argument_hint; has_argument_completions; path; runtime = Node }
  | _ -> None

let ocaml_sdk_command_of_json path (j : Yojson.Safe.t) : command option =
  match js_command_of_json path j with
  | Some command -> Some { command with runtime = Ocaml_sdk }
  | None -> None

let normalize_shortcut_spec spec =
  let spec = String.lowercase_ascii (String.trim spec) in
  let spec =
    if String.length spec >= 2 && String.sub spec 0 2 = "c-" then "ctrl+" ^ String.sub spec 2 (String.length spec - 2)
    else if String.length spec >= 8 && String.sub spec 0 8 = "control+" then
      "ctrl+" ^ String.sub spec 8 (String.length spec - 8)
    else spec
  in
  spec |> String.split_on_char ' ' |> String.concat ""

let js_shortcut_of_json path (j : Yojson.Safe.t) : shortcut option =
  match j |> member "spec" with
  | `String spec when String.trim spec <> "" ->
    let spec = normalize_shortcut_spec spec in
    let description =
      match j |> member "description" with
      | `String s -> s
      | _ -> ""
    in
    let command =
      match j |> member "command" with
      | `String s when String.trim s <> "" -> Some s
      | _ -> None
    in
    let has_handler = match j |> member "hasHandler" with `Bool b -> b | _ -> false in
    Some { spec; description; path; command; has_handler }
  | _ -> None

let js_message_renderer_of_json path (j : Yojson.Safe.t) : message_renderer option =
  match j |> member "name" with
  | `String name when String.trim name <> "" ->
    let description =
      match j |> member "description" with
      | `String s -> s
      | _ -> ""
    in
    let target =
      match j |> member "target" with
      | `String s when String.trim s <> "" -> String.trim s
      | _ -> (
        match j |> member "kind" with
        | `String s when String.trim s <> "" -> String.trim s
        | _ -> "all")
    in
    Some { name = String.trim name; description; target; path }
  | _ -> None

let register_command (cmd : command) =
  command_registry := cmd :: List.filter (fun (c : command) -> c.name <> cmd.name) !command_registry

let register_shortcut shortcut =
  shortcut_registry := shortcut :: List.filter (fun s -> s.spec <> shortcut.spec) !shortcut_registry

let register_message_renderer (renderer : message_renderer) =
  message_renderer_registry :=
    renderer :: List.filter (fun (r : message_renderer) -> r.name <> renderer.name) !message_renderer_registry

let register_events ?(runtime = Node) path events =
  let supported =
    [ "session_start";
      "session_before_switch";
      "session_before_fork";
      "session_before_compact";
      "session_before_tree";
      "session_tree";
      "session_shutdown";
      "session_compact";
      "before_agent_start";
      "agent_start";
      "agent_end";
      "turn_start";
      "turn_end";
      "context";
      "message_start";
      "message_update";
      "message_end";
      "tool_execution_start";
      "tool_execution_update";
      "tool_execution_end";
      "input";
      "tool_call";
	      "tool_result";
	      "user_bash";
	      "before_provider_request";
	      "after_provider_response";
	      "model_select";
      "thinking_level_select";
      "resources_discover" ]
  in
  let events = List.filter (fun e -> List.mem e supported) events in
  if events <> [] then
    match runtime with
    | Node -> event_paths := (path, events) :: List.remove_assoc path !event_paths
    | Ocaml_sdk -> ocaml_event_paths := (path, events) :: List.remove_assoc path !ocaml_event_paths

let all_event_targets () =
  (List.map (fun (path, events) -> (Node, path, events)) !event_paths)
  @ List.map (fun (path, events) -> (Ocaml_sdk, path, events)) !ocaml_event_paths

let register_js_commands path json =
  match json |> member "commands" with
  | `List xs ->
    xs
    |> List.filter_map (js_command_of_json path)
    |> List.iter register_command
  | _ -> ()

let register_ocaml_sdk_commands path json =
  match json |> member "commands" with
  | `List xs ->
    xs
    |> List.filter_map (ocaml_sdk_command_of_json path)
    |> List.iter register_command
  | _ -> ()

let register_js_shortcuts path json =
  match json |> member "shortcuts" with
  | `List xs ->
    xs
    |> List.filter_map (js_shortcut_of_json path)
    |> List.iter register_shortcut
  | _ -> ()

let register_js_message_renderers path json =
  match json |> member "renderers" with
  | `List xs ->
    xs
    |> List.filter_map (js_message_renderer_of_json path)
    |> List.iter register_message_renderer
  | _ -> ()

let strings_from_json = function
  | `List xs -> List.filter_map (function `String s when String.trim s <> "" -> Some s | _ -> None) xs
  | `String s when String.trim s <> "" -> [ s ]
  | _ -> []

let first_string json names =
  List.find_map
    (fun name ->
      match json |> member name with
      | `String s when String.trim s <> "" -> Some s
      | _ -> None)
    names

let headers_from_json = function
  | `List xs -> List.filter_map (function `String s when String.trim s <> "" -> Some s | _ -> None) xs
  | `Assoc fields ->
    fields
    |> List.filter_map (function
           | key, `String value when String.trim key <> "" && String.trim value <> "" -> Some (key ^ ": " ^ value)
           | _ -> None)
  | _ -> []

let int_from_json = function
  | `Int n -> Some n
  | `Intlit s -> int_of_string_opt s
  | `Float f -> Some (int_of_float f)
  | _ -> None

let usage_from_json json =
  let pick names =
    List.find_map
      (fun name ->
        match json |> member name with
        | value -> int_from_json value)
      names
    |> Option.value ~default:0
  in
  { Llm.input_tokens = pick [ "inputTokens"; "input_tokens"; "prompt_tokens"; "promptTokens" ];
    output_tokens = pick [ "outputTokens"; "output_tokens"; "completion_tokens"; "completionTokens" ] }

let content_blocks_from_json json =
  match json |> member "content" with
  | `List xs -> List.map Llm.content_of_json xs
  | `String s -> [ Llm.Text s ]
  | _ -> (
    match json |> member "text" with
    | `String s -> [ Llm.Text s ]
    | _ -> [])

let register_provider_models provider default_model models_json =
  let register ?context_window id =
    let context_window = Option.value context_window ~default:128000 in
    if String.trim id <> "" then Models.register_extension_model { Models.provider; id; context_window }
  in
  register default_model;
  match models_json with
  | `List models ->
    List.iter
      (function
        | `String id -> register id
        | `Assoc _ as model -> (
          match first_string model [ "id"; "name"; "model" ] with
          | None -> ()
          | Some id ->
            let context_window =
              List.find_map
                (fun name -> int_from_json (model |> member name))
                [ "contextWindow"; "context_window"; "maxContext"; "maxTokens" ]
            in
            register ?context_window id)
        | _ -> ())
      models
  | _ -> ()

let register_js_provider_runtime path provider_name runtime =
  Llm.register_provider_runtime runtime
    (fun cfg ~system ~on_text ~tools_enabled ?tool_names turns ->
      let tool_schemas =
        if tools_enabled then `List (Tools.openai_schemas ?allowed:tool_names ()) else `List []
      in
      let request =
        `Assoc
          ([ ("mode", `String "provider");
             ("path", `String path);
             ("provider", `String provider_name);
             ("model", `String cfg.Llm.model);
             ("system", `String system);
             ("messages", `List (List.map Llm.turn_to_json turns));
             ("tools", tool_schemas);
             ("toolsEnabled", `Bool tools_enabled);
             ("maxTokens", `Int cfg.Llm.max_tokens);
             ("thinking", `String cfg.Llm.thinking) ]
          @
          match tool_names with
          | Some names -> [ ("toolNames", `List (List.map (fun name -> `String name) names)) ]
          | None -> [])
      in
      match run_node_bridge request with
      | Error msg -> raise (Llm.Api_error msg)
      | Ok json ->
        let blocks = content_blocks_from_json json in
        List.iter (function Llm.Text text -> on_text text | _ -> ()) blocks;
        let usage = usage_from_json (json |> member "usage") in
        (blocks, usage))

let register_js_providers path json =
  match json |> member "providers" with
  | `List providers ->
    providers
    |> List.iter (fun provider ->
           match first_string provider [ "name"; "id"; "provider" ] with
           | None -> ()
           | Some name ->
             let provider_path =
               Option.value (first_string provider [ "extensionPath"; "path" ]) ~default:path
             in
             let aliases = strings_from_json (provider |> member "aliases") @ strings_from_json (provider |> member "names") in
             let protocol =
               match first_string provider [ "protocol"; "wireProtocol"; "api"; "type" ] with
               | Some s when List.mem (String.lowercase_ascii (String.trim s)) [ "anthropic"; "claude" ] ->
                 Llm.Anthropic
               | _ -> Llm.Openai
             in
             let has_runtime =
               match provider |> member "hasRuntime" with
               | `Bool b -> b
               | `String s -> truthy s
               | _ -> false
             in
             let base_url =
               Option.value
                 (first_string provider [ "baseUrl"; "baseURL"; "base_url"; "url" ])
                 ~default:(if has_runtime then "extension://" ^ String.lowercase_ascii (String.trim name) else "https://api.openai.com/v1")
             in
             let env_keys =
               let keys =
                 strings_from_json (provider |> member "envKeys")
                 @ strings_from_json (provider |> member "env_keys")
                 @ strings_from_json (provider |> member "apiKeyEnvVars")
                 @ strings_from_json (provider |> member "apiKeyEnvVar")
                 @ strings_from_json (provider |> member "apiKeyEnv")
                 @ strings_from_json (provider |> member "envKey")
               in
               if keys = [] && not has_runtime then
                 [ String.uppercase_ascii
                     (name |> String.map (fun c -> if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then c else '_'))
                   ^ "_API_KEY" ]
               else keys
             in
             let default_model =
               match first_string provider [ "defaultModel"; "default_model"; "model" ] with
               | Some model -> model
               | None -> name
             in
             let headers = headers_from_json (provider |> member "headers") in
             let runtime =
               if has_runtime then Some (provider_path ^ "#" ^ String.lowercase_ascii (String.trim name)) else None
             in
             Llm.register_provider ?runtime ~name ~aliases ~headers ~protocol ~base_url ~env_keys ~default_model ();
             Option.iter (register_js_provider_runtime provider_path name) runtime;
             register_provider_models (String.lowercase_ascii (String.trim name)) default_model (provider |> member "models"))
  | _ -> ()

let () = apply_provider_registrations := register_js_providers "<runtime>"

let register_js_tools path json =
  match json |> member "tools" with
  | `List entries ->
    List.filter_map
      (fun j ->
        match js_tool_of_json path j with
        | Some t when Tools.register t -> Some t.Tools.name
        | Some _ -> None
        | None -> None)
      entries
  | _ -> []

let register_ocaml_sdk_tools path json =
  match json |> member "tools" with
  | `List entries ->
    List.filter_map
      (fun j ->
        match ocaml_sdk_tool_of_json path j with
        | Some t when Tools.register t -> Some t.Tools.name
        | Some _ -> None
        | None -> None)
      entries
  | _ -> []

let optional_string name = function
  | Some value when String.trim value <> "" -> [ (name, `String value) ]
  | _ -> []

let session_payload ?current_session_file ?current_session_id ?current_session_name ?target_session_file
    ?source_session_file ?previous_session_file ?session_file ?session_id ?session_name ?entry_id ?position
    event_type reason =
  `Assoc
    ([ ("type", `String event_type); ("reason", `String reason); ("cwd", `String (Sys.getcwd ())) ]
    @ optional_string "currentSessionFile" current_session_file
    @ optional_string "currentSessionId" current_session_id
    @ optional_string "currentSessionName" current_session_name
    @ optional_string "targetSessionFile" target_session_file
    @ optional_string "sourceSessionFile" source_session_file
    @ optional_string "previousSessionFile" previous_session_file
    @ optional_string "sessionFile" session_file
    @ optional_string "sessionId" session_id
    @ optional_string "sessionName" session_name
    @ optional_string "entryId" entry_id
    @ optional_string "position" position)

let emit_session_start_for_path ?previous_session_file ?session_file ?session_id ?session_name ?(reason = "startup")
    path =
  let payload =
    session_payload ?previous_session_file ?session_file ?session_id ?session_name "session_start" reason
  in
  match
    run_node_bridge
      (`Assoc [ ("mode", `String "event"); ("path", `String path); ("event", `String "session_start"); ("payload", payload) ])
  with
  | Error _ -> []
  | Ok json ->
    register_js_commands path json;
    register_js_shortcuts path json;
    register_js_message_renderers path json;
    register_js_providers path json;
    register_js_tools path json

let emit_session_start ?previous_session_file ?session_file ?session_id ?session_name ~reason () =
  let registered = ref [] in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if List.mem "session_start" events then
           registered :=
             !registered
             @ emit_session_start_for_path ?previous_session_file ?session_file ?session_id ?session_name ~reason path);
  !registered

let json_string_list field json =
  match json |> member field with
  | `List xs -> List.filter_map (function `String s when String.trim s <> "" -> Some s | _ -> None) xs
  | `String s when String.trim s <> "" -> [ s ]
  | _ -> []

let resource_result json =
  match json |> member "result" with
  | `Assoc _ as result -> result
  | _ -> json

let emit_resources_discover ~reason () =
  discovered_skill_paths := [];
  discovered_prompt_paths := [];
  discovered_theme_paths := [];
  let payload =
    `Assoc
      [ ("type", `String "resources_discover");
        ("cwd", `String (Sys.getcwd ()));
        ("reason", `String reason) ]
  in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if List.mem "resources_discover" events then
           match
             run_node_bridge
               (`Assoc
                 [ ("mode", `String "event");
                   ("path", `String path);
                   ("event", `String "resources_discover");
                   ("payload", payload) ])
           with
           | Error _ -> ()
           | Ok json ->
             let result = resource_result json in
             discovered_skill_paths := !discovered_skill_paths @ json_string_list "skillPaths" result;
             discovered_prompt_paths := !discovered_prompt_paths @ json_string_list "promptPaths" result;
             discovered_theme_paths := !discovered_theme_paths @ json_string_list "themePaths" result);
  discovered_skill_paths := Config_paths.uniq !discovered_skill_paths;
  discovered_prompt_paths := Config_paths.uniq !discovered_prompt_paths;
  discovered_theme_paths := Config_paths.uniq !discovered_theme_paths

let load_js_extension ?(reason = "startup") path =
  if Sys.command "command -v node >/dev/null 2>&1" <> 0 then begin
    Printf.eprintf "[warning] extension %s requires node, but node was not found\n%!" path;
    []
  end
  else
    let request = `Assoc [ ("mode", `String "describe"); ("path", `String path) ] in
    match run_node_bridge request with
    | Error msg ->
      Printf.eprintf "[warning] failed to load extension %s: %s\n%!" path msg;
      []
    | Ok json ->
      js_extension_paths := Config_paths.uniq (!js_extension_paths @ [ path ]);
      register_js_commands path json;
      register_js_shortcuts path json;
      register_js_message_renderers path json;
      register_js_providers path json;
      let events =
        match json |> member "events" with
        | `List xs -> List.filter_map (function `String s -> Some s | _ -> None) xs
        | _ -> []
      in
      register_events path events;
      let names = register_js_tools path json in
      if List.mem "session_start" events then names @ emit_session_start_for_path ~reason path else names

let load_ocaml_sdk_extension ?(reason = "startup") path =
  let request = `Assoc [ ("mode", `String "describe"); ("path", `String path); ("reason", `String reason) ] in
  match run_ocaml_sdk_bridge path request with
  | Error msg ->
    Printf.eprintf "[warning] failed to load OCaml extension %s: %s\n%!" path msg;
    []
  | Ok json ->
    register_ocaml_sdk_commands path json;
    register_ocaml_sdk_tools path json

let load_path ?(reason = "startup") path =
  if is_json_file path then load_manifest path
  else if is_js_extension_file path then load_js_extension ~reason path
  else if is_ocaml_sdk_extension_file path then load_ocaml_sdk_extension ~reason path
  else []

(* Load and register manifest tools; returns the names registered.
   Prints a warning to stderr if the manifest is malformed. *)
let load ?(reason = "startup") () : string list =
  Tools.reset_extensions ();
  clear_active_tools ();
  command_registry := [];
  shortcut_registry := [];
  message_renderer_registry := [];
  event_paths := [];
  js_extension_paths := [];
  discovered_skill_paths := [];
  discovered_prompt_paths := [];
  discovered_theme_paths := [];
  Llm.clear_extension_providers ();
  Models.clear_extension_models ();
  let names =
    manifest_paths ()
    |> List.concat_map expand_manifest_path
    |> List.concat_map (load_path ~reason)
  in
  emit_resources_discover ~reason ();
  names

let command_menu () =
  !command_registry
  |> List.map (fun (c : command) ->
         let detail =
           match c.argument_hint with
           | Some hint -> if c.description = "" then hint else hint ^ " - " ^ c.description
           | None -> c.description
         in
         ("/" ^ c.name, detail))
  |> List.sort compare

let command_argument_completions name prefix =
  let name =
    let name = String.trim name in
    if String.length name > 0 && name.[0] = '/' then String.sub name 1 (String.length name - 1) else name
  in
  match
    List.find_opt
      (fun (command : command) -> command.name = name && command.has_argument_completions)
      !command_registry
  with
  | None -> []
  | Some command -> (
    let request =
      `Assoc
        [ ("mode", `String "command_completions");
          ("path", `String command.path);
          ("command", `String name);
          ("prefix", `String prefix) ]
    in
    match run_extension_bridge command.runtime command.path request with
    | Error msg ->
      Printf.eprintf "[warning] extension command completion /%s failed: %s\n%!" name msg;
      []
    | Ok json -> (
      match json |> member "items" with
      | `List items ->
        items
        |> List.filter_map (fun item ->
               match item |> member "value" with
               | `String value when String.trim value <> "" -> Some value
               | _ -> (
                 match item |> member "label" with
                 | `String label when String.trim label <> "" -> Some label
                 | _ -> None))
      | _ -> []))

let shortcut_menu () =
  !shortcut_registry
  |> List.map (fun s -> (s.spec, s.description))
  |> List.sort compare

let has_message_renderers () = !message_renderer_registry <> []

let ui_capture_of_json json =
  let strings field =
    match json |> member field with
    | `List xs -> List.filter_map (function `String s -> Some s | _ -> None) xs
    | _ -> []
  in
  let requests =
    match json |> member "requests" with
    | `List xs -> xs
    | _ -> []
  in
  let surfaces =
    match json |> member "surfaces" with
    | `List xs -> xs
    | _ -> []
  in
  let messages =
    match json |> member "messages" with
    | `List xs -> xs
    | _ -> []
  in
  { notifications = strings "notifications"; requests; surfaces; messages }

let response_ui json =
  match json |> member "ui" with
  | `Assoc _ as ui -> ui_capture_of_json ui
  | _ -> { notifications = []; requests = []; surfaces = []; messages = [] }

let text_response json =
  let text =
    match json |> member "text" with
    | `String s -> s
    | value -> Yojson.Safe.to_string value
  in
  let thinking_level =
    match json |> member "thinkingLevelChanged", json |> member "thinkingLevel" with
    | `Bool true, `String level -> Some (Model_spec.normalize_thinking level)
    | _ -> None
  in
  let model_choice =
    match json |> member "modelChanged", json |> member "model" with
    | `Bool true, model_json -> model_choice_of_json model_json
    | _ -> None
  in
  let session_name =
    match json |> member "sessionNameChanged", json |> member "sessionName" with
    | `Bool true, `String name -> Some name
    | `Bool true, `Null -> Some ""
    | _ -> None
  in
  let session_entries =
    match json |> member "sessionEntries" with
    | `List entries -> entries
    | _ -> []
  in
  let theme_name =
    match json |> member "themeChanged", json |> member "themeName" with
    | `Bool true, `String name when String.trim name <> "" -> Some (String.trim name)
    | _ -> None
  in
  let tools_expanded =
    match json |> member "toolsExpandedChanged", json |> member "toolsExpanded" with
    | `Bool true, `Bool expanded -> Some expanded
    | _ -> None
  in
  let compact_requests =
    match json |> member "compactRequests" with
    | `List requests -> requests
    | _ -> []
  in
  let session_actions =
    match json |> member "sessionActions" with
    | `List actions -> actions
    | _ -> []
  in
  { text;
    ui = response_ui json;
    thinking_level;
    model_choice;
    session_name;
    session_entries;
    theme_name;
    tools_expanded;
    abort_requested = (json |> member "abortRequested" = `Bool true);
    shutdown_requested = (json |> member "shutdownRequested" = `Bool true);
    compact_requests;
    reload_requested = (json |> member "reloadRequested" = `Bool true);
    session_actions }

let empty_ui_capture = { notifications = []; requests = []; surfaces = []; messages = [] }

let error_command_response msg =
  { text = msg;
    ui = empty_ui_capture;
    thinking_level = None;
    model_choice = None;
    session_name = None;
    session_entries = [];
    theme_name = None;
    tools_expanded = None;
    abort_requested = false;
    shutdown_requested = false;
    compact_requests = [];
    reload_requested = false;
    session_actions = [] }

let components_of_json json =
  match json |> member "components" with
  | `List xs -> xs
  | _ -> []

let session_context_json ?(entries = []) ?info turns =
  let _turn_ids, turn_entries =
    turns
    |> List.mapi (fun i turn ->
           let id = "turn-" ^ string_of_int i in
           let parent_id = if i = 0 then `Null else `String ("turn-" ^ string_of_int (i - 1)) in
           ( id,
             `Assoc
               [ ("type", `String "message");
                 ("id", `String id);
                 ("parentId", parent_id);
                 ("timestamp", `String "");
                 ("message", Llm.turn_to_json turn) ] ))
    |> List.split
  in
  let all_entries = turn_entries @ entries in
  let entry_id json =
    match json |> member "id" with
    | `String id when String.trim id <> "" -> Some id
    | _ -> None
  in
  let leaf_id_after_entry current json =
    match json |> member "type" with
    | `String "leaf" -> (
      match json |> member "targetId" with
      | `String id when String.trim id <> "" -> Some id
      | `Null -> None
      | _ -> current)
    | `String ("message" | "custom_message" | "branch_summary" | "compaction" | "thinking_level_change" | "model_change") -> (
      match entry_id json with
      | Some _ as id -> id
      | None -> current)
    | _ -> current
  in
  let leaf_id =
    List.fold_left leaf_id_after_entry None all_entries
  in
  let info_fields =
    match info with
    | None -> [ ("cwd", `String (Sys.getcwd ())) ]
    | Some (info : Session.info) ->
      [ ("id", `String info.id);
        ("path", `String info.path);
        ("sessionDir", `String (Filename.dirname info.path));
        ("name", `String info.name);
        ("created", `Float info.created);
        ("cwd", `String (if info.cwd = "" then Sys.getcwd () else info.cwd)) ]
  in
  `Assoc
    (info_fields
    @ [ ("entries", `List all_entries);
        ( "leafId",
          match leaf_id with
          | Some id -> `String id
          | None -> `Null ) ])

let model_catalog_json () =
  Models.list ()
  |> List.map (fun (entry : Models.entry) ->
         `Assoc
           [ ("id", `String entry.id);
             ("name", `String entry.id);
             ("provider", `String entry.provider);
             ("api", `String entry.provider);
             ("contextWindow", `Int entry.context_window);
             ("maxTokens", `Int 4096) ])

let string_member names json =
  List.find_map
    (fun name ->
      match json |> member name with
      | `String s when String.trim s <> "" -> Some s
      | _ -> None)
    names

let rec component_plain_text json =
  match json with
  | `String s -> s
  | `List xs -> xs |> List.map component_plain_text |> List.filter (fun s -> String.trim s <> "") |> String.concat "\n"
  | `Assoc _ as obj -> (
    match string_member [ "type"; "kind"; "component" ] obj with
    | Some "text" -> Option.value (string_member [ "text"; "value"; "label" ] obj) ~default:""
    | Some "markdown" -> Option.value (string_member [ "markdown"; "text"; "value" ] obj) ~default:""
    | Some "code" ->
      let lang = Option.value (string_member [ "language"; "lang" ] obj) ~default:"" in
      let code = Option.value (string_member [ "code"; "text"; "value" ] obj) ~default:"" in
      "```" ^ lang ^ "\n" ^ code ^ "\n```"
    | Some "link" ->
      let label = Option.value (string_member [ "label"; "text"; "title"; "href"; "url" ] obj) ~default:"" in
      (match string_member [ "href"; "url" ] obj with
       | Some url when url <> label -> label ^ " <" ^ url ^ ">"
       | _ -> label)
    | Some "button" -> "[" ^ Option.value (string_member [ "label"; "text"; "title" ] obj) ~default:"button" ^ "]"
    | Some ("list" | "ul" | "ol") -> component_list_text obj
    | Some "table" -> component_table_text obj
    | Some ("panel" | "card" | "section") -> component_panel_text obj
    | _ -> (
      match component_children obj with
      | [] -> Option.value (string_member [ "text"; "markdown"; "body"; "label"; "title" ] obj) ~default:(Yojson.Safe.to_string obj)
      | children -> component_plain_text (`List children)))
  | json -> Yojson.Safe.to_string json

and component_children obj =
  match obj |> member "children" with
  | `List xs -> xs
  | _ -> (
    match obj |> member "content" with
    | `List xs -> xs
    | `String s -> [ `String s ]
    | _ -> (
      match obj |> member "body" with
      | `List xs -> xs
      | `String s -> [ `String s ]
      | _ -> []))

and component_list_text obj =
  let items =
    match obj |> member "items" with
    | `List xs -> xs
    | _ -> component_children obj
  in
  items
  |> List.mapi (fun i item ->
         let marker =
           match string_member [ "ordered"; "type"; "kind" ] obj with
           | Some "true" | Some "ol" -> string_of_int (i + 1) ^ ". "
           | _ -> "- "
         in
         marker ^ component_plain_text item)
  |> String.concat "\n"

and component_table_text obj =
  let cell_text cell =
    match cell with
    | `String s -> s
    | `Int n -> string_of_int n
    | `Float f -> string_of_float f
    | _ -> component_plain_text cell
  in
  let columns =
    match obj |> member "columns" with
    | `List xs -> List.map cell_text xs
    | _ -> []
  in
  let row_cells row =
    match row with
    | `List xs -> List.map cell_text xs
    | `Assoc fields when columns <> [] ->
      List.map
        (fun col ->
          match List.assoc_opt col fields with
          | Some value -> cell_text value
          | None -> "")
        columns
    | _ -> [ cell_text row ]
  in
  let rows =
    match obj |> member "rows" with
    | `List xs -> List.map row_cells xs
    | _ -> []
  in
  let all_rows = if columns = [] then rows else columns :: rows in
  let widths =
    List.fold_left
      (fun widths row ->
        List.mapi (fun i cell -> max (String.length cell) (try List.nth widths i with _ -> 0)) row)
      [] all_rows
  in
  let pad width s = s ^ String.make (max 0 (width - String.length s)) ' ' in
  let render_row row =
    "| "
    ^ (widths
       |> List.mapi (fun i width -> pad width (try List.nth row i with _ -> ""))
       |> String.concat " | ")
    ^ " |"
  in
  let sep = "|-" ^ (widths |> List.map (fun width -> String.make width '-') |> String.concat "-|-") ^ "-|" in
  match all_rows with
  | [] -> ""
  | header :: rest when columns <> [] -> String.concat "\n" (render_row header :: sep :: List.map render_row rest)
  | rows -> rows |> List.map render_row |> String.concat "\n"

and component_panel_text obj =
  let title = string_member [ "title"; "label"; "header" ] obj in
  let body = `List (component_children obj) |> component_plain_text |> String.split_on_char '\n' in
  let body = List.filter (fun line -> String.trim line <> "") body in
  let title_text = Option.value title ~default:"" in
  let width =
    List.fold_left max (String.length title_text)
      (List.map String.length body)
    |> max 4
  in
  let border = "+" ^ String.make (width + 2) '-' ^ "+" in
  let line s = "| " ^ s ^ String.make (max 0 (width - String.length s)) ' ' ^ " |" in
  let title_lines = if title_text = "" then [] else [ line title_text; border ] in
  String.concat "\n" (border :: title_lines @ List.map line body @ [ border ])

let components_text components =
  components |> List.map component_plain_text |> List.filter (fun s -> String.trim s <> "") |> String.concat "\n"

let render_response_of_json json =
  let components = components_of_json json in
  let rendered =
    match components_text components with
    | s when String.trim s <> "" -> s
    | _ -> (
      match json |> member "text" with
      | `String s -> s
      | value -> Yojson.Safe.to_string value)
  in
  { rendered; components; render_ui = response_ui json }

let merge_ui left right =
  { notifications = left.notifications @ right.notifications;
    requests = left.requests @ right.requests;
    surfaces = left.surfaces @ right.surfaces;
    messages = left.messages @ right.messages }

let add_runtime_context ?session_name ?session_context ?themes ?theme_name ?model ?models ?commands ?context_usage ?system_prompt
    ?(has_ui = false) ?(is_idle = true) ?(has_pending_messages = false) ?(tools_expanded = false) fields =
  fields
  @ (match session_name with Some name -> [ ("sessionName", `String name) ] | None -> [])
  @ (match session_context with Some session -> [ ("session", session) ] | None -> [])
  @ (match themes with Some values -> [ ("themes", `List values) ] | None -> [])
  @ (match theme_name with Some name -> [ ("themeName", `String name) ] | None -> [])
  @ (match model with Some value -> [ ("model", value) ] | None -> [])
  @ (match models with Some values -> [ ("models", `List values) ] | None -> [])
  @ (match commands with Some values -> [ ("commands", `List values) ] | None -> [])
  @ (match context_usage with Some value -> [ ("contextUsage", value) ] | None -> [])
  @ (match system_prompt with Some value -> [ ("systemPrompt", `String value) ] | None -> [])
  @
  [ ("hasUI", `Bool has_ui);
    ("isIdle", `Bool is_idle);
    ("hasPendingMessages", `Bool has_pending_messages);
    ("toolsExpanded", `Bool tools_expanded) ]

let execute_command_response ?session_name ?session_context ?themes ?theme_name ?model ?models ?commands ?context_usage ?system_prompt ?has_ui
    ?is_idle ?has_pending_messages ?tools_expanded line =
  let line = String.trim line in
  if line = "" || line.[0] <> '/' then None
  else
    let command_part, args =
      match String.index_opt line ' ' with
      | None -> (String.sub line 1 (String.length line - 1), "")
      | Some i ->
        ( String.sub line 1 (i - 1),
          String.sub line (i + 1) (String.length line - i - 1) |> String.trim )
    in
    match List.find_opt (fun (c : command) -> c.name = command_part) !command_registry with
    | None -> None
    | Some c ->
      let request =
        `Assoc
          (add_runtime_context ?session_name ?session_context ?themes ?theme_name ?model ?models ?commands ?context_usage ?system_prompt ?has_ui
             ?is_idle ?has_pending_messages ?tools_expanded
             [ ("mode", `String "command");
               ("path", `String c.path);
               ("command", `String c.name);
               ("args", `String args) ])
      in
      Some
        (match run_extension_bridge c.runtime c.path request with
         | Ok json -> text_response json
         | Error msg -> error_command_response ("Error: " ^ msg))

let execute_command line = Option.map (fun (response : command_response) -> response.text) (execute_command_response line)

let execute_shortcut_response ?session_name ?session_context ?themes ?theme_name ?model ?models ?commands ?context_usage ?system_prompt ?has_ui
    ?is_idle ?has_pending_messages ?tools_expanded spec =
  let spec = normalize_shortcut_spec spec in
  match List.find_opt (fun s -> s.spec = spec) !shortcut_registry with
  | None -> None
  | Some shortcut ->
    let request =
      `Assoc
        (add_runtime_context ?session_name ?session_context ?themes ?theme_name ?model ?models ?commands ?context_usage ?system_prompt ?has_ui
           ?is_idle ?has_pending_messages ?tools_expanded
           [ ("mode", `String "shortcut");
             ("path", `String shortcut.path);
             ("shortcut", `String shortcut.spec) ])
    in
    Some
      (match run_node_bridge request with
       | Ok json -> (
         match json |> member "command", json |> member "text" with
         | `String command, _ -> Shortcut_response_command command
         | _, `String _ -> Shortcut_response_output (text_response json)
         | _ ->
           Shortcut_response_output
             { text = Yojson.Safe.to_string json;
               ui = response_ui json;
               thinking_level = None;
               model_choice = None;
               session_name = None;
               session_entries = [];
               theme_name = None;
               tools_expanded = None;
               abort_requested = false;
               shutdown_requested = false;
               compact_requests = [];
               reload_requested = false;
               session_actions = [] })
       | Error msg -> Shortcut_response_output (error_command_response ("Error: " ^ msg)))

let execute_shortcut spec =
  match execute_shortcut_response spec with
  | None -> None
  | Some (Shortcut_response_command command) -> Some (Shortcut_command command)
  | Some (Shortcut_response_output response) -> Some (Shortcut_output response.text)

let render_response ?(role = "") ?(tool_name = "") ~kind text =
  if !message_renderer_registry = [] || String.trim text = "" then
    { rendered = text; components = []; render_ui = { notifications = []; requests = []; surfaces = []; messages = [] } }
  else
    let current = ref text in
    let collected_components = ref [] in
    let collected_ui = ref { notifications = []; requests = []; surfaces = []; messages = [] } in
    !message_renderer_registry
    |> List.rev
    |> List.iter (fun renderer ->
           if renderer.target = "all" || renderer.target = kind then
             let request =
               `Assoc
                 [ ("mode", `String "render");
                   ("path", `String renderer.path);
                   ("kind", `String kind);
                   ("role", `String role);
                   ("toolName", `String tool_name);
                   ("text", `String !current) ]
             in
             match run_node_bridge request with
             | Ok json -> (
               let response = render_response_of_json json in
               collected_components := !collected_components @ response.components;
               collected_ui := merge_ui !collected_ui response.render_ui;
               if String.trim response.rendered <> "" then current := response.rendered)
             | Error _ -> ());
    let rendered =
      match components_text !collected_components with
      | s when String.trim s <> "" -> s
      | _ -> !current
    in
    { rendered; components = !collected_components; render_ui = !collected_ui }

let render_text ?(role = "") ?(tool_name = "") ~kind text =
  let response = render_response ~role ~tool_name ~kind text in
  response.rendered

let content_text json =
  match json with
  | `String s -> Some s
  | `List xs ->
    let texts =
      xs
      |> List.filter_map (function
             | `String s -> Some s
             | `Assoc _ as obj -> (
               match obj |> member "type", obj |> member "text" with
               | `String "text", `String s -> Some s
               | _ -> None)
             | _ -> None)
    in
    if texts = [] then None else Some (String.concat "\n" texts)
  | _ -> None

let emit_event path event payload =
  run_node_bridge (`Assoc [ ("mode", `String "event"); ("path", `String path); ("event", `String event); ("payload", payload) ])

let emit_before_provider_request payload =
  let current = ref payload in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if List.mem "before_provider_request" events then
           let event_payload =
             `Assoc [ ("type", `String "before_provider_request"); ("payload", !current) ]
           in
           match emit_event path "before_provider_request" event_payload with
           | Ok json -> (
             match json |> member "result" with
             | `Null -> ()
             | replacement -> current := replacement)
           | Error _ -> ());
  !current

let headers_json headers =
  `Assoc (headers |> List.map (fun (name, value) -> (name, `String value)))

let turn_of_agent_message_json json =
  match json with
  | `Assoc fields -> (
    match List.assoc_opt "role" fields, List.assoc_opt "content" fields with
    | Some _, Some (`List _) -> Llm.turn_of_json json
    | Some _, Some (`String text) ->
      let role =
        match json |> member "role" with
        | `String "assistant" -> Llm.Assistant
        | _ -> Llm.User
      in
      { Llm.role; content = [ Llm.Text text ] }
    | _, Some (`String text) -> { Llm.role = Llm.User; content = [ Llm.Text text ] }
    | _ -> Llm.turn_of_json json)
  | _ -> Llm.turn_of_json json

let turns_from_json = function
  | `List messages -> List.map turn_of_agent_message_json messages
  | `Assoc _ as message -> [ turn_of_agent_message_json message ]
  | _ -> []

let emit_notification event payload =
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if List.mem event events then ignore (emit_event path event payload))

let emit_after_provider_response ~status ~headers =
  emit_notification "after_provider_response"
    (`Assoc
      [ ("type", `String "after_provider_response");
        ("status", `Int status);
        ("headers", headers_json headers) ])

type session_before_result =
  | Session_continue
  | Session_cancel of string

type session_before_tree_result =
  { tree_cancel : string option;
    tree_label : string option;
    tree_summary : Yojson.Safe.t option;
    tree_custom_instructions : string option;
    tree_replace_instructions : bool option }

let is_true_member names json =
  List.exists
    (fun name ->
      match json |> member name with
      | `Bool true -> true
      | `String s -> truthy s
      | _ -> false)
    names

let bool_member name json =
  match json |> member name with
  | `Bool value -> Some value
  | `String s -> (
    match String.lowercase_ascii (String.trim s) with
    | "1" | "true" | "yes" | "y" | "on" -> Some true
    | "0" | "false" | "no" | "n" | "off" -> Some false
    | _ -> None)
  | _ -> None

let reason_member json =
  List.find_map
    (fun name ->
      match json |> member name with
      | `String s when String.trim s <> "" -> Some s
      | _ -> None)
    [ "reason"; "message"; "error" ]

let cancellation_from_json json =
  match json with
  | `Assoc _ as obj when is_true_member [ "cancel"; "cancelled"; "abort"; "aborted" ] obj ->
    Some (Option.value (reason_member obj) ~default:"Cancelled by extension")
  | _ -> None

let summary_from_json json =
  match json with
  | `Assoc _ -> (
    match json |> member "summary" with
    | `Assoc _ as summary -> Some summary
    | `String summary when String.trim summary <> "" -> Some (`Assoc [ ("summary", `String summary) ])
    | _ -> None)
  | _ -> None

let session_before event payload =
  let decision = ref Session_continue in
  let cancelled () = match !decision with Session_cancel _ -> true | Session_continue -> false in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if (not (cancelled ())) && List.mem event events then
           match emit_event path event payload with
           | Error _ -> ()
           | Ok json ->
             let result_cancel = cancellation_from_json (json |> member "result") in
             let event_cancel = cancellation_from_json (json |> member "event") in
             (match Option.value result_cancel ~default:(Option.value event_cancel ~default:"") with
              | "" -> ()
              | reason -> decision := Session_cancel reason));
  !decision

let emit_session_before_switch ?current_session_file ?current_session_id ?current_session_name ?target_session_file
    ~reason () =
  session_before "session_before_switch"
    (session_payload ?current_session_file ?current_session_id ?current_session_name ?target_session_file
       "session_before_switch" reason)

let emit_session_before_fork ?current_session_file ?current_session_id ?current_session_name ?source_session_file
    ?entry_id ?position ~reason () =
  session_before "session_before_fork"
    (session_payload ?current_session_file ?current_session_id ?current_session_name ?source_session_file ?entry_id
       ?position "session_before_fork" reason)

let emit_session_before_compact ?session_file ?session_id ?session_name ~turn_count () =
  session_before "session_before_compact"
    (`Assoc
      ([ ("type", `String "session_before_compact");
         ("reason", `String "compact");
         ("cwd", `String (Sys.getcwd ()));
         ("turnCount", `Int turn_count) ]
      @ optional_string "sessionFile" session_file
      @ optional_string "sessionId" session_id
      @ optional_string "sessionName" session_name))

let emit_session_before_tree ~target_id ?old_leaf_id ?common_ancestor_id ?label ?custom_instructions
    ?replace_instructions ~user_wants_summary ~entries_to_summarize () =
  let preparation =
    `Assoc
      ([ ("targetId", `String target_id);
         ("oldLeafId", (match old_leaf_id with Some id -> `String id | None -> `Null));
         ("commonAncestorId", (match common_ancestor_id with Some id -> `String id | None -> `Null));
         ("entriesToSummarize", `List entries_to_summarize);
         ("userWantsSummary", `Bool user_wants_summary) ]
      @ optional_string "label" label
      @ optional_string "customInstructions" custom_instructions
      @
      match replace_instructions with
      | Some replace -> [ ("replaceInstructions", `Bool replace) ]
      | None -> [])
  in
  let payload =
    `Assoc
      [ ("type", `String "session_before_tree");
        ("preparation", preparation);
        ("signal", `Assoc [ ("aborted", `Bool false) ]) ]
  in
  let result =
    ref
      { tree_cancel = None;
        tree_label = None;
        tree_summary = None;
        tree_custom_instructions = None;
        tree_replace_instructions = None }
  in
  let cancelled () = Option.is_some (!result).tree_cancel in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if (not (cancelled ())) && List.mem "session_before_tree" events then
           match emit_event path "session_before_tree" payload with
           | Error _ -> ()
           | Ok json ->
             let event =
               match json |> member "event" with
               | `Assoc _ as event -> event
               | _ -> `Assoc []
             in
             let handler_result =
               match json |> member "result" with
               | `Assoc _ as result -> result
               | _ -> `Assoc []
             in
             let result_cancel = cancellation_from_json handler_result in
             let event_cancel = cancellation_from_json event in
             (match Option.value result_cancel ~default:(Option.value event_cancel ~default:"") with
              | reason when String.trim reason <> "" ->
                result := { !result with tree_cancel = Some reason }
              | _ ->
                let event_preparation = event |> member "preparation" in
                let next_label =
                  match first_string handler_result [ "label" ] with
                  | Some label -> Some label
                  | None -> first_string event_preparation [ "label" ]
                in
                let next_summary =
                  match summary_from_json handler_result with
                  | Some summary -> Some summary
                  | None -> summary_from_json event
                in
                let next_custom_instructions =
                  match first_string handler_result [ "customInstructions" ] with
                  | Some instructions -> Some instructions
                  | None -> first_string event_preparation [ "customInstructions" ]
                in
                let next_replace_instructions =
                  match bool_member "replaceInstructions" handler_result with
                  | Some _ as replace -> replace
                  | None -> bool_member "replaceInstructions" event_preparation
                in
                result :=
                  { !result with
                    tree_label = (match next_label with Some _ -> next_label | None -> (!result).tree_label);
                    tree_summary =
                      (match next_summary with Some _ -> next_summary | None -> (!result).tree_summary);
                    tree_custom_instructions =
                      (match next_custom_instructions with
                       | Some _ -> next_custom_instructions
                       | None -> (!result).tree_custom_instructions);
                    tree_replace_instructions =
                      (match next_replace_instructions with
                       | Some _ -> next_replace_instructions
                       | None -> (!result).tree_replace_instructions) }));
  !result

let emit_session_tree ?old_leaf_id ?new_leaf_id ?summary_entry ?from_extension () =
  emit_notification "session_tree"
    (`Assoc
      ([ ("type", `String "session_tree");
         ("oldLeafId", (match old_leaf_id with Some id -> `String id | None -> `Null));
         ("newLeafId", (match new_leaf_id with Some id -> `String id | None -> `Null)) ]
      @ (match summary_entry with Some summary -> [ ("summaryEntry", summary) ] | None -> [])
      @ (match from_extension with Some value -> [ ("fromExtension", `Bool value) ] | None -> [])))

let emit_session_shutdown ?session_file ?session_id ?session_name ~reason () =
  emit_notification "session_shutdown"
    (session_payload ?session_file ?session_id ?session_name "session_shutdown" reason)

let emit_session_compact ?session_file ?session_id ?session_name ~before_turn_count ~after_turn_count () =
  emit_notification "session_compact"
    (`Assoc
      ([ ("type", `String "session_compact");
         ("reason", `String "compact");
         ("cwd", `String (Sys.getcwd ()));
         ("beforeTurnCount", `Int before_turn_count);
         ("afterTurnCount", `Int after_turn_count) ]
      @ optional_string "sessionFile" session_file
      @ optional_string "sessionId" session_id
      @ optional_string "sessionName" session_name))

let provider_name = function Llm.Anthropic -> "anthropic" | Llm.Openai -> "openai"

let inferred_model_provider (cfg : Llm.config) =
  let matches (known : Llm.known) =
    known.protocol = cfg.provider && known.base_url = cfg.base_url && known.runtime = cfg.runtime
    && (known.default_model = cfg.model || cfg.runtime <> None || cfg.base_url <> "")
  in
  match List.find_opt matches (Llm.registry ()) with
  | Some { names = n :: _; _ } -> n
  | Some _ | None -> provider_name cfg.provider

let model_payload ?provider (cfg : Llm.config) =
  let provider = Option.value provider ~default:(inferred_model_provider cfg) in
  `Assoc
    ([ ("provider", `String provider);
       ("id", `String cfg.model);
       ("model", `String cfg.model);
       ("name", `String cfg.model);
       ("wireProtocol", `String (provider_name cfg.provider)) ]
    @
    match Models.context_window cfg.model with
    | Some window -> [ ("contextWindow", `Int window) ]
    | None -> [])

let same_model (left : Llm.config) (right : Llm.config) =
  inferred_model_provider left = inferred_model_provider right && left.model = right.model

let emit_thinking_level_select ~previous_level level =
  if previous_level <> level then
    emit_notification "thinking_level_select"
      (`Assoc
        [ ("type", `String "thinking_level_select");
          ("level", `String level);
          ("previousLevel", `String previous_level) ])

let emit_model_select ?(source = "set") ~(previous_model : Llm.config option) (cfg : Llm.config) =
  match previous_model with
  | Some previous when same_model previous cfg -> ()
  | previous_model ->
    emit_notification "model_select"
      (`Assoc
        ([ ("type", `String "model_select");
           ("model", model_payload cfg);
           ("source", `String source) ]
        @
        match previous_model with
        | Some previous -> [ ("previousModel", model_payload previous) ]
        | None -> []))

let emit_context messages =
  let current = ref messages in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if List.mem "context" events then
           let payload = `Assoc [ ("type", `String "context"); ("messages", `List (List.map Llm.turn_to_json !current)) ] in
           match emit_event path "context" payload with
           | Error _ -> ()
           | Ok json ->
             let next =
               match json |> member "result" with
               | `Assoc _ as result -> (
                 match result |> member "messages" with
                 | `List _ as messages -> turns_from_json messages
                 | _ -> [])
               | _ -> []
             in
             let next =
               if next <> [] then next
               else
                 match json |> member "event" with
                 | `Assoc _ as event -> (
                   match event |> member "messages" with
                   | `List _ as messages -> turns_from_json messages
                   | _ -> [])
                 | _ -> []
             in
             if next <> [] then current := next);
  !current

type before_agent_start_result =
  { injected_messages : Llm.turn list;
    system_prompt : string option }

let emit_before_agent_start ~prompt ~system_prompt =
  let current_system_prompt = ref system_prompt in
  let injected = ref [] in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if List.mem "before_agent_start" events then
           let payload =
             `Assoc
               [ ("type", `String "before_agent_start");
                 ("prompt", `String prompt);
                 ("systemPrompt", `String !current_system_prompt);
                 ( "systemPromptOptions",
                   `Assoc
                     [ ("cwd", `String (Sys.getcwd ()));
                       ("contextFiles", `List []);
                       ("skills", `List []);
                       ("selectedTools", `List []) ] ) ]
           in
           match emit_event path "before_agent_start" payload with
           | Error _ -> ()
           | Ok json -> (
             let result =
               match json |> member "result" with
               | `Assoc _ as result -> result
               | _ -> `Assoc []
             in
             (match result |> member "message" with
              | `Assoc _ as message -> injected := !injected @ turns_from_json message
              | _ -> ());
             (match result |> member "messages" with
              | `List _ as messages -> injected := !injected @ turns_from_json messages
              | _ -> ());
             match result |> member "systemPrompt" with
             | `String s -> current_system_prompt := s
             | _ -> ()));
  { injected_messages = !injected;
    system_prompt = if !current_system_prompt = system_prompt then None else Some !current_system_prompt }

let emit_agent_start () =
  emit_notification "agent_start" (`Assoc [ ("type", `String "agent_start") ])

let emit_agent_end ~messages =
  emit_notification "agent_end"
    (`Assoc [ ("type", `String "agent_end"); ("messages", `List (List.map Llm.turn_to_json messages)) ])

let emit_turn_start ~turn_index =
  let timestamp = int_of_float (Unix.gettimeofday () *. 1000.) in
  emit_notification "turn_start"
    (`Assoc [ ("type", `String "turn_start"); ("turnIndex", `Int turn_index); ("timestamp", `Int timestamp) ])

let emit_turn_end ~turn_index ~message ~tool_results =
  emit_notification "turn_end"
    (`Assoc
      [ ("type", `String "turn_end");
        ("turnIndex", `Int turn_index);
        ("message", Llm.turn_to_json message);
        ("toolResults", `List (List.map Llm.content_to_json tool_results)) ])

let emit_message_start turn =
  emit_notification "message_start" (`Assoc [ ("type", `String "message_start"); ("message", Llm.turn_to_json turn) ])

let emit_message_update ?(delta = "") turn =
  emit_notification "message_update"
    (`Assoc
      [ ("type", `String "message_update");
        ("message", Llm.turn_to_json turn);
        ("assistantMessageEvent", `Assoc [ ("type", `String "text_delta"); ("text", `String delta) ]) ])

let emit_message_end turn =
  let current = ref turn in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if List.mem "message_end" events then
           let payload = `Assoc [ ("type", `String "message_end"); ("message", Llm.turn_to_json !current) ] in
           match emit_event path "message_end" payload with
           | Error _ -> ()
           | Ok json -> (
             let result_message =
               match json |> member "result" with
               | `Assoc _ as result -> (
                 match result |> member "message" with
                 | `Assoc _ as msg -> Some msg
                 | _ -> None)
               | _ -> None
             in
             let event_message =
               match json |> member "event" with
               | `Assoc _ as event -> (
                 match event |> member "message" with
                 | `Assoc _ as msg -> Some msg
                 | _ -> None)
               | _ -> None
             in
             match Option.value result_message ~default:(Option.value event_message ~default:`Null) with
             | `Assoc _ as msg ->
               let next = Llm.turn_of_json msg in
               if next.Llm.role = (!current).Llm.role then current := next
             | _ -> ()));
  !current

let emit_tool_execution_start ~tool_call_id ~tool_name ~input =
  emit_notification "tool_execution_start"
    (`Assoc
      [ ("type", `String "tool_execution_start");
        ("toolCallId", `String tool_call_id);
        ("toolName", `String (Tools.wire_name tool_name));
        ("args", input) ])

let emit_tool_execution_update ~tool_call_id ~tool_name ~input partial_result =
  emit_notification "tool_execution_update"
    (`Assoc
      [ ("type", `String "tool_execution_update");
        ("toolCallId", `String tool_call_id);
        ("toolName", `String (Tools.wire_name tool_name));
        ("args", input);
        ("partialResult", partial_result) ])

let emit_tool_execution_end ~tool_call_id ~tool_name ~result ~is_error =
  emit_notification "tool_execution_end"
    (`Assoc
      [ ("type", `String "tool_execution_end");
        ("toolCallId", `String tool_call_id);
        ("toolName", `String (Tools.wire_name tool_name));
        ("result", `Assoc [ ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String result) ] ]) ]);
        ("isError", `Bool is_error) ])

let emit_tool_call ~tool_call_id ~tool_name input =
  let input = ref input in
  let blocked = ref None in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if !blocked = None && List.mem "tool_call" events then
           let payload =
             `Assoc
               [ ("type", `String "tool_call");
                 ("toolCallId", `String tool_call_id);
                 ("toolName", `String (Tools.wire_name tool_name));
                 ("input", !input) ]
           in
           match emit_event path "tool_call" payload with
           | Error msg -> blocked := Some ("Extension failed, blocking execution: " ^ msg)
           | Ok json ->
             (match json |> member "event" |> member "input" with
              | `Null -> ()
              | next -> input := next);
             (match json |> member "result" with
              | `Assoc _ as result -> (
                match result |> member "block" with
                | `Bool true ->
                  let reason = match result |> member "reason" with `String s -> s | _ -> "Blocked by extension" in
                  blocked := Some reason
                | _ -> ())
              | _ -> ()));
  match !blocked with
  | Some reason -> Tool_block reason
  | None -> Tool_continue !input

let emit_tool_result ~tool_call_id ~tool_name ~input result =
  let result_text = ref result in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if List.mem "tool_result" events then
           let payload =
             `Assoc
               [ ("type", `String "tool_result");
                 ("toolCallId", `String tool_call_id);
                 ("toolName", `String (Tools.wire_name tool_name));
                 ("input", input);
                 ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String !result_text) ] ]);
                 ("isError", `Bool (String.length !result_text >= 6 && String.sub !result_text 0 6 = "Error:")) ]
           in
           match emit_event path "tool_result" payload with
           | Error _ -> ()
          | Ok json -> (
             match json |> member "result" |> member "content" |> content_text with
             | Some text -> result_text := text
             | None -> (
               match json |> member "event" |> member "content" |> content_text with
               | Some text -> result_text := text
               | None -> ())));
  !result_text

let emit_input ?(source = "interactive") text =
  let text = ref text in
  let handled = ref false in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if (not !handled) && List.mem "input" events then
           let payload =
             `Assoc
               [ ("type", `String "input");
                 ("text", `String !text);
                 ("source", `String source) ]
           in
           match emit_event path "input" payload with
           | Error _ -> ()
           | Ok json -> (
             let result =
               match json |> member "result" with
               | `Assoc _ as result -> result
               | _ -> `Assoc []
             in
             let event =
               match json |> member "event" with
               | `Assoc _ as event -> event
               | _ -> `Assoc []
             in
             (match result |> member "action" with
              | `String "handled" -> handled := true
              | `String "transform" -> (
                match result |> member "text" with
                | `String s -> text := s
                | _ -> ())
              | _ -> (
                match event |> member "text" with
                | `String s -> text := s
                | _ -> ()))));
  if !handled then Input_handled else Input_continue !text

let int_member names json =
  List.find_map
    (fun name ->
      match json |> member name with
      | `Int n -> Some n
      | `Intlit s -> int_of_string_opt s
      | _ -> None)
    names

let bash_result json =
  match json with
  | `Assoc _ ->
    let result =
      match json |> member "result" with
      | `Assoc _ as result -> result
      | _ -> json
    in
    let output =
      match result |> member "output" with
      | `String s -> Some s
      | _ -> (
        match result |> member "stdout" with
        | `String s -> Some s
        | _ -> None)
    in
    (match output, int_member [ "exitCode"; "exit_code"; "code" ] result with
     | Some output, Some exit_code -> Some { exit_code; output }
     | _ -> None)
  | _ -> None

let emit_user_bash ~command ~exclude_from_context =
  let replacement = ref None in
  !event_paths
  |> List.rev
  |> List.iter (fun (path, events) ->
         if !replacement = None && List.mem "user_bash" events then
           let payload =
             `Assoc
               [ ("type", `String "user_bash");
                 ("command", `String command);
                 ("excludeFromContext", `Bool exclude_from_context);
                 ("cwd", `String (Sys.getcwd ())) ]
           in
           match
             run_node_bridge
               (`Assoc
                 [ ("mode", `String "event");
                   ("path", `String path);
                   ("event", `String "user_bash");
                   ("payload", payload);
                   ("executionCommand", `String (Tools.apply_command_prefix command)) ])
           with
           | Error _ -> ()
           | Ok json -> (
             match bash_result (json |> member "result") with
             | Some result -> replacement := Some result
             | None -> ()));
  !replacement

let () =
  Llm.set_provider_request_hook emit_before_provider_request;
  Llm.set_provider_response_hook emit_after_provider_response
