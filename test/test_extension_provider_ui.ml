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

let contains hay needle =
  let hay_len = String.length hay and needle_len = String.length needle in
  let rec loop i =
    i + needle_len <= hay_len
    && (String.sub hay i needle_len = needle || loop (i + 1))
  in
  needle_len = 0 || loop 0

let () =
  let dir = Filename.temp_dir "agent_extension_provider_ui_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  let node_available = Sys.command "command -v node >/dev/null 2>&1" = 0 in
  write_file ".pi/extensions/provider.ts"
    {|export default function(pi) {
  pi.registerProvider({
    name: "localai",
    aliases: ["local"],
    protocol: "openai",
    baseUrl: "https://local.invalid/v1",
    apiKeyEnvVar: "LOCALAI_API_KEY",
    defaultModel: "local-large",
    headers: { "X-Local": "1" },
    models: [{ id: "local-large", contextWindow: 4242 }, "local-small"],
  });
}
|};
  write_file ".pi/extensions/provider-unregister.ts"
    {|export default function(pi) {
  pi.registerCommand("provideradd", {
    description: "Register provider at runtime",
    handler: async () => {
      pi.registerProvider({
        name: "lateai",
        protocol: "openai",
        baseUrl: "https://late.invalid/v1",
        apiKeyEnvVar: "LATEAI_API_KEY",
        defaultModel: "late-small",
        models: [{ id: "late-small", contextWindow: 777 }],
      });
      return "added";
    },
  });
  pi.registerCommand("providerdrop", {
    description: "Unregister provider by alias",
    handler: async () => {
      pi.unregisterProvider("local");
      return "dropped";
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
  write_file ".pi/extensions/auth-storage.ts"
    {|const { AuthStorage, ModelRegistry } = require("@earendil-works/pi-coding-agent");

const oauthConfig = {
  name: "OAuth Probe",
  api: "openai",
  baseUrl: "https://oauth.invalid/v1",
  defaultModel: "oauth-small",
  models: [{ id: "oauth-small", name: "OAuth Small", api: "openai", contextWindow: 123 }],
  oauth: {
    name: "OAuth Login",
    async login(callbacks) {
      callbacks.openUrl?.("https://oauth.invalid/login");
      const code = callbacks.readCode ? await callbacks.readCode() : "missing";
      return { access: `login-${code}`, refresh: "refresh-token", expires: Date.now() - 1000 };
    },
    async refreshToken(credentials) {
      return { access: `${credentials.access}-refreshed`, refresh: credentials.refresh, expires: Date.now() + 60000 };
    },
    getApiKey(credentials) {
      return `Bearer ${credentials.access}`;
    },
    modifyModels(models, credentials) {
      return models.map((model) => model.provider === "oauthprobe" ? { ...model, baseUrl: `https://${credentials.access}.invalid/v1` } : model);
    },
  },
};

export default function(pi) {
  pi.registerCommand("authprobe", {
    description: "Probe AuthStorage OAuth provider parity",
    handler: async () => {
      const storage = AuthStorage.inMemory();
      const registry = ModelRegistry.inMemory(storage);
      registry.registerProvider("oauthprobe", oauthConfig);
      let opened = "";
      await storage.login("oauthprobe", {
        openUrl(url) { opened = url; },
        readCode: async () => "code",
      });
      const before = storage.get("oauthprobe");
      const key = await storage.getApiKey("oauthprobe");
      const after = storage.get("oauthprobe");
      registry.refresh();
      const model = registry.find("oauthprobe", "oauth-small");
      const auth = await registry.getApiKeyAndHeaders(model);
      const usingOAuth = registry.isUsingOAuth(model);
      const commandStorage = AuthStorage.inMemory({ storedcmd: { type: "api_key", key: "!printf stored-secret" } });
      const commandRegistry = ModelRegistry.inMemory(commandStorage);
      commandRegistry.registerProvider("cmdai", {
        name: "Command AI",
        api: "openai",
        baseUrl: "https://cmd.invalid/v1",
        apiKey: "!printf provider-secret",
        authHeader: true,
        headers: { "X-Command": "!printf header-secret", "X-Literal": "literal-header" },
        defaultModel: "cmd-small",
        models: [{ id: "cmd-small", name: "Command Small", api: "openai", contextWindow: 321 }],
      });
      const commandModel = commandRegistry.find("cmdai", "cmd-small");
      const commandAuth = await commandRegistry.getApiKeyAndHeaders(commandModel);
      const storedCommandKey = await commandStorage.getApiKey("storedcmd");
      storage.logout("oauthprobe");
      return [
        storage.getOAuthProviders().some((provider) => provider.id === "oauthprobe"),
        opened,
        before.access,
        key,
        after.access,
        registry.getProviderDisplayName("oauthprobe"),
        usingOAuth,
        model.baseUrl,
        auth.ok,
        auth.apiKey,
        storedCommandKey,
        commandAuth.apiKey,
        commandAuth.headers["X-Command"],
        commandAuth.headers["X-Literal"],
        commandAuth.headers.Authorization,
        storage.has("oauthprobe"),
      ].join("|");
    },
  });
}
|};
  write_file ".pi/extensions/shortcut-renderer.ts"
    {|export default function(pi) {
  pi.registerFlag("voice", { description: "Voice flag", type: "string", defaultValue: "calm" });
  pi.registerFlag({ name: "dry-run", description: "Dry run", type: "boolean", default: false });
  pi.registerShortcut("ctrl+g", { description: "Show voice shortcut", handler: async () => `shortcut ${pi.getFlag("voice")}` });
  pi.registerShortcut({ key: "ctrl+h", description: "Run tshello shortcut", command: "/tshello Shortcut" });
  pi.registerMessageRenderer("tagger", {
    description: "Tag assistant and tool text",
    target: "all",
    render: async (event) => `[${event.kind}:${event.toolName || event.role}] ${event.text}`,
  });
  pi.registerCommand("tshello", {
    description: "Say hello from TypeScript",
    handler: async (args) => ({ content: [{ type: "text", text: `Command ${args || "world"}` }] }),
  });
  pi.registerCommand("flagshow", {
    description: "Show registered flags",
    handler: async () => `${pi.getFlag("voice")}:${pi.getFlag("dry-run")}`,
  });
}
|};
  write_file ".pi/extensions/ui.ts"
    {|export default function(pi) {
  pi.registerTool({
    name: "ui_tool",
    label: "UI Tool",
    description: "Use extension UI fallback",
    parameters: { type: "object", properties: {} },
    async execute(_id, _params, _operations, _system, ctx) {
      ctx.ui.notify("tool notice");
      const ok = await ctx.ui.confirm("continue?");
      const name = await ctx.ui.input("name?", { defaultValue: "anon" });
      const pick = await ctx.ui.select("pick?", ["one", "two"], { defaultIndex: 1 });
      return { content: [{ type: "text", text: `tool ${ok}:${name}:${pick}` }], details: {} };
    },
  });
  pi.registerShortcut("ctrl+u", {
    description: "Use UI fallback shortcut",
    handler: async (ctx) => {
      ctx.ui.notify("shortcut notice");
      return await ctx.ui.input("label?", { defaultValue: "fallback" });
    },
  });
}
|};
  write_file ".pi/extensions/rich-renderer.ts"
    {|export default function(pi) {
  pi.registerRenderer("richbox", {
    description: "Rich renderer fallback",
    target: "rich_component",
    render: async (event) => ({
      type: "panel",
      children: [
        { type: "markdown", markdown: `**Rich** ${event.text}` },
        { type: "text", text: "tail" },
      ],
    }),
  });
}
|};

  ignore (Extensions.load ());
  check "TypeScript extension tool ctx.ui fallback returns defaults"
    ((not node_available)
     ||
     match Tools.find "ui_tool" with
     | Some t ->
       let output = t.Tools.execute (`Assoc []) in
       contains output "tool notice" && contains output "tool false:anon:two"
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
  check "TypeScript SDK AuthStorage supports extension OAuth providers"
    ((not node_available)
     ||
     match Extensions.execute_command "/authprobe" with
     | Some output ->
       output
       = "true|https://oauth.invalid/login|login-code|Bearer login-code-refreshed|login-code-refreshed|OAuth Probe|true|https://login-code-refreshed.invalid/v1|true|Bearer login-code-refreshed|stored-secret|provider-secret|header-secret|literal-header|Bearer provider-secret|false"
     | None -> false);
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
     | Some (Extensions.Shortcut_output output) -> contains output "shortcut notice" && contains output "fallback"
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
       contains response.text "fallback" && List.mem "notify" kinds && List.mem "input" kinds
     | _ -> false);
  check "hotkeys include extension shortcuts"
    ((not node_available)
     || (contains (Commands.hotkeys ()) "ctrl+g" && contains (Commands.hotkeys ()) "Show voice shortcut"));
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
     contains output "Rich" && contains output "hello" && contains output "tail");
  check "TypeScript extension rich renderer exposes structured components"
    ((not node_available)
     ||
     let response = Extensions.render_response ~kind:"rich_component" ~role:"assistant" "hello" in
     response.components <> []
     && contains response.rendered "+--"
     && contains response.rendered "Rich"
     && contains response.rendered "tail");

  if !failures > 0 then exit 1
