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
    runtime : extension_runtime;
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
    path : string;
    runtime : extension_runtime }

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
  let command = Printf.sprintf "node %s" (Filename.quote (Extension_node_bridge.path ())) in
  let code, body = Tools.run_process ~stdin_data:(Yojson.Safe.to_string request) command in
  if code <> 0 then Error (Printf.sprintf "node bridge exited %d: %s" code body)
  else
    try
      match Yojson.Safe.from_string body with
      | `Assoc _ as json -> (
        match bridge_json_result body json with
        | Ok json ->
          apply_runtime_state_from_json json;
          Ok json
        | Error _ as error -> error)
      | _ -> Error body
    with
    | Sys.Break as e -> raise e
    | e -> Error (Printexc.to_string e ^ ": " ^ body)

let ocaml_sdk_command path =
  let direct () = Filename.quote path in
  try
    match Yojson.Safe.from_file path with
    | `Assoc _ as json -> (
      match json |> member "command" with
      | `String command when String.trim command <> "" ->
        let cwd =
          match json |> member "cwd" with
          | `String dir when String.trim dir <> "" ->
            let dir = Config_paths.expand_tilde (String.trim dir) in
            if Filename.is_relative dir then Filename.concat (Filename.dirname path) dir else dir
          | _ -> Sys.getcwd ()
        in
        Printf.sprintf "cd %s && %s" (Filename.quote cwd) command
      | _ -> direct ())
    | _ -> direct ()
  with
  | Sys.Break as e -> raise e
  | _ -> direct ()

let run_ocaml_sdk_bridge path request =
  let request = bridge_request_context request in
  let command = ocaml_sdk_command path in
  let code, body = Tools.run_process ~stdin_data:(Yojson.Safe.to_string request) command in
  if code <> 0 then Error (Printf.sprintf "OCaml extension exited %d: %s" code body)
  else
    try
      match Yojson.Safe.from_string body with
      | `Assoc _ as json -> (
        match bridge_json_result body json with
        | Ok json ->
          apply_runtime_state_from_json json;
          Ok json
        | Error _ as error -> error)
      | _ -> Error body
    with
    | Sys.Break as e -> raise e
    | e -> Error (Printexc.to_string e ^ ": " ^ body)

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

let shortcut_of_json runtime path (j : Yojson.Safe.t) : shortcut option =
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
    Some { spec; description; path; runtime; command; has_handler }
  | _ -> None

let js_shortcut_of_json path = shortcut_of_json Node path
let ocaml_sdk_shortcut_of_json path = shortcut_of_json Ocaml_sdk path

let message_renderer_of_json runtime path (j : Yojson.Safe.t) : message_renderer option =
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
    Some { name = String.trim name; description; target; path; runtime }
  | _ -> None

let js_message_renderer_of_json path = message_renderer_of_json Node path
let ocaml_sdk_message_renderer_of_json path = message_renderer_of_json Ocaml_sdk path

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

let register_ocaml_sdk_shortcuts path json =
  match json |> member "shortcuts" with
  | `List xs ->
    xs
    |> List.filter_map (ocaml_sdk_shortcut_of_json path)
    |> List.iter register_shortcut
  | _ -> ()

let register_js_message_renderers path json =
  match json |> member "renderers" with
  | `List xs ->
    xs
    |> List.filter_map (js_message_renderer_of_json path)
    |> List.iter register_message_renderer
  | _ -> ()

let register_ocaml_sdk_message_renderers path json =
  match json |> member "renderers" with
  | `List xs ->
    xs
    |> List.filter_map (ocaml_sdk_message_renderer_of_json path)
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

let register_provider_runtime_for runtime_kind path provider_name runtime =
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
      match run_extension_bridge runtime_kind path request with
      | Error msg -> raise (Llm.Api_error msg)
      | Ok json ->
        let blocks = content_blocks_from_json json in
        List.iter (function Llm.Text text -> on_text text | _ -> ()) blocks;
        let usage = usage_from_json (json |> member "usage") in
        (blocks, usage))

let register_js_provider_runtime path provider_name runtime =
  register_provider_runtime_for Node path provider_name runtime

let register_ocaml_sdk_provider_runtime path provider_name runtime =
  register_provider_runtime_for Ocaml_sdk path provider_name runtime

let register_providers_for runtime_kind path json =
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
               | `String s -> Extension_event_json.truthy s
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
             Option.iter (register_provider_runtime_for runtime_kind provider_path name) runtime;
             register_provider_models (String.lowercase_ascii (String.trim name)) default_model (provider |> member "models"))
  | _ -> ()

let register_js_providers path json = register_providers_for Node path json
let register_ocaml_sdk_providers path json = register_providers_for Ocaml_sdk path json

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

let register_ocaml_sdk_response path json =
  register_ocaml_sdk_commands path json;
  register_ocaml_sdk_shortcuts path json;
  register_ocaml_sdk_message_renderers path json;
  register_ocaml_sdk_providers path json;
  register_ocaml_sdk_tools path json

let emit_session_start_for_path ?previous_session_file ?session_file ?session_id ?session_name ?(reason = "startup")
    runtime path =
  let payload =
    Extension_event_json.session_payload ?previous_session_file ?session_file ?session_id ?session_name "session_start" reason
  in
  match
    run_extension_bridge runtime path
      (`Assoc [ ("mode", `String "event"); ("path", `String path); ("event", `String "session_start"); ("payload", payload) ])
  with
  | Error _ -> []
  | Ok json ->
    (match runtime with
     | Node ->
       register_js_commands path json;
       register_js_shortcuts path json;
       register_js_message_renderers path json;
       register_js_providers path json;
       register_js_tools path json
     | Ocaml_sdk -> register_ocaml_sdk_response path json)

let emit_session_start ?previous_session_file ?session_file ?session_id ?session_name ~reason () =
  let registered = ref [] in
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if List.mem "session_start" events then
           registered :=
             !registered
             @ emit_session_start_for_path ?previous_session_file ?session_file ?session_id ?session_name ~reason runtime path);
  !registered

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
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if List.mem "resources_discover" events then
           match
             run_extension_bridge runtime path
               (`Assoc
                 [ ("mode", `String "event");
                   ("path", `String path);
                   ("event", `String "resources_discover");
                   ("payload", payload) ])
           with
           | Error _ -> ()
           | Ok json ->
             let result = Extension_event_json.resource_result json in
             discovered_skill_paths := !discovered_skill_paths @ Extension_event_json.json_string_list "skillPaths" result;
             discovered_prompt_paths := !discovered_prompt_paths @ Extension_event_json.json_string_list "promptPaths" result;
             discovered_theme_paths := !discovered_theme_paths @ Extension_event_json.json_string_list "themePaths" result);
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
      if List.mem "session_start" events then names @ emit_session_start_for_path ~reason Node path else names

let load_ocaml_sdk_extension ?(reason = "startup") path =
  let request = `Assoc [ ("mode", `String "describe"); ("path", `String path); ("reason", `String reason) ] in
  match run_ocaml_sdk_bridge path request with
  | Error msg ->
    Printf.eprintf "[warning] failed to load OCaml extension %s: %s\n%!" path msg;
    []
  | Ok json ->
    register_ocaml_sdk_commands path json;
    register_ocaml_sdk_shortcuts path json;
    register_ocaml_sdk_message_renderers path json;
    register_ocaml_sdk_providers path json;
    let events =
      match json |> member "events" with
      | `List xs -> List.filter_map (function `String s -> Some s | _ -> None) xs
      | _ -> []
    in
    register_events ~runtime:Ocaml_sdk path events;
    let names = register_ocaml_sdk_tools path json in
    if List.mem "session_start" events then names @ emit_session_start_for_path ~reason Ocaml_sdk path else names

let load_path ?(reason = "startup") path =
  if Extension_manifest.is_json_file path then Extension_manifest.load_manifest path
  else if Extension_manifest.is_js_extension_file path then load_js_extension ~reason path
  else if Extension_manifest.is_ocaml_sdk_extension_file path then load_ocaml_sdk_extension ~reason path
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
  ocaml_event_paths := [];
  js_extension_paths := [];
  discovered_skill_paths := [];
  discovered_prompt_paths := [];
  discovered_theme_paths := [];
  Llm.clear_extension_providers ();
  Models.clear_extension_models ();
  let names =
    Extension_manifest.manifest_paths ()
    |> List.concat_map Extension_manifest.expand_manifest_path
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

let render_response_of_json json =
  let components = components_of_json json in
  let rendered =
    match Extension_component_text.components_text components with
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
    Some
      (match shortcut.command with
       | Some command -> Shortcut_response_command command
       | None ->
         let request =
           `Assoc
             (add_runtime_context ?session_name ?session_context ?themes ?theme_name ?model ?models ?commands ?context_usage
                ?system_prompt ?has_ui ?is_idle ?has_pending_messages ?tools_expanded
                [ ("mode", `String "shortcut");
                  ("path", `String shortcut.path);
                  ("shortcut", `String shortcut.spec) ])
         in
         match run_extension_bridge shortcut.runtime shortcut.path request with
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
             match run_extension_bridge renderer.runtime renderer.path request with
             | Ok json -> (
               let response = render_response_of_json json in
               collected_components := !collected_components @ response.components;
               collected_ui := merge_ui !collected_ui response.render_ui;
               if String.trim response.rendered <> "" then current := response.rendered)
             | Error _ -> ());
    let rendered =
      match Extension_component_text.components_text !collected_components with
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

let emit_event runtime path event payload =
  run_extension_bridge runtime path
    (`Assoc [ ("mode", `String "event"); ("path", `String path); ("event", `String event); ("payload", payload) ])

let emit_before_provider_request payload =
  let current = ref payload in
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if List.mem "before_provider_request" events then
           let event_payload =
             `Assoc [ ("type", `String "before_provider_request"); ("payload", !current) ]
           in
           match emit_event runtime path "before_provider_request" event_payload with
           | Ok json -> (
             match json |> member "result" with
             | `Null -> ()
             | replacement -> current := replacement)
           | Error _ -> ());
  !current

let emit_notification event payload =
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if List.mem event events then ignore (emit_event runtime path event payload))

let emit_after_provider_response ~status ~headers =
  emit_notification "after_provider_response"
    (`Assoc
      [ ("type", `String "after_provider_response");
        ("status", `Int status);
        ("headers", Extension_event_json.headers_json headers) ])

type session_before_result =
  | Session_continue
  | Session_cancel of string

type session_before_tree_result =
  { tree_cancel : string option;
    tree_label : string option;
    tree_summary : Yojson.Safe.t option;
    tree_custom_instructions : string option;
    tree_replace_instructions : bool option }

let session_before event payload =
  let decision = ref Session_continue in
  let cancelled () = match !decision with Session_cancel _ -> true | Session_continue -> false in
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if (not (cancelled ())) && List.mem event events then
           match emit_event runtime path event payload with
           | Error _ -> ()
           | Ok json ->
             let result_cancel = Extension_event_json.cancellation_from_json (json |> member "result") in
             let event_cancel = Extension_event_json.cancellation_from_json (json |> member "event") in
             (match Option.value result_cancel ~default:(Option.value event_cancel ~default:"") with
              | "" -> ()
              | reason -> decision := Session_cancel reason));
  !decision

let emit_session_before_switch ?current_session_file ?current_session_id ?current_session_name ?target_session_file
    ~reason () =
  session_before "session_before_switch"
    (Extension_event_json.session_payload ?current_session_file ?current_session_id ?current_session_name ?target_session_file
       "session_before_switch" reason)

let emit_session_before_fork ?current_session_file ?current_session_id ?current_session_name ?source_session_file
    ?entry_id ?position ~reason () =
  session_before "session_before_fork"
    (Extension_event_json.session_payload ?current_session_file ?current_session_id ?current_session_name ?source_session_file ?entry_id
       ?position "session_before_fork" reason)

let emit_session_before_compact ?session_file ?session_id ?session_name ~turn_count () =
  session_before "session_before_compact"
    (`Assoc
      ([ ("type", `String "session_before_compact");
         ("reason", `String "compact");
         ("cwd", `String (Sys.getcwd ()));
         ("turnCount", `Int turn_count) ]
      @ Extension_event_json.optional_string "sessionFile" session_file
      @ Extension_event_json.optional_string "sessionId" session_id
      @ Extension_event_json.optional_string "sessionName" session_name))

let emit_session_before_tree ~target_id ?old_leaf_id ?common_ancestor_id ?label ?custom_instructions
    ?replace_instructions ~user_wants_summary ~entries_to_summarize () =
  let preparation =
    `Assoc
      ([ ("targetId", `String target_id);
         ("oldLeafId", (match old_leaf_id with Some id -> `String id | None -> `Null));
         ("commonAncestorId", (match common_ancestor_id with Some id -> `String id | None -> `Null));
         ("entriesToSummarize", `List entries_to_summarize);
         ("userWantsSummary", `Bool user_wants_summary) ]
      @ Extension_event_json.optional_string "label" label
      @ Extension_event_json.optional_string "customInstructions" custom_instructions
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
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if (not (cancelled ())) && List.mem "session_before_tree" events then
           match emit_event runtime path "session_before_tree" payload with
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
             let result_cancel = Extension_event_json.cancellation_from_json handler_result in
             let event_cancel = Extension_event_json.cancellation_from_json event in
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
                  match Extension_event_json.summary_from_json handler_result with
                  | Some summary -> Some summary
                  | None -> Extension_event_json.summary_from_json event
                in
                let next_custom_instructions =
                  match first_string handler_result [ "customInstructions" ] with
                  | Some instructions -> Some instructions
                  | None -> first_string event_preparation [ "customInstructions" ]
                in
                let next_replace_instructions =
                  match Extension_event_json.bool_member "replaceInstructions" handler_result with
                  | Some _ as replace -> replace
                  | None -> Extension_event_json.bool_member "replaceInstructions" event_preparation
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
    (Extension_event_json.session_payload ?session_file ?session_id ?session_name "session_shutdown" reason)

let emit_session_compact ?session_file ?session_id ?session_name ~before_turn_count ~after_turn_count () =
  emit_notification "session_compact"
    (`Assoc
      ([ ("type", `String "session_compact");
         ("reason", `String "compact");
         ("cwd", `String (Sys.getcwd ()));
         ("beforeTurnCount", `Int before_turn_count);
         ("afterTurnCount", `Int after_turn_count) ]
      @ Extension_event_json.optional_string "sessionFile" session_file
      @ Extension_event_json.optional_string "sessionId" session_id
      @ Extension_event_json.optional_string "sessionName" session_name))

let emit_thinking_level_select ~previous_level level =
  if previous_level <> level then
    emit_notification "thinking_level_select"
      (`Assoc
        [ ("type", `String "thinking_level_select");
          ("level", `String level);
          ("previousLevel", `String previous_level) ])

let emit_model_select ?(source = "set") ~(previous_model : Llm.config option) (cfg : Llm.config) =
  match previous_model with
  | Some previous when Extension_event_json.same_model previous cfg -> ()
  | previous_model ->
    emit_notification "model_select"
      (`Assoc
        ([ ("type", `String "model_select");
           ("model", Extension_event_json.model_payload cfg);
           ("source", `String source) ]
        @
        match previous_model with
        | Some previous -> [ ("previousModel", Extension_event_json.model_payload previous) ]
        | None -> []))

let emit_context messages =
  let current = ref messages in
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if List.mem "context" events then
           let payload = `Assoc [ ("type", `String "context"); ("messages", `List (List.map Llm.turn_to_json !current)) ] in
           match emit_event runtime path "context" payload with
           | Error _ -> ()
           | Ok json ->
             let next =
               match json |> member "result" with
               | `Assoc _ as result -> (
                 match result |> member "messages" with
                 | `List _ as messages -> Extension_event_json.turns_from_json messages
                 | _ -> [])
               | _ -> []
             in
             let next =
               if next <> [] then next
               else
                 match json |> member "event" with
                 | `Assoc _ as event -> (
                   match event |> member "messages" with
                   | `List _ as messages -> Extension_event_json.turns_from_json messages
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
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
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
           match emit_event runtime path "before_agent_start" payload with
           | Error _ -> ()
           | Ok json -> (
             let result =
               match json |> member "result" with
               | `Assoc _ as result -> result
               | _ -> `Assoc []
             in
             (match result |> member "message" with
              | `Assoc _ as message -> injected := !injected @ Extension_event_json.turns_from_json message
              | _ -> ());
             (match result |> member "messages" with
              | `List _ as messages -> injected := !injected @ Extension_event_json.turns_from_json messages
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
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if List.mem "message_end" events then
           let payload = `Assoc [ ("type", `String "message_end"); ("message", Llm.turn_to_json !current) ] in
           match emit_event runtime path "message_end" payload with
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
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if !blocked = None && List.mem "tool_call" events then
           let payload =
             `Assoc
               [ ("type", `String "tool_call");
                 ("toolCallId", `String tool_call_id);
                 ("toolName", `String (Tools.wire_name tool_name));
                 ("input", !input) ]
           in
           match emit_event runtime path "tool_call" payload with
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
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
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
           match emit_event runtime path "tool_result" payload with
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
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if (not !handled) && List.mem "input" events then
           let payload =
             `Assoc
               [ ("type", `String "input");
                 ("text", `String !text);
                 ("source", `String source) ]
           in
           match emit_event runtime path "input" payload with
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
  all_event_targets ()
  |> List.rev
  |> List.iter (fun (runtime, path, events) ->
         if !replacement = None && List.mem "user_bash" events then
           let payload =
             `Assoc
               [ ("type", `String "user_bash");
                 ("command", `String command);
                 ("excludeFromContext", `Bool exclude_from_context);
                 ("cwd", `String (Sys.getcwd ())) ]
           in
           match
             run_extension_bridge runtime path
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
