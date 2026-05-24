(* RPC driver over stdin/stdout (one JSON object per line), for embedding the
   agent in other programs.

   The original OCaml driver accepted small JSON-RPC-ish `method` requests. Pi
   uses command objects with a top-level `type` field. This module accepts both:

   Legacy: {"method":"send","params":{"message":"..."}}
   Pi:     {"id":"1","type":"prompt","message":"..."}

   Pi commands emit {"type":"response","command":...,"success":...} objects.
   Streaming/tool events continue to use the existing event objects. *)

open Yojson.Safe.Util

let emit_stdout (j : Yojson.Safe.t) =
  print_string (Yojson.Safe.to_string j);
  print_char '\n';
  flush stdout

let event emit ty fields = emit (`Assoc (("type", `String ty) :: fields))
let error_event emit msg = event emit "error" [ ("message", `String msg) ]

(* The RPC bash/execute_bash endpoint runs arbitrary shell commands from stdin.
   Enabled by default, but can be disabled (e.g. when the RPC channel may carry
   untrusted input) by setting AGENT_RPC_ALLOW_BASH to a falsy value. *)
let rpc_bash_allowed () =
  match Sys.getenv_opt "AGENT_RPC_ALLOW_BASH" with
  | None -> true
  | Some v -> (
    match String.lowercase_ascii (String.trim v) with
    | "0" | "false" | "no" | "off" | "" -> false
    | _ -> true)

let id_field = function Some id -> [ ("id", `String id) ] | None -> []

let response ?id ?data command =
  let fields =
    id_field id
    @ [ ("type", `String "response"); ("command", `String command); ("success", `Bool true) ]
    @ match data with Some d -> [ ("data", d) ] | None -> []
  in
  `Assoc fields

let response_error ?id command msg =
  `Assoc
    (id_field id
    @ [ ("type", `String "response");
        ("command", `String command);
        ("success", `Bool false);
        ("error", `String msg) ])

let provider_name = function Llm.Anthropic -> "anthropic" | Llm.Openai -> "openai"

let extension_theme_json (theme : Themes.t) =
  `Assoc
    [ ("name", `String theme.name);
      ("path", (if theme.location = "<builtin>" then `Null else `String theme.location));
      ("location", `String theme.location) ]

let extension_theme_context () =
  (List.map extension_theme_json (Themes.discover ()), (Themes.current_theme ()).Themes.name)

let extension_context_usage agent =
  let used, window, frac = Agent.usage_info agent in
  `Assoc
    [ ("tokens", `Int used);
      ("contextWindow", `Int window);
      ("percent", `Float (frac *. 100.)) ]

let extension_session_context agent =
  match Agent.session agent with
  | Some session -> Extensions.session_context_json ~entries:session.Session.entries ~info:(Session.info_of session) (Agent.turns agent)
  | None -> Extensions.session_context_json (Agent.turns agent)

let make_frontend emit : Agent.frontend =
  { text_delta = (fun s -> event emit "text_delta" [ ("text", `String s) ]);
    text_done = (fun () -> event emit "text_done" []);
    thinking = (fun s -> if String.trim s <> "" then event emit "thinking" [ ("text", `String s) ]);
    tool_call = (fun name prev -> event emit "tool_call" [ ("name", `String name); ("input", `String prev) ]);
    tool_result = (fun res -> event emit "tool_result" [ ("content", `String res) ]);
    notice = (fun s -> event emit "notice" [ ("text", `String s) ]);
    message_end = (fun _ _ _ _ -> ());
    tool_result_end = (fun _ -> ());
    confirm_bash =
      (fun cmd ->
        event emit "bash_denied" [ ("command", `String cmd) ];
        Agent.Deny) }

let usage_json agent =
  let used, window, pct = Agent.usage_info agent in
  `Assoc
    [ ("context_used", `Int used);
      ("context_window", `Int window);
      ("percent", `Float pct);
      ("contextUsed", `Int used);
      ("contextWindow", `Int window) ]

let opt_str j k =
  match j |> member k with
  | `String s -> Some s
  | `Int n -> Some (string_of_int n)
  | _ -> None

let opt_bool j k =
  match j |> member k with
  | `Bool b -> Some b
  | `String s -> (
    match String.lowercase_ascii (String.trim s) with
    | "1" | "true" | "yes" | "y" | "on" -> Some true
    | "0" | "false" | "no" | "n" | "off" -> Some false
    | _ -> None)
  | _ -> None

let opt_str_any j keys = List.find_map (opt_str j) keys

let required_str j keys =
  match opt_str_any j keys with
  | Some s when String.trim s <> "" -> Ok s
  | _ -> Error ("missing required field: " ^ String.concat "/" keys)

let images_of_json j =
  match j |> member "images" with
  | `List images ->
    images
    |> List.filter_map (fun image ->
           match opt_str_any image [ "mimeType"; "mime_type" ], opt_str image "data" with
           | Some mime_type, Some data when String.trim mime_type <> "" && String.trim data <> "" ->
             Some (Llm.Image { mime_type; data })
           | _ -> None)
  | _ -> []

let prompt_content ?(prefix = []) j text =
  let images = images_of_json j in
  let body = if String.trim text = "" then images else Llm.Text text :: images in
  prefix @ body

let model_entry_json (e : Models.entry) =
  `Assoc
    [ ("id", `String e.id);
      ("name", `String e.id);
      ("provider", `String e.provider);
      ("baseUrl", `String "");
      ("reasoning", `Bool true);
      ("input", `List [ `String "text" ]);
      ("cost", `Assoc [ ("input", `Int 0); ("output", `Int 0); ("cacheRead", `Int 0); ("cacheWrite", `Int 0) ]);
      ("contextWindow", `Int e.context_window);
      ("maxTokens", `Int 4096) ]

let inferred_model_provider (cfg : Llm.config) =
  match List.find_opt (fun (e : Models.entry) -> e.id = cfg.model) (Models.list ()) with
  | Some e -> e.provider
  | None -> provider_name cfg.provider

let config_model_json ?provider (cfg : Llm.config) =
  let context_window = Option.value (Models.context_window cfg.model) ~default:0 in
  let provider = Option.value provider ~default:(inferred_model_provider cfg) in
  `Assoc
    [ ("id", `String cfg.model);
      ("name", `String cfg.model);
      ("provider", `String provider);
      ("api", `String (provider_name cfg.provider));
      ("baseUrl", `String cfg.base_url);
      ("reasoning", `Bool (cfg.thinking <> "off"));
      ("input", `List [ `String "text" ]);
      ("cost", `Assoc [ ("input", `Int 0); ("output", `Int 0); ("cacheRead", `Int 0); ("cacheWrite", `Int 0) ]);
      ("contextWindow", `Int context_window);
      ("maxTokens", `Int cfg.max_tokens) ]

let session_fields agent =
  match Agent.session agent with
  | Some s ->
    let i = Session.info_of s in
    [ ("sessionFile", `String i.path); ("sessionId", `String i.id) ]
    @ if String.trim i.name = "" then [] else [ ("sessionName", `String i.name) ]
  | None -> [ ("sessionId", `String "ephemeral") ]

let state_json agent =
  let cfg = Agent.config agent in
  `Assoc
    ([ ("model", config_model_json cfg);
       ("thinkingLevel", `String cfg.thinking);
       ("isStreaming", `Bool false);
       ("isCompacting", `Bool false);
       ("steeringMode", `String (Agent.steering_mode agent));
       ("followUpMode", `String (Agent.follow_up_mode agent));
       ("autoCompactionEnabled", `Bool (Agent.auto_compact agent));
       ("autoRetryEnabled", `Bool (Agent.auto_retry agent));
       ("messageCount", `Int (Agent.turn_count agent));
       ("pendingMessageCount", `Int 0);
       ("usage", usage_json agent) ]
    @ session_fields agent)

let session_stats_json agent =
  let user_messages, assistant_messages, tool_calls, tool_results =
    List.fold_left
      (fun (u, a, calls, results) (turn : Llm.turn) ->
        let calls =
          calls
          + List.fold_left
              (fun n -> function Llm.Tool_use _ -> n + 1 | _ -> n)
              0 turn.Llm.content
        in
        let results =
          results
          + List.fold_left
              (fun n -> function Llm.Tool_result _ -> n + 1 | _ -> n)
              0 turn.Llm.content
        in
        match turn.role with
        | User -> (u + 1, a, calls, results)
        | Assistant -> (u, a + 1, calls, results))
      (0, 0, 0, 0) (Agent.turns agent)
  in
  let used, window, pct = Agent.usage_info agent in
  `Assoc
    ([ ("userMessages", `Int user_messages);
       ("assistantMessages", `Int assistant_messages);
       ("toolCalls", `Int tool_calls);
       ("toolResults", `Int tool_results);
       ("totalMessages", `Int (Agent.turn_count agent));
       ( "tokens",
         `Assoc
           [ ("input", `Int used);
             ("output", `Int 0);
             ("cacheRead", `Int 0);
             ("cacheWrite", `Int 0);
             ("total", `Int used) ] );
       ("cost", `Float 0.);
       ("contextUsage", `Assoc [ ("used", `Int used); ("window", `Int window); ("percent", `Float pct) ]) ]
    @ session_fields agent)

let text_of_turn (turn : Llm.turn) =
  String.concat "\n" (List.filter_map (function Llm.Text s -> Some s | _ -> None) turn.content)

let fork_messages_json agent =
  let _, messages =
    List.fold_left
      (fun (i, acc) (turn : Llm.turn) ->
        let next = i + 1 in
        match turn.role with
        | User ->
          let text = text_of_turn turn in
          if String.trim text = "" then (next, acc)
          else
            ( next,
              `Assoc [ ("entryId", `String (string_of_int i)); ("text", `String text) ] :: acc )
        | Assistant -> (next, acc))
      (0, []) (Agent.turns agent)
  in
  `Assoc [ ("messages", `List (List.rev messages)) ]

let source_info source path =
  `Assoc
    [ ("path", `String path);
      ("source", `String source);
      ("scope", `String "temporary");
      ("origin", `String "top-level") ]

let command_json name description source path =
  `Assoc
    [ ("name", `String name);
      ("slashCommand", `String ("/" ^ name));
      ("description", `String description);
      ("source", `String source);
      ("sourceInfo", source_info source path) ]

let commands_json () =
  let extension_tools =
    Tools.extension_names ()
    |> List.filter_map (fun name ->
           match Tools.find name with
           | Some t -> Some (command_json name t.Tools.description "extension" "")
           | None -> None)
  in
  let extension_commands =
    Extensions.command_menu ()
    |> List.map (fun (name, description) ->
           let name =
             if String.length name > 0 && name.[0] = '/' then String.sub name 1 (String.length name - 1)
             else name
           in
           command_json name description "extension_command" "")
  in
  let prompts =
    Prompts.discover ()
    |> List.map (fun (p : Prompts.t) -> command_json p.name p.description "prompt" p.location)
  in
  let skills =
    Skills.discover ()
    |> List.map (fun (s : Skills.t) -> command_json ("skill:" ^ s.name) s.description "skill" s.location)
  in
  `Assoc [ ("commands", `List (extension_tools @ extension_commands @ prompts @ skills)) ]

let prompt_skill_commands_json () =
  let prompts =
    Prompts.discover ()
    |> List.map (fun (p : Prompts.t) -> command_json p.name p.description "prompt" p.location)
  in
  let skills =
    Skills.discover ()
    |> List.map (fun (s : Skills.t) -> command_json ("skill:" ^ s.name) s.description "skill" s.location)
  in
  prompts @ skills

let ui_capture_json (ui : Extensions.ui_capture) =
  `Assoc
    [ ("notifications", `List (List.map (fun text -> `String text) ui.notifications));
      ("requests", `List ui.requests);
      ("surfaces", `List ui.surfaces);
      ("messages", `List ui.messages) ]

let json_string_field name json =
  match json |> member name with
  | `String s -> Some s
  | `Int n -> Some (string_of_int n)
  | `Bool b -> Some (string_of_bool b)
  | _ -> None

let json_int_field name json =
  match json |> member name with
  | `Int n -> Some n
  | `Float f -> Some (int_of_float f)
  | `String s -> int_of_string_opt (String.trim s)
  | _ -> None

let json_string_list_field name json =
  match json |> member name with
  | `List xs -> List.filter_map (function `String s -> Some s | _ -> None) xs
  | _ -> []

let select_option_labels request =
  match request |> member "options" with
  | `List options ->
    options
    |> List.filter_map (fun option ->
           match json_string_field "label" option with
           | Some label -> Some label
           | None -> json_string_field "value" option)
  | _ -> []

let maybe_field name = function Some value -> [ (name, value) ] | None -> []

let timeout_field json =
  maybe_field "timeout" (Option.map (fun n -> `Int n) (json_int_field "timeout" json))

let extension_ui_request_event request =
  let id = Option.value (json_string_field "id" request) ~default:"ui-request" in
  let message = Option.value (json_string_field "message" request) ~default:"" in
  match json_string_field "kind" request with
  | Some "notify" ->
    Some
      (`Assoc
        ([ ("type", `String "extension_ui_request");
           ("id", `String id);
           ("method", `String "notify");
           ("message", `String message) ]
        @ maybe_field "notifyType" (Option.map (fun s -> `String s) (json_string_field "notifyType" request))))
  | Some "confirm" ->
    let options = request |> member "options" in
    Some
      (`Assoc
        ([ ("type", `String "extension_ui_request");
           ("id", `String id);
           ("method", `String "confirm");
           ("title", `String (Option.value (json_string_field "title" request) ~default:message));
           ("message", `String message) ]
        @ timeout_field options))
  | Some "input" ->
    let options = request |> member "options" in
    Some
      (`Assoc
        ([ ("type", `String "extension_ui_request");
           ("id", `String id);
           ("method", `String "input");
           ("title", `String (Option.value (json_string_field "title" request) ~default:message)) ]
        @ maybe_field "placeholder" (Option.map (fun s -> `String s) (json_string_field "placeholder" request))
        @ timeout_field options))
  | Some "select" ->
    let settings = request |> member "settings" in
    Some
      (`Assoc
        ([ ("type", `String "extension_ui_request");
           ("id", `String id);
           ("method", `String "select");
           ("title", `String message);
           ("options", `List (List.map (fun label -> `String label) (select_option_labels request))) ]
        @ timeout_field settings))
  | Some "editor" ->
    Some
      (`Assoc
        [ ("type", `String "extension_ui_request");
          ("id", `String id);
          ("method", `String "editor");
          ("title", `String (Option.value (json_string_field "title" request) ~default:message));
          ("prefill", `String (Option.value (json_string_field "prefill" request) ~default:"")) ])
  | _ -> None

let surface_action surface = Option.value (json_string_field "action" surface) ~default:"set"

let extension_ui_surface_event surface =
  let id = Option.value (json_string_field "id" surface) ~default:"ui-surface" in
  match json_string_field "kind" surface with
  | Some "status" ->
    Some
      (`Assoc
        [ ("type", `String "extension_ui_request");
          ("id", `String id);
          ("method", `String "setStatus");
          ("statusKey", `String (Option.value (json_string_field "key" surface) ~default:""));
          ( "statusText",
            match json_string_field "text" surface with
            | Some text -> `String text
            | None -> `Null ) ])
  | Some "widget" ->
    let options = surface |> member "options" in
    let lines =
      if surface_action surface = "clear" then `Null
      else `List (List.map (fun line -> `String line) (json_string_list_field "lines" surface))
    in
    Some
      (`Assoc
        ([ ("type", `String "extension_ui_request");
           ("id", `String id);
           ("method", `String "setWidget");
           ("widgetKey", `String (Option.value (json_string_field "key" surface) ~default:""));
           ("widgetLines", lines) ]
        @ maybe_field "widgetPlacement" (Option.map (fun s -> `String s) (json_string_field "placement" options))))
  | Some "title" ->
    Some
      (`Assoc
        [ ("type", `String "extension_ui_request");
          ("id", `String id);
          ("method", `String "setTitle");
          ("title", `String (Option.value (json_string_field "title" surface) ~default:"")) ])
  | Some "editor_text" | Some "paste" ->
    Some
      (`Assoc
        [ ("type", `String "extension_ui_request");
          ("id", `String id);
          ("method", `String "set_editor_text");
          ("text", `String (Option.value (json_string_field "text" surface) ~default:"")) ])
  | _ -> None

let emit_extension_ui_events emit (ui : Extensions.ui_capture) =
  List.iter (fun request -> Option.iter emit (extension_ui_request_event request)) ui.requests;
  List.iter (fun surface -> Option.iter emit (extension_ui_surface_event surface)) ui.surfaces;
  List.iter emit ui.messages

let empty_ui : Extensions.ui_capture = { notifications = []; requests = []; surfaces = []; messages = [] }

let execute_slash_command_response_json agent line =
  let line = String.trim line in
  let line = if line = "" || line.[0] = '/' then line else "/" ^ line in
  let themes, theme_name = extension_theme_context () in
  match
    Extensions.execute_command_response ?session_name:(Agent.session_name agent) ~themes ~theme_name
      ~session_context:(extension_session_context agent)
      ~model:(config_model_json (Agent.config agent)) ~models:(Extensions.model_catalog_json ())
      ~commands:(prompt_skill_commands_json ())
      ~context_usage:(extension_context_usage agent)
      ~system_prompt:(Agent.system_prompt agent) ~has_ui:false line
  with
  | Some response ->
    let model_fields =
      match response.Extensions.model_choice with
      | Some choice -> (
        match Agent.apply_extension_model agent choice with
        | Ok cfg -> [ ("model", config_model_json cfg) ]
        | Error msg -> [ ("modelError", `String msg) ])
      | None -> []
    in
    let reload_fields =
      if response.Extensions.reload_requested then begin
        ignore (Extensions.load ~reason:"reload" ());
        Agent.reload_system_prompt agent;
        [ ("reloadRequested", `Bool true) ]
      end
      else []
    in
    let compact_fields =
      match response.Extensions.compact_requests with
      | [] -> []
      | requests ->
        let results =
          List.map
            (fun request -> `Assoc [ ("request", request); ("text", `String (Agent.compact agent)) ])
            requests
        in
        [ ("compactRequests", `List requests); ("compactResults", `List results) ]
    in
    let session_action_fields =
      match response.Extensions.session_actions with
      | [] -> []
      | actions ->
        let results = Commands.apply_extension_session_actions agent actions in
        [ ("sessionActions", `List actions); ("sessionActionResults", `List results) ]
    in
    Option.iter (fun name -> ignore (Agent.set_session_name agent name)) response.Extensions.session_name;
    List.iter (fun entry -> ignore (Agent.append_extension_session_entry agent entry)) response.Extensions.session_entries;
    Option.iter (fun name -> ignore (Themes.set_active_name ~persist:true name)) response.Extensions.theme_name;
    Option.iter (Agent.set_thinking agent) response.Extensions.thinking_level;
    Ok
      ( `Assoc
          ([ ("kind", `String "extension_command");
             ("text", `String response.Extensions.text);
             ("ui", ui_capture_json response.ui) ]
           @ model_fields
           @ reload_fields
           @ compact_fields
           @ session_action_fields
           @
           (if response.Extensions.abort_requested then [ ("abortRequested", `Bool true) ] else [])
           @
           (if response.Extensions.shutdown_requested then [ ("shutdownRequested", `Bool true) ] else [])
           @
           (match response.Extensions.session_name with
            | Some name -> [ ("sessionName", `String name) ]
            | None -> [])
           @
           (if response.Extensions.session_entries = [] then []
            else [ ("sessionEntries", `List response.Extensions.session_entries) ])
           @
           (match response.Extensions.theme_name with
            | Some name -> [ ("themeName", `String name) ]
            | None -> [])
           @
           (match response.Extensions.tools_expanded with
            | Some expanded -> [ ("toolsExpanded", `Bool expanded) ]
            | None -> [])
           @
           match response.Extensions.thinking_level with
           | Some level -> [ ("thinkingLevel", `String level) ]
           | None -> []),
        response.ui )
  | None -> (
    match Prompts.expand_command line with
    | Some prompt ->
      Ok
        ( `Assoc
            [ ("kind", `String "prompt");
              ("text", `String prompt);
              ("ui", ui_capture_json empty_ui) ],
          empty_ui )
    | None -> Error ("Unknown slash command: " ^ line))

let execute_slash_command_json agent line = Result.map fst (execute_slash_command_response_json agent line)

let parse_bash_result raw =
  let prefix = "(exit " in
  let prefix_len = String.length prefix in
  if String.length raw >= prefix_len && String.sub raw 0 prefix_len = prefix then
    match String.index_opt raw ')' with
    | Some end_idx when end_idx >= prefix_len ->
      let code =
        String.sub raw prefix_len (end_idx - prefix_len) |> String.trim |> int_of_string_opt
      in
      let output_start = min (String.length raw) (end_idx + 1) in
      let output =
        if output_start < String.length raw && raw.[output_start] = '\n' then
          String.sub raw (output_start + 1) (String.length raw - output_start - 1)
        else String.sub raw output_start (String.length raw - output_start)
      in
      (code, output)
    | _ -> (None, raw)
  else (Some 0, raw)

let bash_result_json raw =
  let code, output = parse_bash_result raw in
  `Assoc
    ([ ("output", `String output); ("cancelled", `Bool false); ("truncated", `Bool false) ]
    @ match code with Some n -> [ ("exitCode", `Int n) ] | None -> [ ("exitCode", `Null) ])

type session_change =
  | Session_applied
  | Session_cancelled of string

let current_session_info agent = Option.map Session.info_of (Agent.session agent)

let emit_session_shutdown agent reason =
  match current_session_info agent with
  | None -> ()
  | Some s ->
    Extensions.emit_session_shutdown ~reason ~session_file:s.path ~session_id:s.id ~session_name:s.name ()

let adopt_session_with_events agent ~reason ~turns session =
  let previous_session_file = Option.map (fun (s : Session.info) -> s.path) (current_session_info agent) in
  emit_session_shutdown agent reason;
  Agent.adopt_session agent ~turns (Some session);
  let info = Session.info_of session in
  ignore
    (Extensions.emit_session_start ~reason ?previous_session_file ~session_file:info.path ~session_id:info.id
       ~session_name:info.name ())

let before_switch agent ?target_session_file reason =
  let current = current_session_info agent in
  Extensions.emit_session_before_switch ~reason ?target_session_file
    ?current_session_file:(Option.map (fun (s : Session.info) -> s.path) current)
    ?current_session_id:(Option.map (fun (s : Session.info) -> s.id) current)
    ?current_session_name:(Option.map (fun (s : Session.info) -> s.name) current)
    ()

let before_fork agent ?source_session_file ?entry_id ?position reason =
  let current = current_session_info agent in
  Extensions.emit_session_before_fork ~reason ?source_session_file ?entry_id ?position
    ?current_session_file:(Option.map (fun (s : Session.info) -> s.path) current)
    ?current_session_id:(Option.map (fun (s : Session.info) -> s.id) current)
    ?current_session_name:(Option.map (fun (s : Session.info) -> s.name) current)
    ()

let switch_session agent spec =
  match Session.resolve_path spec with
  | None -> Error ("No matching session: " ^ spec)
  | Some path ->
    (match before_switch agent ~target_session_file:path "switch" with
     | Extensions.Session_cancel reason -> Ok (Session_cancelled reason)
     | Extensions.Session_continue ->
    let turns = Session.load_turns path in
    adopt_session_with_events agent ~reason:"switch" ~turns (Session.open_file path);
    Ok Session_applied)

let create_or_fork_session agent parent =
  match parent with
  | Some p when String.trim p <> "" -> (
    match Session.resolve_path p with
    | None -> Error ("No matching parent session: " ^ p)
    | Some source_path -> (
      match before_fork agent ~source_session_file:source_path ~entry_id:p ~position:"source" "fork" with
      | Extensions.Session_cancel reason -> Ok (Session_cancelled reason)
      | Extensions.Session_continue ->
        let turns = Session.load_turns source_path in
        let s = Session.clone_from turns in
        adopt_session_with_events agent ~reason:"fork" ~turns s;
        Ok Session_applied))
  | _ ->
    (match before_switch agent "new" with
     | Extensions.Session_cancel reason -> Ok (Session_cancelled reason)
     | Extensions.Session_continue ->
    let s = Session.create_new () in
    adopt_session_with_events agent ~reason:"new" ~turns:[] s;
    Ok Session_applied)

let set_model agent params =
  let provider = opt_str_any params [ "provider" ] in
  let model = opt_str_any params [ "modelId"; "model" ] in
  match (provider, model) with
  | Some p, m ->
    let cfg = Llm.config_for ?model:m p in
    Agent.set_config agent cfg;
    Ok (config_model_json ~provider:p cfg)
  | None, Some spec ->
    let parsed = Model_spec.parse (Some spec) in
    let cfg =
      match parsed.provider with
      | Some p -> Llm.config_for ?model:parsed.model p
      | None -> { (Agent.config agent) with Llm.model = spec }
    in
    let cfg = match parsed.thinking with Some thinking -> { cfg with Llm.thinking = thinking } | None -> cfg in
    Agent.set_config agent cfg;
    Ok (config_model_json cfg)
  | None, None -> Error "set_model requires provider/modelId or model"

let scope_active () =
  match Sys.getenv_opt "AGENT_SCOPED_MODELS" with
  | Some s when String.trim s <> "" -> true
  | _ -> Settings.string_list "enabledModels" <> []

let available_model_entries () =
  let available_providers =
    Llm.provider_status () |> List.filter_map (fun (name, available) -> if available then Some name else None)
  in
  let scoped = Models.scoped_from_env () in
  let filtered = List.filter (fun (e : Models.entry) -> List.mem e.provider available_providers) scoped in
  if filtered = [] then scoped else filtered

let rotate_after_current agent entries =
  let cfg = Agent.config agent in
  let provider = inferred_model_provider cfg in
  let rec find_index i = function
    | [] -> None
    | (e : Models.entry) :: _ when e.provider = provider && e.id = cfg.model -> Some i
    | _ :: rest -> find_index (i + 1) rest
  in
  match find_index 0 entries with
  | None -> entries
  | Some i ->
    let after =
      entries |> List.mapi (fun idx entry -> (idx, entry)) |> List.filter (fun (idx, _) -> idx > i) |> List.map snd
    in
    let before =
      entries |> List.mapi (fun idx entry -> (idx, entry)) |> List.filter (fun (idx, _) -> idx <= i) |> List.map snd
    in
    after @ before

let config_for_entry agent (entry : Models.entry) =
  match Llm.config_for ~model:entry.id entry.provider with
  | cfg -> Ok cfg
  | exception Llm.Config_error msg ->
    let current = Agent.config agent in
    if entry.provider = inferred_model_provider current then Ok { current with Llm.model = entry.id }
    else Error msg

let cycle_model agent =
  let rec first_configurable = function
    | [] -> None
    | (entry : Models.entry) :: rest -> (
      match config_for_entry agent entry with
      | Ok cfg -> Some (entry, cfg)
      | Error _ -> first_configurable rest)
  in
  match first_configurable (rotate_after_current agent (available_model_entries ())) with
  | None -> None
  | Some (entry, cfg) ->
    Agent.set_config ~source:"cycle" agent cfg;
    Some
      (`Assoc
        [ ("model", config_model_json ~provider:entry.provider cfg);
          ("thinkingLevel", `String cfg.thinking);
          ("isScoped", `Bool (scope_active ())) ])

let cycle_thinking agent =
  let levels = [ "off"; "low"; "medium"; "high" ] in
  let current = (Agent.config agent).thinking in
  let rec next = function
    | [] -> "off"
    | [ _ ] -> List.hd levels
    | x :: y :: _ when x = current -> y
    | _ :: rest -> next rest
  in
  let level = next levels in
  Agent.set_thinking agent level;
  level

let valid_queue_mode = function "all" | "one-at-a-time" -> true | _ -> false

let success emit ?id ?data command = emit (response ?id ?data command)
let failure emit ?id command msg = emit (response_error ?id command msg)

let handle_pi_command ?(prompt_prefix = []) emit agent j =
  let id = opt_str j "id" in
  let command = match opt_str j "type" with Some s -> s | None -> "" in
  let command_for_response = if command = "" then "parse" else command in
  try
    match command with
    | "prompt" | "steer" | "follow_up" -> (
      match required_str j [ "message"; "prompt" ] with
      | Error msg -> failure emit ?id command_for_response msg
      | Ok msg ->
        success emit ?id command_for_response;
        (try
           let final = Agent.send_content agent (prompt_content ~prefix:prompt_prefix j msg) in
           event emit "turn_done" [ ("text", `String final); ("usage", usage_json agent) ]
         with
         | Llm.Api_error e -> error_event emit e
         | e -> error_event emit (Printexc.to_string e)))
    | "send" -> (
      match required_str j [ "message"; "prompt" ] with
      | Error msg -> failure emit ?id command_for_response msg
      | Ok msg ->
        let final = Agent.send_content agent (prompt_content ~prefix:prompt_prefix j msg) in
        event emit "turn_done" [ ("text", `String final); ("usage", usage_json agent) ];
        success emit ?id ~data:(`Assoc [ ("text", `String final); ("usage", usage_json agent) ])
          command_for_response)
    | "abort" -> success emit ?id "abort"
    | "new_session" -> (
      match create_or_fork_session agent (opt_str j "parentSession") with
      | Ok Session_applied -> success emit ?id ~data:(`Assoc [ ("cancelled", `Bool false) ]) "new_session"
      | Ok (Session_cancelled reason) ->
        success emit ?id ~data:(`Assoc [ ("cancelled", `Bool true); ("reason", `String reason) ]) "new_session"
      | Error msg -> failure emit ?id "new_session" msg)
    | "get_state" | "get_status" -> success emit ?id ~data:(state_json agent) command
    | "set_model" -> (
      match set_model agent j with
      | Ok model -> success emit ?id ~data:model "set_model"
      | Error msg -> failure emit ?id "set_model" msg)
    | "cycle_model" -> (
      match cycle_model agent with
      | Some data -> success emit ?id ~data "cycle_model"
      | None -> success emit ?id ~data:`Null "cycle_model")
    | "get_available_models" | "list_models" ->
      let models = Models.scoped_from_env () |> List.map model_entry_json in
      success emit ?id ~data:(`Assoc [ ("models", `List models) ]) command
    | "set_thinking_level" | "set_thinking" -> (
      match required_str j [ "level"; "thinking" ] with
      | Error msg -> failure emit ?id command msg
      | Ok level ->
        Agent.set_thinking agent level;
        success emit ?id command)
    | "cycle_thinking_level" ->
      let level = cycle_thinking agent in
      success emit ?id ~data:(`Assoc [ ("level", `String level) ]) "cycle_thinking_level"
    | "set_steering_mode" -> (
      match required_str j [ "mode" ] with
      | Error msg -> failure emit ?id "set_steering_mode" msg
      | Ok mode when valid_queue_mode mode ->
        Agent.set_steering_mode agent mode;
        success emit ?id "set_steering_mode"
      | Ok mode -> failure emit ?id "set_steering_mode" ("Invalid steering mode: " ^ mode))
    | "set_follow_up_mode" -> (
      match required_str j [ "mode" ] with
      | Error msg -> failure emit ?id "set_follow_up_mode" msg
      | Ok mode when valid_queue_mode mode ->
        Agent.set_follow_up_mode agent mode;
        success emit ?id "set_follow_up_mode"
      | Ok mode -> failure emit ?id "set_follow_up_mode" ("Invalid follow-up mode: " ^ mode))
    | "compact" ->
      let text = Agent.compact agent in
      let compacted =
        not
          (String.starts_with ~prefix:"Nothing" text || String.starts_with ~prefix:"Compaction cancelled" text
         || String.starts_with ~prefix:"Compaction failed" text)
      in
      success emit ?id
        ~data:(`Assoc [ ("text", `String text); ("compacted", `Bool compacted) ])
        "compact"
    | "set_auto_compaction" -> (
      match opt_bool j "enabled" with
      | Some enabled ->
        Agent.set_auto_compact agent enabled;
        success emit ?id "set_auto_compaction"
      | None -> failure emit ?id "set_auto_compaction" "set_auto_compaction requires boolean enabled")
    | "set_auto_retry" -> (
      match opt_bool j "enabled" with
      | Some enabled ->
        Agent.set_auto_retry agent enabled;
        success emit ?id "set_auto_retry"
      | None -> failure emit ?id "set_auto_retry" "set_auto_retry requires boolean enabled")
    | "abort_retry" -> success emit ?id "abort_retry"
    | "bash" | "execute_bash" when not (rpc_bash_allowed ()) ->
      failure emit ?id command "bash is disabled over RPC (set AGENT_RPC_ALLOW_BASH=1 to enable)"
    | "bash" | "execute_bash" -> (
      match required_str j [ "command" ] with
      | Error msg -> failure emit ?id command msg
      | Ok command_text ->
        let exclude =
          Option.value (opt_bool j "excludeFromContext") ~default:false
          || Option.value (opt_bool j "exclude_from_context") ~default:false
        in
        let raw = Agent.run_user_bash ~exclude_from_context:exclude agent command_text in
        success emit ?id ~data:(bash_result_json raw) command)
    | "abort_bash" -> success emit ?id "abort_bash"
    | "get_session_stats" -> success emit ?id ~data:(session_stats_json agent) "get_session_stats"
    | "export_html" ->
      let output =
        match opt_str_any j [ "outputPath"; "path" ] with
        | Some p -> p
        | None ->
          let sid =
            match Agent.session agent with Some s -> (Session.info_of s).Session.id | None -> "session"
          in
          sid ^ ".html"
      in
      Session.export_html
        (match Agent.session agent with
         | Some s -> Session.info_of s
         | None -> { Session.id = "export"; path = output; name = ""; created = 0.; cwd = Sys.getcwd () })
        (Agent.turns agent) output;
      success emit ?id ~data:(`Assoc [ ("path", `String output) ]) "export_html"
    | "switch_session" -> (
      match required_str j [ "sessionPath"; "session"; "path" ] with
      | Error msg -> failure emit ?id "switch_session" msg
      | Ok spec -> (
        match switch_session agent spec with
        | Ok Session_applied -> success emit ?id ~data:(`Assoc [ ("cancelled", `Bool false) ]) "switch_session"
        | Ok (Session_cancelled reason) ->
          success emit ?id ~data:(`Assoc [ ("cancelled", `Bool true); ("reason", `String reason) ]) "switch_session"
        | Error msg -> failure emit ?id "switch_session" msg))
    | "fork" -> (
      let spec = opt_str_any j [ "entryId"; "sessionPath"; "session" ] in
      match Commands.fork agent spec with
      | msg when String.starts_with ~prefix:"No matching" msg -> failure emit ?id "fork" msg
      | msg when Commands.session_cancelled msg ->
        success emit ?id ~data:(`Assoc [ ("text", `String msg); ("cancelled", `Bool true) ]) "fork"
      | msg -> success emit ?id ~data:(`Assoc [ ("text", `String msg); ("cancelled", `Bool false) ]) "fork")
    | "clone" ->
      let msg = Commands.clone agent in
      success emit ?id
        ~data:(`Assoc [ ("text", `String msg); ("cancelled", `Bool (Commands.session_cancelled msg)) ])
        "clone"
    | "get_fork_messages" -> success emit ?id ~data:(fork_messages_json agent) "get_fork_messages"
    | "get_last_assistant_text" ->
      let text = match Commands.last_assistant_text agent with Some s -> `String s | None -> `Null in
      success emit ?id ~data:(`Assoc [ ("text", text) ]) "get_last_assistant_text"
    | "set_session_name" -> (
      match required_str j [ "name" ] with
      | Error msg -> failure emit ?id "set_session_name" msg
      | Ok name ->
        let name = String.trim name in
        if name = "" then failure emit ?id "set_session_name" "Session name cannot be empty"
        else (
          ignore (Commands.name agent name);
          success emit ?id "set_session_name"))
    | "get_messages" ->
      let messages = Agent.turns agent |> List.map Llm.turn_to_json in
      success emit ?id ~data:(`Assoc [ ("messages", `List messages) ]) "get_messages"
    | "get_commands" -> success emit ?id ~data:(commands_json ()) "get_commands"
    | "execute_command" | "run_command" | "slash_command" -> (
      match required_str j [ "command"; "line"; "name" ] with
      | Error msg -> failure emit ?id command msg
      | Ok line -> (
        match execute_slash_command_response_json agent line with
        | Ok (data, ui) ->
          emit_extension_ui_events emit ui;
          success emit ?id ~data command
        | Error msg -> failure emit ?id command msg))
    | "quit" ->
      success emit ?id "quit";
      raise Exit
    | "" -> failure emit ?id "parse" "missing type"
    | other -> failure emit ?id other ("Unknown command: " ^ other)
  with
  | Llm.Api_error e -> failure emit ?id command_for_response e
  | Llm.Config_error e -> failure emit ?id command_for_response e
  | Exit -> raise Exit
  | Sys.Break as e -> raise e
  | e -> failure emit ?id command_for_response (Printexc.to_string e)

let handle_legacy_method ?(prompt_prefix = []) emit agent j =
  match j |> member "method" with
  | `String "send" -> (
    let params = j |> member "params" in
    match opt_str params "message" with
    | None -> error_event emit "send requires params.message"
    | Some msg -> (
      try
        let final = Agent.send_content agent (prompt_content ~prefix:prompt_prefix params msg) in
        event emit "turn_done" [ ("text", `String final); ("usage", usage_json agent) ]
      with
      | Llm.Api_error e -> error_event emit e
      | Sys.Break as e -> raise e
      | e -> error_event emit (Printexc.to_string e)))
  | `String "set_model" -> (
    let params = j |> member "params" in
    try
      let c =
        match opt_str params "provider" with
        | Some p -> Llm.config_for ?model:(opt_str params "model") p
        | None -> (
          match opt_str params "model" with
          | Some m -> { (Agent.config agent) with Llm.model = m }
          | None -> Agent.config agent)
      in
      Agent.set_config agent c;
      event emit "ok" [ ("config", `String (Llm.describe c)) ]
    with
    | Llm.Config_error e -> error_event emit e
    | Sys.Break as e -> raise e)
  | `String "session" ->
    let c = Agent.config agent in
    event emit "ok"
      [ ("config", `String (Llm.describe c)); ("turns", `Int (Agent.turn_count agent)); ("usage", usage_json agent) ]
  | `String "new" ->
    let msg = Commands.new_session agent in
    event emit "ok" [ ("text", `String msg); ("cancelled", `Bool (Commands.session_cancelled msg)) ]
  | `String "quit" -> raise Exit
  | `String other -> error_event emit ("unknown method: " ^ other)
  | _ -> error_event emit "missing method"

let handle_command ?(prompt_prefix = []) emit agent j =
  match j |> member "type" with
  | `String "extension_ui_response" ->
    let id = opt_str j "id" in
    success emit ?id ~data:(`Assoc [ ("accepted", `Bool true); ("interactive", `Bool false) ])
      "extension_ui_response"
  | `String _ -> handle_pi_command ~prompt_prefix emit agent j
  | _ -> handle_legacy_method ~prompt_prefix emit agent j

let handle_command_for_test ?(prompt_prefix = []) agent j =
  let out = ref [] in
  let emit j = out := j :: !out in
  handle_command ~prompt_prefix emit agent j;
  List.rev !out

let run ?(prompt_prefix = []) agent =
  Agent.set_frontend agent (make_frontend emit_stdout);
  let c = Agent.config agent in
  event emit_stdout "ready" [ ("config", `String (Llm.describe c)) ];
  let handle line =
    match Yojson.Safe.from_string line with
    | exception _ -> error_event emit_stdout "invalid JSON"
    | j -> handle_command ~prompt_prefix emit_stdout agent j
  in
  let rec loop () =
    match In_channel.input_line stdin with
    | None -> ()
    | Some line ->
      if String.trim line <> "" then begin
        try handle line with
        | Exit -> raise Exit
        | Sys.Break as e -> raise e
        | e -> error_event emit_stdout (Printexc.to_string e)
      end;
      loop ()
  in
  try loop () with Exit -> ()
