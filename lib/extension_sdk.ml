open Yojson.Safe.Util

type tool =
  { name : string;
    description : string;
    parameters : Yojson.Safe.t;
    execute : Yojson.Safe.t -> Yojson.Safe.t }

type command =
  { name : string;
    description : string;
    argument_hint : string option;
    complete : (string -> string list) option;
    handler : string -> Yojson.Safe.t }

type provider =
  { provider_name : string;
    aliases : string list;
    protocol : string;
    base_url : string option;
    env_keys : string list;
    default_model : string;
    models : Yojson.Safe.t list;
    complete : Yojson.Safe.t -> Yojson.Safe.t }

let tools : tool list ref = ref []
let commands : command list ref = ref []
let providers : provider list ref = ref []
let hooks : (string * (Yojson.Safe.t -> Yojson.Safe.t option)) list ref = ref []

let empty_parameters = `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ]

let ok fields = `Assoc (("ok", `Bool true) :: fields)
let error message = `Assoc [ ("ok", `Bool false); ("error", `String message) ]

let trim_slash name =
  let name = String.trim name in
  if String.length name > 0 && name.[0] = '/' then String.sub name 1 (String.length name - 1) else name

let register_tool ?(parameters = empty_parameters) ~name ~description ~execute () =
  let name = trim_slash name in
  if name <> "" then
    tools :=
      { name; description; parameters; execute = (fun input -> ok [ ("text", `String (execute input)) ]) }
      :: List.filter (fun (tool : tool) -> tool.name <> name) !tools

let register_tool_response ?(parameters = empty_parameters) ~name ~description ~execute () =
  let name = trim_slash name in
  if name <> "" then
    tools := { name; description; parameters; execute } :: List.filter (fun (tool : tool) -> tool.name <> name) !tools

let register_command ?argument_hint ?complete ~name ~description ~handler () =
  let name = trim_slash name in
  if name <> "" then
    commands :=
      { name; description; argument_hint; complete; handler = (fun args -> ok [ ("text", `String (handler args)) ]) }
      :: List.filter (fun (command : command) -> command.name <> name) !commands

let register_command_response ?argument_hint ?complete ~name ~description ~handler () =
  let name = trim_slash name in
  if name <> "" then
    commands :=
      { name; description; argument_hint; complete; handler }
      :: List.filter (fun (command : command) -> command.name <> name) !commands

let register_provider ?(aliases = []) ?base_url ?(env_keys = []) ?(models = []) ?(protocol = "openai")
    ~name ~default_model ~complete () =
  let provider_name = String.trim name in
  if provider_name <> "" && String.trim default_model <> "" then
    providers :=
      { provider_name;
        aliases;
        protocol;
        base_url;
        env_keys;
        default_model;
        models;
        complete }
      :: List.filter (fun provider -> provider.provider_name <> provider_name) !providers

let on event handler =
  let event = String.trim event in
  if event <> "" then hooks := (event, handler) :: !hooks

let read_stdin () =
  let chunks = ref [] in
  (try
     while true do
       chunks := input_line stdin :: !chunks
     done
   with End_of_file -> ());
  String.concat "\n" (List.rev !chunks)

let tool_json (tool : tool) =
  `Assoc
    [ ("name", `String tool.name);
      ("description", `String tool.description);
      ("parameters", tool.parameters) ]

let command_json (command : command) =
  `Assoc
    ([ ("name", `String command.name);
       ("slashCommand", `String ("/" ^ command.name));
       ("description", `String command.description);
       ("hasArgumentCompletions", `Bool (Option.is_some command.complete)) ]
    @
    match command.argument_hint with
    | Some hint when String.trim hint <> "" -> [ ("argumentHint", `String hint) ]
    | _ -> [])

let provider_json (provider : provider) =
  `Assoc
    ([ ("name", `String provider.provider_name);
       ("aliases", `List (List.map (fun alias -> `String alias) provider.aliases));
       ("protocol", `String provider.protocol);
       ("defaultModel", `String provider.default_model);
       ("models", `List provider.models);
       ("hasRuntime", `Bool true) ]
    @ (match provider.base_url with Some url -> [ ("baseUrl", `String url) ] | None -> [])
    @
    match provider.env_keys with
    | [] -> []
    | keys -> [ ("envKeys", `List (List.map (fun key -> `String key) keys)) ])

let registered_events () =
  !hooks |> List.map fst |> List.sort_uniq String.compare

let ui ?(notifications = []) ?(requests = []) ?(surfaces = []) ?(messages = []) () =
  `Assoc
    [ ("notifications", `List (List.map (fun text -> `String text) notifications));
      ("requests", `List requests);
      ("surfaces", `List surfaces);
      ("messages", `List messages) ]

let response ?ui ?(extra = []) text =
  ok
    ((("text", `String text)
      ::
      (match ui with
       | Some ui -> [ ("ui", ui) ]
       | None -> []))
    @ extra)

let describe () =
  ok
    [ ("tools", `List (List.rev_map tool_json !tools));
      ("commands", `List (List.rev_map command_json !commands));
      ("providers", `List (List.rev_map provider_json !providers));
      ("events", `List (List.map (fun event -> `String event) (registered_events ()))) ]

let execute_tool request =
  match request |> member "tool" with
  | `String name -> (
    let name = trim_slash name in
    match List.find_opt (fun (tool : tool) -> tool.name = name) !tools with
    | None -> error ("tool not registered: " ^ name)
    | Some tool ->
      let input =
        match request |> member "input" with
        | `Null -> `Assoc []
        | json -> json
      in
      (try tool.execute input with e -> error (Printexc.to_string e)))
  | _ -> error "missing tool"

let execute_command request =
  match request |> member "command" with
  | `String name -> (
    let name = trim_slash name in
    match List.find_opt (fun (command : command) -> command.name = name) !commands with
    | None -> error ("command not registered: " ^ name)
    | Some command ->
      let args =
        match request |> member "args" with
        | `String s -> s
        | _ -> ""
      in
      (try command.handler args with e -> error (Printexc.to_string e)))
  | _ -> error "missing command"

let command_completions request =
  match request |> member "command" with
  | `String name -> (
    let name = trim_slash name in
    match List.find_opt (fun (command : command) -> command.name = name) !commands with
    | Some { complete = Some complete; _ } ->
      let prefix =
        match request |> member "prefix" with
        | `String s -> s
        | _ -> ""
      in
      let items =
        complete prefix
        |> List.map (fun value -> `Assoc [ ("value", `String value); ("label", `String value) ])
      in
      ok [ ("items", `List items) ]
    | Some _ | None -> ok [ ("items", `List []) ])
  | _ -> error "missing command"

let execute_provider request =
  match request |> member "provider" with
  | `String name -> (
    let normalized = String.lowercase_ascii (String.trim name) in
    match
      List.find_opt
        (fun provider ->
          String.lowercase_ascii provider.provider_name = normalized
          || List.exists (fun alias -> String.lowercase_ascii alias = normalized) provider.aliases)
        !providers
    with
    | None -> error ("provider not registered: " ^ name)
    | Some provider -> (
      try provider.complete request with e -> error (Printexc.to_string e)))
  | _ -> error "missing provider"

let execute_event request =
  let event_name =
    match request |> member "event" with
    | `String event -> event
    | _ -> ""
  in
  let event_payload =
    match request |> member "payload" with
    | `Null -> `Assoc []
    | payload -> payload
  in
  let current_event = ref event_payload in
  let last_result = ref `Null in
  !hooks
  |> List.rev
  |> List.iter (fun (registered, handler) ->
         if registered = event_name then
           match handler !current_event with
           | None -> ()
           | Some result ->
             last_result := result;
             (match event_name, result with
              | "before_provider_request", replacement -> current_event := replacement
              | "input", `Assoc _ as obj when member "action" obj = `String "transform" -> (
                match member "text" obj with
                | `String text -> current_event := `Assoc [ ("type", `String "input"); ("text", `String text) ]
                | _ -> ())
              | _ -> ()));
  ok [ ("event", !current_event); ("result", !last_result) ]

let handle request =
  match request |> member "mode" with
  | `String "describe" -> describe ()
  | `String "execute" -> execute_tool request
  | `String "command" -> execute_command request
  | `String "command_completions" -> command_completions request
  | `String "provider" -> execute_provider request
  | `String "event" -> execute_event request
  | `String mode -> error ("unknown mode: " ^ mode)
  | _ -> error "missing mode"

let run () =
  let response =
    match String.trim (read_stdin ()) with
    | "" -> error "empty request"
    | raw -> (
      match Yojson.Safe.from_string raw with
      | request -> handle request
      | exception e -> error (Printexc.to_string e))
  in
  print_string (Yojson.Safe.to_string response);
  flush stdout
