open Yojson.Safe.Util

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

let json_string_list field json =
  match json |> member field with
  | `List xs -> List.filter_map (function `String s when String.trim s <> "" -> Some s | _ -> None) xs
  | `String s when String.trim s <> "" -> [ s ]
  | _ -> []

let resource_result json =
  match json |> member "result" with
  | `Assoc _ as result -> result
  | _ -> json

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
