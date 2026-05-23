(* Provider-agnostic LLM layer.

   Supports two wire protocols, selected by env var:
   - Anthropic Messages API  (Claude, and Kimi/Moonshot's anthropic endpoint)
   - OpenAI Chat Completions (DeepSeek, Kimi/Moonshot, OpenAI, others)

   The agent works only with the normalized [content]/[turn] types below; each
   provider serializes them to its own wire format and parses responses back. *)

open Yojson.Safe.Util

exception Config_error of string
exception Api_error of string

(* --- normalized conversation types --- *)

type content =
  | Text of string
  | Thinking of { text : string; signature : string } (* Anthropic extended thinking *)
  | Tool_use of { id : string; name : string; input : Yojson.Safe.t }
  | Tool_result of { id : string; content : string }

type role = User | Assistant

type turn = { role : role; content : content list }

(* Token accounting reported by the API. input_tokens approximates the size of
   the whole conversation we sent, so it doubles as a context-usage gauge. *)
type usage = { input_tokens : int; output_tokens : int }

let zero_usage = { input_tokens = 0; output_tokens = 0 }

(* --- configuration from environment --- *)

type provider = Anthropic | Openai

type config =
  { provider : provider;
    base_url : string;
    api_key : string;
    model : string;
    max_tokens : int;
    extra_headers : string list;
    thinking : string (* "off" | "low" | "medium" | "high" *) }

let getenv k =
  match Sys.getenv_opt k with Some v when String.trim v <> "" -> Some v | _ -> None

let getenv_or k d = match getenv k with Some v -> v | None -> d

let first_env keys = List.find_map getenv keys

(* --- built-in provider registry ---
   Each entry supplies the wire protocol, default base URL, the env var(s) that
   hold its API key, and a default model. The list order is also the
   auto-detection priority: with no AGENT_PROVIDER set, the first entry whose key
   env var is present in the environment wins. Model defaults are best-effort and
   can always be overridden with AGENT_MODEL. *)
type known =
  { names : string list; (* accepted AGENT_PROVIDER aliases; head is canonical *)
    protocol : provider;
    base_url : string;
    env_keys : string list;
    default_model : string;
    headers : string list (* extra request headers, e.g. a required User-Agent *) }

let mk ?(headers = []) names protocol base_url env_keys default_model =
  { names; protocol; base_url; env_keys; default_model; headers }

let registry : known list =
  [ mk [ "anthropic"; "claude" ] Anthropic "https://api.anthropic.com" [ "ANTHROPIC_API_KEY" ]
      "claude-opus-4-7";
    mk [ "deepseek" ] Openai "https://api.deepseek.com" [ "DEEPSEEK_API_KEY" ] "deepseek-v4-pro";
    (* Kimi For Coding subscription: dedicated Anthropic-protocol endpoint,
       keyed by KIMI_API_KEY, requires the Kimi CLI User-Agent. *)
    mk [ "kimi"; "kimi-coding"; "kfc" ] Anthropic "https://api.kimi.com/coding" [ "KIMI_API_KEY" ]
      "kimi-for-coding" ~headers:[ "User-Agent: KimiCLI/1.5" ];
    (* Moonshot general API (different product, different key). *)
    mk [ "moonshot" ] Openai "https://api.moonshot.cn/v1" [ "MOONSHOT_API_KEY" ]
      "kimi-k2-0905-preview";
    mk [ "openai" ] Openai "https://api.openai.com/v1" [ "OPENAI_API_KEY" ] "gpt-4o";
    mk [ "openrouter" ] Openai "https://openrouter.ai/api/v1" [ "OPENROUTER_API_KEY" ]
      "openai/gpt-4o";
    mk [ "groq" ] Openai "https://api.groq.com/openai/v1" [ "GROQ_API_KEY" ]
      "llama-3.3-70b-versatile";
    mk [ "xai"; "grok" ] Openai "https://api.x.ai/v1" [ "XAI_API_KEY" ] "grok-2-latest";
    mk [ "mistral" ] Openai "https://api.mistral.ai/v1" [ "MISTRAL_API_KEY" ]
      "mistral-large-latest";
    (* Z.AI GLM Coding Plan: coding-specific OpenAI-compatible endpoint. *)
    mk [ "zai"; "zhipu"; "glm" ] Openai "https://api.z.ai/api/coding/paas/v4"
      [ "ZAI_API_KEY"; "ZHIPU_API_KEY" ] "glm-4.6";
    mk [ "gemini"; "google" ] Openai "https://generativelanguage.googleapis.com/v1beta/openai"
      [ "GEMINI_API_KEY"; "GOOGLE_API_KEY" ] "gemini-2.0-flash" ]

let find_known name =
  let n = String.lowercase_ascii name in
  List.find_opt (fun k -> List.mem n k.names) registry

let all_env_keys = List.concat_map (fun k -> k.env_keys) registry

let read_max_tokens () =
  match getenv "AGENT_MAX_TOKENS" with Some s -> ( try int_of_string s with _ -> 4096) | None -> 4096

let read_thinking () =
  match getenv "AGENT_THINKING" with
  | Some s -> (
    match String.lowercase_ascii (String.trim s) with
    | ("off" | "none" | "") -> "off"
    | ("low" | "minimal") -> "low"
    | "medium" -> "medium"
    | ("high" | "xhigh") -> "high"
    | other -> other)
  | None -> "off"

(* Canonical provider names paired with whether their key env var is set. *)
let provider_status () =
  List.map (fun k -> (List.hd k.names, first_env k.env_keys <> None)) registry

(* Build a config for an explicitly named provider (used by /model switching).
   Uses the registry's own base URL / default model, reads the key from the
   provider's env vars (or AGENT_API_KEY), and lets [model] override the model. *)
let config_for ?model alias : config =
  match find_known alias with
  | None ->
    raise
      (Config_error
         (Printf.sprintf "unknown provider %S; known: %s" alias
            (String.concat ", " (List.map (fun (n, _) -> n) (provider_status ())))))
  | Some k ->
    let api_key =
      match first_env k.env_keys with
      | Some x -> x
      | None -> (
        match getenv "AGENT_API_KEY" with
        | Some x -> x
        | None ->
          raise
            (Config_error
               (Printf.sprintf "no API key for %s; set %s" alias (String.concat " or " k.env_keys))))
    in
    let model = match model with Some m -> m | None -> k.default_model in
    { provider = k.protocol;
      base_url = k.base_url;
      api_key;
      model;
      max_tokens = read_max_tokens ();
      extra_headers = k.headers;
      thinking = read_thinking () }

let config () : config =
  let max_tokens = read_max_tokens () in
  (* Resolve the chosen provider entry (or a generic fallback). *)
  let chosen : [ `Known of known | `Generic of provider ] =
    match getenv "AGENT_PROVIDER" with
    | Some p -> (
      match find_known p with
      | Some k -> `Known k
      | None ->
        (* Unknown alias: treat as a generic endpoint; guess protocol by name. *)
        let proto = match String.lowercase_ascii p with "anthropic" | "claude" -> Anthropic | _ -> Openai in
        `Generic proto)
    | None -> (
      (* Auto-detect: first registry entry whose key env var is present. *)
      match List.find_opt (fun k -> first_env k.env_keys <> None) registry with
      | Some k -> `Known k
      | None -> if getenv "AGENT_API_KEY" <> None then `Generic Openai else `Generic Anthropic)
  in
  let provider, default_base, key_envs, default_model, extra_headers =
    match chosen with
    | `Known k -> (k.protocol, k.base_url, k.env_keys, Some k.default_model, k.headers)
    | `Generic Anthropic ->
      (Anthropic, "https://api.anthropic.com", [ "ANTHROPIC_API_KEY" ], Some "claude-opus-4-7", [])
    | `Generic Openai -> (Openai, "https://api.openai.com/v1", [ "OPENAI_API_KEY" ], None, [])
  in
  let base_url = getenv_or "AGENT_BASE_URL" default_base in
  let api_key =
    match getenv "AGENT_API_KEY" with
    | Some k -> k
    | None -> (
      match first_env key_envs with
      | Some k -> k
      | None ->
        raise
          (Config_error
             (Printf.sprintf
                "no API key found. Set one of these env vars and re-run:\n  %s\nor set AGENT_API_KEY (with AGENT_PROVIDER / AGENT_BASE_URL for a custom endpoint)."
                (String.concat ", " all_env_keys))))
  in
  let model =
    match getenv "AGENT_MODEL" with
    | Some m -> m
    | None -> (
      match default_model with
      | Some m -> m
      | None -> raise (Config_error "AGENT_MODEL must be set for a generic openai endpoint"))
  in
  { provider; base_url; api_key; model; max_tokens; extra_headers; thinking = read_thinking () }

let describe cfg =
  let p = match cfg.provider with Anthropic -> "anthropic" | Openai -> "openai" in
  Printf.sprintf "%s | %s | %s" p cfg.model cfg.base_url

(* Extract the payload of an SSE "data:" line, if this line is one. *)
let sse_data line =
  let n = String.length line in
  if n >= 6 && String.sub line 0 6 = "data: " then Some (String.sub line 6 (n - 6))
  else if n >= 5 && String.sub line 0 5 = "data:" then Some (String.sub line 5 (n - 5))
  else None

(* --- normalized turn <-> JSON (for session persistence) --- *)

let content_to_json = function
  | Text s -> `Assoc [ ("type", `String "text"); ("text", `String s) ]
  | Thinking { text; signature } ->
    `Assoc [ ("type", `String "thinking"); ("text", `String text); ("signature", `String signature) ]
  | Tool_use { id; name; input } ->
    `Assoc
      [ ("type", `String "tool_use"); ("id", `String id); ("name", `String name); ("input", input) ]
  | Tool_result { id; content } ->
    `Assoc [ ("type", `String "tool_result"); ("id", `String id); ("content", `String content) ]

let content_of_json j =
  try
    match j |> member "type" |> to_string with
    | "text" -> Text (j |> member "text" |> to_string)
    | "thinking" ->
      Thinking { text = j |> member "text" |> to_string; signature = j |> member "signature" |> to_string }
    | "tool_use" ->
      Tool_use
        { id = j |> member "id" |> to_string;
          name = j |> member "name" |> to_string;
          input = j |> member "input" }
    | "tool_result" ->
      Tool_result { id = j |> member "id" |> to_string; content = j |> member "content" |> to_string }
    | other -> failwith ("unknown content type: " ^ other)
  with _ -> Text "[unparseable content block]"

let turn_to_json t =
  `Assoc
    [ ("role", `String (match t.role with User -> "user" | Assistant -> "assistant"));
      ("content", `List (List.map content_to_json t.content)) ]

let turn_of_json j =
  try
    let role = match j |> member "role" |> to_string with "assistant" -> Assistant | _ -> User in
    { role; content = j |> member "content" |> to_list |> List.map content_of_json }
  with _ -> { role = User; content = [ Text "[unparseable turn]" ] }

(* --- Anthropic protocol --- *)

(* Thinking budget (Anthropic) / reasoning effort (OpenAI) per level. *)
let thinking_budget = function "low" -> 2048 | "medium" -> 8192 | "high" -> 16384 | _ -> 0

let anthropic_block = function
  | Text s -> `Assoc [ ("type", `String "text"); ("text", `String s) ]
  | Thinking { text; signature } ->
    `Assoc [ ("type", `String "thinking"); ("thinking", `String text); ("signature", `String signature) ]
  | Tool_use { id; name; input } ->
    `Assoc
      [ ("type", `String "tool_use"); ("id", `String id); ("name", `String name); ("input", input) ]
  | Tool_result { id; content } ->
    `Assoc
      [ ("type", `String "tool_result");
        ("tool_use_id", `String id);
        ("content", `String content) ]

let anthropic_role = function User -> "user" | Assistant -> "assistant"

let anthropic_messages turns =
  List.map
    (fun t ->
      `Assoc
        [ ("role", `String (anthropic_role t.role));
          ("content", `List (List.map anthropic_block t.content)) ])
    turns

(* A content block under construction while streaming. *)
type builder =
  | BText of Buffer.t
  | BThinking of { text : Buffer.t; sign : Buffer.t }
  | BTool of { id : string; name : string; json : Buffer.t }

let anthropic_complete cfg ~system ~on_text ~tools_enabled ?tool_names turns : content list * usage =
  let budget = thinking_budget cfg.thinking in
  (* Extended thinking requires max_tokens strictly greater than the budget. *)
  let max_tokens = if budget > 0 && cfg.max_tokens <= budget then budget + 4096 else cfg.max_tokens in
  let tool_schemas = if tools_enabled then Tools.anthropic_schemas ?allowed:tool_names () else [] in
  let body =
    `Assoc
      ([ ("model", `String cfg.model);
         ("max_tokens", `Int max_tokens);
         ("system", `String system);
         ("stream", `Bool true);
         ("messages", `List (anthropic_messages turns)) ]
      @ (if tool_schemas = [] then [] else [ ("tools", `List tool_schemas) ])
      @
      if budget > 0 then
        [ ("thinking", `Assoc [ ("type", `String "enabled"); ("budget_tokens", `Int budget) ]) ]
      else [])
  in
  let url = cfg.base_url ^ "/v1/messages" in
  let headers =
    [ "content-type: application/json";
      "x-api-key: " ^ cfg.api_key;
      "anthropic-version: 2023-06-01" ]
    @ cfg.extra_headers
  in
  (* index -> builder, plus ordered output as blocks complete. *)
  let builders : (int, builder) Hashtbl.t = Hashtbl.create 8 in
  let blocks = ref [] in
  let err = Buffer.create 64 in
  let in_tok = ref 0 and out_tok = ref 0 in
  let read_usage u =
    (match u |> member "input_tokens" with `Int n -> in_tok := n | _ -> ());
    match u |> member "output_tokens" with `Int n -> out_tok := n | _ -> ()
  in
  let finalize idx =
    match Hashtbl.find_opt builders idx with
    | None -> ()
    | Some (BText b) -> blocks := Text (Buffer.contents b) :: !blocks
    | Some (BThinking { text; sign }) ->
      blocks := Thinking { text = Buffer.contents text; signature = Buffer.contents sign } :: !blocks
    | Some (BTool { id; name; json }) ->
      let input =
        let s = Buffer.contents json in
        match Yojson.Safe.from_string (if s = "" then "{}" else s) with
        | j -> j
        | exception _ -> `Assoc []
      in
      blocks := Tool_use { id; name; input } :: !blocks
  in
  let handle data =
    try
      match Yojson.Safe.from_string data with
      | exception _ -> ()
      | j -> (
        match j |> member "type" |> to_string with
        | "error" -> raise (Api_error data)
        | "message_start" -> read_usage (j |> member "message" |> member "usage")
        | "message_delta" -> read_usage (j |> member "usage")
        | "content_block_start" ->
          let idx = j |> member "index" |> to_int in
          let cb = j |> member "content_block" in
          (match cb |> member "type" |> to_string with
           | "text" -> Hashtbl.replace builders idx (BText (Buffer.create 256))
           | "thinking" ->
             Hashtbl.replace builders idx (BThinking { text = Buffer.create 256; sign = Buffer.create 64 })
           | "tool_use" ->
             Hashtbl.replace builders idx
               (BTool
                  { id = cb |> member "id" |> to_string;
                    name = cb |> member "name" |> to_string;
                    json = Buffer.create 128 })
           | _ -> ())
        | "content_block_delta" -> (
          let idx = j |> member "index" |> to_int in
          let d = j |> member "delta" in
          match (d |> member "type" |> to_string, Hashtbl.find_opt builders idx) with
          | "text_delta", Some (BText b) ->
            let t = d |> member "text" |> to_string in
            Buffer.add_string b t;
            on_text t
          | "input_json_delta", Some (BTool { json; _ }) ->
            Buffer.add_string json (d |> member "partial_json" |> to_string)
          | "thinking_delta", Some (BThinking { text; _ }) ->
            Buffer.add_string text (d |> member "thinking" |> to_string)
          | "signature_delta", Some (BThinking { sign; _ }) ->
            Buffer.add_string sign (d |> member "signature" |> to_string)
          | _ -> ())
        | "content_block_stop" -> finalize (j |> member "index" |> to_int)
        | _ -> ())
    with _ -> ()
  in
  (try
     Http.post_stream ~url ~headers body ~on_line:(fun line ->
         match sse_data line with
         | Some data when data <> "[DONE]" -> handle data
         | Some _ -> ()
         | None -> if String.trim line <> "" then Buffer.add_string err (line ^ "\n"))
   with Http.Http_error e ->
     let msg = if Buffer.length err > 0 then e ^ "\n" ^ Buffer.contents err else e in
     raise (Api_error msg));
  if !blocks = [] && Buffer.length err > 0 then raise (Api_error (Buffer.contents err));
  (List.rev !blocks, { input_tokens = !in_tok; output_tokens = !out_tok })

(* --- OpenAI protocol --- *)

let openai_messages ~system turns =
  let sys_msg = `Assoc [ ("role", `String "system"); ("content", `String system) ] in
  let of_turn t =
    match t.role with
    | User ->
      List.map
        (fun c ->
          match c with
          | Text s -> `Assoc [ ("role", `String "user"); ("content", `String s) ]
          | Tool_result { id; content } ->
            `Assoc
              [ ("role", `String "tool");
                ("tool_call_id", `String id);
                ("content", `String content) ]
          | Thinking _ | Tool_use _ -> `Null (* not represented in OpenAI user turns *))
        t.content
      |> List.filter (fun j -> j <> `Null)
    | Assistant ->
      let text =
        t.content
        |> List.filter_map (function Text s -> Some s | _ -> None)
        |> String.concat "\n"
      in
      let tool_calls =
        t.content
        |> List.filter_map (function
             | Tool_use { id; name; input } ->
               Some
                 (`Assoc
                    [ ("id", `String id);
                      ("type", `String "function");
                      ( "function",
                        `Assoc
                          [ ("name", `String name);
                            ("arguments", `String (Yojson.Safe.to_string input)) ] ) ])
             | _ -> None)
      in
      let content_field = if text = "" then `Null else `String text in
      let fields = [ ("role", `String "assistant"); ("content", content_field) ] in
      let fields = if tool_calls = [] then fields else fields @ [ ("tool_calls", `List tool_calls) ] in
      [ `Assoc fields ]
  in
  sys_msg :: List.concat_map of_turn turns

let openai_complete cfg ~system ~on_text ~tools_enabled ?tool_names turns : content list * usage =
  let tool_schemas = if tools_enabled then Tools.openai_schemas ?allowed:tool_names () else [] in
  let body =
    `Assoc
      ([ ("model", `String cfg.model);
         ("max_tokens", `Int cfg.max_tokens);
         ("stream", `Bool true);
         ("stream_options", `Assoc [ ("include_usage", `Bool true) ]);
         ("messages", `List (openai_messages ~system turns)) ]
      @ (if cfg.thinking <> "off" then [ ("reasoning_effort", `String cfg.thinking) ] else [])
      @
      if tool_schemas <> [] then
        [ ("tools", `List tool_schemas); ("tool_choice", `String "auto") ]
      else [])
  in
  let url = cfg.base_url ^ "/chat/completions" in
  let headers =
    [ "content-type: application/json"; "Authorization: Bearer " ^ cfg.api_key ] @ cfg.extra_headers
  in
  let text = Buffer.create 256 in
  (* tool calls accumulate by streamed index *)
  let tools : (int, string ref * string ref * Buffer.t) Hashtbl.t = Hashtbl.create 4 in
  let order = ref [] in
  let err = Buffer.create 64 in
  let in_tok = ref 0 and out_tok = ref 0 in
  let get_tool idx =
    match Hashtbl.find_opt tools idx with
    | Some t -> t
    | None ->
      let t = (ref "", ref "", Buffer.create 128) in
      Hashtbl.replace tools idx t;
      order := idx :: !order;
      t
  in
  let handle data =
    try
      match Yojson.Safe.from_string data with
      | exception _ -> ()
      | j -> (
        match j |> member "error" with
        | `Null -> (
          (match j |> member "usage" with
           | `Null -> ()
           | u ->
             (match u |> member "prompt_tokens" with `Int n -> in_tok := n | _ -> ());
             (match u |> member "completion_tokens" with `Int n -> out_tok := n | _ -> ()));
          match j |> member "choices" |> to_list with
          | choice :: _ -> (
            let d = choice |> member "delta" in
            (match d |> member "content" with
             | `String s when s <> "" ->
               Buffer.add_string text s;
               on_text s
             | _ -> ());
            match d |> member "tool_calls" with
            | `Null -> ()
            | calls ->
              calls |> to_list
              |> List.iter (fun c ->
                     let idx = match c |> member "index" with `Int i -> i | _ -> 0 in
                     let id, name, args = get_tool idx in
                     (match c |> member "id" with `String s when s <> "" -> id := s | _ -> ());
                     let fn = c |> member "function" in
                     (match fn |> member "name" with `String s when s <> "" -> name := s | _ -> ());
                     match fn |> member "arguments" with
                     | `String s -> Buffer.add_string args s
                     | _ -> ()))
          | [] -> ())
        | _ -> raise (Api_error data))
    with _ -> ()
  in
  (try
     Http.post_stream ~url ~headers body ~on_line:(fun line ->
         match sse_data line with
         | Some data when data <> "[DONE]" -> handle data
         | Some _ -> ()
         | None -> if String.trim line <> "" then Buffer.add_string err (line ^ "\n"))
   with Http.Http_error e ->
     let msg = if Buffer.length err > 0 then e ^ "\n" ^ Buffer.contents err else e in
     raise (Api_error msg));
  let text_blocks =
    if Buffer.length text > 0 then [ Text (Buffer.contents text) ] else []
  in
  let tool_blocks =
    List.rev !order
    |> List.map (fun idx ->
           let id, name, args = Hashtbl.find tools idx in
           let s = Buffer.contents args in
           let input =
             match Yojson.Safe.from_string (if s = "" then "{}" else s) with
             | j -> j
             | exception _ -> `Assoc []
           in
           Tool_use { id = !id; name = !name; input })
  in
  if text_blocks = [] && tool_blocks = [] && Buffer.length err > 0 then
    raise (Api_error (Buffer.contents err));
  (text_blocks @ tool_blocks, { input_tokens = !in_tok; output_tokens = !out_tok })

(* --- dispatch --- *)

let complete cfg ~system ?(on_text = fun _ -> ()) ?(tools_enabled = true) ?tool_names turns : content list * usage =
  match cfg.provider with
  | Anthropic -> anthropic_complete cfg ~system ~on_text ~tools_enabled ?tool_names turns
  | Openai -> openai_complete cfg ~system ~on_text ~tools_enabled ?tool_names turns
