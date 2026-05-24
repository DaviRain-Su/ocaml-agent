(* Public SDK surface of ocaml-agent. See ocaml_agent.ml for rationale.

   [Transport] and [Llm] are curated by hand (the stable SDK core). [Tools],
   [Agent], and [Extension_sdk] are re-exported in full via [module type of]
   since they are larger and still evolving. *)

(* Pluggable HTTP transport. The default [Llm] transport shells out to curl;
   implement this record with a native OCaml HTTP/TLS client and install it via
   [Llm.set_transport] to replace curl without touching protocol code. *)
module Transport : sig
  exception Http_error of string

  type t =
    { post_stream : url:string -> headers:string list -> on_line:(string -> unit) -> Yojson.Safe.t -> unit;
      post_json : url:string -> headers:string list -> Yojson.Safe.t -> Yojson.Safe.t }
end

(* Tool definitions and registries. Declared before [Llm] because a client's
   tool registry ([Llm.client_tools]) has type [Tools.registry]. *)
module Tools : module type of Tools

(* Provider-agnostic LLM layer: normalized conversation types, configuration,
   provider registry, and the streaming [complete] entry point. *)
module Llm : sig
  (* Re-exported with manifest equations (type t = Llm.t = ...) so these are the
     same types/exceptions the rest of the library uses — required for interop
     with [Agent], and so callers can construct/match the constructors. *)
  exception Config_error of string
  exception Api_error of string

  type content = Llm.content =
    | Text of string
    | Image of { mime_type : string; data : string }
    | Thinking of { text : string; signature : string }
    | Tool_use of { id : string; name : string; input : Yojson.Safe.t }
    | Tool_result of { id : string; content : string }

  type role = Llm.role = User | Assistant
  type turn = Llm.turn = { role : role; content : content list }
  type usage = Llm.usage = { input_tokens : int; output_tokens : int }

  val zero_usage : usage

  type provider = Llm.provider = Anthropic | Openai

  type config = Llm.config =
    { provider : provider;
      base_url : string;
      api_key : string;
      model : string;
      max_tokens : int;
      extra_headers : string list;
      runtime : string option;
      thinking : string }

  type runtime_complete = Llm.runtime_complete

  (* Manifest alias (not abstract) so this is the same type the [Agent] module
     consumes — Agent.create ?client expects exactly [Llm.client]. *)
  type client = Llm.client

  val create_client : unit -> client
  val default_client : client

  (* The client's tool registry (e.g. to register tools into a specific client). *)
  val client_tools : client -> Tools.registry

  (* --- pure construction (no env/disk/registry reads) --- *)

  val make_config :
    ?max_tokens:int ->
    ?extra_headers:string list ->
    ?runtime:string ->
    ?thinking:string ->
    provider:provider ->
    base_url:string ->
    api_key:string ->
    model:string ->
    unit ->
    config

  val config_of_known :
    ?client:client ->
    ?model:string ->
    ?thinking:string ->
    ?max_tokens:int ->
    api_key:string ->
    string ->
    config

  (* --- environment/settings convenience layers --- *)

  val config_for : ?client:client -> ?model:string -> string -> config
  val config : ?client:client -> unit -> config
  val describe : config -> string

  (* --- transport selection --- *)

  val set_transport : ?client:client -> Transport.t -> unit
  val transport : ?client:client -> unit -> Transport.t

  (* --- provider registry --- *)

  val register_provider :
    ?client:client ->
    ?aliases:string list ->
    ?headers:string list ->
    ?runtime:string ->
    name:string ->
    protocol:provider ->
    base_url:string ->
    env_keys:string list ->
    default_model:string ->
    unit ->
    unit

  val register_provider_runtime : ?client:client -> string -> runtime_complete -> unit
  val is_known_provider : ?client:client -> string -> bool

  (* --- request/response hooks --- *)

  val set_provider_request_hook : ?client:client -> (Yojson.Safe.t -> Yojson.Safe.t) -> unit
  val set_provider_response_hook :
    ?client:client -> (status:int -> headers:(string * string) list -> unit) -> unit
  val apply_provider_request_hooks : ?client:client -> Yojson.Safe.t -> Yojson.Safe.t
  val emit_provider_response_hooks :
    ?client:client -> status:int -> headers:(string * string) list -> unit -> unit

  (* --- completion --- *)

  val complete :
    ?client:client ->
    config ->
    system:string ->
    ?on_text:(string -> unit) ->
    ?tools_enabled:bool ->
    ?tool_names:string list ->
    ?transport:Transport.t ->
    turn list ->
    content list * usage
end

module Agent : module type of Agent
module Extension_sdk : module type of Extension_sdk
