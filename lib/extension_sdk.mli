type tool
type command
type provider

val empty_parameters : Yojson.Safe.t
val ok : (string * Yojson.Safe.t) list -> Yojson.Safe.t
val error : string -> Yojson.Safe.t

val register_tool :
  ?parameters:Yojson.Safe.t ->
  name:string ->
  description:string ->
  execute:(Yojson.Safe.t -> string) ->
  unit ->
  unit

val register_tool_response :
  ?parameters:Yojson.Safe.t ->
  name:string ->
  description:string ->
  execute:(Yojson.Safe.t -> Yojson.Safe.t) ->
  unit ->
  unit

val register_command :
  ?argument_hint:string ->
  ?complete:(string -> string list) ->
  name:string ->
  description:string ->
  handler:(string -> string) ->
  unit ->
  unit

val register_command_response :
  ?argument_hint:string ->
  ?complete:(string -> string list) ->
  name:string ->
  description:string ->
  handler:(string -> Yojson.Safe.t) ->
  unit ->
  unit

val register_provider :
  ?aliases:string list ->
  ?base_url:string ->
  ?env_keys:string list ->
  ?models:Yojson.Safe.t list ->
  ?protocol:string ->
  name:string ->
  default_model:string ->
  complete:(Yojson.Safe.t -> Yojson.Safe.t) ->
  unit ->
  unit

val on : string -> (Yojson.Safe.t -> Yojson.Safe.t option) -> unit

val ui :
  ?notifications:string list ->
  ?requests:Yojson.Safe.t list ->
  ?surfaces:Yojson.Safe.t list ->
  ?messages:Yojson.Safe.t list ->
  unit ->
  Yojson.Safe.t

val response : ?ui:Yojson.Safe.t -> ?extra:(string * Yojson.Safe.t) list -> string -> Yojson.Safe.t
val run : unit -> unit

(* In-process API: mount registered tools without the stdin/stdout protocol. *)

(* (name, description, parameters) of each registered tool. *)
val tool_specs : unit -> (string * string * Yojson.Safe.t) list

(* Run a registered tool by name; returns {ok;text} or {ok:false;error} JSON. *)
val invoke_tool : string -> Yojson.Safe.t -> Yojson.Safe.t
