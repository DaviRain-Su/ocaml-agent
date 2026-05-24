(* HTTP transport abstraction for the LLM layer.

   The provider-specific code in [Llm] only needs to POST a JSON body and
   either stream back response lines (SSE) or read a single JSON reply. Putting
   that behind this record lets the default curl-based [Http] implementation be
   swapped for a native OCaml HTTP/TLS client (piaf, cohttp-eio, ...) without
   touching any protocol code. A transport implementation signals transport-level
   failures (non-2xx, connection errors) by raising [Http_error]. *)

exception Http_error of string

type t =
  { post_stream : url:string -> headers:string list -> on_line:(string -> unit) -> Yojson.Safe.t -> unit;
    post_json : url:string -> headers:string list -> Yojson.Safe.t -> Yojson.Safe.t }
