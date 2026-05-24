(* Shared HTTP helper: POST a JSON body with headers via `curl`, return parsed
   JSON. Shelling out to curl keeps the project free of a TLS/HTTP stack. *)

(* Alias the shared transport error so callers can catch either name and a
   native transport can raise the same exception. *)
exception Http_error = Transport.Http_error

let max_http_response_bytes = 50 * 1024 * 1024

let read_all ic =
  let buf = Buffer.create 4096 in
  let max = max_http_response_bytes in
  (try
     while Buffer.length buf < max do
       Buffer.add_channel buf ic (min 4096 (max - Buffer.length buf))
     done
   with End_of_file -> ());
  Buffer.contents buf

(* Determine if an HTTP error is retryable (5xx, timeout, connection error)
   or not (4xx client errors). We parse the curl exit code from the error
   message if present, or check for common client error indicators. *)
let is_retryable_error msg =
  (* Check for HTTP 4xx status codes in the message - these are client errors
     and should NOT be retried. *)
  let client_error_re = Str.regexp "HTTP/[0-9.]+ 4[0-9][0-9]" in
  try
    ignore (Str.search_forward client_error_re msg 0);
    false
  with Not_found ->
    (* Check for curl exit code 22 (HTTP 4xx returned with --fail) *)
    if Str.string_match (Str.regexp ".*exited 22.*") msg 0 then false
    else true

(* Retry with exponential backoff for transient HTTP errors only.
   Client errors (4xx) are not retried. *)
let rec with_retry ~retries ~delay f =
  try f () with
  | Http_error msg when retries > 0 && is_retryable_error msg ->
    Unix.sleepf delay;
    with_retry ~retries:(retries - 1) ~delay:(delay *. 2.0) f
  | Http_error _ as e -> raise e

(* Escape a string for use inside a double-quoted value in a curl config file.
   curl interprets backslash escapes (backslash, double-quote, t, n, r) inside
   double-quoted values. *)
let curl_config_escape s =
  let b = Buffer.create (String.length s + 2) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '"' -> Buffer.add_string b "\\\""
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* Write request headers to a curl --config file (mode 0600) rather than passing
   them as -H args, so API keys in headers aren't visible in `ps`. *)
let write_curl_config headers =
  let path = Filename.temp_file "agent_curlcfg" ".conf" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      List.iter (fun h -> Printf.fprintf oc "header = \"%s\"\n" (curl_config_escape h)) headers);
  path

(* Run [cmd] capturing stdout, always reaping the child even on exception. *)
let run_capture cmd =
  let ic = Unix.open_process_in cmd in
  let out, exn =
    try (read_all ic, None) with e -> ("", Some e)
  in
  let status = try Unix.close_process_in ic with Sys.Break as e -> raise e | _ -> Unix.WEXITED 1 in
  match exn with Some e -> raise e | None -> (out, status)

(* POST [body] as JSON to [url] with the given [headers] (each "Key: Value"). *)
let post_json ~url ~(headers : string list) (body : Yojson.Safe.t) : Yojson.Safe.t =
  let once () =
    let tmp = Filename.temp_file "agent_req" ".json" in
    let cfg = write_curl_config headers in
    Fun.protect
      ~finally:(fun () ->
        (try Sys.remove tmp with Sys_error _ -> ());
        (try Sys.remove cfg with Sys_error _ -> ()))
      (fun () ->
        let oc = open_out tmp in
        Yojson.Safe.to_channel oc body;
        close_out oc;
        let cmd =
          Printf.sprintf "curl -sS --fail-with-body --config %s %s --data-binary @%s"
            (Filename.quote cfg) (Filename.quote url) (Filename.quote tmp)
        in
        let out, status = run_capture cmd in
        (match status with
         | Unix.WEXITED 0 -> ()
         | Unix.WEXITED c -> raise (Http_error (Printf.sprintf "curl exited %d: %s" c out))
         | Unix.WSIGNALED s | Unix.WSTOPPED s ->
           raise (Http_error (Printf.sprintf "curl killed by signal %d" s)));
        match Yojson.Safe.from_string out with
        | json -> json
        | exception _ -> raise (Http_error ("invalid JSON from server: " ^ out)))
  in
  with_retry ~retries:3 ~delay:0.5 once

(* Stream a POST: invoke [on_line] for each line curl emits (server-sent events),
   as it arrives. We deliberately omit --fail so that HTTP error bodies are still
   delivered to [on_line] for the caller to detect.

   NOTE: we do NOT retry here because callers accumulate mutable state (text
   buffers, tool-call tables) while streaming; a retry would append duplicate
   state. Retry belongs at the non-streaming post_json level or above. *)
let post_stream ~url ~(headers : string list) ~(on_line : string -> unit) (body : Yojson.Safe.t) : unit =
  let tmp = Filename.temp_file "agent_req" ".json" in
  let cfg = write_curl_config headers in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove tmp with Sys_error _ -> ());
      (try Sys.remove cfg with Sys_error _ -> ()))
    (fun () ->
      let oc = open_out tmp in
      Yojson.Safe.to_channel oc body;
      close_out oc;
      let cmd =
        Printf.sprintf "curl -sS -N --no-buffer --config %s %s --data-binary @%s"
          (Filename.quote cfg) (Filename.quote url) (Filename.quote tmp)
      in
      let ic = Unix.open_process_in cmd in
      (* Always reap the child, even if [on_line] raises (e.g. a streamed
         Api_error), so we never leak the curl process. *)
      let exn =
        try
          (try
             while true do
               on_line (input_line ic)
             done
           with End_of_file -> ());
          None
        with e -> Some e
      in
      let status = try Unix.close_process_in ic with Sys.Break as e -> raise e | _ -> Unix.WEXITED 1 in
      match exn with
      | Some e -> raise e
      | None -> (
        match status with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED c -> raise (Http_error (Printf.sprintf "curl exited %d" c))
        | Unix.WSIGNALED s | Unix.WSTOPPED s ->
          raise (Http_error (Printf.sprintf "curl killed by signal %d" s))))

(* This module packaged as a [Transport.t], the default LLM transport. *)
let transport : Transport.t = { Transport.post_stream; post_json }
