(* Shared HTTP helper: POST a JSON body with headers via `curl`, return parsed
   JSON. Shelling out to curl keeps the project free of a TLS/HTTP stack. *)

exception Http_error of string

let read_all ic =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 4096
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

(* POST [body] as JSON to [url] with the given [headers] (each "Key: Value"). *)
let post_json ~url ~(headers : string list) (body : Yojson.Safe.t) : Yojson.Safe.t =
  let once () =
    let tmp = Filename.temp_file "agent_req" ".json" in
    Fun.protect
      ~finally:(fun () -> (try Sys.remove tmp with Sys_error _ -> ()))
      (fun () ->
        let oc = open_out tmp in
        Yojson.Safe.to_channel oc body;
        close_out oc;
        let header_args =
          headers
          |> List.map (fun h -> Printf.sprintf "-H %s" (Filename.quote h))
          |> String.concat " "
        in
        let cmd =
          Printf.sprintf "curl -sS --fail-with-body %s %s --data-binary @%s"
            (Filename.quote url) header_args (Filename.quote tmp)
        in
        let ic = Unix.open_process_in cmd in
        let out = read_all ic in
        let status = Unix.close_process_in ic in
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
   delivered to [on_line] for the caller to detect. *)
let post_stream ~url ~(headers : string list) ~(on_line : string -> unit) (body : Yojson.Safe.t) : unit =
  let once () =
    let tmp = Filename.temp_file "agent_req" ".json" in
    Fun.protect
      ~finally:(fun () -> (try Sys.remove tmp with Sys_error _ -> ()))
      (fun () ->
        let oc = open_out tmp in
        Yojson.Safe.to_channel oc body;
        close_out oc;
        let header_args =
          headers
          |> List.map (fun h -> Printf.sprintf "-H %s" (Filename.quote h))
          |> String.concat " "
        in
        let cmd =
          Printf.sprintf "curl -sS -N --no-buffer %s %s --data-binary @%s"
            (Filename.quote url) header_args (Filename.quote tmp)
        in
        let ic = Unix.open_process_in cmd in
        (try
           while true do
             on_line (input_line ic)
           done
         with End_of_file -> ());
        let status = Unix.close_process_in ic in
        match status with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED c -> raise (Http_error (Printf.sprintf "curl exited %d" c))
        | Unix.WSIGNALED s | Unix.WSTOPPED s ->
          raise (Http_error (Printf.sprintf "curl killed by signal %d" s)))
  in
  with_retry ~retries:3 ~delay:0.5 once
