(* Extension loading: register custom tools declared in a JSON manifest. Each
   tool runs an external command, receiving the tool input as JSON on stdin and
   returning its stdout (and stderr) as the result. This is the lightweight,
   language-agnostic extension mechanism — no in-process plugin runtime.

   Manifest (default .ocaml-agent/tools.json, or AGENT_TOOLS_FILE):
     { "tools": [
         { "name": "weather",
           "description": "Get weather for a city.",
           "parameters": { "type":"object",
                           "properties": { "city": {"type":"string"} },
                           "required": ["city"] },
           "command": "python3 ./ext/weather.py" } ] } *)

open Yojson.Safe.Util

let manifest_path () =
  match Sys.getenv_opt "AGENT_TOOLS_FILE" with
  | Some p when String.trim p <> "" -> p
  | _ -> ".ocaml-agent/tools.json"

(* Run [command], feeding [input] on stdin, returning combined stdout+stderr. *)
let run_command command input =
  let ic, oc = Unix.open_process (command ^ " 2>&1") in
  output_string oc input;
  close_out oc;
  let buf = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_channel buf ic 4096
     done
   with End_of_file -> ());
  let status = Unix.close_process (ic, oc) in
  let body = Buffer.contents buf in
  match status with
  | Unix.WEXITED 0 -> body
  | Unix.WEXITED c -> Printf.sprintf "(exit %d)\n%s" c body
  | Unix.WSIGNALED s | Unix.WSTOPPED s -> Printf.sprintf "(killed by signal %d)\n%s" s body

let tool_of_json (j : Yojson.Safe.t) : Tools.tool option =
  match (j |> member "name", j |> member "command") with
  | `String name, `String command when name <> "" && command <> "" ->
    let description = match j |> member "description" with `String s -> s | _ -> "" in
    let parameters =
      match j |> member "parameters" with
      | `Null -> `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ]
      | p -> p
    in
    Some
      { Tools.name;
        description;
        parameters;
        execute = (fun input -> try run_command command (Yojson.Safe.to_string input) with
        | Sys.Break as e -> raise e
        | e -> "Error: " ^ Printexc.to_string e) }
  | _ -> None

(* Load and register manifest tools; returns the names registered. *)
let load () : string list =
  let path = manifest_path () in
  if not (Sys.file_exists path) then []
  else
    match
      let ic = open_in path in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> Yojson.Safe.from_channel ic)
    with
    | exception _ -> []
    | json ->
      let entries = match json |> member "tools" with `List l -> l | _ -> [] in
      List.filter_map
        (fun j ->
          match tool_of_json j with
          | Some t -> Tools.register t; Some t.Tools.name
          | None -> None)
        entries
