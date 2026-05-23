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
  let code, body = Tools.run_process ~stdin_data:input command in
  if code = 0 then body else Printf.sprintf "(exit %d)\n%s" code body

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
        requires_approval = true;
        execute = (fun input -> try run_command command (Yojson.Safe.to_string input) with
        | Sys.Break as e -> raise e
        | e -> "Error: " ^ Printexc.to_string e) }
  | _ -> None

(* Load and register manifest tools; returns the names registered.
   Prints a warning to stderr if the manifest is malformed. *)
let load () : string list =
  let path = manifest_path () in
  if not (Sys.file_exists path) then []
  else
    match
      let ic = open_in path in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> Yojson.Safe.from_channel ic)
    with
    | exception Yojson.Json_error msg ->
      Printf.eprintf "[warning] extension manifest %s has invalid JSON: %s\n%!" path msg;
      []
    | exception e ->
      Printf.eprintf "[warning] failed to read extension manifest %s: %s\n%!" path (Printexc.to_string e);
      []
    | json ->
      let entries = match json |> member "tools" with `List l -> l | _ -> [] in
      if entries = [] then
        Printf.eprintf "[warning] extension manifest %s has no tools array or it is empty\n%!" path;
      List.filter_map
        (fun j ->
          match tool_of_json j with
          | Some t when Tools.register t -> Some t.Tools.name
          | Some _ -> None
          | None ->
            let name =
              match j |> member "name" with `String s -> s | _ -> "(unnamed)"
            in
            Printf.eprintf "[warning] extension tool %s in %s is missing a required field (name or command)\n%!" name path;
            None)
        entries
