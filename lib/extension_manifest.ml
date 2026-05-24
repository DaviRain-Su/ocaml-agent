open Yojson.Safe.Util

let env_nonempty k =
  match Sys.getenv_opt k with Some s when String.trim s <> "" -> Some s | _ -> None

let truthy s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "y" | "all" -> true
  | _ -> false

let split_paths s =
  s |> String.split_on_char '\n' |> List.map String.trim |> List.filter (fun p -> p <> "")

let user_extensions_dir () = Filename.concat (Config_paths.agent_dir ()) "extensions"

let manifest_paths () =
  let explicit =
    (match env_nonempty "AGENT_TOOLS_FILE" with Some p -> [ p ] | None -> [])
    @ (match env_nonempty "AGENT_EXTENSION_PATHS" with Some s -> split_paths s | None -> [])
  in
  let defaults =
    match Sys.getenv_opt "AGENT_NO_EXTENSIONS" with
    | Some s when truthy s -> []
    | _ ->
      [ Config_paths.user_tools_manifest ();
        Config_paths.user_tools_dir ();
        user_extensions_dir ();
        ".ocaml-agent/extensions";
        ".ocaml-agent/tools.json";
        ".pi/extensions";
        ".pi/tools.json" ]
      @ Packages.paths Packages.Extension
      @ Settings.string_list "extensions"
  in
  Config_paths.uniq (defaults @ explicit)

let is_json_file path = Filename.check_suffix path ".json"

let is_js_extension_file path =
  List.exists (Filename.check_suffix path) [ ".ts"; ".js"; ".mjs"; ".cjs" ]

let is_ocaml_sdk_extension_file path =
  List.exists (Filename.check_suffix path) [ ".ocamlext"; ".ocaml-extension" ]

let files_in_dir path =
  match Sys.readdir path with
  | exception _ -> []
  | entries ->
    Array.to_list entries
    |> List.filter (fun name -> is_json_file name || is_js_extension_file name || is_ocaml_sdk_extension_file name)
    |> List.sort compare
    |> List.map (Filename.concat path)

let index_files path =
  [ "index.ts"; "index.js"; "index.mjs"; "index.cjs"; "index.ocamlext"; "index.ocaml-extension" ]
  |> List.map (Filename.concat path)
  |> List.filter Sys.file_exists

let expand_manifest_path path =
  if Sys.file_exists path && Sys.is_directory path then
    let direct = files_in_dir path in
    let nested =
      match Sys.readdir path with
      | exception _ -> []
      | entries ->
        Array.to_list entries
        |> List.concat_map (fun name ->
               let full = Filename.concat path name in
               if Sys.file_exists full && Sys.is_directory full then index_files full else [])
    in
    direct @ nested |> Config_paths.uniq
  else if is_json_file path || is_js_extension_file path || is_ocaml_sdk_extension_file path then [ path ]
  else []

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
        execute =
          (fun input ->
            try run_command command (Yojson.Safe.to_string input) with
            | Sys.Break as e -> raise e
            | e -> "Error: " ^ Printexc.to_string e) }
  | _ -> None

let load_manifest path =
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
            let name = match j |> member "name" with `String s -> s | _ -> "(unnamed)" in
            Printf.eprintf "[warning] extension tool %s in %s is missing a required field (name or command)\n%!" name path;
            None)
        entries
