open Yojson.Safe.Util

let getenv_nonempty k =
  match Sys.getenv_opt k with Some s when String.trim s <> "" -> Some s | _ -> None

let home_dir () =
  match getenv_nonempty "HOME" with Some h -> h | None -> "."

let expand_tilde path =
  if path = "~" then home_dir ()
  else if String.length path > 2 && path.[0] = '~' && path.[1] = '/' then
    Filename.concat (home_dir ()) (String.sub path 2 (String.length path - 2))
  else path

let first_env keys = List.find_map getenv_nonempty keys

let agent_dir () =
  match first_env [ "PI_CODING_AGENT_DIR"; "AGENT_CODING_AGENT_DIR"; "OCAML_AGENT_DIR"; "AGENT_DIR" ] with
  | Some dir -> expand_tilde dir
  | None -> Filename.concat (Filename.concat (home_dir ()) ".pi") "agent"

let user_skills_dir () = Filename.concat (agent_dir ()) "skills"
let user_prompts_dir () = Filename.concat (agent_dir ()) "prompts"
let user_tools_dir () = Filename.concat (agent_dir ()) "tools"
let user_tools_manifest () = Filename.concat (agent_dir ()) "tools.json"
let user_themes_dir () = Filename.concat (agent_dir ()) "themes"
let user_settings_file () = Filename.concat (agent_dir ()) "settings.json"

let uniq paths =
  let rec loop seen out = function
    | [] -> List.rev out
    | p :: rest when List.mem p seen -> loop seen out rest
    | p :: rest -> loop (p :: seen) (p :: out) rest
  in
  loop [] [] paths

let parent dir =
  let p = Filename.dirname dir in
  if p = dir then None else Some p

let rec ancestors dir =
  match parent dir with
  | None -> [ dir ]
  | Some p -> dir :: ancestors p

let context_candidates dir =
  [ Filename.concat dir "AGENTS.md";
    Filename.concat dir "AGENTS.MD";
    Filename.concat dir "CLAUDE.md";
    Filename.concat dir "CLAUDE.MD" ]

let existing_context_files dir =
  context_candidates dir
  |> List.filter (fun path -> Sys.file_exists path && not (Sys.is_directory path))

let context_files () =
  let global = existing_context_files (agent_dir ()) in
  let cwd = Sys.getcwd () in
  let project =
    ancestors cwd
    |> List.rev
    |> List.concat_map existing_context_files
  in
  uniq (global @ project)

let project_settings_files () =
  [ Filename.concat (Filename.concat (Sys.getcwd ()) ".ocaml-agent") "settings.json";
    Filename.concat (Filename.concat (Sys.getcwd ()) ".pi") "settings.json" ]

let settings_files () =
  user_settings_file () :: project_settings_files ()
  |> uniq
  |> List.filter (fun path -> Sys.file_exists path && not (Sys.is_directory path))

let rec merge_json base over =
  match (base, over) with
  | `Assoc a, `Assoc b ->
    let keys =
      List.map fst a @ List.map fst b
      |> List.sort_uniq String.compare
    in
    `Assoc
      (List.map
         (fun k ->
           let v =
             match (List.assoc_opt k a, List.assoc_opt k b) with
             | Some av, Some bv -> merge_json av bv
             | Some av, None -> av
             | None, Some bv -> bv
             | None, None -> `Null
           in
           (k, v))
         keys)
  | _, `Null -> base
  | _, v -> v

let settings_json () =
  settings_files ()
  |> List.fold_left
       (fun acc path ->
         try merge_json acc (Yojson.Safe.from_file path) with
         | Sys.Break as e -> raise e
         | _ -> acc)
       (`Assoc [])

let settings_member path =
  let rec loop json = function
    | [] -> json
    | k :: rest -> (
      match json with
      | `Assoc _ -> loop (json |> member k) rest
      | _ -> `Null)
  in
  loop (settings_json ()) path

let settings_string path =
  match settings_member path with
  | `String s when String.trim s <> "" -> Some s
  | _ -> None

let settings_bool path =
  match settings_member path with
  | `Bool b -> Some b
  | _ -> None

let settings_float path =
  match settings_member path with
  | `Float f -> Some f
  | `Int i -> Some (float_of_int i)
  | _ -> None

let settings_string_list path =
  match settings_member path with
  | `List xs ->
    xs
    |> List.filter_map (function `String s when String.trim s <> "" -> Some s | _ -> None)
  | _ -> []

let sessions_dir () =
  match first_env [ "AGENT_SESSION_DIR"; "PI_CODING_AGENT_SESSION_DIR" ] with
  | Some dir -> expand_tilde dir
  | None -> (
    match settings_string [ "sessionDir" ] with
    | Some dir -> expand_tilde dir
    | None -> Filename.concat (agent_dir ()) "sessions")
