open Yojson.Safe.Util

type kind = Extension | Skill | Prompt | Theme

type scope = User | Project

type source =
  { root : string;
    source : string;
    scope : scope;
    filters : (kind * string list option) list }

type configured =
  { source : string;
    scope : scope;
    filtered : bool;
    installed_path : string option }

type resource =
  { resource_source : string;
    resource_scope : scope;
    resource_kind : kind;
    resource_path : string;
    resource_enabled : bool }

type npm_source = { spec : string; name : string; pinned : bool }

type git_source =
  { repo : string;
    host : string;
    path : string;
    ref_ : string option;
    pinned : bool }

type parsed_source = Npm of npm_source | Git of git_source | Local of string

let kind_field = function
  | Extension -> "extensions"
  | Skill -> "skills"
  | Prompt -> "prompts"
  | Theme -> "themes"

let kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "extension" | "extensions" -> Some Extension
  | "skill" | "skills" -> Some Skill
  | "prompt" | "prompts" | "prompt-template" | "prompt-templates" -> Some Prompt
  | "theme" | "themes" -> Some Theme
  | _ -> None

let conventional_dir = function
  | Extension -> "extensions"
  | Skill -> "skills"
  | Prompt -> "prompts"
  | Theme -> "themes"

let is_local_source s =
  let s = String.trim s in
  s <> "" && not (String.contains s ':')

let normalize path =
  let path = Config_paths.expand_tilde path in
  try Unix.realpath path with _ -> path

let resolve_path ~base path =
  let path = Config_paths.expand_tilde path in
  if Filename.is_relative path then normalize (Filename.concat base path) else normalize path

let project_root () = Sys.getcwd ()
let project_agent_dir () = Filename.concat (project_root ()) ".pi"

let base_dir_for_scope = function
  | User -> Config_paths.agent_dir ()
  | Project -> project_agent_dir ()

let npm_install_root scope = Filename.concat (base_dir_for_scope scope) "npm"
let npm_install_path source scope = Filename.concat (Filename.concat (npm_install_root scope) "node_modules") source.name

let git_install_root scope = Filename.concat (base_dir_for_scope scope) "git"
let git_install_path source scope =
  Filename.concat (Filename.concat (git_install_root scope) source.host) source.path

let strip_suffix suffix s =
  let ns = String.length s and nx = String.length suffix in
  if ns >= nx && String.sub s (ns - nx) nx = suffix then String.sub s 0 (ns - nx) else s

let split_once_char c s =
  match String.index_opt s c with
  | None -> None
  | Some i -> Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let split_ref s =
  match String.index_opt s '@' with
  | None -> (s, None)
  | Some i ->
    let left = String.sub s 0 i in
    let right = String.sub s (i + 1) (String.length s - i - 1) in
    if left = "" || right = "" then (s, None) else (left, Some right)

let starts_with s prefix =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let ends_with s suffix =
  let ns = String.length s and nx = String.length suffix in
  ns >= nx && String.sub s (ns - nx) nx = suffix

let is_override_pattern s =
  String.length s > 0 && List.mem s.[0] [ '!'; '+'; '-' ]

let strip_override_marker s =
  if is_override_pattern s then String.sub s 1 (String.length s - 1) else s

let has_glob_pattern s = String.contains s '*' || String.contains s '?'

let posix path = String.map (function '\\' -> '/' | c -> c) path

let relative_to ~base path =
  let base = normalize base |> posix in
  let path = normalize path |> posix in
  let prefix = if ends_with base "/" then base else base ^ "/" in
  if path = base then Filename.basename path
  else if starts_with path prefix then String.sub path (String.length prefix) (String.length path - String.length prefix)
  else path

let normalize_exact_pattern pattern =
  let pattern = posix pattern in
  if starts_with pattern "./" then String.sub pattern 2 (String.length pattern - 2) else pattern

let skill_parent_candidates path base =
  if Filename.basename path <> "SKILL.md" then []
  else
    let parent = Filename.dirname path in
    [ relative_to ~base parent; normalize parent |> posix; Filename.basename parent ]

let pattern_matches ~base pattern path =
  let pattern = posix pattern in
  let rel = relative_to ~base path in
  let full = normalize path |> posix in
  let candidates = Config_paths.uniq (rel :: Filename.basename path :: full :: skill_parent_candidates path base) in
  List.exists (fun candidate -> Tools.glob_match pattern candidate) candidates

let exact_pattern_matches ~base pattern path =
  let pattern = normalize_exact_pattern pattern in
  let rel = relative_to ~base path |> normalize_exact_pattern in
  let full = normalize path |> posix in
  let candidates =
    Config_paths.uniq (rel :: full :: Filename.basename path :: List.map normalize_exact_pattern (skill_parent_candidates path base))
  in
  List.exists (( = ) pattern) candidates

let apply_patterns all_paths patterns base =
  let includes, excludes, force_includes, force_excludes =
    List.fold_left
      (fun (includes, excludes, force_includes, force_excludes) pattern ->
        if starts_with pattern "+" then (includes, excludes, String.sub pattern 1 (String.length pattern - 1) :: force_includes, force_excludes)
        else if starts_with pattern "-" then (includes, excludes, force_includes, String.sub pattern 1 (String.length pattern - 1) :: force_excludes)
        else if starts_with pattern "!" then (includes, String.sub pattern 1 (String.length pattern - 1) :: excludes, force_includes, force_excludes)
        else (pattern :: includes, excludes, force_includes, force_excludes))
      ([], [], [], []) patterns
  in
  let selected =
    match List.rev includes with
    | [] -> all_paths
    | includes -> List.filter (fun path -> List.exists (fun pattern -> pattern_matches ~base pattern path) includes) all_paths
  in
  let selected =
    List.filter (fun path -> not (List.exists (fun pattern -> pattern_matches ~base pattern path) excludes)) selected
  in
  let selected =
    List.fold_left
      (fun acc pattern ->
        all_paths
        |> List.filter (fun path -> exact_pattern_matches ~base pattern path)
        |> List.fold_left (fun acc path -> if List.mem path acc then acc else acc @ [ path ]) acc)
      selected force_includes
  in
  List.filter (fun path -> not (List.exists (fun pattern -> exact_pattern_matches ~base pattern path) force_excludes)) selected

let parse_npm_spec spec =
  let spec = String.trim spec in
  let name, version =
    if starts_with spec "@" then
      match String.index_from_opt spec 1 '@' with
      | None -> (spec, None)
      | Some i -> (String.sub spec 0 i, Some (String.sub spec (i + 1) (String.length spec - i - 1)))
    else
      match String.index_opt spec '@' with
      | None -> (spec, None)
      | Some i -> (String.sub spec 0 i, Some (String.sub spec (i + 1) (String.length spec - i - 1)))
  in
  { spec; name; pinned = Option.is_some version }

let normalize_git_path path =
  let rec drop_slashes s =
    if String.length s > 0 && s.[0] = '/' then drop_slashes (String.sub s 1 (String.length s - 1)) else s
  in
  drop_slashes path |> strip_suffix ".git"

let host_without_userinfo host =
  match String.rindex_opt host '@' with
  | None -> host
  | Some i -> String.sub host (i + 1) (String.length host - i - 1)

let git_path_is_repo path =
  match String.split_on_char '/' path with
  | a :: b :: _ -> a <> "" && b <> ""
  | _ -> false

let parse_git_source source =
  let source = String.trim source in
  let has_git_prefix = starts_with source "git:" in
  let raw = if has_git_prefix then String.sub source 4 (String.length source - 4) |> String.trim else source in
  let explicit_protocol =
    starts_with raw "http://" || starts_with raw "https://" || starts_with raw "ssh://" || starts_with raw "git://"
  in
  if (not has_git_prefix) && (not explicit_protocol) then None
  else if starts_with raw "git@" then
    match split_once_char ':' raw with
    | None -> None
    | Some (user_host, rest) ->
      let host =
        match split_once_char '@' user_host with Some (_, h) -> h | None -> user_host
      in
      let path, ref_ = split_ref rest in
      let path = normalize_git_path path in
      if host = "" || not (git_path_is_repo path) then None
      else Some { repo = "git@" ^ host ^ ":" ^ path; host; path; ref_; pinned = Option.is_some ref_ }
  else if explicit_protocol then
    match Str.string_match (Str.regexp "^\\([a-z]+://\\)\\([^/]+\\)/\\(.+\\)$") raw 0 with
    | false -> None
    | true ->
      let scheme = Str.matched_group 1 raw in
      let host_part = Str.matched_group 2 raw in
      let host = host_without_userinfo host_part in
      let path_with_ref = Str.matched_group 3 raw in
      let path, ref_ = split_ref path_with_ref in
      let path = normalize_git_path path in
      if host = "" || not (git_path_is_repo path) then None
      else Some { repo = scheme ^ host_part ^ "/" ^ path; host; path; ref_; pinned = Option.is_some ref_ }
  else
    match split_once_char '/' raw with
    | None -> None
    | Some (host, rest) ->
      let path, ref_ = split_ref rest in
      let path = normalize_git_path path in
      if
        host = ""
        || not (git_path_is_repo path)
        || ((not (String.contains host '.')) && host <> "localhost")
      then None
      else Some { repo = "https://" ^ host ^ "/" ^ path; host; path; ref_; pinned = Option.is_some ref_ }

let parse_install_source source =
  let source = String.trim source in
  if starts_with source "npm:" then Npm (parse_npm_spec (String.sub source 4 (String.length source - 4)))
  else
    match parse_git_source source with
    | Some git -> Git git
    | None -> Local source

let expected_installed_path source scope =
  match parse_install_source source with
  | Local path ->
    resolve_path ~base:(base_dir_for_scope scope) path
  | Npm npm -> npm_install_path npm scope
  | Git git -> git_install_path git scope

let installed_path source scope =
  let path = expected_installed_path source scope in
  if Sys.file_exists path then Some path else None

let strings_of_json = function
  | `List xs ->
    xs
    |> List.filter_map (function `String s when String.trim s <> "" -> Some (String.trim s) | _ -> None)
  | _ -> []

let parse_filter kind json =
  match json |> member (kind_field kind) with
  | `Null -> None
  | `List xs -> Some (kind, Some (strings_of_json (`List xs)))
  | _ -> None

let parse_source ~settings_dir ~scope = function
  | `String s -> (
    match parse_install_source s with
    | Local _ when is_local_source s -> Some { root = resolve_path ~base:settings_dir s; source = s; scope; filters = [] }
    | Npm _ | Git _ -> (
      match installed_path s scope with
      | Some root -> Some { root; source = s; scope; filters = [] }
      | None -> None)
    | Local _ -> None)
  | `Assoc _ as obj -> (
    match obj |> member "source" with
    | `String s ->
      let filters =
        [ Extension; Skill; Prompt; Theme ]
        |> List.filter_map (fun kind -> parse_filter kind obj)
      in
      (match parse_install_source s with
       | Local _ when is_local_source s -> Some { root = resolve_path ~base:settings_dir s; source = s; scope; filters }
       | Npm _ | Git _ -> (
         match installed_path s scope with
         | Some root -> Some { root; source = s; scope; filters }
         | None -> None)
       | Local _ -> None)
    | _ -> None)
  | _ -> None

let project_settings_file () =
  Filename.concat (Filename.concat (Sys.getcwd ()) ".pi") "settings.json"

let settings_file = function
  | User -> Config_paths.user_settings_file ()
  | Project -> project_settings_file ()

let scope_name = function User -> "user" | Project -> "project"

let source_string = function
  | `String s -> Some s
  | `Assoc _ as obj -> (
    match obj |> member "source" with
    | `String s -> Some s
    | _ -> None)
  | _ -> None

let has_filters = function
  | `Assoc fields ->
    List.exists
      (fun (k, _) -> List.mem k [ "extensions"; "skills"; "prompts"; "themes" ])
      fields
  | _ -> false

let configured_packages_from_file scope path =
  let settings_dir = Filename.dirname path in
  match Yojson.Safe.from_file path |> member "packages" with
  | `List xs ->
    xs
    |> List.filter_map (fun pkg ->
           match source_string pkg with
           | None -> None
           | Some source ->
             let installed_path =
               match parse_install_source source with
               | Local _ when is_local_source source -> Some (resolve_path ~base:settings_dir source)
               | Local _ -> None
               | Npm _ | Git _ -> installed_path source scope
             in
             Some { source; scope; filtered = has_filters pkg; installed_path })
  | _ -> []
  | exception _ -> []

let configured_packages () =
  let user =
    if Sys.file_exists (settings_file User) then configured_packages_from_file User (settings_file User) else []
  in
  let project =
    Config_paths.project_settings_files ()
    |> List.filter (fun path -> Sys.file_exists path && not (Sys.is_directory path))
    |> List.concat_map (configured_packages_from_file Project)
  in
  user @ project

let package_entries fields =
  match List.assoc_opt "packages" fields with
  | Some (`List xs) -> xs
  | _ -> []

let write_package_entries path entries =
  let fields = Settings.read_fields path in
  Settings.write_fields path (("packages", `List entries) :: List.remove_assoc "packages" fields)

let local_identity ~base path = "local:" ^ resolve_path ~base path

let package_identity_for_settings ~scope source =
  match parse_install_source source with
  | Npm npm -> "npm:" ^ npm.name
  | Git git -> "git:" ^ git.host ^ "/" ^ git.path
  | Local path -> local_identity ~base:(base_dir_for_scope scope) path

let package_identity_for_input source =
  match parse_install_source source with
  | Npm npm -> "npm:" ^ npm.name
  | Git git -> "git:" ^ git.host ^ "/" ^ git.path
  | Local path -> local_identity ~base:(Sys.getcwd ()) path

let source_for_settings source =
  match parse_install_source source with
  | Local path -> resolve_path ~base:(Sys.getcwd ()) path
  | Npm _ | Git _ -> String.trim source

let package_matches ~scope source json =
  match source_string json with
  | Some s -> package_identity_for_settings ~scope s = package_identity_for_input source
  | None -> false

let update_package_source json source =
  match json with
  | `Assoc fields -> `Assoc (("source", `String source) :: List.remove_assoc "source" fields)
  | _ -> `String source

let add_source_to_settings ?(local = false) source =
  let scope = if local then Project else User in
  let path = settings_file scope in
  let source = source_for_settings source in
  let fields = Settings.read_fields path in
  let entries = package_entries fields in
  let rec replace changed = function
    | [] -> (changed, [])
    | entry :: rest when package_matches ~scope source entry ->
      let next = update_package_source entry source in
      (match source_string entry with
       | Some existing when existing = source -> (false, entry :: rest)
       | _ -> (true, next :: rest))
    | entry :: rest ->
      let changed, rest = replace changed rest in
      (changed, entry :: rest)
  in
  match replace false entries with
  | true, entries ->
    write_package_entries path entries;
    true
  | false, _ when List.exists (package_matches ~scope source) entries -> false
  | false, entries ->
    write_package_entries path (entries @ [ `String source ]);
    true

let remove_source_from_settings ?(local = false) source =
  let scope = if local then Project else User in
  let path = settings_file scope in
  let fields = Settings.read_fields path in
  let entries = package_entries fields in
  let kept = List.filter (fun entry -> not (package_matches ~scope source entry)) entries in
  if List.length kept = List.length entries then false
  else begin
    write_package_entries path kept;
    true
  end

let command_text ?cwd program args =
  let body = String.concat " " (List.map Filename.quote (program :: args)) in
  match cwd with
  | None -> body
  | Some dir -> Printf.sprintf "cd %s && %s" (Filename.quote dir) body

let run_checked ?cwd ?(timeout_s = 600) program args =
  let command = command_text ?cwd program args in
  let code, output = Tools.run_process ~timeout_s command in
  if code = 0 then ()
  else
    let output = String.trim output in
    let suffix = if output = "" then "" else ":\n" ^ output in
    failwith (Printf.sprintf "%s failed with exit %d%s" command code suffix)

let npm_command () =
  match Settings.string_list "npmCommand" with
  | command :: args -> (command, args)
  | [] -> ("npm", [])

let npm_manager_name () =
  let command, args = npm_command () in
  let parts = command :: args in
  let rec after_separator last = function
    | [] -> last
    | "--" :: x :: rest -> after_separator x rest
    | x :: rest -> after_separator x rest
  in
  Filename.basename (after_separator command parts)

let run_npm ?cwd args =
  let command, configured_args = npm_command () in
  run_checked ?cwd command (configured_args @ args)

let ensure_file path content =
  if not (Sys.file_exists path) then Tools.write_file_contents path content

let ensure_gitignore dir =
  Settings.ensure_dir dir;
  ensure_file (Filename.concat dir ".gitignore") "*\n!.gitignore\n"

let ensure_npm_project root =
  Settings.ensure_dir root;
  ensure_gitignore root;
  ensure_file (Filename.concat root "package.json") "{\n  \"name\": \"pi-extensions\",\n  \"private\": true\n}\n"

(* By default we refuse to run package lifecycle scripts (pre/postinstall),
   since installing an extension package would otherwise execute arbitrary code
   from an untrusted remote. Opt in with the "allowInstallScripts" setting. *)
let ignore_scripts_flag () =
  if Option.value (Settings.bool "allowInstallScripts") ~default:false then [] else [ "--ignore-scripts" ]

let npm_install_args specs root =
  let ignore_scripts = ignore_scripts_flag () in
  match npm_manager_name () with
  | "bun" -> "install" :: specs @ [ "--cwd"; root; "--omit=peer" ] @ ignore_scripts
  | "pnpm" ->
    "install" :: specs
    @
    [ "--prefix";
      root;
      "--config.auto-install-peers=false";
      "--config.strict-peer-dependencies=false";
      "--config.strict-dep-builds=false" ]
    @ ignore_scripts
  | _ -> "install" :: specs @ [ "--prefix"; root; "--legacy-peer-deps" ] @ ignore_scripts

let npm_uninstall_args name root =
  match npm_manager_name () with
  | "bun" -> [ "uninstall"; name; "--cwd"; root ]
  | _ -> [ "uninstall"; name; "--prefix"; root ]

let install_npm npm scope =
  if npm.name = "" then failwith "Invalid npm package source";
  let root = npm_install_root scope in
  ensure_npm_project root;
  run_npm (npm_install_args [ npm.spec ] root)

let uninstall_npm npm scope =
  let root = npm_install_root scope in
  if Sys.file_exists root then run_npm (npm_uninstall_args npm.name root)

let install_git_dependencies target =
  if Sys.file_exists (Filename.concat target "package.json") then
    let args =
      match Settings.string_list "npmCommand" with
      | [] -> [ "install"; "--omit=dev" ]
      | _ -> [ "install" ]
    in
    run_npm ~cwd:target (args @ ignore_scripts_flag ())

let checkout_git_ref target ref_ =
  run_checked ~cwd:target "git" [ "fetch"; "origin"; ref_ ];
  run_checked ~cwd:target "git" [ "checkout"; "FETCH_HEAD" ]

let install_git git scope =
  let target = git_install_path git scope in
  if Sys.file_exists target then begin
    if not (Sys.file_exists (Filename.concat target ".git")) then
      failwith (Printf.sprintf "Git package target exists but is not a git checkout: %s" target);
    (match git.ref_ with
     | Some ref_ -> checkout_git_ref target ref_
     | None -> run_checked ~cwd:target "git" [ "pull"; "--ff-only" ]);
    install_git_dependencies target
  end
  else begin
    ensure_gitignore (git_install_root scope);
    Settings.ensure_dir (Filename.dirname target);
    run_checked "git" [ "clone"; git.repo; target ];
    Option.iter (checkout_git_ref target) git.ref_;
    install_git_dependencies target
  end

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
    Sys.readdir path
    |> Array.iter (fun name ->
           if name <> "." && name <> ".." then remove_tree (Filename.concat path name));
    Unix.rmdir path
  | _ -> Sys.remove path
  | exception _ -> ()

let path_is_under ~root path =
  let root = normalize root in
  let path = normalize path in
  let prefix = root ^ Filename.dir_sep in
  path <> root
  && String.length path > String.length prefix
  && String.sub path 0 (String.length prefix) = prefix

let prune_empty_parents ~root target =
  let root = normalize root in
  let rec loop dir =
    let dir = normalize dir in
    if dir = root || not (path_is_under ~root dir) then ()
    else
      match Sys.readdir dir with
      | [||] ->
        (try Unix.rmdir dir with _ -> ());
        loop (Filename.dirname dir)
      | _ -> ()
      | exception _ -> ()
  in
  loop (Filename.dirname target)

let remove_git git scope =
  let root = git_install_root scope in
  let target = git_install_path git scope in
  if Sys.file_exists target then begin
    if not (path_is_under ~root target) then
      failwith (Printf.sprintf "Refusing to remove package outside managed git root: %s" target);
    remove_tree target;
    prune_empty_parents ~root target
  end

let install_local source =
  let resolved = resolve_path ~base:(Sys.getcwd ()) source in
  if Sys.file_exists resolved then resolved
  else failwith (Printf.sprintf "local package path not found: %s" source)

let install_source ?(local = false) source =
  let scope = if local then Project else User in
  try
    let label, installed =
      match parse_install_source source with
      | Local path ->
        let resolved = install_local path in
        ("local package", resolved)
      | Npm npm ->
        install_npm npm scope;
        ("npm package", npm_install_path npm scope)
      | Git git ->
        install_git git scope;
        ("git package", git_install_path git scope)
    in
    if add_source_to_settings ~local source then
      Printf.sprintf "Installed %s %s in %s settings." label installed (if local then "project" else "user")
    else Printf.sprintf "Package %s is already installed in %s settings." source (if local then "project" else "user")
  with
  | Failure msg -> "Error: " ^ msg
  | Unix.Unix_error (err, fn, arg) ->
    Printf.sprintf "Error: %s%s%s" (Unix.error_message err)
      (if fn = "" then "" else " in " ^ fn)
      (if arg = "" then "" else " " ^ arg)

let remove_source ?(local = false) source =
  let scope = if local then Project else User in
  try
    (match parse_install_source source with
     | Local _ -> ()
     | Npm npm -> uninstall_npm npm scope
     | Git git -> remove_git git scope);
    if remove_source_from_settings ~local source then
      Printf.sprintf "Removed package %s from %s settings." source (if local then "project" else "user")
    else Printf.sprintf "Package %s is not installed in %s settings." source (if local then "project" else "user")
  with
  | Failure msg -> "Error: " ^ msg
  | Unix.Unix_error (err, fn, arg) ->
    Printf.sprintf "Error: %s%s%s" (Unix.error_message err)
      (if fn = "" then "" else " in " ^ fn)
      (if arg = "" then "" else " " ^ arg)

let source_matches_configured input configured =
  package_identity_for_input input = package_identity_for_settings ~scope:configured.scope configured.source

let update_one configured =
  match parse_install_source configured.source with
  | Local _ -> ()
  | Npm npm -> if not npm.pinned then install_npm npm configured.scope
  (* Pinned git packages are already at their requested ref; skip like npm. *)
  | Git git -> if not git.pinned then install_git git configured.scope

let update_source ?source () =
  try
    let packages = configured_packages () in
    let targets =
      match source with
      | None -> packages
      | Some source -> List.filter (source_matches_configured source) packages
    in
    if Option.is_some source && targets = [] then
      Printf.sprintf "Error: No matching package found for %s" (Option.get source)
    else begin
      (* Isolate per-package failures so one bad repo doesn't abort the batch. *)
      let errors =
        List.filter_map
          (fun configured ->
            try
              update_one configured;
              None
            with
            | Failure msg -> Some (Printf.sprintf "%s: %s" configured.source msg)
            | Unix.Unix_error (err, fn, arg) ->
              Some
                (Printf.sprintf "%s: %s%s%s" configured.source (Unix.error_message err)
                   (if fn = "" then "" else " in " ^ fn)
                   (if arg = "" then "" else " " ^ arg)))
          targets
      in
      if errors = [] then "Updated packages."
      else Printf.sprintf "Updated with errors:\n%s" (String.concat "\n" errors)
    end
  with
  | Failure msg -> "Error: " ^ msg
  | Unix.Unix_error (err, fn, arg) ->
    Printf.sprintf "Error: %s%s%s" (Unix.error_message err)
      (if fn = "" then "" else " in " ^ fn)
      (if arg = "" then "" else " " ^ arg)

let format_configured_packages () =
  match configured_packages () with
  | [] -> "No packages installed."
  | packages ->
    packages
    |> List.map (fun p ->
           let suffix =
             match p.installed_path with
             | Some path -> " -> " ^ path
             | None -> ""
           in
           Printf.sprintf "%s%s%s%s"
             (scope_name p.scope)
             (if p.filtered then " filtered " else " ")
	     p.source suffix)
    |> String.concat "\n"

let kind_display kind =
  match kind with
  | Extension -> "extensions"
  | Skill -> "skills"
  | Prompt -> "prompts"
  | Theme -> "themes"

let strings_json xs = `List (List.map (fun s -> `String s) xs)

let assoc_without_nulls fields =
  `Assoc (List.filter (function _, `Null -> false | _ -> true) fields)

let ensure_package_object source = function
  | `Assoc fields -> fields
  | _ -> [ ("source", `String source) ]

let update_resource_filter fields kind pattern enabled =
  let field = kind_field kind in
  let current =
    match List.assoc_opt field fields with
    | Some (`List xs) -> strings_of_json (`List xs)
    | _ -> []
  in
  let current =
    current
    |> List.filter (fun existing -> strip_override_marker existing <> pattern)
  in
  let marker = if enabled then "+" else "-" in
  let next = current @ [ marker ^ pattern ] in
  assoc_without_nulls ((field, strings_json next) :: List.remove_assoc field fields)

let set_resource_enabled ?(local = false) ~source ~kind ~path ~enabled () =
  let scope = if local then Project else User in
  let settings_path = settings_file scope in
  let fields = Settings.read_fields settings_path in
  let entries = package_entries fields in
  let updated = ref false in
  let entries =
    entries
    |> List.map (fun entry ->
           if package_matches ~scope source entry then begin
             updated := true;
             let entry_source = Option.value (source_string entry) ~default:(source_for_settings source) in
             let root = expected_installed_path entry_source scope in
             let resource_path = if Filename.is_relative path then resolve_path ~base:root path else normalize path in
             let pattern = relative_to ~base:root resource_path in
             let object_fields = ensure_package_object entry_source entry in
             update_resource_filter object_fields kind pattern enabled
           end
           else entry)
  in
  if not !updated then
    Printf.sprintf "Error: No matching package found for %s in %s settings." source (scope_name scope)
  else begin
    write_package_entries settings_path entries;
    Printf.sprintf "%s %s %s for %s in %s settings."
      (if enabled then "Enabled" else "Disabled")
      (kind_display kind)
      path source (scope_name scope)
  end

let sources_from_settings_file scope path =
  let settings_dir = Filename.dirname path in
  match Yojson.Safe.from_file path |> member "packages" with
  | `List xs -> List.filter_map (parse_source ~settings_dir ~scope) xs
  | _ -> []
  | exception _ -> []

let sources () =
  let user =
    if Sys.file_exists (settings_file User) then sources_from_settings_file User (settings_file User) else []
  in
  let project =
    Config_paths.project_settings_files ()
    |> List.filter (fun path -> Sys.file_exists path && not (Sys.is_directory path))
    |> List.concat_map (sources_from_settings_file Project)
  in
  user @ project
  |> List.fold_left
       (fun acc src -> src :: List.filter (fun old -> old.root <> src.root) acc)
       []
  |> List.rev

let read_package_json root =
  let path = Filename.concat root "package.json" in
  try Some (Yojson.Safe.from_file path) with _ -> None

let package_manifest_paths root kind =
  match read_package_json root with
  | Some json -> strings_of_json (json |> member "pi" |> member (kind_field kind))
  | None -> []

let filter_for kind src = List.assoc_opt kind src.filters

let direct_files_with_suffixes suffixes dir =
  match Sys.readdir dir with
  | exception _ -> []
  | entries ->
    Array.to_list entries
    |> List.filter (fun name -> List.exists (ends_with name) suffixes)
    |> List.sort compare
    |> List.map (Filename.concat dir)

let rec recursive_files_with_suffixes suffixes dir =
  match Sys.readdir dir with
  | exception _ -> []
  | entries ->
    Array.to_list entries
    |> List.sort compare
    |> List.concat_map (fun name ->
           if name = "node_modules" || starts_with name "." then []
           else
             let path = Filename.concat dir name in
             if Sys.file_exists path && Sys.is_directory path then recursive_files_with_suffixes suffixes path
             else if List.exists (ends_with name) suffixes then [ path ]
             else [])

let extension_files dir =
  let direct = direct_files_with_suffixes [ ".json"; ".ts"; ".js"; ".mjs"; ".cjs" ] dir in
  let nested =
    match Sys.readdir dir with
    | exception _ -> []
    | entries ->
      Array.to_list entries
      |> List.sort compare
      |> List.concat_map (fun name ->
             let path = Filename.concat dir name in
             if Sys.file_exists path && Sys.is_directory path then
               [ "index.ts"; "index.js"; "index.mjs"; "index.cjs" ]
               |> List.map (Filename.concat path)
               |> List.filter Sys.file_exists
             else [])
  in
  Config_paths.uniq (direct @ nested)

let collect_kind_files_from_path kind path =
  if not (Sys.file_exists path) then []
  else if Sys.is_directory path then
    match kind with
    | Extension -> extension_files path
    | Skill -> recursive_files_with_suffixes [ ".md" ] path
    | Prompt -> direct_files_with_suffixes [ ".md" ] path
    | Theme -> direct_files_with_suffixes [ ".json" ] path
  else
    let ok =
      match kind with
      | Extension -> List.exists (ends_with path) [ ".json"; ".ts"; ".js"; ".mjs"; ".cjs" ]
      | Skill | Prompt -> ends_with path ".md"
      | Theme -> ends_with path ".json"
    in
    if ok then [ path ] else []

let all_kind_files_under root kind =
  match kind with
  | Extension -> recursive_files_with_suffixes [ ".json"; ".ts"; ".js"; ".mjs"; ".cjs" ] root
  | Skill -> recursive_files_with_suffixes [ ".md" ] root
  | Prompt -> recursive_files_with_suffixes [ ".md" ] root
  | Theme -> recursive_files_with_suffixes [ ".json" ] root

let collect_manifest_files root kind entries =
  let source_entries = List.filter (fun entry -> not (is_override_pattern entry)) entries in
  source_entries
  |> List.concat_map (fun entry ->
         if has_glob_pattern entry then
           all_kind_files_under root kind
           |> List.filter (fun path -> pattern_matches ~base:root entry path)
         else collect_kind_files_from_path kind (resolve_path ~base:root entry))
  |> Config_paths.uniq

let default_package_files root kind =
  match package_manifest_paths root kind with
  | entries when entries <> [] ->
    let files = collect_manifest_files root kind entries in
    let manifest_overrides = List.filter is_override_pattern entries in
    if manifest_overrides = [] then files else apply_patterns files manifest_overrides root
  | _ ->
    let dir = Filename.concat root (conventional_dir kind) in
    collect_kind_files_from_path kind dir

let package_resources src kind =
  let files = default_package_files src.root kind in
  let enabled =
    match filter_for kind src with
    | Some (Some patterns) -> apply_patterns files patterns src.root
    | _ -> files
  in
  files
  |> List.map (fun path ->
         { resource_source = src.source;
           resource_scope = src.scope;
           resource_kind = kind;
           resource_path = path;
           resource_enabled = List.mem path enabled })

let resources () =
  sources ()
  |> List.concat_map (fun src -> [ Extension; Skill; Prompt; Theme ] |> List.concat_map (package_resources src))

let paths kind =
  resources ()
  |> List.filter (fun resource -> resource.resource_kind = kind && resource.resource_enabled)
  |> List.map (fun resource -> resource.resource_path)
  |> Config_paths.uniq

let format_config_resources () =
  match resources () with
  | [] -> "No package resources found."
  | resources ->
    resources
    |> List.sort (fun a b ->
           compare
             (scope_name a.resource_scope, a.resource_source, kind_display a.resource_kind, a.resource_path)
             (scope_name b.resource_scope, b.resource_source, kind_display b.resource_kind, b.resource_path))
    |> List.map (fun r ->
           Printf.sprintf "%s %s %s %s %s"
             (if r.resource_enabled then "[x]" else "[ ]")
             (scope_name r.resource_scope)
             r.resource_source
             (kind_display r.resource_kind)
             (relative_to ~base:(expected_installed_path r.resource_source r.resource_scope) r.resource_path))
    |> String.concat "\n"

let parse_source_kind_for_test source =
  match parse_install_source source with
  | Npm npm -> Printf.sprintf "npm:%s:%b" npm.name npm.pinned
  | Git git -> Printf.sprintf "git:%s/%s:%b" git.host git.path git.pinned
  | Local path -> "local:" ^ path

let installed_path_for_test source scope = expected_installed_path source scope
