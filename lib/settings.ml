let string field = Config_paths.settings_string [ field ]
let bool field = Config_paths.settings_bool [ field ]
let float field = Config_paths.settings_float [ field ]
let string_list field = Config_paths.settings_string_list [ field ]

let nested_bool field subfield = Config_paths.settings_bool [ field; subfield ]
let nested_float field subfield = Config_paths.settings_float [ field; subfield ]

let rec ensure_dir dir =
  if dir = "" || dir = "." || Sys.file_exists dir then ()
  else begin
    ensure_dir (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let read_fields path =
  match Yojson.Safe.from_file path with
  | `Assoc xs -> xs
  | _ -> []
  | exception _ -> []

let read_global_fields () =
  let path = Config_paths.user_settings_file () in
  read_fields path

let write_fields path fields =
  ensure_dir (Filename.dirname path);
  let json = Yojson.Safe.to_string (`Assoc fields) in
  let tmp = path ^ ".tmp" in
  let oc = open_out tmp in
  try
    output_string oc json;
    output_char oc '\n';
    close_out oc;
    Sys.rename tmp path
  with e ->
    close_out_noerr oc;
    (try Sys.remove tmp with Sys_error _ -> ());
    raise e

let write_global_fields fields =
  let path = Config_paths.user_settings_file () in
  write_fields path fields

let set_global field value =
  let fields = read_global_fields () in
  write_global_fields ((field, value) :: List.remove_assoc field fields)

let set_global_string field value = set_global field (`String value)
let set_global_bool field value = set_global field (`Bool value)

let startup_env_default env field =
  match (Sys.getenv_opt env, string field) with
  | (Some s, _) when String.trim s <> "" -> ()
  | (_, Some value) -> Unix.putenv env value
  | _ -> ()

let startup_env_bool_default env field =
  match (Sys.getenv_opt env, bool field) with
  | (Some s, _) when String.trim s <> "" -> ()
  | (_, Some value) -> Unix.putenv env (if value then "1" else "0")
  | _ -> ()

let startup_env_nested_bool_default env field subfield =
  match (Sys.getenv_opt env, nested_bool field subfield) with
  | (Some s, _) when String.trim s <> "" -> ()
  | (_, Some value) -> Unix.putenv env (if value then "1" else "0")
  | _ -> ()

let apply_startup_defaults ?provider ?model ?thinking ?models ?session_dir () =
  if provider = None then startup_env_default "AGENT_PROVIDER" "defaultProvider";
  if model = None then startup_env_default "AGENT_MODEL" "defaultModel";
  if thinking = None then startup_env_default "AGENT_THINKING" "defaultThinkingLevel";
  if models = None then
    match (Sys.getenv_opt "AGENT_SCOPED_MODELS", string_list "enabledModels") with
    | (Some s, _) when String.trim s <> "" -> ()
    | (_, patterns) when patterns <> [] -> Unix.putenv "AGENT_SCOPED_MODELS" (String.concat "\n" patterns)
    | _ -> ();
  if session_dir = None then startup_env_default "AGENT_SESSION_DIR" "sessionDir";
  startup_env_nested_bool_default "AGENT_AUTO_COMPACT" "compaction" "enabled";
  startup_env_bool_default "AGENT_QUIET_STARTUP" "quietStartup"
