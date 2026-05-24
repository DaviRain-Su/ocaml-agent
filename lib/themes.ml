open Notty
open Yojson.Safe.Util

type t = { name : string; location : string; colors : (string * A.color option) list }

let getenv_nonempty k =
  match Sys.getenv_opt k with Some s when String.trim s <> "" -> Some s | _ -> None

let truthy s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "y" | "all" -> true
  | _ -> false

let split_paths s =
  s |> String.split_on_char '\n' |> List.map String.trim |> List.filter (fun p -> p <> "")

let disabled () =
  match Sys.getenv_opt "AGENT_NO_THEMES" with Some s when truthy s -> true | _ -> false

let theme_dirs () =
  Config_paths.uniq [ Config_paths.user_themes_dir (); ".ocaml-agent/themes"; ".pi/themes" ]

let extra_paths () =
  match getenv_nonempty "AGENT_THEME_PATHS" with Some s -> split_paths s | None -> []

let json_files_in_dir path =
  match Sys.readdir path with
  | exception _ -> []
  | entries ->
    Array.to_list entries
    |> List.filter (fun name -> Filename.check_suffix name ".json")
    |> List.sort compare
    |> List.map (Filename.concat path)

let expand_path path =
  if Sys.file_exists path && Sys.is_directory path then json_files_in_dir path else [ path ]

let hex_value c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let parse_hex_color s =
  if String.length s <> 7 || s.[0] <> '#' then None
  else
    let pair i =
      match (hex_value s.[i], hex_value s.[i + 1]) with
      | Some hi, Some lo -> Some ((hi * 16) + lo)
      | _ -> None
    in
    match (pair 1, pair 3, pair 5) with
    | Some r, Some g, Some b -> Some (A.rgb_888 ~r ~g ~b)
    | _ -> None

let indexed_color n =
  if n < 0 || n > 255 then None
  else
    match n with
    | 0 -> Some A.black
    | 1 -> Some A.red
    | 2 -> Some A.green
    | 3 -> Some A.yellow
    | 4 -> Some A.blue
    | 5 -> Some A.magenta
    | 6 -> Some A.cyan
    | 7 -> Some A.white
    | 8 -> Some A.lightblack
    | 9 -> Some A.lightred
    | 10 -> Some A.lightgreen
    | 11 -> Some A.lightyellow
    | 12 -> Some A.lightblue
    | 13 -> Some A.lightmagenta
    | 14 -> Some A.lightcyan
    | 15 -> Some A.lightwhite
    | n when n >= 232 -> Some (A.gray (n - 232))
    | n ->
      let i = n - 16 in
      Some (A.rgb ~r:(i / 36) ~g:((i / 6) mod 6) ~b:(i mod 6))

let named_color = function
  | "black" -> Some A.black
  | "red" -> Some A.red
  | "green" -> Some A.green
  | "yellow" -> Some A.yellow
  | "blue" -> Some A.blue
  | "magenta" -> Some A.magenta
  | "cyan" -> Some A.cyan
  | "white" -> Some A.white
  | "gray" | "grey" -> Some (A.gray 12)
  | "dimgray" | "dimgrey" -> Some (A.gray 8)
  | "darkgray" | "darkgrey" -> Some (A.gray 6)
  | _ -> None

let rec resolve_color vars value depth =
  if depth > 8 then None
  else
    match value with
    | `String "" -> None
    | `String s when String.length s > 0 && s.[0] = '#' -> parse_hex_color s
    | `String s -> (
      match List.assoc_opt s vars with
      | Some v -> resolve_color vars v (depth + 1)
      | None -> named_color (String.lowercase_ascii s))
    | `Int n -> indexed_color n
    | _ -> None

let assoc_or_empty name json =
  match json |> member name with
  | `Assoc xs -> xs
  | _ -> []

let load_file path =
  try
    let json = Yojson.Safe.from_file path in
    let vars = assoc_or_empty "vars" json in
    let colors =
      assoc_or_empty "colors" json
      |> List.map (fun (name, value) -> (name, resolve_color vars value 0))
    in
    let name =
      match json |> member "name" |> to_string_option with
      | Some s when String.trim s <> "" -> String.trim s
      | _ -> Filename.remove_extension (Filename.basename path)
    in
    Some { name; location = path; colors }
  with Sys.Break as e -> raise e | _ -> None

let builtin name = { name; location = "<builtin>"; colors = [] }

let dedupe_by_name themes =
  let rec loop kept = function
    | [] -> List.rev kept
    | t :: rest ->
      let kept = List.filter (fun old -> old.name <> t.name) kept in
      loop (t :: kept) rest
  in
  loop [] themes

let discover () =
  let paths =
    let defaults =
      if disabled () then []
      else
        List.concat_map expand_path (theme_dirs ())
        @ List.concat_map expand_path (Packages.paths Packages.Theme)
        @ List.concat_map expand_path (Settings.string_list "themes")
        @ List.concat_map expand_path (Extensions.theme_paths ())
    in
    defaults @ List.concat_map expand_path (extra_paths ())
  in
  [ builtin "dark"; builtin "light" ] @ List.filter_map load_file paths
  |> dedupe_by_name

let setting_theme_name () =
  Settings.string "theme"

let active_name () =
  match Config_paths.first_env [ "AGENT_THEME"; "PI_THEME" ] with
  | Some name -> Some name
  | None -> setting_theme_name ()

let current = ref None

let find_by_name name themes =
  List.find_opt (fun t -> String.equal t.name name) themes

let active () =
  let themes = discover () in
  let selected =
    match active_name () with
    | Some name -> find_by_name name themes
    | None -> None
  in
  let theme =
    match selected with
    | Some t -> t
    | None -> (match find_by_name "dark" themes with Some t -> t | None -> builtin "dark")
  in
  current := Some theme;
  theme

let current_theme () =
  match !current with Some t -> t | None -> active ()

let set_active_name ?(persist = false) name =
  Unix.putenv "AGENT_THEME" name;
  if persist then Settings.set_global_string "theme" name;
  active ()

let color_of_token theme token =
  match List.assoc_opt token theme.colors with
  | Some c -> c
  | None -> None

let fallback = function
  | "accent" | "borderAccent" | "mdCode" | "mdListBullet" | "bashMode" -> Some A.cyan
  | "border" -> Some A.blue
  | "success" | "toolDiffAdded" | "mdCodeBlock" -> Some A.green
  | "error" | "toolDiffRemoved" -> Some A.red
  | "warning" | "mdHeading" -> Some A.yellow
  | "muted" | "toolOutput" | "toolDiffContext" | "mdQuote" -> Some (A.gray 12)
  | "dim" | "borderMuted" | "mdLinkUrl" | "mdHr" -> Some (A.gray 8)
  | "thinkingText" -> Some (A.gray 10)
  | "selectedBg" -> Some A.cyan
  | "text" | "userMessageText" | "toolTitle" -> None
  | _ -> None

let color ?theme token =
  let theme = match theme with Some t -> t | None -> current_theme () in
  match color_of_token theme token with Some _ as c -> c | None -> fallback token

let fg ?theme token =
  match color ?theme token with Some c -> A.fg c | None -> A.empty

let bg ?theme token =
  match color ?theme token with Some c -> A.bg c | None -> A.empty

let attr ?theme ?fg:fg_token ?bg:bg_token () =
  let a = match fg_token with Some token -> fg ?theme token | None -> A.empty in
  match bg_token with
  | Some token ->
    let b = bg ?theme token in
    A.(a ++ b)
  | None -> a
