(* Skills: markdown files with a small YAML frontmatter block, discovered from
   skill directories and injected into the system prompt as an inventory. The
   model reads a skill's file (via read_file) when a task matches it — the same
   prompt-injection model pi uses; there is no separate execution runtime. *)

type t = { name : string; description : string; location : string }

(* Directories searched for "*.md" skills, relative to the cwd plus the Pi-style
   user agent directory. *)
let skill_dirs () = Config_paths.uniq [ Config_paths.user_skills_dir (); ".ocaml-agent/skills"; ".pi/skills"; ".claude/skills" ]

(* Also check for Veldt scaffold in the project root *)
let veldt_scaffold_marker = "scaffold/veldt"

let getenv_nonempty k =
  match Sys.getenv_opt k with Some s when String.trim s <> "" -> Some s | _ -> None

let extra_paths () =
  match getenv_nonempty "AGENT_SKILL_PATHS" with
  | None -> []
  | Some s -> s |> String.split_on_char '\n' |> List.map String.trim |> List.filter (fun p -> p <> "")

let settings_paths () = Settings.string_list "skills"

let disabled () =
  match getenv_nonempty "AGENT_NO_SKILLS" with
  | Some ("1" | "true" | "yes" | "y") -> true
  | _ -> false

let max_file_bytes = 1024 * 1024

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
       let total = in_channel_length ic in
       let len = min max_file_bytes total in
       let s = really_input_string ic len in
       if total > max_file_bytes then s ^ "\n... (truncated)" else s)

(* Parse a leading "---\n ... \n---" frontmatter block into key/value pairs. *)
let parse_frontmatter content =
  let lines = String.split_on_char '\n' content in
  match lines with
  | first :: rest when String.trim first = "---" ->
    let rec collect acc = function
      | [] -> acc
      | l :: _ when String.trim l = "---" -> acc
      | l :: tl ->
        let acc =
          match String.index_opt l ':' with
          | Some i ->
            let k = String.trim (String.sub l 0 i) in
            let v = String.trim (String.sub l (i + 1) (String.length l - i - 1)) in
            (String.lowercase_ascii k, v) :: acc
          | None -> acc
        in
        collect acc tl
    in
    collect [] rest
  | _ -> []

let parse_skill path : t option =
  match read_file path with
  | exception _ -> None
  | content ->
    let fm = parse_frontmatter content in
    let name = match List.assoc_opt "name" fm with Some n when n <> "" -> n | _ -> Filename.remove_extension (Filename.basename path) in
    let description = match List.assoc_opt "description" fm with Some d -> d | None -> "" in
    (* Skip skills explicitly opted out of model invocation. *)
    (match Option.map String.lowercase_ascii (List.assoc_opt "disable-model-invocation" fm) with
     | Some ("true" | "yes" | "1") -> None
     | _ -> Some { name; description; location = path })

let is_real_dir path =
  try
    let st = Unix.lstat path in
    st.Unix.st_kind = Unix.S_DIR
  with _ -> false

let discover_dir dir =
  if (try Sys.is_directory dir with _ -> false) then
    let rec go root visited =
      let canon =
        try Unix.realpath root with _ -> root
      in
      if List.mem canon visited then []
      else
        let visited = canon :: visited in
        let entries = try Sys.readdir root |> Array.to_list |> List.sort compare with _ -> [] in
        let markdown =
          entries
          |> List.filter (fun f -> Filename.check_suffix f ".md")
          |> List.filter_map (fun f -> parse_skill (Filename.concat root f))
        in
        let nested =
          entries
          |> List.concat_map (fun e ->
                 let path = Filename.concat root e in
                 if is_real_dir path then
                   let skill_file = Filename.concat path "SKILL.md" in
                   if Sys.file_exists skill_file then
                     match parse_skill skill_file with Some s -> [ s ] | None -> []
                   else go path visited
                 else [])
        in
        markdown @ nested
    in
    go dir []
  else []

let discover_path path =
  if (try Sys.is_directory path with _ -> false) then discover_dir path
  else if Sys.file_exists path && Filename.check_suffix path ".md" then (match parse_skill path with Some s -> [ s ] | None -> [])
  else []

let discover () : t list =
  if disabled () then List.concat_map discover_path (extra_paths ())
  else
    List.concat_map discover_dir (skill_dirs ())
    @ List.concat_map discover_path (Packages.paths Packages.Skill)
    @ List.concat_map discover_path (settings_paths ())
    @ List.concat_map discover_path (Extensions.skill_paths ())
    @ List.concat_map discover_path (extra_paths ())

(* Render the skill inventory for the system prompt. Returns "" if none. *)
let format = function
  | [] -> ""
  | skills ->
    let buf = Buffer.create 512 in
    Buffer.add_string buf
      "\n\nThe following skills are available. When a task matches a skill, first read \
       its file at the given location (with read_file) for detailed instructions.\n\
       <available_skills>\n";
    List.iter
      (fun s ->
        Buffer.add_string buf
          (Printf.sprintf "  <skill>\n    <name>%s</name>\n    <description>%s</description>\n    <location>%s</location>\n  </skill>\n"
             s.name s.description s.location))
      skills;
    Buffer.add_string buf "</available_skills>";
    Buffer.contents buf
