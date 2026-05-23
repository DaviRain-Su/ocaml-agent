(* Skills: markdown files with a small YAML frontmatter block, discovered from
   skill directories and injected into the system prompt as an inventory. The
   model reads a skill's file (via read_file) when a task matches it — the same
   prompt-injection model pi uses; there is no separate execution runtime. *)

type t = { name : string; description : string; location : string }

(* Directories searched for "*.md" skills, relative to the cwd. *)
let skill_dirs = [ ".ocaml-agent/skills"; ".claude/skills" ]

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic (in_channel_length ic))

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
    (match List.assoc_opt "disable-model-invocation" fm with
     | Some ("true" | "yes" | "1") -> None
     | _ -> Some { name; description; location = path })

let discover () : t list =
  List.concat_map
    (fun dir ->
      if (try Sys.is_directory dir with _ -> false) then
        Sys.readdir dir |> Array.to_list
        |> List.filter (fun f -> Filename.check_suffix f ".md")
        |> List.sort compare
        |> List.filter_map (fun f -> parse_skill (Filename.concat dir f))
      else [])
    skill_dirs

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
