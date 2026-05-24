(* Session persistence and management.

   A session is a JSONL file: the first line is a header object (tagged
   "_session") with id/name/created/cwd; each subsequent line is one turn. The
   default home is a sessions directory so multiple sessions can be listed,
   resumed, cloned, named, and exported. An explicit file path (legacy
   AGENT_SESSION_FILE) is also supported, with or without a header. *)

open Yojson.Safe.Util

type info = { id : string; path : string; name : string; created : float; cwd : string }

type t =
  { id : string;
    path : string;
    mutable name : string;
    created : float;
    cwd : string;
    mutable entries : Yojson.Safe.t list;
    mutable oc : out_channel }

let default_dir () =
  Config_paths.sessions_dir ()

let ensure_dir d =
  if not (Sys.file_exists d) then ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote d)))

let dir () =
  let d = default_dir () in
  ensure_dir d;
  d

let gen_id () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d-%02d%02d%02d-%04d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec (Random.int 10000)

let header_json id name created cwd =
  `Assoc
    [ ("_session", `Int 1);
      ("id", `String id);
      ("name", `String name);
      ("created", `Float created);
      ("cwd", `String cwd) ]

let write_header oc id name created cwd =
  output_string oc (Yojson.Safe.to_string (header_json id name created cwd));
  output_char oc '\n';
  flush oc

let is_header_json j = match j |> member "_session" with `Null -> false | _ -> true
let is_turn_json j = match j |> member "role", j |> member "content" with `String _, `List _ -> true | _ -> false

let branch_summary_prefix =
  "The following is a summary of a branch that this conversation came back from:\n\n<summary>\n"

let branch_summary_suffix = "</summary>"

let compaction_summary_prefix =
  "The conversation history before this point was compacted into the following summary:\n\n<summary>\n"

let compaction_summary_suffix = "\n</summary>"

let read_body path =
  if not (Sys.file_exists path) then []
  else begin
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let rec loop acc =
          match input_line ic with
          | exception End_of_file -> List.rev acc
          | line ->
            let line = String.trim line in
            let acc =
              if line = "" then acc
              else
                match Yojson.Safe.from_string line with
                | exception _ -> acc
                | j -> if is_header_json j then acc else j :: acc
            in
            loop acc
        in
        loop [])
  end

(* Read the first line of [path] as a session header, if present. *)
let read_header path : info option =
  if not (Sys.file_exists path) then None
  else begin
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        match input_line ic with
        | exception End_of_file -> None
        | line -> (
          match Yojson.Safe.from_string line with
          | exception _ -> None
          | j -> (
            match j |> member "_session" with
            | `Null -> None
            | _ ->
              Some
                { id = (match j |> member "id" with `String s -> s | _ -> Filename.basename path);
                  path;
                  name = (match j |> member "name" with `String s -> s | _ -> "");
                  created = (match j |> member "created" with `Float f -> f | `Int i -> float_of_int i | _ -> 0.);
                  cwd = (match j |> member "cwd" with `String s -> s | _ -> "") })))
  end

(* Load just the turns from a session file, skipping the header line. *)
let load_turns path : Llm.turn list =
  read_body path |> List.filter is_turn_json |> List.filter_map (fun j -> try Some (Llm.turn_of_json j) with Sys.Break as e -> raise e | _ -> None)

let load_entries path = read_body path |> List.filter (fun j -> not (is_turn_json j))

let content_blocks_of_json = function
  | `String text -> [ Llm.Text text ]
  | `List blocks -> List.map Llm.content_of_json blocks
  | _ -> []

let string_member name json =
  match json |> member name with
  | `String value -> value
  | _ -> ""

let bool_member name json =
  match json |> member name with
  | `Bool value -> value
  | _ -> false

let int_member name json =
  match json |> member name with
  | `Int value -> Some value
  | `Float value -> Some (int_of_float value)
  | _ -> None

let bash_execution_text json =
  let command = string_member "command" json in
  let output = string_member "output" json in
  let body = if output = "" then "(no output)" else Printf.sprintf "```\n%s\n```" output in
  let exit_text =
    if bool_member "cancelled" json then "\n\n(command cancelled)"
    else
      match int_member "exitCode" json with
      | Some code when code <> 0 -> Printf.sprintf "\n\nCommand exited with code %d" code
      | _ -> ""
  in
  let truncated_text =
    if bool_member "truncated" json then
      match string_member "fullOutputPath" json with
      | "" -> ""
      | path -> Printf.sprintf "\n\n[Output truncated. Full output: %s]" path
    else ""
  in
  Printf.sprintf "Ran `%s`\n%s%s%s" command body exit_text truncated_text

let context_turn_of_message json =
  match json |> member "role" with
  | `String "custom" -> (
    match content_blocks_of_json (json |> member "content") with
    | [] -> None
    | content -> Some { Llm.role = Llm.User; content })
  | `String "bashExecution" ->
    if bool_member "excludeFromContext" json then None
    else Some { Llm.role = Llm.User; content = [ Llm.Text (bash_execution_text json) ] }
  | `String "branchSummary" -> (
    match json |> member "summary" with
    | `String summary when String.trim summary <> "" ->
      Some { Llm.role = Llm.User; content = [ Llm.Text (branch_summary_prefix ^ summary ^ branch_summary_suffix) ] }
    | _ -> None)
  | `String "compactionSummary" -> (
    match json |> member "summary" with
    | `String summary when String.trim summary <> "" ->
      Some { Llm.role = Llm.User; content = [ Llm.Text (compaction_summary_prefix ^ summary ^ compaction_summary_suffix) ] }
    | _ -> None)
  | `String ("user" | "assistant" | "toolResult") -> (
    match json |> member "content" with
    | `List _ -> Some (Llm.turn_of_json json)
    | _ -> None)
  | _ -> None

let context_turn_of_entry json =
  match json |> member "type" with
  | `String "message" -> context_turn_of_message (json |> member "message")
  | `String "custom_message" -> (
    match content_blocks_of_json (json |> member "content") with
    | [] -> None
    | content -> Some { Llm.role = Llm.User; content })
  | `String "branch_summary" -> (
    match json |> member "summary" with
    | `String summary when String.trim summary <> "" ->
      Some { Llm.role = Llm.User; content = [ Llm.Text (branch_summary_prefix ^ summary ^ branch_summary_suffix) ] }
    | _ -> None)
  | `String "compaction" -> (
    match json |> member "summary" with
    | `String summary when String.trim summary <> "" ->
      Some { Llm.role = Llm.User; content = [ Llm.Text (compaction_summary_prefix ^ summary ^ compaction_summary_suffix) ] }
    | _ -> None)
  | _ -> None

let context_turn_of_json json =
  if is_turn_json json then Some (Llm.turn_of_json json) else context_turn_of_entry json

let load_context_turns path : Llm.turn list =
  read_body path |> List.filter_map (fun j -> try context_turn_of_json j with Sys.Break as e -> raise e | _ -> None)

(* legacy single-file helper kept for the old API name *)
let load = load_turns

let create_new ?(name = "") () : t =
  let d = dir () in
  let id = gen_id () in
  let path = Filename.concat d (id ^ ".jsonl") in
  let created = Unix.time () in
  let cwd = Sys.getcwd () in
  let oc = open_out path in
  try
    write_header oc id name created cwd;
    { id; path; name; created; cwd; entries = []; oc }
  with e ->
    close_out_noerr oc;
    raise e

(* Open a specific file for appending (legacy AGENT_SESSION_FILE). Adds a header
   if the file is new/empty. *)
let open_file path : t =
  let parent = Filename.dirname path in
  if parent <> "." && not (Sys.file_exists parent) then ensure_dir parent;
  match read_header path with
  | Some i ->
    let oc = open_out_gen [ Open_append; Open_creat ] 0o644 path in
    { id = i.id; path; name = i.name; created = i.created; cwd = i.cwd;
      entries = load_entries path; oc }
  | None ->
    let id = Filename.remove_extension (Filename.basename path) in
    let created = Unix.time () in
    let cwd = Sys.getcwd () in
    let exists_nonempty =
      try Sys.file_exists path && (Unix.stat path).Unix.st_size > 0
      with Unix.Unix_error _ -> false
    in
    let oc = open_out_gen [ Open_append; Open_creat ] 0o644 path in
    try
      if not exists_nonempty then write_header oc id "" created cwd;
      { id; path; name = ""; created; cwd; entries = load_entries path; oc }
    with e ->
      close_out_noerr oc;
      raise e

let append t (turn : Llm.turn) =
  output_string t.oc (Yojson.Safe.to_string (Llm.turn_to_json turn));
  output_char t.oc '\n';
  flush t.oc

let append_entry t entry =
  t.entries <- t.entries @ [ entry ];
  output_string t.oc (Yojson.Safe.to_string entry);
  output_char t.oc '\n';
  flush t.oc

let entry_id () = "entry-" ^ gen_id ()

let timestamp () = Printf.sprintf "%.0f" (Unix.time ())

let append_custom_entry t custom_type data =
  let entry =
    `Assoc
      [ ("type", `String "custom");
        ("id", `String (entry_id ()));
        ("timestamp", `String (timestamp ()));
        ("customType", `String custom_type);
        ("data", data) ]
  in
  append_entry t entry

let append_label_change t target_id label =
  let label_field = match label with Some value -> `String value | None -> `Null in
  let entry =
    `Assoc
      [ ("type", `String "label");
        ("id", `String (entry_id ()));
        ("timestamp", `String (timestamp ()));
        ("targetId", `String target_id);
        ("label", label_field) ]
  in
  append_entry t entry

let append_leaf t ?parent_id target_id =
  let target_field = match target_id with Some id -> `String id | None -> `Null in
  let entry =
    `Assoc
      [ ("type", `String "leaf");
        ("id", `String (entry_id ()));
        ("parentId", (match parent_id with Some id -> `String id | None -> `Null));
        ("timestamp", `String (timestamp ()));
        ("targetId", target_field) ]
  in
  append_entry t entry;
  entry

let append_branch_summary t ?parent_id ?details ?from_hook ~from_id summary =
  let entry =
    `Assoc
      ([ ("type", `String "branch_summary");
         ("id", `String (entry_id ()));
         ("parentId", (match parent_id with Some id -> `String id | None -> `Null));
         ("timestamp", `String (timestamp ()));
         ("fromId", `String from_id);
         ("summary", `String summary) ]
      @ (match details with Some value -> [ ("details", value) ] | None -> [])
      @ (match from_hook with Some value -> [ ("fromHook", `Bool value) ] | None -> []))
  in
  append_entry t entry;
  entry

(* Truncate and rewrite the whole file (header + all turns). Used after
   compaction, naming, and forking.
   Writes to a temp file and atomically renames so the original is never
   in a partially-written state. *)
let save_all t (turns : Llm.turn list) =
  let tmp = t.path ^ ".tmp" in
  let oc = open_out tmp in
  try
    write_header oc t.id t.name t.created t.cwd;
    List.iter
      (fun turn ->
        output_string oc (Yojson.Safe.to_string (Llm.turn_to_json turn));
        output_char oc '\n')
      turns;
    List.iter
      (fun entry ->
        output_string oc (Yojson.Safe.to_string entry);
        output_char oc '\n')
      t.entries;
    flush oc;
    close_out oc;
    Sys.rename tmp t.path;
    let old_oc = t.oc in
    t.oc <- open_out_gen [ Open_append; Open_creat ] 0o644 t.path;
    close_out_noerr old_oc
  with e ->
    close_out_noerr oc;
    (try Sys.remove tmp with Sys_error _ -> ());
    raise e

let set_name t name turns =
  t.name <- name;
  save_all t turns

let close t = close_out_noerr t.oc

let info_of t : info = { id = t.id; path = t.path; name = t.name; created = t.created; cwd = t.cwd }

(* All sessions in the directory, newest first. *)
let list () : info list =
  let d = default_dir () in
  if not (try Sys.is_directory d with Sys.Break as e -> raise e | _ -> false) then []
  else
    Sys.readdir d |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".jsonl")
    |> List.filter_map (fun f -> read_header (Filename.concat d f))
    |> List.sort (fun (a : info) (b : info) -> compare b.created a.created)

let has_path_separator s = String.contains s '/' || String.contains s '\\'

let starts_with ~prefix s =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let find spec =
  if spec = "" then None
  else if Sys.file_exists spec || has_path_separator spec || Filename.check_suffix spec ".jsonl" then
    Some
      (match read_header spec with
       | Some i -> i
       | None ->
         { id = Filename.remove_extension (Filename.basename spec);
           path = spec;
           name = "";
           created = 0.;
           cwd = "" })
  else
    list ()
    |> List.find_opt (fun (i : info) -> i.id = spec || starts_with ~prefix:spec i.id || i.name = spec)

let resolve_path spec =
  match find spec with Some i -> Some i.path | None -> None

(* Duplicate [turns] into a brand-new session; returns the new session. *)
let clone_from ?(name = "") turns : t =
  let s = create_new ~name () in
  List.iter (append s) turns;
  s

let fork_from spec : t option =
  match resolve_path spec with
  | None -> None
  | Some path ->
    let turns = load_turns path in
    Some (clone_from turns)

let import_from path : (t * Llm.turn list, string) result =
  if not (Sys.file_exists path) then Error ("File not found: " ^ path)
  else if Sys.is_directory path then Error ("Import path is a directory: " ^ path)
  else
    let turns = load_turns path in
    let name =
      match read_header path with
      | Some i when String.trim i.name <> "" -> i.name
      | Some i when String.trim i.id <> "" -> i.id
      | _ -> Filename.remove_extension (Filename.basename path)
    in
    let session = clone_from ~name turns in
    Ok (session, turns)

let export_jsonl turns path =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      List.iter
        (fun turn ->
          output_string oc (Yojson.Safe.to_string (Llm.turn_to_json turn));
          output_char oc '\n')
        turns)

let html_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '&' -> Buffer.add_string b "&amp;"
      | '"' -> Buffer.add_string b "&quot;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let export_html (i : info) turns path =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      Printf.fprintf oc
        "<!doctype html><meta charset=utf-8><title>%s</title><style>body{font:14px/1.5 monospace;max-width:900px;margin:2rem auto;padding:0 1rem}.u{color:#06c}.a{color:#080}pre{background:#f4f4f4;padding:.5rem;white-space:pre-wrap}</style><h1>%s</h1>"
        (html_escape (if i.name = "" then i.id else i.name))
        (html_escape (if i.name = "" then i.id else i.name));
      List.iter
        (fun (turn : Llm.turn) ->
          let role, cls = match turn.Llm.role with User -> ("user", "u") | Assistant -> ("assistant", "a") in
          Printf.fprintf oc "<h3 class=%s>%s</h3>" cls role;
          List.iter
            (fun c ->
              let text =
                match c with
                | Llm.Text s -> s
                | Llm.Image { mime_type; _ } -> "[image] " ^ mime_type
                | Llm.Thinking { text; _ } -> "[thinking] " ^ text
                | Llm.Tool_use { name; input; _ } -> Printf.sprintf "[tool %s] %s" name (Yojson.Safe.to_string input)
                | Llm.Tool_result { content; _ } -> "[result]\n" ^ content
              in
              Printf.fprintf oc "<pre>%s</pre>" (html_escape text))
            turn.Llm.content)
        turns;
      output_string oc "</body></html>")
