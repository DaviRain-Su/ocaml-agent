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
    mutable oc : out_channel }

let default_dir () =
  match Sys.getenv_opt "AGENT_SESSION_DIR" with
  | Some d when String.trim d <> "" -> d
  | _ -> ".ocaml-agent/sessions"

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
                | j -> (
                  match j |> member "_session" with
                  | `Null ->
                    (try Llm.turn_of_json j :: acc with _ -> acc)
                  | _ -> acc)
            in
            loop acc
        in
        loop [])
  end

(* legacy single-file helper kept for the old API name *)
let load = load_turns

let create_new ?(name = "") () : t =
  let d = dir () in
  let id = gen_id () in
  let path = Filename.concat d (id ^ ".jsonl") in
  let created = Unix.time () in
  let cwd = Sys.getcwd () in
  let oc = open_out path in
  write_header oc id name created cwd;
  { id; path; name; created; cwd; oc }

(* Open a specific file for appending (legacy AGENT_SESSION_FILE). Adds a header
   if the file is new/empty. *)
let open_file path : t =
  let parent = Filename.dirname path in
  if parent <> "." && not (Sys.file_exists parent) then ensure_dir parent;
  match read_header path with
  | Some i ->
    { id = i.id; path; name = i.name; created = i.created; cwd = i.cwd;
      oc = open_out_gen [ Open_append; Open_creat ] 0o644 path }
  | None ->
    let id = Filename.remove_extension (Filename.basename path) in
    let created = Unix.time () in
    let cwd = Sys.getcwd () in
    let exists_nonempty =
      try Sys.file_exists path && (Unix.stat path).Unix.st_size > 0
      with Unix.Unix_error _ -> false
    in
    let oc = open_out_gen [ Open_append; Open_creat ] 0o644 path in
    if not exists_nonempty then write_header oc id "" created cwd;
    { id; path; name = ""; created; cwd; oc }

let append t (turn : Llm.turn) =
  output_string t.oc (Yojson.Safe.to_string (Llm.turn_to_json turn));
  output_char t.oc '\n';
  flush t.oc

(* Truncate and rewrite the whole file (header + all turns). Used after
   compaction, naming, and forking. *)
let save_all t (turns : Llm.turn list) =
  close_out_noerr t.oc;
  let oc = open_out t.path in
  try
    write_header oc t.id t.name t.created t.cwd;
    List.iter
      (fun turn ->
        output_string oc (Yojson.Safe.to_string (Llm.turn_to_json turn));
        output_char oc '\n')
      turns;
    flush oc;
    t.oc <- oc
  with e ->
    close_out_noerr oc;
    (try t.oc <- open_out_gen [ Open_append; Open_creat ] 0o644 t.path with _ -> ());
    raise e

let set_name t name turns =
  t.name <- name;
  save_all t turns

let close t = close_out_noerr t.oc

let info_of t : info = { id = t.id; path = t.path; name = t.name; created = t.created; cwd = t.cwd }

(* All sessions in the directory, newest first. *)
let list () : info list =
  let d = default_dir () in
  if not (try Sys.is_directory d with _ -> false) then []
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
                | Llm.Thinking { text; _ } -> "[thinking] " ^ text
                | Llm.Tool_use { name; input; _ } -> Printf.sprintf "[tool %s] %s" name (Yojson.Safe.to_string input)
                | Llm.Tool_result { content; _ } -> "[result]\n" ^ content
              in
              Printf.fprintf oc "<pre>%s</pre>" (html_escape text))
            turn.Llm.content)
        turns)
