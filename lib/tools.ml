(* Tool definitions and executors for the code agent.

   Each tool exposes:
   - a [schema] (the JSON the Anthropic API expects in the `tools` array)
   - an [execute] function mapping the model's JSON input to a result string. *)

open Yojson.Safe.Util

type tool =
  { name : string;
    description : string;
    parameters : Yojson.Safe.t; (* JSON Schema object for the tool's input *)
    requires_approval : bool;
    execute : Yojson.Safe.t -> string }

(* --- helpers --- *)

let read_file_contents path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write_file_contents path content =
  (* Create parent directories if needed. *)
  let dir = Filename.dirname path in
  if dir <> "." && not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)));
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let str_field obj key =
  match obj |> member key with
  | `String s -> s
  | _ -> failwith ("field " ^ key ^ " must be a string")

let opt_str_field obj key =
  match obj |> member key with `String s -> Some s | _ -> None

let command_timeout_s = 300

(* Directories never descended into during recursive search. *)
let skip_dirs = [ ".git"; "_build"; "node_modules"; ".hg"; ".svn"; "dist"; "target"; ".venv" ]

(* Custom exception for flow control when a search limit is reached. *)
exception Limit_reached

(* Recursively visit every regular file under [root], calling [f relpath]. The
   path passed to [f] is relative to [root] (or [root] itself for a file).
   Raises Limit_reached if the caller wants to abort early (e.g. hit a result limit). *)
let walk root (f : string -> unit) =
  let rec go rel =
    let full = if rel = "" then root else Filename.concat root rel in
    match Sys.is_directory full with
    | true ->
      let entries = try Sys.readdir full with Sys_error _ -> [||] in
      Array.sort compare entries;
      Array.iter
        (fun e ->
          if not (List.mem e skip_dirs) then go (if rel = "" then e else Filename.concat rel e))
        entries
    | false -> f rel
    | exception Sys_error _ -> ()
  in
  if Sys.file_exists root then if Sys.is_directory root then go "" else f (Filename.basename root)

(* Convert a glob with star/doublestar/question wildcards to an anchored regex. *)
let glob_to_regex pat =
  let b = Buffer.create (String.length pat * 2) in
  Buffer.add_char b '^';
  let n = String.length pat in
  let i = ref 0 in
  while !i < n do
    (match pat.[!i] with
     | '*' ->
       if !i + 1 < n && pat.[!i + 1] = '*' then
         if !i + 2 < n && pat.[!i + 2] = '/' then (
           (* "**/" matches any number of path segments, including none *)
           Buffer.add_string b "\\(.*/\\)?";
           i := !i + 2)
         else (
           Buffer.add_string b ".*";
           incr i)
       else Buffer.add_string b "[^/]*"
     | '?' -> Buffer.add_string b "[^/]"
     | '.' -> Buffer.add_string b "\\."
     | '+' | '(' | ')' | '[' | ']' | '{' | '}' | '^' | '$' | '\\' | '|' as c ->
       Buffer.add_char b '\\';
       Buffer.add_char b c
     | c -> Buffer.add_char b c);
    incr i
  done;
  Buffer.add_char b '$';
  Buffer.contents b

let looks_binary s =
  let n = min (String.length s) 8000 in
  let rec scan i = if i >= n then false else if s.[i] = '\000' then true else scan (i + 1) in
  scan 0

(* --- schema builder --- *)

(* Build the JSON Schema object describing a tool's input parameters. *)
let params ~props ~required =
  `Assoc
    [ ("type", `String "object");
      ("properties", `Assoc props);
      ("required", `List (List.map (fun s -> `String s) required)) ]

let strprop description = `Assoc [ ("type", `String "string"); ("description", `String description) ]

(* --- read_file --- *)

let read_file =
  { name = "read_file";
    description = "Read the full contents of a file at the given relative or absolute path.";
    parameters = params ~props:[ ("path", strprop "Path to the file to read.") ] ~required:[ "path" ];
    requires_approval = false;
    execute =
      (fun input ->
        let path = str_field input "path" in
        try read_file_contents path
        with Sys_error e -> "Error: " ^ e) }

(* --- write_file --- *)

let write_file =
  { name = "write_file";
    description =
      "Write content to a file, overwriting it if it exists. Creates parent \
       directories as needed.";
    parameters =
      params
        ~props:
          [ ("path", strprop "Path to the file to write.");
            ("content", strprop "The full content to write to the file.") ]
        ~required:[ "path"; "content" ];
    requires_approval = false;
    execute =
      (fun input ->
        let path = str_field input "path" in
        let content = str_field input "content" in
        try
          write_file_contents path content;
          Printf.sprintf "Wrote %d bytes to %s" (String.length content) path
        with Sys_error e -> "Error: " ^ e) }

(* --- edit_file: exact substring replacement(s) with a diff --- *)

(* Replace the first occurrence of [old_] with [new_] in [s]; None if absent. *)
let replace_first s old_ new_ =
  if old_ = "" then None
  else
    match Str.search_forward (Str.regexp_string old_) s 0 with
    | i ->
      let before = String.sub s 0 i in
      let after = String.sub s (i + String.length old_) (String.length s - i - String.length old_) in
      Some (before ^ new_ ^ after)
    | exception Not_found -> None

(* Prefix every line of [s] with [p] for diff display. *)
let prefix_lines p s =
  let parts = String.split_on_char '\n' s in
  let parts = match List.rev parts with "" :: rest -> List.rev rest | _ -> parts in
  parts |> List.map (fun l -> p ^ l) |> String.concat "\n"

(* Apply edits to an in-memory string first so edit_file is atomic on failure. *)
let apply_edits content edits =
  let rec check i s = function
    | [] -> Ok s
    | (o, n) :: rest -> (
      match replace_first s o n with
      | None -> Error (Printf.sprintf "edit %d: old_str not found in file" (i + 1))
      | Some s' -> check (i + 1) s' rest)
  in
  check 0 content edits

let edit_file =
  { name = "edit_file";
    description =
      "Edit a file by exact substring replacement. Provide a single old_str/new_str, \
       or an `edits` array of {old_str,new_str} applied in order. Each old_str must \
       match exactly (including whitespace); the first occurrence is replaced. All \
       edits are validated before any are applied; if any edit fails validation, \
       no changes are made. Returns a diff of the changes.";
    parameters =
      params
        ~props:
          [ ("path", strprop "Path to the file to edit.");
            ("old_str", strprop "Exact text to find (single-edit form).");
            ("new_str", strprop "Replacement text (single-edit form).");
            ( "edits",
              `Assoc
                [ ("type", `String "array");
                  ("description", `String "Multiple edits applied in order.");
                  ( "items",
                    `Assoc
                      [ ("type", `String "object");
                        ( "properties",
                          `Assoc
                            [ ("old_str", strprop "Exact text to find.");
                              ("new_str", strprop "Replacement text.") ] );
                        ("required", `List [ `String "old_str"; `String "new_str" ]) ] ) ] ) ]
        ~required:[ "path" ];
    requires_approval = false;
    execute =
      (fun input ->
        let path = str_field input "path" in
        let edits =
          match input |> member "edits" with
          | `List l -> List.map (fun e -> (str_field e "old_str", str_field e "new_str")) l
          | _ -> (
            match (opt_str_field input "old_str", opt_str_field input "new_str") with
            | Some o, Some n -> [ (o, n) ]
            | _ -> [])
        in
        if edits = [] then "Error: provide old_str/new_str or a non-empty edits array"
        else
          try
            let content = read_file_contents path in
            match apply_edits content edits with
            | Error e -> "Error: " ^ e
            | Ok updated ->
              let diff = Buffer.create 256 in
              List.iteri
                (fun i (o, n) ->
                  Buffer.add_string diff (Printf.sprintf "@@ change %d @@\n" (i + 1));
                  if o <> "" then Buffer.add_string diff (prefix_lines "-" o ^ "\n");
                  if n <> "" then Buffer.add_string diff (prefix_lines "+" n ^ "\n"))
                edits;
              write_file_contents path updated;
              Printf.sprintf "Edited %s (%d change%s):\n%s" path (List.length edits)
                (if List.length edits = 1 then "" else "s")
                (Buffer.contents diff)
          with Sys_error e -> "Error: " ^ e) }

(* --- list_dir --- *)

let list_dir =
  { name = "list_dir";
    description = "List the entries (files and directories) in a directory.";
    parameters =
      params
        ~props:[ ("path", strprop "Directory path. Defaults to the current directory if omitted.") ]
        ~required:[];
    requires_approval = false;
    execute =
      (fun input ->
        let path = match opt_str_field input "path" with Some p -> p | None -> "." in
        try
          let entries = Sys.readdir path in
          Array.sort compare entries;
          let lines =
            Array.to_list entries
            |> List.map (fun e ->
                   let full = Filename.concat path e in
                   if (try Sys.is_directory full with _ -> false) then e ^ "/" else e)
          in
          if lines = [] then "(empty)" else String.concat "\n" lines
        with Sys_error e -> "Error: " ^ e) }

(* --- subprocess runner --- *)

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

let write_temp_stdin content =
  let path = Filename.temp_file "agent_stdin" ".json" in
  try
    let oc = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc content);
    path
  with e ->
    (try Sys.remove path with Sys_error _ -> ());
    raise e

let run_process ?stdin_data ?(timeout_s = command_timeout_s) command =
  let stdin_path = ref None in
  let stdin_fd =
    match stdin_data with
    | None -> Unix.stdin
    | Some content ->
      let path = write_temp_stdin content in
      stdin_path := Some path;
      Unix.openfile path [ Unix.O_RDONLY ] 0
  in
  let rd, wr = Unix.pipe () in
  let pid =
    try Unix.create_process "/bin/sh" [| "sh"; "-c"; command |] stdin_fd wr wr
    with e ->
      if stdin_fd <> Unix.stdin then close_noerr stdin_fd;
      close_noerr rd;
      close_noerr wr;
      Option.iter (fun p -> try Sys.remove p with Sys_error _ -> ()) !stdin_path;
      raise e
  in
  if stdin_fd <> Unix.stdin then close_noerr stdin_fd;
  close_noerr wr;
  Unix.set_nonblock rd;
  let buf = Buffer.create 4096 in
  let bytes = Bytes.create 4096 in
  let rec drain () =
    match Unix.read rd bytes 0 (Bytes.length bytes) with
    | 0 -> ()
    | n ->
      Buffer.add_subbytes buf bytes 0 n;
      drain ()
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> drain ()
  in
  let deadline = Unix.gettimeofday () +. float_of_int timeout_s in
  let timed_out = ref false in
  let status = ref None in
  let rec wait_loop () =
    drain ();
    (match Unix.waitpid [ Unix.WNOHANG ] pid with
     | 0, _ -> ()
     | _, s -> status := Some s
     | exception Unix.Unix_error (Unix.EINTR, _, _) -> ());
    match !status with
    | Some _ -> ()
    | None ->
      let now = Unix.gettimeofday () in
      if now >= deadline then begin
        timed_out := true;
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        (match Unix.waitpid [] pid with
         | _, s -> status := Some s
         | exception Unix.Unix_error _ -> status := Some (Unix.WEXITED 124));
        drain ()
      end
      else begin
        let timeout = min 0.1 (deadline -. now) in
        ignore (Unix.select [ rd ] [] [] timeout);
        wait_loop ()
      end
  in
  Fun.protect
    ~finally:(fun () ->
      close_noerr rd;
      Option.iter (fun p -> try Sys.remove p with Sys_error _ -> ()) !stdin_path)
    (fun () ->
      wait_loop ();
      let out =
        Buffer.contents buf
        ^
        if !timed_out then "\n[Error: command timed out after 5 minutes]" else ""
      in
      let code =
        if !timed_out then 124
        else
          match !status with
          | Some (Unix.WEXITED c) -> c
          | Some (Unix.WSIGNALED s) -> 128 + s
          | Some (Unix.WSTOPPED s) -> 128 + s
          | None -> 1
      in
      (code, out))

(* --- run_bash --- *)

let run_bash =
  { name = "run_bash";
    description =
      "Run a bash command in the current working directory and return its combined \
       stdout and stderr. Use for building, testing, searching, and inspecting the \
       project. Long-running commands are killed after 5 minutes.";
    parameters =
      params ~props:[ ("command", strprop "The bash command to execute.") ] ~required:[ "command" ];
    requires_approval = true;
    execute =
      (fun input ->
        let command = str_field input "command" in
        let code, out = run_process command in
        Printf.sprintf "(exit %d)\n%s" code out) }

(* --- grep: regex search across files --- *)

let grep_limit = 200

let grep =
  { name = "grep";
    description =
      "Search file contents by regular expression, recursively under a path. Returns \
       matching lines as path:line:text. Skips binary files and common vendor dirs \
       (.git, _build, node_modules, ...).";
    parameters =
      params
        ~props:
          [ ("pattern", strprop "Regular expression to search for (OCaml Str syntax).");
            ("path", strprop "File or directory to search. Defaults to the current directory.");
            ("include", strprop "Optional filename glob to restrict which files are searched, e.g. \"*.ml\".") ]
        ~required:[ "pattern" ];
    requires_approval = false;
    execute =
      (fun input ->
        let pattern = str_field input "pattern" in
        let root = match opt_str_field input "path" with Some p -> p | None -> "." in
        let include_re =
          match opt_str_field input "include" with
          | Some g -> Some (Str.regexp (glob_to_regex g))
          | None -> None
        in
        match Str.regexp pattern with
        | exception _ -> "Error: invalid regex pattern"
        | re ->
          let out = Buffer.create 4096 in
          let count = ref 0 in
          let truncated = ref false in
          let max_size = 10_000_000 in
          (try
             walk root (fun rel ->
                 if !count >= grep_limit then (
                   truncated := true;
                   raise Limit_reached);
                 let name_ok =
                   match include_re with
                   | None -> true
                   | Some r -> Str.string_match r (Filename.basename rel) 0
                 in
                 if name_ok then begin
                   let full = if Sys.is_directory root then Filename.concat root rel else root in
                   let size = try (Unix.stat full).Unix.st_size with _ -> 0 in
                   if size > max_size then ()
                   else
                     match read_file_contents full with
                     | exception _ -> ()
                     | content ->
                       if not (looks_binary content) then begin
                         let lines = String.split_on_char '\n' content in
                         List.iteri
                           (fun idx line ->
                             if !count < grep_limit then (
                               match Str.search_forward re line 0 with
                               | _ ->
                                 Buffer.add_string out (Printf.sprintf "%s:%d:%s\n" rel (idx + 1) line);
                                 incr count
                               | exception Not_found -> ())
                             else truncated := true)
                           lines
                       end
                 end)
           with Limit_reached -> ());
          if !count = 0 then "No matches."
          else Buffer.contents out ^ if !truncated then Printf.sprintf "\n(truncated at %d matches)" grep_limit else "") }

(* --- find: locate files by glob --- *)

let find_limit = 500

let find =
  { name = "find";
    description =
      "Find files by name using a glob pattern (*, **, ?), recursively under a path. \
       A pattern without '/' matches the file's basename; otherwise it matches the \
       path relative to the search root. Skips common vendor dirs.";
    parameters =
      params
        ~props:
          [ ("pattern", strprop "Glob pattern, e.g. \"*.ml\" or \"lib/**/*.mli\".");
            ("path", strprop "Directory to search under. Defaults to the current directory.") ]
        ~required:[ "pattern" ];
    requires_approval = false;
    execute =
      (fun input ->
        let pattern = str_field input "pattern" in
        let root = match opt_str_field input "path" with Some p -> p | None -> "." in
        let match_basename = not (String.contains pattern '/') in
        let re = Str.regexp (glob_to_regex pattern) in
        let out = Buffer.create 2048 in
        let count = ref 0 in
        let truncated = ref false in
        (try
           walk root (fun rel ->
               if !count >= find_limit then (
                 truncated := true;
                 raise Limit_reached);
               let candidate = if match_basename then Filename.basename rel else rel in
               if Str.string_match re candidate 0 && Str.match_end () = String.length candidate then begin
                 Buffer.add_string out (rel ^ "\n");
                 incr count
               end)
         with Limit_reached -> ());
        if !count = 0 then "No files found."
        else Buffer.contents out ^ if !truncated then Printf.sprintf "\n(truncated at %d files)" find_limit else "") }

(* --- veldt_init: initialize a Veldt scaffold project --- *)

let veldt_scaffold_dir =
  let exe_dir = Filename.dirname Sys.executable_name in
  (* Try to find the scaffold relative to the executable, then relative to cwd.
     The Veldt submodule is at scaffold/veldt/ and its OCaml scaffold is at
     scaffold/veldt/scaffold/ocaml/ *)
  let candidates =
    [ Filename.concat exe_dir "../scaffold/veldt/scaffold/ocaml";
      Filename.concat exe_dir "../../scaffold/veldt/scaffold/ocaml";
      Filename.concat exe_dir "../../../scaffold/veldt/scaffold/ocaml";
      "scaffold/veldt/scaffold/ocaml";
      Filename.concat (Sys.getcwd ()) "scaffold/veldt/scaffold/ocaml" ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some d -> if Filename.is_relative d then Filename.concat (Sys.getcwd ()) d else d
  | None -> ""

let copy_dir src dst =
  let rec go rel =
    let src_full = if rel = "" then src else Filename.concat src rel in
    let dst_full = if rel = "" then dst else Filename.concat dst rel in
    match Sys.is_directory src_full with
    | true ->
      if not (Sys.file_exists dst_full) then
        ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dst_full)));
      let entries = try Sys.readdir src_full with Sys_error _ -> [||] in
      Array.iter (fun e -> go (if rel = "" then e else Filename.concat rel e)) entries
    | false ->
      let content = read_file_contents src_full in
      write_file_contents dst_full content
    | exception Sys_error _ -> ()
  in
  if Sys.file_exists src then go ""

let veldt_init =
  { name = "veldt_init";
    description =
      "Initialize a Veldt scaffold project for building programming language \
       interpreters, compilers, or DSLs in OCaml. Copies the Veldt scaffold \
       (lexer, parser, value system, environment, etc.) to the target directory. \
       You then write bin/main.ml to implement the interpreter core.";
    parameters =
      params
        ~props:
          [ ("path", strprop "Directory to create the project in. Created if it doesn't exist.");
            ( "lang",
              strprop "Target language hint (e.g. lua, jq, php, typst). Used to customize the main.ml stub." ) ]
        ~required:[ "path" ];
    requires_approval = false;
    execute =
      (fun input ->
        let path = str_field input "path" in
        let lang_hint = match opt_str_field input "lang" with Some l -> l | None -> "interp" in
        if veldt_scaffold_dir = "" then
          "Error: Veldt scaffold not found. Expected at scaffold/veldt/ relative to the executable."
        else
          try
            copy_dir veldt_scaffold_dir path;
            (* Customize the main.ml stub based on language hint *)
            let main_ml = Filename.concat path "bin/main.ml" in
            let stub =
              Printf.sprintf
                "(* main.ml — %s interpreter/compiler stub\n\n\
                 Replace this with your implementation. Use `open Lang` to access:\n\
                 - Lexer: configurable tokenizer\n\
                 - Pratt: table-driven parser\n\
                 - Value: runtime type system\n\
                 - Env: scoped variable environments\n\
                 - Interp: error handling infrastructure\n\
                 *)\n\n\
                 open Lang\n\n\
                 let () =\n\
                 \040\040Printf.eprintf \"Not yet implemented: %s interpreter\\n\";\n\
                 \040\040exit 1\n"
                lang_hint lang_hint
            in
            write_file_contents main_ml stub;
            Printf.sprintf "Initialized Veldt project at %s for %s\n\
                             Scaffold: %d files copied from %s\n\
                             Write your implementation in: %s/bin/main.ml\n\
                             Build: cd %s && eval $(opam env) && dune build"
              path lang_hint
              (Array.length (Sys.readdir (Filename.concat path "lib")))
              veldt_scaffold_dir
              path path
          with Sys_error e -> "Error: " ^ e) }

(* --- task: spawn a sub-agent (executed by the agent loop, not here) --- *)

let task =
  { name = "task";
    description =
      "Delegate a self-contained sub-task to a fresh sub-agent with its own tool loop. \
       Give a complete, standalone instruction; the sub-agent cannot see this \
       conversation. Returns the sub-agent's final answer. Use for focused research or \
       multi-step work you want to isolate.";
    parameters =
      params ~props:[ ("prompt", strprop "The full, self-contained task for the sub-agent.") ] ~required:[ "prompt" ];
    requires_approval = false;
    (* Intercepted by the agent loop; never actually called. *)
    execute = (fun _ -> "Error: task tool must be handled by the agent") }

(* --- registry (extensible at startup) --- *)

let builtin = [ read_file; write_file; edit_file; list_dir; grep; find; run_bash; veldt_init; task ]
let builtin_names = List.map (fun t -> t.name) builtin

let canonical_name name =
  match String.lowercase_ascii (String.trim name) with
  | "read" -> "read_file"
  | "write" -> "write_file"
  | "edit" -> "edit_file"
  | "ls" -> "list_dir"
  | "bash" -> "run_bash"
  | "subagent" -> "task"
  | other -> other

let canonical_names names =
  names
  |> List.map canonical_name
  |> List.filter (fun s -> s <> "")
  |> List.sort_uniq String.compare

let is_builtin_name name = List.mem (canonical_name name) builtin_names

let registry = ref builtin

(* Register an extension-provided tool. Built-in names are reserved. *)
let register (t : tool) =
  if is_builtin_name t.name then false
  else begin
    registry := List.filter (fun x -> x.name <> t.name) !registry @ [ t ];
    true
  end

let all ?allowed () =
  match allowed with
  | None -> !registry
  | Some names ->
    let names = canonical_names names in
    List.filter (fun t -> List.mem t.name names) !registry

(* Anthropic tool schema: {name, description, input_schema}. *)
let anthropic_schema t =
  `Assoc
    [ ("name", `String t.name);
      ("description", `String t.description);
      ("input_schema", t.parameters) ]

(* OpenAI tool schema: {type:"function", function:{name, description, parameters}}. *)
let openai_schema t =
  `Assoc
    [ ("type", `String "function");
      ( "function",
        `Assoc
          [ ("name", `String t.name);
            ("description", `String t.description);
            ("parameters", t.parameters) ] ) ]

let anthropic_schemas ?allowed () = List.map anthropic_schema (all ?allowed ())
let openai_schemas ?allowed () = List.map openai_schema (all ?allowed ())

let find name =
  let name = canonical_name name in
  List.find_opt (fun t -> t.name = name) !registry
