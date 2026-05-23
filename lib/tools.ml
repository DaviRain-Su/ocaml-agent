(* Tool definitions and executors for the code agent.

   Each tool exposes:
   - a [schema] (the JSON the Anthropic API expects in the `tools` array)
   - an [execute] function mapping the model's JSON input to a result string. *)

open Yojson.Safe.Util

type tool =
  { name : string;
    description : string;
    parameters : Yojson.Safe.t; (* JSON Schema object for the tool's input *)
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

let str_field obj key = obj |> member key |> to_string

let opt_str_field obj key =
  match obj |> member key with `String s -> Some s | _ -> None

(* Directories never descended into during recursive search. *)
let skip_dirs = [ ".git"; "_build"; "node_modules"; ".hg"; ".svn"; "dist"; "target"; ".venv" ]

(* Recursively visit every regular file under [root], calling [f relpath]. The
   path passed to [f] is relative to [root] (or [root] itself for a file). *)
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

let edit_file =
  { name = "edit_file";
    description =
      "Edit a file by exact substring replacement. Provide a single old_str/new_str, \
       or an `edits` array of {old_str,new_str} applied in order. Each old_str must \
       match exactly (including whitespace); the first occurrence is replaced. Returns \
       a diff of the changes.";
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
            let diff = Buffer.create 256 in
            let rec apply i s = function
              | [] -> Ok s
              | (o, n) :: rest -> (
                match replace_first s o n with
                | None -> Error (Printf.sprintf "edit %d: old_str not found in %s" (i + 1) path)
                | Some s' ->
                  Buffer.add_string diff (Printf.sprintf "@@ change %d @@\n" (i + 1));
                  if o <> "" then Buffer.add_string diff (prefix_lines "-" o ^ "\n");
                  if n <> "" then Buffer.add_string diff (prefix_lines "+" n ^ "\n");
                  apply (i + 1) s' rest)
            in
            match apply 0 content edits with
            | Error e -> "Error: " ^ e
            | Ok updated ->
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

(* --- run_bash --- *)

(* Prefix that enforces a 5-minute timeout, if a timeout binary is available. *)
let timeout_prefix =
  lazy
    (if Sys.command "command -v timeout >/dev/null 2>&1" = 0 then "timeout 300 "
     else if Sys.command "command -v gtimeout >/dev/null 2>&1" = 0 then "gtimeout 300 "
     else "")

let run_bash =
  { name = "run_bash";
    description =
      "Run a bash command in the current working directory and return its combined \
       stdout and stderr. Use for building, testing, searching, and inspecting the \
       project. Long-running commands are killed after 5 minutes.";
    parameters =
      params ~props:[ ("command", strprop "The bash command to execute.") ] ~required:[ "command" ];
    execute =
      (fun input ->
        let command = str_field input "command" in
        (* Wrap with timeout to prevent indefinite hangs — but only if a timeout
           binary exists (macOS has neither by default; coreutils ships gtimeout). *)
        let wrapped = Printf.sprintf "%ssh -c %s 2>&1" (Lazy.force timeout_prefix) (Filename.quote command) in
        let ic = Unix.open_process_in wrapped in
        let buf = Buffer.create 4096 in
        (try
           while true do
             Buffer.add_channel buf ic 4096
           done
         with End_of_file -> ());
        let status = Unix.close_process_in ic in
        let code, out =
          match status with
          | Unix.WEXITED 124 ->
            (* timeout exits 124 when the command is killed. *)
            (124, Buffer.contents buf ^ "\n[Error: command timed out after 5 minutes]")
          | Unix.WEXITED c -> (c, Buffer.contents buf)
          | Unix.WSIGNALED s -> (128 + s, Buffer.contents buf)
          | Unix.WSTOPPED s -> (128 + s, Buffer.contents buf)
        in
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
                   raise Exit);
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
           with Exit -> ());
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
                 raise Exit);
               let candidate = if match_basename then Filename.basename rel else rel in
               if Str.string_match re candidate 0 && Str.match_end () = String.length candidate then begin
                 Buffer.add_string out (rel ^ "\n");
                 incr count
               end)
         with Exit -> ());
        if !count = 0 then "No files found."
        else Buffer.contents out ^ if !truncated then Printf.sprintf "\n(truncated at %d files)" find_limit else "") }

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
    (* Intercepted by the agent loop; never actually called. *)
    execute = (fun _ -> "Error: task tool must be handled by the agent") }

(* --- registry (extensible at startup) --- *)

let builtin = [ read_file; write_file; edit_file; list_dir; grep; find; run_bash; task ]

let registry = ref builtin

(* Register an extension-provided tool. Replaces any existing tool of the same name. *)
let register (t : tool) =
  registry := List.filter (fun x -> x.name <> t.name) !registry @ [ t ]

let all () = !registry

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

let anthropic_schemas () = List.map anthropic_schema !registry
let openai_schemas () = List.map openai_schema !registry

let find name = List.find_opt (fun t -> t.name = name) !registry
