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

(* --- edit_file: replace an exact substring --- *)

let edit_file =
  { name = "edit_file";
    description =
      "Replace the first exact occurrence of old_str with new_str in a file. \
       old_str must match exactly (including whitespace) and appear exactly once \
       for a reliable edit.";
    parameters =
      params
        ~props:
          [ ("path", strprop "Path to the file to edit.");
            ("old_str", strprop "Exact text to find and replace.");
            ("new_str", strprop "Replacement text.") ]
        ~required:[ "path"; "old_str"; "new_str" ];
    execute =
      (fun input ->
        let path = str_field input "path" in
        let old_str = str_field input "old_str" in
        let new_str = str_field input "new_str" in
        try
          let content = read_file_contents path in
          (* find first occurrence of old_str *)
          let idx =
            try Some (Str.search_forward (Str.regexp_string old_str) content 0)
            with Not_found -> None
          in
          match idx with
          | None -> Printf.sprintf "Error: old_str not found in %s" path
          | Some i ->
            let before = String.sub content 0 i in
            let after =
              String.sub content
                (i + String.length old_str)
                (String.length content - i - String.length old_str)
            in
            let updated = before ^ new_str ^ after in
            write_file_contents path updated;
            Printf.sprintf "Edited %s" path
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

let run_bash =
  { name = "run_bash";
    description =
      "Run a bash command in the current working directory and return its combined \
       stdout and stderr. Use for building, testing, searching, and inspecting the \
       project.";
    parameters =
      params ~props:[ ("command", strprop "The bash command to execute.") ] ~required:[ "command" ];
    execute =
      (fun input ->
        let command = str_field input "command" in
        (* Redirect stderr into stdout so the model sees everything. *)
        let ic = Unix.open_process_in (Printf.sprintf "%s 2>&1" command) in
        let buf = Buffer.create 4096 in
        (try
           while true do
             Buffer.add_channel buf ic 4096
           done
         with End_of_file -> ());
        let status = Unix.close_process_in ic in
        let code =
          match status with
          | Unix.WEXITED c -> c
          | Unix.WSIGNALED s -> 128 + s
          | Unix.WSTOPPED s -> 128 + s
        in
        let out = Buffer.contents buf in
        Printf.sprintf "(exit %d)\n%s" code out) }

(* --- registry --- *)

let all = [ read_file; write_file; edit_file; list_dir; run_bash ]

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

let anthropic_schemas = List.map anthropic_schema all
let openai_schemas = List.map openai_schema all

let find name = List.find_opt (fun t -> t.name = name) all
