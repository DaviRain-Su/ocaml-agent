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

let opt_int_field obj key =
  match obj |> member key with
  | `Int n -> Some n
  | `Float f -> Some (int_of_float f)
  | _ -> None

let opt_bool_field obj key =
  match obj |> member key with
  | `Bool b -> Some b
  | `String s -> (
    match String.lowercase_ascii (String.trim s) with
    | "1" | "true" | "yes" | "y" -> Some true
    | "0" | "false" | "no" | "n" -> Some false
    | _ -> None)
  | _ -> None

let opt_str_field_any obj keys =
  List.find_map (fun key -> opt_str_field obj key) keys

let str_field_any obj keys =
  match opt_str_field_any obj keys with
  | Some s -> s
  | None -> failwith ("one of fields " ^ String.concat "/" keys ^ " must be a string")

let path_field obj = str_field_any obj [ "path"; "file_path" ]

let command_timeout_s = 300

(* Directories never descended into during recursive search. *)
let skip_dirs = [ ".git"; "_build"; "node_modules"; ".hg"; ".svn"; "dist"; "target"; ".venv" ]

(* Custom exception for flow control when a search limit is reached. *)
exception Limit_reached

(* Recursively visit every regular file under [root], calling [f relpath]. The
   path passed to [f] is relative to [root] (or [root] itself for a file).
   Raises Limit_reached if the caller wants to abort early (e.g. hit a result limit). *)
let walk ?(ignored = fun _ -> false) root (f : string -> unit) =
  let rec go rel =
    let full = if rel = "" then root else Filename.concat root rel in
    match Sys.is_directory full with
    | true ->
      let entries = try Sys.readdir full with Sys_error _ -> [||] in
      Array.sort compare entries;
      Array.iter
        (fun e ->
          let child = if rel = "" then e else Filename.concat rel e in
          if (not (List.mem e skip_dirs)) && not (ignored child) then go child)
        entries
    | false -> if not (ignored rel) then f rel
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

let string_ends_with s suffix =
  let sl = String.length s and pl = String.length suffix in
  sl >= pl && String.sub s (sl - pl) pl = suffix

let string_starts_with s prefix =
  let sl = String.length s and pl = String.length prefix in
  sl >= pl && String.sub s 0 pl = prefix

let trim_line s =
  let n = String.length s in
  let left = ref 0 in
  while !left < n && (s.[!left] = ' ' || s.[!left] = '\t' || s.[!left] = '\r') do
    incr left
  done;
  let right = ref (n - 1) in
  while !right >= !left && (s.[!right] = ' ' || s.[!right] = '\t' || s.[!right] = '\r') do
    decr right
  done;
  if !right < !left then "" else String.sub s !left (!right - !left + 1)

let glob_match pattern rel =
  let candidate = if String.contains pattern '/' then rel else Filename.basename rel in
  try
    let re = Str.regexp (glob_to_regex pattern) in
    Str.string_match re candidate 0 && Str.match_end () = String.length candidate
  with _ -> false

let load_gitignore root =
  let dir =
    try if Sys.is_directory root then root else Filename.dirname root with Sys_error _ -> Filename.dirname root
  in
  let path = Filename.concat dir ".gitignore" in
  if not (Sys.file_exists path) then []
  else
    try
      read_file_contents path |> String.split_on_char '\n'
      |> List.map trim_line
      |> List.filter (fun line -> line <> "" && not (string_starts_with line "#") && not (string_starts_with line "!"))
    with _ -> []

let ignored_by patterns rel =
  List.exists
    (fun pat ->
      if string_ends_with pat "/" then
        let dir = String.sub pat 0 (String.length pat - 1) in
        rel = dir || string_starts_with rel (dir ^ "/") || Filename.basename rel = dir
      else glob_match pat rel)
    patterns

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
let intprop description = `Assoc [ ("type", `String "number"); ("description", `String description) ]

(* --- read_file --- *)

let read_line_limit = 2000
let read_byte_limit = 50 * 1024

let drop_last = function
  | [] -> []
  | xs -> List.rev (List.tl (List.rev xs))

let file_lines content =
  let lines = String.split_on_char '\n' content in
  if String.length content > 0 && content.[String.length content - 1] = '\n' then drop_last lines else lines

let take n xs =
  let rec loop acc n = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: rest -> loop (x :: acc) (n - 1) rest
  in
  loop [] n xs

let drop n xs =
  let rec loop n xs =
    match (n, xs) with
    | n, _ when n <= 0 -> xs
    | _, [] -> []
    | n, _ :: rest -> loop (n - 1) rest
  in
  loop n xs

let take_under_bytes max_bytes lines =
  let rec loop acc bytes count = function
    | [] -> (List.rev acc, false)
    | line :: rest ->
      let add = String.length line + if acc = [] then 0 else 1 in
      if count > 0 && bytes + add > max_bytes then (List.rev acc, true)
      else if count = 0 && bytes + add > max_bytes then ([ String.sub line 0 (min (String.length line) max_bytes) ], true)
      else loop (line :: acc) (bytes + add) (count + 1) rest
  in
  loop [] 0 0 lines

let read_slice content ~offset ~limit =
  let lines = file_lines content in
  let total = List.length lines in
  let offset = max 1 offset in
  if offset > total && total > 0 then
    Error (Printf.sprintf "Error: Offset %d is beyond end of file (%d lines total)" offset total)
  else
    let start_idx = offset - 1 in
    let remaining = drop start_idx lines in
    let requested_limit = Option.value limit ~default:read_line_limit in
    let by_line = take requested_limit remaining in
    let by_line_truncated = List.length remaining > List.length by_line in
    let selected, by_byte_truncated = take_under_bytes read_byte_limit by_line in
    let output = String.concat "\n" selected in
    let shown = List.length selected in
    let next_offset = offset + shown in
    let note =
      if by_byte_truncated then
        Some
          (Printf.sprintf
             "[Showing lines %d-%d of %d (%dKB limit). Use offset=%d to continue.]"
             offset (max offset (next_offset - 1)) total (read_byte_limit / 1024) next_offset)
      else if by_line_truncated then
        match limit with
        | Some _ ->
          Some
            (Printf.sprintf "[%d more lines in file. Use offset=%d to continue.]" (total - next_offset + 1)
               next_offset)
        | None ->
          Some
            (Printf.sprintf "[Showing lines %d-%d of %d. Use offset=%d to continue.]" offset
               (next_offset - 1) total next_offset)
      else None
    in
    Ok (match note with Some n when output <> "" -> output ^ "\n" ^ n | Some n -> n | None -> output)

let read_file =
  { name = "read_file";
    description =
      "Read the contents of a file at the given relative or absolute path. Supports \
       optional 1-indexed offset and line limit; large files are truncated with a \
       continuation hint.";
    parameters =
      params
        ~props:
          [ ("path", strprop "Path to the file to read.");
            ("file_path", strprop "Legacy alias for path.");
            ("offset", intprop "Optional 1-indexed line number to start reading from.");
            ("limit", intprop "Optional maximum number of lines to read.") ]
        ~required:[ "path" ];
    requires_approval = false;
    execute =
      (fun input ->
        let path = path_field input in
        let offset = Option.value (opt_int_field input "offset") ~default:1 in
        let limit = opt_int_field input "limit" in
        try
          let content = read_file_contents path in
          let needs_line_mode = offset <> 1 || limit <> None in
          if not needs_line_mode && String.length content <= read_byte_limit && List.length (file_lines content) <= read_line_limit then
            content
          else
            match read_slice content ~offset ~limit with Ok s -> s | Error e -> e
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
            ("file_path", strprop "Legacy alias for path.");
            ("content", strprop "The full content to write to the file.") ]
        ~required:[ "path"; "content" ];
    requires_approval = false;
    execute =
      (fun input ->
        let path = path_field input in
        let content = str_field input "content" in
        try
          write_file_contents path content;
          Printf.sprintf "Wrote %d bytes to %s" (String.length content) path
        with Sys_error e -> "Error: " ^ e) }

(* --- edit_file: exact substring replacement(s) with a diff --- *)

type edit_input = { old_text : string; new_text : string }

type text_match =
  { found : bool;
    match_index : int;
    match_length : int;
    used_fuzzy : bool }

type matched_edit =
  { edit_index : int;
    index : int;
    length : int;
    replacement : string }

let find_substring s needle =
  if needle = "" then None
  else
    match Str.search_forward (Str.regexp_string needle) s 0 with
    | i -> Some i
    | exception Not_found -> None

let replace_at s index length replacement =
  String.sub s 0 index ^ replacement ^ String.sub s (index + length) (String.length s - index - length)

let count_substring s needle =
  if needle = "" then 0
  else
    let re = Str.regexp_string needle in
    let step = max 1 (String.length needle) in
    let rec loop pos count =
      match Str.search_forward re s pos with
      | i -> loop (i + step) (count + 1)
      | exception Not_found -> count
    in
    loop 0 0

let starts_with_at s i prefix =
  let n = String.length prefix in
  i + n <= String.length s && String.sub s i n = prefix

let strip_bom s =
  let bom = "\239\187\191" in
  if starts_with_at s 0 bom then (bom, String.sub s 3 (String.length s - 3)) else ("", s)

let detect_line_ending s =
  match (find_substring s "\n", find_substring s "\r\n") with
  | Some lf, Some crlf when crlf < lf -> `CRLF
  | _ -> `LF

let normalize_to_lf s =
  s |> Str.global_replace (Str.regexp_string "\r\n") "\n" |> Str.global_replace (Str.regexp_string "\r") "\n"

let restore_line_endings s = function
  | `LF -> s
  | `CRLF -> Str.global_replace (Str.regexp_string "\n") "\r\n" s

let normalize_unicode_variants s =
  let b = Buffer.create (String.length s) in
  let byte i = Char.code s.[i] in
  let add_ascii code = Buffer.add_char b (Char.chr code) in
  let rec loop i =
    if i >= String.length s then ()
    else if i + 2 < String.length s && s.[i] = 'e' && byte (i + 1) = 0xCC && byte (i + 2) = 0x81 then (
      Buffer.add_string b "\195\169";
      loop (i + 3))
    else if i + 2 < String.length s && s.[i] = 'E' && byte (i + 1) = 0xCC && byte (i + 2) = 0x81 then (
      Buffer.add_string b "\195\137";
      loop (i + 3))
    else if i + 1 < String.length s && byte i = 0xC2 && byte (i + 1) = 0xA0 then (
      Buffer.add_char b ' ';
      loop (i + 2))
    else if i + 2 < String.length s then (
      let b0 = byte i and b1 = byte (i + 1) and b2 = byte (i + 2) in
      if b0 = 0xEF && b1 = 0xBC && b2 >= 0x81 && b2 <= 0xBF then (
        add_ascii (b2 - 0x60);
        loop (i + 3))
      else if b0 = 0xEF && b1 = 0xBD && b2 >= 0x80 && b2 <= 0x9E then (
        add_ascii (b2 - 0x20);
        loop (i + 3))
      else if b0 = 0xE2 && b1 = 0x80 && List.mem b2 [ 0x98; 0x99; 0x9A; 0x9B ] then (
        Buffer.add_char b '\'';
        loop (i + 3))
      else if b0 = 0xE2 && b1 = 0x80 && List.mem b2 [ 0x9C; 0x9D; 0x9E; 0x9F ] then (
        Buffer.add_char b '"';
        loop (i + 3))
      else if b0 = 0xE2 && b1 = 0x80 && ((b2 >= 0x82 && b2 <= 0x8A) || b2 = 0xAF) then (
        Buffer.add_char b ' ';
        loop (i + 3))
      else if b0 = 0xE2 && b1 = 0x80 && b2 >= 0x90 && b2 <= 0x95 then (
        Buffer.add_char b '-';
        loop (i + 3))
      else if b0 = 0xE2 && b1 = 0x88 && b2 = 0x92 then (
        Buffer.add_char b '-';
        loop (i + 3))
      else if b0 = 0xE2 && b1 = 0x81 && b2 = 0x9F then (
        Buffer.add_char b ' ';
        loop (i + 3))
      else if b0 = 0xE3 && b1 = 0x80 && b2 = 0x80 then (
        Buffer.add_char b ' ';
        loop (i + 3))
      else (
        Buffer.add_char b s.[i];
        loop (i + 1)))
    else (
      Buffer.add_char b s.[i];
      loop (i + 1))
  in
  loop 0;
  Buffer.contents b

let trim_trailing_ascii_ws line =
  let rec last_non_ws i =
    if i < 0 then -1
    else
      match line.[i] with
      | ' ' | '\t' -> last_non_ws (i - 1)
      | _ -> i
  in
  let last = last_non_ws (String.length line - 1) in
  if last < 0 then "" else String.sub line 0 (last + 1)

let normalize_for_fuzzy_match s =
  normalize_unicode_variants s |> String.split_on_char '\n' |> List.map trim_trailing_ascii_ws
  |> String.concat "\n"

let fuzzy_find_text content old_text =
  match find_substring content old_text with
  | Some index -> { found = true; match_index = index; match_length = String.length old_text; used_fuzzy = false }
  | None ->
    let fuzzy_content = normalize_for_fuzzy_match content in
    let fuzzy_old = normalize_for_fuzzy_match old_text in
    (match find_substring fuzzy_content fuzzy_old with
     | Some index ->
       { found = true; match_index = index; match_length = String.length fuzzy_old; used_fuzzy = true }
     | None -> { found = false; match_index = -1; match_length = 0; used_fuzzy = false })

let count_occurrences content old_text =
  count_substring (normalize_for_fuzzy_match content) (normalize_for_fuzzy_match old_text)

let edit_not_found_error path i total =
  if total = 1 then
    Printf.sprintf "Could not find the exact text in %s. The old text must match exactly including all whitespace and newlines." path
  else
    Printf.sprintf "Could not find edits[%d] in %s. The oldText must match exactly including all whitespace and newlines." i path

let edit_duplicate_error path i total occurrences =
  if total = 1 then
    Printf.sprintf
      "Found %d occurrences of the text in %s. The text must be unique. Please provide more context to make it unique."
      occurrences path
  else
    Printf.sprintf
      "Found %d occurrences of edits[%d] in %s. Each oldText must be unique. Please provide more context to make it unique."
      occurrences i path

let edit_empty_old_text_error path i total =
  if total = 1 then Printf.sprintf "oldText must not be empty in %s." path
  else Printf.sprintf "edits[%d].oldText must not be empty in %s." i path

let edit_no_change_error path total =
  if total = 1 then
    Printf.sprintf
      "No changes made to %s. The replacement produced identical content. This might indicate an issue with special characters or the text not existing as expected."
      path
  else Printf.sprintf "No changes made to %s. The replacements produced identical content." path

(* Prefix every line of [s] with [p] for diff display. *)
let prefix_lines p s =
  let parts = String.split_on_char '\n' s in
  let parts = match List.rev parts with "" :: rest -> List.rev rest | _ -> parts in
  parts |> List.map (fun l -> p ^ l) |> String.concat "\n"

(* Apply edits to an in-memory string first so edit_file is atomic on failure. *)
let apply_edits_to_normalized content edits path =
  let edits = List.map (fun e -> { old_text = normalize_to_lf e.old_text; new_text = normalize_to_lf e.new_text }) edits in
  let total = List.length edits in
  let empty_index = List.find_index (fun e -> e.old_text = "") edits in
  match empty_index with
  | Some i -> Error (edit_empty_old_text_error path i total)
  | None ->
    let initial_matches = List.map (fun e -> fuzzy_find_text content e.old_text) edits in
    let base_content =
      if List.exists (fun m -> m.used_fuzzy) initial_matches then normalize_for_fuzzy_match content else content
    in
    let rec collect i acc = function
      | [] -> Ok (List.rev acc)
      | edit :: rest ->
        let m = fuzzy_find_text base_content edit.old_text in
        if not m.found then Error (edit_not_found_error path i total)
        else
          let occurrences = count_occurrences base_content edit.old_text in
          if occurrences > 1 then Error (edit_duplicate_error path i total occurrences)
          else
            collect (i + 1)
              ({ edit_index = i; index = m.match_index; length = m.match_length; replacement = edit.new_text } :: acc)
              rest
    in
    (match collect 0 [] edits with
     | Error e -> Error e
     | Ok matched ->
       let matched = List.sort (fun a b -> compare a.index b.index) matched in
       let rec check_overlap = function
         | a :: (b :: _ as rest) ->
           if a.index + a.length > b.index then
             Error
               (Printf.sprintf
                  "edits[%d] and edits[%d] overlap in %s. Merge them into one edit or target disjoint regions."
                  a.edit_index b.edit_index path)
           else check_overlap rest
         | _ -> Ok ()
       in
       (match check_overlap matched with
        | Error e -> Error e
        | Ok () ->
          let updated =
            matched |> List.rev
            |> List.fold_left (fun s edit -> replace_at s edit.index edit.length edit.replacement) base_content
          in
          if updated = base_content then Error (edit_no_change_error path total) else Ok (base_content, updated)))

let edit_item e =
  { old_text = str_field_any e [ "oldText"; "old_str"; "old_text" ];
    new_text = str_field_any e [ "newText"; "new_str"; "new_text" ] }

let edits_array input =
  match input |> member "edits" with
  | `List l -> Some l
  | `String s -> (
    match Yojson.Safe.from_string s with
    | `List l -> Some l
    | _ -> None
    | exception _ -> None)
  | _ -> None

let edit_inputs input =
  let edits =
    match edits_array input with
    | Some l -> List.map edit_item l
    | None -> []
  in
  match
    ( opt_str_field_any input [ "oldText"; "old_str"; "old_text" ],
      opt_str_field_any input [ "newText"; "new_str"; "new_text" ] )
  with
  | Some old_text, Some new_text -> edits @ [ { old_text; new_text } ]
  | _ -> edits

let apply_edits content edits path =
  let bom, content = strip_bom content in
  let ending = detect_line_ending content in
  let normalized = normalize_to_lf content in
  match apply_edits_to_normalized normalized edits path with
  | Error e -> Error e
  | Ok (_base, updated) -> Ok (bom ^ restore_line_endings updated ending)

let edit_count_label n = if n = 1 then "change" else "changes"

let block_count_label n = if n = 1 then "block" else "blocks"

let edit_diff edits =
  let diff = Buffer.create 256 in
  List.iteri
    (fun i e ->
      Buffer.add_string diff (Printf.sprintf "@@ change %d @@\n" (i + 1));
      if e.old_text <> "" then Buffer.add_string diff (prefix_lines "-" e.old_text ^ "\n");
      if e.new_text <> "" then Buffer.add_string diff (prefix_lines "+" e.new_text ^ "\n"))
    edits;
  Buffer.contents diff

let edit_file =
  { name = "edit_file";
    description =
      "Edit a file by exact text replacement. Prefer the Pi-compatible `edits` \
       array of {oldText,newText}; legacy old_str/new_str and top-level \
       oldText/newText are also accepted. Each oldText must match a unique, \
       non-overlapping region of the original file. All edits are validated \
       before any are applied; if any edit fails validation, no changes are made.";
    parameters =
      params
        ~props:
          [ ("path", strprop "Path to the file to edit.");
            ("file_path", strprop "Legacy alias for path.");
            ( "edits",
              `Assoc
                [ ("type", `String "array");
                  ( "description",
                    `String
                      "One or more targeted replacements matched against the original file, not incrementally." );
                  ( "items",
                    `Assoc
                      [ ("type", `String "object");
                        ( "properties",
                          `Assoc
                            [ ( "oldText",
                                strprop
                                  "Exact text for one targeted replacement. Must be unique and non-overlapping." );
                              ("newText", strprop "Replacement text for this targeted edit.") ] );
                        ("required", `List [ `String "oldText"; `String "newText" ]) ] ) ] );
            ("oldText", strprop "Legacy single-edit exact text to find.");
            ("newText", strprop "Legacy single-edit replacement text.");
            ("old_str", strprop "Legacy OCaml-agent alias for oldText.");
            ("new_str", strprop "Legacy OCaml-agent alias for newText.") ]
        ~required:[ "path" ];
    requires_approval = false;
    execute =
      (fun input ->
        let path = path_field input in
        let edits = edit_inputs input in
        if edits = [] then "Error: edits must contain at least one replacement"
        else
          try
            let content = read_file_contents path in
            match apply_edits content edits path with
            | Error e -> "Error: " ^ e
            | Ok updated ->
              write_file_contents path updated;
              let count = List.length edits in
              Printf.sprintf "Successfully replaced %d %s in %s.\nEdited %s (%d %s):\n%s" count
                (block_count_label count) path path count (edit_count_label count) (edit_diff edits)
          with Sys_error e -> "Error: " ^ e) }

(* --- list_dir --- *)

let list_dir =
  { name = "list_dir";
    description =
      "List directory contents. Returns entries sorted alphabetically with '/' suffix \
       for directories. Includes dotfiles and supports an optional limit.";
    parameters =
      params
        ~props:
          [ ("path", strprop "Directory path. Defaults to the current directory if omitted.");
            ("limit", intprop "Maximum number of entries to return. Defaults to 500.") ]
        ~required:[];
    requires_approval = false;
    execute =
      (fun input ->
        let path = match opt_str_field input "path" with Some p -> p | None -> "." in
        let limit = max 1 (Option.value (opt_int_field input "limit") ~default:500) in
        try
          if not (Sys.file_exists path) then Printf.sprintf "Error: Path not found: %s" path
          else if not (Sys.is_directory path) then Printf.sprintf "Error: Not a directory: %s" path
          else
          let entries = Sys.readdir path in
          Array.sort
            (fun a b -> compare (String.lowercase_ascii a, a) (String.lowercase_ascii b, b))
            entries;
          let lines =
            Array.to_list entries
            |> take limit
            |> List.map (fun e ->
                   let full = Filename.concat path e in
                   if (try Sys.is_directory full with _ -> false) then e ^ "/" else e)
          in
          let truncated = Array.length entries > List.length lines in
          if lines = [] then "(empty directory)"
          else
            String.concat "\n" lines
            ^
            if truncated then
              Printf.sprintf "\n\n[%d entries limit reached. Use limit=%d for more]" limit (limit * 2)
            else ""
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

let configured_shell_path () =
  match Config_paths.first_env [ "AGENT_SHELL_PATH"; "PI_SHELL_PATH" ] with
  | Some path -> path
  | None -> (
    match Settings.string "shellPath" with
    | Some path -> path
    | None -> if Sys.file_exists "/bin/bash" then "/bin/bash" else "/bin/sh")

let configured_command_prefix () =
  match Config_paths.first_env [ "AGENT_SHELL_COMMAND_PREFIX"; "PI_SHELL_COMMAND_PREFIX" ] with
  | Some prefix -> Some prefix
  | None -> Settings.string "shellCommandPrefix"

let shell_argv shell command =
  [| Filename.basename shell; "-c"; command |]

let apply_command_prefix command =
  match configured_command_prefix () with
  | Some prefix when String.trim prefix <> "" -> prefix ^ "\n" ^ command
  | _ -> command

let run_process ?stdin_data ?(timeout_s = command_timeout_s) ?(use_shell_settings = false) command =
  let command = if use_shell_settings then apply_command_prefix command else command in
  let shell = if use_shell_settings then configured_shell_path () else "/bin/sh" in
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
    try Unix.create_process shell (shell_argv shell command) stdin_fd wr wr
    with e ->
      if stdin_fd <> Unix.stdin then close_noerr stdin_fd;
      close_noerr rd;
      close_noerr wr;
      Option.iter (fun p -> try Sys.remove p with Sys_error _ -> ()) !stdin_path;
      let msg = Printf.sprintf "Error: failed to start shell %s: %s" shell (Printexc.to_string e) in
      raise (Failure msg)
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
      params
        ~props:
          [ ("command", strprop "The bash command to execute.");
            ("timeout", `Assoc [ ("type", `String "number"); ("description", `String "Optional timeout in seconds.") ]) ]
        ~required:[ "command" ];
    requires_approval = true;
    execute =
      (fun input ->
        let command = str_field input "command" in
        let timeout_s = Option.value (opt_int_field input "timeout") ~default:command_timeout_s in
        let code, out = run_process ~timeout_s ~use_shell_settings:true command in
        Printf.sprintf "(exit %d)\n%s" code out) }

(* --- grep: regex search across files --- *)

let grep_default_limit = 100

let line_matches ~literal ~ignore_case pattern line =
  if literal then
    let pattern, line =
      if ignore_case then (String.lowercase_ascii pattern, String.lowercase_ascii line) else (pattern, line)
    in
    find_substring line pattern <> None
  else
    let re =
      try Some ((if ignore_case then Str.regexp_case_fold else Str.regexp) pattern) with _ -> None
    in
    match re with
    | None -> false
    | Some re -> (
      match Str.search_forward re line 0 with _ -> true | exception Not_found -> false)

let grep =
  { name = "grep";
    description =
      "Search file contents for a pattern. Supports regex or literal matching, \
       glob file filters, case-insensitive matching, context lines, and result limits.";
    parameters =
      params
        ~props:
          [ ("pattern", strprop "Search pattern (regex by default, literal when literal=true).");
            ("path", strprop "File or directory to search. Defaults to the current directory.");
            ("include", strprop "Legacy alias for glob.");
            ("glob", strprop "Optional filename glob to restrict which files are searched, e.g. \"*.ml\".");
            ("ignoreCase", `Assoc [ ("type", `String "boolean"); ("description", `String "Case-insensitive search.") ]);
            ("literal", `Assoc [ ("type", `String "boolean"); ("description", `String "Treat pattern as literal text.") ]);
            ("context", intprop "Number of lines to show before and after each match.");
            ("limit", intprop "Maximum number of matches to return. Defaults to 100.") ]
        ~required:[ "pattern" ];
    requires_approval = false;
    execute =
      (fun input ->
        let pattern = str_field input "pattern" in
        let root = match opt_str_field input "path" with Some p -> p | None -> "." in
        let glob = match opt_str_field input "glob" with Some g -> Some g | None -> opt_str_field input "include" in
        let ignore_case = Option.value (opt_bool_field input "ignoreCase") ~default:false in
        let literal = Option.value (opt_bool_field input "literal") ~default:false in
        let context = max 0 (Option.value (opt_int_field input "context") ~default:0) in
        let limit = max 1 (Option.value (opt_int_field input "limit") ~default:grep_default_limit) in
        let ignore_patterns = load_gitignore root in
        let ignored rel = ignored_by ignore_patterns rel in
        let file_ok rel =
          match glob with
          | None -> true
          | Some g -> glob_match g rel
        in
        if (not literal) && (try ignore ((if ignore_case then Str.regexp_case_fold else Str.regexp) pattern); false with _ -> true) then
          "Error: invalid regex pattern"
        else
          let out = Buffer.create 4096 in
          let count = ref 0 in
          let truncated = ref false in
          let max_size = 10_000_000 in
          (try
             walk ~ignored root (fun rel ->
                 if !count >= limit then (
                   truncated := true;
                   raise Limit_reached);
                 if file_ok rel then begin
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
                             if !count >= limit then (
                               truncated := true;
                               raise Limit_reached)
                             else if line_matches ~literal ~ignore_case pattern line then begin
                               let start = max 0 (idx - context) in
                               let finish = min (List.length lines - 1) (idx + context) in
                               for current = start to finish do
                                 let marker = if current = idx then ":" else "-" in
                                 let text = List.nth lines current in
                                 Buffer.add_string out
                                   (Printf.sprintf "%s%s%d%s %s\n" rel marker (current + 1) marker text)
                               done;
                               incr count;
                               if !count >= limit then (
                                 truncated := true;
                                 raise Limit_reached)
                             end)
                           lines
                       end
                 end)
           with Limit_reached -> ());
          if !count = 0 then "No matches found"
          else
            Buffer.contents out
            ^
            if !truncated then
              Printf.sprintf "\n[%d matches limit reached. Use limit=%d for more, or refine pattern]" limit (limit * 2)
            else "") }

(* --- find: locate files by glob --- *)

let find_default_limit = 1000

let find =
  { name = "find";
    description =
      "Find files by name using a glob pattern (*, **, ?), recursively under a path. \
       A pattern without '/' matches the file's basename; otherwise it matches the \
       path relative to the search root. Includes hidden files and applies simple \
       .gitignore rules.";
    parameters =
      params
        ~props:
          [ ("pattern", strprop "Glob pattern, e.g. \"*.ml\" or \"lib/**/*.mli\".");
            ("path", strprop "Directory to search under. Defaults to the current directory.");
            ("limit", intprop "Maximum number of results. Defaults to 1000.") ]
        ~required:[ "pattern" ];
    requires_approval = false;
    execute =
      (fun input ->
        let pattern = str_field input "pattern" in
        let root = match opt_str_field input "path" with Some p -> p | None -> "." in
        let limit = max 1 (Option.value (opt_int_field input "limit") ~default:find_default_limit) in
        let ignore_patterns = load_gitignore root in
        let ignored rel = ignored_by ignore_patterns rel in
        let out = Buffer.create 2048 in
        let count = ref 0 in
        let truncated = ref false in
        (try
           walk ~ignored root (fun rel ->
               if !count >= limit then (
                 truncated := true;
                 raise Limit_reached);
               if glob_match pattern rel then begin
                 Buffer.add_string out (rel ^ "\n");
                 incr count
               end)
         with Limit_reached -> ());
        if !count = 0 then "No files found matching pattern"
        else
          Buffer.contents out
          ^
          if !truncated then
            Printf.sprintf "\n[%d results limit reached. Use limit=%d for more, or refine pattern]" limit (limit * 2)
          else "") }

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

let wire_name name =
  match canonical_name name with
  | "read_file" -> "read"
  | "write_file" -> "write"
  | "edit_file" -> "edit"
  | "list_dir" -> "ls"
  | "run_bash" -> "bash"
  | other -> other

let canonical_names names =
  names
  |> List.map canonical_name
  |> List.filter (fun s -> s <> "")
  |> List.sort_uniq String.compare

let is_builtin_name name = List.mem (canonical_name name) builtin_names

let registry = ref builtin
let extension_registered_names : string list ref = ref []

let reset_extensions () =
  registry := builtin;
  extension_registered_names := []

(* Register an extension-provided tool. Pi lets extensions override built-ins. *)
let register (t : tool) =
  let name = canonical_name t.name in
  let t = { t with name } in
  registry := List.filter (fun x -> x.name <> name) !registry @ [ t ];
  extension_registered_names := name :: List.filter (fun existing -> existing <> name) !extension_registered_names;
  true

let all ?allowed () =
  match allowed with
  | None -> !registry
  | Some names ->
    let names = canonical_names names in
    List.filter (fun t -> List.mem t.name names) !registry

let extension_names () =
  !extension_registered_names
  |> List.filter (fun name -> List.exists (fun t -> t.name = name) !registry)
  |> List.map wire_name
  |> List.sort_uniq String.compare

let tool_info_json t =
  let source =
    if List.mem t.name !extension_registered_names then "extension"
    else if is_builtin_name t.name then "builtin"
    else "extension"
  in
  `Assoc
    [ ("name", `String (wire_name t.name));
      ("description", `String t.description);
      ("parameters", t.parameters);
      ( "sourceInfo",
        `Assoc
          [ ("path", `String ("<" ^ source ^ ":" ^ wire_name t.name ^ ">"));
            ("source", `String source);
            ("scope", `String "temporary");
            ("origin", `String "top-level") ] ) ]

let tool_infos ?allowed () = List.map tool_info_json (all ?allowed ())

let names ?allowed () = all ?allowed () |> List.map (fun t -> wire_name t.name)

(* Anthropic tool schema: {name, description, input_schema}. *)
let anthropic_schema t =
  `Assoc
    [ ("name", `String (wire_name t.name));
      ("description", `String t.description);
      ("input_schema", t.parameters) ]

(* OpenAI tool schema: {type:"function", function:{name, description, parameters}}. *)
let openai_schema t =
  `Assoc
    [ ("type", `String "function");
      ( "function",
        `Assoc
          [ ("name", `String (wire_name t.name));
            ("description", `String t.description);
            ("parameters", t.parameters) ] ) ]

let anthropic_schemas ?allowed () = List.map anthropic_schema (all ?allowed ())
let openai_schemas ?allowed () = List.map openai_schema (all ?allowed ())

let find name =
  let name = canonical_name name in
  List.find_opt (fun t -> t.name = name) !registry
