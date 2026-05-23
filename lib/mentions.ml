(* Expand @file mentions in a user message: any "@path" that names an existing
   file has its contents appended to the message as a fenced block, so the model
   sees both the reference and the file. Directories and non-existent paths are
   left untouched. *)

let mention_re = Str.regexp "@\\([^ \t\n]+\\)"

let max_bytes = 100_000

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = min max_bytes (in_channel_length ic) in
      let s = really_input_string ic len in
      if in_channel_length ic > max_bytes then s ^ "\n... (truncated)" else s)

(* Strip trailing punctuation that commonly follows a path in prose. *)
let trim_path p =
  let n = ref (String.length p) in
  while !n > 0 && String.contains ".,;:)]}" p.[!n - 1] do
    decr n
  done;
  String.sub p 0 !n

let file_block path content = Printf.sprintf "<file name=\"%s\">\n%s\n</file>\n" path content

let expand_file_args paths =
  let blocks =
    List.filter_map
      (fun p ->
        if Sys.file_exists p && not (Sys.is_directory p) then
          match read_file p with
          | content -> Some (file_block p content)
          | exception _ -> None
        else None)
      paths
  in
  String.concat "" blocks

(* Collect referenced file paths (deduplicated, in first-seen order). *)
let referenced input =
  let paths = ref [] and i = ref 0 in
  (try
     while true do
       let start = Str.search_forward mention_re input !i in
       i := start + String.length (Str.matched_string input);
       let p = trim_path (Str.matched_group 1 input) in
       if p <> "" && (not (List.mem p !paths)) && (try Sys.file_exists p && not (Sys.is_directory p) with _ -> false)
       then paths := p :: !paths
     done
   with Not_found -> ());
  List.rev !paths

let expand input =
  match referenced input with
  | [] -> input
  | paths ->
    let buf = Buffer.create (String.length input + 256) in
    Buffer.add_string buf input;
    List.iter
      (fun p ->
        match read_file p with
        | exception _ -> ()
        | content -> Buffer.add_string buf ("\n\n" ^ file_block p content))
      paths;
    Buffer.contents buf
