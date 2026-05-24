(* Expand @file mentions in a user message: any "@path" that names an existing
   file has its contents appended to the message as a fenced block, so the model
   sees both the reference and the file. Directories and non-existent paths are
   left untouched. *)

let mention_re = Str.regexp "@\\([^ \t\n]+\\)"

let max_bytes = 100_000

type image = { mime_type : string; data : string }

type expanded = { text : string; images : image list }

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let total = in_channel_length ic in
      let len = max 0 (min max_bytes total) in
      let s = really_input_string ic len in
      if total > max_bytes then s ^ "\n... (truncated)" else s)

(* Strip trailing punctuation that commonly follows a path in prose. *)
let trim_path p =
  let n = ref (String.length p) in
  while !n > 0 && String.contains ".,;:)]}" p.[!n - 1] do
    decr n
  done;
  String.sub p 0 !n

let file_block path content = Printf.sprintf "<file name=\"%s\">\n%s\n</file>\n" path content

let image_block path mime_type = Printf.sprintf "<file name=\"%s\">[Image: %s]</file>\n" path mime_type

let read_file_full path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let read_file_prefix path n =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = max 0 (min n (in_channel_length ic)) in
      really_input_string ic len)

let base64_table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let b64 n = base64_table.[n land 63]

let base64_encode s =
  let n = String.length s in
  let b = Buffer.create ((n + 2) / 3 * 4) in
  let i = ref 0 in
  while !i + 2 < n do
    let a = Char.code s.[!i] in
    let c = Char.code s.[!i + 1] in
    let d = Char.code s.[!i + 2] in
    Buffer.add_char b (b64 (a lsr 2));
    Buffer.add_char b (b64 (((a land 0x03) lsl 4) lor (c lsr 4)));
    Buffer.add_char b (b64 (((c land 0x0f) lsl 2) lor (d lsr 6)));
    Buffer.add_char b (b64 d);
    i := !i + 3
  done;
  if !i < n then begin
    let a = Char.code s.[!i] in
    Buffer.add_char b (b64 (a lsr 2));
    if !i + 1 < n then begin
      let c = Char.code s.[!i + 1] in
      Buffer.add_char b (b64 (((a land 0x03) lsl 4) lor (c lsr 4)));
      Buffer.add_char b (b64 ((c land 0x0f) lsl 2));
      Buffer.add_char b '='
    end
    else begin
      Buffer.add_char b (b64 ((a land 0x03) lsl 4));
      Buffer.add_char b '=';
      Buffer.add_char b '='
    end
  end;
  Buffer.contents b

let lowercase_ext path =
  let base = Filename.basename path in
  match String.rindex_opt base '.' with
  | None -> ""
  | Some i -> String.lowercase_ascii (String.sub base i (String.length base - i))

let starts_with s prefix =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let image_mime_type path =
  let magic =
    try
      let s = read_file_prefix path 12 in
      if starts_with s "\137PNG\r\n\026\n" then Some "image/png"
      else if starts_with s "\255\216\255" then Some "image/jpeg"
      else if starts_with s "GIF87a" || starts_with s "GIF89a" then Some "image/gif"
      else if String.length s >= 12 && String.sub s 0 4 = "RIFF" && String.sub s 8 4 = "WEBP" then
        Some "image/webp"
      else None
    with Sys.Break as e -> raise e | _ -> None
  in
  match magic with
  | Some _ as m -> m
  | None -> (
    match lowercase_ext path with
    | ".png" -> Some "image/png"
    | ".jpg" | ".jpeg" -> Some "image/jpeg"
    | ".gif" -> Some "image/gif"
    | ".webp" -> Some "image/webp"
    | _ -> None)

let image_of_file path mime_type =
  let max_image_bytes = 10 * 1024 * 1024 in
  let ic = open_in_bin path in
  let data =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        if len < 0 || len > max_image_bytes then failwith (Printf.sprintf "Image too large: %d bytes" len);
        really_input_string ic len)
  in
  { mime_type; data = base64_encode data }

let expand_file_args_rich paths =
  let text = Buffer.create 256 in
  let images = ref [] in
  List.iter
    (fun p ->
      if Sys.file_exists p && not (Sys.is_directory p) then
        match image_mime_type p with
        | Some mime_type -> (
          try
            let image = image_of_file p mime_type in
            images := image :: !images;
            Buffer.add_string text (image_block p mime_type)
          with
          | Sys.Break as e -> raise e
          | _ -> ())
        | None -> (
          try Buffer.add_string text (file_block p (read_file p)) with
          | Sys.Break as e -> raise e
          | _ -> ()))
    paths;
  { text = Buffer.contents text; images = List.rev !images }

let expand_file_args paths = (expand_file_args_rich paths).text

(* Collect referenced file paths (deduplicated, in first-seen order). *)
let referenced input =
  let paths = ref [] and i = ref 0 in
  (try
     while true do
       let start = Str.search_forward mention_re input !i in
       i := start + String.length (Str.matched_string input);
       let p = trim_path (Str.matched_group 1 input) in
       if p <> "" && (not (List.mem p !paths)) && (try Sys.file_exists p && not (Sys.is_directory p) with Sys.Break as e -> raise e | _ -> false)
       then paths := p :: !paths
     done
   with Not_found -> ());
  List.rev !paths

let expand_rich input =
  match referenced input with
  | [] -> { text = input; images = [] }
  | paths ->
    let buf = Buffer.create (String.length input + 256) in
    let images = ref [] in
    Buffer.add_string buf input;
    List.iter
      (fun p ->
        match image_mime_type p with
        | Some mime_type -> (
          try
            let image = image_of_file p mime_type in
            images := image :: !images;
            Buffer.add_string buf ("\n\n" ^ image_block p mime_type)
          with
          | Sys.Break as e -> raise e
          | _ -> ())
        | None -> (
          try Buffer.add_string buf ("\n\n" ^ file_block p (read_file p)) with
          | Sys.Break as e -> raise e
          | _ -> ()))
      paths;
    { text = Buffer.contents buf; images = List.rev !images }

let expand input = (expand_rich input).text
