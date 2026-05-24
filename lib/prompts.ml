(* Prompt templates: Markdown snippets invoked as slash commands, following the
   same core shape as pi's prompt templates. *)

type t =
  { name : string;
    description : string;
    argument_hint : string option;
    body : string;
    location : string }

let prompt_dirs () = Config_paths.uniq [ Config_paths.user_prompts_dir (); ".ocaml-agent/prompts"; ".pi/prompts" ]

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

let getenv_nonempty k =
  match Sys.getenv_opt k with Some s when String.trim s <> "" -> Some s | _ -> None

let split_lines s = String.split_on_char '\n' s

let parse_frontmatter content =
  let lines = split_lines content in
  match lines with
  | first :: rest when String.trim first = "---" ->
    let rec collect fm body = function
      | [] -> (List.rev fm, String.concat "\n" (List.rev body))
      | l :: tl when String.trim l = "---" -> (List.rev fm, String.concat "\n" tl)
      | l :: tl ->
        let fm =
          match String.index_opt l ':' with
          | Some i ->
            let k = String.trim (String.sub l 0 i) |> String.lowercase_ascii in
            let v = String.trim (String.sub l (i + 1) (String.length l - i - 1)) in
            (k, v) :: fm
          | None -> fm
        in
        collect fm body tl
    in
    collect [] [] rest
  | _ -> ([], content)

let first_nonempty_line body =
  body |> split_lines |> List.find_opt (fun s -> String.trim s <> "") |> Option.value ~default:""

let parse path =
  match read_file path with
  | exception _ -> None
  | content ->
    let fm, body = parse_frontmatter content in
    let name = Filename.remove_extension (Filename.basename path) in
    let description =
      match List.assoc_opt "description" fm with
      | Some d when d <> "" -> d
      | _ -> first_nonempty_line body
    in
    let argument_hint = match List.assoc_opt "argument-hint" fm with Some s when s <> "" -> Some s | _ -> None in
    Some { name; description; argument_hint; body; location = path }

let templates_from_dir dir =
  if (try Sys.is_directory dir with _ -> false) then
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".md")
    |> List.sort compare
    |> List.filter_map (fun f -> parse (Filename.concat dir f))
  else []

let templates_from_path path =
  if (try Sys.is_directory path with _ -> false) then templates_from_dir path
  else if Sys.file_exists path && Filename.check_suffix path ".md" then (match parse path with Some t -> [ t ] | None -> [])
  else []

let extra_paths () =
  match getenv_nonempty "AGENT_PROMPT_TEMPLATE_PATHS" with
  | None -> []
  | Some s -> s |> String.split_on_char '\n' |> List.map String.trim |> List.filter (fun p -> p <> "")

let settings_paths () = Settings.string_list "prompts"

let disabled () =
  match getenv_nonempty "AGENT_NO_PROMPT_TEMPLATES" with
  | Some ("1" | "true" | "yes" | "y") -> true
  | _ -> false

let discover () =
  if disabled () then List.concat_map templates_from_path (extra_paths ())
  else
    List.concat_map templates_from_dir (prompt_dirs ())
    @ List.concat_map templates_from_path (Packages.paths Packages.Prompt)
    @ List.concat_map templates_from_path (settings_paths ())
    @ List.concat_map templates_from_path (Extensions.prompt_paths ())
    @ List.concat_map templates_from_path (extra_paths ())

let menu () =
  discover ()
  |> List.map (fun p ->
         let detail =
           match p.argument_hint with
           | Some h -> if p.description = "" then h else h ^ " - " ^ p.description
           | None -> p.description
         in
         ("/" ^ p.name, detail))

let template_by_name name =
  discover () |> List.find_opt (fun p -> p.name = name)

let split_args s =
  let len = String.length s in
  let buf = Buffer.create len in
  let out = ref [] in
  let quote = ref None in
  let push () =
    if Buffer.length buf > 0 then begin
      out := Buffer.contents buf :: !out;
      Buffer.clear buf
    end
  in
  for i = 0 to len - 1 do
    match (!quote, s.[i]) with
    | Some q, c when c = q -> quote := None
    | Some _, c -> Buffer.add_char buf c
    | None, ('"' | '\'' as q) -> quote := Some q
    | None, (' ' | '\t') -> push ()
    | None, c -> Buffer.add_char buf c
  done;
  push ();
  List.rev !out

let shell_words line =
  let line = String.trim line in
  if line = "" then []
  else if line.[0] = '/' then split_args (String.sub line 1 (String.length line - 1))
  else split_args line

let join_args args = String.concat " " args

let slice args start len =
  let start = max 1 start in
  let rec drop n xs = if n <= 1 then xs else match xs with [] -> [] | _ :: tl -> drop (n - 1) tl in
  let xs = drop start args in
  let rec take n acc = function
    | _ when n = Some 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: tl -> take (Option.map (fun v -> v - 1) n) (x :: acc) tl
  in
  take len [] xs

let replace_regex re f s =
  let b = Buffer.create (String.length s) in
  let pos = ref 0 in
  let continue = ref true in
  while !continue do
    match Str.search_forward re s !pos with
    | exception Not_found -> continue := false
    | start ->
      Buffer.add_substring b s !pos (start - !pos);
      Buffer.add_string b (f s);
      pos := Str.match_end ()
  done;
  Buffer.add_substring b s !pos (String.length s - !pos);
  Buffer.contents b

let expand_body body args =
  let body =
    let re = Str.regexp "\\${@:\\([0-9][0-9]*\\):\\([0-9][0-9]*\\)}" in
    replace_regex re
      (fun source ->
        let start = Option.value (int_of_string_opt (Str.matched_group 1 source)) ~default:0 in
        let len = Option.map (fun v -> Option.value (int_of_string_opt v) ~default:0) (Some (Str.matched_group 2 source)) in
        join_args (slice args start len))
      body
  in
  let body =
    let re = Str.regexp "\\${@:\\([0-9][0-9]*\\)}" in
    replace_regex re
      (fun source ->
        let start = Option.value (int_of_string_opt (Str.matched_group 1 source)) ~default:0 in
        join_args (slice args start None))
      body
  in
  let body =
    let re = Str.regexp "\\$\\([0-9]+\\)" in
    replace_regex re
      (fun source ->
        let idx = Option.value (int_of_string_opt (Str.matched_group 1 source)) ~default:0 in
        List.nth_opt args (idx - 1) |> Option.value ~default:"")
      body
  in
  body
  |> Str.global_replace (Str.regexp_string "$ARGUMENTS") (join_args args)
  |> Str.global_replace (Str.regexp_string "$@") (join_args args)

let expand_command line =
  match shell_words line with
  | [] -> None
  | name :: args -> (
    match template_by_name name with
    | None -> None
    | Some t -> Some (expand_body t.body args))
