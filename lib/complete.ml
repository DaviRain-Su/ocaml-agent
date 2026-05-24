(* Autocomplete for the TUI input line: slash commands and file paths.
   Pure functions so they can be unit-tested without a terminal. *)

(* Slash commands with one-line help, used for the live completion menu. *)
let builtin_command_help =
  [ ("/model", "switch provider/model, or open the picker");
    ("/scoped-models", "show or set model picker scope");
    ("/think", "reasoning level (off/low/medium/high)");
    ("/compact", "summarize older turns");
    ("/session", "model, turns, context usage");
    ("/sessions", "list saved sessions");
    ("/resume", "resume a saved session");
    ("/name", "name the current session");
    ("/fork", "fork current or named session");
    ("/clone", "duplicate the current session");
    ("/export", "export session (.html/.jsonl)");
    ("/import", "import and resume a JSONL session");
    ("/copy", "copy last reply to clipboard");
    ("/changelog", "show changelog entries");
    ("/hotkeys", "show keyboard shortcuts");
    ("/reload", "reload prompt/resources");
    ("/settings", "toggle auto-approve / compact / thinking / theme");
    ("/new", "clear the conversation");
    ("/help", "show help");
    ("/exit", "quit");
    ("/quit", "quit") ]

let command_help () = builtin_command_help @ Prompts.menu () @ Extensions.command_menu ()

let commands () = List.map fst (command_help ())

let starts_with ~prefix s =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let common_prefix = function
  | [] -> ""
  | first :: rest ->
    let p = ref first in
    List.iter
      (fun s ->
        let n = ref 0 in
        let m = min (String.length !p) (String.length s) in
        while !n < m && !p.[!n] = s.[!n] do
          incr n
        done;
        p := String.sub !p 0 !n)
      rest;
    !p

(* The region of [input] to be replaced by a completion: (start_index, token).
   For a slash command (starts with '/', no space yet) it's the whole input;
   otherwise it's the last whitespace-separated token. *)
let token_of input =
  if String.length input > 0 && input.[0] = '/' && not (String.contains input ' ') then (0, input)
  else
    match String.rindex_opt input ' ' with
    | Some i -> (i + 1, String.sub input (i + 1) (String.length input - i - 1))
    | None -> (0, input)

let path_candidates tok =
  let dir = if String.contains tok '/' then Filename.dirname tok else "." in
  let base = Filename.basename tok in
  if base = "" then []
  else
    try
      let entries = Sys.readdir dir in
      Array.to_list entries
      |> List.filter (fun e -> starts_with ~prefix:base e)
      |> List.sort compare
      |> List.map (fun e ->
             let shown = if String.contains tok '/' then Filename.concat dir e else e in
             if (try Sys.is_directory (Filename.concat dir e) with Sys.Break as e -> raise e | _ -> false) then shown ^ "/" else shown)
    with
    | Sys.Break as e -> raise e
    | _ -> []

let slash_argument input =
  if String.length input > 0 && input.[0] = '/' then
    match String.index_opt input ' ' with
    | Some i when i > 1 ->
      let name = String.sub input 1 (i - 1) in
      let args = String.sub input (i + 1) (String.length input - i - 1) in
      Some (i + 1, name, args)
    | _ -> None
  else None

(* Matching (command, help) pairs when [input] is a slash-command prefix being
   typed (starts with '/', no space yet). Empty otherwise. *)
let menu input =
  if String.length input > 0 && input.[0] = '/' && not (String.contains input ' ') then
    List.filter (fun (c, _) -> starts_with ~prefix:input c) (command_help ())
  else []

(* Completion candidates (full replacements for the token region of [input]). *)
let completion input =
  match slash_argument input with
  | Some (start, name, args) -> (
    match Extensions.command_argument_completions name args with
    | [] ->
      let start, tok = token_of input in
      (start, path_candidates tok)
    | items -> (start, items))
  | None ->
    let start, tok = token_of input in
    if String.length input > 0 && input.[0] = '/' && not (String.contains input ' ') then
      (start, List.filter (fun c -> starts_with ~prefix:tok c) (commands ()))
    else (start, path_candidates tok)

let candidates input = snd (completion input)
