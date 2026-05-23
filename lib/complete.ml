(* Autocomplete for the TUI input line: slash commands and file paths.
   Pure functions so they can be unit-tested without a terminal. *)

let commands =
  [ "/model"; "/think"; "/compact"; "/session"; "/sessions"; "/resume"; "/name"; "/clone";
    "/export"; "/copy"; "/new"; "/help"; "/exit"; "/quit" ]

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
  match Sys.readdir dir with
  | exception _ -> []
  | entries ->
    Array.to_list entries
    |> List.filter (fun e -> starts_with ~prefix:base e)
    |> List.sort compare
    |> List.map (fun e ->
           let shown = if String.contains tok '/' then Filename.concat dir e else e in
           if (try Sys.is_directory (Filename.concat dir e) with _ -> false) then shown ^ "/" else shown)

(* Completion candidates (full replacements for the token region of [input]). *)
let candidates input =
  let _, tok = token_of input in
  if String.length input > 0 && input.[0] = '/' && not (String.contains input ' ') then
    List.filter (fun c -> starts_with ~prefix:tok c) commands
  else path_candidates tok
