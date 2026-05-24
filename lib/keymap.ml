(* Configurable key bindings for the TUI. A small set of high-level actions can
   be rebound via .ocaml-agent/keybindings.json (or ~/.ocaml-agent/...), e.g.:
     { "quit": "ctrl+d", "model_picker": "ctrl+p", "settings": "ctrl+s" }
   Editing keys (arrows, backspace, Ctrl-A/E/U/K/W, Tab) are fixed. *)

type action = Quit | Model_picker | Settings | Help

(* A binding key is a (lowercased char, ctrl-held?) pair. *)
type t = ((char * bool) * action) list

let action_of_string = function
  | "quit" -> Some Quit
  | "model_picker" | "model" -> Some Model_picker
  | "settings" -> Some Settings
  | "help" -> Some Help
  | _ -> None

(* Parse a spec like "ctrl+p", "C-s", or "p" into (char, ctrl?). *)
let parse_spec s =
  let s = String.lowercase_ascii (String.trim s) in
  let ctrl, rest =
    if String.length s >= 5 && String.sub s 0 5 = "ctrl+" then (true, String.sub s 5 (String.length s - 5))
    else if String.length s >= 2 && String.sub s 0 2 = "c-" then (true, String.sub s 2 (String.length s - 2))
    else (false, s)
  in
  if String.length rest = 1 then Some (rest.[0], ctrl) else None

let default : t =
  [ (('d', true), Quit); (('c', true), Quit); (('p', true), Model_picker); (('s', true), Settings) ]

let config_paths () =
  let home = match Sys.getenv_opt "HOME" with Some h -> [ Filename.concat h ".ocaml-agent/keybindings.json" ] | None -> [] in
  ".ocaml-agent/keybindings.json" :: home

let load () : t =
  let path = List.find_opt Sys.file_exists (config_paths ()) in
  match path with
  | None -> default
  | Some p -> (
    try
      match
        let ic = open_in p in
        Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> Yojson.Safe.from_channel ic)
      with
      | `Assoc fields ->
        (* Overrides take precedence; start from defaults. *)
        List.fold_left
          (fun acc (k, v) ->
            match (action_of_string k, v) with
            | Some act, `String spec -> (
              match parse_spec spec with Some key -> (key, act) :: acc | None -> acc)
            | _ -> acc)
          default fields
      | _ -> default
    with
    | Sys.Break as e -> raise e
    | _ -> default)

let lookup (km : t) key = List.assoc_opt key km
