(* Frontend-agnostic implementations of the session-management slash commands.
   Each returns a short status string the caller renders however it likes. *)

let last_assistant_text agent =
  List.fold_left
    (fun acc (turn : Llm.turn) ->
      match turn.Llm.role with
      | Assistant ->
        let t = String.concat "\n" (List.filter_map (function Llm.Text s -> Some s | _ -> None) turn.Llm.content) in
        if String.trim t <> "" then Some t else acc
      | _ -> acc)
    None (Agent.turns agent)

let format_sessions () =
  match Session.list () with
  | [] -> "(no saved sessions)"
  | ss ->
    String.concat "\n"
      (List.mapi
         (fun i (s : Session.info) ->
           Printf.sprintf "%2d  %s  %s" (i + 1) s.id (if s.name = "" then "" else "(" ^ s.name ^ ")"))
         ss)

(* Resume by 1-based index (from /sessions) or by id/name. *)
let resume agent arg =
  let ss = Session.list () in
  let pick =
    match int_of_string_opt arg with
    | Some n -> List.nth_opt ss (n - 1)
    | None -> List.find_opt (fun (s : Session.info) -> s.id = arg || s.name = arg) ss
  in
  match pick with
  | None -> "No matching session: " ^ arg
  | Some (i : Session.info) ->
    let turns = Session.load_turns i.path in
    Agent.adopt_session agent ~turns (Some (Session.open_file i.path));
    Printf.sprintf "Resumed %s (%d turns)" i.id (List.length turns)

let name agent n =
  match Agent.session agent with
  | Some s -> Session.set_name s n (Agent.turns agent); "Named session: " ^ n
  | None -> "No active session to name."

let clone agent =
  let turns = Agent.turns agent in
  let s = Session.clone_from turns in
  Agent.adopt_session agent ~turns (Some s);
  "Cloned to new session " ^ s.Session.id

let export agent path =
  let turns = Agent.turns agent in
  if Filename.check_suffix path ".html" then begin
    let info =
      match Agent.session agent with
      | Some s -> Session.info_of s
      | None -> { Session.id = "export"; path; name = ""; created = 0.; cwd = "" }
    in
    Session.export_html info turns path;
    "Exported HTML to " ^ path
  end
  else begin
    Session.export_jsonl turns path;
    "Exported JSONL to " ^ path
  end

let copy agent =
  match last_assistant_text agent with
  | None -> "Nothing to copy."
  | Some text -> (
    try
      let oc = Unix.open_process_out "pbcopy" in
      Fun.protect
        ~finally:(fun () -> ignore (Unix.close_process_out oc))
        (fun () -> output_string oc text);
      "Copied last reply to clipboard."
    with _ -> "Clipboard copy failed (is pbcopy available?).")
