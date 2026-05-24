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

let starts_with ~prefix s =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let session_cancelled msg =
  starts_with ~prefix:"Session switch cancelled:" msg || starts_with ~prefix:"Session fork cancelled:" msg

let current_session_info agent = Option.map Session.info_of (Agent.session agent)

let before_switch agent ?target_session_file reason =
  let current = current_session_info agent in
  Extensions.emit_session_before_switch ~reason ?target_session_file
    ?current_session_file:(Option.map (fun (s : Session.info) -> s.path) current)
    ?current_session_id:(Option.map (fun (s : Session.info) -> s.id) current)
    ?current_session_name:(Option.map (fun (s : Session.info) -> s.name) current)
    ()

let before_fork agent ?source_session_file ?entry_id ?position reason =
  let current = current_session_info agent in
  Extensions.emit_session_before_fork ~reason ?source_session_file ?entry_id ?position
    ?current_session_file:(Option.map (fun (s : Session.info) -> s.path) current)
    ?current_session_id:(Option.map (fun (s : Session.info) -> s.id) current)
    ?current_session_name:(Option.map (fun (s : Session.info) -> s.name) current)
    ()

let emit_shutdown agent reason =
  match current_session_info agent with
  | None -> ()
  | Some s ->
    Extensions.emit_session_shutdown ~reason ~session_file:s.path ~session_id:s.id ~session_name:s.name ()

let emit_start ?previous_session_file session reason =
  let info = Session.info_of session in
  ignore
    (Extensions.emit_session_start ~reason ?previous_session_file ~session_file:info.path ~session_id:info.id
       ~session_name:info.name ())

let adopt_with_events agent ~reason ~turns session =
  let previous_session_file = Option.map (fun (s : Session.info) -> s.path) (current_session_info agent) in
  emit_shutdown agent reason;
  Agent.adopt_session agent ~turns (Some session);
  emit_start ?previous_session_file session reason

let new_session agent =
  match before_switch agent "new" with
  | Extensions.Session_cancel reason -> "Session switch cancelled: " ^ reason
  | Extensions.Session_continue ->
    let session = Session.create_new () in
    adopt_with_events agent ~reason:"new" ~turns:[] session;
    "Started new session " ^ session.Session.id

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
    (match before_switch agent ~target_session_file:i.path "resume" with
     | Extensions.Session_cancel reason -> "Session switch cancelled: " ^ reason
     | Extensions.Session_continue ->
    let turns = Session.load_turns i.path in
    let session = Session.open_file i.path in
    adopt_with_events agent ~reason:"resume" ~turns session;
    Printf.sprintf "Resumed %s (%d turns)" i.id (List.length turns))

let name agent n =
  match Agent.session agent with
  | Some s -> Session.set_name s n (Agent.turns agent); "Named session: " ^ n
  | None -> "No active session to name."

let clone agent =
  match before_fork agent ~position:"current" "clone" with
  | Extensions.Session_cancel reason -> "Session fork cancelled: " ^ reason
  | Extensions.Session_continue ->
    let turns = Agent.turns agent in
    let s = Session.clone_from turns in
    adopt_with_events agent ~reason:"clone" ~turns s;
    "Cloned to new session " ^ s.Session.id

let fork agent spec =
  match spec with
  | None -> clone agent
  | Some s -> (
    match Session.resolve_path s with
    | None -> "No matching session to fork: " ^ s
    | Some source_path -> (
      match before_fork agent ~source_session_file:source_path ~entry_id:s ~position:"source" "fork" with
      | Extensions.Session_cancel reason -> "Session fork cancelled: " ^ reason
      | Extensions.Session_continue ->
        let turns = Session.load_turns source_path in
        let session = Session.clone_from turns in
        adopt_with_events agent ~reason:"fork" ~turns session;
        Printf.sprintf "Forked %s into session %s (%d turns)" s session.Session.id (List.length turns)))

let json_string name json =
  match Yojson.Safe.Util.member name json with
  | `String s when String.trim s <> "" -> Some s
  | _ -> None

let turn_prefix ?(include_target = true) agent entry_id =
  let prefix = "turn-" in
  if not (starts_with ~prefix entry_id) then None
  else
    let raw = String.sub entry_id (String.length prefix) (String.length entry_id - String.length prefix) in
    match int_of_string_opt raw with
    | None -> None
    | Some index ->
      let count = if include_target then index + 1 else index in
      if count < 0 then None else Some (Agent.turns agent |> List.filteri (fun i _ -> i < count))

let turn_index entry_id =
  let prefix = "turn-" in
  if not (starts_with ~prefix entry_id) then None
  else
    let raw = String.sub entry_id (String.length prefix) (String.length entry_id - String.length prefix) in
    int_of_string_opt raw

let turn_text (turn : Llm.turn) =
  turn.Llm.content
  |> List.filter_map (function Llm.Text text -> Some text | _ -> None)
  |> String.concat "\n"

let action_result ?(cancelled = false) ?(extra = []) ?error action text =
  `Assoc
    ([ ("kind", `String (Option.value (json_string "kind" action) ~default:"unknown"));
       ("text", `String text);
       ("cancelled", `Bool cancelled) ]
    @ extra
    @ match error with Some msg -> [ ("error", `String msg) ] | None -> [])

let action_text_result action text =
  action_result ~cancelled:(session_cancelled text) action text

let json_list name json =
  match Yojson.Safe.Util.member name json with
  | `List values -> values
  | _ -> []

let json_object name json =
  match Yojson.Safe.Util.member name json with
  | `Assoc _ as obj -> obj
  | _ -> `Assoc []

let json_bool name json =
  match Yojson.Safe.Util.member name json with
  | `Bool b -> Some b
  | `String s ->
    Some
      (match String.lowercase_ascii (String.trim s) with
       | "1" | "true" | "yes" | "y" | "on" -> true
       | _ -> false)
  | _ -> None

let apply_session_action_side_effects agent action =
  Option.iter (fun name -> ignore (Agent.set_session_name agent name)) (json_string "sessionName" action);
  json_list "sessionEntries" action
  |> List.iter (fun entry -> ignore (Agent.append_extension_session_entry agent entry))

let action_with_side_effects agent action result =
  apply_session_action_side_effects agent action;
  result

let apply_navigate_tree_label agent target_id label =
  match label with
  | Some label ->
    ignore
      (Agent.append_extension_session_entry agent
         (`Assoc
           [ ("type", `String "label");
             ("targetId", `String target_id);
             ("label", `String label) ]))
  | None -> ()

let branch_summary_text summary =
  match Yojson.Safe.Util.member "summary" summary with
  | `String text when String.trim text <> "" -> Some text
  | _ -> (
    match Yojson.Safe.Util.member "text" summary with
    | `String text when String.trim text <> "" -> Some text
    | _ -> None)

let branch_summary_details summary =
  match Yojson.Safe.Util.member "details" summary with
  | `Null -> None
  | details -> Some details

let llm_content_text = function
  | Llm.Text text -> text
  | Llm.Image { mime_type; _ } -> "[image " ^ mime_type ^ "]"
  | Llm.Thinking _ -> ""
  | Llm.Tool_use { name; input; _ } -> Printf.sprintf "[tool %s %s]" name (Yojson.Safe.to_string input)
  | Llm.Tool_result { content; _ } -> "[tool result] " ^ content

let transcript_of_turn (turn : Llm.turn) =
  let role = match turn.role with Llm.User -> "user" | Llm.Assistant -> "assistant" in
  let text = turn.content |> List.map llm_content_text |> List.filter (fun text -> String.trim text <> "") |> String.concat "\n" in
  if text = "" then None else Some (role ^ ": " ^ text)

let content_json_text json =
  match json with
  | `String text -> Some text
  | `List blocks ->
    let text =
      blocks
      |> List.map (fun block -> Llm.content_of_json block)
      |> List.map llm_content_text
      |> List.filter (fun text -> String.trim text <> "")
      |> String.concat "\n"
    in
    if text = "" then None else Some text
  | _ -> None

let summary_text_entry entry =
  let member name = Yojson.Safe.Util.member name entry in
  match member "type" with
  | `String "message" -> (
    try transcript_of_turn (Llm.turn_of_json (member "message")) with Sys.Break as e -> raise e | _ -> None)
  | `String "custom_message" -> (
    match content_json_text (member "content") with
    | Some text -> Some ("custom: " ^ text)
    | None -> None)
  | `String "branch_summary" -> (
    match member "summary" with
    | `String summary when String.trim summary <> "" -> Some ("branch_summary: " ^ summary)
    | _ -> None)
  | `String "compaction" -> (
    match member "summary" with
    | `String summary when String.trim summary <> "" -> Some ("compaction_summary: " ^ summary)
    | _ -> None)
  | _ -> None

let default_branch_summary agent ?custom_instructions ?replace_instructions entries =
  let transcript =
    entries |> List.filter_map summary_text_entry |> String.concat "\n\n"
  in
  if String.trim transcript = "" then Ok None
  else
    let base_prompt =
      "Summarize the branch above for a coding agent that is navigating back to another \
       point in the session tree. Preserve concrete user intent, important decisions, \
       files touched, tool results, unresolved issues, and any facts needed to continue. \
       Output only the branch summary."
    in
    let instructions =
      match replace_instructions, custom_instructions with
      | Some true, Some custom when String.trim custom <> "" -> custom
      | _, Some custom when String.trim custom <> "" -> base_prompt ^ "\n\nAdditional focus: " ^ custom
      | _ -> base_prompt
    in
    let system =
      "You summarize abandoned conversation branches for a coding agent. Output only the summary."
    in
    let prompt =
      Printf.sprintf "<conversation>\n%s\n</conversation>\n\n%s" transcript instructions
    in
    try
      let blocks, _ =
        Llm.complete (Agent.config agent) ~system ~tools_enabled:false
          [ { Llm.role = Llm.User; content = [ Llm.Text prompt ] } ]
      in
      let summary =
        blocks
        |> List.filter_map (function Llm.Text text -> Some text | _ -> None)
        |> String.concat "\n"
        |> String.trim
      in
      if summary = "" then Ok None else Ok (Some (`Assoc [ ("summary", `String summary) ]))
    with
    | Llm.Config_error msg | Llm.Api_error msg -> Error msg
    | Sys.Break as e -> raise e
    | e -> Error (Printexc.to_string e)

let append_navigate_tree_summary agent ?(from_hook = true) ?parent_id summary =
  match branch_summary_text summary with
  | None -> None
  | Some text ->
    let from_id = Option.value parent_id ~default:"root" in
    Agent.append_branch_summary agent ?parent_id ?details:(branch_summary_details summary) ~from_hook
      ~from_id text

let session_entries agent =
  match Agent.session agent with
  | Some session -> session.Session.entries
  | None -> []

let session_info agent =
  match Agent.session agent with
  | Some session -> Some (Session.info_of session)
  | None -> None

let session_context_entries agent =
  match
    Extensions.session_context_json ?info:(session_info agent) ~entries:(session_entries agent) (Agent.turns agent)
    |> Yojson.Safe.Util.member "entries"
  with
  | `List entries -> entries
  | _ -> []

let session_context_leaf_id agent =
  match
    Extensions.session_context_json ?info:(session_info agent) ~entries:(session_entries agent) (Agent.turns agent)
    |> Yojson.Safe.Util.member "leafId"
  with
  | `String id when String.trim id <> "" -> Some id
  | _ -> None

let entry_id json =
  match Yojson.Safe.Util.member "id" json with
  | `String id when String.trim id <> "" -> Some id
  | _ -> None

let entry_by_id entries target_id =
  List.find_opt (fun entry -> entry_id entry = Some target_id) entries

let entry_parent_id json =
  match Yojson.Safe.Util.member "parentId" json with
  | `String id when String.trim id <> "" -> Some id
  | _ -> None

let entries_after_target entries target_id =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | entry :: rest ->
      let current_is_target = entry_id entry = Some target_id in
      if seen then loop true (entry :: acc) rest else loop current_is_target acc rest
  in
  loop false [] entries

let rec nearest_turn_leaf entries ?(seen = []) leaf_id =
  match leaf_id with
  | None -> Ok None
  | Some id when List.mem id seen -> Error ("Cycle in session tree at " ^ id)
  | Some id -> (
    match turn_index id with
    | Some _ -> Ok (Some id)
    | None -> (
      match entry_by_id entries id with
      | Some entry -> nearest_turn_leaf entries ~seen:(id :: seen) (entry_parent_id entry)
      | None -> Error ("No matching parent entry: " ^ id)))

let turns_for_leaf agent entries leaf_id =
  match nearest_turn_leaf entries leaf_id with
  | Error _ as err -> err
  | Ok None -> Ok []
  | Ok (Some turn_id) -> (
    match turn_prefix agent turn_id with
    | Some turns -> Ok turns
    | None -> Error ("No matching parent entry: " ^ turn_id))

let turns_for_side_leaf agent entries target_id =
  match turns_for_leaf agent entries (Some target_id) with
  | Error _ as err -> err
  | Ok _ as ok -> ok

let custom_message_editor_text entry =
  match Yojson.Safe.Util.member "content" entry with
  | `String text when String.trim text <> "" -> Some text
  | `List blocks ->
    let text =
      blocks
      |> List.filter_map (fun block ->
             match Yojson.Safe.Util.member "type" block, Yojson.Safe.Util.member "text" block with
             | `String "text", `String text -> Some text
             | _ -> None)
      |> String.concat ""
    in
    if String.trim text = "" then None else Some text
  | _ -> None

type navigate_target =
  { navigate_turns : Llm.turn list;
    navigate_new_leaf_id : string option;
    navigate_editor_text : string option }

let resolve_navigate_target agent entries target_id =
  match turn_index target_id with
  | Some index -> (
    match List.nth_opt (Agent.turns agent) index with
    | None -> Error ("No matching entry: " ^ target_id)
    | Some target_turn ->
      let target_is_user = target_turn.Llm.role = Llm.User in
      let include_target = not target_is_user in
      let navigate_turns = Option.value (turn_prefix ~include_target agent target_id) ~default:[] in
      let navigate_new_leaf_id =
        if target_is_user then (
          if index = 0 then None else Some ("turn-" ^ string_of_int (index - 1)))
        else Some target_id
      in
      let navigate_editor_text =
        if target_is_user then
          let text = turn_text target_turn in
          if String.trim text = "" then None else Some text
        else None
      in
      Ok { navigate_turns; navigate_new_leaf_id; navigate_editor_text })
  | None -> (
    match entry_by_id entries target_id with
    | Some entry when Yojson.Safe.Util.member "type" entry = `String "custom_message" ->
      let navigate_new_leaf_id = entry_parent_id entry in
      (match turns_for_leaf agent entries navigate_new_leaf_id with
       | Error msg -> Error msg
       | Ok navigate_turns ->
         Ok
           { navigate_turns;
             navigate_new_leaf_id;
             navigate_editor_text = custom_message_editor_text entry })
    | Some entry
      when List.mem (Yojson.Safe.Util.member "type" entry)
             [ `String "branch_summary"; `String "compaction" ] ->
      (match turns_for_side_leaf agent entries target_id with
       | Error msg -> Error msg
       | Ok navigate_turns ->
         Ok
           { navigate_turns;
             navigate_new_leaf_id = Some target_id;
             navigate_editor_text = None })
    | Some _ -> Error ("Unsupported navigateTree target: " ^ target_id)
    | None -> Error ("No matching entry: " ^ target_id))

let apply_extension_session_action agent action =
  match json_string "kind" action with
  | Some "new_session" -> (
    match json_string "parentSession" action with
    | Some parent when String.trim parent <> "" ->
      action_with_side_effects agent action
        (action_text_result action (fork agent (Some parent)))
    | _ ->
      action_with_side_effects agent action
        (action_text_result action (new_session agent)))
  | Some "switch_session" -> (
    match json_string "sessionPath" action with
    | Some spec ->
      action_with_side_effects agent action
        (action_text_result action (resume agent spec))
    | None -> action_result ~error:"switchSession requires sessionPath" action "switchSession failed")
  | Some "fork" ->
    let entry_id = Option.value (json_string "entryId" action) ~default:"" in
    let include_target = match json_string "position" action with Some "before" -> false | _ -> true in
    (match turn_prefix ~include_target agent entry_id with
     | Some turns -> (
       match before_fork agent ~entry_id ~position:(if include_target then "at" else "before") "fork" with
       | Extensions.Session_cancel reason -> action_result ~cancelled:true action ("Session fork cancelled: " ^ reason)
       | Extensions.Session_continue ->
         let session = Session.clone_from turns in
         adopt_with_events agent ~reason:"fork" ~turns session;
         action_with_side_effects agent action
           (action_result action (Printf.sprintf "Forked %s into session %s (%d turns)" entry_id session.Session.id (List.length turns))))
     | None ->
       action_with_side_effects agent action
         (action_text_result action (clone agent)))
  | Some "navigate_tree" ->
    let target_id = Option.value (json_string "targetId" action) ~default:"" in
    let entries = session_context_entries agent in
    (match resolve_navigate_target agent entries target_id with
     | Error msg -> action_result ~error:msg action "navigateTree failed"
     | Ok target ->
       let options = json_object "options" action in
       let original_label = json_string "label" options in
       let entries_to_summarize = entries_after_target entries target_id in
       let old_leaf_id = session_context_leaf_id agent in
       let user_wants_summary = Option.value (json_bool "summarize" options) ~default:false in
       let before_tree =
         Extensions.emit_session_before_tree ~target_id ?old_leaf_id
           ?common_ancestor_id:target.navigate_new_leaf_id ?label:original_label
           ?custom_instructions:(json_string "customInstructions" options)
           ?replace_instructions:(json_bool "replaceInstructions" options)
           ~user_wants_summary
           ~entries_to_summarize ()
       in
       (match before_tree.Extensions.tree_cancel with
        | Some reason -> action_result ~cancelled:true action ("navigateTree cancelled: " ^ reason)
        | None ->
          let custom_instructions =
            match before_tree.Extensions.tree_custom_instructions with
            | Some _ as value -> value
            | None -> json_string "customInstructions" options
          in
          let replace_instructions =
            match before_tree.Extensions.tree_replace_instructions with
            | Some _ as value -> value
            | None -> json_bool "replaceInstructions" options
          in
          let summary_result =
            match user_wants_summary, before_tree.Extensions.tree_summary with
            | true, Some summary -> Ok (Some (true, summary))
            | true, None when entries_to_summarize <> [] -> (
              match
                default_branch_summary agent ?custom_instructions ?replace_instructions entries_to_summarize
              with
              | Ok (Some summary) -> Ok (Some (false, summary))
              | Ok None -> Ok None
              | Error msg -> Error msg)
            | _ -> Ok None
          in
          (match summary_result with
           | Error msg -> action_result ~error:msg action "navigateTree failed"
           | Ok summary ->
             Agent.replace_turns agent target.navigate_turns;
             let summary_entry =
               match summary with
               | Some (from_hook, summary) ->
                 append_navigate_tree_summary agent ~from_hook ?parent_id:target.navigate_new_leaf_id summary
               | None -> None
             in
             if summary_entry = None then
               ignore (Agent.append_leaf agent ?parent_id:old_leaf_id target.navigate_new_leaf_id);
             let label = match before_tree.Extensions.tree_label with Some _ as label -> label | None -> original_label in
             (match summary_entry with
              | Some (`Assoc fields) -> (
                match List.assoc_opt "id" fields with
                | Some (`String summary_id) -> apply_navigate_tree_label agent summary_id label
                | _ -> apply_navigate_tree_label agent target_id label)
              | _ -> apply_navigate_tree_label agent target_id label);
             let emitted_new_leaf_id =
               match summary_entry with
               | Some (`Assoc fields) -> (
                 match List.assoc_opt "id" fields with
                 | Some (`String summary_id) -> Some summary_id
                 | _ -> target.navigate_new_leaf_id)
               | _ -> target.navigate_new_leaf_id
             in
             Extensions.emit_session_tree ?old_leaf_id ?new_leaf_id:emitted_new_leaf_id ?summary_entry
               ?from_extension:(Option.map fst summary) ();
             action_with_side_effects agent action
               (action_result
                  ~extra:(match target.navigate_editor_text with Some text -> [ ("editorText", `String text) ] | None -> [])
                  action (Printf.sprintf "Navigated to %s (%d turns)" target_id (List.length target.navigate_turns))))))
  | Some kind -> action_result ~error:("Unsupported session action: " ^ kind) action "session action failed"
  | None -> action_result ~error:"Session action missing kind" action "session action failed"

let apply_extension_session_actions agent actions =
  List.map (apply_extension_session_action agent) actions

let scoped_models arg =
  match arg with
  | None -> (
    match Sys.getenv_opt "AGENT_SCOPED_MODELS" with
    | Some s when String.trim s <> "" -> "Scoped models:\n" ^ s
    | _ -> "Scoped models: all")
  | Some s ->
    let patterns =
      match String.lowercase_ascii (String.trim s) with
      | "" | "all" | "clear" | "none" -> []
      | _ -> Model_spec.split_csv s
    in
    Unix.putenv "AGENT_SCOPED_MODELS" (String.concat "\n" patterns);
    if patterns = [] then "Scoped models cleared."
    else Printf.sprintf "Scoped models set: %s" (String.concat ", " patterns)

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

let import_session agent path =
  match before_switch agent ~target_session_file:path "import" with
  | Extensions.Session_cancel reason -> "Session switch cancelled: " ^ reason
  | Extensions.Session_continue -> (
    match Session.import_from path with
    | Error msg -> "Import failed: " ^ msg
    | Ok (session, turns) ->
      adopt_with_events agent ~reason:"import" ~turns session;
      Printf.sprintf "Imported %s into session %s (%d turns)" path session.Session.id (List.length turns))

let max_file_bytes = 1024 * 1024

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
       let total = in_channel_length ic in
       let len = max 0 (min max_file_bytes total) in
       let s = really_input_string ic len in
       if total > max_file_bytes then s ^ "\n... (truncated)" else s)

let changelog () =
  if Sys.file_exists "CHANGELOG.md" && not (Sys.is_directory "CHANGELOG.md") then
    try read_file "CHANGELOG.md" with Sys.Break as e -> raise e | _ -> "Failed to read CHANGELOG.md."
  else "No changelog entries found."

let hotkeys () =
  let extension_shortcuts =
    match Extensions.shortcut_menu () with
    | [] -> []
    | shortcuts ->
      ""
      :: "Extensions:"
      :: List.map
           (fun (spec, description) ->
             Printf.sprintf "  %-18s %s" spec (if String.trim description = "" then "extension shortcut" else description))
           shortcuts
  in
  String.concat "\n"
    [ "Navigation:";
      "  Up/Down            browse input history";
      "  Left/Right         move cursor";
      "  PgUp/PgDn or wheel scroll output";
      "  End                jump to latest output";
      "";
      "Editing:";
      "  Tab                complete slash command or path";
      "  Ctrl-A/E           move to start/end";
      "  Ctrl-U/K/W         delete line before/after cursor or previous word";
      "  Ctrl-J, Alt-Enter  insert newline";
      "";
      "Other:";
      "  Ctrl-P             open model picker";
      "  Ctrl-S             open settings";
      "  Ctrl-D/C           quit";
      "  !cmd               run shell and add output to context";
      "  !!cmd              run shell without adding output to context";
      "  /                  slash commands" ]
  ^ if extension_shortcuts = [] then "" else "\n" ^ String.concat "\n" extension_shortcuts

(* Clipboard utilities to try, in order, across macOS / X11 / Wayland. *)
let clipboard_commands = [ "pbcopy"; "xclip -selection clipboard"; "xsel --clipboard --input"; "wl-copy" ]

let command_exists cmd =
  let bin = match String.split_on_char ' ' cmd with b :: _ -> b | [] -> cmd in
  Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" (Filename.quote bin)) = 0

let copy agent =
  match last_assistant_text agent with
  | None -> "Nothing to copy."
  | Some text -> (
    match List.find_opt command_exists clipboard_commands with
    | None -> "Clipboard copy failed (no clipboard tool found; install pbcopy, xclip, xsel, or wl-copy)."
    | Some cmd -> (
      let ok = ref false in
      try
        let oc = Unix.open_process_out cmd in
        Fun.protect
          ~finally:(fun () ->
            match Unix.close_process_out oc with
            | Unix.WEXITED 0 -> ok := true
            | _ -> ())
          (fun () -> output_string oc text);
        if !ok then "Copied last reply to clipboard."
        else Printf.sprintf "Clipboard copy failed (%s)." cmd
      with
      | Sys.Break as e -> raise e
      | _ -> Printf.sprintf "Clipboard copy failed (%s)." cmd))
