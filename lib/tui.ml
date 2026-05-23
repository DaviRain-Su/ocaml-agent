(* Full-screen terminal UI built on notty: a scrollback viewport above a live
   input editor. The Agent core is unchanged — we install a frontend whose
   callbacks append styled lines to the scrollback and redraw. Streaming happens
   inside the (blocking) Agent.send call; each callback redraws immediately. *)

open Notty
open Notty_unix

type ui =
  { term : Term.t;
    mutable lines : (A.t * string) list; (* scrollback, oldest first *)
    mutable input : string;
    mutable cursor : int; (* byte index into input *)
    mutable history : string list; (* newest first *)
    mutable hist_idx : int; (* -1 = editing a fresh line *)
    buf : Buffer.t; (* partial streamed assistant line *)
    mutable agent : Agent.t;
    mutable running : bool }

let max_lines = 5000

(* --- UTF-8 cursor helpers --- *)
let is_cont c = Char.code c land 0xC0 = 0x80

let prev_cp_start s i =
  let j = ref (i - 1) in
  while !j > 0 && is_cont s.[!j] do
    decr j
  done;
  !j

let next_cp_start s i =
  let n = String.length s in
  let j = ref (i + 1) in
  while !j < n && is_cont s.[!j] do
    incr j
  done;
  !j

let cp_count s upto =
  let c = ref 0 in
  String.iteri (fun i ch -> if i < upto && not (is_cont ch) then incr c) s;
  !c

(* --- scrollback --- *)
let push ui attr s =
  ui.lines <- ui.lines @ [ (attr, s) ];
  let len = List.length ui.lines in
  if len > max_lines then
    ui.lines <- List.filteri (fun i _ -> i >= len - max_lines) ui.lines

(* Wrap a string to width [w] without splitting UTF-8 sequences. *)
let wrap_line w s =
  if w <= 0 || String.length s <= w then [ s ]
  else begin
    let n = String.length s in
    let rec go i acc =
      if i >= n then List.rev acc
      else begin
        let stop = min n (i + w) in
        let stop =
          if stop < n then (
            let j = ref stop in
            while !j > i && is_cont s.[!j] do
              decr j
            done;
            if !j <= i then min n (i + w) else !j)
          else stop
        in
        go stop (String.sub s i (stop - i) :: acc)
      end
    in
    go 0 []
  end

let take_last n l =
  let len = List.length l in
  if len <= n then l else List.filteri (fun i _ -> i >= len - n) l

let prompt_label = "you> "

let redraw ui =
  let w, h = Term.size ui.term in
  let w = max 1 w and h = max 3 h in
  let visible = max 1 (h - 2) in
  let src = take_last visible ui.lines in
  let body_imgs =
    List.concat_map
      (fun (a, s) -> if s = "" then [ I.void 1 1 ] else List.map (I.string a) (wrap_line w s))
      src
  in
  let body_imgs = take_last visible body_imgs in
  let body = I.vsnap ~align:`Bottom visible (I.vcat body_imgs) in
  let sep = I.char A.(fg (gray 6)) '_' w 1 in
  let prompt = I.(string A.(fg green) prompt_label <|> string A.empty ui.input) in
  Term.image ui.term I.(body <-> sep <-> prompt);
  Term.cursor ui.term (Some (String.length prompt_label + cp_count ui.input ui.cursor, h - 1))

(* --- frontend callbacks --- *)
let feed_assistant ui s =
  Buffer.add_string ui.buf s;
  let content = Buffer.contents ui.buf in
  let parts = String.split_on_char '\n' content in
  let rec loop = function
    | [ last ] ->
      Buffer.clear ui.buf;
      Buffer.add_string ui.buf last
    | line :: rest ->
      push ui A.empty line;
      loop rest
    | [] -> ()
  in
  loop parts;
  redraw ui

let flush_assistant ui =
  let rem = Buffer.contents ui.buf in
  if rem <> "" then push ui A.empty rem;
  Buffer.clear ui.buf;
  redraw ui

let push_result ui res =
  String.split_on_char '\n' res
  |> List.iter (fun line ->
         let a =
           if line = "" then A.empty
           else
             match line.[0] with
             | '-' -> A.(fg red)
             | '+' -> A.(fg green)
             | _ -> A.(fg (gray 12))
         in
         push ui a line);
  redraw ui

let rec confirm ui cmd =
  match Term.event ui.term with
  | `Key (`ASCII ('y' | 'Y'), _) -> Agent.Approve_once
  | `Key (`ASCII ('a' | 'A'), _) -> Agent.Approve_always
  | `Key (`ASCII ('n' | 'N'), _) | `Key (`Enter, _) | `Key (`Escape, _) -> Agent.Deny
  | `Key (`ASCII 'c', [ `Ctrl ]) -> Agent.Deny
  | `Resize _ ->
    redraw ui;
    confirm ui cmd
  | _ -> confirm ui cmd

let make_frontend ui : Agent.frontend =
  { text_delta = (fun s -> feed_assistant ui s);
    text_done = (fun () -> flush_assistant ui);
    thinking = (fun s -> if String.trim s <> "" then (push ui A.(fg (gray 10)) ("\xf0\x9f\x92\xad " ^ s); redraw ui));
    tool_call = (fun name prev -> push ui A.(fg cyan) ("\xe2\x9a\x99 " ^ name ^ " " ^ prev); redraw ui);
    tool_result = (fun res -> if String.trim res <> "" then push_result ui res);
    notice = (fun s -> push ui A.(fg yellow) s; redraw ui);
    confirm_bash =
      (fun cmd ->
        push ui A.(fg yellow) ("\xe2\x9a\xa0 run bash: " ^ cmd);
        push ui A.(fg (gray 12)) "approve? [y]es / [N]o / [a]lways";
        redraw ui;
        confirm ui cmd) }

(* --- slash commands (TUI-native) --- *)
let cmd ui line =
  let parts = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in
  match parts with
  | ("/exit" | "/quit") :: _ -> ui.running <- false
  | "/help" :: _ ->
    List.iter (push ui A.empty)
      [ "/model [alias] [name]  switch provider/model or list providers";
        "/think <level>         reasoning level (off/low/medium/high)";
        "/compact               summarize older turns";
        "/session               model, turns, context usage";
        "/sessions              list saved sessions";
        "/resume <n|id>         resume a saved session";
        "/name <text>           name the current session";
        "/clone                 duplicate the current session";
        "/export <file>         export (.html or .jsonl)";
        "/copy                  copy last reply to clipboard";
        "/new                   clear the conversation";
        "/exit                  quit" ];
    redraw ui
  | "/session" :: _ ->
    let used, window, pct = Agent.usage_info ui.agent in
    let c = Agent.config ui.agent in
    push ui A.empty
      (Printf.sprintf "%s | think:%s | %d turns | ctx ~%d/%d (%.0f%%)" (Llm.describe c) c.Llm.thinking
         (Agent.turn_count ui.agent) used window (pct *. 100.));
    redraw ui
  | "/new" :: _ -> Agent.reset ui.agent; push ui A.(fg (gray 12)) "Conversation cleared."; redraw ui
  | "/compact" :: _ -> push ui A.(fg (gray 12)) (Agent.compact ui.agent); redraw ui
  | "/sessions" :: _ ->
    List.iter (push ui A.empty) (String.split_on_char '\n' (Commands.format_sessions ()));
    redraw ui
  | "/resume" :: a :: _ -> push ui A.(fg (gray 12)) (Commands.resume ui.agent a); redraw ui
  | "/name" :: rest when rest <> [] -> push ui A.(fg (gray 12)) (Commands.name ui.agent (String.concat " " rest)); redraw ui
  | "/clone" :: _ -> push ui A.(fg (gray 12)) (Commands.clone ui.agent); redraw ui
  | "/export" :: p :: _ -> push ui A.(fg (gray 12)) (Commands.export ui.agent p); redraw ui
  | "/copy" :: _ -> push ui A.(fg (gray 12)) (Commands.copy ui.agent); redraw ui
  | "/think" :: rest ->
    let lvl = match rest with l :: _ -> l | [] -> "off" in
    Agent.set_thinking ui.agent lvl;
    push ui A.(fg (gray 12)) ("reasoning level = " ^ lvl);
    redraw ui
  | "/model" :: [] ->
    push ui A.empty ("Current: " ^ Llm.describe (Agent.config ui.agent));
    List.iter (fun (n, has) -> push ui A.empty ((if has then "* " else "  ") ^ n)) (Llm.provider_status ());
    redraw ui
  | "/model" :: alias :: rest ->
    let model = match rest with m :: _ -> Some m | [] -> None in
    (match Llm.config_for ?model alias with
     | c -> Agent.set_config ui.agent c; push ui A.(fg green) ("Switched: " ^ Llm.describe c)
     | exception Llm.Config_error e -> push ui A.(fg red) ("Error: " ^ e));
    redraw ui
  | c :: _ -> push ui A.(fg red) ("Unknown command " ^ c ^ " (try /help)"); redraw ui
  | [] -> ()

let submit ui =
  let line = String.trim ui.input in
  ui.input <- "";
  ui.cursor <- 0;
  ui.hist_idx <- -1;
  if line <> "" then begin
    ui.history <- line :: ui.history;
    push ui A.(fg green) (prompt_label ^ line);
    redraw ui;
    if String.length line > 0 && line.[0] = '/' then cmd ui line
    else begin
      (try ignore (Agent.send ui.agent line) with
       | Llm.Api_error m -> push ui A.(fg red) ("API error: " ^ m)
       | e -> push ui A.(fg red) ("Error: " ^ Printexc.to_string e));
      flush_assistant ui;
      redraw ui
    end
  end

(* --- input editing --- *)
let insert ui str =
  let n = String.length ui.input in
  ui.input <- String.sub ui.input 0 ui.cursor ^ str ^ String.sub ui.input ui.cursor (n - ui.cursor);
  ui.cursor <- ui.cursor + String.length str

let backspace ui =
  if ui.cursor > 0 then begin
    let st = prev_cp_start ui.input ui.cursor in
    let n = String.length ui.input in
    ui.input <- String.sub ui.input 0 st ^ String.sub ui.input ui.cursor (n - ui.cursor);
    ui.cursor <- st
  end

let recall ui dir =
  (* dir = +1 older, -1 newer *)
  let len = List.length ui.history in
  let idx = ui.hist_idx + dir in
  if idx >= 0 && idx < len then begin
    ui.hist_idx <- idx;
    ui.input <- List.nth ui.history idx;
    ui.cursor <- String.length ui.input
  end
  else if idx < 0 then begin
    ui.hist_idx <- -1;
    ui.input <- "";
    ui.cursor <- 0
  end

let run agent =
  let term = Term.create () in
  let ui =
    { term; lines = []; input = ""; cursor = 0; history = []; hist_idx = -1;
      buf = Buffer.create 256; agent; running = true }
  in
  Agent.set_frontend agent (make_frontend ui);
  push ui A.(fg green ++ st bold) "OCaml Code Agent";
  push ui A.(fg (gray 12)) (Llm.describe (Agent.config agent));
  push ui A.(fg (gray 12)) "Type your request. /help for commands, /exit or Ctrl-D to quit.";
  push ui A.empty "";
  redraw ui;
  while ui.running do
    match Term.event term with
    | `Key (`ASCII 'c', [ `Ctrl ]) | `Key (`ASCII 'd', [ `Ctrl ]) -> ui.running <- false
    | `Key (`Enter, _) -> submit ui
    | `Key (`Backspace, _) -> backspace ui; redraw ui
    | `Key (`Escape, _) -> ui.input <- ""; ui.cursor <- 0; redraw ui
    | `Key (`Arrow `Left, _) -> if ui.cursor > 0 then ui.cursor <- prev_cp_start ui.input ui.cursor; redraw ui
    | `Key (`Arrow `Right, _) ->
      if ui.cursor < String.length ui.input then ui.cursor <- next_cp_start ui.input ui.cursor;
      redraw ui
    | `Key (`Arrow `Up, _) -> recall ui 1; redraw ui
    | `Key (`Arrow `Down, _) -> recall ui (-1); redraw ui
    | `Key (`ASCII c, mods) when not (List.mem `Ctrl mods) -> insert ui (String.make 1 c); redraw ui
    | `Key (`Uchar u, mods) when not (List.mem `Ctrl mods) ->
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b u;
      insert ui (Buffer.contents b);
      redraw ui
    | `Resize _ -> redraw ui
    | `End -> ui.running <- false
    | _ -> ()
  done;
  Term.release term
