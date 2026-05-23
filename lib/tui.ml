(* Full-screen terminal UI built on notty: a scrollback viewport above a live
   input editor. The Agent core is unchanged — we install a frontend whose
   callbacks append styled lines and redraw. Streaming happens inside the
   (blocking) Agent.send call; a background spinner thread animates a "thinking"
   indicator while the turn runs. All terminal writes are serialized by a mutex. *)

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
    mutable running : bool;
    mutable scroll : int; (* display lines scrolled up from the bottom; 0 = latest *)
    mutable turn_active : bool; (* a send is in progress *)
    mutable quiet : bool; (* suppress spinner redraws (e.g. during a modal) *)
    mutable spin : int;
    mutable turn_start : float;
    mutable asst_in_code : bool; (* inside a ``` fence while streaming assistant text *)
    mtx : Mutex.t }

let max_lines = 5000
let spinner = [| "\xe2\xa0\x8b"; "\xe2\xa0\x99"; "\xe2\xa0\xb9"; "\xe2\xa0\xb8"; "\xe2\xa0\xbc"; "\xe2\xa0\xb4"; "\xe2\xa0\xa6"; "\xe2\xa0\xa7"; "\xe2\xa0\x87"; "\xe2\xa0\x8f" |]

(* --- UTF-8 cursor helpers --- *)
let is_cont c = Char.code c land 0xC0 = 0x80
let prev_cp_start s i = let j = ref (i - 1) in while !j > 0 && is_cont s.[!j] do decr j done; !j
let next_cp_start s i = let n = String.length s in let j = ref (i + 1) in while !j < n && is_cont s.[!j] do incr j done; !j
let cp_count s upto = let c = ref 0 in String.iteri (fun i ch -> if i < upto && not (is_cont ch) then incr c) s; !c

(* --- scrollback --- *)
let push ui attr s =
  ui.lines <- ui.lines @ [ (attr, s) ];
  let len = List.length ui.lines in
  if len > max_lines then ui.lines <- List.filteri (fun i _ -> i >= len - max_lines) ui.lines

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
            while !j > i && is_cont s.[!j] do decr j done;
            if !j <= i then min n (i + w) else !j)
          else stop
        in
        go stop (String.sub s i (stop - i) :: acc)
      end
    in
    go 0 []
  end

let prompt_label = "you> "

(* All scrollback lines wrapped to width [w] as (attr, text). *)
let wrapped ui w =
  List.concat_map (fun (a, s) -> if s = "" then [ (A.empty, "") ] else List.map (fun l -> (a, l)) (wrap_line w s)) ui.lines

let sublist start len l = l |> List.filteri (fun i _ -> i >= start && i < start + len)

let redraw ui =
  Mutex.lock ui.mtx;
  let w, h = Term.size ui.term in
  let w = max 1 w and h = max 4 h in
  let visible = max 1 (h - 3) in
  let all = wrapped ui w in
  let total = List.length all in
  let maxscroll = max 0 (total - visible) in
  if ui.scroll > maxscroll then ui.scroll <- maxscroll;
  let start = max 0 (total - visible - ui.scroll) in
  let window = sublist start visible all in
  let body = I.vsnap ~align:`Bottom visible (I.vcat (List.map (fun (a, l) -> I.string a l) window)) in
  let status =
    if ui.turn_active then
      I.string A.(fg yellow) (Printf.sprintf "%s thinking… %.0fs" spinner.(ui.spin mod Array.length spinner) (Unix.gettimeofday () -. ui.turn_start))
    else if ui.scroll > 0 then I.string A.(fg (gray 8)) (Printf.sprintf "-- scrolled %d lines (End to return) --" ui.scroll)
    else I.void 1 1
  in
  let sep = I.char A.(fg (gray 6)) '_' w 1 in
  let prompt = I.(string A.(fg green) prompt_label <|> string A.empty ui.input) in
  Term.image ui.term I.(body <-> status <-> sep <-> prompt);
  Term.cursor ui.term (Some (String.length prompt_label + cp_count ui.input ui.cursor, h - 1));
  Mutex.unlock ui.mtx

(* Line-level markdown styling for streamed assistant text, tracking code-fence
   state across the message. *)
let asst_attr ui line =
  let t = String.trim line in
  let fence = String.length t >= 3 && String.sub t 0 3 = "```" in
  if fence then (ui.asst_in_code <- not ui.asst_in_code; A.(fg (gray 8)))
  else if ui.asst_in_code then A.(fg cyan)
  else if String.length t > 0 && t.[0] = '#' then A.(st bold)
  else A.empty

(* --- frontend callbacks --- *)
let feed_assistant ui s =
  Buffer.add_string ui.buf s;
  let content = Buffer.contents ui.buf in
  let parts = String.split_on_char '\n' content in
  let rec loop = function
    | [ last ] -> Buffer.clear ui.buf; Buffer.add_string ui.buf last
    | line :: rest -> push ui (asst_attr ui line) line; loop rest
    | [] -> ()
  in
  loop parts;
  redraw ui

let flush_assistant ui =
  let rem = Buffer.contents ui.buf in
  if rem <> "" then push ui (asst_attr ui rem) rem;
  Buffer.clear ui.buf;
  ui.asst_in_code <- false;
  redraw ui

let push_result ui res =
  String.split_on_char '\n' res
  |> List.iter (fun line ->
         let a =
           if line = "" then A.empty
           else match line.[0] with '-' -> A.(fg red) | '+' -> A.(fg green) | _ -> A.(fg (gray 12))
         in
         push ui a line);
  redraw ui

let rec confirm ui cmd =
  ui.quiet <- true;
  let r =
    match Term.event ui.term with
    | `Key (`ASCII ('y' | 'Y'), _) -> Agent.Approve_once
    | `Key (`ASCII ('a' | 'A'), _) -> Agent.Approve_always
    | `Key (`ASCII ('n' | 'N'), _) | `Key (`Enter, _) | `Key (`Escape, _) -> Agent.Deny
    | `Key (`ASCII 'c', [ `Ctrl ]) -> Agent.Deny
    | `Resize _ -> redraw ui; confirm ui cmd
    | _ -> confirm ui cmd
  in
  ui.quiet <- false;
  r

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

(* --- modal selector --- *)
(* Render and run a blocking list picker; returns the chosen index or None. *)
let select ui ~title (items : string list) : int option =
  if items = [] then None
  else begin
    ui.quiet <- true;
    let n = List.length items in
    let sel = ref 0 in
    let render () =
      Mutex.lock ui.mtx;
      let w, h = Term.size ui.term in
      let header = I.string A.(fg green ++ st bold) title in
      let rows =
        List.mapi
          (fun i it ->
            let a = if i = !sel then A.(fg black ++ bg cyan) else A.empty in
            I.string a (Printf.sprintf " %s %s " (if i = !sel then ">" else " ") it))
          items
      in
      let hint = I.string A.(fg (gray 8)) "↑/↓ select · Enter confirm · Esc cancel" in
      let img = I.vsnap ~align:`Top (max 4 h) (I.vcat ((header :: rows) @ [ I.void 1 1; hint ])) in
      Term.image ui.term (I.hsnap ~align:`Left (max 1 w) img);
      Term.cursor ui.term None;
      Mutex.unlock ui.mtx
    in
    render ();
    let rec loop () =
      match Term.event ui.term with
      | `Key (`Arrow `Up, _) -> sel := (if !sel = 0 then n - 1 else !sel - 1); render (); loop ()
      | `Key (`Arrow `Down, _) -> sel := (!sel + 1) mod n; render (); loop ()
      | `Key (`Enter, _) -> Some !sel
      | `Key (`Escape, _) | `Key (`ASCII 'c', [ `Ctrl ]) -> None
      | `Resize _ -> render (); loop ()
      | _ -> loop ()
    in
    let r = loop () in
    ui.quiet <- false;
    redraw ui;
    r
  end

(* Pick a model from the catalog, limited to providers whose key is present. *)
let model_picker ui =
  let avail = List.filter_map (fun (n, has) -> if has then Some n else None) (Llm.provider_status ()) in
  let entries =
    let all = Models.list () in
    let filtered = List.filter (fun (e : Models.entry) -> List.mem e.Models.provider avail) all in
    if filtered = [] then all else filtered
  in
  let labels = List.map (fun (e : Models.entry) -> e.Models.provider ^ " / " ^ e.Models.id) entries in
  match select ui ~title:"Select a model" labels with
  | None -> ()
  | Some i ->
    let e = List.nth entries i in
    (match Llm.config_for ~model:e.Models.id e.Models.provider with
     | c -> Agent.set_config ui.agent c; push ui A.(fg green) ("Switched: " ^ Llm.describe c)
     | exception Llm.Config_error m -> push ui A.(fg red) ("Error: " ^ m));
    redraw ui

(* --- slash commands (TUI-native) --- *)
let cmd ui line =
  let parts = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in
  match parts with
  | ("/exit" | "/quit") :: _ -> ui.running <- false
  | "/help" :: _ ->
    List.iter (push ui A.empty)
      [ "/model [alias] [name]  switch model (no args = picker)";
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
        "Tab                    complete command / file path";
        "Ctrl-A/E/U/K/W         line edit (home/end/kill-start/kill-end/kill-word)";
        "PgUp/PgDn or wheel     scroll · End jumps to latest · Ctrl-P model picker";
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
  | "/sessions" :: _ -> List.iter (push ui A.empty) (String.split_on_char '\n' (Commands.format_sessions ())); redraw ui
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
  | "/model" :: [] -> model_picker ui
  | "/model" :: alias :: rest ->
    let model = match rest with m :: _ -> Some m | [] -> None in
    (match Llm.config_for ?model alias with
     | c -> Agent.set_config ui.agent c; push ui A.(fg green) ("Switched: " ^ Llm.describe c)
     | exception Llm.Config_error e -> push ui A.(fg red) ("Error: " ^ e));
    redraw ui
  | c :: _ -> push ui A.(fg red) ("Unknown command " ^ c ^ " (try /help)"); redraw ui
  | [] -> ()

(* Run one turn with an animated spinner thread. *)
let run_turn ui line =
  ui.turn_active <- true;
  ui.turn_start <- Unix.gettimeofday ();
  ui.spin <- 0;
  let spinner_thread =
    Thread.create
      (fun () ->
        while ui.turn_active do
          if not ui.quiet then (ui.spin <- ui.spin + 1; redraw ui);
          Thread.delay 0.1
        done)
      ()
  in
  (try ignore (Agent.send ui.agent line) with
   | Llm.Api_error m -> push ui A.(fg red) ("API error: " ^ m)
   | e -> push ui A.(fg red) ("Error: " ^ Printexc.to_string e));
  ui.turn_active <- false;
  Thread.join spinner_thread;
  flush_assistant ui;
  redraw ui

let submit ui =
  let line = String.trim ui.input in
  ui.input <- "";
  ui.cursor <- 0;
  ui.hist_idx <- -1;
  ui.scroll <- 0;
  if line <> "" then begin
    ui.history <- line :: ui.history;
    push ui A.(fg green) (prompt_label ^ line);
    redraw ui;
    if String.length line > 0 && line.[0] = '/' then cmd ui line else run_turn ui line
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
  let len = List.length ui.history in
  let idx = ui.hist_idx + dir in
  if idx >= 0 && idx < len then (ui.hist_idx <- idx; ui.input <- List.nth ui.history idx; ui.cursor <- String.length ui.input)
  else if idx < 0 then (ui.hist_idx <- -1; ui.input <- ""; ui.cursor <- 0)

let page ui delta =
  let _, h = Term.size ui.term in
  let step = max 1 (h - 4) in
  ui.scroll <- max 0 (ui.scroll + (delta * step));
  redraw ui

(* Tab completion of the current token (slash command or file path). *)
let complete ui =
  let start, _ = Complete.token_of ui.input in
  match Complete.candidates ui.input with
  | [] -> ()
  | cands ->
    let repl = match cands with [ one ] -> one | _ -> Complete.common_prefix cands in
    if repl <> "" then (ui.input <- String.sub ui.input 0 start ^ repl; ui.cursor <- String.length ui.input);
    (match cands with _ :: _ :: _ -> push ui A.(fg (gray 10)) (String.concat "   " cands) | _ -> ());
    redraw ui

let kill_to_start ui =
  ui.input <- String.sub ui.input ui.cursor (String.length ui.input - ui.cursor);
  ui.cursor <- 0

let kill_to_end ui = ui.input <- String.sub ui.input 0 ui.cursor

let kill_word ui =
  let i = ref ui.cursor in
  while !i > 0 && ui.input.[!i - 1] = ' ' do decr i done;
  while !i > 0 && ui.input.[!i - 1] <> ' ' do decr i done;
  ui.input <- String.sub ui.input 0 !i ^ String.sub ui.input ui.cursor (String.length ui.input - ui.cursor);
  ui.cursor <- !i

let run agent =
  let term = Term.create () in
  let ui =
    { term; lines = []; input = ""; cursor = 0; history = []; hist_idx = -1; buf = Buffer.create 256;
      agent; running = true; scroll = 0; turn_active = false; quiet = false; spin = 0; turn_start = 0.;
      asst_in_code = false; mtx = Mutex.create () }
  in
  Agent.set_frontend agent (make_frontend ui);
  push ui A.(fg green ++ st bold) "OCaml Code Agent";
  push ui A.(fg (gray 12)) (Llm.describe (Agent.config agent));
  push ui A.(fg (gray 12)) "Type your request. /help for commands, Ctrl-P to pick a model, Ctrl-D to quit.";
  push ui A.empty "";
  redraw ui;
  while ui.running do
    match Term.event term with
    | `Key (`ASCII 'c', [ `Ctrl ]) | `Key (`ASCII 'd', [ `Ctrl ]) -> ui.running <- false
    | `Key (`ASCII 'p', [ `Ctrl ]) -> model_picker ui
    | `Key (`Tab, _) -> complete ui
    | `Key (`ASCII 'a', [ `Ctrl ]) -> ui.cursor <- 0; redraw ui
    | `Key (`ASCII 'e', [ `Ctrl ]) -> ui.cursor <- String.length ui.input; redraw ui
    | `Key (`ASCII 'u', [ `Ctrl ]) -> kill_to_start ui; redraw ui
    | `Key (`ASCII 'k', [ `Ctrl ]) -> kill_to_end ui; redraw ui
    | `Key (`ASCII 'w', [ `Ctrl ]) -> kill_word ui; redraw ui
    | `Key (`Enter, _) -> submit ui
    | `Key (`Backspace, _) -> backspace ui; redraw ui
    | `Key (`Escape, _) -> ui.input <- ""; ui.cursor <- 0; redraw ui
    | `Key (`Page `Up, _) -> page ui 1
    | `Key (`Page `Down, _) -> page ui (-1)
    | `Key (`Home, _) -> ui.cursor <- 0; redraw ui
    | `Key (`End, _) -> ui.scroll <- 0; ui.cursor <- String.length ui.input; redraw ui
    | `Key (`Arrow `Left, _) -> if ui.cursor > 0 then ui.cursor <- prev_cp_start ui.input ui.cursor; redraw ui
    | `Key (`Arrow `Right, _) -> if ui.cursor < String.length ui.input then ui.cursor <- next_cp_start ui.input ui.cursor; redraw ui
    | `Key (`Arrow `Up, _) -> recall ui 1; redraw ui
    | `Key (`Arrow `Down, _) -> recall ui (-1); redraw ui
    | `Mouse (`Press (`Scroll `Up), _, _) -> page ui 1
    | `Mouse (`Press (`Scroll `Down), _, _) -> page ui (-1)
    | `Key (`ASCII c, mods) when not (List.mem `Ctrl mods) -> insert ui (String.make 1 c); redraw ui
    | `Key (`Uchar u, mods) when not (List.mem `Ctrl mods) ->
      let b = Buffer.create 4 in Buffer.add_utf_8_uchar b u; insert ui (Buffer.contents b); redraw ui
    | `Resize _ -> redraw ui
    | `End -> ui.running <- false
    | _ -> ()
  done;
  Term.release term
