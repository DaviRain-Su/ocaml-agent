(* Full-screen terminal UI built on notty: a scrollback viewport above a live
   input editor. The Agent core is unchanged — we install a frontend whose
   callbacks append styled lines and redraw. Streaming happens inside the
   (blocking) Agent.send call; a background spinner thread animates a "thinking"
   indicator while the turn runs. All terminal writes are serialized by a mutex. *)

open Notty
open Notty_unix

type ui =
  { term : Term.t;
    mutable lines : (A.t * string) list list; (* scrollback (oldest first); each line is a list of styled segments *)
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
    mutable menu_sel : int; (* selected row in the live slash-command menu *)
    mutable pasting : bool; (* inside a bracketed-paste sequence *)
    mtx : Mutex.t }

let menu_max = 8

let max_lines = 5000
let spinner = [| "\xe2\xa0\x8b"; "\xe2\xa0\x99"; "\xe2\xa0\xb9"; "\xe2\xa0\xb8"; "\xe2\xa0\xbc"; "\xe2\xa0\xb4"; "\xe2\xa0\xa6"; "\xe2\xa0\xa7"; "\xe2\xa0\x87"; "\xe2\xa0\x8f" |]

(* --- UTF-8 cursor helpers --- *)
let is_cont c = Char.code c land 0xC0 = 0x80
let prev_cp_start s i = let j = ref (i - 1) in while !j > 0 && is_cont s.[!j] do decr j done; max 0 !j
let next_cp_start s i = let n = String.length s in let j = ref (i + 1) in while !j < n && is_cont s.[!j] do incr j done; !j
let cp_count s upto = let c = ref 0 in String.iteri (fun i ch -> if i < upto && not (is_cont ch) then incr c) s; !c

(* --- scrollback --- *)
let push_segs ui (segs : (A.t * string) list) =
  ui.lines <- ui.lines @ [ segs ];
  let len = List.length ui.lines in
  if len > max_lines then ui.lines <- List.filteri (fun i _ -> i >= len - max_lines) ui.lines

(* Single-segment line; preserves the old push API for plain lines. *)
let push ui attr s = push_segs ui [ (attr, s) ]

(* Split a UTF-8 string into single-codepoint substrings. *)
let codepoints s =
  let n = String.length s in
  let rec go i acc = if i >= n then List.rev acc else let j = next_cp_start s i in go j (String.sub s i (j - i) :: acc) in
  go 0 []

(* Parse inline **bold** and `code` into styled segments (markers removed). *)
let style_inline base s =
  let segs = ref [] and buf = Buffer.create 16 and bold = ref false and code = ref false in
  let active () =
    let a = base in
    let a = if !bold then A.(a ++ st bold) else a in
    if !code then A.(a ++ fg cyan) else a
  in
  let flush () = if Buffer.length buf > 0 then (segs := (active (), Buffer.contents buf) :: !segs; Buffer.clear buf) in
  let n = String.length s and i = ref 0 in
  while !i < n do
    if !i + 1 < n && s.[!i] = '*' && s.[!i + 1] = '*' then (flush (); bold := not !bold; i := !i + 2)
    else if s.[!i] = '`' then (flush (); code := not !code; incr i)
    else (Buffer.add_char buf s.[!i]; incr i)
  done;
  flush ();
  match List.rev !segs with [] -> [ (base, "") ] | l -> l

(* Wrap a styled-segment line to width [w], coalescing runs of the same attr.
   Embedded newlines start a new display line; tabs expand to two spaces; other
   control characters are dropped (notty's I.string rejects control chars). *)
let wrap_segs w segs =
  if w <= 0 then [ segs ]
  else begin
    let out = ref [] and line = ref [] and col = ref 0 in
    let buf = Buffer.create 32 and cur = ref A.empty in
    let flush_run () = if Buffer.length buf > 0 then (line := (!cur, Buffer.contents buf) :: !line; Buffer.clear buf) in
    let newline () = flush_run (); out := List.rev !line :: !out; line := []; col := 0 in
    let add_str s n = if !col >= w then (newline (); cur := !cur); Buffer.add_string buf s; col := !col + n in
    List.iter
      (fun (a, s) ->
        flush_run ();
        cur := a;
        List.iter
          (fun cp ->
            if cp = "\n" then newline ()
            else if String.length cp = 1 && (Char.code cp.[0] < 0x20 || Char.code cp.[0] = 0x7f) then (
              if cp.[0] = '\t' then add_str "  " 2)
            else add_str cp 1)
          (codepoints s))
      segs;
    flush_run ();
    out := List.rev !line :: !out;
    List.rev !out
  end

(* Strip control chars (tabs -> two spaces) from a single line for safe display. *)
let safe_line s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c -> if c = '\t' then Buffer.add_string b "  " else if Char.code c >= 0x20 && Char.code c <> 0x7f then Buffer.add_char b c)
    s;
  Buffer.contents b

let prompt_label = "you> "

let sublist start len l = l |> List.filteri (fun i _ -> i >= start && i < start + len)

let cursor_row_col input cursor =
  let row = ref 0 and col = ref 0 in
  String.iteri
    (fun i ch ->
      if i < cursor then
        if ch = '\n' then (
          incr row;
          col := 0)
        else if not (is_cont ch) then incr col)
    input;
  (!row, !col)

let input_window ~height ~menu_rows ~cursor_row ~total_rows =
  let capacity = max 1 (height - 3 - menu_rows) in
  let rows = min total_rows capacity in
  let max_start = max 0 (total_rows - rows) in
  let start =
    if cursor_row < rows then 0
    else min max_start (cursor_row - rows + 1)
  in
  (start, rows)

let redraw ui =
  Mutex.lock ui.mtx;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock ui.mtx)
    (fun () ->
      try
      let w, h = Term.size ui.term in
      let w = max 1 w and h = max 4 h in
      (* Live slash-command completion menu (clamped). *)
      let matches = Complete.menu ui.input in
      let menu_rows = min menu_max (List.length matches) in
      if menu_rows > 0 && ui.menu_sel >= menu_rows then ui.menu_sel <- menu_rows - 1;
      if ui.menu_sel < 0 then ui.menu_sel <- 0;
      let in_lines = String.split_on_char '\n' ui.input in
      let in_rows = List.length in_lines in
      let cursor_row, cursor_col = cursor_row_col ui.input ui.cursor in
      let input_start, input_rows = input_window ~height:h ~menu_rows ~cursor_row ~total_rows:in_rows in
      let visible = max 1 (h - 2 - menu_rows - input_rows) in
      let all = List.concat_map (wrap_segs w) ui.lines in
      let total = List.length all in
      (* maxscroll: how many lines we can scroll up from the bottom.
         Ensure it's at least 0, and allow scrolling even when total == visible
         (in which case we still want to see earlier content if it exists). *)
      let maxscroll = max 0 (total - visible) in
      (* Only clamp scroll if it genuinely exceeds the content; preserve user
         intent when they have explicitly scrolled. *)
      if ui.scroll > maxscroll then ui.scroll <- maxscroll;
      let start = max 0 (total - visible - ui.scroll) in
      let window = sublist start visible all in
      let line_img segs = match segs with [] -> I.void 1 1 | _ -> I.hcat (List.map (fun (a, s) -> I.string a s) segs) in
      (* When scrolling, align from the top so the oldest visible line is at
         the top of the viewport. When not scrolling, align from the bottom
         so the newest content appears just above the input area. *)
      let body =
        if ui.scroll > 0 then
          I.vcat (List.map line_img window)
        else
          I.vsnap ~align:`Bottom visible (I.vcat (List.map line_img window))
      in
      let menu_img =
        if menu_rows = 0 then I.void 0 0
        else
          let shown = List.filteri (fun i _ -> i < menu_max) matches in
          I.vcat
            (List.mapi
               (fun i (c, help) ->
                 let selected = i = ui.menu_sel in
                 let mark = if selected then A.(fg black ++ bg cyan) else A.(fg green) in
                 I.(string mark (Printf.sprintf " %-12s " c) <|> string A.(fg (gray 9)) (" " ^ help)))
               shown)
      in
      let status =
        if ui.turn_active then
          I.string A.(fg yellow) (Printf.sprintf "%s thinking… %.0fs" spinner.(ui.spin mod Array.length spinner) (Unix.gettimeofday () -. ui.turn_start))
        else if ui.scroll > 0 then I.string A.(fg (gray 8)) (Printf.sprintf "-- scrolled %d lines (End to return) --" ui.scroll)
        else I.void 1 1
      in
      let sep = I.char A.(fg (gray 6)) '_' w 1 in
      let indent = String.make (String.length prompt_label) ' ' in
      let prompt =
        I.vcat
          (List.mapi
             (fun i l ->
               let absolute = input_start + i in
               let pfx = if absolute = 0 then I.string A.(fg green) prompt_label else I.string A.empty indent in
               I.(pfx <|> string A.empty (safe_line l)))
             (sublist input_start input_rows in_lines))
      in
      Term.image ui.term I.(body <-> status <-> menu_img <-> sep <-> prompt);
      let cursor_y = h - input_rows + (cursor_row - input_start) in
      let cursor_x = min (w - 1) (String.length prompt_label + cursor_col) in
      Term.cursor ui.term (Some (cursor_x, cursor_y))
      with exn ->
        (* Don't silently swallow rendering errors; at least log them. *)
        Printf.eprintf "[tui redraw error] %s\n%!" (Printexc.to_string exn))

(* Styled segments for one streamed assistant line, tracking code-fence state. *)
let asst_segs ui line =
  let t = String.trim line in
  let fence = String.length t >= 3 && String.sub t 0 3 = "```" in
  if fence then (ui.asst_in_code <- not ui.asst_in_code; [ (A.(fg (gray 8)), line) ])
  else if ui.asst_in_code then [ (A.(fg cyan), line) ]
  else if String.length t > 0 && t.[0] = '#' then [ (A.(st bold), line) ]
  else style_inline A.empty line

(* --- frontend callbacks --- *)
let feed_assistant ui s =
  Buffer.add_string ui.buf s;
  let content = Buffer.contents ui.buf in
  let parts = String.split_on_char '\n' content in
  let rec loop = function
    | [ last ] -> Buffer.clear ui.buf; Buffer.add_string ui.buf last
    | line :: rest -> push_segs ui (asst_segs ui line); loop rest
    | [] -> ()
  in
  loop parts;
  redraw ui

let flush_assistant ui =
  let rem = Buffer.contents ui.buf in
  if rem <> "" then push_segs ui (asst_segs ui rem);
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
  try
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
  with e -> ui.quiet <- false; raise e

let make_frontend ui : Agent.frontend =
  { text_delta = (fun s -> feed_assistant ui s);
    text_done = (fun () -> flush_assistant ui);
    thinking = (fun s -> if String.trim s <> "" then (push ui A.(fg (gray 10)) ("\xf0\x9f\x92\xad " ^ s); redraw ui));
    tool_call = (fun name prev -> push ui A.(fg cyan) ("\xe2\x9a\x99 " ^ name ^ " " ^ prev); redraw ui);
    tool_result = (fun res -> if String.trim res <> "" then push_result ui res);
    notice = (fun s -> push ui A.(fg yellow) s; redraw ui);
    message_end = (fun _ _ _ _ -> ());
    tool_result_end = (fun _ -> ());
    confirm_bash =
      (fun cmd ->
        push ui A.(fg yellow) ("\xe2\x9a\xa0 run command: " ^ cmd);
        push ui A.(fg (gray 12)) "approve? [y]es / [N]o / [a]lways";
        redraw ui;
        confirm ui cmd) }

(* --- modal selector --- *)
(* Render and run a blocking list picker; returns the chosen index or None. *)
let select ui ~title (items : string list) : int option =
  if items = [] then None
  else begin
    ui.quiet <- true;
    try
      let n = List.length items in
      let sel = ref 0 in
      let render () =
        Mutex.lock ui.mtx;
        Fun.protect
          ~finally:(fun () -> Mutex.unlock ui.mtx)
          (fun () ->
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
            Term.cursor ui.term None)
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
    with e -> ui.quiet <- false; redraw ui; raise e
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

(* Interactive settings modal: pick a row to toggle/cycle it; Esc closes. *)
let rec settings ui =
  let a = ui.agent in
  let items =
    [ Printf.sprintf "auto-approve cmd  : %b" (Agent.auto_approve a);
      Printf.sprintf "auto-compact      : %b" (Agent.auto_compact a);
      Printf.sprintf "thinking level    : %s" (Agent.config a).Llm.thinking;
      "close" ]
  in
  match select ui ~title:"Settings (Enter toggles)" items with
  | Some 0 -> Agent.set_auto_approve a (not (Agent.auto_approve a)); settings ui
  | Some 1 -> Agent.set_auto_compact a (not (Agent.auto_compact a)); settings ui
  | Some 2 ->
    let next = match (Agent.config a).Llm.thinking with "off" -> "low" | "low" -> "medium" | "medium" -> "high" | _ -> "off" in
    Agent.set_thinking a next;
    settings ui
  | _ -> redraw ui

(* --- slash commands (TUI-native) --- *)
let rec cmd ui line =
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
        "/reload                reload context / skills / prompts / extensions";
        "/settings              toggle auto-approve / compact / thinking";
        "/new                   clear the conversation";
        "Tab / type /           live command completion menu";
        "Tab                    complete command / file path";
        "!cmd / !!cmd          run shell (with / without model context)";
        "Alt-Enter / Ctrl-J     insert a newline (multi-line input)";
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
  | "/reload" :: _ ->
    ignore (Extensions.load ());
    Agent.reload_system_prompt ui.agent;
    push ui A.(fg (gray 12)) "Reloaded resources.";
    redraw ui
  | "/settings" :: _ -> settings ui
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
  | c :: _ -> (
    match Prompts.expand_command line with
    | Some prompt -> run_turn ui prompt
    | None -> push ui A.(fg red) ("Unknown command " ^ c ^ " (try /help)"); redraw ui)
  | [] -> ()

(* Run one turn with an animated spinner thread. *)
and run_turn ui line =
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

and run_bang ui line =
  let starts_with prefix s =
    String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix
  in
  let exclude = starts_with "!!" line in
  let off = if exclude then 2 else 1 in
  let command = String.trim (String.sub line off (String.length line - off)) in
  if command = "" then false
  else begin
    push ui A.(fg cyan) ("$ " ^ command ^ if exclude then " (no context)" else "");
    redraw ui;
    let result = Agent.run_user_bash ~exclude_from_context:exclude ui.agent command in
    push_result ui result;
    true
  end

let submit ui =
  let line = String.trim ui.input in
  ui.input <- "";
  ui.cursor <- 0;
  ui.hist_idx <- -1;
  ui.scroll <- 0;
  if line <> "" then begin
    ui.history <- line :: ui.history;
    (* echo the (possibly multi-line) input under the prompt *)
    let indent = String.make (String.length prompt_label) ' ' in
    List.iteri
      (fun i l -> push ui A.(fg green) ((if i = 0 then prompt_label else indent) ^ l))
      (String.split_on_char '\n' line);
    redraw ui;
    if line.[0] = '!' && run_bang ui line then ()
    else if line.[0] = '/' then cmd ui line
    else run_turn ui line
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
  (* Scroll by a fraction of the screen, not nearly the full height,
     so users can scroll smoothly even with moderate content. *)
  let step = max 1 (h / 2) in
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
  (* Mouse wheel scrolling is enabled by default for a better UX.
     Set AGENT_TUI_MOUSE=0 to disable if it interferes with terminal
     selection/copy. *)
  let mouse = match Sys.getenv_opt "AGENT_TUI_MOUSE" with
    | Some ("0" | "false" | "no") -> false
    | _ -> true
  in
  let term = Term.create ~mouse () in
  let ui =
    { term; lines = []; input = ""; cursor = 0; history = []; hist_idx = -1; buf = Buffer.create 256;
      agent; running = true; scroll = 0; turn_active = false; quiet = false; spin = 0; turn_start = 0.;
      asst_in_code = false; menu_sel = 0; pasting = false; mtx = Mutex.create () }
  in
  Agent.set_frontend agent (make_frontend ui);
  push ui A.(fg green ++ st bold) "OCaml Code Agent";
  push ui A.(fg (gray 12)) (Llm.describe (Agent.config agent));
  push ui A.(fg (gray 12)) "Type your request. /help for commands, Ctrl-P model picker, Ctrl-D to quit.";
  push ui A.(fg (gray 12)) "PgUp/PgDn or mouse wheel to scroll; End to jump to latest.";
  push ui A.(fg (gray 12)) "Set AGENT_TUI_MOUSE=0 if mouse wheel interferes with text selection.";
  push ui A.empty "";
  let km = Keymap.load () in
  redraw ui;
  let menu_items () = Complete.menu ui.input in
  let accept_menu () =
    let m = menu_items () in
    if m <> [] then (
      let i = max 0 (min ui.menu_sel (List.length m - 1)) in
      let c, _ = List.nth m i in
      ui.input <- c;
      ui.cursor <- String.length c;
      ui.menu_sel <- 0)
  in
  (* A Ctrl-key or other ASCII press: a configured action, an editing key, or insert. *)
  let on_ascii c mods =
    let ctrl = List.mem `Ctrl mods in
    match Keymap.lookup km (Char.lowercase_ascii c, ctrl) with
    | Some Keymap.Quit -> ui.running <- false
    | Some Keymap.Model_picker -> model_picker ui
    | Some Keymap.Settings -> settings ui
    | Some Keymap.Help -> cmd ui "/help"
    | None ->
      if ctrl then (
        match Char.lowercase_ascii c with
        | 'a' -> ui.cursor <- 0; redraw ui
        | 'e' -> ui.cursor <- String.length ui.input; redraw ui
        | 'u' -> kill_to_start ui; ui.menu_sel <- 0; redraw ui
        | 'k' -> kill_to_end ui; redraw ui
        | 'w' -> kill_word ui; ui.menu_sel <- 0; redraw ui
        | 'j' -> insert ui "\n"; ui.menu_sel <- 0; redraw ui (* Ctrl-J inserts a newline *)
        | _ -> ())
      else (insert ui (String.make 1 c); ui.menu_sel <- 0; redraw ui)
  in
  Fun.protect
    ~finally:(fun () -> try Term.release term with _ -> ())
    (fun () ->
      while ui.running do
        match Term.event term with
        | `Key (`Tab, _) -> if menu_items () <> [] then (accept_menu (); redraw ui) else complete ui
        | `Paste `Start -> ui.pasting <- true
        | `Paste `End -> ui.pasting <- false; redraw ui
        | `Key (`Enter, mods) when ui.pasting || List.mem `Meta mods -> insert ui "\n"; ui.menu_sel <- 0; redraw ui
        | `Key (`Enter, _) ->
          if menu_items () <> [] && not (List.mem ui.input (Complete.commands ())) then (accept_menu (); redraw ui)
          else submit ui
        | `Key (`Backspace, _) -> backspace ui; ui.menu_sel <- 0; redraw ui
        | `Key (`Escape, _) -> ui.input <- ""; ui.cursor <- 0; ui.menu_sel <- 0; redraw ui
        | `Key (`Page `Up, _) -> page ui 1
        | `Key (`Page `Down, _) -> page ui (-1)
        | `Key (`Home, _) -> ui.cursor <- 0; redraw ui
        | `Key (`End, _) -> ui.scroll <- 0; ui.cursor <- String.length ui.input; redraw ui
        | `Key (`Arrow `Left, _) -> if ui.cursor > 0 then ui.cursor <- prev_cp_start ui.input ui.cursor; redraw ui
        | `Key (`Arrow `Right, _) -> if ui.cursor < String.length ui.input then ui.cursor <- next_cp_start ui.input ui.cursor; redraw ui
        | `Key (`Arrow `Up, _) ->
          if menu_items () <> [] then (ui.menu_sel <- max 0 (ui.menu_sel - 1); redraw ui) else (recall ui 1; redraw ui)
        | `Key (`Arrow `Down, _) ->
          let n = List.length (menu_items ()) in
          if n > 0 then (ui.menu_sel <- min (n - 1) (ui.menu_sel + 1); redraw ui) else (recall ui (-1); redraw ui)
        | `Mouse (`Press (`Scroll `Up), _, _) -> page ui 1
        | `Mouse (`Press (`Scroll `Down), _, _) -> page ui (-1)
        | `Key (`ASCII c, mods) -> on_ascii c mods
        | `Key (`Uchar u, mods) when not (List.mem `Ctrl mods) ->
          let b = Buffer.create 4 in Buffer.add_utf_8_uchar b u; insert ui (Buffer.contents b); ui.menu_sel <- 0; redraw ui
        | `Resize _ -> redraw ui
        | `End -> ui.running <- false
        | _ -> ()
      done)
