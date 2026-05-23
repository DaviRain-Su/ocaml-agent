(* Lightweight terminal rendering: a line-buffered markdown renderer that works
   while text is still streaming, plus colorized previews for tool results.

   Streaming constraint: we can only style a line once it's complete, so the
   renderer buffers the current partial line and flushes styled lines as their
   trailing newline arrives. Inline styling (bold, code) is applied per line. *)

let reset = "\027[0m"
let bold s = "\027[1m" ^ s ^ reset
let dim s = "\027[2m" ^ s ^ reset
let cyan s = "\027[36m" ^ s ^ reset
let green s = "\027[32m" ^ s ^ reset
let red s = "\027[31m" ^ s ^ reset

let bold_re = Str.regexp "\\*\\*\\([^*]+\\)\\*\\*"
let code_re = Str.regexp "`\\([^`]+\\)`"
let header_re = Str.regexp "^\\(#+\\)[ \t]+\\(.*\\)$"
let bullet_re = Str.regexp "^\\([ \t]*\\)\\([-*]\\)[ \t]+"

(* Apply inline **bold** and `code` styling to a single line. *)
let style_inline line =
  let line = Str.global_replace bold_re ("\027[1m\\1" ^ reset) line in
  Str.global_replace code_re ("\027[36m\\1" ^ reset) line

(* Render one complete markdown line given the current code-fence state.
   Returns (styled_line, new_in_code). *)
let render_line ~in_code line =
  let trimmed = String.trim line in
  let is_fence = String.length trimmed >= 3 && String.sub trimmed 0 3 = "```" in
  if is_fence then (dim line, not in_code)
  else if in_code then (cyan line, true)
  else if Str.string_match header_re line 0 then (bold (Str.matched_group 2 line), false)
  else if Str.string_match bullet_re line 0 then
    let indent = Str.matched_group 1 line in
    let rest = Str.string_after line (Str.match_end ()) in
    (indent ^ cyan "•" ^ " " ^ style_inline rest, false)
  else (style_inline line, false)

(* Stateful streaming renderer. *)
type t = { buf : Buffer.t; mutable in_code : bool }

let create () = { buf = Buffer.create 120; in_code = false }

(* Feed a streamed chunk; print every line that is now complete. *)
let feed t (chunk : string) =
  Buffer.add_string t.buf chunk;
  let s = Buffer.contents t.buf in
  let rec loop start =
    match String.index_from_opt s start '\n' with
    | Some nl ->
      let line = String.sub s start (nl - start) in
      let styled, in_code = render_line ~in_code:t.in_code line in
      t.in_code <- in_code;
      print_string styled;
      print_char '\n';
      loop (nl + 1)
    | None ->
      (* keep the trailing partial line for the next chunk *)
      Buffer.clear t.buf;
      Buffer.add_string t.buf (String.sub s start (String.length s - start))
  in
  loop 0;
  flush stdout

(* Flush any buffered partial line at end of stream. *)
let finish t =
  let rem = Buffer.contents t.buf in
  if rem <> "" then begin
    let styled, _ = render_line ~in_code:t.in_code rem in
    print_string styled;
    print_char '\n'
  end;
  Buffer.clear t.buf;
  t.in_code <- false;
  flush stdout

(* Colorized, truncated preview of a tool result. Diff lines (-, +, @@) are
   colored; everything else is dimmed. *)
let tool_result ?(max_lines = 12) result =
  let lines = String.split_on_char '\n' result in
  let shown = List.filteri (fun i _ -> i < max_lines) lines in
  let style line =
    if line = "" then ""
    else
      match line.[0] with
      | '-' -> red line
      | '+' -> green line
      | '@' -> dim line
      | _ -> dim line
  in
  let body = String.concat "\n" (List.map style shown) in
  if List.length lines > max_lines then
    body ^ "\n" ^ dim (Printf.sprintf "  ... (%d more lines)" (List.length lines - max_lines))
  else body
