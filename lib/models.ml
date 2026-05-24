(* A small, best-effort catalog of known models and their context windows. Used
   to size the context-usage gauge and to back `--list-models`. Values are
   approximate and may drift; AGENT_CONTEXT_WINDOW always overrides. *)

type entry = { provider : string; id : string; context_window : int }

let catalog : entry list =
  [ { provider = "anthropic"; id = "claude-opus-4-7"; context_window = 200000 };
    { provider = "anthropic"; id = "claude-sonnet-4-6"; context_window = 200000 };
    { provider = "deepseek"; id = "deepseek-v4-flash"; context_window = 1000000 };
    { provider = "deepseek"; id = "deepseek-v4-pro"; context_window = 1000000 };
    { provider = "kimi"; id = "kimi-for-coding"; context_window = 262144 };
    { provider = "kimi"; id = "kimi-k2-thinking"; context_window = 262144 };
    { provider = "moonshot"; id = "kimi-k2-0905-preview"; context_window = 131072 };
    { provider = "openai"; id = "gpt-4o"; context_window = 128000 };
    { provider = "openai"; id = "gpt-4o-mini"; context_window = 128000 };
    { provider = "openrouter"; id = "openai/gpt-4o"; context_window = 128000 };
    { provider = "groq"; id = "llama-3.3-70b-versatile"; context_window = 131072 };
    { provider = "xai"; id = "grok-2-latest"; context_window = 131072 };
    { provider = "mistral"; id = "mistral-large-latest"; context_window = 131072 };
    { provider = "zai"; id = "glm-4.6"; context_window = 204800 };
    { provider = "zai"; id = "glm-4.5"; context_window = 131072 };
    { provider = "gemini"; id = "gemini-2.0-flash"; context_window = 1048576 } ]

let extension_catalog : entry list ref = ref []

let all () = !extension_catalog @ catalog

let register_extension_model entry =
  extension_catalog :=
    entry
    :: List.filter
         (fun e -> not (e.provider = entry.provider && e.id = entry.id))
         !extension_catalog

let clear_extension_models () =
  extension_catalog := []

(* Context window for a model id (exact match), if known. *)
let context_window id =
  match List.find_opt (fun e -> e.id = id) (all ()) with Some e -> Some e.context_window | None -> None

(* Catalog entries whose provider or id contains [pat] (case-insensitive). *)
let list ?(pat = "") () =
  let catalog = all () in
  if pat = "" then catalog
  else
    let p = String.lowercase_ascii pat in
    let re = try Some (Str.regexp_string p) with _ -> None in
    match re with
    | None -> []
    | Some re ->
      let contains hay =
        match Str.search_forward re (String.lowercase_ascii hay) 0 with
        | _ -> true
        | exception Not_found -> false
      in
      List.filter
        (fun e ->
          contains e.provider || contains e.id)
        catalog

let split_scoped_patterns s =
  s |> String.split_on_char '\n' |> List.concat_map Model_spec.split_csv

let glob_to_regex pat =
  let b = Buffer.create (String.length pat * 2) in
  Buffer.add_char b '^';
  String.iter
    (function
      | '*' -> Buffer.add_string b ".*"
      | '?' -> Buffer.add_char b '.'
      | c when List.mem c [ '.'; '+'; '('; ')'; '['; ']'; '{'; '}'; '^'; '$'; '|'; '\\' ] ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
      | c -> Buffer.add_char b c)
    pat;
  Buffer.add_char b '$';
  Buffer.contents b

let pattern_without_thinking pattern = fst (Model_spec.split_thinking pattern)

let pattern_matches pattern (e : entry) =
  let pattern = String.lowercase_ascii (pattern_without_thinking pattern) in
  if pattern = "" then false
  else
    let candidates =
      [ String.lowercase_ascii e.provider;
        String.lowercase_ascii e.id;
        String.lowercase_ascii (e.provider ^ "/" ^ e.id) ]
    in
    if String.contains pattern '*' || String.contains pattern '?' then
      let re = Str.regexp (glob_to_regex pattern) in
      List.exists
        (fun candidate ->
          match Str.string_match re candidate 0 with
          | true -> Str.match_end () = String.length candidate
          | false -> false
          | exception _ -> false)
        candidates
    else
      let re = Str.regexp_string pattern in
      List.exists
        (fun candidate ->
          match Str.search_forward re candidate 0 with
          | _ -> true
          | exception Not_found -> false)
        candidates

let scoped patterns =
  match patterns with
  | [] -> all ()
  | patterns -> List.filter (fun e -> List.exists (fun p -> pattern_matches p e) patterns) (all ())

let scoped_from_env () =
  match Sys.getenv_opt "AGENT_SCOPED_MODELS" with
  | Some s when String.trim s <> "" -> scoped (split_scoped_patterns s)
  | _ -> scoped (Settings.string_list "enabledModels")
