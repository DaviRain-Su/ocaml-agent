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

(* Context window for a model id (exact match), if known. *)
let context_window id =
  match List.find_opt (fun e -> e.id = id) catalog with Some e -> Some e.context_window | None -> None

(* Catalog entries whose provider or id contains [pat] (case-insensitive). *)
let list ?(pat = "") () =
  let p = String.lowercase_ascii pat in
  let contains hay = p = "" || (
    let hay = String.lowercase_ascii hay in
    let nh = String.length hay and np = String.length p in
    let rec go i = if i + np > nh then false else if String.sub hay i np = p then true else go (i + 1) in
    go 0)
  in
  List.filter (fun e -> contains e.provider || contains e.id) catalog
