type parsed = { provider : string option; model : string option; thinking : string option }

let valid_thinking_level s =
  match String.lowercase_ascii (String.trim s) with
  | "off" | "none" | "minimal" | "low" | "medium" | "high" | "xhigh" -> true
  | _ -> false

let normalize_thinking s =
  match String.lowercase_ascii (String.trim s) with
  | "none" | "off" | "" -> "off"
  | "minimal" -> "low"
  | "xhigh" -> "high"
  | other -> other

let split_thinking spec =
  match String.rindex_opt spec ':' with
  | Some i when i > 0 && i + 1 < String.length spec ->
    let suffix = String.sub spec (i + 1) (String.length spec - i - 1) in
    if valid_thinking_level suffix then (String.sub spec 0 i, Some suffix) else (spec, None)
  | _ -> (spec, None)

let split_provider_model spec =
  match String.index_opt spec '/' with
  | Some i when i > 0 && i + 1 < String.length spec ->
    let prefix = String.sub spec 0 i in
    if Llm.is_known_provider prefix then
      Some (prefix, String.sub spec (i + 1) (String.length spec - i - 1))
    else None
  | _ -> None

let parse ?provider ?thinking spec =
  match spec with
  | None -> { provider; model = None; thinking }
  | Some raw ->
    let model, suffix_thinking = split_thinking raw in
    let thinking = match thinking with Some _ -> thinking | None -> suffix_thinking in
    (match provider with
     | Some _ -> { provider; model = Some model; thinking }
     | None -> (
       match split_provider_model model with
       | Some (provider, model) -> { provider = Some provider; model = Some model; thinking }
       | None when Llm.is_known_provider model -> { provider = Some model; model = None; thinking }
       | None -> { provider = None; model = Some model; thinking }))

let split_csv s =
  s |> String.split_on_char ',' |> List.map String.trim |> List.filter (fun x -> x <> "")
