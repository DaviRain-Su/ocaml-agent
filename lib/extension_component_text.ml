open Yojson.Safe.Util

let string_member names json =
  List.find_map
    (fun name ->
      match json |> member name with
      | `String s when String.trim s <> "" -> Some s
      | _ -> None)
    names

let rec component_plain_text json =
  match json with
  | `String s -> s
  | `List xs -> xs |> List.map component_plain_text |> List.filter (fun s -> String.trim s <> "") |> String.concat "\n"
  | `Assoc _ as obj -> (
    match string_member [ "type"; "kind"; "component" ] obj with
    | Some "text" -> Option.value (string_member [ "text"; "value"; "label" ] obj) ~default:""
    | Some "markdown" -> Option.value (string_member [ "markdown"; "text"; "value" ] obj) ~default:""
    | Some "code" ->
      let lang = Option.value (string_member [ "language"; "lang" ] obj) ~default:"" in
      let code = Option.value (string_member [ "code"; "text"; "value" ] obj) ~default:"" in
      "```" ^ lang ^ "\n" ^ code ^ "\n```"
    | Some "link" ->
      let label = Option.value (string_member [ "label"; "text"; "title"; "href"; "url" ] obj) ~default:"" in
      (match string_member [ "href"; "url" ] obj with
       | Some url when url <> label -> label ^ " <" ^ url ^ ">"
       | _ -> label)
    | Some "button" -> "[" ^ Option.value (string_member [ "label"; "text"; "title" ] obj) ~default:"button" ^ "]"
    | Some ("list" | "ul" | "ol") -> component_list_text obj
    | Some "table" -> component_table_text obj
    | Some ("panel" | "card" | "section") -> component_panel_text obj
    | _ -> (
      match component_children obj with
      | [] -> Option.value (string_member [ "text"; "markdown"; "body"; "label"; "title" ] obj) ~default:(Yojson.Safe.to_string obj)
      | children -> component_plain_text (`List children)))
  | json -> Yojson.Safe.to_string json

and component_children obj =
  match obj |> member "children" with
  | `List xs -> xs
  | _ -> (
    match obj |> member "content" with
    | `List xs -> xs
    | `String s -> [ `String s ]
    | _ -> (
      match obj |> member "body" with
      | `List xs -> xs
      | `String s -> [ `String s ]
      | _ -> []))

and component_list_text obj =
  let items =
    match obj |> member "items" with
    | `List xs -> xs
    | _ -> component_children obj
  in
  items
  |> List.mapi (fun i item ->
         let marker =
           match string_member [ "ordered"; "type"; "kind" ] obj with
           | Some "true" | Some "ol" -> string_of_int (i + 1) ^ ". "
           | _ -> "- "
         in
         marker ^ component_plain_text item)
  |> String.concat "\n"

and component_table_text obj =
  let cell_text cell =
    match cell with
    | `String s -> s
    | `Int n -> string_of_int n
    | `Float f -> string_of_float f
    | _ -> component_plain_text cell
  in
  let columns =
    match obj |> member "columns" with
    | `List xs -> List.map cell_text xs
    | _ -> []
  in
  let row_cells row =
    match row with
    | `List xs -> List.map cell_text xs
    | `Assoc fields when columns <> [] ->
      List.map
        (fun col ->
          match List.assoc_opt col fields with
          | Some value -> cell_text value
          | None -> "")
        columns
    | _ -> [ cell_text row ]
  in
  let rows =
    match obj |> member "rows" with
    | `List xs -> List.map row_cells xs
    | _ -> []
  in
  let all_rows = if columns = [] then rows else columns :: rows in
  let widths =
    List.fold_left
      (fun widths row ->
        List.mapi (fun i cell -> max (String.length cell) (try List.nth widths i with _ -> 0)) row)
      [] all_rows
  in
  let pad width s = s ^ String.make (max 0 (width - String.length s)) ' ' in
  let render_row row =
    "| "
    ^ (widths
       |> List.mapi (fun i width -> pad width (try List.nth row i with _ -> ""))
       |> String.concat " | ")
    ^ " |"
  in
  let sep = "|-" ^ (widths |> List.map (fun width -> String.make width '-') |> String.concat "-|-") ^ "-|" in
  match all_rows with
  | [] -> ""
  | header :: rest when columns <> [] -> String.concat "\n" (render_row header :: sep :: List.map render_row rest)
  | rows -> rows |> List.map render_row |> String.concat "\n"

and component_panel_text obj =
  let title = string_member [ "title"; "label"; "header" ] obj in
  let body = `List (component_children obj) |> component_plain_text |> String.split_on_char '\n' in
  let body = List.filter (fun line -> String.trim line <> "") body in
  let title_text = Option.value title ~default:"" in
  let width =
    List.fold_left max (String.length title_text)
      (List.map String.length body)
    |> max 4
  in
  let border = "+" ^ String.make (width + 2) '-' ^ "+" in
  let line s = "| " ^ s ^ String.make (max 0 (width - String.length s)) ' ' ^ " |" in
  let title_lines = if title_text = "" then [] else [ line title_text; border ] in
  String.concat "\n" (border :: title_lines @ List.map line body @ [ border ])

let components_text components =
  components |> List.map component_plain_text |> List.filter (fun s -> String.trim s <> "") |> String.concat "\n"
