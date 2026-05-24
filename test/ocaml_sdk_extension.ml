open Agent_lib

let member_string name json =
  match Yojson.Safe.Util.member name json with
  | `String s -> s
  | _ -> ""

let contains hay needle =
  try ignore (Str.search_forward (Str.regexp_string needle) hay 0); true with Not_found -> false

let () =
  Extension_sdk.register_tool ~name:"ocaml_greet" ~description:"Greet from an OCaml SDK extension"
    ~parameters:
      (`Assoc
        [ ("type", `String "object");
          ( "properties",
            `Assoc [ ("name", `Assoc [ ("type", `String "string") ]) ] );
          ("required", `List [ `String "name" ]) ])
    ~execute:(fun input -> "Hello " ^ member_string "name" input)
    ();
  Extension_sdk.register_command ~name:"ocamlhello" ~description:"Say hello from OCaml"
    ~argument_hint:"<name>"
    ~complete:(fun prefix ->
      [ "ocaml"; "pi"; "sdk" ]
      |> List.filter (fun item ->
             String.length item >= String.length prefix
             && String.sub item 0 (String.length prefix) = prefix))
    ~handler:(fun args -> "OCaml command " ^ if String.trim args = "" then "world" else String.trim args)
    ();
  Extension_sdk.register_command_response ~name:"ocamlui" ~description:"Return OCaml SDK UI surfaces"
    ~handler:(fun _args ->
      Extension_sdk.response
        ~ui:
          (Extension_sdk.ui
             ~notifications:[ "ocaml notice" ]
             ~surfaces:
               [ `Assoc
                   [ ("kind", `String "status");
                     ("key", `String "ocaml");
                     ("text", `String "ready") ];
                 `Assoc
                   [ ("kind", `String "widget");
                     ("key", `String "ocaml-widget");
                     ("lines", `List [ `String "OCaml widget" ]) ] ]
             ())
        "ui ok")
    ();
  Extension_sdk.register_provider ~name:"ocamlai" ~aliases:[ "ocaml-runtime" ] ~default_model:"ocaml-small"
    ~models:[ `Assoc [ ("id", `String "ocaml-small"); ("contextWindow", `Int 4321) ] ]
    ~complete:(fun request ->
      let model = member_string "model" request in
      let system = member_string "system" request in
      let message_count =
        match Yojson.Safe.Util.member "messages" request with
        | `List xs -> List.length xs
        | _ -> 0
      in
      Extension_sdk.ok
        [ ( "content",
            `List
              [ `Assoc
                  [ ("type", `String "text");
                    ("text", `String (Printf.sprintf "ocaml provider %s:%b:%d" model (contains system "SDK-SYSTEM") message_count)) ] ] );
          ("usage", `Assoc [ ("inputTokens", `Int 5); ("outputTokens", `Int 2) ]) ])
    ();
  Extension_sdk.on "before_provider_request" (fun event ->
    match Yojson.Safe.Util.member "payload" event with
    | `Assoc fields -> Some (`Assoc (("ocamlHooked", `Bool true) :: List.remove_assoc "ocamlHooked" fields))
    | payload -> Some payload);
  Extension_sdk.on "after_provider_response" (fun event ->
    let status =
      match Yojson.Safe.Util.member "status" event with
      | `Int n -> string_of_int n
      | _ -> "unknown"
    in
    let oc = open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 "ocaml-hooks.log" in
    output_string oc ("after " ^ status ^ "\n");
    close_out oc;
    None);
  Extension_sdk.run ()
