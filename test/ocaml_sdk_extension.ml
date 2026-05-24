open Agent_lib

let member_string name json =
  match Yojson.Safe.Util.member name json with
  | `String s -> s
  | _ -> ""

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
  Extension_sdk.run ()
