(* A native OCaml agent extension built with Extension_sdk.

   Extension_sdk is a stdin/stdout protocol framework: you register tools,
   commands, providers, and event hooks, then call [run ()], which reads one
   JSON request from stdin, dispatches it, and writes one JSON response. This is
   the native-OCaml counterpart to the JS/TS extension bridge.

   Try it directly:
     echo '{"mode":"describe"}'                                  | ./sdk_extension.exe
     echo '{"mode":"execute","tool":"add","input":{"a":2,"b":3}}' | ./sdk_extension.exe
     echo '{"mode":"command","command":"greet","args":"Ada"}'     | ./sdk_extension.exe

   The companion host (sdk_host.ml) spawns this binary and uses the "add" tool
   from inside an agent loop. *)

open Agent_lib
module U = Yojson.Safe.Util

let () =
  Extension_sdk.register_tool ~name:"add" ~description:"Add two integers and return the sum."
    ~parameters:
      (`Assoc
        [ ("type", `String "object");
          ( "properties",
            `Assoc
              [ ("a", `Assoc [ ("type", `String "number") ]);
                ("b", `Assoc [ ("type", `String "number") ]) ] );
          ("required", `List [ `String "a"; `String "b" ]) ])
    ~execute:(fun input ->
      let n key = match U.member key input with `Int i -> i | `Float f -> int_of_float f | _ -> 0 in
      string_of_int (n "a" + n "b"))
    ();
  Extension_sdk.register_command ~name:"greet" ~description:"Greet someone by name."
    ~handler:(fun args -> Printf.sprintf "Hello, %s!" (if String.trim args = "" then "world" else args))
    ();
  Extension_sdk.run ()
