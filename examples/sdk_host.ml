(* End-to-end SDK host — runs fully offline (no network, no API key).

   Uses only the curated public facade [Ocaml_agent] and the in-process
   extension API:
   - Extension_sdk : register an "add" tool, then mount it in-process via
     [tool_specs] / [invoke_tool] (no subprocess; see sdk_extension.ml for the
     stdin/stdout protocol variant).
   - Llm.client / Llm.client_tools : an isolated client whose tool registry we
     populate with the bridged tool.
   - Transport.t : a mock HTTP transport standing in for curl, so the agent loop
     runs deterministically.
   - Llm.make_config : build a config programmatically (no env vars).
   - Agent.create ~client / Agent.send : drive the agent loop end to end.

   Flow: agent -> "add" tool -> Extension_sdk.invoke_tool -> result -> agent ->
   final answer.

   Build & run:  dune exec examples/sdk_host.exe *)

open Agent_lib.Ocaml_agent
module J = Yojson.Safe
module U = Yojson.Safe.Util

(* --- 1. Define a custom tool with the extension SDK (in this process) --- *)

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
    ()

(* --- 2. Mount every registered SDK tool into a client's registry --- *)

(* The agent's registry holds Tools.tool values whose [execute] returns a string;
   extension-SDK tools return JSON {ok; text}. We bridge each registered tool by
   calling Extension_sdk.invoke_tool in-process and extracting the text. *)
let install_sdk_tools ~reg =
  List.iter
    (fun (name, description, parameters) ->
      let execute input =
        let resp = Extension_sdk.invoke_tool name input in
        match (U.member "ok" resp, U.member "text" resp) with
        | `Bool true, `String text -> text
        | _ -> ( match U.member "error" resp with `String e -> "Error: " ^ e | _ -> J.to_string resp)
      in
      ignore (Tools.register ~reg { Tools.name; description; parameters; requires_approval = false; execute }))
    (Extension_sdk.tool_specs ())

(* --- 3. A mock HTTP transport (stands in for curl) --- *)

let request_has_tool_result body =
  match U.member "messages" body with
  | `List msgs ->
    List.exists
      (fun m ->
        match U.member "content" m with
        | `List blocks -> List.exists (fun b -> U.member "type" b = `String "tool_result") blocks
        | _ -> false)
      msgs
  | _ -> false

(* Anthropic-style streaming SSE: a tool_use calling "add" on the first round,
   then a final text answer once the request carries the tool result. *)
let mock_transport : Transport.t =
  { Transport.post_stream =
      (fun ~url:_ ~headers:_ ~on_line body ->
        let emit j = on_line ("data: " ^ J.to_string j) in
        emit
          (`Assoc
            [ ("type", `String "message_start");
              ("message", `Assoc [ ("usage", `Assoc [ ("input_tokens", `Int 12); ("output_tokens", `Int 0) ]) ]) ]);
        (if request_has_tool_result body then begin
           emit
             (`Assoc
               [ ("type", `String "content_block_start");
                 ("index", `Int 0);
                 ("content_block", `Assoc [ ("type", `String "text"); ("text", `String "") ]) ]);
           emit
             (`Assoc
               [ ("type", `String "content_block_delta");
                 ("index", `Int 0);
                 ("delta", `Assoc [ ("type", `String "text_delta"); ("text", `String "The sum is 5.") ]) ]);
           emit (`Assoc [ ("type", `String "content_block_stop"); ("index", `Int 0) ])
         end
         else begin
           emit
             (`Assoc
               [ ("type", `String "content_block_start");
                 ("index", `Int 0);
                 ( "content_block",
                   `Assoc
                     [ ("type", `String "tool_use");
                       ("id", `String "toolu_demo");
                       ("name", `String "add");
                       ("input", `Assoc []) ] ) ]);
           emit
             (`Assoc
               [ ("type", `String "content_block_delta");
                 ("index", `Int 0);
                 ("delta", `Assoc [ ("type", `String "input_json_delta"); ("partial_json", `String "{\"a\":2,\"b\":3}") ]) ]);
           emit (`Assoc [ ("type", `String "content_block_stop"); ("index", `Int 0) ])
         end);
        emit (`Assoc [ ("type", `String "message_delta"); ("usage", `Assoc [ ("output_tokens", `Int 6) ]) ]));
    post_json = (fun ~url:_ ~headers:_ _ -> `Assoc []) }

(* --- 4. Assemble an isolated client and run the agent --- *)

let () =
  let client = Llm.create_client () in
  install_sdk_tools ~reg:(Llm.client_tools client);
  Llm.set_transport ~client mock_transport;
  let cfg =
    Llm.make_config ~provider:Llm.Anthropic ~base_url:"https://mock.invalid" ~api_key:"demo-key"
      ~model:"demo-model" ()
  in
  let agent = Agent.create ~client cfg in
  print_endline "=== running agent (offline mock transport; tool runs in-process via Extension_sdk) ===";
  let answer = Agent.send agent "What is 2 + 3? Use the add tool." in
  Printf.printf "\n=== final answer ===\n%s\n" answer
