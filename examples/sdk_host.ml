(* End-to-end SDK host — runs fully offline (no network, no API key).

   Ties the native-SDK pieces together:
   - Extension_sdk : the "add" tool lives in a separate native extension binary
     (sdk_extension.ml); this host invokes it over the extension stdin/stdout
     protocol, exactly as the agent would drive any native extension.
   - Llm.client    : an isolated client with its own tool registry + transport.
   - Transport.t   : a mock HTTP transport standing in for curl, so the agent
     loop runs deterministically without contacting a provider.
   - Llm.make_config : build a config programmatically (no env vars).
   - Agent.create ~client / Agent.send : drive the agent loop end to end.

   Flow: agent -> "add" tool -> sdk_extension subprocess -> result -> agent ->
   final answer. The mock provider asks to use "add", then returns a final text
   answer once it sees the tool result.

   Build & run:  dune exec examples/sdk_host.exe *)

open Agent_lib
module J = Yojson.Safe
module U = Yojson.Safe.Util

(* The extension binary is built alongside this host in the same directory. *)
let extension_exe = Filename.concat (Filename.dirname Sys.executable_name) "sdk_extension.exe"

let read_all ic =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buf ic 4096
     done
   with End_of_file -> ());
  Buffer.contents buf

(* Send one JSON request to the extension on stdin, read its one JSON reply. *)
let call_extension (request : J.t) : J.t =
  let ic, oc = Unix.open_process extension_exe in
  output_string oc (J.to_string request);
  close_out oc;
  let out = read_all ic in
  ignore (Unix.close_process (ic, oc));
  try J.from_string (String.trim out) with _ -> `Null

(* --- bridge a tool from the native extension into a client's registry --- *)

let add_name = "add"
let add_description = "Add two integers and return the sum."

let add_parameters =
  `Assoc
    [ ("type", `String "object");
      ( "properties",
        `Assoc
          [ ("a", `Assoc [ ("type", `String "number") ]);
            ("b", `Assoc [ ("type", `String "number") ]) ] );
      ("required", `List [ `String "a"; `String "b" ]) ]

let bridge_extension_tool ~reg ~name ~description ~parameters =
  let execute input =
    let resp = call_extension (`Assoc [ ("mode", `String "execute"); ("tool", `String name); ("input", input) ]) in
    match (U.member "ok" resp, U.member "text" resp) with
    | `Bool true, `String text -> text
    | _ -> ( match U.member "error" resp with `String e -> "Error: " ^ e | _ -> "Error: bad extension response")
  in
  ignore (Tools.register ~reg { Tools.name; description; parameters; requires_approval = false; execute })

(* --- a mock HTTP transport (stands in for curl) --- *)

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

(* --- assemble an isolated client and run the agent --- *)

let () =
  (* Show the raw extension protocol once, for illustration. *)
  let probe = call_extension (`Assoc [ ("mode", `String "command"); ("command", `String "greet"); ("args", `String "Ada") ]) in
  Printf.printf "extension /greet -> %s\n" (J.to_string probe);

  let client = Llm.create_client () in
  bridge_extension_tool ~reg:client.Llm.tools ~name:add_name ~description:add_description ~parameters:add_parameters;
  Llm.set_transport ~client mock_transport;
  let cfg =
    Llm.make_config ~provider:Llm.Anthropic ~base_url:"https://mock.invalid" ~api_key:"demo-key"
      ~model:"demo-model" ()
  in
  let agent = Agent.create ~client cfg in
  print_endline "=== running agent (offline mock transport; tool runs in extension subprocess) ===";
  let answer = Agent.send agent "What is 2 + 3? Use the add tool." in
  Printf.printf "\n=== final answer ===\n%s\n" answer
