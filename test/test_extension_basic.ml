open Agent_lib

let j = Yojson.Safe.from_string
let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let run name input =
  match Tools.find name with
  | Some t -> t.execute (j input)
  | None -> failwith ("no tool " ^ name)

let contains0 hay needle =
  try ignore (Str.search_forward (Str.regexp_string needle) hay 0); true with Not_found -> false

let () =
  let dir = Filename.temp_dir "agent_extension_basic_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  (* --- task tool registered --- *)
  check "task tool present" (Tools.find "task" <> None);
  check "Pi tool aliases resolve" (Tools.find "bash" <> None && Tools.canonical_name "ls" = "list_dir");
  let readonly_schema = Yojson.Safe.to_string (`List (Tools.openai_schemas ~allowed:[ "read"; "grep"; "ls" ] ())) in
  check "tool schemas honor Pi allowlist aliases"
    (contains0 readonly_schema "\"name\":\"read\"" && contains0 readonly_schema "\"name\":\"ls\""
     && not (contains0 readonly_schema "\"name\":\"write\"") && not (contains0 readonly_schema "read_file"));
  let builtin_schema = Yojson.Safe.to_string (`List (Tools.openai_schemas ())) in
  check "builtin schemas expose Pi wire names"
    (contains0 builtin_schema "\"name\":\"read\"" && contains0 builtin_schema "\"name\":\"write\""
     && contains0 builtin_schema "\"name\":\"edit\"" && contains0 builtin_schema "\"name\":\"bash\""
     && not (contains0 builtin_schema "\"name\":\"read_file\"")
     && not (contains0 builtin_schema "\"name\":\"run_bash\""));

  (* --- extensions: custom tool from manifest --- *)
  Tools.write_file_contents ".ocaml-agent/tools.json"
    {|{"tools":[{"name":"echoizer","description":"echo back","parameters":{"type":"object","properties":{}},"command":"cat"}]}|};
  let names = Extensions.load () in
  check "extension registered" (List.mem "echoizer" names);
  check "extension findable" (Tools.find "echoizer" <> None);
  (match Tools.find "echoizer" with
   | Some t -> check "extension requires approval" t.Tools.requires_approval
   | None -> check "extension requires approval" false);
  (match Tools.find "echoizer" with
   | Some t -> check "extension executes via subprocess" (contains0 (t.execute (j {|{"x":1}|})) "\"x\":1")
   | None -> check "extension executes via subprocess" false);
  check "extension in schemas"
    (contains0 (Yojson.Safe.to_string (`List (Tools.openai_schemas ()))) "echoizer");
  Tools.write_file_contents ".ocaml-agent/tools.json"
    {|{"tools":[{"name":"run_bash","description":"override","parameters":{"type":"object","properties":{}},"command":"cat"}]}|};
  let names = Extensions.load () in
  check "extension can override builtin canonical name" (List.mem "run_bash" names);
  (match Tools.find "run_bash" with
   | Some t ->
     check "builtin canonical override executes extension"
       (t.Tools.description = "override" && contains0 (t.Tools.execute (`Assoc [ ("x", `Int 1) ])) "\"x\":1")
   | None -> check "builtin canonical override executes extension" false);
  Tools.write_file_contents ".ocaml-agent/tools.json"
    {|{"tools":[{"name":"bash","description":"override alias","parameters":{"type":"object","properties":{}},"command":"cat"}]}|};
  let names = Extensions.load () in
  check "extension can override Pi alias builtin" (List.mem "bash" names);
  (match Tools.find "bash" with
   | Some t ->
     check "builtin alias override executes extension"
       (t.Tools.description = "override alias" && contains0 (t.Tools.execute (`Assoc [ ("command", `String "echo hi") ])) "echo hi")
   | None -> check "builtin alias override executes extension" false);
  let _ =
    run "write_file"
      {|{"path":"extra-tools.json","content":"{\"tools\":[{\"name\":\"from_explicit_manifest\",\"description\":\"explicit\",\"parameters\":{\"type\":\"object\",\"properties\":{}},\"command\":\"cat\"}]}"}|}
  in
  Unix.putenv "AGENT_NO_EXTENSIONS" "1";
  Unix.putenv "AGENT_EXTENSION_PATHS" "extra-tools.json";
  let names = Extensions.load () in
  check "explicit extension loads when defaults disabled" (List.mem "from_explicit_manifest" names);
  check "extension reload restores unloaded builtin overrides"
    (match Tools.find "bash" with
     | Some t -> t.Tools.description <> "override alias"
     | None -> false);
  let ext_schema = Yojson.Safe.to_string (`List (Tools.openai_schemas ~allowed:(Tools.extension_names ()) ())) in
  check "extension-only schema excludes builtins"
    (contains0 ext_schema "from_explicit_manifest" && not (contains0 ext_schema "\"name\":\"read\""));
  Unix.putenv "AGENT_NO_EXTENSIONS" "";
  Unix.putenv "AGENT_EXTENSION_PATHS" "";
  let _ =
    run "write_file"
      (Printf.sprintf
         {|{"path":"%s","content":"{\"tools\":[{\"name\":\"from_pi_agent_dir\",\"description\":\"global\",\"parameters\":{\"type\":\"object\",\"properties\":{}},\"command\":\"cat\"}]}"}|}
         (Filename.concat (Sys.getenv "PI_CODING_AGENT_DIR") "tools.json"))
  in
  let names = Extensions.load () in
  check "global Pi extension manifest loads" (List.mem "from_pi_agent_dir" names);
  let sdk_exe = Filename.concat (Filename.dirname Sys.executable_name) "ocaml_sdk_extension.exe" in
  let sdk_descriptor =
    Yojson.Safe.to_string
      (`Assoc
        [ ("runtime", `String "ocaml");
          ("command", `String (Filename.quote sdk_exe)) ])
  in
  let _ =
    run "write_file"
      (Yojson.Safe.to_string
         (`Assoc [ ("path", `String ".pi/extensions/ocaml-sdk.ocamlext"); ("content", `String sdk_descriptor) ]))
  in
  let names = Extensions.load () in
  check "OCaml SDK extension registers tool" (List.mem "ocaml_greet" names);
  check "OCaml SDK extension executes registered tool"
    (match Tools.find "ocaml_greet" with
     | Some t -> t.Tools.execute (`Assoc [ ("name", `String "SDK") ]) = "Hello SDK"
     | None -> false);
  check "OCaml SDK extension command appears in completion"
    (List.mem_assoc "/ocamlhello" (Complete.menu "/ocaml"));
  check "OCaml SDK extension command argument completions feed Tab candidates"
    (let start, cands = Complete.completion "/ocamlhello s" in
     start = String.length "/ocamlhello " && cands = [ "sdk" ]);
  check "OCaml SDK extension command executes"
    (match Extensions.execute_command "/ocamlhello Pi" with
     | Some output -> output = "OCaml command Pi"
     | None -> false);
  check "OCaml SDK extension command returns UI surfaces"
    (match Extensions.execute_command_response "/ocamlui" with
     | Some response ->
       response.Extensions.text = "ui ok"
       && List.mem "ocaml notice" response.Extensions.ui.notifications
       &&
       (match response.Extensions.ui.surfaces with
        | `Assoc fields :: _ ->
          List.assoc_opt "kind" fields = Some (`String "status")
          && List.assoc_opt "key" fields = Some (`String "ocaml")
        | _ -> false)
     | None -> false);
  check "OCaml SDK extension before_provider_request mutates payload"
    (let mutated = Llm.apply_provider_request_hooks (j {|{"model":"base"}|}) in
     Yojson.Safe.Util.member "ocamlHooked" mutated = `Bool true);
  check "OCaml SDK extension after_provider_response receives payload"
    (Llm.emit_provider_response_hooks ~status:299 ~headers:[ ("x-ocaml", "ok") ] ();
     contains0 (Tools.read_file_contents "ocaml-hooks.log") "after 299");
  check "OCaml SDK extension registers provider runtime"
    (let cfg = Llm.config_for "ocamlai" in
     let blocks, usage =
       Llm.complete cfg ~system:"SDK-SYSTEM" ~tools_enabled:false
         [ { Llm.role = Llm.User; content = [ Llm.Text "hello" ] } ]
     in
     match blocks with
     | [ Llm.Text text ] ->
       text = "ocaml provider ocaml-small:true:1"
       && usage.Llm.input_tokens = 5
       && usage.Llm.output_tokens = 2
     | _ -> false);
  Printf.printf "\n%s\n" (if !failures = 0 then "All extension basic tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
