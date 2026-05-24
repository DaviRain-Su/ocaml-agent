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

let write_file path content =
  ignore
    (run "write_file"
       (Yojson.Safe.to_string
          (`Assoc [ ("path", `String path); ("content", `String content) ])))

let contains hay needle =
  let hay_len = String.length hay and needle_len = String.length needle in
  let rec loop i =
    i + needle_len <= hay_len
    && (String.sub hay i needle_len = needle || loop (i + 1))
  in
  needle_len = 0 || loop 0

let () =
  let dir = Filename.temp_dir "agent_extension_tree_summary_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  let node_available = Sys.command "command -v node >/dev/null 2>&1" = 0 in
  write_file ".pi/extensions/runtime-provider.ts"
    {|const fs = require("node:fs");
export default function(pi) {
  pi.registerProvider({
    name: "runtimeai",
    aliases: ["runtime"],
    defaultModel: "runtime-small",
    models: [{ id: "runtime-small", contextWindow: 9001 }],
    complete: async (request) => {
      fs.appendFileSync("runtime-request.log", JSON.stringify(request.messages) + "\n");
      return {
        content: [{ type: "text", text: `runtime ${request.model}:${request.system}:${request.messages.length}:${request.toolsEnabled}` }],
        usage: { inputTokens: 12, outputTokens: 3 },
      };
    },
  });
}
|};
  write_file ".pi/extensions/session-hooks.ts"
    {|const fs = require("node:fs");
export default function(pi) {
  pi.on("session_before_tree", async (event) => {
    const prep = event.preparation || {};
    fs.appendFileSync("lifecycle.log", `session_before_tree_default ${prep.targetId || ""} ${prep.userWantsSummary} ${prep.customInstructions || ""} ${prep.replaceInstructions}\n`);
    return { label: "default-summary-label", customInstructions: "Hook focus", replaceInstructions: false };
  });
  pi.on("session_tree", async (event) => {
    fs.appendFileSync("lifecycle.log", `session_tree_default ${event.newLeafId || ""} ${event.summaryEntry ? event.summaryEntry.type : ""} ${event.fromExtension}\n`);
  });
}
|};
  ignore (Extensions.load ());
  check "rpc Pi navigateTree uses default branch summarizer when hook omits summary"
    ((not node_available)
     ||
     let turns =
       [ { Llm.role = Llm.User; content = [ Llm.Text "one" ] };
         { Llm.role = Llm.Assistant; content = [ Llm.Text "two" ] } ]
     in
     let session = Session.create_new ~name:"nav-default-summary" () in
     List.iter (Session.append session) turns;
     let agent = Agent.create ~session ~initial_turns:turns (Llm.config_for "runtime") in
     let results =
       Commands.apply_extension_session_actions agent
         [ j
             {|{"kind":"navigate_tree","targetId":"turn-0","options":{"summarize":true,"label":"default-summary","customInstructions":"Original focus","replaceInstructions":true}}|} ]
     in
     let summary_entry =
       session.Session.entries
       |> List.find_map (function
              | `Assoc fields when List.assoc_opt "type" fields = Some (`String "branch_summary") -> Some fields
              | _ -> None)
     in
     let summary_id =
       match summary_entry with
       | Some fields -> (
         match List.assoc_opt "id" fields with
         | Some (`String id) -> Some id
         | _ -> None)
       | None -> None
     in
     let has_summary =
       match summary_entry with
       | Some fields ->
         List.assoc_opt "parentId" fields = Some `Null
         && List.assoc_opt "fromId" fields = Some (`String "root")
         && List.assoc_opt "fromHook" fields = Some (`Bool false)
         &&
         (match List.assoc_opt "summary" fields with
          | Some (`String summary) -> contains summary "runtime runtime-small:"
          | _ -> false)
       | None -> false
     in
     let has_label =
       match summary_id with
       | Some id ->
         List.exists
           (function
             | `Assoc fields ->
               List.assoc_opt "type" fields = Some (`String "label")
               && List.assoc_opt "targetId" fields = Some (`String id)
               && List.assoc_opt "label" fields = Some (`String "default-summary-label")
             | _ -> false)
           session.Session.entries
       | None -> false
     in
     let log = Tools.read_file_contents "runtime-request.log" in
     let lifecycle = Tools.read_file_contents "lifecycle.log" in
     let result_ok =
       match results with
       | [ `Assoc fields ] ->
         List.assoc_opt "kind" fields = Some (`String "navigate_tree")
         && List.assoc_opt "cancelled" fields = Some (`Bool false)
       | _ -> false
     in
     let ok =
       result_ok && has_summary && has_label && Agent.turn_count agent = 0
       && contains log "Hook focus" && not (contains log "Original focus")
       && contains lifecycle "session_tree_default"
       && contains lifecycle "branch_summary false"
     in
     Option.iter Session.close (Agent.session agent);
     ok);

  if !failures > 0 then exit 1
