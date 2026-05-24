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

let () =
  let dir = Filename.temp_dir "agent_extension_runtime_lifecycle_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  let node_available = Sys.command "command -v node >/dev/null 2>&1" = 0 in
  write_file ".pi/extensions/runtime-lifecycle.ts"
    {|const fs = require("node:fs");
const path = require("node:path");
const { AgentSessionRuntime, createAgentSession, createAgentSessionRuntime } = require("@earendil-works/pi-coding-agent");

export default function(pi) {
  pi.on("session_before_switch", async (event) => {
    fs.appendFileSync("runtime-events.log", `before:${event.reason}:${path.basename(event.targetSessionFile || "")}\n`);
  });
  pi.on("session_shutdown", async (event) => {
    fs.appendFileSync("runtime-events.log", `shutdown:${event.reason}:${path.basename(event.targetSessionFile || "")}\n`);
  });
  pi.on("session_start", async (event) => {
    fs.appendFileSync("runtime-events.log", `start:${event.reason}:${path.basename(event.previousSessionFile || "")}\n`);
  });
  pi.registerCommand("runtimeparity", {
    description: "Probe AgentSessionRuntime import/switch/dispose behavior",
    handler: async () => {
      const agentDir = path.resolve("runtime-agent");
      const created = await createAgentSession({ cwd: process.cwd(), agentDir });
      await created.session.prompt("imported message");
      const jsonPath = created.session.exportToJsonl(path.resolve("source-session.jsonl"));
      const runtimeResult = await createAgentSessionRuntime({ cwd: process.cwd(), agentDir });
      let rebindCount = 0;
      let invalidated = 0;
      runtimeResult.runtime.setRebindSession(async (session) => {
        rebindCount++;
        await session.bindExtensions();
      });
      runtimeResult.runtime.setBeforeSessionInvalidate(() => {
        invalidated++;
      });
      const importResult = await runtimeResult.runtime.importFromJsonl(jsonPath, process.cwd());
      const importStats = runtimeResult.runtime.session.getSessionStats();
      const importedFile = runtimeResult.runtime.session.sessionFile || "";
      const copiedExists = fs.existsSync(path.join(runtimeResult.runtime.session.sessionManager.getSessionDir(), path.basename(jsonPath)));
      const switchResult = await runtimeResult.runtime.switchSession(importedFile, { cwdOverride: process.cwd() });
      const switchStats = runtimeResult.runtime.session.getSessionStats();
      await runtimeResult.runtime.dispose();
      const log = fs.existsSync("runtime-events.log") ? fs.readFileSync("runtime-events.log", "utf8") : "";
      return [
        runtimeResult.runtime instanceof AgentSessionRuntime,
        typeof runtimeResult.runtime.importFromJsonl,
        typeof runtimeResult.runtime.switchSession,
        typeof runtimeResult.runtime.dispose,
        importResult.cancelled === false,
        importStats.totalMessages,
        importStats.userMessages,
        path.basename(jsonPath),
        path.basename(importedFile),
        copiedExists,
        switchResult.cancelled === false,
        switchStats.totalMessages,
        runtimeResult.runtime.session.disposed,
        rebindCount >= 2,
        invalidated >= 3,
        log.includes("before:resume:source-session.jsonl"),
        log.includes("shutdown:resume:source-session.jsonl"),
        log.includes("start:resume:"),
        log.includes("shutdown:quit:"),
      ].join(":");
    },
  });
}
|};
  ignore (Extensions.load ());
  check "TypeScript AgentSessionRuntime imports, switches, and disposes sessions"
    ((not node_available)
     ||
     match Extensions.execute_command "/runtimeparity" with
     | Some output ->
       let parts = String.split_on_char ':' output in
       List.length parts = 19
       && List.nth parts 1 = "function"
       && List.nth parts 2 = "function"
       && List.nth parts 3 = "function"
       && List.nth parts 5 = "1"
       && List.nth parts 6 = "1"
       && List.nth parts 7 = "source-session.jsonl"
       && List.nth parts 8 = "source-session.jsonl"
       && List.nth parts 11 = "1"
       && List.for_all
            (fun index -> List.nth parts index = "true")
            [ 0; 4; 9; 10; 12; 13; 14; 15; 16; 17; 18 ]
     | None -> false);

  if !failures > 0 then exit 1
