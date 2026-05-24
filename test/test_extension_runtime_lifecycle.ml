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
  pi.on("session_before_fork", async (event) => {
    fs.appendFileSync("runtime-events.log", `beforefork:${event.position}:${event.entryId || ""}\n`);
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
  pi.registerCommand("runtimeforkparity", {
    description: "Probe AgentSessionRuntime fork replacement behavior",
    handler: async () => {
      const agentDir = path.resolve("runtime-fork-agent");
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
      await runtimeResult.runtime.newSession({
        setup: async (manager) => {
          manager.appendMessage({ role: "user", content: [{ type: "text", text: "fork text" }] });
          manager.appendMessage({ role: "assistant", content: [{ type: "text", text: "fork answer" }] });
        },
      });
      const beforeFile = runtimeResult.runtime.session.sessionFile || "";
      const userEntry = runtimeResult.runtime.session.sessionManager.getEntries().find((entry) => entry.type === "message" && entry.message.role === "user");
      const forkResult = await runtimeResult.runtime.fork(userEntry.id, {
        position: "before",
        withSession: async (next) => {
          await next.sendMessage({ customType: "fork-note", content: "body", display: true }, { triggerTurn: false });
        },
      });
      const afterFile = runtimeResult.runtime.session.sessionFile || "";
      const stats = runtimeResult.runtime.session.getSessionStats();
      const header = runtimeResult.runtime.session.sessionManager.getHeader();
      await runtimeResult.runtime.dispose();
      const log = fs.existsSync("runtime-events.log") ? fs.readFileSync("runtime-events.log", "utf8") : "";
      return [
        forkResult.cancelled === false,
        forkResult.selectedText,
        beforeFile !== afterFile,
        fs.existsSync(afterFile),
        path.resolve(header.parentSession || "") === path.resolve(beforeFile),
        stats.totalMessages,
        stats.userMessages,
        rebindCount >= 2,
        invalidated >= 2,
        log.includes("beforefork:before:"),
        log.includes("shutdown:fork:"),
        log.includes("start:fork:"),
      ].join(":");
    },
  });
}
|};
  write_file ".pi/extensions/session-manager-sdk.ts"
    {|const fs = require("node:fs");
const path = require("node:path");
const { SessionManager } = require("@earendil-works/pi-coding-agent");

export default function(pi) {
  pi.registerCommand("sessionmanagerparity", {
    description: "Probe SessionManager static constructors and persistence",
    handler: async () => {
      const dir = path.resolve("sdk-static-sessions");
      const forkDir = path.resolve("sdk-fork-sessions");
      const forkCwd = path.resolve("fork-cwd");
      fs.mkdirSync(forkCwd, { recursive: true });

      const created = SessionManager.create(process.cwd(), dir);
      const createdFile = created.getSessionFile();
      const root = created.appendMessage({ role: "user", content: [{ type: "text", text: "static hello" }] });
      const label = created.appendLabelChange(root, "root-label");
      const sessionInfo = created.appendSessionInfo("static session");

      const opened = SessionManager.open(createdFile, dir);
      const continued = SessionManager.continueRecent(process.cwd(), dir);
      const memory = SessionManager.inMemory(process.cwd());
      const memoryRoot = memory.appendMessage({ role: "user", content: "memory hello" });
      const memoryAssistant = memory.appendMessage({ role: "assistant", content: "memory answer" });
      memory.branch(memoryRoot);
      const memoryBranch = memory.appendMessage({ role: "user", content: "memory branch" });
      const summary = memory.branchWithSummary(memoryRoot, "memory summary", { ok: true }, true);
      memory.resetLeaf();
      const resetRoot = memory.appendCustomEntry("reset", { ok: true });
      const switched = SessionManager.inMemory(process.cwd());
      switched.setSessionFile(createdFile);
      const forked = SessionManager.forkFrom(createdFile, forkCwd, forkDir);
      const listed = await SessionManager.list(process.cwd(), dir);
      const returnedIds = [root, label, sessionInfo, memoryRoot, memoryAssistant, memoryBranch, summary, resetRoot]
        .every((id) => typeof id === "string" && id.length > 0);

      return [
        created instanceof SessionManager,
        opened instanceof SessionManager,
        continued instanceof SessionManager,
        memory instanceof SessionManager,
        forked instanceof SessionManager,
        fs.existsSync(createdFile),
        path.basename(createdFile).endsWith(".jsonl"),
        opened.buildSessionContext().messages.length,
        opened.getLabel(root),
        continued.getSessionId() === created.getSessionId(),
        memory.getSessionFile() === undefined,
        memory.getEntries().length,
        fs.existsSync(forked.getSessionFile()),
        forked.getCwd() === forkCwd,
        path.resolve(forked.getHeader().parentSession || "") === path.resolve(createdFile),
        forked.getEntries().length === opened.getEntries().length,
        listed.some((session) => session.id === created.getSessionId() && session.messageCount === 1),
        returnedIds,
        opened.getSessionName() === "static session",
        opened.getEntry(root).id === root,
        memory.getLeafId() === resetRoot,
        memory.getBranch(summary).some((entry) => entry.type === "branch_summary" && entry.summary === "memory summary"),
        switched.getSessionId() === created.getSessionId(),
        switched.isPersisted() === false,
        typeof created._persist === "function",
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
  check "TypeScript AgentSessionRuntime fork replaces session like Pi"
    ((not node_available)
     ||
     match Extensions.execute_command "/runtimeforkparity" with
     | Some output ->
       let parts = String.split_on_char ':' output in
       List.length parts = 12
       && List.nth parts 1 = "fork text"
       && List.nth parts 5 = "1"
       && List.nth parts 6 = "0"
       && List.for_all
            (fun index -> List.nth parts index = "true")
            [ 0; 2; 3; 4; 7; 8; 9; 10; 11 ]
     | None -> false);
  check "TypeScript SessionManager static constructors match Pi SDK behavior"
    ((not node_available)
     ||
     match Extensions.execute_command "/sessionmanagerparity" with
     | Some output ->
       let parts = String.split_on_char ':' output in
       List.length parts = 25
       && List.nth parts 7 = "1"
       && List.nth parts 8 = "root-label"
       && List.nth parts 11 = "5"
       && List.for_all
            (fun index -> List.nth parts index = "true")
            [ 0; 1; 2; 3; 4; 5; 6; 9; 10; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24 ]
     | None -> false);

  if !failures > 0 then exit 1
