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
  let dir = Filename.temp_dir "agent_extension_package_manager_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  let node_available = Sys.command "command -v node >/dev/null 2>&1" = 0 in
  write_file ".pi/extensions/package-manager.ts"
    {|const fs = require("node:fs");
const path = require("node:path");
const { DefaultPackageManager, SettingsManager } = require("@earendil-works/pi-coding-agent");

function packageName(spec) {
  if (spec.startsWith("@")) {
    const index = spec.indexOf("@", 1);
    return index > 0 ? spec.slice(0, index) : spec;
  }
  const index = spec.indexOf("@");
  return index > 0 ? spec.slice(0, index) : spec;
}

export default function(pi) {
  pi.registerCommand("packagemanagerparity", {
    description: "Probe DefaultPackageManager managed npm install/remove behavior",
    handler: async () => {
      const agentDir = path.resolve("pkg-agent");
      const fakeNpm = path.resolve("fake-npm.js");
      fs.writeFileSync(fakeNpm, `
const fs = require("node:fs");
const path = require("node:path");
const args = process.argv.slice(2);
fs.appendFileSync("npm.log", args.join(" ") + "\\n");
const prefixIndex = args.indexOf("--prefix");
const cwdIndex = args.indexOf("--cwd");
const root = prefixIndex >= 0 ? args[prefixIndex + 1] : (cwdIndex >= 0 ? args[cwdIndex + 1] : process.cwd());
function packageName(spec) {
  if (spec.startsWith("@")) {
    const index = spec.indexOf("@", 1);
    return index > 0 ? spec.slice(0, index) : spec;
  }
  const index = spec.indexOf("@");
  return index > 0 ? spec.slice(0, index) : spec;
}
if (args[0] === "install") {
  const spec = args[1];
  const name = packageName(spec);
  const target = path.join(root, "node_modules", ...name.split("/"));
  fs.mkdirSync(target, { recursive: true });
  fs.mkdirSync(path.join(target, "extensions"), { recursive: true });
  fs.writeFileSync(path.join(target, "extensions", "main.ts"), "export default function(pi) {}\\n", "utf8");
  fs.writeFileSync(path.join(target, "package.json"), JSON.stringify({ name, version: "1.0.0", pi: { extensions: ["extensions/main.ts"] } }), "utf8");
}
if (args[0] === "uninstall") {
  const name = args[1];
  fs.rmSync(path.join(root, "node_modules", ...name.split("/")), { recursive: true, force: true });
}
if (args[0] === "view") {
  const name = args[1];
  process.stdout.write(JSON.stringify(name === "@scope/updatable" ? "2.0.0" : "1.0.0"));
}
`, "utf8");
      const settings = SettingsManager.inMemory({ npmCommand: [process.execPath, fakeNpm] });
      const pm = new DefaultPackageManager({ cwd: process.cwd(), agentDir, settingsManager: settings });
      const progress = [];
      pm.setProgressCallback((event) => progress.push(`${event.action}:${event.type}`));
      const missingPathUndefined = pm.getInstalledPath("npm:@scope/missing", "user") === undefined;

      const filterPkg = path.resolve("filter-pkg");
      fs.mkdirSync(path.join(filterPkg, "extensions"), { recursive: true });
      fs.mkdirSync(path.join(filterPkg, "skills", "keep-skill"), { recursive: true });
      fs.mkdirSync(path.join(filterPkg, "skills", "drop-skill"), { recursive: true });
      fs.writeFileSync(path.join(filterPkg, "extensions", "foo.ts"), "export default function(pi) {}\n", "utf8");
      fs.writeFileSync(path.join(filterPkg, "extensions", "bar.ts"), "export default function(pi) {}\n", "utf8");
      fs.writeFileSync(path.join(filterPkg, "extensions", "baz.ts"), "export default function(pi) {}\n", "utf8");
      fs.writeFileSync(path.join(filterPkg, "skills", "keep-skill", "SKILL.md"), "---\nname: keep-skill\ndescription: keep\n---\n", "utf8");
      fs.writeFileSync(path.join(filterPkg, "skills", "drop-skill", "SKILL.md"), "---\nname: drop-skill\ndescription: drop\n---\n", "utf8");
      fs.writeFileSync(path.join(filterPkg, "package.json"), JSON.stringify({
        name: "filter-pkg",
        pi: { extensions: ["extensions", "!**/baz.ts"] },
      }), "utf8");
      const filterSettings = SettingsManager.inMemory();
      filterSettings.setPackages([{
        source: filterPkg,
        extensions: ["!**/bar.ts"],
        skills: ["!**/*", "+skills/keep-skill"],
        prompts: [],
      }]);
      const filtered = await new DefaultPackageManager({ cwd: process.cwd(), agentDir, settingsManager: filterSettings }).resolve();
      const fooEnabled = filtered.extensions.some((entry) => entry.path.endsWith(path.join("extensions", "foo.ts")) && entry.enabled);
      const barDisabled = filtered.extensions.some((entry) => entry.path.endsWith(path.join("extensions", "bar.ts")) && !entry.enabled);
      const bazAbsent = !filtered.extensions.some((entry) => entry.path.endsWith(path.join("extensions", "baz.ts")));
      const keepSkillEnabled = filtered.skills.some((entry) => entry.path.includes("keep-skill") && entry.enabled);
      const dropSkillDisabled = filtered.skills.some((entry) => entry.path.includes("drop-skill") && !entry.enabled);
      const promptsDisabled = filtered.prompts.length === 0;

      const dedupeSettings = SettingsManager.inMemory();
      dedupeSettings.setPackages([filterPkg]);
      dedupeSettings.setProjectPackages([filterPkg]);
      const deduped = await new DefaultPackageManager({ cwd: process.cwd(), agentDir, settingsManager: dedupeSettings }).resolve();
      const filterExts = deduped.extensions.filter((entry) => entry.path.includes("filter-pkg"));
      const projectWins = filterExts.length === 2 && filterExts.every((entry) => entry.metadata.scope === "project");

      const tempResolved = await pm.resolveExtensionSources(["npm:@scope/temp@2.0.0"], { temporary: true });
      const tempExtension = tempResolved.extensions.some((entry) =>
        entry.path.endsWith(path.join("node_modules", "@scope", "temp", "extensions", "main.ts")) &&
        entry.metadata.scope === "temporary"
      );
      await pm.installAndPersist("npm:@scope/pkg@1.2.3");
      const installedPath = pm.getInstalledPath("npm:@scope/pkg", "user");
      const installedExists = fs.existsSync(installedPath);
      const packageJsonExists = fs.existsSync(path.join(installedPath, "package.json"));
      const configured = pm.listConfiguredPackages();
      const removed = await pm.removeAndPersist("npm:@scope/pkg");
      const log = fs.readFileSync("npm.log", "utf8");
      const configuredAfterRemove = pm.listConfiguredPackages().length;
      await pm.installAndPersist("npm:@scope/updatable");
      const updates = await pm.checkForAvailableUpdates();
      const npmUpdateDetected = updates.some((entry) =>
        entry.source === "npm:@scope/updatable" &&
        entry.displayName === "@scope/updatable" &&
        entry.type === "npm" &&
        entry.scope === "user"
      );
      return [
        typeof DefaultPackageManager,
        installedExists,
        packageJsonExists,
        configured.length,
        configured[0] && configured[0].source,
        configured[0] && configured[0].installedPath === installedPath,
        removed,
        !fs.existsSync(installedPath),
        configuredAfterRemove,
        log.includes("install @scope/pkg@1.2.3"),
        log.includes("uninstall @scope/pkg"),
        progress.includes("install:start"),
        progress.includes("install:complete"),
        progress.includes("remove:complete"),
        tempExtension,
        fooEnabled,
        barDisabled,
        bazAbsent,
        keepSkillEnabled,
        dropSkillDisabled,
        promptsDisabled,
        projectWins,
        missingPathUndefined,
        npmUpdateDetected,
      ].join("|");
    },
  });
}
|};
  ignore (Extensions.load ());
  check "TypeScript DefaultPackageManager installs and removes managed npm packages"
    ((not node_available)
     ||
     match Extensions.execute_command "/packagemanagerparity" with
     | Some output ->
       let parts = String.split_on_char '|' output in
       let passed =
         List.length parts = 24
         && List.nth parts 0 = "function"
         && List.nth parts 3 = "1"
         && List.nth parts 4 = "npm:@scope/pkg@1.2.3"
         && List.nth parts 8 = "0"
         && List.for_all
              (fun index -> List.nth parts index = "true")
              [ 1; 2; 5; 6; 7; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23 ]
       in
       if not passed then Printf.printf "package-manager output: %s\n" output;
       passed
     | None -> false);

  if !failures > 0 then exit 1
