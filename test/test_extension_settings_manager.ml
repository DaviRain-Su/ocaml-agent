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
  let dir = Filename.temp_dir "agent_extension_settings_manager_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  let node_available = Sys.command "command -v node >/dev/null 2>&1" = 0 in
  write_file ".pi/extensions/settings-manager.ts"
    {|const fs = require("node:fs");
const path = require("node:path");
const { SettingsManager } = require("@earendil-works/pi-coding-agent");

export default function(pi) {
  pi.registerCommand("settingsmanagerparity", {
    description: "Probe SettingsManager SDK settings parity",
    handler: async () => {
      const cwd = path.resolve("settings-cwd");
      const agentDir = path.resolve("settings-agent");
      fs.mkdirSync(path.join(cwd, ".pi"), { recursive: true });
      fs.mkdirSync(agentDir, { recursive: true });

      const defaults = SettingsManager.inMemory();
      process.env.PI_CLEAR_ON_SHRINK = "1";
      process.env.PI_HARDWARE_CURSOR = "1";
      const envDefaults = SettingsManager.inMemory();
      const configured = SettingsManager.inMemory({
        thinkingBudgets: { high: 123 },
        markdown: { codeBlockIndent: "    " },
        warnings: { destructive: true },
        treeFilterMode: "bad",
        terminal: { imageWidthCells: 0 },
      });

      const settings = SettingsManager.create(cwd, agentDir);
      settings.setShellPath("/bin/zsh");
      settings.setShellCommandPrefix("prefix");
      settings.setQuietStartup(true);
      settings.setCollapseChangelog(true);
      settings.setEnableInstallTelemetry(false);
      settings.setEnableSkillCommands(false);
      settings.setShowImages(false);
      settings.setImageWidthCells(0);
      settings.setClearOnShrink(true);
      settings.setShowTerminalProgress(true);
      settings.setImageAutoResize(false);
      settings.setBlockImages(true);
      settings.setEnabledModels(["anthropic/*", "openai/gpt-*"]);
      settings.setDoubleEscapeAction("fork");
      settings.setTreeFilterMode("user-only");
      settings.setShowHardwareCursor(true);
      settings.setEditorPaddingX(7);
      settings.setAutocompleteMaxVisible(2);
      settings.setWarnings({ destructive: false, custom: true });
      settings.setNpmCommand(["npm", "--silent"]);

      const reloaded = SettingsManager.create(cwd, agentDir);
      return [
        defaults.getQuietStartup() === false,
        defaults.getEnableInstallTelemetry() === true,
        defaults.getEnableSkillCommands() === true,
        defaults.getShowImages() === true,
        defaults.getImageWidthCells() === 60,
        defaults.getShowTerminalProgress() === false,
        defaults.getImageAutoResize() === true,
        defaults.getBlockImages() === false,
        defaults.getDoubleEscapeAction() === "tree",
        defaults.getTreeFilterMode() === "default",
        defaults.getEditorPaddingX() === 0,
        defaults.getAutocompleteMaxVisible() === 5,
        envDefaults.getClearOnShrink() === true,
        envDefaults.getShowHardwareCursor() === true,
        configured.getThinkingBudgets().high === 123,
        configured.getCodeBlockIndent() === "    ",
        configured.getWarnings().destructive === true,
        configured.getTreeFilterMode() === "default",
        configured.getImageWidthCells() === 1,
        reloaded.getShellPath() === "/bin/zsh",
        reloaded.getShellCommandPrefix() === "prefix",
        reloaded.getQuietStartup() === true,
        reloaded.getCollapseChangelog() === true,
        reloaded.getEnableInstallTelemetry() === false,
        reloaded.getEnableSkillCommands() === false,
        reloaded.getShowImages() === false,
        reloaded.getImageWidthCells() === 1,
        reloaded.getClearOnShrink() === true,
        reloaded.getShowTerminalProgress() === true,
        reloaded.getImageAutoResize() === false,
        reloaded.getBlockImages() === true,
        reloaded.getEnabledModels().join(",") === "anthropic/*,openai/gpt-*",
        reloaded.getDoubleEscapeAction() === "fork",
        reloaded.getTreeFilterMode() === "user-only",
        reloaded.getShowHardwareCursor() === true,
        reloaded.getEditorPaddingX() === 3,
        reloaded.getAutocompleteMaxVisible() === 3,
        reloaded.getWarnings().custom === true,
        reloaded.getNpmCommand().join(" ") === "npm --silent",
        reloaded.getGlobalSettings().npmCommand.join(" ") === "npm --silent",
      ].join("|");
    },
  });
}
|};
  ignore (Extensions.load ());
  check "TypeScript SettingsManager exposes Pi SDK settings APIs"
    ((not node_available)
     ||
     match Extensions.execute_command "/settingsmanagerparity" with
     | Some output ->
       let parts = String.split_on_char '|' output in
       List.length parts = 40 && List.for_all (( = ) "true") parts
     | None -> false);

  if !failures > 0 then exit 1
