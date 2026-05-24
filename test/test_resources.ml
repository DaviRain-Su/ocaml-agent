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

let contains = contains0

let () =
  let dir = Filename.temp_dir "agent_resource_test" "" in
  Sys.chdir dir;
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  Unix.putenv "PI_CODING_AGENT_DIR" (Filename.concat dir "pi-agent");
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" (Filename.concat dir "pi-agent/sessions");

  (* --- skills --- *)
  let _ =
    run "write_file"
      {|{"path":".ocaml-agent/skills/deploy.md","content":"---\nname: deploy\ndescription: How to deploy the app\n---\nDetailed steps."}|}
  in
  let _ =
    run "write_file"
      (Printf.sprintf
         {|{"path":"%s","content":"---\nname: global_skill\ndescription: Global Pi skill\n---\nGlobal details."}|}
         (Filename.concat (Filename.concat (Sys.getenv "PI_CODING_AGENT_DIR") "skills") "global.md"))
  in
  let _ =
    run "write_file"
      {|{"path":".ocaml-agent/skills/hidden.md","content":"---\nname: hidden\ndescription: nope\ndisable-model-invocation: true\n---\nx"}|}
  in
  let skills = Skills.discover () in
  check "skills discovered" (List.exists (fun (s : Skills.t) -> s.name = "deploy") skills);
  check "global Pi skills discovered" (List.exists (fun (s : Skills.t) -> s.name = "global_skill") skills);
  check "skills honor disable flag" (not (List.exists (fun (s : Skills.t) -> s.name = "hidden") skills));
  let sf = Skills.format skills in
  check "skills format has name + location"
    (contains0 sf "deploy" && contains0 sf ".ocaml-agent/skills/deploy.md");
  let _ =
    run "write_file"
      {|{"path":"extra_skill.md","content":"---\nname: extra\ndescription: Loaded explicitly\n---\nExtra skill."}|}
  in
  Unix.putenv "AGENT_NO_SKILLS" "1";
  Unix.putenv "AGENT_SKILL_PATHS" "extra_skill.md";
  let only_extra = Skills.discover () in
  check "skill CLI path works when discovery disabled"
    (List.exists (fun (s : Skills.t) -> s.name = "extra") only_extra
     && not (List.exists (fun (s : Skills.t) -> s.name = "deploy") only_extra));
  Unix.putenv "AGENT_NO_SKILLS" "";
  Unix.putenv "AGENT_SKILL_PATHS" "";

  (* --- prompt templates --- *)
  let _ =
    run "write_file"
      {|{"path":".ocaml-agent/prompts/component.md","content":"---\ndescription: Create a component\nargument-hint: <name> [features]\n---\nBuild component $1 with: ${@:2}\nAll: $ARGUMENTS"}|}
  in
  let _ =
    run "write_file"
      (Printf.sprintf {|{"path":"%s","content":"---\ndescription: Global prompt\n---\nGlobal says $1"}|}
         (Filename.concat (Filename.concat (Sys.getenv "PI_CODING_AGENT_DIR") "prompts") "global_prompt.md"))
  in
  let prompts = Prompts.discover () in
  check "prompt templates discovered" (List.exists (fun (p : Prompts.t) -> p.name = "component") prompts);
  check "global Pi prompt templates discovered" (List.exists (fun (p : Prompts.t) -> p.name = "global_prompt") prompts);
  check "prompt templates appear in completion" (List.mem_assoc "/component" (Complete.menu "/co"));
  check "global Pi prompt expands" (Prompts.expand_command "/global_prompt hi" = Some "Global says hi");
  let expanded = Prompts.expand_command {|/component Button "click handler" disabled|} in
  check "prompt template expands positional and rest args"
    (expanded = Some "Build component Button with: click handler disabled\nAll: Button click handler disabled");
  let _ =
    run "write_file"
      {|{"path":"extra_prompt.md","content":"---\ndescription: Extra prompt\n---\nExtra ${@:2:1} / $@"}|}
  in
  Unix.putenv "AGENT_NO_PROMPT_TEMPLATES" "1";
  Unix.putenv "AGENT_PROMPT_TEMPLATE_PATHS" "extra_prompt.md";
  check "prompt template CLI path works when discovery disabled"
    (Prompts.expand_command "/extra_prompt first second third" = Some "Extra second / first second third");
  Unix.putenv "AGENT_NO_PROMPT_TEMPLATES" "";
  Unix.putenv "AGENT_PROMPT_TEMPLATE_PATHS" "";

  (* --- Pi-style settings.json --- *)
  let _ =
    run "write_file"
      {|{"path":"settings_skill.md","content":"---\nname: settings-skill\ndescription: From settings\n---\nSettings skill."}|}
  in
  let _ =
    run "write_file"
      {|{"path":"settings_prompt.md","content":"---\ndescription: From settings\n---\nSettings prompt $1"}|}
  in
  let _ =
    run "write_file"
      {|{"path":"settings-tools.json","content":"{\"tools\":[{\"name\":\"from_settings_manifest\",\"description\":\"settings\",\"parameters\":{\"type\":\"object\",\"properties\":{}},\"command\":\"cat\"}]}"}|}
  in
  let _ =
    run "write_file"
      {|{"path":"settings-theme.json","content":"{\"name\":\"settings-theme\",\"colors\":{\"accent\":\"#abcdef\"}}"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/settings.json","content":"{\"defaultProvider\":\"openai\",\"defaultModel\":\"gpt-4o\",\"defaultThinkingLevel\":\"high\",\"sessionDir\":\"settings-sessions\",\"enabledModels\":[\"zai/*\"],\"skills\":[\"settings_skill.md\"],\"prompts\":[\"settings_prompt.md\"],\"extensions\":[\"settings-tools.json\"],\"themes\":[\"settings-theme.json\"],\"quietStartup\":true,\"shellPath\":\"/bin/sh\",\"shellCommandPrefix\":\"export OCAML_AGENT_FROM_SETTINGS=from-settings\",\"compaction\":{\"enabled\":false}}"}|}
  in
  check "settings skill path loads"
    (List.exists (fun (s : Skills.t) -> s.name = "settings-skill") (Skills.discover ()));
  check "settings prompt path expands"
    (Prompts.expand_command "/settings_prompt hi" = Some "Settings prompt hi");
  check "settings extension path loads" (List.mem "from_settings_manifest" (Extensions.load ()));
  check "settings theme path loads"
    (List.exists (fun (t : Themes.t) -> t.name = "settings-theme") (Themes.discover ()));
  Unix.putenv "AGENT_SCOPED_MODELS" "";
  check "settings enabledModels scopes model picker"
    (List.for_all (fun (e : Models.entry) -> e.provider = "zai") (Models.scoped_from_env ()));
  Unix.putenv "AGENT_PROVIDER" "";
  Unix.putenv "AGENT_MODEL" "";
  Unix.putenv "AGENT_THINKING" "";
  Unix.putenv "AGENT_API_KEY" "sk-test";
  let settings_cfg = Llm.config () in
  check "settings default provider/model/thinking"
    (settings_cfg.Llm.provider = Llm.Openai && settings_cfg.model = "gpt-4o" && settings_cfg.thinking = "high");
  Unix.putenv "AGENT_AUTO_COMPACT" "";
  let settings_agent = Agent.create settings_cfg in
  check "settings compaction flag applies" (not (Agent.auto_compact settings_agent));
  let settings_bash = Agent.run_user_bash ~exclude_from_context:true settings_agent {|printf "$OCAML_AGENT_FROM_SETTINGS"|} in
  check "settings shellCommandPrefix applies to user bash" (contains settings_bash "from-settings");
  let saved_pi_session_dir = Sys.getenv_opt "PI_CODING_AGENT_SESSION_DIR" in
  Unix.putenv "AGENT_SESSION_DIR" "";
  Unix.putenv "PI_CODING_AGENT_SESSION_DIR" "";
  check "settings sessionDir applies" (Session.default_dir () = "settings-sessions");
  (match saved_pi_session_dir with Some s -> Unix.putenv "PI_CODING_AGENT_SESSION_DIR" s | None -> ());
  Unix.putenv "AGENT_API_KEY" "";
  Unix.putenv "AGENT_SCOPED_MODELS" "";

  (* --- Pi package local resource discovery --- *)
  let _ =
    run "write_file"
      {|{"path":"local-pkg/package.json","content":"{\"name\":\"local-pkg\",\"pi\":{\"extensions\":[\"extensions\",\"extensions/ts-extension.ts\"],\"skills\":[\"skills\"],\"prompts\":[\"prompts\"],\"themes\":[\"themes\"]}}"}|}
  in
  let _ = run "write_file" {|{"path":"local-pkg/extensions/ts-extension.ts","content":"export default function(pi) {}"}|} in
  let _ =
    run "write_file"
      {|{"path":"local-pkg/extensions/pkg-tools.json","content":"{\"tools\":[{\"name\":\"from_pkg_manifest\",\"description\":\"package\",\"parameters\":{\"type\":\"object\",\"properties\":{}},\"command\":\"cat\"}]}"}|}
  in
  let _ =
    run "write_file"
      {|{"path":"local-pkg/skills/pkg-skill/SKILL.md","content":"---\nname: pkg-skill\ndescription: Package skill\n---\nPackage skill body."}|}
  in
  let _ =
    run "write_file"
      {|{"path":"local-pkg/skills/pkg-disabled/SKILL.md","content":"---\nname: pkg-disabled\ndescription: Disabled package skill\n---\nDisabled package skill body."}|}
  in
  let _ =
    run "write_file"
      {|{"path":"local-pkg/prompts/pkg_prompt.md","content":"---\ndescription: Package prompt\n---\nPackage prompt $1"}|}
  in
  let _ =
    run "write_file"
      {|{"path":"local-pkg/themes/pkg-theme.json","content":"{\"name\":\"pkg-theme\",\"colors\":{\"accent\":\"#123456\"}}"}|}
  in
  let _ =
    run "write_file"
      {|{"path":".pi/settings.json","content":"{\"packages\":[\"../local-pkg\"]}"}|}
  in
  check "package manifest extension loads" (List.mem "from_pkg_manifest" (Extensions.load ()));
  check "package manifest recursive skill loads"
    (List.exists (fun (s : Skills.t) -> s.name = "pkg-skill") (Skills.discover ()));
  check "package manifest prompt expands" (Prompts.expand_command "/pkg_prompt hi" = Some "Package prompt hi");
  check "package manifest theme loads"
    (List.exists (fun (t : Themes.t) -> t.name = "pkg-theme") (Themes.discover ()));
  let pkg_abs = Unix.realpath "local-pkg" in
  let install_msg = Packages.install_source ~local:true "local-pkg" in
  check "package install persists local project source"
    (contains0 install_msg "Installed local package" && contains0 (Tools.read_file_contents ".pi/settings.json") pkg_abs);
  check "package list shows project package"
    (contains0 (Packages.format_configured_packages ()) "project" && contains0 (Packages.format_configured_packages ()) pkg_abs);
  let disable_msg =
    Packages.set_resource_enabled ~local:true ~source:"local-pkg" ~kind:Packages.Skill
      ~path:"skills/pkg-disabled/SKILL.md" ~enabled:false ()
  in
  let package_resource_config = Packages.format_config_resources () in
  check "package config disables package resources with Pi patterns"
    (contains0 disable_msg "Disabled skills"
     && contains0 (Tools.read_file_contents ".pi/settings.json") "-skills/pkg-disabled/SKILL.md"
     && contains0 package_resource_config "[ ] project"
     && contains0 package_resource_config "pkg-disabled");
  check "package disabled resources are not discovered"
    (List.exists (fun (s : Skills.t) -> s.name = "pkg-skill") (Skills.discover ())
     && not (List.exists (fun (s : Skills.t) -> s.name = "pkg-disabled") (Skills.discover ())));
  let enable_msg =
    Packages.set_resource_enabled ~local:true ~source:"local-pkg" ~kind:Packages.Skill
      ~path:"skills/pkg-disabled/SKILL.md" ~enabled:true ()
  in
  check "package config re-enables package resources"
    (contains0 enable_msg "Enabled skills"
     && contains0 (Tools.read_file_contents ".pi/settings.json") "+skills/pkg-disabled/SKILL.md"
     && List.exists (fun (s : Skills.t) -> s.name = "pkg-disabled") (Skills.discover ()));
  check "package npm source parser handles scopes and pins"
    (Packages.parse_source_kind_for_test "npm:@example/pkg@1.2.3" = "npm:@example/pkg:true");
  check "package git source parser handles ssh shorthand ref"
    (Packages.parse_source_kind_for_test "git:git@github.com:user/repo@main" = "git:github.com/user/repo:true");
  check "package npm install path uses Pi agent npm root"
    (Packages.installed_path_for_test "npm:@example/pkg" Packages.User
     = Filename.concat
         (Filename.concat (Filename.concat (Sys.getenv "PI_CODING_AGENT_DIR") "npm") "node_modules")
         "@example/pkg");
  check "package update missing source reports no match"
    (contains0 (Packages.update_source ~source:"npm:@missing/pkg" ()) "No matching package found");
  let remove_msg = Packages.remove_source ~local:true "local-pkg" in
  check "package remove deletes local project source"
    (contains0 remove_msg "Removed package" && not (contains0 (Tools.read_file_contents ".pi/settings.json") pkg_abs));

  (* --- themes --- *)
  let global_theme_path =
    Filename.concat (Filename.concat (Sys.getenv "PI_CODING_AGENT_DIR") "themes") "collision.json"
  in
  let project_theme_path = ".pi/themes/collision.json" in
  let _ =
    run "write_file"
      (Printf.sprintf
         {|{"path":"%s","content":"{\"name\":\"collision\",\"vars\":{\"accent\":\"#111111\"},\"colors\":{\"accent\":\"accent\",\"selectedBg\":17}}"}|}
         global_theme_path)
  in
  let _ =
    run "write_file"
      {|{"path":".pi/themes/collision.json","content":"{\"name\":\"collision\",\"vars\":{\"accent\":\"#22cc88\"},\"colors\":{\"accent\":\"accent\",\"selectedBg\":18}}"}|}
  in
  let themes = Themes.discover () in
  check "themes discovered"
    (List.exists (fun (t : Themes.t) -> t.name = "dark") themes
     && List.exists (fun (t : Themes.t) -> t.name = "collision") themes);
  check "project theme overrides global by name"
    (match List.find_opt (fun (t : Themes.t) -> t.name = "collision") themes with
     | Some t -> t.location = project_theme_path
     | None -> false);
  check "theme token resolves vars" (Themes.color ~theme:(List.find (fun (t : Themes.t) -> t.name = "collision") themes) "accent" <> None);
  let _ = run "write_file" {|{"path":".pi/settings.json","content":"{\"theme\":\"collision\"}"}|} in
  Unix.putenv "AGENT_THEME" "";
  check "active theme reads Pi settings" ((Themes.active ()).Themes.name = "collision");
  ignore (Themes.set_active_name ~persist:true "dark");
  check "theme selection persists to Pi settings"
    (contains (run "read_file"
                 (Printf.sprintf {|{"path":"%s"}|} (Config_paths.user_settings_file ())))
       "\"theme\":\"dark\"");
  Unix.putenv "AGENT_THEME" "";
  let _ =
    run "write_file"
      {|{"path":"extra_theme.json","content":"{\"name\":\"extra-theme\",\"colors\":{\"accent\":\"#abcdef\"}}"}|}
  in
  Unix.putenv "AGENT_NO_THEMES" "1";
  Unix.putenv "AGENT_THEME_PATHS" "extra_theme.json";
  let explicit_themes = Themes.discover () in
  check "theme CLI path works when discovery disabled"
    (List.exists (fun (t : Themes.t) -> t.name = "extra-theme") explicit_themes
     && not (List.exists (fun (t : Themes.t) -> t.location = project_theme_path) explicit_themes));
  Unix.putenv "AGENT_NO_THEMES" "";
  Unix.putenv "AGENT_THEME_PATHS" "";

  Printf.printf "\n%s\n" (if !failures = 0 then "All resource tests passed." else "FAILURES present.");
  exit (if !failures = 0 then 0 else 1)
