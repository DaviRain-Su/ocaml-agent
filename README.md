# OCaml Code Agent

A small but real coding agent written in OCaml. It talks to an LLM in a tool-use
loop: the model reads/writes/edits files, lists directories, and runs shell
commands to accomplish software tasks in your current working directory.

It speaks two wire protocols, so it works with **Claude (Anthropic)**,
**DeepSeek**, **Kimi / Moonshot**, **OpenAI**, and any OpenAI- or
Anthropic-compatible endpoint — all selected through environment variables.

## Design

```
bin/main.ml        REPL / one-shot entrypoint
lib/http.ml        Shared curl-based JSON POST + streaming (SSE) helper
lib/llm.ml         Config (from env) + normalized types + Anthropic & OpenAI adapters
lib/tools.ml       Tool schemas + executors (read/write/edit/list/grep/find/bash/task)
lib/render.ml      Streaming markdown renderer + colorized tool-result previews
lib/skills.ml      Skill discovery (markdown + frontmatter) and prompt injection
lib/prompts.ml     Prompt templates loaded as slash commands
lib/models.ml      Best-effort model catalog (context windows) for --list-models
lib/mentions.ml    @file expansion for prompts and CLI file arguments
lib/extensions.ml  Load custom subprocess-backed tools from a JSON manifest
lib/themes.ml      Pi-style terminal theme discovery and token resolution
lib/settings.ml    Pi-style settings.json helpers and startup defaults
lib/packages.ml    Pi package discovery plus local/npm/git install lifecycle
lib/commands.ml    Session slash-command implementations (frontend-agnostic)
lib/session.ml     Session manager: dir, headers, list, resume, clone, import, export
lib/tui.ml         Full-screen notty TUI (scrollback + input editor)
lib/rpc.ml         JSON-RPC (JSONL) driver for --mode rpc
lib/agent.ml       The agent loop: frontend, approval, tools, sub-agents, compaction
test/test_tools.ml Offline tests (tools, sessions, render, skills, models, extensions)
```

## Features

- **Multi-provider** via env vars (Anthropic / OpenAI / DeepSeek / Kimi / any compatible endpoint).
- **Tools**: Pi wire names `read`, `write`, `edit`, `ls`, `grep`, `find`, `bash`;
  legacy OCaml names such as `read_file` and `run_bash` remain accepted. `read`
  supports offset/limit + large-file truncation; `edit` supports Pi-style
  `oldText`/`newText`, multi-edit + diff.
- **Full-screen TUI** (notty) — scrollback viewport above a live input editor, with
  PgUp/PgDn + mouse-wheel scrolling, input history, Tab autocomplete (commands +
  paths), inline markdown styling (bold/`code`/code blocks), an animated thinking
  spinner, a model picker (Ctrl-P), a `/settings` modal, emacs line editing
  (Ctrl-A/E/U/K/W), configurable keybindings (`.ocaml-agent/keybindings.json`), and a
  bash-approval modal. Terminal colors use Pi-style themes. Falls back to the
  plain line REPL when not a TTY or with `--no-tui`.
- **Streaming output** — assistant text is printed token-by-token as it arrives (SSE),
  with live, line-buffered markdown rendering (headers, bold, inline code, code fences).
- **Tool-result previews** — colorized, truncated output under each tool call (diff
  coloring for edits).
- **Reasoning levels** — `off/low/medium/high` mapped to Anthropic thinking budgets
  and OpenAI `reasoning_effort`; thinking output shown dimmed.
- **Context tracking + auto-compaction** — token usage is read from the API and
  older turns are summarized automatically as the context window fills.
- **Project-context injection** — `AGENTS.md` / `CLAUDE.md` in the cwd, plus the
  working directory, date, and the live provider/model identity, folded into the system prompt.
- **File references** — pass `@path` on the CLI or mention `@path` in a prompt to
  include readable file contents as `<file>` blocks. PNG/JPEG/GIF/WebP paths are
  attached as native inline image blocks for Anthropic/OpenAI-compatible models.
- **Bang shell commands** — type `!command` to run shell and add the output to
  model context, or `!!command` to run shell without adding it to context.
- **Pi subagent CLI compatibility** — supports `--no-session`, `--tools/-t`, and
  file-backed `--append-system-prompt`, which are the flags Pi's subagent
  extension uses when spawning isolated child agents.
- **Tool approval** — tools auto-run by default (like pi). Set `AGENT_AUTO_APPROVE=0`
  (or toggle in `/settings`) to be prompted `[y]es / [N]o / [a]lways` before `bash`
  and subprocess extension tools; with approval on and no TTY, those tools are denied.
- **Session persistence** — sessions default to the Pi-style user agent directory
  (`~/.pi/agent/sessions`), with `AGENT_SESSION_FILE`, `AGENT_SESSION_DIR`, and
  `PI_CODING_AGENT_SESSION_DIR` overrides.
- **Pi-style settings** — reads `~/.pi/agent/settings.json` plus project
  `.pi/settings.json` / `.ocaml-agent/settings.json` for default model/provider,
  thinking, session dir, compaction, model scope, and resource paths.
- **Pi packages** — `packages` entries in settings can point at local package
  directories or installed `npm:` / `git:` sources. Packages contribute
  extensions, skills, prompts, and themes via `package.json` `pi` manifests or
  conventional `extensions/`, `skills/`, `prompts/`, and `themes/` directories.
  `install/remove/uninstall/list/update/config` are accepted as Pi-style package
  commands; `config` lists package resources and supports `+path` / `-path`
  enable-disable filters in settings. npm/git operations shell out to `npm` and `git`. TypeScript and
  JavaScript extensions can register core `pi.registerTool()` tools and
  `pi.registerCommand()` slash commands through the Node bridge, including
  `argumentHint` display text and async `getArgumentCompletions()` Tab
  completions, plus shared `pi.events` EventBus `on()` / `emit()` handlers
  across loaded JS extensions, plus
  `session_start`, `session_before_switch`, `session_before_fork`,
  `session_before_compact`, `session_shutdown`, `session_compact`,
  `agent_start`, `agent_end`, `turn_start`, `turn_end`, `before_agent_start`,
  `context`, `message_start`, `message_update`, `message_end`,
  `tool_execution_start`, `tool_execution_update`, `tool_execution_end`,
  `input`, `tool_call`, `tool_result`, `user_bash`, `model_select`, and
  `thinking_level_select`, plus `resources_discover` for dynamic skill,
  prompt, and theme paths, plus
  `registerFlag()` / `getFlag()` with `PI_FLAG_*` / `AGENT_FLAG_*` env overrides
  and `registerShortcut()` for TUI command/action shortcuts. Extension
  `registerProvider()` can add OpenAI/Anthropic-compatible providers and models
  to `/model`, `--list-models`, and runtime provider resolution, or provide a
  local extension runtime through `complete` / `chat` / `generate` handlers.
  `registerMessageRenderer()` / `registerRenderer()` can transform
  assistant/tool display text without changing the persisted model conversation;
  common rich component return values are preserved as component metadata and
  rendered through terminal text adapters. `registerMessageRenderer("type",
  renderer)` also renders matching `sendMessage()` custom messages for TUI/RPC
  adapters.
  Extension tool handlers use Pi's
  `execute(toolCallId, params, signal, onUpdate, ctx)` order; `onUpdate`
  callbacks are captured as tool-update surface metadata, and optional
  `prepareArguments()` hooks normalize raw tool input before execution.
  Pi-style local file and
  bash operations (`readFile`, `writeFile`, `listDir`, `stat`, `exists`, `exec` /
  `bash.exec`) are available through `ctx.operations`, with compatibility methods
  also exposed on the signal object for older local fixtures. Extension
  command/tool/shortcut handlers also
  receive `ctx.ui` noninteractive fallbacks for `notify`, `confirm`, `input`,
  and `select`; notifications are surfaced in text output and prompts return
  supplied/default values when no interactive frontend is attached. Command and
  shortcut execution preserves captured UI request metadata so frontends can
  surface pending confirm/input/select requests, plus terminal/RPC metadata for
  status, widget, title, header/footer, editor, and working-indicator surface
  calls. Component factories that can render terminal lines are executed through
  a noninteractive adapter and exposed as surface metadata. The same bridge also
  supports interception hooks for startup-time dynamic tool/command registration, system
  prompt/message context mutation, session switch/fork/compact cancellation,
  session lifecycle notifications, turn lifecycle notifications,
  provider request/response lifecycle hooks,
  finalized-message replacement, input transforms, blocking, argument mutation,
  text-result replacement, and replacement results or custom BashOperations for
  `!cmd` / `!!cmd`.
- **Sub-agents** — the `task` tool delegates a self-contained sub-task to a fresh
  nested agent (bounded depth) and returns its answer.
- **Skills** — markdown files with frontmatter in `.ocaml-agent/skills/` or
  `.pi/skills/`, `.claude/skills/`, or the Pi-style user agent directory are
  discovered and listed in the system prompt; the model reads a skill's file on
  demand (prompt-injection model, no separate runtime). Use `--skill`/`--no-skills`
  for Pi-style resource control.
- **Prompt templates** — markdown files in `.ocaml-agent/prompts/` or `.pi/prompts/`
  or the Pi-style user agent directory become slash commands with `$1`, `$@`,
  `$ARGUMENTS`, and `${@:N[:L]}` expansion.
- **Extensions** — declare custom tools in `.ocaml-agent/tools.json`, `.pi/tools.json`,
  `AGENT_TOOLS_FILE`, or explicit `--extension` paths; each runs an external command
  receiving the tool input as JSON on stdin and returning its output. Extension tools
  can replace built-ins using either canonical names (`run_bash`) or Pi wire names
  (`bash`) and require the same approval path as shell commands.
- **Themes** — Pi-style JSON themes are discovered from `~/.pi/agent/themes`,
  `.pi/themes`, `.ocaml-agent/themes`, and explicit `--theme` paths. The active
  theme is selected by `AGENT_THEME` / `PI_THEME` or `settings.json`.
- **Interactive commands**: `/model`, `/scoped-models`, `/think`, `/compact`,
  `/session`, `/sessions`, `/resume`, `/name`, `/fork`, `/clone`, `/export`,
  `/import`, `/copy`, `/changelog`, `/hotkeys`, `/reload`, `/new`, `/help`.
- **Embeddable**: `--mode rpc` accepts Pi-style JSONL command objects
  (`type: "prompt"`, `get_state`, `get_available_models`, `bash`,
  `get_commands`, `execute_command`, `cycle_model`, queue/retry controls,
  session/export commands, etc.) while preserving the older `method` requests. RPC command discovery includes
  extension slash commands, and `execute_command` returns extension command text
  plus captured UI request/surface metadata, while also emitting Pi-style
  `extension_ui_request` events for supported UI calls. CLI `@file` / image
  arguments supplied when starting RPC mode are included as prompt prefix context.
  `--mode json` prints Pi-style JSONL turn/tool events; `--list-models [pat]`;
  `--export`.
- **CLI flags**: `-m/--model`, `--provider`, `--api-key`, `--thinking`,
  `--system-prompt`, `--append-system-prompt`, `-c`, `-r`, `--session`,
  `--fork`, `--session-dir`, `--models`, `-p`, `--no-session`, `--tools/-t`,
  `--no-tools`, `--no-builtin-tools`, `--extension`, `--no-extensions`,
  `--skill`, `--no-skills`, `--prompt-template`, `--no-prompt-templates`,
  `--theme`, `--no-themes`, `--no-context-files`, `--offline`, `--verbose`,
  `--version`, `--no-tui`,
  `--mode`, `--list-models`, `--export`.

Runtime dependencies are declared in `dune` / `ocaml_agent.opam`. HTTP is done
by shelling out to `curl`, so there's no TLS/HTTP stack to install.

The agent loop works only with the normalized `content`/`turn` types in
`lib/llm.ml`; each provider serializes them to its own wire format (Anthropic
Messages or OpenAI Chat Completions) and parses responses back, so adding a new
provider doesn't touch `agent.ml`.

### The loop

1. The user's message is appended to the conversation.
2. We POST the conversation + tool schemas to the Messages API.
3. If the model returns `tool_use` blocks, each tool is executed and the results
   are sent back as a `tool_result` user turn. Go to 2.
4. When the model stops requesting tools, its text is the final answer.

### Tools

| Tool | Purpose |
|------|---------|
| `read` | Read file contents, with optional `offset`/`limit` |
| `write` | Write/overwrite a file (creates parent dirs) |
| `edit` | Pi-style unique exact replacements, including multi-edit |
| `ls` | List a directory's entries |
| `grep` | Search file contents |
| `find` | Find files by glob |
| `bash` | Run a shell command, capturing stdout+stderr+exit |
| `task` | Delegate a prompt to an isolated nested agent |

## Build

```sh
opam switch default        # needs ocaml + dune + yojson
eval $(opam env)
dune build
```

## Configuration

**Zero-config by default: just export a provider's API key and run.** With no
`AGENT_PROVIDER` set, the agent scans the environment for a known key and picks
that provider plus a sensible default model — the same auto-detection pattern as
larger agents. Detection priority (first key found wins):

| Provider          | Key env var(s)                  | Base URL                                          | Default model            |
|-------------------|---------------------------------|---------------------------------------------------|--------------------------|
| `anthropic`/`claude` | `ANTHROPIC_API_KEY`          | `https://api.anthropic.com`                       | `claude-opus-4-7`        |
| `deepseek`        | `DEEPSEEK_API_KEY`              | `https://api.deepseek.com`                        | `deepseek-v4-pro`        |
| `kimi`/`kimi-coding`/`kfc` | `KIMI_API_KEY`         | `https://api.kimi.com/coding` (Anthropic protocol)| `kimi-for-coding`        |
| `moonshot`        | `MOONSHOT_API_KEY`              | `https://api.moonshot.cn/v1`                      | `kimi-k2-0905-preview`   |
| `openai`          | `OPENAI_API_KEY`                | `https://api.openai.com/v1`                       | `gpt-4o`                 |
| `openrouter`      | `OPENROUTER_API_KEY`            | `https://openrouter.ai/api/v1`                    | `openai/gpt-4o`          |
| `groq`            | `GROQ_API_KEY`                  | `https://api.groq.com/openai/v1`                  | `llama-3.3-70b-versatile`|
| `xai`/`grok`      | `XAI_API_KEY`                   | `https://api.x.ai/v1`                             | `grok-2-latest`          |
| `mistral`         | `MISTRAL_API_KEY`               | `https://api.mistral.ai/v1`                       | `mistral-large-latest`   |
| `zai`/`zhipu`/`glm`| `ZAI_API_KEY` / `ZHIPU_API_KEY`| `https://api.z.ai/api/coding/paas/v4`            | `glm-4.6`                |
| `gemini`/`google` | `GEMINI_API_KEY` / `GOOGLE_API_KEY` | `https://generativelanguage.googleapis.com/v1beta/openai` | `gemini-2.0-flash` |

Model defaults are best-effort and may drift — override with `AGENT_MODEL`.

Overrides (all optional):

| Variable             | Purpose                                                      |
|----------------------|--------------------------------------------------------------|
| `AGENT_PROVIDER`     | Force a provider (alias above) instead of auto-detecting     |
| `AGENT_MODEL`        | Model name                                                   |
| `AGENT_API_KEY`      | API key (overrides the provider-specific key env)            |
| `AGENT_BASE_URL`     | API base URL override (for custom / compatible endpoints)    |
| `AGENT_MAX_TOKENS`   | Max output tokens (default `4096`)                           |
| `AGENT_AUTO_APPROVE` | Auto-run tools without prompting (default on; set `0` to require approval) |
| `AGENT_MAX_TOOL_ROUNDS` | Max tool-use rounds per turn before stopping (default `20`) |
| `AGENT_SESSION_FILE` | JSONL file to persist to / resume from                       |
| `AGENT_SESSION_DIR` | Directory for saved sessions (overridden by `--session-dir`) |
| `PI_CODING_AGENT_DIR` | Pi-compatible user config directory (default `~/.pi/agent`) |
| `PI_CODING_AGENT_SESSION_DIR` | Pi-compatible session directory override |
| `AGENT_THINKING`     | Reasoning level: `off` (default), `low`, `medium`, `high`    |
| `AGENT_SCOPED_MODELS` | Newline/comma-separated model patterns for the model picker |
| `AGENT_SYSTEM_PROMPT` | Replace the default system prompt; if it names a file, that file is read |
| `AGENT_APPEND_SYSTEM_PROMPT` | Append extra system prompt text; if it names a file, that file is read |
| `AGENT_NO_CONTEXT_FILES` | Disable `AGENTS.md` / `CLAUDE.md` prompt injection |
| `AGENT_SKILL_PATHS` | Newline-separated skill files or directories to load |
| `AGENT_NO_SKILLS` | Disable default skill discovery while still allowing explicit paths |
| `AGENT_PROMPT_TEMPLATE_PATHS` | Newline-separated prompt template files or directories to load |
| `AGENT_NO_PROMPT_TEMPLATES` | Disable default prompt template discovery while still allowing explicit paths |
| `AGENT_EXTENSION_PATHS` | Newline-separated extension manifest files or directories to load |
| `AGENT_NO_EXTENSIONS` | Disable default extension discovery while still allowing explicit paths |
| `AGENT_THEME` / `PI_THEME` | Active terminal theme name |
| `AGENT_THEME_PATHS` | Newline-separated theme files or directories to load |
| `AGENT_NO_THEMES` | Disable default theme discovery while still allowing explicit paths |
| `AGENT_SHELL_PATH` / `PI_SHELL_PATH` | Shell executable used for bash-mode commands |
| `AGENT_SHELL_COMMAND_PREFIX` / `PI_SHELL_COMMAND_PREFIX` | Prefix prepended to bash-mode commands |
| `AGENT_CONTEXT_WINDOW` | Context window in tokens for compaction (default `128000`) |
| `AGENT_AUTO_COMPACT` | Auto-summarize older turns near the limit (default on)       |
| `AGENT_COMPACT_THRESHOLD` | Fraction of the window that triggers compaction (default `0.75`) |

Pi-compatible `settings.json` is loaded from the global agent directory first
and then from project settings, with project values overriding global values.
Supported fields include:

| Field | Purpose |
|-------|---------|
| `defaultProvider` | Default provider when no env/CLI provider is set |
| `defaultModel` | Default model when no env/CLI model is set |
| `defaultThinkingLevel` | Default reasoning level |
| `sessionDir` | Session directory when no session-dir env/CLI override is set |
| `enabledModels` | Model picker scope patterns |
| `skills` / `prompts` / `extensions` / `themes` | Extra resource files or directories |
| `packages` | Local, npm, and git Pi packages to load resources from |
| `npmCommand` | Optional argv-style npm command for package install/update |
| `theme` | Active TUI theme |
| `compaction.enabled` | Enable/disable auto-compaction |
| `shellPath` | Shell executable used for bash-mode commands |
| `shellCommandPrefix` | Prefix prepended to `bash` and `!cmd` commands |
| `quietStartup` | Suppress nonessential startup notices unless `--verbose` is used |

## Run

```sh
# Simplest: export any supported key, then run — provider is auto-detected.
export DEEPSEEK_API_KEY=sk-...
dune exec ocaml-agent

export ANTHROPIC_API_KEY=sk-ant-...
dune exec ocaml-agent

# Force a specific provider when you have several keys set:
AGENT_PROVIDER=kimi dune exec ocaml-agent

# Pi-style model selectors: provider prefix, thinking suffix, and picker scope:
dune exec ocaml-agent -- --model openai/gpt-4o "Help me refactor this code"
dune exec ocaml-agent -- --model claude-sonnet-4-6:high "Plan this migration"
dune exec ocaml-agent -- --models "anthropic/*,gpt-4o,glm:low"

# Kimi via its Anthropic-compatible endpoint instead of the OpenAI one:
AGENT_PROVIDER=anthropic \
AGENT_BASE_URL=https://api.moonshot.cn/anthropic \
ANTHROPIC_API_KEY=<moonshot key> \
AGENT_MODEL=kimi-k2-0905-preview \
dune exec ocaml-agent

# Any other OpenAI-compatible endpoint:
AGENT_PROVIDER=openai \
AGENT_BASE_URL=https://your-endpoint/v1 \
AGENT_API_KEY=... \
AGENT_MODEL=your-model \
dune exec ocaml-agent

# One-shot, then exit:
dune exec ocaml-agent -- "add a function to lib/foo.ml that reverses a list"

# One-shot with file references:
dune exec ocaml-agent -- -p @README.md "summarize this project"

# Pi-style isolated child invocation (used by subagent-style wrappers):
dune exec ocaml-agent -- --mode json -p --no-session --tools read,grep,find,ls \
  --append-system-prompt reviewer.md "Task: review the current diff"

# Embedding/RPC mode (one JSON command per line on stdin):
printf '%s\n' '{"id":"s1","type":"get_state"}' '{"id":"m1","type":"get_available_models"}' \
  | dune exec ocaml-agent -- --mode rpc

# Pi package registration:
dune exec ocaml-agent -- install ./local-pkg --local
dune exec ocaml-agent -- install npm:@scope/pkg
dune exec ocaml-agent -- install git:github.com/user/repo
dune exec ocaml-agent -- list
dune exec ocaml-agent -- config
dune exec ocaml-agent -- config --disable ./local-pkg skills skills/foo/SKILL.md --local
dune exec ocaml-agent -- config --enable ./local-pkg skills skills/foo/SKILL.md --local
dune exec ocaml-agent -- update --extensions
dune exec ocaml-agent -- update --extension npm:@scope/pkg
dune exec ocaml-agent -- remove ./local-pkg --local
```

Pi packages can be loaded from settings:

```json
{
  "packages": ["../my-pi-package", "npm:@scope/pkg", "git:github.com/user/repo"]
}
```

For package directories, `package.json` `pi.extensions` / `pi.skills` /
`pi.prompts` / `pi.themes` paths are honored. Without a `pi` manifest, the
agent looks for conventional resource directories with those names. Remote
package installation requires local `npm` and `git` access. TypeScript/JavaScript
extensions support the core `pi.registerTool()` and `pi.registerCommand()` paths
through a Node bridge, including `tool_call` and `tool_result` hooks for tool
interception, `input` transforms/handled events, `user_bash` result replacement
or BashOperations backends for `!cmd` / `!!cmd`, `session_start` dynamic
tool/command registration, `before_agent_start` system prompt/message injection,
`context` message mutation before LLM calls, `agent_start` / `agent_end` /
  `turn_start` / `turn_end` / `message_*` turn lifecycle hooks,
  `tool_execution_*` notifications, session switch/fork/compact guard events,
  model/thinking selection events, `resources_discover` dynamic resource paths,
  command `argumentHint` labels and `getArgumentCompletions()` Tab candidates,
  `pi.getCommands()` command inventory with extension/prompt/skill source
  metadata,
  shared `pi.events` EventBus handlers for loaded JS extension communication,
  extension `registerFlag()` / `getFlag()` defaults plus `PI_FLAG_*` /
  `AGENT_FLAG_*` environment overrides, `registerShortcut()` TUI shortcuts, and
  `getActiveTools()` / `getAllTools()` / `setActiveTools()` runtime tool
  scoping for extension presets and mode switches, `setModel()` runtime model
  switching, `getThinkingLevel()` / `setThinkingLevel()` runtime thinking-mode
  switches, `getSessionName()` / `setSessionName()` session metadata, and
  `appendEntry()` / `setLabel()` session side-entry persistence. Extension UI
  contexts expose theme lookup/switching through `getAllThemes()`, `getTheme()`,
  and `setTheme()`, plus tool-panel state through `getToolsExpanded()` /
  `setToolsExpanded()`. Command/event contexts expose current model, system prompt,
  idle/pending state, context usage, a Pi-style readonly `sessionManager`
  snapshot including `getChildren()`, a readonly `modelRegistry` adapter, and
  runtime action requests for `abort()`, `shutdown()`,
  `compact()`, command-context `reload()`, `newSession()`, `fork()`,
  `navigateTree()`, and `switchSession()`; `newSession` / `fork` /
  `switchSession` capture serializable `setup` / `withSession` side effects
  such as session entries, labels, session names, setup-time
  `appendMessage()` / `appendCustomMessageEntry()` / `appendThinkingLevelChange()` /
  `appendModelChange()` / `appendCompaction()` calls, harness-style
  `appendSessionName()` / `appendLabel()` aliases, session tree `branch()` /
  `resetLeaf()` / `branchWithSummary()` moves, and custom messages.
  `navigateTree()` emits `session_before_tree` / `session_tree` hooks so
  extensions can cancel navigation, override the target label, or provide
  extension branch summaries for summarized navigation; user-message targets
  and `custom_message` targets return Pi-style `editorText` for editor refill.
  Unsummarized navigation persists Pi-style `leaf` entries so the active
  session-tree position survives labels and session metadata side entries.
  Branch summaries and `custom_message` entries are converted into user-role
  context messages for the next model request. Extension `pi.exec()` runs
  argv-style subprocesses and returns Pi-style `stdout` / `stderr` / `code` /
  `exitCode` / `killed` result fields.
	  OpenAI/Anthropic-compatible `registerProvider()` / `unregisterProvider()`
	  providers, extension provider runtimes, and text fallback
	  `registerMessageRenderer()` / `registerRenderer()` renderers with structured
	  rich component metadata plus terminal text adapters.
	  Provider lifecycle hooks can inspect and replace built-in request payloads
	  before dispatch and receive response metadata after successful responses.
  Extension `sendMessage()` / `sendUserMessage()` custom messages are captured,
  and matching `registerMessageRenderer("type", renderer)` handlers provide
  rendered TUI/RPC message text.
Extension tool execute handlers follow Pi's
`execute(toolCallId, params, signal, onUpdate, ctx)` signature, with optional
`prepareArguments()` normalization and local file/bash operation backends
available on `ctx.operations`. Command/tool/shortcut handlers get
noninteractive `ctx.ui.notify` / `confirm` / `input` / `select`
fallbacks, with captured UI request metadata exposed to the TUI/RPC-compatible
execution layer. The bridge exports Pi's `withFileMutationQueue()` helper for
custom tools that need same-file read-modify-write serialization, plus
tool-event type guards such as `isToolCallEventType()` and
`isBashToolResult()`, and `wrapRegisteredTool()` / `wrapRegisteredTools()` for
SDK-compatible tool wrapping. It also exposes the Pi extension loader/runtime
entry points `createExtensionRuntime()`, `loadExtensionFromFactory()`,
`loadExtensions()`, `discoverAndLoadExtensions()`, and `ExtensionRunner()` for
programmatic extension composition. Custom UI surface calls such as status, widget, title,
header/footer, editor text, paste, and working-state updates are captured for
terminal and RPC adapters. TUI working message, visibility, and indicator
settings drive the active turn status row. Component factories that can render
to terminal lines are adapted into surface metadata; rendered
header/footer/widget/editor-component surfaces are mounted into persistent TUI
bands, while custom overlay requests, overlay options, and synthetic overlay
handle actions are captured for RPC/TUI adapters. Fully focusable component
input handling remains parity work.

In the REPL, type your request at the `you>` prompt. Tool calls are shown as
`⚙ tool_name {input}`. Type `/exit` or Ctrl-D to quit.

## Test

```sh
dune exec test/test_tools.exe   # offline tool tests
```

## Safety note

`bash` executes arbitrary shell commands the model chooses, with your
permissions and no sandbox. **Tools auto-run by default** (matching pi), so only
use the agent in a directory you trust. Set `AGENT_AUTO_APPROVE=0` (or toggle in
`/settings`) to require interactive `[y]es / [N]o / [a]lways` approval before
`bash` and subprocess extension tools (those declared in
`.ocaml-agent/tools.json`).
