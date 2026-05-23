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
lib/tools.ml       Tool schemas + executors (read/write/edit/list/bash)
lib/session.ml     JSONL session persistence (save + resume)
lib/agent.ml       The agent loop: system prompt, approval gate, run tools, repeat
test/test_tools.ml Offline smoke tests (tools, turn JSON, session, system prompt)
```

## Features

- **Multi-provider** via env vars (Anthropic / OpenAI / DeepSeek / Kimi / any compatible endpoint).
- **Tools**: `read_file`, `write_file`, `edit_file` (multi-edit + diff), `list_dir`,
  `grep` (regex search), `find` (glob), `run_bash`.
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
- **run_bash approval gate** — shell commands prompt for `[y]es / [N]o / [a]lways`
  before running (skip with `AGENT_AUTO_APPROVE=1`; denied automatically when there's no TTY).
- **Session persistence** — set `AGENT_SESSION_FILE` (or use `-c`) to append each
  turn as JSONL and resume the conversation on the next run.
- **Interactive commands**: `/model`, `/think`, `/compact`, `/session`, `/new`, `/help`.
- **CLI flags**: `-m/--model`, `--provider`, `--thinking`, `-c`, `-p`, `--no-tools`.

The only dependencies are `yojson`, `unix`, and `str`. HTTP is done by shelling
out to `curl`, so there's no TLS/HTTP stack to install.

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

| Tool         | Purpose                                             |
|--------------|-----------------------------------------------------|
| `read_file`  | Read a file's contents                              |
| `write_file` | Write/overwrite a file (creates parent dirs)        |
| `edit_file`  | Replace the first exact occurrence of a substring   |
| `list_dir`   | List a directory's entries                          |
| `run_bash`   | Run a shell command, capturing stdout+stderr+exit   |

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
| `deepseek`        | `DEEPSEEK_API_KEY`              | `https://api.deepseek.com`                        | `deepseek-chat`          |
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
| `AGENT_AUTO_APPROVE` | Skip the run_bash approval prompt when truthy                |
| `AGENT_SESSION_FILE` | JSONL file to persist to / resume from                       |
| `AGENT_THINKING`     | Reasoning level: `off` (default), `low`, `medium`, `high`    |
| `AGENT_CONTEXT_WINDOW` | Context window in tokens for compaction (default `128000`) |
| `AGENT_AUTO_COMPACT` | Auto-summarize older turns near the limit (default on)       |
| `AGENT_COMPACT_THRESHOLD` | Fraction of the window that triggers compaction (default `0.75`) |

## Run

```sh
# Simplest: export any supported key, then run — provider is auto-detected.
export DEEPSEEK_API_KEY=sk-...
dune exec ocaml-agent

export ANTHROPIC_API_KEY=sk-ant-...
dune exec ocaml-agent

# Force a specific provider when you have several keys set:
AGENT_PROVIDER=kimi dune exec ocaml-agent

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
```

In the REPL, type your request at the `you>` prompt. Tool calls are shown as
`⚙ tool_name {input}`. Type `/exit` or Ctrl-D to quit.

## Test

```sh
dune exec test/test_tools.exe   # offline tool tests
```

## Safety note

`run_bash` executes arbitrary shell commands the model chooses, with your
permissions and no sandbox. By default each command must be approved
interactively; `AGENT_AUTO_APPROVE=1` removes that gate, so only use it in a
directory you trust.
