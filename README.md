# Agent Hub

Agent Hub is a native desktop client for Codex App Server, built with Rust and GPUI.
Right now, the product direction is intentionally simple: it is a Codex clone focused on a
fast local desktop experience.

## Current Status

The app already supports the main Codex workflow end to end:
- account login/logout and session state
- workspace and thread management
- streaming turns with live tool output
- approval handling for command/file-change requests
- model and reasoning-effort selection
- image attachments
- workspace Git diff panel

## How It Works

Agent Hub starts `codex app-server` as a child process and communicates over line-delimited
JSON-RPC on stdio. The UI is implemented in GPUI, and most app state/orchestration currently
lives in `src/main.rs`.

If you want protocol details, the docs in `docs/events/` are a good reference for observed
request/response/notification payloads.

## Local Development

### Prerequisites

- Rust toolchain (edition 2024 project)
- Codex CLI with `app-server` support available on your machine
- macOS is the primary tested platform for packaging right now

The app tries to find `codex` at:
- `/opt/homebrew/bin/codex`
- `/usr/local/bin/codex`
- `~/.cargo/bin/codex`
- fallback to `codex` from `PATH`

### Run

```bash
cargo run
```

### Test

```bash
cargo test
```

## Build

To package the app:

```bash
cargo packager --release
```

The repository also includes a macOS release workflow in `.github/workflows/release.yml`
for signed/notarized release artifacts.

## Project Map

- `src/main.rs`: app shell, UI, state management, and event handling
- `src/codex/app_server.rs`: Codex App Server transport and RPC plumbing
- `src/diff_view.rs`: unified diff parsing used by file-change/branch-diff UI
- `src/components/`: reusable UI cards and approval panel
- `src/sidebar/`: sidebar primitives and interaction patterns
- `docs/events/`: captured API payload examples

## References

- Codex App Server docs: <https://developers.openai.com/codex/app-server>
- Codex harness overview: <https://openai.com/index/unlocking-the-codex-harness/>
