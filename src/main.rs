mod app_assets;
mod codex;
mod theme;

use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use codex::app_server::{AppServer, AppServerError, RequestMethod, ServerNotification};
use gpui::prelude::*;
use gpui::{
    App, Application, Bounds, Context, Entity, KeyBinding, Menu, MenuItem, Render, ScrollHandle,
    Subscription, Window, WindowBounds, WindowOptions, actions, div, px, size,
};
use gpui_component::Root;
use gpui_component::input::{Input, InputEvent, InputState};
use gpui_component::sidebar::{
    Sidebar, SidebarFooter, SidebarGroup, SidebarHeader, SidebarMenu, SidebarMenuItem,
    SidebarToggleButton,
};
use gpui_component::text::{TextView, TextViewState, TextViewStyle};
use gpui_component::theme::{Theme as ComponentTheme, hsl};
use gpui_component::{Icon, IconName};
use gpui_component_assets::Assets;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use theme::Theme as AppTheme;

actions!(
    agent_hub,
    [
        About,
        Settings,
        Hide,
        HideOthers,
        ShowAll,
        Quit,
        NewWorkspace
    ]
);

fn app_menus() -> Vec<Menu> {
    vec![
        Menu {
            name: "Agent Hub".into(),
            items: vec![
                MenuItem::action("About Agent Hub", About),
                MenuItem::separator(),
                MenuItem::action("Settings...", Settings),
                MenuItem::separator(),
                MenuItem::action("Hide Agent Hub", Hide),
                MenuItem::action("Hide Others", HideOthers),
                MenuItem::action("Show All", ShowAll),
                MenuItem::separator(),
                MenuItem::action("Quit Agent Hub", Quit),
            ],
        },
        Menu {
            name: "File".into(),
            items: vec![MenuItem::action("New Workspace", NewWorkspace)],
        },
    ]
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ThreadRow {
    id: String,
    title: String,
    updated_at: Option<u64>,
    #[serde(default)]
    cwd: String,
    #[serde(default)]
    path: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct ThreadListResponse {
    data: Vec<ThreadListItem>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct ThreadListItem {
    id: String,
    preview: Option<String>,
    created_at: Option<u64>,
    updated_at: Option<u64>,
    cwd: Option<String>,
    path: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ThreadCache {
    saved_at: u64,
    cwd: String,
    threads: Vec<ThreadRow>,
}

#[derive(Debug, Clone)]
struct WorkspaceGroup {
    cwd: String,
    name: String,
    latest_ts: u64,
    threads: Vec<ThreadRow>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ThreadSpeaker {
    User,
    Assistant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ThreadMessageKind {
    Text,
    CommandExecution {
        header: String,
        status_label: String,
        expanded: bool,
    },
}

struct ThreadMessage {
    speaker: ThreadSpeaker,
    content: String,
    view_state: Entity<TextViewState>,
    kind: ThreadMessageKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RenderableMessage {
    Text {
        speaker: ThreadSpeaker,
        content: String,
    },
    CommandExecution {
        header: String,
        status_label: String,
        content: String,
        expanded: bool,
    },
}

struct AppShell {
    started: bool,
    loading_threads: bool,
    sidebar_collapsed: bool,
    thread_error: Option<String>,
    cwd: String,
    cache_path: PathBuf,
    raw_cache_path: PathBuf,
    thread_content_cache_path: PathBuf,
    thread_resume_content_cache_path: PathBuf,
    threads: Vec<ThreadRow>,
    expanded_workspace_threads: HashSet<String>,
    selected_thread_id: Option<String>,
    selected_thread_title: Option<String>,
    selected_thread_messages: Vec<ThreadMessage>,
    selected_thread_loaded_from_api_id: Option<String>,
    loading_selected_thread: bool,
    selected_thread_error: Option<String>,
    sending_message: bool,
    streaming_message_ix: Option<usize>,
    scroll_handle: ScrollHandle,
    input_state: Entity<InputState>,
    _subscriptions: Vec<Subscription>,
    _app_server: Option<Arc<AppServer>>,
}

impl AppShell {
    const MAX_THREADS_PER_WORKSPACE: usize = 10;
    const MAX_RENDERED_THREAD_CHARS: usize = 80_000;

    fn new(window: &mut Window, cx: &mut Context<Self>) -> Self {
        let cwd = Self::current_cwd();
        let cache_path = Self::thread_cache_path(&cwd);
        let raw_cache_path = Self::raw_thread_cache_path(&cwd);
        let thread_content_cache_path = Self::thread_content_cache_path(&cwd);
        let thread_resume_content_cache_path = Self::thread_resume_content_cache_path(&cwd);
        let (threads, thread_error) = match Self::read_threads_cache(&cache_path) {
            Ok(rows) => (rows, None),
            Err(error) => (Vec::new(), Some(format!("cache read failed: {error}"))),
        };

        let input_state = cx.new(|cx| {
            InputState::new(window, cx)
                .placeholder("Ask Codex anything, @ to add files, / for commands")
                .auto_grow(1, 8)
        });

        let _subscriptions = vec![cx.subscribe_in(&input_state, window, {
            move |this: &mut Self, _, ev: &InputEvent, window, cx| {
                if let InputEvent::PressEnter { secondary } = ev {
                    if !*secondary {
                        this.send_message(window, cx);
                    }
                }
            }
        })];

        let mut shell = Self {
            started: false,
            loading_threads: false,
            sidebar_collapsed: false,
            thread_error,
            cwd,
            cache_path,
            raw_cache_path,
            thread_content_cache_path,
            thread_resume_content_cache_path,
            threads,
            expanded_workspace_threads: HashSet::new(),
            selected_thread_id: None,
            selected_thread_title: None,
            selected_thread_messages: Vec::new(),
            selected_thread_loaded_from_api_id: None,
            loading_selected_thread: false,
            selected_thread_error: None,
            sending_message: false,
            streaming_message_ix: None,
            scroll_handle: ScrollHandle::new(),
            input_state,
            _subscriptions,
            _app_server: None,
        };
        shell.refresh_selected_thread(cx);
        shell
    }

    fn current_cwd() -> String {
        std::env::current_dir()
            .ok()
            .and_then(|path| path.to_str().map(str::to_owned))
            .unwrap_or_else(|| ".".to_string())
    }

    fn make_title(preview: Option<&str>, id: &str) -> String {
        let text = preview
            .unwrap_or_default()
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");

        if text.is_empty() {
            let short_id = id.chars().take(8).collect::<String>();
            return format!("thread-{short_id}");
        }

        Self::truncate(&text, 34)
    }

    fn truncate(input: &str, max_chars: usize) -> String {
        let mut output = String::new();
        for (index, ch) in input.chars().enumerate() {
            if index >= max_chars {
                output.push_str("...");
                return output;
            }
            output.push(ch);
        }
        output
    }

    fn workspace_name(cwd: &str) -> String {
        if cwd.is_empty() {
            return "unknown".to_string();
        }

        Path::new(cwd)
            .file_name()
            .and_then(|segment| segment.to_str())
            .map(str::to_owned)
            .unwrap_or_else(|| cwd.to_string())
    }

    fn relative_time(unix_ts: u64) -> String {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|duration| duration.as_secs())
            .unwrap_or(unix_ts);
        let delta = now.saturating_sub(unix_ts);

        if delta < 60 {
            return "now".to_string();
        }
        if delta < 3_600 {
            return format!("{}m", delta / 60);
        }
        if delta < 86_400 {
            return format!("{}h", delta / 3_600);
        }

        format!("{}d", delta / 86_400)
    }

    fn unix_now() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|duration| duration.as_secs())
            .unwrap_or(0)
    }

    fn thread_cache_path(cwd: &str) -> PathBuf {
        PathBuf::from(cwd).join(".agent-hub-threads.json")
    }

    fn raw_thread_cache_path(cwd: &str) -> PathBuf {
        PathBuf::from(cwd).join(".agent-hub-threads-raw.json")
    }

    fn thread_content_cache_path(cwd: &str) -> PathBuf {
        PathBuf::from(cwd).join(".agent-hub-thread-content.json")
    }

    fn thread_resume_content_cache_path(cwd: &str) -> PathBuf {
        PathBuf::from(cwd).join(".agent-hub-thread-resume-content.json")
    }

    fn write_thread_content_cache(path: &Path, value: &Value) -> Result<(), String> {
        let text = serde_json::to_string_pretty(&value)
            .map_err(|serialize_error| format!("serialize failed: {serialize_error}"))?;
        fs::write(path, text).map_err(|write_error| write_error.to_string())
    }

    fn app_server_error_as_json(error: &AppServerError) -> Value {
        match error {
            AppServerError::Rpc(rpc_error) => json!({
                "type": "rpc",
                "code": rpc_error.code,
                "message": rpc_error.message,
                "data": rpc_error.data
            }),
            AppServerError::Io(io_error) => json!({
                "type": "io",
                "message": io_error.to_string()
            }),
            AppServerError::Json(json_error) => json!({
                "type": "json",
                "message": json_error.to_string()
            }),
            AppServerError::Disconnected => json!({
                "type": "disconnected",
                "message": "app-server disconnected"
            }),
        }
    }

    fn read_threads_cache(path: &Path) -> Result<Vec<ThreadRow>, String> {
        let text = match fs::read_to_string(path) {
            Ok(text) => text,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
            Err(error) => return Err(error.to_string()),
        };

        let cache: ThreadCache =
            serde_json::from_str(&text).map_err(|error| format!("invalid json: {error}"))?;
        Ok(cache.threads)
    }

    fn write_threads_cache(path: &Path, cwd: &str, threads: &[ThreadRow]) -> Result<(), String> {
        let cache = ThreadCache {
            saved_at: Self::unix_now(),
            cwd: cwd.to_string(),
            threads: threads.to_vec(),
        };
        let text = serde_json::to_string_pretty(&cache)
            .map_err(|error| format!("serialize failed: {error}"))?;
        fs::write(path, text).map_err(|error| error.to_string())
    }

    fn write_raw_threads_cache(path: &Path, cwd: &str, payload: &Value) -> Result<(), String> {
        let value = json!({
            "saved_at": Self::unix_now(),
            "cwd": cwd,
            "thread_list_response": payload
        });
        let text = serde_json::to_string_pretty(&value)
            .map_err(|error| format!("serialize failed: {error}"))?;
        fs::write(path, text).map_err(|error| error.to_string())
    }

    fn group_threads_by_workspace(threads: &[ThreadRow], current_cwd: &str) -> Vec<WorkspaceGroup> {
        let mut buckets: HashMap<String, Vec<ThreadRow>> = HashMap::new();
        for row in threads.iter().cloned() {
            let key = if row.cwd.is_empty() {
                "unknown".to_string()
            } else {
                row.cwd.clone()
            };
            buckets.entry(key).or_default().push(row);
        }

        let mut groups = buckets
            .into_iter()
            .map(|(cwd, mut rows)| {
                rows.sort_by_key(|row| Reverse(row.updated_at.unwrap_or(0)));
                let latest_ts = rows.first().and_then(|row| row.updated_at).unwrap_or(0);
                WorkspaceGroup {
                    name: Self::workspace_name(&cwd),
                    cwd,
                    latest_ts,
                    threads: rows,
                }
            })
            .collect::<Vec<_>>();

        groups.sort_by(|a, b| {
            let a_current = a.cwd == current_cwd;
            let b_current = b.cwd == current_cwd;
            b_current
                .cmp(&a_current)
                .then_with(|| b.latest_ts.cmp(&a.latest_ts))
                .then_with(|| a.name.cmp(&b.name))
        });
        groups
    }

    fn parse_thread_rows(value: Value, cwd: &str) -> Result<Vec<ThreadRow>, String> {
        let response: ThreadListResponse =
            serde_json::from_value(value).map_err(|error| format!("invalid response: {error}"))?;

        let mut rows = response
            .data
            .into_iter()
            .map(|entry| {
                let timestamp = entry.updated_at.or(entry.created_at);
                let id = entry.id;
                ThreadRow {
                    title: Self::make_title(entry.preview.as_deref(), &id),
                    id,
                    updated_at: timestamp,
                    cwd: entry.cwd.unwrap_or_else(|| cwd.to_string()),
                    path: entry.path.filter(|path| !path.is_empty()),
                }
            })
            .collect::<Vec<_>>();

        rows.sort_by_key(|row| Reverse(row.updated_at.unwrap_or(0)));
        rows.truncate(50);

        Ok(rows)
    }

    fn preferred_thread(
        threads: &[ThreadRow],
        cwd: &str,
        selected_id: Option<&str>,
    ) -> Option<ThreadRow> {
        if let Some(selected_id) = selected_id {
            if let Some(row) = threads.iter().find(|row| row.id == selected_id) {
                return Some(row.clone());
            }
        }

        threads
            .iter()
            .find(|row| row.cwd == cwd && row.path.is_some())
            .or_else(|| threads.iter().find(|row| row.path.is_some()))
            .or_else(|| threads.first())
            .cloned()
    }

    fn new_message_state(text: &str, cx: &mut Context<Self>) -> Entity<TextViewState> {
        cx.new(|cx| TextViewState::markdown(text, cx))
    }

    fn build_thread_message(
        speaker: ThreadSpeaker,
        content: String,
        cx: &mut Context<Self>,
    ) -> ThreadMessage {
        let view_state = Self::new_message_state(&content, cx);
        ThreadMessage {
            speaker,
            content,
            view_state,
            kind: ThreadMessageKind::Text,
        }
    }

    fn build_message_from_renderable(
        message: RenderableMessage,
        cx: &mut Context<Self>,
    ) -> ThreadMessage {
        match message {
            RenderableMessage::Text { speaker, content } => {
                Self::build_thread_message(speaker, content, cx)
            }
            RenderableMessage::CommandExecution {
                header,
                status_label,
                content,
                expanded,
            } => {
                let view_state = Self::new_message_state(&content, cx);
                ThreadMessage {
                    speaker: ThreadSpeaker::Assistant,
                    content,
                    view_state,
                    kind: ThreadMessageKind::CommandExecution {
                        header,
                        status_label,
                        expanded,
                    },
                }
            }
        }
    }

    fn build_messages_from_raw(
        raw: Vec<RenderableMessage>,
        cx: &mut Context<Self>,
    ) -> Vec<ThreadMessage> {
        raw.into_iter()
            .map(|message| Self::build_message_from_renderable(message, cx))
            .collect()
    }

    fn renderable_messages_from_session(path: &Path) -> Result<Vec<RenderableMessage>, String> {
        let text = fs::read_to_string(path).map_err(|error| error.to_string())?;
        let mut messages = Vec::new();
        let mut used_chars = 0usize;
        let mut was_truncated = false;

        for line in text.lines() {
            let value: Value = match serde_json::from_str(line) {
                Ok(value) => value,
                Err(_) => continue,
            };
            if value.get("type").and_then(Value::as_str) != Some("event_msg") {
                continue;
            }

            let payload = match value.get("payload") {
                Some(payload) => payload,
                None => continue,
            };
            let speaker = match payload.get("type").and_then(Value::as_str) {
                Some("user_message") => ThreadSpeaker::User,
                Some("agent_message") => ThreadSpeaker::Assistant,
                _ => continue,
            };
            let Some(message) = payload.get("message").and_then(Value::as_str) else {
                continue;
            };
            Self::push_renderable_item_with_budget(
                &mut messages,
                RenderableMessage::Text {
                    speaker,
                    content: message.to_string(),
                },
                &mut used_chars,
                &mut was_truncated,
            );

            if was_truncated {
                break;
            }
        }

        if messages.is_empty() {
            return Err("no user or assistant messages found in session log".to_string());
        }

        if was_truncated {
            messages.push(RenderableMessage::Text {
                speaker: ThreadSpeaker::Assistant,
                content: "_Thread content truncated for performance._".to_string(),
            });
        }

        Ok(messages)
    }

    fn budgeted_content(
        raw_content: &str,
        used_chars: &mut usize,
        was_truncated: &mut bool,
    ) -> Option<String> {
        if *was_truncated {
            return None;
        }

        let content = raw_content.trim();
        if content.is_empty() {
            return None;
        }

        if *used_chars >= Self::MAX_RENDERED_THREAD_CHARS {
            *was_truncated = true;
            return None;
        }

        let remaining = Self::MAX_RENDERED_THREAD_CHARS - *used_chars;
        let content_len = content.chars().count();
        let content = if content_len > remaining {
            *was_truncated = true;
            format!("{}...", Self::truncate(content, remaining))
        } else {
            content.to_string()
        };

        *used_chars += content.chars().count();
        Some(content)
    }

    fn push_renderable_item_with_budget(
        messages: &mut Vec<RenderableMessage>,
        message: RenderableMessage,
        used_chars: &mut usize,
        was_truncated: &mut bool,
    ) {
        let raw_content = match &message {
            RenderableMessage::Text { content, .. } => content.as_str(),
            RenderableMessage::CommandExecution { content, .. } => content.as_str(),
        };

        let Some(content) = Self::budgeted_content(raw_content, used_chars, was_truncated) else {
            return;
        };

        let message = match message {
            RenderableMessage::Text { speaker, .. } => RenderableMessage::Text { speaker, content },
            RenderableMessage::CommandExecution {
                header,
                status_label,
                expanded,
                ..
            } => RenderableMessage::CommandExecution {
                header,
                status_label,
                content,
                expanded,
            },
        };
        messages.push(message);
    }

    fn simplify_shell_command(command: &str) -> String {
        let command = command.trim();
        let shell_wrappers = [
            ("/bin/zsh -lc '", "'"),
            ("/bin/bash -lc '", "'"),
            ("zsh -lc '", "'"),
            ("bash -lc '", "'"),
            ("/bin/zsh -lc \"", "\""),
            ("/bin/bash -lc \"", "\""),
            ("zsh -lc \"", "\""),
            ("bash -lc \"", "\""),
        ];
        for (prefix, suffix) in shell_wrappers {
            if let Some(rest) = command.strip_prefix(prefix) {
                if let Some(inner) = rest.strip_suffix(suffix) {
                    return inner.to_string();
                }
            }
        }

        command.to_string()
    }

    fn extract_display_command(item: &Value) -> Option<String> {
        if let Some(action_command) = item
            .get("commandActions")
            .and_then(Value::as_array)
            .and_then(|actions| actions.first())
            .and_then(|first| first.get("command"))
            .and_then(Value::as_str)
        {
            let action_command = action_command.trim();
            if !action_command.is_empty() {
                return Some(action_command.to_string());
            }
        }

        item.get("command")
            .and_then(Value::as_str)
            .map(Self::simplify_shell_command)
            .map(|command| command.trim().to_string())
            .filter(|command| !command.is_empty())
    }

    fn command_duration_label(duration_ms: Option<i64>) -> Option<String> {
        let duration_ms = duration_ms?;
        if duration_ms <= 0 {
            return None;
        }

        if duration_ms < 1_000 {
            return Some(format!("for {duration_ms}ms"));
        }

        let seconds = (duration_ms + 500) / 1_000;
        Some(format!("for {seconds}s"))
    }

    fn command_status_label(status: &str, exit_code: Option<i64>) -> String {
        match (status, exit_code) {
            ("completed", Some(0)) => "Success".to_string(),
            ("completed", Some(code)) => format!("Exit {code}"),
            ("completed", None) => "Completed".to_string(),
            ("inProgress", _) => "Running".to_string(),
            ("failed", Some(code)) => format!("Failed ({code})"),
            ("failed", None) => "Failed".to_string(),
            ("declined", _) => "Declined".to_string(),
            _ => status.to_string(),
        }
    }

    fn format_command_execution_item(item: &Value) -> Option<RenderableMessage> {
        let command = Self::extract_display_command(item)?;
        let status = item
            .get("status")
            .and_then(Value::as_str)
            .unwrap_or("unknown");
        let cwd = item
            .get("cwd")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .trim();
        let exit_code = item.get("exitCode").and_then(Value::as_i64);
        let duration_ms = item.get("durationMs").and_then(Value::as_i64);
        let output = item
            .get("aggregatedOutput")
            .and_then(Value::as_str)
            .unwrap_or_default();
        let output = output.trim_end();

        let header = if let Some(duration_label) = Self::command_duration_label(duration_ms) {
            format!("Ran command {duration_label}")
        } else {
            "Ran command".to_string()
        };
        let status_label = Self::command_status_label(status, exit_code);

        let mut body = format!("```bash\n$ {command}");
        if !output.is_empty() {
            body.push('\n');
            body.push_str(output);
        }
        body.push_str("\n```");

        let mut metadata = Vec::new();
        if !cwd.is_empty() {
            metadata.push(format!("cwd `{cwd}`"));
        }
        if let Some(code) = exit_code {
            metadata.push(format!("exit `{code}`"));
        }
        if !metadata.is_empty() {
            body.push_str("\n\n");
            body.push_str(&metadata.join(" | "));
        }

        Some(RenderableMessage::CommandExecution {
            header,
            status_label,
            content: body,
            expanded: true,
        })
    }

    fn format_web_search_item(item: &Value) -> Option<String> {
        let mut query = item
            .get("query")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .trim()
            .to_string();

        let action = item.get("action");
        let action_type = action
            .and_then(|value| value.get("type"))
            .and_then(Value::as_str)
            .unwrap_or("unknown");

        let action_query = action
            .and_then(|value| value.get("query"))
            .and_then(Value::as_str)
            .map(str::trim)
            .filter(|value| !value.is_empty());
        if query.is_empty() {
            if let Some(action_query) = action_query {
                query = action_query.to_string();
            }
        }
        if query.is_empty() {
            if let Some(queries) = action
                .and_then(|value| value.get("queries"))
                .and_then(Value::as_array)
            {
                if let Some(first_query) = queries
                    .iter()
                    .filter_map(Value::as_str)
                    .map(str::trim)
                    .find(|value| !value.is_empty())
                {
                    query = first_query.to_string();
                }
            }
        }

        let url = action
            .and_then(|value| value.get("url"))
            .and_then(Value::as_str)
            .map(str::trim)
            .filter(|value| !value.is_empty());

        if query.is_empty() && url.is_none() {
            return None;
        }

        let mut lines = vec!["**Web search**".to_string()];
        if !query.is_empty() {
            lines.push(format!("query: `{query}`"));
        }
        if let Some(url) = url {
            lines.push(format!("opened: {url}"));
        }
        lines.push(format!("action: `{action_type}`"));
        Some(lines.join("\n"))
    }

    fn renderable_message_from_tool_item(item: &Value) -> Option<RenderableMessage> {
        match item.get("type").and_then(Value::as_str) {
            Some("commandExecution") => Self::format_command_execution_item(item),
            Some("webSearch") => {
                Self::format_web_search_item(item).map(|content| RenderableMessage::Text {
                    speaker: ThreadSpeaker::Assistant,
                    content,
                })
            }
            _ => None,
        }
    }

    fn renderable_messages_from_thread_response(
        payload: &Value,
    ) -> Result<Vec<RenderableMessage>, String> {
        let turns = payload
            .get("thread")
            .and_then(|thread| thread.get("turns"))
            .and_then(Value::as_array)
            .ok_or_else(|| "thread response missing thread.turns".to_string())?;

        let mut messages = Vec::new();
        let mut used_chars = 0usize;
        let mut was_truncated = false;

        for turn in turns {
            let Some(items) = turn.get("items").and_then(Value::as_array) else {
                continue;
            };
            for item in items {
                match item.get("type").and_then(Value::as_str) {
                    Some("userMessage") => {
                        let Some(content_blocks) = item.get("content").and_then(Value::as_array)
                        else {
                            continue;
                        };
                        let text_parts = content_blocks
                            .iter()
                            .filter(|block| {
                                block.get("type").and_then(Value::as_str) == Some("text")
                            })
                            .filter_map(|block| block.get("text").and_then(Value::as_str))
                            .map(str::trim)
                            .filter(|text| !text.is_empty())
                            .collect::<Vec<_>>();
                        if text_parts.is_empty() {
                            continue;
                        }
                        let content = text_parts.join("\n\n");
                        Self::push_renderable_item_with_budget(
                            &mut messages,
                            RenderableMessage::Text {
                                speaker: ThreadSpeaker::User,
                                content,
                            },
                            &mut used_chars,
                            &mut was_truncated,
                        );
                    }
                    Some("agentMessage") => {
                        let Some(text) = item.get("text").and_then(Value::as_str) else {
                            continue;
                        };
                        Self::push_renderable_item_with_budget(
                            &mut messages,
                            RenderableMessage::Text {
                                speaker: ThreadSpeaker::Assistant,
                                content: text.to_string(),
                            },
                            &mut used_chars,
                            &mut was_truncated,
                        );
                    }
                    Some("commandExecution") | Some("webSearch") => {
                        let Some(tool_message) = Self::renderable_message_from_tool_item(item)
                        else {
                            continue;
                        };
                        Self::push_renderable_item_with_budget(
                            &mut messages,
                            tool_message,
                            &mut used_chars,
                            &mut was_truncated,
                        );
                    }
                    _ => {}
                }

                if was_truncated {
                    break;
                }
            }

            if was_truncated {
                break;
            }
        }

        if messages.is_empty() {
            return Err(
                "thread response returned no user, assistant, or tool messages".to_string(),
            );
        }

        if was_truncated {
            messages.push(RenderableMessage::Text {
                speaker: ThreadSpeaker::Assistant,
                content: "_Thread content truncated for performance._".to_string(),
            });
        }

        Ok(messages)
    }

    fn maybe_fetch_selected_thread_from_api(&mut self, cx: &mut Context<Self>) {
        let Some(server) = self._app_server.clone() else {
            return;
        };
        let Some(selected_thread_id) = self.selected_thread_id.clone() else {
            return;
        };
        if self.loading_selected_thread {
            return;
        }
        if self.selected_thread_loaded_from_api_id.as_deref() == Some(selected_thread_id.as_str()) {
            return;
        }

        self.loading_selected_thread = true;
        let thread_content_cache_path = self.thread_content_cache_path.clone();
        let thread_resume_content_cache_path = self.thread_resume_content_cache_path.clone();
        cx.notify();

        cx.spawn(async move |view, cx| {
            let response = server
                .call(
                    RequestMethod::ThreadResume,
                    json!({
                        "threadId": selected_thread_id.clone()
                    }),
                )
                .await;

            let _ = view.update(cx, |view, cx| {
                view.loading_selected_thread = false;

                if view.selected_thread_id.as_deref() != Some(selected_thread_id.as_str()) {
                    cx.notify();
                    return;
                }

                match response {
                    Ok(payload) => {
                        let _ =
                            Self::write_thread_content_cache(&thread_content_cache_path, &payload);
                        let _ = Self::write_thread_content_cache(
                            &thread_resume_content_cache_path,
                            &payload,
                        );
                        match Self::renderable_messages_from_thread_response(&payload) {
                            Ok(raw) => {
                                view.selected_thread_messages =
                                    Self::build_messages_from_raw(raw, cx);
                                view.selected_thread_error = None;
                                view.selected_thread_loaded_from_api_id =
                                    Some(selected_thread_id.clone());
                            }
                            Err(error) => {
                                view.selected_thread_error = Some(format!(
                                    "failed to parse thread/resume response: {error}"
                                ));
                                view.selected_thread_loaded_from_api_id =
                                    Some(selected_thread_id.clone());
                            }
                        }
                    }
                    Err(error) => {
                        view.selected_thread_error = Some(format!("thread/resume failed: {error}"));
                        let raw_error = json!({
                            "error": Self::app_server_error_as_json(&error),
                            "request": {
                                "method": "thread/resume",
                                "params": {
                                    "threadId": selected_thread_id
                                }
                            }
                        });
                        let _ = Self::write_thread_content_cache(
                            &thread_content_cache_path,
                            &raw_error,
                        );
                        let _ = Self::write_thread_content_cache(
                            &thread_resume_content_cache_path,
                            &raw_error,
                        );
                    }
                }

                cx.notify();
            });
        })
        .detach();
    }

    fn refresh_selected_thread(&mut self, cx: &mut Context<Self>) {
        let selected =
            Self::preferred_thread(&self.threads, &self.cwd, self.selected_thread_id.as_deref());

        let Some(row) = selected else {
            self.selected_thread_id = None;
            self.selected_thread_title = None;
            self.selected_thread_messages.clear();
            self.selected_thread_loaded_from_api_id = None;
            self.loading_selected_thread = false;
            self.selected_thread_error = None;
            self.streaming_message_ix = None;
            return;
        };

        self.selected_thread_id = Some(row.id.clone());
        self.selected_thread_title = Some(row.title.clone());
        self.selected_thread_loaded_from_api_id = None;
        self.loading_selected_thread = false;
        self.streaming_message_ix = None;

        let Some(path) = row.path.as_deref() else {
            self.selected_thread_messages.clear();
            self.selected_thread_error = Some("thread log path is unavailable".to_string());
            return;
        };

        match Self::renderable_messages_from_session(Path::new(path)) {
            Ok(raw) => {
                self.selected_thread_messages = Self::build_messages_from_raw(raw, cx);
                self.selected_thread_error = None;
            }
            Err(error) => {
                self.selected_thread_messages.clear();
                self.selected_thread_error = Some(format!("failed to load session: {error}"));
            }
        }
    }

    fn select_thread_by_id(&mut self, thread_id: &str, cx: &mut Context<Self>) {
        self.selected_thread_id = Some(thread_id.to_string());
        self.refresh_selected_thread(cx);
    }

    fn maybe_load_threads(&mut self, cx: &mut Context<Self>) {
        if self.started {
            return;
        }

        self.started = true;
        self.loading_threads = true;
        let cwd = self.cwd.clone();
        let cache_path = self.cache_path.clone();
        let raw_cache_path = self.raw_cache_path.clone();
        if let Err(error) = Self::write_threads_cache(&self.cache_path, &self.cwd, &self.threads) {
            self.thread_error = Some(format!("cache write failed: {error}"));
        }
        cx.notify();

        cx.spawn(async move |view, cx| {
            let connection = AppServer::connect().await;

            match connection {
                Ok(server) => {
                    let response = server
                        .call(RequestMethod::ThreadList, json!({ "limit": 50 }))
                        .await;
                    let _ = view.update(cx, |view, cx| {
                        view.loading_threads = false;
                        view._app_server = Some(Arc::clone(&server));
                        view.start_event_pump(Arc::clone(&server), cx);
                        match response {
                            Ok(payload) => match Self::parse_thread_rows(payload.clone(), &cwd) {
                                Ok(rows) => {
                                    view.thread_error = None;
                                    view.threads = rows;
                                    view.refresh_selected_thread(cx);
                                    view.maybe_fetch_selected_thread_from_api(cx);
                                    if let Err(error) =
                                        Self::write_threads_cache(&cache_path, &cwd, &view.threads)
                                    {
                                        view.thread_error =
                                            Some(format!("cache write failed: {error}"));
                                    }
                                    if let Err(error) = Self::write_raw_threads_cache(
                                        &raw_cache_path,
                                        &cwd,
                                        &payload,
                                    ) {
                                        view.thread_error =
                                            Some(format!("raw cache write failed: {error}"));
                                    }
                                }
                                Err(error) => {
                                    view.thread_error = Some(error);
                                }
                            },
                            Err(error) => {
                                view.thread_error = Some(error.to_string());
                            }
                        }
                        cx.notify();
                    });
                }
                Err(error) => {
                    let _ = view.update(cx, |view, cx| {
                        view.loading_threads = false;
                        view.thread_error = Some(error.to_string());
                        cx.notify();
                    });
                }
            }
        })
        .detach();
    }

    fn start_event_pump(&mut self, server: Arc<AppServer>, cx: &mut Context<Self>) {
        let event_rx = server.subscribe();
        cx.spawn(async move |view, cx| {
            while let Ok(notification) = event_rx.recv().await {
                let should_continue = view
                    .update(cx, |view, cx| {
                        view.handle_notification(&notification, cx);
                        cx.notify();
                        true
                    })
                    .unwrap_or(false);
                if !should_continue {
                    break;
                }
            }
        })
        .detach();
    }

    fn handle_notification(&mut self, notification: &ServerNotification, cx: &mut Context<Self>) {
        let notification_thread_id = notification.params.get("threadId").and_then(Value::as_str);
        if let Some(notification_thread_id) = notification_thread_id {
            if self.selected_thread_id.as_deref() != Some(notification_thread_id) {
                return;
            }
        }

        match notification.method.as_str() {
            "item/agentMessage/delta" => {
                if let Some(delta) = notification.params.get("delta").and_then(Value::as_str) {
                    let ix = match self.streaming_message_ix {
                        Some(ix) => ix,
                        None => {
                            let msg = Self::build_thread_message(
                                ThreadSpeaker::Assistant,
                                String::new(),
                                cx,
                            );
                            self.selected_thread_messages.push(msg);
                            let ix = self.selected_thread_messages.len() - 1;
                            self.streaming_message_ix = Some(ix);
                            ix
                        }
                    };

                    let msg = &mut self.selected_thread_messages[ix];
                    msg.content.push_str(delta);
                    msg.view_state.update(cx, |state, cx| {
                        state.push_str(delta, cx);
                    });
                    self.scroll_handle.scroll_to_bottom();
                }
            }
            "item/completed" => {
                if let Some(item) = notification.params.get("item") {
                    if let Some(tool_message) = Self::renderable_message_from_tool_item(item) {
                        self.selected_thread_messages
                            .push(Self::build_message_from_renderable(tool_message, cx));
                        self.scroll_handle.scroll_to_bottom();
                    }
                }
            }
            "turn/completed" => {
                self.sending_message = false;
                self.streaming_message_ix = None;
            }
            _ => {}
        }
    }

    fn send_message(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        let text = self.input_state.read(cx).value().to_string();
        let text = text.trim().to_string();
        if text.is_empty() || self.sending_message {
            return;
        }

        let Some(server) = self._app_server.clone() else {
            self.selected_thread_error = Some("Not connected to app-server".to_string());
            cx.notify();
            return;
        };

        self.input_state.update(cx, |state, cx| {
            state.set_value("", window, cx);
        });

        self.selected_thread_messages
            .push(Self::build_thread_message(
                ThreadSpeaker::User,
                text.clone(),
                cx,
            ));
        self.sending_message = true;
        self.streaming_message_ix = None;
        cx.notify();

        let thread_id = self.selected_thread_id.clone();
        let cwd = self.cwd.clone();

        cx.spawn(async move |view, cx| {
            let thread_id = if let Some(id) = thread_id {
                id
            } else {
                let start_result = server
                    .call(RequestMethod::ThreadStart, json!({ "cwd": cwd }))
                    .await;
                match start_result {
                    Ok(payload) => {
                        let id = payload
                            .get("thread")
                            .and_then(|t| t.get("id"))
                            .and_then(Value::as_str)
                            .unwrap_or_default()
                            .to_string();
                        if id.is_empty() {
                            let _ = view.update(cx, |view, cx| {
                                view.sending_message = false;
                                view.selected_thread_error =
                                    Some("thread/start returned no thread id".to_string());
                                cx.notify();
                            });
                            return;
                        }
                        let _ = view.update(cx, |view, cx| {
                            view.selected_thread_id = Some(id.clone());
                            view.selected_thread_title = Some("New thread".to_string());
                            cx.notify();
                        });
                        id
                    }
                    Err(error) => {
                        let _ = view.update(cx, |view, cx| {
                            view.sending_message = false;
                            view.selected_thread_error =
                                Some(format!("thread/start failed: {error}"));
                            cx.notify();
                        });
                        return;
                    }
                }
            };

            let result = server
                .call(
                    RequestMethod::TurnStart,
                    json!({
                        "threadId": thread_id,
                        "input": [{ "type": "text", "text": text }],
                        "cwd": cwd,
                    }),
                )
                .await;

            let _ = view.update(cx, |view, cx| {
                match result {
                    Ok(_payload) => {
                        view.selected_thread_error = None;
                    }
                    Err(error) => {
                        view.sending_message = false;
                        view.selected_thread_error = Some(format!("turn/start failed: {error}"));
                    }
                }
                cx.notify();
            });
        })
        .detach();
    }
}

impl Render for AppShell {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        self.maybe_load_threads(cx);
        self.maybe_fetch_selected_thread_from_api(cx);

        let workspace_groups = Self::group_threads_by_workspace(&self.threads, &self.cwd);
        let this = cx.entity().downgrade();
        let selected_id = self.selected_thread_id.clone();
        let can_send = !self.input_state.read(cx).value().trim().is_empty()
            && !self.sending_message
            && self._app_server.is_some();

        let thread_workspace_items = if self.loading_threads && self.threads.is_empty() {
            vec![
                SidebarMenuItem::new("Loading threads...")
                    .icon(IconName::LoaderCircle)
                    .disable(true),
            ]
        } else if self.threads.is_empty() {
            if let Some(error) = &self.thread_error {
                vec![
                    SidebarMenuItem::new(Self::truncate(&format!("Failed to load: {error}"), 40))
                        .icon(IconName::TriangleAlert)
                        .disable(true),
                ]
            } else {
                vec![
                    SidebarMenuItem::new("No threads yet")
                        .icon(IconName::Inbox)
                        .disable(true),
                ]
            }
        } else {
            let mut items = Vec::new();
            for group in &workspace_groups {
                let show_all = self.expanded_workspace_threads.contains(&group.cwd);
                let visible_count = if show_all {
                    group.threads.len()
                } else {
                    group.threads.len().min(Self::MAX_THREADS_PER_WORKSPACE)
                };
                let hidden_count = group.threads.len().saturating_sub(visible_count);

                let mut children = group
                    .threads
                    .iter()
                    .take(visible_count)
                    .map(|row| {
                        let is_active = selected_id.as_deref() == Some(row.id.as_str());
                        let mut item = SidebarMenuItem::new(row.title.clone())
                            .icon(IconName::Bot)
                            .active(is_active)
                            .on_click({
                                let this = this.clone();
                                let thread_id = row.id.clone();
                                move |_, _, cx| {
                                    let _ = this.update(cx, |view, cx| {
                                        view.select_thread_by_id(&thread_id, cx);
                                        cx.notify();
                                    });
                                }
                            });
                        if let Some(updated_at) = row.updated_at {
                            item = item.suffix(move |_, _| {
                                div().text_xs().child(Self::relative_time(updated_at))
                            });
                        }
                        item
                    })
                    .collect::<Vec<_>>();

                if hidden_count > 0 {
                    children.push(
                        SidebarMenuItem::new(format!("Show more ({hidden_count})"))
                            .icon(IconName::ChevronDown)
                            .on_click({
                                let this = this.clone();
                                let show_more_target = group.cwd.clone();
                                move |_, _, cx| {
                                    let _ = this.update(cx, |view, cx| {
                                        view.expanded_workspace_threads
                                            .insert(show_more_target.clone());
                                        cx.notify();
                                    });
                                }
                            }),
                    );
                } else if show_all && group.threads.len() > Self::MAX_THREADS_PER_WORKSPACE {
                    children.push(
                        SidebarMenuItem::new("Show less")
                            .icon(IconName::ChevronUp)
                            .on_click({
                                let this = this.clone();
                                let show_less_target = group.cwd.clone();
                                move |_, _, cx| {
                                    let _ = this.update(cx, |view, cx| {
                                        view.expanded_workspace_threads.remove(&show_less_target);
                                        cx.notify();
                                    });
                                }
                            }),
                    );
                }

                items.push(
                    SidebarMenuItem::new(group.name.clone())
                        .icon(if group.cwd == self.cwd {
                            IconName::FolderOpen
                        } else {
                            IconName::Folder
                        })
                        .active(group.cwd == self.cwd)
                        .default_open(group.cwd == self.cwd)
                        .suffix({
                            let count = group.threads.len().to_string();
                            move |_, _| div().text_xs().child(count.clone())
                        })
                        .children(children),
                );
            }
            items
        };

        let app_theme = cx.global::<AppTheme>();
        let sidebar_bg = app_theme.mantle;
        let divider_color = app_theme.surface0;
        let content_bg = app_theme.base;
        let text_color = app_theme.text;
        let subtext_color = app_theme.subtext0;
        let overlay_color = app_theme.overlay1;
        let surface0 = app_theme.surface0;
        let surface1 = app_theme.surface1;
        let mantle = app_theme.mantle;
        let red_color = app_theme.red;
        let blue_color = app_theme.blue;
        let crust = app_theme.crust;
        let font_sans: gpui::SharedString = app_theme.font_sans.clone();
        let font_mono: gpui::SharedString = app_theme.font_mono.clone();

        div()
            .size_full()
            .font_family(font_sans)
            .bg(sidebar_bg)
            .flex()
            .flex_row()
            .child(
                Sidebar::new("sidebar")
                    .w(px(300.))
                    .collapsed(self.sidebar_collapsed)
                    .header(
                        SidebarHeader::new().child(
                            div()
                                .w_full()
                                .pt(px(6.))
                                .flex()
                                .items_center()
                                .justify_between()
                                .child(
                                    div()
                                        .flex()
                                        .items_center()
                                        .gap(px(8.))
                                        .when(!self.sidebar_collapsed, |this| {
                                            this.child("Agent Hub")
                                        }),
                                )
                                .child(
                                    SidebarToggleButton::new()
                                        .collapsed(self.sidebar_collapsed)
                                        .on_click({
                                            let this = this.clone();
                                            move |_, _, cx| {
                                                let _ = this.update(cx, |view, cx| {
                                                    view.sidebar_collapsed =
                                                        !view.sidebar_collapsed;
                                                    cx.notify();
                                                });
                                            }
                                        }),
                                ),
                        ),
                    )
                    .child(
                        SidebarGroup::new("Main").child(
                            SidebarMenu::new()
                                .child(SidebarMenuItem::new("New thread").icon(IconName::Plus))
                                .child(SidebarMenuItem::new("Automations").icon(IconName::Bell))
                                .child(SidebarMenuItem::new("Skills").icon(IconName::BookOpen)),
                        ),
                    )
                    .child(
                        SidebarGroup::new("Current").child(
                            SidebarMenu::new().child(
                                SidebarMenuItem::new("Implement kanban coding age...")
                                    .icon(IconName::SquareTerminal)
                                    .active(true)
                                    .suffix(|_, _| div().text_xs().child("2d")),
                            ),
                        ),
                    )
                    .child(
                        SidebarGroup::new("Threads")
                            .child(SidebarMenu::new().children(thread_workspace_items)),
                    )
                    .footer(
                        SidebarFooter::new().child(
                            div()
                                .w_full()
                                .flex()
                                .items_center()
                                .gap(px(8.))
                                .child(Icon::new(IconName::Settings))
                                .when(!self.sidebar_collapsed, |this| this.child("Settings")),
                        ),
                    ),
            )
            .child(div().w(px(1.)).h_full().bg(divider_color))
            .child(
                div()
                    .flex_1()
                    .h_full()
                    .bg(content_bg)
                    .flex()
                    .flex_col()
                    .child(
                        div()
                            .w_full()
                            .h(px(44.))
                            .px(px(16.))
                            .flex()
                            .items_center()
                            .text_sm()
                            .text_color(text_color)
                            .child(
                                self.selected_thread_title
                                    .clone()
                                    .unwrap_or_else(|| "Thread".to_string()),
                            ),
                    )
                    .child(div().w_full().h(px(1.)).bg(divider_color))
                    .child(
                        div()
                            .id("chat-scroll")
                            .flex_1()
                            .min_h_0()
                            .overflow_y_scroll()
                            .track_scroll(&self.scroll_handle)
                            .px(px(20.))
                            .py(px(16.))
                            .child({
                                div().w_full().flex().justify_center().child(
                                    div().w_full().max_w(px(920.)).py(px(18.)).child(if !self
                                        .selected_thread_messages
                                        .is_empty()
                                    {
                                        let chat_rows = self
                                            .selected_thread_messages
                                            .iter()
                                            .enumerate()
                                            .map(|(message_ix, message)| {
                                                match message.speaker {
                                                    ThreadSpeaker::User => div()
                                                        .w_full()
                                                        .mb(px(14.))
                                                        .flex()
                                                        .justify_end()
                                                        .child(
                                                            div()
                                                                .max_w(px(700.))
                                                                .bg(surface0)
                                                                .rounded(px(14.))
                                                                .px(px(14.))
                                                                .py(px(10.))
                                                                .text_color(text_color)
                                                                .child(
                                                                    TextView::new(
                                                                        &message.view_state,
                                                                    )
                                                                    .text_color(text_color)
                                                                    .selectable(true),
                                                                ),
                                                        )
                                                        .into_any_element(),
                                                    ThreadSpeaker::Assistant => {
                                                        match &message.kind {
                                                            ThreadMessageKind::Text => div()
                                                                .w_full()
                                                                .mb(px(16.))
                                                                .flex()
                                                                .justify_start()
                                                                .child(
                                                                    div()
                                                                        .max_w(px(860.))
                                                                        .text_color(text_color)
                                                                        .child(
                                                                            TextView::new(
                                                                                &message.view_state,
                                                                            )
                                                                            .style(
                                                                                TextViewStyle::default()
                                                                                    .code_block(
                                                                                        gpui::StyleRefinement::default()
                                                                                            .bg(mantle)
                                                                                            .text_color(
                                                                                                text_color,
                                                                                            )
                                                                                            .border_1()
                                                                                            .border_color(
                                                                                                surface1,
                                                                                            )
                                                                                            .rounded(px(6.))
                                                                                            .px(px(12.))
                                                                                            .py(px(10.)),
                                                                                    ),
                                                                            )
                                                                            .text_color(text_color)
                                                                            .selectable(true),
                                                                        ),
                                                                )
                                                                .into_any_element(),
                                                            ThreadMessageKind::CommandExecution {
                                                                header,
                                                                status_label,
                                                                expanded,
                                                            } => {
                                                                let command_row_accordion_id =
                                                                    format!(
                                                                        "command-row-{message_ix}"
                                                                    );
                                                                let header = header.clone();
                                                                let status_label = status_label.clone();
                                                                let expanded = *expanded;
                                                                div()
                                                                    .w_full()
                                                                    .mb(px(16.))
                                                                    .flex()
                                                                    .justify_start()
                                                                    .child(
                                                                        div()
                                                                            .w_full()
                                                                            .max_w(px(860.))
                                                                            .child(
                                                                                gpui_component::accordion::Accordion::new(
                                                                                    command_row_accordion_id,
                                                                                )
                                                                                .item(|this| {
                                                                                    this
                                                                                        .open(expanded)
                                                                                        .title(
                                                                                            format!(
                                                                                                "{header} · {status_label}"
                                                                                            ),
                                                                                        )
                                                                                        .child(
                                                                                            TextView::new(
                                                                                                &message.view_state,
                                                                                            )
                                                                                            .style(
                                                                                                TextViewStyle::default()
                                                                                                    .code_block(
                                                                                                        gpui::StyleRefinement::default()
                                                                                                            .bg(surface0)
                                                                                                            .text_color(
                                                                                                                text_color,
                                                                                                            )
                                                                                                            .rounded(px(6.))
                                                                                                            .px(px(12.))
                                                                                                            .py(px(10.)),
                                                                                                    ),
                                                                                            )
                                                                                            .text_color(text_color)
                                                                                            .selectable(true),
                                                                                        )
                                                                                })
                                                                                .on_toggle_click(
                                                                                    cx.listener(
                                                                                        move |view, open_ixs: &[usize], _, cx| {
                                                                                            if let Some(message) = view
                                                                                                .selected_thread_messages
                                                                                                .get_mut(message_ix)
                                                                                            {
                                                                                                if let ThreadMessageKind::CommandExecution {
                                                                                                    expanded,
                                                                                                    ..
                                                                                                } = &mut message.kind
                                                                                                {
                                                                                                    *expanded = open_ixs
                                                                                                        .contains(&0);
                                                                                                    cx.notify();
                                                                                                }
                                                                                            }
                                                                                        },
                                                                                    ),
                                                                                ),
                                                                            ),
                                                                    )
                                                                    .into_any_element()
                                                            }
                                                        }
                                                    }
                                                }
                                            })
                                            .collect::<Vec<_>>();
                                        div().w_full().children(chat_rows).into_any_element()
                                    } else if let Some(error) = &self.selected_thread_error {
                                        div()
                                            .text_sm()
                                            .text_color(red_color)
                                            .child(format!("Unable to render thread: {error}"))
                                            .into_any_element()
                                    } else if self.loading_threads || self.loading_selected_thread {
                                        div()
                                            .text_sm()
                                            .text_color(subtext_color)
                                            .child("Loading thread content...")
                                            .into_any_element()
                                    } else {
                                        div()
                                            .text_sm()
                                            .text_color(subtext_color)
                                            .child("Select a thread to view its content.")
                                            .into_any_element()
                                    }),
                                )
                            }),
                    )
                    .child(
                        div()
                            .w_full()
                            .px(px(24.))
                            .pt(px(10.))
                            .pb(px(12.))
                            .flex()
                            .justify_center()
                            .child(
                                div()
                                    .w_full()
                                    .max_w(px(930.))
                                    .bg(mantle)
                                    .border_1()
                                    .border_color(surface1)
                                    .rounded(px(22.))
                                    .shadow(vec![gpui::BoxShadow {
                                        color: gpui::Hsla {
                                            h: 0.,
                                            s: 0.,
                                            l: 0.,
                                            a: 0.08,
                                        },
                                        blur_radius: px(14.),
                                        spread_radius: px(0.),
                                        offset: gpui::point(px(0.), px(2.)),
                                    }])
                                    .p(px(10.))
                                    .flex()
                                    .flex_col()
                                    .gap(px(10.))
                                    .child(
                                        div().w_full().min_h(px(43.)).child(
                                            Input::new(&self.input_state)
                                                .appearance(false)
                                                .bordered(false)
                                                .focus_bordered(false)
                                                .h_full()
                                                .text_color(text_color)
                                                .disabled(
                                                    self.sending_message
                                                        || self._app_server.is_none(),
                                                ),
                                        ),
                                    )
                                    .child(
                                        div()
                                            .w_full()
                                            .flex()
                                            .items_center()
                                            .justify_between()
                                            .child(
                                                div()
                                                    .flex()
                                                    .items_center()
                                                    .gap(px(18.))
                                                    .child(
                                                        Icon::new(IconName::Plus)
                                                            .size(px(24.))
                                                            .text_color(overlay_color),
                                                    )
                                                    .child(
                                                        div()
                                                            .flex()
                                                            .items_center()
                                                            .gap(px(8.))
                                                            .text_sm()
                                                            .text_color(overlay_color)
                                                            .child("GPT-5.3-Codex")
                                                            .child(
                                                                Icon::new(IconName::ChevronDown)
                                                                    .size(px(16.))
                                                                    .text_color(overlay_color),
                                                            ),
                                                    )
                                                    .child(
                                                        div()
                                                            .flex()
                                                            .items_center()
                                                            .gap(px(8.))
                                                            .text_sm()
                                                            .text_color(overlay_color)
                                                            .child("High")
                                                            .child(
                                                                Icon::new(IconName::ChevronDown)
                                                                    .size(px(16.))
                                                                    .text_color(overlay_color),
                                                            ),
                                                    ),
                                            )
                                            .child(
                                                div()
                                                    .flex()
                                                    .items_center()
                                                    .gap(px(12.))
                                                    .child(
                                                        div()
                                                            .text_sm()
                                                            .text_color(overlay_color)
                                                            .child("0."),
                                                    )
                                                    .child(
                                                        div()
                                                            .size(px(34.))
                                                            .rounded(px(999.))
                                                            .flex()
                                                            .items_center()
                                                            .justify_center()
                                                            .bg(if can_send {
                                                                blue_color
                                                            } else {
                                                                surface1
                                                            })
                                                            .when(can_send, |this| {
                                                                this.cursor_pointer()
                                                                    .on_mouse_down(
                                                                        gpui::MouseButton::Left,
                                                                        cx.listener(
                                                                            |view, _, window, cx| {
                                                                                view.send_message(
                                                                                    window, cx,
                                                                                );
                                                                            },
                                                                        ),
                                                                    )
                                                            })
                                                            .child(
                                                                Icon::new(IconName::ArrowUp)
                                                                    .size(px(20.))
                                                                    .text_color(crust),
                                                            ),
                                                    ),
                                            ),
                                    ),
                            ),
                    ),
            )
    }
}

fn main() {
    Application::new().with_assets(Assets).run(|cx: &mut App| {
        gpui_component::init(cx);
        AppTheme::init(cx);
        let app_theme = cx.global::<AppTheme>();
        let text_color = app_theme.text;
        let blue_color = app_theme.blue;
        let theme = ComponentTheme::global_mut(cx);
        theme.foreground = text_color;
        theme.caret = text_color;
        theme.accent = hsl(210., 26., 88.);
        theme.accent_foreground = hsl(220., 18., 20.);
        theme.link = blue_color;
        theme.link_hover = blue_color;

        let bounds = Bounds::centered(None, size(px(1_520.), px(920.)), cx);

        let window_options = WindowOptions {
            window_bounds: Some(WindowBounds::Windowed(bounds)),
            titlebar: Some(gpui::TitlebarOptions {
                title: Some("Agent Hub".into()),
                appears_transparent: true,
                traffic_light_position: Some(gpui::point(px(16.), px(16.))),
            }),
            focus: true,
            show: true,
            kind: gpui::WindowKind::Normal,
            is_movable: true,
            ..Default::default()
        };

        cx.open_window(window_options, |window, cx| {
            let app_view = cx.new(|cx| AppShell::new(window, cx));
            cx.new(|cx| Root::new(app_view, window, cx))
        })
        .unwrap();

        cx.on_action(|_: &About, _cx| {});
        cx.on_action(|_: &Settings, _cx| {});

        #[cfg(target_os = "macos")]
        cx.on_action(|_: &Hide, cx| cx.hide());

        #[cfg(target_os = "macos")]
        cx.on_action(|_: &HideOthers, cx| cx.hide_other_apps());

        #[cfg(target_os = "macos")]
        cx.on_action(|_: &ShowAll, cx| cx.unhide_other_apps());

        cx.on_action(|_: &Quit, cx| cx.quit());

        cx.bind_keys([KeyBinding::new("cmd-q", Quit, None)]);
        cx.set_menus(app_menus());
        cx.activate(true);
    });
}

#[cfg(test)]
mod tests {
    use super::{AppShell, RenderableMessage, ThreadSpeaker};
    use serde_json::json;

    #[test]
    fn format_command_execution_item_includes_metadata_and_output() {
        let item = json!({
            "type": "commandExecution",
            "command": "/bin/zsh -lc 'ls -1'",
            "cwd": "/tmp/repo",
            "status": "completed",
            "exitCode": 0,
            "durationMs": 52,
            "aggregatedOutput": "Cargo.toml\nsrc\n"
        });

        let rendered = AppShell::format_command_execution_item(&item).expect("should render");
        match rendered {
            RenderableMessage::CommandExecution {
                header,
                status_label,
                content,
                expanded,
            } => {
                assert_eq!(header, "Ran command for 52ms");
                assert_eq!(status_label, "Success");
                assert!(expanded);
                assert!(content.contains("```bash"));
                assert!(content.contains("$ ls -1"));
                assert!(content.contains("Cargo.toml"));
                assert!(content.contains("cwd `/tmp/repo`"));
                assert!(content.contains("exit `0`"));
            }
            other => panic!("expected command execution renderable message, got {other:?}"),
        }
    }

    #[test]
    fn format_web_search_item_uses_action_query_and_url() {
        let item = json!({
            "type": "webSearch",
            "query": "",
            "action": {
                "type": "openPage",
                "query": "Hacker News top stories API",
                "url": "https://news.ycombinator.com/"
            }
        });

        let rendered = AppShell::format_web_search_item(&item).expect("should render");
        assert!(rendered.contains("**Web search**"));
        assert!(rendered.contains("query: `Hacker News top stories API`"));
        assert!(rendered.contains("opened: https://news.ycombinator.com/"));
        assert!(rendered.contains("action: `openPage`"));
    }

    #[test]
    fn renderable_messages_from_thread_response_includes_tool_items() {
        let payload = json!({
            "thread": {
                "turns": [
                    {
                        "items": [
                            {
                                "type": "userMessage",
                                "id": "u1",
                                "content": [{ "type": "text", "text": "run ls" }]
                            },
                            {
                                "type": "commandExecution",
                                "id": "c1",
                                "command": "ls -1",
                                "commandActions": [],
                                "cwd": "/tmp/repo",
                                "status": "completed",
                                "aggregatedOutput": "Cargo.toml\nsrc\n",
                                "durationMs": 12,
                                "exitCode": 0,
                                "processId": "123"
                            },
                            {
                                "type": "webSearch",
                                "id": "w1",
                                "query": "Hacker News top stories API",
                                "action": {
                                    "type": "search",
                                    "query": "Hacker News top stories API"
                                }
                            },
                            {
                                "type": "agentMessage",
                                "id": "a1",
                                "text": "done"
                            }
                        ]
                    }
                ]
            }
        });

        let messages = AppShell::renderable_messages_from_thread_response(&payload)
            .expect("thread response should parse");
        assert_eq!(messages.len(), 4);
        assert!(matches!(
            &messages[0],
            RenderableMessage::Text {
                speaker: ThreadSpeaker::User,
                ..
            }
        ));
        assert!(matches!(
            &messages[1],
            RenderableMessage::CommandExecution { .. }
        ));
        assert!(matches!(
            &messages[2],
            RenderableMessage::Text {
                speaker: ThreadSpeaker::Assistant,
                content,
            } if content.contains("**Web search**")
        ));
        assert!(matches!(
            &messages[3],
            RenderableMessage::Text {
                speaker: ThreadSpeaker::Assistant,
                content,
            } if content == "done"
        ));
    }
}
