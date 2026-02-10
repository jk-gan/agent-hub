mod app_assets;
mod codex;
mod diff_view;
mod sidebar;
mod theme;

use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{BufRead, BufReader, ErrorKind, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use codex::app_server::{AppServer, RequestMethod, ServerNotification};
use gpui::prelude::*;
use gpui::{
    Animation, AnimationExt as _, App, Application, Bounds, ClipboardEntry, Context, Entity,
    ExternalPaths, ImageFormat, KeyBinding, Menu, MenuItem, PathBuilder, Render, ScrollHandle,
    Subscription, Transformation, Window, WindowBounds, WindowOptions, actions, canvas, div, img,
    percentage, point, px, size,
};
use gpui_component::Root;
use gpui_component::button::{Button, ButtonVariants as _};
use gpui_component::highlighter::HighlightTheme;
use gpui_component::input::{
    Enter as InputEnter, Input, InputEvent, InputState, MoveDown as InputMoveDown,
    MoveUp as InputMoveUp, Paste as InputPaste, Position,
};
use gpui_component::menu::{DropdownMenu as _, PopupMenu, PopupMenuItem};
use gpui_component::scroll::ScrollableElement;
use gpui_component::text::{TextView, TextViewState, TextViewStyle};
use gpui_component::theme::{Theme as ComponentTheme, hsl};
use gpui_component::tooltip::Tooltip;
use gpui_component::{Disableable as _, Icon, IconName, Sizable as _};
use gpui_component_assets::Assets;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use sidebar::{
    AnySidebarItem, Sidebar, SidebarFooter, SidebarGroup, SidebarMenu, SidebarMenuItem,
    SidebarToggleButton,
};
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
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct SkillsListResponse {
    #[serde(default)]
    data: Vec<SkillsListWorkspaceItem>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct SkillsListWorkspaceItem {
    cwd: String,
    #[serde(default)]
    skills: Vec<SkillListItem>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct SkillListItem {
    name: String,
    #[serde(default)]
    description: String,
    path: String,
    #[serde(default)]
    short_description: Option<String>,
    #[serde(default)]
    scope: String,
    #[serde(default = "default_true")]
    enabled: bool,
}

const fn default_true() -> bool {
    true
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct ModelListResponse {
    #[serde(default)]
    data: Vec<ModelListItem>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct ModelListItem {
    model: String,
    display_name: String,
    #[serde(default)]
    supported_reasoning_efforts: Vec<ModelReasoningEffortItem>,
    default_reasoning_effort: Option<String>,
    #[serde(default)]
    is_default: bool,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct ModelReasoningEffortItem {
    reasoning_effort: String,
}

#[derive(Debug, Clone, Deserialize)]
struct ConfigReadResponse {
    config: ConfigReadConfig,
}

#[derive(Debug, Clone, Deserialize)]
struct ConfigReadConfig {
    model: Option<String>,
    model_reasoning_effort: Option<String>,
}

#[derive(Debug, Clone)]
struct SkillOption {
    name: String,
    description: String,
    path: String,
    scope: String,
}

#[derive(Debug, Clone)]
struct ModelOption {
    model: String,
    display_name: String,
    supported_reasoning_efforts: Vec<String>,
    default_reasoning_effort: Option<String>,
    is_default: bool,
}

#[derive(Debug, Clone)]
struct SkillPickerState {
    items: Vec<SkillOption>,
}

#[derive(Debug, Clone)]
struct FilePickerState {
    items: Vec<String>,
}

fn is_skill_name_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, '-' | '_')
}

fn skill_token_before_cursor(prefix: &str) -> Option<(usize, String)> {
    let mut start = prefix.len();
    while start > 0 {
        let (prev_ix, prev_ch) = prefix[..start].char_indices().next_back()?;
        if !is_skill_name_char(prev_ch) {
            break;
        }
        start = prev_ix;
    }

    if start == 0 {
        return None;
    }

    let (dollar_ix, dollar_ch) = prefix[..start].char_indices().next_back()?;
    if dollar_ch != '$' {
        return None;
    }

    if dollar_ix > 0
        && let Some((_, prev_char)) = prefix[..dollar_ix].char_indices().next_back()
        && (is_skill_name_char(prev_char) || prev_char == '$')
    {
        return None;
    }

    let query = prefix[start..].to_string();
    Some((dollar_ix, query))
}

fn extract_skill_mentions(text: &str) -> Vec<String> {
    let mut mentions = Vec::new();
    let mut cursor = 0usize;

    while cursor < text.len() {
        let Some(rel_ix) = text[cursor..].find('$') else {
            break;
        };
        let dollar_ix = cursor + rel_ix;

        if dollar_ix > 0
            && let Some((_, prev_char)) = text[..dollar_ix].char_indices().next_back()
            && (is_skill_name_char(prev_char) || prev_char == '$')
        {
            cursor = dollar_ix + 1;
            continue;
        }

        let mut end = dollar_ix + 1;
        while end < text.len() {
            let Some(next_char) = text[end..].chars().next() else {
                break;
            };
            if !is_skill_name_char(next_char) {
                break;
            }
            end += next_char.len_utf8();
        }

        if end > dollar_ix + 1 {
            mentions.push(text[(dollar_ix + 1)..end].to_string());
        }

        cursor = end.max(dollar_ix + 1);
    }

    mentions
}

fn is_file_trigger_prefix_char(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '"' | '\'' | '`' | '(' | '[' | '{')
}

fn file_token_before_cursor(prefix: &str) -> Option<(usize, String)> {
    let at_ix = prefix.rfind('@')?;

    if at_ix > 0
        && let Some((_, prev_char)) = prefix[..at_ix].char_indices().next_back()
        && !is_file_trigger_prefix_char(prev_char)
    {
        return None;
    }

    let query = &prefix[(at_ix + 1)..];
    if query.chars().any(char::is_whitespace) {
        return None;
    }

    Some((at_ix, query.to_string()))
}

fn input_position_for_offset(text: &str, offset: usize) -> Position {
    let clamped_offset = offset.min(text.len());
    let mut line = 0u32;
    let mut character = 0u32;
    let mut consumed = 0usize;

    for ch in text.chars() {
        if consumed >= clamped_offset {
            break;
        }

        let len = ch.len_utf8();
        if consumed + len > clamped_offset {
            break;
        }

        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += ch.len_utf16() as u32;
        }
        consumed += len;
    }

    Position::new(line, character)
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

#[derive(Debug, Clone, PartialEq)]
enum ThreadMessageKind {
    Text,
    CommandExecution {
        header: String,
        status_label: String,
        expanded: bool,
    },
    FileChange {
        status_label: String,
        file_diffs: Vec<diff_view::ParsedFileDiff>,
        expanded: bool,
    },
}

struct ThreadMessage {
    speaker: ThreadSpeaker,
    content: String,
    image_refs: Vec<String>,
    view_state: Entity<TextViewState>,
    kind: ThreadMessageKind,
}

#[derive(Debug, Clone, PartialEq)]
enum RenderableMessage {
    Text {
        speaker: ThreadSpeaker,
        content: String,
        image_refs: Vec<String>,
    },
    CommandExecution {
        header: String,
        status_label: String,
        content: String,
        expanded: bool,
    },
    FileChange {
        status_label: String,
        file_diffs: Vec<diff_view::ParsedFileDiff>,
        expanded: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
struct UserMessageContent {
    text: String,
    image_refs: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
struct ApprovalOption {
    label: String,
    detail: Option<String>,
    response_payload: Value,
}

#[derive(Debug, Clone, Default)]
struct ThreadTokenUsage {
    total_tokens: u64,
    input_tokens: u64,
    cached_input_tokens: u64,
    output_tokens: u64,
    reasoning_output_tokens: u64,
    model_context_window: u64,
}

impl ThreadTokenUsage {
    fn used_percent(&self) -> f64 {
        if self.model_context_window == 0 {
            return 0.0;
        }
        (self.total_tokens as f64 / self.model_context_window as f64 * 100.0).clamp(0.0, 100.0)
    }

    fn remaining_percent(&self) -> f64 {
        100.0 - self.used_percent()
    }

    fn format_tokens(n: u64) -> String {
        if n >= 1_000_000 {
            format!("{:.1}M", n as f64 / 1_000_000.0)
        } else if n >= 1_000 {
            format!("{}k", n / 1_000)
        } else {
            format!("{n}")
        }
    }
}

#[derive(Debug, Clone, Default)]
struct RateLimitEntry {
    used_percent: f64,
    window_duration_mins: u64,
    resets_at: u64,
}

#[derive(Debug, Clone, Default)]
struct RateLimits {
    primary: Option<RateLimitEntry>,
    secondary: Option<RateLimitEntry>,
}

#[derive(Debug, Clone, PartialEq)]
struct PendingApprovalRequest {
    request_id: Value,
    method: String,
    thread_id: Option<String>,
    title: String,
    reason: Option<String>,
    command: Option<String>,
    options: Vec<ApprovalOption>,
}

struct AppShell {
    started: bool,
    loading_auth_state: bool,
    is_authenticated: bool,
    account_email: Option<String>,
    account_plan_type: Option<String>,
    rate_limits: Option<RateLimits>,
    auth_error: Option<String>,
    login_in_progress: bool,
    logout_in_progress: bool,
    pending_login_id: Option<String>,
    login_url: Option<String>,
    loading_threads: bool,
    sidebar_collapsed: bool,
    thread_error: Option<String>,
    threads: Vec<ThreadRow>,
    known_workspace_cwds: HashSet<String>,
    expanded_workspace_groups: HashSet<String>,
    expanded_workspace_threads: HashSet<String>,
    selected_thread_id: Option<String>,
    selected_thread_title: Option<String>,
    selected_thread_messages: Vec<ThreadMessage>,
    selected_thread_loaded_from_api_id: Option<String>,
    loading_selected_thread: bool,
    selected_thread_error: Option<(String, Entity<TextViewState>)>,
    inflight_threads: HashSet<String>,
    pending_new_thread: bool,
    composing_new_thread_cwd: Option<String>,
    streaming_message_ix: Option<usize>,
    live_tool_message_ix_by_item_id: HashMap<String, usize>,
    skills_by_cwd: HashMap<String, Vec<SkillOption>>,
    loading_skills_cwds: HashSet<String>,
    active_skills_cwd: String,
    active_skills: Vec<SkillOption>,
    files_by_cwd: HashMap<String, Vec<String>>,
    loading_files_cwds: HashSet<String>,
    active_files_cwd: String,
    active_files: Vec<String>,
    available_models: Vec<ModelOption>,
    loading_model_options: bool,
    selected_model: Option<String>,
    selected_reasoning_effort: Option<String>,
    skill_picker_selected_ix: usize,
    file_picker_selected_ix: usize,
    pending_approvals: Vec<PendingApprovalRequest>,
    approval_selected_ix: usize,
    thread_token_usage: Option<ThreadTokenUsage>,
    attached_images: Vec<PathBuf>,
    skill_picker_scroll_handle: ScrollHandle,
    file_picker_scroll_handle: ScrollHandle,
    scroll_handle: ScrollHandle,
    input_state: Entity<InputState>,
    _subscriptions: Vec<Subscription>,
    _app_server: Option<Arc<AppServer>>,
}

impl AppShell {
    const MAX_THREADS_PER_WORKSPACE: usize = 10;
    const MAX_RENDERED_THREAD_CHARS: usize = 80_000;
    const MAX_FILE_OPTIONS_PER_WORKSPACE: usize = 20_000;
    const MAX_FILE_PICKER_ITEMS: usize = 500;
    const SKIPPED_FILE_PICKER_DIRS: [&'static str; 11] = [
        ".git",
        ".next",
        ".turbo",
        ".cache",
        ".idea",
        ".zed",
        ".vscode",
        "node_modules",
        "target",
        "dist",
        "build",
    ];

    fn new(window: &mut Window, cx: &mut Context<Self>) -> Self {
        let expanded_workspace_groups = HashSet::new();
        let threads = Vec::new();
        let thread_error = None;
        let known_workspace_cwds = HashSet::new();

        let input_state = cx.new(|cx| {
            InputState::new(window, cx)
                .placeholder("Ask Codex anything, @ to add files, / for commands")
                .auto_grow(1, 8)
        });

        let _subscriptions = vec![cx.subscribe_in(&input_state, window, {
            move |this: &mut Self, _, ev: &InputEvent, window, cx| match ev {
                InputEvent::Change => {
                    this.skill_picker_selected_ix = 0;
                    this.skill_picker_scroll_handle.scroll_to_item(0);
                    this.file_picker_selected_ix = 0;
                    this.file_picker_scroll_handle.scroll_to_item(0);
                }
                InputEvent::PressEnter { secondary } if !*secondary => {
                    if this.apply_selected_completion_if_open(window, cx) {
                        return;
                    }
                    this.send_message(window, cx);
                }
                _ => {}
            }
        })];

        let mut shell = Self {
            started: false,
            loading_auth_state: false,
            is_authenticated: false,
            account_email: None,
            account_plan_type: None,
            rate_limits: None,
            auth_error: None,
            login_in_progress: false,
            logout_in_progress: false,
            pending_login_id: None,
            login_url: None,
            loading_threads: false,
            sidebar_collapsed: false,
            thread_error,
            threads,
            known_workspace_cwds,
            expanded_workspace_groups,
            expanded_workspace_threads: HashSet::new(),
            selected_thread_id: None,
            selected_thread_title: None,
            selected_thread_messages: Vec::new(),
            selected_thread_loaded_from_api_id: None,
            loading_selected_thread: false,
            selected_thread_error: None,
            inflight_threads: HashSet::new(),
            pending_new_thread: false,
            composing_new_thread_cwd: None,
            streaming_message_ix: None,
            live_tool_message_ix_by_item_id: HashMap::new(),
            skills_by_cwd: HashMap::new(),
            loading_skills_cwds: HashSet::new(),
            active_skills_cwd: String::new(),
            active_skills: Vec::new(),
            files_by_cwd: HashMap::new(),
            loading_files_cwds: HashSet::new(),
            active_files_cwd: String::new(),
            active_files: Vec::new(),
            available_models: Vec::new(),
            loading_model_options: false,
            selected_model: None,
            selected_reasoning_effort: None,
            skill_picker_selected_ix: 0,
            file_picker_selected_ix: 0,
            pending_approvals: Vec::new(),
            approval_selected_ix: 0,
            thread_token_usage: None,
            attached_images: Vec::new(),
            skill_picker_scroll_handle: ScrollHandle::new(),
            file_picker_scroll_handle: ScrollHandle::new(),
            scroll_handle: ScrollHandle::new(),
            input_state,
            _subscriptions,
            _app_server: None,
        };
        shell.refresh_selected_thread(cx);
        shell
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

    fn workspace_action_id(prefix: &str, workspace_cwd: &str) -> String {
        let suffix = workspace_cwd
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '-' })
            .collect::<String>();
        if suffix.is_empty() {
            return format!("{prefix}-workspace");
        }
        format!("{prefix}-{suffix}")
    }

    fn active_composer_cwd(&self) -> String {
        if let Some(cwd) = &self.composing_new_thread_cwd {
            return cwd.clone();
        }

        self.selected_thread_id
            .as_deref()
            .and_then(|thread_id| self.threads.iter().find(|thread| thread.id == thread_id))
            .map(|thread| thread.cwd.clone())
            .filter(|cwd| !cwd.is_empty())
            .unwrap_or_default()
    }

    fn parse_model_options(payload: Value) -> Result<Vec<ModelOption>, String> {
        let response: ModelListResponse = serde_json::from_value(payload)
            .map_err(|error| format!("invalid response: {error}"))?;

        let mut options = response
            .data
            .into_iter()
            .filter_map(|item| {
                let model = item.model.trim();
                if model.is_empty() {
                    return None;
                }

                let display_name = if item.display_name.trim().is_empty() {
                    model.to_string()
                } else {
                    item.display_name
                };
                let supported_reasoning_efforts = item
                    .supported_reasoning_efforts
                    .into_iter()
                    .map(|effort| effort.reasoning_effort)
                    .filter(|effort| !effort.trim().is_empty())
                    .collect::<Vec<_>>();

                Some(ModelOption {
                    model: model.to_string(),
                    display_name: display_name.trim().to_string(),
                    supported_reasoning_efforts,
                    default_reasoning_effort: item.default_reasoning_effort,
                    is_default: item.is_default,
                })
            })
            .collect::<Vec<_>>();

        options.sort_by_key(|option| (!option.is_default, option.display_name.to_lowercase()));
        Ok(options)
    }

    fn parse_config_read(payload: Value) -> Result<ConfigReadConfig, String> {
        let response: ConfigReadResponse = serde_json::from_value(payload)
            .map_err(|error| format!("invalid response: {error}"))?;
        Ok(response.config)
    }

    fn selected_model_option(&self) -> Option<&ModelOption> {
        self.selected_model.as_deref().and_then(|selected| {
            self.available_models
                .iter()
                .find(|option| option.model == selected)
        })
    }

    fn available_reasoning_efforts(&self) -> Vec<String> {
        self.selected_model_option()
            .map(|option| option.supported_reasoning_efforts.clone())
            .unwrap_or_default()
    }

    fn normalize_selected_reasoning_effort(&mut self) {
        let reasoning_efforts = self.available_reasoning_efforts();
        if reasoning_efforts.is_empty() {
            self.selected_reasoning_effort = None;
            return;
        }

        if let Some(selected) = self.selected_reasoning_effort.as_deref()
            && reasoning_efforts.iter().any(|effort| effort == selected)
        {
            return;
        }

        if let Some(default_effort) = self
            .selected_model_option()
            .and_then(|option| option.default_reasoning_effort.as_deref())
            .filter(|effort| {
                reasoning_efforts
                    .iter()
                    .any(|candidate| candidate == *effort)
            })
        {
            self.selected_reasoning_effort = Some(default_effort.to_string());
            return;
        }

        self.selected_reasoning_effort = reasoning_efforts.first().cloned();
    }

    fn apply_model_config(
        &mut self,
        available_models: Vec<ModelOption>,
        config_model: Option<String>,
        config_reasoning_effort: Option<String>,
    ) {
        let previous_model = self.selected_model.clone();
        self.available_models = available_models;

        if self.available_models.is_empty() {
            self.selected_model = None;
            self.selected_reasoning_effort = None;
            return;
        }

        let next_model = previous_model
            .filter(|model| {
                self.available_models
                    .iter()
                    .any(|option| &option.model == model)
            })
            .or(config_model.filter(|model| {
                self.available_models
                    .iter()
                    .any(|option| option.model == model.as_str())
            }))
            .or_else(|| {
                self.available_models
                    .iter()
                    .find(|option| option.is_default)
                    .map(|option| option.model.clone())
            })
            .or_else(|| {
                self.available_models
                    .first()
                    .map(|option| option.model.clone())
            });

        self.selected_model = next_model;

        let reasoning_efforts = self.available_reasoning_efforts();
        self.selected_reasoning_effort = config_reasoning_effort.filter(|effort| {
            reasoning_efforts
                .iter()
                .any(|candidate| candidate == effort)
        });
        self.normalize_selected_reasoning_effort();
    }

    fn load_model_and_reasoning_options(&mut self, server: Arc<AppServer>, cx: &mut Context<Self>) {
        if self.loading_model_options {
            return;
        }

        self.loading_model_options = true;
        cx.notify();

        cx.spawn(async move |view, cx| {
            let model_list = server.call(RequestMethod::ModelList, json!({})).await;
            let config_read = server.call(RequestMethod::ConfigRead, json!({})).await;

            let _ = view.update(cx, |view, cx| {
                view.loading_model_options = false;

                if let (Ok(model_payload), Ok(config_payload)) = (model_list, config_read)
                    && let (Ok(models), Ok(config)) = (
                        Self::parse_model_options(model_payload),
                        Self::parse_config_read(config_payload),
                    )
                {
                    view.apply_model_config(models, config.model, config.model_reasoning_effort);
                }

                cx.notify();
            });
        })
        .detach();
    }

    fn selected_model_label(&self) -> String {
        if self.loading_model_options && self.available_models.is_empty() {
            return "Loading model...".to_string();
        }

        self.selected_model_option()
            .map(|option| option.display_name.clone())
            .unwrap_or_else(|| "Model".to_string())
    }

    fn selected_reasoning_label(&self) -> String {
        if self.loading_model_options && self.available_models.is_empty() {
            return "Loading reasoning...".to_string();
        }

        self.selected_reasoning_effort
            .as_deref()
            .map(Self::format_reasoning_effort_label)
            .unwrap_or_else(|| "Reasoning".to_string())
    }

    fn format_reasoning_effort_label(effort: &str) -> String {
        match effort {
            "xhigh" => "X-High".to_string(),
            "high" => "High".to_string(),
            "medium" => "Medium".to_string(),
            "low" => "Low".to_string(),
            other => {
                let mut chars = other.chars();
                match chars.next() {
                    Some(first) => {
                        let mut result = first.to_uppercase().to_string();
                        result.push_str(chars.as_str());
                        result
                    }
                    None => "Reasoning".to_string(),
                }
            }
        }
    }

    fn should_skip_file_picker_dir(name: &str) -> bool {
        Self::SKIPPED_FILE_PICKER_DIRS.contains(&name)
    }

    fn collect_workspace_files_with_rg(root: &Path, max_files: usize) -> Option<Vec<String>> {
        let mut child = Command::new("rg")
            .arg("--files")
            .current_dir(root)
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .ok()?;
        let stdout = child.stdout.take()?;
        let reader = BufReader::new(stdout);
        let mut files = Vec::new();
        let mut reached_limit = false;

        for line in reader.lines() {
            let Ok(line) = line else {
                break;
            };
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            files.push(trimmed.replace('\\', "/"));
            if files.len() >= max_files {
                reached_limit = true;
                break;
            }
        }

        if reached_limit {
            let _ = child.kill();
            let _ = child.wait();
        } else if !child.wait().ok()?.success() {
            return None;
        }

        files.sort_unstable_by_key(|path| path.to_ascii_lowercase());
        files.dedup();
        Some(files)
    }

    fn collect_workspace_files(cwd: &str, max_files: usize) -> Vec<String> {
        let root = PathBuf::from(cwd);

        // Avoid broad filesystem scans like "/" or "/Users".
        if root.components().count() <= 2 {
            return Vec::new();
        }

        let Ok(metadata) = fs::metadata(&root) else {
            return Vec::new();
        };
        if !metadata.is_dir() {
            return Vec::new();
        }

        if let Some(files) = Self::collect_workspace_files_with_rg(&root, max_files) {
            return files;
        }

        let mut files = Vec::new();
        let mut stack = vec![root.clone()];

        while let Some(dir) = stack.pop() {
            let read_dir = match fs::read_dir(&dir) {
                Ok(read_dir) => read_dir,
                Err(_) => continue,
            };

            for entry_result in read_dir {
                if files.len() >= max_files {
                    break;
                }

                let Ok(entry) = entry_result else {
                    continue;
                };
                let Ok(file_type) = entry.file_type() else {
                    continue;
                };

                if file_type.is_symlink() {
                    continue;
                }

                let path = entry.path();
                if file_type.is_dir() {
                    let name = entry.file_name();
                    let name = name.to_string_lossy();
                    if Self::should_skip_file_picker_dir(name.as_ref()) {
                        continue;
                    }
                    stack.push(path);
                    continue;
                }

                if !file_type.is_file() {
                    continue;
                }

                let Ok(relative) = path.strip_prefix(&root) else {
                    continue;
                };
                if relative.as_os_str().is_empty() {
                    continue;
                }
                files.push(relative.to_string_lossy().replace('\\', "/"));
            }

            if files.len() >= max_files {
                break;
            }
        }

        files.sort_unstable_by_key(|path| path.to_ascii_lowercase());
        files.dedup();
        files
    }

    fn set_active_skills_for_cwd(&mut self, cwd: &str) {
        self.active_skills_cwd = cwd.to_string();
        self.active_skills = self.skills_by_cwd.get(cwd).cloned().unwrap_or_default();
    }

    fn parse_skills_for_cwd(payload: Value, cwd: &str) -> Result<Vec<SkillOption>, String> {
        let response: SkillsListResponse = serde_json::from_value(payload)
            .map_err(|error| format!("invalid response: {error}"))?;

        let mut workspaces = response.data.into_iter();
        let first_workspace = workspaces.next();
        let workspace = first_workspace
            .map(|first| {
                if first.cwd == cwd {
                    first
                } else {
                    workspaces
                        .find(|workspace| workspace.cwd == cwd)
                        .unwrap_or(first)
                }
            })
            .ok_or_else(|| "skills/list returned no workspace entries".to_string())?;

        let mut skills = workspace
            .skills
            .into_iter()
            .filter(|skill| skill.enabled)
            .filter_map(|skill| {
                let name = skill.name.trim();
                let path = skill.path.trim();
                if name.is_empty() || path.is_empty() {
                    return None;
                }

                let description = if skill.description.trim().is_empty() {
                    skill.short_description.unwrap_or_default()
                } else {
                    skill.description
                };

                Some(SkillOption {
                    name: name.to_string(),
                    description: description.trim().to_string(),
                    path: path.to_string(),
                    scope: skill.scope.trim().to_string(),
                })
            })
            .collect::<Vec<_>>();
        skills.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(skills)
    }

    fn ensure_skills_for_cwd(&mut self, cwd: String, cx: &mut Context<Self>) {
        if cwd.is_empty() {
            return;
        }

        if self.active_skills_cwd != cwd {
            self.set_active_skills_for_cwd(&cwd);
        }

        if self.skills_by_cwd.contains_key(&cwd) || self.loading_skills_cwds.contains(&cwd) {
            return;
        }

        let Some(server) = self._app_server.clone() else {
            return;
        };

        self.loading_skills_cwds.insert(cwd.clone());
        cx.spawn(async move |view, cx| {
            let response = server
                .call(RequestMethod::SkillsList, json!({ "cwds": [cwd.clone()] }))
                .await;

            let _ = view.update(cx, |view, _cx| {
                view.loading_skills_cwds.remove(&cwd);
                if let Ok(payload) = response
                    && let Ok(skills) = Self::parse_skills_for_cwd(payload, &cwd)
                {
                    view.skills_by_cwd.insert(cwd.clone(), skills.clone());
                    if view.active_skills_cwd == cwd {
                        view.active_skills = skills;
                    }
                }
            });
        })
        .detach();
    }

    fn ensure_skills_for_active_workspace(&mut self, cx: &mut Context<Self>) {
        let cwd = self.active_composer_cwd();
        self.ensure_skills_for_cwd(cwd, cx);
    }

    fn set_active_files_for_cwd(&mut self, cwd: &str) {
        self.active_files_cwd = cwd.to_string();
        self.active_files = self.files_by_cwd.get(cwd).cloned().unwrap_or_default();
    }

    fn ensure_files_for_cwd(&mut self, cwd: String, cx: &mut Context<Self>) {
        if cwd.is_empty() {
            return;
        }

        if self.active_files_cwd != cwd {
            self.set_active_files_for_cwd(&cwd);
        }

        if self.files_by_cwd.contains_key(&cwd) || self.loading_files_cwds.contains(&cwd) {
            return;
        }

        self.loading_files_cwds.insert(cwd.clone());
        cx.spawn(async move |view, cx| {
            let files = Self::collect_workspace_files(&cwd, Self::MAX_FILE_OPTIONS_PER_WORKSPACE);

            let _ = view.update(cx, |view, _cx| {
                view.loading_files_cwds.remove(&cwd);
                view.files_by_cwd.insert(cwd.clone(), files.clone());
                if view.active_files_cwd == cwd {
                    view.active_files = files;
                }
            });
        })
        .detach();
    }

    fn ensure_files_for_active_workspace(&mut self, cx: &mut Context<Self>) {
        let cwd = self.active_composer_cwd();
        self.ensure_files_for_cwd(cwd, cx);
    }

    fn ensure_composer_options_for_cwd(&mut self, cwd: String, cx: &mut Context<Self>) {
        self.ensure_skills_for_cwd(cwd.clone(), cx);
        self.ensure_files_for_cwd(cwd, cx);
    }

    fn ensure_composer_options_for_active_workspace(&mut self, cx: &mut Context<Self>) {
        self.ensure_skills_for_active_workspace(cx);
        self.ensure_files_for_active_workspace(cx);
    }

    fn skill_input_items_for_text(&self, text: &str) -> Vec<Value> {
        let first_skill_by_name = self
            .active_skills
            .iter()
            .map(|skill| (skill.name.as_str(), skill))
            .collect::<HashMap<_, _>>();

        let mut seen = HashSet::new();
        extract_skill_mentions(text)
            .into_iter()
            .filter_map(|skill_name| {
                if !seen.insert(skill_name.clone()) {
                    return None;
                }
                let skill = first_skill_by_name.get(skill_name.as_str())?;
                Some(json!({
                    "type": "skill",
                    "name": &skill.name,
                    "path": &skill.path,
                }))
            })
            .collect()
    }

    fn current_skill_picker_state(&self, cx: &App) -> Option<SkillPickerState> {
        let (text, cursor) = {
            let input = self.input_state.read(cx);
            (input.value().to_string(), input.cursor())
        };

        let prefix = text.get(..cursor)?;
        let (_, query) = skill_token_before_cursor(prefix)?;

        let query_lower = query.to_ascii_lowercase();
        let mut filtered = self
            .active_skills
            .iter()
            .cloned()
            .filter_map(|skill| {
                let name_lower = skill.name.to_ascii_lowercase();
                if query_lower.is_empty() {
                    return Some((0u8, skill));
                }
                if name_lower.starts_with(&query_lower) {
                    return Some((0u8, skill));
                }
                if name_lower.contains(&query_lower) {
                    return Some((1u8, skill));
                }
                None
            })
            .collect::<Vec<_>>();

        filtered.sort_by(|(score_a, skill_a), (score_b, skill_b)| {
            score_a
                .cmp(score_b)
                .then_with(|| skill_a.name.cmp(&skill_b.name))
        });

        let items = filtered
            .into_iter()
            .map(|(_, skill)| skill)
            .take(60)
            .collect::<Vec<_>>();
        if items.is_empty() {
            return None;
        }

        Some(SkillPickerState { items })
    }

    fn current_file_picker_state(&self, cx: &App) -> Option<FilePickerState> {
        let (text, cursor) = {
            let input = self.input_state.read(cx);
            (input.value().to_string(), input.cursor())
        };

        let prefix = text.get(..cursor)?;
        let (_, query) = file_token_before_cursor(prefix)?;
        let query_lower = query.to_ascii_lowercase();

        let mut filtered = self
            .active_files
            .iter()
            .cloned()
            .filter_map(|path| {
                if query_lower.is_empty() {
                    return Some((0u8, path));
                }

                let path_lower = path.to_ascii_lowercase();
                let file_name_lower = path_lower.rsplit('/').next().unwrap_or(path_lower.as_str());
                if file_name_lower.starts_with(&query_lower) {
                    return Some((0u8, path));
                }
                if path_lower.starts_with(&query_lower) {
                    return Some((1u8, path));
                }
                if path_lower.contains(&query_lower) {
                    return Some((2u8, path));
                }
                None
            })
            .collect::<Vec<_>>();

        filtered.sort_by(|(score_a, path_a), (score_b, path_b)| {
            score_a
                .cmp(score_b)
                .then_with(|| path_a.len().cmp(&path_b.len()))
                .then_with(|| path_a.cmp(path_b))
        });

        let items = filtered
            .into_iter()
            .map(|(_, path)| path)
            .take(Self::MAX_FILE_PICKER_ITEMS)
            .collect::<Vec<_>>();
        if items.is_empty() {
            return None;
        }

        Some(FilePickerState { items })
    }

    fn apply_skill_completion(
        &mut self,
        skill_name: &str,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> bool {
        let (text, cursor) = {
            let input = self.input_state.read(cx);
            (input.value().to_string(), input.cursor())
        };

        let Some(prefix) = text.get(..cursor) else {
            return false;
        };
        let Some((dollar_ix, _query)) = skill_token_before_cursor(prefix) else {
            return false;
        };

        let token_start = dollar_ix + 1;
        if token_start > cursor || cursor > text.len() {
            return false;
        }

        let mut token_end = cursor;
        while token_end < text.len() {
            let Some(next_char) = text[token_end..].chars().next() else {
                break;
            };
            if !is_skill_name_char(next_char) {
                break;
            }
            token_end += next_char.len_utf8();
        }

        let before = &text[..token_start];
        let after = &text[token_end..];
        let trailing_space = if after.is_empty() { " " } else { "" };
        let replaced = format!("{before}{skill_name}{trailing_space}{after}");
        let next_cursor = before.len() + skill_name.len() + trailing_space.len();

        self.input_state.update(cx, move |state, cx| {
            state.set_value(replaced.clone(), window, cx);
            state.set_cursor_position(
                input_position_for_offset(&replaced, next_cursor),
                window,
                cx,
            );
        });
        true
    }

    fn apply_selected_skill_completion_if_open(
        &mut self,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> bool {
        let Some(skill_picker) = self.current_skill_picker_state(cx) else {
            return false;
        };
        let Some(selected) = skill_picker.items.get(
            self.skill_picker_selected_ix
                .min(skill_picker.items.len().saturating_sub(1)),
        ) else {
            return false;
        };
        self.apply_skill_completion(&selected.name, window, cx)
    }

    fn move_skill_picker_selection(&mut self, delta: i32, cx: &mut Context<Self>) -> bool {
        let Some(skill_picker) = self.current_skill_picker_state(cx) else {
            return false;
        };
        let len = skill_picker.items.len();
        if len == 0 {
            return false;
        }

        let current = self.skill_picker_selected_ix.min(len.saturating_sub(1)) as i32;
        let next = (current + delta).rem_euclid(len as i32) as usize;
        if next != self.skill_picker_selected_ix {
            self.skill_picker_selected_ix = next;
            self.skill_picker_scroll_handle.scroll_to_item(next);
            cx.notify();
        }
        true
    }

    fn apply_file_completion(
        &mut self,
        file_path: &str,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> bool {
        let (text, cursor) = {
            let input = self.input_state.read(cx);
            (input.value().to_string(), input.cursor())
        };

        let Some(prefix) = text.get(..cursor) else {
            return false;
        };
        let Some((at_ix, _query)) = file_token_before_cursor(prefix) else {
            return false;
        };

        if at_ix > cursor || cursor > text.len() {
            return false;
        }

        let mut token_end = cursor;
        while token_end < text.len() {
            let Some(next_char) = text[token_end..].chars().next() else {
                break;
            };
            if next_char.is_whitespace() {
                break;
            }
            token_end += next_char.len_utf8();
        }

        let before = &text[..at_ix];
        let after = &text[token_end..];
        let trailing_space = if after
            .chars()
            .next()
            .is_none_or(|next_char| !next_char.is_whitespace())
        {
            " "
        } else {
            ""
        };
        let replaced = format!("{before}{file_path}{trailing_space}{after}");
        let next_cursor = before.len() + file_path.len() + trailing_space.len();

        self.input_state.update(cx, move |state, cx| {
            state.set_value(replaced.clone(), window, cx);
            state.set_cursor_position(
                input_position_for_offset(&replaced, next_cursor),
                window,
                cx,
            );
        });
        true
    }

    fn apply_selected_file_completion_if_open(
        &mut self,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> bool {
        let Some(file_picker) = self.current_file_picker_state(cx) else {
            return false;
        };
        let Some(selected) = file_picker.items.get(
            self.file_picker_selected_ix
                .min(file_picker.items.len().saturating_sub(1)),
        ) else {
            return false;
        };
        self.apply_file_completion(selected, window, cx)
    }

    fn move_file_picker_selection(&mut self, delta: i32, cx: &mut Context<Self>) -> bool {
        let Some(file_picker) = self.current_file_picker_state(cx) else {
            return false;
        };
        let len = file_picker.items.len();
        if len == 0 {
            return false;
        }

        let current = self.file_picker_selected_ix.min(len.saturating_sub(1)) as i32;
        let next = (current + delta).rem_euclid(len as i32) as usize;
        if next != self.file_picker_selected_ix {
            self.file_picker_selected_ix = next;
            self.file_picker_scroll_handle.scroll_to_item(next);
            cx.notify();
        }
        true
    }

    fn apply_selected_completion_if_open(
        &mut self,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> bool {
        if self.apply_selected_skill_completion_if_open(window, cx) {
            return true;
        }
        self.apply_selected_file_completion_if_open(window, cx)
    }

    fn move_completion_picker_selection(&mut self, delta: i32, cx: &mut Context<Self>) -> bool {
        if self.current_skill_picker_state(cx).is_some() {
            return self.move_skill_picker_selection(delta, cx);
        }
        self.move_file_picker_selection(delta, cx)
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

    fn group_threads_by_workspace(
        threads: &[ThreadRow],
        known_workspace_cwds: &HashSet<String>,
    ) -> Vec<WorkspaceGroup> {
        let mut buckets: HashMap<String, Vec<ThreadRow>> = HashMap::new();
        for row in threads.iter().cloned() {
            let key = if row.cwd.is_empty() {
                "unknown".to_string()
            } else {
                row.cwd.clone()
            };
            buckets.entry(key).or_default().push(row);
        }
        for workspace_cwd in known_workspace_cwds {
            if workspace_cwd.trim().is_empty() {
                continue;
            }
            buckets.entry(workspace_cwd.clone()).or_default();
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
            b.latest_ts
                .cmp(&a.latest_ts)
                .then_with(|| a.name.cmp(&b.name))
        });
        groups
    }

    fn parse_thread_rows(value: Value) -> Result<Vec<ThreadRow>, String> {
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
                    cwd: entry.cwd.unwrap_or_default(),
                }
            })
            .collect::<Vec<_>>();

        rows.sort_by_key(|row| Reverse(row.updated_at.unwrap_or(0)));
        rows.truncate(50);

        Ok(rows)
    }

    fn preferred_thread(threads: &[ThreadRow], selected_id: Option<&str>) -> Option<ThreadRow> {
        if let Some(selected_id) = selected_id
            && let Some(row) = threads.iter().find(|row| row.id == selected_id)
        {
            return Some(row.clone());
        }

        threads.first().cloned()
    }

    fn new_message_state(text: &str, cx: &mut Context<Self>) -> Entity<TextViewState> {
        cx.new(|cx| TextViewState::markdown(text, cx))
    }

    fn set_thread_error(&mut self, message: String, cx: &mut Context<Self>) {
        let view_state = cx.new(|cx| TextViewState::markdown(&message, cx));
        self.selected_thread_error = Some((message, view_state));
    }

    fn build_thread_message(
        speaker: ThreadSpeaker,
        content: String,
        image_refs: Vec<String>,
        cx: &mut Context<Self>,
    ) -> ThreadMessage {
        let view_state = Self::new_message_state(&content, cx);
        ThreadMessage {
            speaker,
            content,
            image_refs,
            view_state,
            kind: ThreadMessageKind::Text,
        }
    }

    fn build_message_from_renderable(
        message: RenderableMessage,
        cx: &mut Context<Self>,
    ) -> ThreadMessage {
        match message {
            RenderableMessage::Text {
                speaker,
                content,
                image_refs,
            } => Self::build_thread_message(speaker, content, image_refs, cx),
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
                    image_refs: Vec::new(),
                    view_state,
                    kind: ThreadMessageKind::CommandExecution {
                        header,
                        status_label,
                        expanded,
                    },
                }
            }
            RenderableMessage::FileChange {
                status_label,
                file_diffs,
                expanded,
            } => {
                let view_state = Self::new_message_state("", cx);
                ThreadMessage {
                    speaker: ThreadSpeaker::Assistant,
                    content: String::new(),
                    image_refs: Vec::new(),
                    view_state,
                    kind: ThreadMessageKind::FileChange {
                        status_label,
                        file_diffs,
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

    fn reset_live_tool_message_state(&mut self) {
        self.live_tool_message_ix_by_item_id.clear();
    }

    fn update_thread_message_from_renderable(
        message: &mut ThreadMessage,
        renderable: RenderableMessage,
        cx: &mut Context<Self>,
    ) {
        match renderable {
            RenderableMessage::Text {
                speaker,
                content,
                image_refs,
            } => {
                message.speaker = speaker;
                message.content = content.clone();
                message.image_refs = image_refs;
                message.view_state = Self::new_message_state(&content, cx);
                message.kind = ThreadMessageKind::Text;
            }
            RenderableMessage::CommandExecution {
                header,
                status_label,
                content,
                expanded,
            } => {
                let next_expanded = match &message.kind {
                    ThreadMessageKind::CommandExecution { expanded, .. } => *expanded,
                    _ => expanded,
                };

                message.speaker = ThreadSpeaker::Assistant;
                message.content = content.clone();
                message.image_refs.clear();
                message.view_state = Self::new_message_state(&content, cx);
                message.kind = ThreadMessageKind::CommandExecution {
                    header,
                    status_label,
                    expanded: next_expanded,
                };
            }
            RenderableMessage::FileChange {
                status_label,
                file_diffs,
                expanded,
            } => {
                let next_expanded = match &message.kind {
                    ThreadMessageKind::FileChange { expanded, .. } => *expanded,
                    _ => expanded,
                };

                message.speaker = ThreadSpeaker::Assistant;
                message.content = String::new();
                message.image_refs.clear();
                message.view_state = Self::new_message_state("", cx);
                message.kind = ThreadMessageKind::FileChange {
                    status_label,
                    file_diffs,
                    expanded: next_expanded,
                };
            }
        }
    }

    fn upsert_live_tool_message(
        &mut self,
        item_id: &str,
        renderable: RenderableMessage,
        cx: &mut Context<Self>,
    ) {
        if let Some(ix) = self.live_tool_message_ix_by_item_id.get(item_id).copied()
            && let Some(existing) = self.selected_thread_messages.get_mut(ix)
        {
            Self::update_thread_message_from_renderable(existing, renderable, cx);
            self.scroll_handle.scroll_to_bottom();
            return;
        }

        self.selected_thread_messages
            .push(Self::build_message_from_renderable(renderable, cx));
        let ix = self.selected_thread_messages.len() - 1;
        self.live_tool_message_ix_by_item_id
            .insert(item_id.to_string(), ix);
        self.scroll_handle.scroll_to_bottom();
    }

    fn append_live_tool_output_delta(
        &mut self,
        item_id: &str,
        delta: &str,
        default_header: &str,
        cx: &mut Context<Self>,
    ) {
        if delta.is_empty() {
            return;
        }

        if let Some(ix) = self.live_tool_message_ix_by_item_id.get(item_id).copied()
            && let Some(existing) = self.selected_thread_messages.get_mut(ix)
        {
            if !existing.content.is_empty()
                && !existing.content.ends_with('\n')
                && !delta.starts_with('\n')
            {
                existing.content.push('\n');
            }
            existing.content.push_str(delta);
            existing.view_state = Self::new_message_state(&existing.content, cx);
            self.scroll_handle.scroll_to_bottom();
            return;
        }

        self.upsert_live_tool_message(
            item_id,
            RenderableMessage::CommandExecution {
                header: default_header.to_string(),
                status_label: "Running".to_string(),
                content: delta.to_string(),
                expanded: false,
            },
            cx,
        );
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
            RenderableMessage::FileChange { .. } => "",
        };

        if let RenderableMessage::Text {
            content,
            image_refs,
            ..
        } = &message
            && content.trim().is_empty()
            && !image_refs.is_empty()
        {
            messages.push(message);
            return;
        }

        let Some(content) = Self::budgeted_content(raw_content, used_chars, was_truncated) else {
            if matches!(&message, RenderableMessage::FileChange { .. }) {
                messages.push(message);
            }
            return;
        };

        let message = match message {
            RenderableMessage::Text {
                speaker,
                image_refs,
                ..
            } => RenderableMessage::Text {
                speaker,
                content,
                image_refs,
            },
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
            file_change @ RenderableMessage::FileChange { .. } => file_change,
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
            if let Some(rest) = command.strip_prefix(prefix)
                && let Some(inner) = rest.strip_suffix(suffix)
            {
                return inner.to_string();
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

    fn file_change_status_label(status: &str) -> String {
        match status {
            "completed" => "Completed".to_string(),
            "inProgress" => "Running".to_string(),
            "failed" => "Failed".to_string(),
            "declined" => "Declined".to_string(),
            _ => status.to_string(),
        }
    }

    fn format_file_change_item(item: &Value) -> Option<RenderableMessage> {
        let status = item
            .get("status")
            .and_then(Value::as_str)
            .unwrap_or("unknown");
        let status_label = Self::file_change_status_label(status);
        let file_diffs: Vec<diff_view::ParsedFileDiff> = item
            .get("changes")
            .and_then(Value::as_array)
            .into_iter()
            .flatten()
            .filter_map(|change| {
                let path = change
                    .get("path")
                    .and_then(Value::as_str)
                    .map(str::trim)
                    .unwrap_or_default();
                if path.is_empty() {
                    return None;
                }
                let diff_text = change
                    .get("diff")
                    .and_then(Value::as_str)
                    .map(str::trim)
                    .unwrap_or_default();
                Some(diff_view::parse_unified_diff(path.to_string(), diff_text))
            })
            .collect();

        Some(RenderableMessage::FileChange {
            status_label,
            file_diffs,
            expanded: true,
        })
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
            expanded: false,
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
        if query.is_empty()
            && let Some(action_query) = action_query
        {
            query = action_query.to_string();
        }
        if query.is_empty()
            && let Some(queries) = action
                .and_then(|value| value.get("queries"))
                .and_then(Value::as_array)
            && let Some(first_query) = queries
                .iter()
                .filter_map(Value::as_str)
                .map(str::trim)
                .find(|value| !value.is_empty())
        {
            query = first_query.to_string();
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

    fn image_ref_for_message(path_or_url: &str) -> String {
        let trimmed = path_or_url.trim();
        if trimmed.is_empty() {
            return String::new();
        }

        if trimmed.starts_with("data:")
            || trimmed.starts_with("http://")
            || trimmed.starts_with("https://")
        {
            return trimmed.to_string();
        }

        if let Some(path) = trimmed.strip_prefix("file://") {
            if path.is_empty() {
                return String::new();
            }
            return path.to_string();
        }

        if Path::new(trimmed).is_absolute() {
            return trimmed.to_string();
        }

        trimmed.to_string()
    }

    fn image_ref_from_content_block(block: &Value) -> Option<String> {
        let image_ref = block
            .get("path")
            .and_then(Value::as_str)
            .or_else(|| block.get("url").and_then(Value::as_str))
            .map(str::trim)
            .filter(|value| !value.is_empty())?;
        let image_ref = Self::image_ref_for_message(image_ref);
        if image_ref.is_empty() {
            None
        } else {
            Some(image_ref)
        }
    }

    fn render_user_message_content(content_blocks: &[Value]) -> Option<UserMessageContent> {
        let mut text_parts = Vec::new();
        let mut image_refs = Vec::new();
        content_blocks
            .iter()
            .for_each(|block| match block.get("type").and_then(Value::as_str) {
                Some("text") => block
                    .get("text")
                    .and_then(Value::as_str)
                    .map(str::trim)
                    .filter(|text| !text.is_empty())
                    .map(ToOwned::to_owned)
                    .into_iter()
                    .for_each(|text| text_parts.push(text)),
                Some("localImage") | Some("image") => {
                    if let Some(image_ref) = Self::image_ref_from_content_block(block) {
                        image_refs.push(image_ref);
                    }
                }
                _ => {}
            });

        let text = text_parts.join("\n\n");
        if text.is_empty() && image_refs.is_empty() {
            None
        } else {
            Some(UserMessageContent { text, image_refs })
        }
    }

    fn renderable_message_from_tool_item(item: &Value) -> Option<RenderableMessage> {
        match item.get("type").and_then(Value::as_str) {
            Some("commandExecution") => Self::format_command_execution_item(item),
            Some("fileChange") => Self::format_file_change_item(item),
            Some("webSearch") => {
                Self::format_web_search_item(item).map(|content| RenderableMessage::Text {
                    speaker: ThreadSpeaker::Assistant,
                    content,
                    image_refs: Vec::new(),
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
                        let Some(user_content) = Self::render_user_message_content(content_blocks)
                        else {
                            continue;
                        };
                        Self::push_renderable_item_with_budget(
                            &mut messages,
                            RenderableMessage::Text {
                                speaker: ThreadSpeaker::User,
                                content: user_content.text,
                                image_refs: user_content.image_refs,
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
                                image_refs: Vec::new(),
                            },
                            &mut used_chars,
                            &mut was_truncated,
                        );
                    }
                    Some("commandExecution") | Some("fileChange") | Some("webSearch") => {
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

        if was_truncated {
            messages.push(RenderableMessage::Text {
                speaker: ThreadSpeaker::Assistant,
                content: "_Thread content truncated for performance._".to_string(),
                image_refs: Vec::new(),
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
                    Ok(payload) => match Self::renderable_messages_from_thread_response(&payload) {
                        Ok(raw) => {
                            view.selected_thread_messages = Self::build_messages_from_raw(raw, cx);
                            view.selected_thread_error = None;
                            view.selected_thread_loaded_from_api_id =
                                Some(selected_thread_id.clone());
                        }
                        Err(error) => {
                            view.set_thread_error(
                                format!("failed to parse thread/resume response: {error}"),
                                cx,
                            );
                            view.selected_thread_loaded_from_api_id =
                                Some(selected_thread_id.clone());
                        }
                    },
                    Err(error) => {
                        view.set_thread_error(format!("thread/resume failed: {error}"), cx);
                    }
                }

                cx.notify();
            });
        })
        .detach();
    }

    fn refresh_selected_thread(&mut self, cx: &mut Context<Self>) {
        if self.composing_new_thread_cwd.is_some() && self.selected_thread_id.is_none() {
            self.selected_thread_title = Some("New thread".to_string());
            self.ensure_composer_options_for_active_workspace(cx);
            return;
        }

        if let Some(selected_thread_id) = self.selected_thread_id.as_deref()
            && !self.threads.iter().any(|row| row.id == selected_thread_id)
            && self.inflight_threads.contains(selected_thread_id)
        {
            // Keep the in-flight selection pinned while thread/list catches up with newly
            // created threads. Falling back here can jump the UI to an older thread.
            self.selected_thread_title
                .get_or_insert_with(|| "New thread".to_string());
            self.ensure_composer_options_for_active_workspace(cx);
            return;
        }

        let selected = Self::preferred_thread(&self.threads, self.selected_thread_id.as_deref());

        let Some(row) = selected else {
            self.selected_thread_id = None;
            self.selected_thread_title = None;
            self.selected_thread_messages.clear();
            self.reset_live_tool_message_state();
            self.selected_thread_loaded_from_api_id = None;
            self.loading_selected_thread = false;
            self.selected_thread_error = None;
            self.streaming_message_ix = None;
            self.thread_token_usage = None;
            self.ensure_composer_options_for_active_workspace(cx);
            return;
        };

        self.selected_thread_id = Some(row.id.clone());
        self.selected_thread_title = Some(row.title.clone());
        self.selected_thread_loaded_from_api_id = None;
        self.loading_selected_thread = false;
        self.streaming_message_ix = None;
        self.thread_token_usage = None;

        self.selected_thread_messages.clear();
        self.reset_live_tool_message_state();
        self.selected_thread_error = None;
        self.ensure_composer_options_for_active_workspace(cx);
    }

    fn select_thread_by_id(&mut self, thread_id: &str, cx: &mut Context<Self>) {
        self.composing_new_thread_cwd = None;
        self.selected_thread_id = Some(thread_id.to_string());
        self.refresh_selected_thread(cx);
    }

    fn selected_thread_is_busy(&self) -> bool {
        match self.selected_thread_id.as_deref() {
            Some(thread_id) => self.inflight_threads.contains(thread_id),
            None => self.pending_new_thread,
        }
    }

    fn has_pending_approval(&self) -> bool {
        !self.pending_approvals.is_empty()
    }

    fn set_approval_selection(&mut self, option_ix: usize) -> bool {
        let Some(approval) = self.pending_approvals.first() else {
            return false;
        };
        if approval.options.is_empty() {
            return false;
        }
        let clamped = option_ix.min(approval.options.len().saturating_sub(1));
        if self.approval_selected_ix == clamped {
            return false;
        }
        self.approval_selected_ix = clamped;
        true
    }

    fn move_approval_selection(&mut self, delta: isize) -> bool {
        let Some(approval) = self.pending_approvals.first() else {
            return false;
        };
        if approval.options.is_empty() {
            return false;
        }

        let len = approval.options.len() as isize;
        let current = self.approval_selected_ix.min(approval.options.len() - 1) as isize;
        let mut next = current + delta;
        if next < 0 {
            next = len - 1;
        } else if next >= len {
            next = 0;
        }
        self.approval_selected_ix = next as usize;
        true
    }

    fn submit_selected_approval(&mut self, cx: &mut Context<Self>) -> bool {
        let Some(server) = self._app_server.clone() else {
            self.set_thread_error("Not connected to app-server".to_string(), cx);
            return false;
        };

        let Some(approval) = self.pending_approvals.first().cloned() else {
            return false;
        };

        if approval.options.is_empty() {
            return false;
        }

        let selected_ix = self.approval_selected_ix.min(approval.options.len() - 1);
        let response_payload = approval.options[selected_ix].response_payload.clone();
        let request_id = approval.request_id.clone();

        self.pending_approvals.remove(0);
        self.approval_selected_ix = 0;

        cx.spawn(async move |view, cx| {
            let result = server.respond(request_id, response_payload).await;
            let _ = view.update(cx, |view, cx| {
                if let Err(error) = result {
                    view.set_thread_error(format!("failed to submit approval: {error}"), cx);
                }
                cx.notify();
            });
        })
        .detach();

        true
    }

    fn queue_approval_request(&mut self, notification: &ServerNotification) -> bool {
        let Some(approval) = Self::pending_approval_from_server_request(notification) else {
            return false;
        };

        let already_queued = self
            .pending_approvals
            .iter()
            .any(|pending| pending.request_id == approval.request_id);
        if already_queued {
            return false;
        }

        self.pending_approvals.push(approval);
        self.approval_selected_ix = 0;
        true
    }

    fn pending_approval_from_server_request(
        notification: &ServerNotification,
    ) -> Option<PendingApprovalRequest> {
        let request_id = notification.request_id.clone()?;
        let thread_id = notification
            .params
            .get("threadId")
            .and_then(Value::as_str)
            .map(str::to_owned);
        let reason = notification
            .params
            .get("reason")
            .and_then(Value::as_str)
            .map(str::trim)
            .map(str::to_owned)
            .filter(|value| !value.is_empty());

        match notification.method.as_str() {
            "item/commandExecution/requestApproval" => {
                let command = notification
                    .params
                    .get("command")
                    .and_then(Value::as_str)
                    .map(Self::simplify_shell_command)
                    .map(|value| value.trim().to_string())
                    .filter(|value| !value.is_empty());

                let proposed_execpolicy_amendment = notification
                    .params
                    .get("proposedExecpolicyAmendment")
                    .and_then(Value::as_array)
                    .map(|parts| {
                        parts
                            .iter()
                            .filter_map(Value::as_str)
                            .map(str::to_owned)
                            .collect::<Vec<_>>()
                    })
                    .filter(|parts| !parts.is_empty());

                let mut options = vec![ApprovalOption {
                    label: "Yes".to_string(),
                    detail: None,
                    response_payload: json!({ "decision": "accept" }),
                }];

                if let Some(amendment) = proposed_execpolicy_amendment.clone() {
                    options.push(ApprovalOption {
                        label: "Yes, and don't ask again for commands that start with".to_string(),
                        detail: Some(amendment.join(" ")),
                        response_payload: json!({
                            "decision": {
                                "acceptWithExecpolicyAmendment": {
                                    "execpolicy_amendment": amendment
                                }
                            }
                        }),
                    });
                } else {
                    options.push(ApprovalOption {
                        label: "Yes, and don't ask again for this session".to_string(),
                        detail: command.clone(),
                        response_payload: json!({ "decision": "acceptForSession" }),
                    });
                }

                options.push(ApprovalOption {
                    label: "No".to_string(),
                    detail: None,
                    response_payload: json!({ "decision": "decline" }),
                });

                Some(PendingApprovalRequest {
                    request_id,
                    method: notification.method.clone(),
                    thread_id,
                    title: "Do you want to run this command?".to_string(),
                    reason,
                    command,
                    options,
                })
            }
            "item/fileChange/requestApproval" => {
                let mut options = vec![ApprovalOption {
                    label: "Yes".to_string(),
                    detail: None,
                    response_payload: json!({ "decision": "accept" }),
                }];
                options.push(ApprovalOption {
                    label: "Yes, and don't ask again for this session".to_string(),
                    detail: None,
                    response_payload: json!({ "decision": "acceptForSession" }),
                });
                options.push(ApprovalOption {
                    label: "No".to_string(),
                    detail: None,
                    response_payload: json!({ "decision": "decline" }),
                });

                Some(PendingApprovalRequest {
                    request_id,
                    method: notification.method.clone(),
                    thread_id,
                    title: "Do you want to apply these file changes?".to_string(),
                    reason,
                    command: None,
                    options,
                })
            }
            _ => None,
        }
    }

    fn render_pending_approval_panel(
        &self,
        text_color: gpui::Hsla,
        subtext_color: gpui::Hsla,
        overlay_color: gpui::Hsla,
        surface0: gpui::Hsla,
        surface1: gpui::Hsla,
        mantle: gpui::Hsla,
        font_mono: gpui::SharedString,
        cx: &mut Context<Self>,
    ) -> Option<gpui::AnyElement> {
        fn compact_line(text: &str, max_chars: usize) -> String {
            let normalized = text
                .split_whitespace()
                .filter(|segment| !segment.is_empty())
                .collect::<Vec<_>>()
                .join(" ");
            let mut compact = String::new();
            for (ix, ch) in normalized.chars().enumerate() {
                if ix >= max_chars {
                    compact.push_str("...");
                    break;
                }
                compact.push(ch);
            }
            compact
        }

        let approval = self.pending_approvals.first()?.clone();
        let selected_ix = self
            .approval_selected_ix
            .min(approval.options.len().saturating_sub(1));
        let command_preview = approval
            .command
            .as_deref()
            .map(|command| compact_line(command, 120));
        let reason_preview = approval
            .reason
            .as_deref()
            .map(|reason| compact_line(reason, 120));
        let queue_hint = if self.pending_approvals.len() > 1 {
            Some(format!("{} pending", self.pending_approvals.len()))
        } else {
            None
        };
        let submit_bg = hsl(220., 12., 8.);
        let submit_text = hsl(0., 0., 98.);

        let mut header_row = div()
            .w_full()
            .flex()
            .items_center()
            .justify_between()
            .gap(px(8.))
            .child(
                div()
                    .flex()
                    .items_center()
                    .gap(px(8.))
                    .child(
                        div()
                            .rounded(px(999.))
                            .px(px(5.))
                            .py(px(1.))
                            .text_xs()
                            .font_family(font_mono.clone())
                            .text_color(overlay_color)
                            .bg(surface0)
                            .child("APPROVAL"),
                    )
                    .child(
                        div()
                            .text_sm()
                            .font_weight(gpui::FontWeight::MEDIUM)
                            .text_color(text_color)
                            .child(approval.title),
                    ),
            );
        if let Some(hint) = queue_hint {
            header_row = header_row.child(div().text_xs().text_color(overlay_color).child(hint));
        }

        let option_rows = approval
            .options
            .iter()
            .enumerate()
            .map(|(option_ix, option)| {
                let is_selected = option_ix == selected_ix;
                let label_preview = compact_line(&option.label, 92);

                let row = div()
                    .w_full()
                    .rounded(px(8.))
                    .border_1()
                    .border_color(if is_selected { surface1 } else { surface0 })
                    .bg(if is_selected { surface0 } else { mantle })
                    .px(px(9.))
                    .py(px(6.))
                    .flex()
                    .items_center()
                    .gap(px(6.))
                    .cursor_pointer()
                    .on_mouse_down(
                        gpui::MouseButton::Left,
                        cx.listener(move |view, _, _, cx| {
                            if view.set_approval_selection(option_ix) {
                                cx.notify();
                            }
                        }),
                    )
                    .child(
                        div()
                            .text_xs()
                            .font_family(font_mono.clone())
                            .text_color(overlay_color)
                            .child(format!("{}.", option_ix + 1)),
                    )
                    .child(
                        div()
                            .flex_1()
                            .min_w_0()
                            .text_sm()
                            .font_weight(gpui::FontWeight::MEDIUM)
                            .text_color(text_color)
                            .overflow_hidden()
                            .text_ellipsis()
                            .whitespace_nowrap()
                            .child(label_preview),
                    );
                row
            });

        let footer = div()
            .w_full()
            .pt(px(1.))
            .flex()
            .items_center()
            .justify_end()
            .child(
                div()
                    .h(px(30.))
                    .rounded(px(999.))
                    .px(px(11.))
                    .flex()
                    .items_center()
                    .gap(px(4.))
                    .bg(submit_bg)
                    .cursor_pointer()
                    .on_mouse_down(
                        gpui::MouseButton::Left,
                        cx.listener(|view, _, _, cx| {
                            if view.submit_selected_approval(cx) {
                                cx.notify();
                            }
                        }),
                    )
                    .child(
                        div()
                            .text_xs()
                            .font_weight(gpui::FontWeight::MEDIUM)
                            .text_color(submit_text)
                            .child("Submit"),
                    )
                    .child(
                        Icon::new(IconName::ArrowRight)
                            .size(px(12.))
                            .text_color(submit_text),
                    ),
            );

        let mut panel = div()
            .w_full()
            .rounded(px(12.))
            .border_1()
            .border_color(surface1)
            .bg(mantle)
            .px(px(9.))
            .py(px(8.))
            .flex()
            .flex_col()
            .gap(px(6.))
            .child(header_row);
        if let Some(command) = command_preview {
            panel = panel.child(
                div()
                    .w_full()
                    .rounded(px(8.))
                    .border_1()
                    .border_color(surface1)
                    .bg(surface0)
                    .px(px(8.))
                    .py(px(5.))
                    .font_family(font_mono.clone())
                    .text_color(subtext_color)
                    .text_xs()
                    .overflow_hidden()
                    .text_ellipsis()
                    .whitespace_nowrap()
                    .child(command),
            );
        }
        if let Some(reason) = reason_preview {
            panel = panel.child(
                div()
                    .w_full()
                    .text_xs()
                    .text_color(subtext_color)
                    .overflow_hidden()
                    .text_ellipsis()
                    .whitespace_nowrap()
                    .child(reason),
            );
        }
        panel = panel.children(option_rows).child(footer);

        Some(panel.into_any_element())
    }

    fn start_new_thread_in_workspace(&mut self, workspace_cwd: String, cx: &mut Context<Self>) {
        self.expanded_workspace_groups.insert(workspace_cwd.clone());
        self.known_workspace_cwds.insert(workspace_cwd.clone());
        self.selected_thread_error = None;
        self.selected_thread_id = None;
        self.selected_thread_messages.clear();
        self.reset_live_tool_message_state();
        self.pending_new_thread = false;
        self.composing_new_thread_cwd = Some(workspace_cwd.clone());
        self.selected_thread_title = Some("New thread".to_string());
        self.selected_thread_loaded_from_api_id = None;
        self.loading_selected_thread = false;
        self.streaming_message_ix = None;
        self.thread_token_usage = None;
        self.ensure_composer_options_for_cwd(workspace_cwd.clone(), cx);
        cx.notify();
    }

    fn add_workspace(&mut self, workspace_cwd: String, cx: &mut Context<Self>) {
        let workspace_cwd = workspace_cwd.trim().to_string();
        if workspace_cwd.is_empty() {
            return;
        }

        self.known_workspace_cwds.insert(workspace_cwd.clone());
        self.expanded_workspace_groups.insert(workspace_cwd.clone());
        self.ensure_composer_options_for_cwd(workspace_cwd, cx);
        cx.notify();
    }

    fn prompt_add_workspace(&mut self, cx: &mut Context<Self>) {
        let paths_rx = cx.prompt_for_paths(gpui::PathPromptOptions {
            files: false,
            directories: true,
            multiple: false,
            prompt: None,
        });
        cx.spawn(async move |view, cx| {
            if let Ok(Ok(Some(selected))) = paths_rx.await
                && let Some(path) = selected.first()
            {
                let workspace_cwd = path.to_string_lossy().to_string();
                let _ = view.update(cx, |view, cx| {
                    view.add_workspace(workspace_cwd.clone(), cx);
                });
            }
        })
        .detach();
    }

    fn clear_workspace_state(&mut self) {
        self.threads.clear();
        self.known_workspace_cwds.clear();
        self.expanded_workspace_groups.clear();
        self.expanded_workspace_threads.clear();
        self.selected_thread_id = None;
        self.selected_thread_title = None;
        self.selected_thread_messages.clear();
        self.reset_live_tool_message_state();
        self.selected_thread_loaded_from_api_id = None;
        self.loading_selected_thread = false;
        self.selected_thread_error = None;
        self.inflight_threads.clear();
        self.pending_new_thread = false;
        self.composing_new_thread_cwd = None;
        self.streaming_message_ix = None;
        self.thread_token_usage = None;
        self.loading_threads = false;
        self.thread_error = None;
        self.skills_by_cwd.clear();
        self.loading_skills_cwds.clear();
        self.active_skills_cwd = String::new();
        self.active_skills.clear();
        self.files_by_cwd.clear();
        self.loading_files_cwds.clear();
        self.active_files_cwd = String::new();
        self.active_files.clear();
        self.available_models.clear();
        self.loading_model_options = false;
        self.selected_model = None;
        self.selected_reasoning_effort = None;
        self.skill_picker_selected_ix = 0;
        self.file_picker_selected_ix = 0;
        self.pending_approvals.clear();
        self.approval_selected_ix = 0;
    }

    fn apply_account_read_payload(&mut self, payload: &Value) {
        let account = payload.get("account").and_then(Value::as_object);
        let requires_openai_auth = payload
            .get("requiresOpenaiAuth")
            .and_then(Value::as_bool)
            .unwrap_or(true);

        self.account_email = account
            .and_then(|value| value.get("email"))
            .and_then(Value::as_str)
            .map(str::to_owned);
        self.account_plan_type = account
            .and_then(|value| value.get("planType"))
            .and_then(Value::as_str)
            .map(str::to_owned);

        self.is_authenticated = account.is_some() || !requires_openai_auth;
        if !self.is_authenticated {
            self.clear_workspace_state();
        }
    }

    fn load_threads_from_server(&mut self, server: Arc<AppServer>, cx: &mut Context<Self>) {
        if self.loading_threads {
            return;
        }

        self.loading_threads = true;
        cx.notify();

        cx.spawn(async move |view, cx| {
            let response = server
                .call(RequestMethod::ThreadList, json!({ "limit": 50 }))
                .await;
            let _ = view.update(cx, |view, cx| {
                view.loading_threads = false;
                match response {
                    Ok(payload) => match Self::parse_thread_rows(payload.clone()) {
                        Ok(rows) => {
                            view.thread_error = None;
                            view.threads = rows;
                            for thread in &view.threads {
                                if !thread.cwd.trim().is_empty() {
                                    view.known_workspace_cwds.insert(thread.cwd.clone());
                                }
                            }
                            view.refresh_selected_thread(cx);
                            view.maybe_fetch_selected_thread_from_api(cx);
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
        })
        .detach();
    }

    fn refresh_account_state(
        &mut self,
        server: Arc<AppServer>,
        load_threads_if_authenticated: bool,
        cx: &mut Context<Self>,
    ) {
        self.loading_auth_state = true;
        cx.notify();

        cx.spawn(async move |view, cx| {
            let response = server.call(RequestMethod::AccountRead, json!({})).await;
            let _ = view.update(cx, |view, cx| {
                view.loading_auth_state = false;
                match response {
                    Ok(payload) => {
                        view.auth_error = None;
                        view.apply_account_read_payload(&payload);
                        if view.is_authenticated {
                            if load_threads_if_authenticated {
                                view.load_threads_from_server(Arc::clone(&server), cx);
                            }
                            view.load_model_and_reasoning_options(Arc::clone(&server), cx);
                            view.fetch_rate_limits(Arc::clone(&server), cx);
                        } else {
                            view.ensure_composer_options_for_active_workspace(cx);
                        }
                    }
                    Err(error) => {
                        view.auth_error = Some(format!("account/read failed: {error}"));
                    }
                }
                cx.notify();
            });
        })
        .detach();
    }

    fn parse_rate_limit_entry(value: &Value) -> Option<RateLimitEntry> {
        Some(RateLimitEntry {
            used_percent: value.get("usedPercent").and_then(Value::as_f64)?,
            window_duration_mins: value.get("windowDurationMins").and_then(Value::as_u64)?,
            resets_at: value.get("resetsAt").and_then(Value::as_u64)?,
        })
    }

    fn parse_rate_limits(payload: &Value) -> RateLimits {
        let limits = payload.get("rateLimits").unwrap_or(payload);
        RateLimits {
            primary: limits.get("primary").and_then(Self::parse_rate_limit_entry),
            secondary: limits
                .get("secondary")
                .and_then(Self::parse_rate_limit_entry),
        }
    }

    fn parse_thread_token_usage(params: &Value) -> Option<ThreadTokenUsage> {
        let usage = params.get("tokenUsage")?;
        let total = usage.get("last")?;
        let context_window = usage
            .get("modelContextWindow")
            .and_then(Value::as_u64)
            .unwrap_or(0);
        if context_window == 0 {
            return None;
        }
        Some(ThreadTokenUsage {
            total_tokens: total
                .get("totalTokens")
                .and_then(Value::as_u64)
                .unwrap_or(0),
            input_tokens: total
                .get("inputTokens")
                .and_then(Value::as_u64)
                .unwrap_or(0),
            cached_input_tokens: total
                .get("cachedInputTokens")
                .and_then(Value::as_u64)
                .unwrap_or(0),
            output_tokens: total
                .get("outputTokens")
                .and_then(Value::as_u64)
                .unwrap_or(0),
            reasoning_output_tokens: total
                .get("reasoningOutputTokens")
                .and_then(Value::as_u64)
                .unwrap_or(0),
            model_context_window: context_window,
        })
    }

    fn fetch_rate_limits(&mut self, server: Arc<AppServer>, cx: &mut Context<Self>) {
        cx.spawn(async move |view, cx| {
            let response = server
                .call(RequestMethod::AccountRateLimitsRead, json!({}))
                .await;
            let _ = view.update(cx, |view, cx| {
                if let Ok(payload) = response {
                    view.rate_limits = Some(Self::parse_rate_limits(&payload));
                    cx.notify();
                }
            });
        })
        .detach();
    }

    fn start_login(&mut self, cx: &mut Context<Self>) {
        if self.login_in_progress {
            return;
        }

        let Some(server) = self._app_server.clone() else {
            self.auth_error = Some("Not connected to app-server".to_string());
            cx.notify();
            return;
        };

        self.login_in_progress = true;
        self.auth_error = None;
        self.login_url = None;
        cx.notify();

        cx.spawn(async move |view, cx| {
            let response = server
                .call(
                    RequestMethod::AccountLoginStart,
                    json!({ "type": "chatgpt" }),
                )
                .await;
            let _ = view.update(cx, |view, cx| {
                view.login_in_progress = false;
                match response {
                    Ok(payload) => {
                        let login_id = payload
                            .get("loginId")
                            .and_then(Value::as_str)
                            .map(str::to_owned);
                        let auth_url = payload
                            .get("authUrl")
                            .and_then(Value::as_str)
                            .map(str::to_owned);

                        match (login_id, auth_url) {
                            (Some(login_id), Some(auth_url)) => {
                                view.pending_login_id = Some(login_id);
                                view.login_url = Some(auth_url.clone());
                                Self::open_auth_url(&auth_url);
                            }
                            _ => {
                                view.auth_error = Some(
                                    "account/login/start returned an invalid response".to_string(),
                                );
                            }
                        }
                    }
                    Err(error) => {
                        view.auth_error = Some(format!("account/login/start failed: {error}"));
                    }
                }
                cx.notify();
            });
        })
        .detach();
    }

    fn start_logout(&mut self, cx: &mut Context<Self>) {
        if self.logout_in_progress {
            return;
        }

        let Some(server) = self._app_server.clone() else {
            self.auth_error = Some("Not connected to app-server".to_string());
            cx.notify();
            return;
        };

        self.logout_in_progress = true;
        self.auth_error = None;
        cx.notify();

        cx.spawn(async move |view, cx| {
            let response = server.call(RequestMethod::AccountLogout, json!({})).await;
            let _ = view.update(cx, |view, cx| {
                view.logout_in_progress = false;
                match response {
                    Ok(_) => {
                        view.login_url = None;
                        view.pending_login_id = None;
                        view.login_in_progress = false;
                        view.auth_error = None;
                        if let Some(server) = view._app_server.clone() {
                            view.refresh_account_state(server, false, cx);
                        }
                    }
                    Err(error) => {
                        view.auth_error = Some(format!("account/logout failed: {error}"));
                    }
                }
                cx.notify();
            });
        })
        .detach();
    }

    fn open_auth_url(url: &str) {
        let trimmed = url.trim();
        let allow_http_local = trimmed
            .strip_prefix("http://")
            .and_then(|rest| rest.split(['/', '?', '#']).next())
            .is_some_and(|host| {
                host.eq_ignore_ascii_case("localhost")
                    || host.starts_with("localhost:")
                    || host == "127.0.0.1"
                    || host.starts_with("127.0.0.1:")
            });
        if !trimmed.starts_with("https://") && !allow_http_local {
            eprintln!("refusing to open non-http(s) auth url: {trimmed}");
            return;
        }

        #[cfg(target_os = "macos")]
        let _ = Command::new("open").arg(trimmed).spawn();

        #[cfg(target_os = "linux")]
        let _ = Command::new("xdg-open").arg(trimmed).spawn();

        #[cfg(target_os = "windows")]
        let _ = Command::new("cmd")
            .args(["/C", "start", "", trimmed])
            .spawn();
    }

    fn write_clipboard_image_to_temp(bytes: &[u8], ext: &str) -> Option<PathBuf> {
        let ts = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let pid = std::process::id();

        for attempt in 0..8 {
            let tmp_path =
                std::env::temp_dir().join(format!("agent-hub-paste-{pid}-{ts}-{attempt}.{ext}"));
            let mut file = match fs::OpenOptions::new()
                .write(true)
                .create_new(true)
                .open(&tmp_path)
            {
                Ok(file) => file,
                Err(error) => {
                    if error.kind() == ErrorKind::AlreadyExists {
                        continue;
                    }
                    return None;
                }
            };

            if file.write_all(bytes).is_ok() {
                return Some(tmp_path);
            }

            let _ = fs::remove_file(&tmp_path);
        }

        None
    }

    fn maybe_load_threads(&mut self, cx: &mut Context<Self>) {
        if self.started {
            return;
        }

        self.started = true;
        self.loading_auth_state = true;
        cx.notify();

        cx.spawn(async move |view, cx| {
            let connection = AppServer::connect().await;

            match connection {
                Ok(server) => {
                    let _ = view.update(cx, |view, cx| {
                        view._app_server = Some(Arc::clone(&server));
                        view.start_event_pump(Arc::clone(&server), cx);
                        view.refresh_account_state(Arc::clone(&server), true, cx);
                        cx.notify();
                    });
                }
                Err(error) => {
                    let _ = view.update(cx, |view, cx| {
                        view.loading_auth_state = false;
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
        if notification.request_id.is_some() {
            if matches!(
                notification.method.as_str(),
                "item/commandExecution/requestApproval" | "item/fileChange/requestApproval"
            ) {
                self.queue_approval_request(notification);
            } else {
                self.set_thread_error(
                    format!("Unsupported server request: {}", notification.method),
                    cx,
                );
            }
            return;
        }

        let notification_thread_id = notification.params.get("threadId").and_then(Value::as_str);
        let applies_to_selected_thread = match notification_thread_id {
            Some(thread_id) => self.selected_thread_id.as_deref() == Some(thread_id),
            None => true,
        };

        match notification.method.as_str() {
            "item/agentMessage/delta" => {
                if !applies_to_selected_thread {
                    return;
                }
                if let Some(delta) = notification.params.get("delta").and_then(Value::as_str) {
                    let ix = match self.streaming_message_ix {
                        Some(ix) => ix,
                        None => {
                            let msg = Self::build_thread_message(
                                ThreadSpeaker::Assistant,
                                String::new(),
                                Vec::new(),
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
            "item/started" => {
                if !applies_to_selected_thread {
                    return;
                }
                self.streaming_message_ix = None;
                if let Some(item) = notification.params.get("item")
                    && let Some(item_id) = item.get("id").and_then(Value::as_str)
                    && let Some(tool_message) = Self::renderable_message_from_tool_item(item)
                {
                    self.upsert_live_tool_message(item_id, tool_message, cx);
                }
            }
            "item/commandExecution/outputDelta" => {
                if !applies_to_selected_thread {
                    return;
                }
                self.streaming_message_ix = None;
                if let Some(item_id) = notification.params.get("itemId").and_then(Value::as_str)
                    && let Some(delta) = notification.params.get("delta").and_then(Value::as_str)
                {
                    self.append_live_tool_output_delta(item_id, delta, "Ran command", cx);
                }
            }
            "item/fileChange/outputDelta" => {
                if !applies_to_selected_thread {
                    return;
                }
                self.streaming_message_ix = None;
                if let Some(item_id) = notification.params.get("itemId").and_then(Value::as_str)
                    && let Some(delta) = notification.params.get("delta").and_then(Value::as_str)
                {
                    self.append_live_tool_output_delta(item_id, delta, "File changes", cx);
                }
            }
            "item/completed" => {
                if !applies_to_selected_thread {
                    return;
                }
                self.streaming_message_ix = None;
                if let Some(item) = notification.params.get("item")
                    && let Some(tool_message) = Self::renderable_message_from_tool_item(item)
                    && let Some(item_id) = item.get("id").and_then(Value::as_str)
                {
                    self.upsert_live_tool_message(item_id, tool_message, cx);
                }
            }
            "turn/completed" => {
                if let Some(thread_id) = notification_thread_id {
                    self.inflight_threads.remove(thread_id);
                } else if let Some(selected_thread_id) = self.selected_thread_id.as_deref() {
                    self.inflight_threads.remove(selected_thread_id);
                } else {
                    self.pending_new_thread = false;
                }

                if applies_to_selected_thread {
                    self.streaming_message_ix = None;

                    if let Some(turn) = notification.params.get("turn")
                        && let Some(error) = turn.get("error")
                        && let Some(message) = error.get("message").and_then(Value::as_str)
                    {
                        self.set_thread_error(message.to_string(), cx);
                    }
                }
            }
            "error" => {
                if applies_to_selected_thread {
                    if let Some(error) = notification.params.get("error")
                        && let Some(message) = error.get("message").and_then(Value::as_str)
                    {
                        self.set_thread_error(message.to_string(), cx);
                    }
                }
            }
            "account/login/completed" => {
                let success = notification
                    .params
                    .get("success")
                    .and_then(Value::as_bool)
                    .unwrap_or(false);
                let login_id = notification
                    .params
                    .get("loginId")
                    .and_then(Value::as_str)
                    .map(str::to_owned);
                if let (Some(expected_login_id), Some(received_login_id)) =
                    (self.pending_login_id.as_deref(), login_id.as_deref())
                    && expected_login_id != received_login_id
                {
                    return;
                }

                self.pending_login_id = None;
                self.login_in_progress = false;

                if success {
                    self.auth_error = None;
                    if let Some(server) = self._app_server.clone() {
                        self.refresh_account_state(server, true, cx);
                    }
                } else {
                    let reason = notification
                        .params
                        .get("error")
                        .and_then(Value::as_str)
                        .unwrap_or("Login failed");
                    self.auth_error = Some(reason.to_string());
                }
            }
            "account/updated" => {
                self.logout_in_progress = false;
                let auth_mode = notification.params.get("authMode").and_then(Value::as_str);
                if auth_mode.is_none() {
                    self.is_authenticated = false;
                    self.account_email = None;
                    self.account_plan_type = None;
                    self.rate_limits = None;
                    self.login_url = None;
                    self.pending_login_id = None;
                    self.login_in_progress = false;
                    self.loading_auth_state = false;
                    self.clear_workspace_state();
                } else if let Some(server) = self._app_server.clone() {
                    self.refresh_account_state(server, true, cx);
                }
            }
            "account/rateLimits/updated" => {
                self.rate_limits = Some(Self::parse_rate_limits(&notification.params));
            }
            "thread/tokenUsage/updated" => {
                if applies_to_selected_thread {
                    self.thread_token_usage = Self::parse_thread_token_usage(&notification.params);
                }
            }
            _ => {}
        }
    }

    fn format_window_label(window_mins: u64) -> String {
        if window_mins >= 1440 {
            let days = window_mins / 1440;
            format!("{days}d")
        } else if window_mins >= 60 {
            let hours = window_mins / 60;
            format!("{hours}h")
        } else {
            format!("{window_mins}m")
        }
    }

    fn format_resets_at(resets_at: u64) -> String {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        if resets_at <= now {
            return "now".to_string();
        }
        let diff = resets_at - now;
        if diff < 3600 {
            let mins = diff / 60;
            format!("{mins}m")
        } else if diff < 86400 {
            let hours = diff / 3600;
            let mins = (diff % 3600) / 60;
            if mins > 0 {
                format!("{hours}h {mins}m")
            } else {
                format!("{hours}h")
            }
        } else {
            let days = diff / 86400;
            format!("{days}d")
        }
    }

    fn render_context_ring(
        diameter: gpui::Pixels,
        stroke_width: gpui::Pixels,
        fraction: f32,
        fill_color: gpui::Hsla,
        track_color: gpui::Hsla,
        _label_color: gpui::Hsla,
    ) -> impl IntoElement {
        let fraction = fraction.clamp(0.0, 1.0);
        canvas(
            move |_, _, _| {},
            move |bounds, _, window, _| {
                let center_x = bounds.center().x;
                let center_y = bounds.center().y;
                let radius = (diameter - stroke_width) / 2.0;

                let num_segments = 64;
                let build_arc = |start_frac: f32,
                                 end_frac: f32|
                 -> Vec<gpui::Point<gpui::Pixels>> {
                    let start_angle =
                        std::f32::consts::FRAC_PI_2 * -1.0 + start_frac * std::f32::consts::TAU;
                    let end_angle =
                        std::f32::consts::FRAC_PI_2 * -1.0 + end_frac * std::f32::consts::TAU;
                    let segments =
                        ((num_segments as f32 * (end_frac - start_frac)).ceil() as usize).max(2);
                    (0..=segments)
                        .map(|i| {
                            let t = i as f32 / segments as f32;
                            let angle = start_angle + t * (end_angle - start_angle);
                            point(
                                center_x + radius * angle.cos(),
                                center_y + radius * angle.sin(),
                            )
                        })
                        .collect()
                };

                if fraction < 1.0 {
                    let track_points = build_arc(fraction, 1.0);
                    if track_points.len() >= 2 {
                        let mut builder = PathBuilder::stroke(stroke_width);
                        builder.move_to(track_points[0]);
                        for p in &track_points[1..] {
                            builder.line_to(*p);
                        }
                        if let Ok(path) = builder.build() {
                            window.paint_path(path, track_color);
                        }
                    }
                }

                if fraction > 0.0 {
                    let fill_points = build_arc(0.0, fraction);
                    if fill_points.len() >= 2 {
                        let mut builder = PathBuilder::stroke(stroke_width);
                        builder.move_to(fill_points[0]);
                        for p in &fill_points[1..] {
                            builder.line_to(*p);
                        }
                        if let Ok(path) = builder.build() {
                            window.paint_path(path, fill_color);
                        }
                    }
                }
            },
        )
        .size(diameter)
    }

    fn render_rate_limit_bar(
        label: &str,
        entry: &RateLimitEntry,
        bar_fill_color: gpui::Hsla,
        bar_track_color: gpui::Hsla,
        subtext_color: gpui::Hsla,
    ) -> gpui::AnyElement {
        let used = entry.used_percent.clamp(0.0, 100.0);
        let remaining = 100.0 - used;
        let resets_label = Self::format_resets_at(entry.resets_at);

        div()
            .w_full()
            .flex()
            .flex_col()
            .gap(px(4.))
            .child(
                div()
                    .w_full()
                    .flex()
                    .items_center()
                    .justify_between()
                    .child(
                        div()
                            .text_xs()
                            .text_color(subtext_color)
                            .child(format!("{label} limit")),
                    )
                    .child(
                        div()
                            .text_xs()
                            .text_color(subtext_color)
                            .child(format!("{remaining:.0}% left · resets {resets_label}")),
                    ),
            )
            .child(
                div()
                    .w_full()
                    .h(px(6.))
                    .rounded(px(3.))
                    .bg(bar_track_color)
                    .overflow_hidden()
                    .flex()
                    .child(
                        div()
                            .h_full()
                            .rounded(px(3.))
                            .bg(bar_fill_color)
                            .flex_shrink_0()
                            .flex_grow()
                            .flex_basis(gpui::relative(remaining as f32 / 100.0)),
                    )
                    .child(
                        div()
                            .h_full()
                            .flex_shrink_0()
                            .flex_grow()
                            .flex_basis(gpui::relative(used as f32 / 100.0)),
                    ),
            )
            .into_any_element()
    }

    const IMAGE_EXTENSIONS: &[&str] = &["png", "jpg", "jpeg", "gif", "webp", "bmp", "tiff", "ico"];

    fn is_image_path(path: &Path) -> bool {
        path.extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| Self::IMAGE_EXTENSIONS.contains(&ext.to_ascii_lowercase().as_str()))
    }

    fn add_image_attachment(&mut self, path: PathBuf, cx: &mut Context<Self>) {
        if !self.attached_images.contains(&path) {
            self.attached_images.push(path);
            cx.notify();
        }
    }

    fn remove_image_attachment(&mut self, index: usize, cx: &mut Context<Self>) {
        if index < self.attached_images.len() {
            self.attached_images.remove(index);
            cx.notify();
        }
    }

    fn handle_dropped_paths(&mut self, paths: &ExternalPaths, cx: &mut Context<Self>) {
        for path in paths.paths() {
            if Self::is_image_path(path) {
                self.add_image_attachment(path.to_path_buf(), cx);
            }
        }
    }

    fn handle_clipboard_paste(&mut self, cx: &mut Context<Self>) {
        let Some(clipboard) = cx.read_from_clipboard() else {
            return;
        };

        for entry in clipboard.entries() {
            match entry {
                ClipboardEntry::Image(image) => {
                    let ext = match image.format {
                        ImageFormat::Png => "png",
                        ImageFormat::Jpeg => "jpg",
                        ImageFormat::Webp => "webp",
                        ImageFormat::Gif => "gif",
                        ImageFormat::Svg => "svg",
                        ImageFormat::Bmp => "bmp",
                        ImageFormat::Tiff => "tiff",
                        ImageFormat::Ico => "ico",
                    };
                    if let Some(tmp_path) = Self::write_clipboard_image_to_temp(&image.bytes, ext) {
                        self.add_image_attachment(tmp_path, cx);
                    }
                }
                ClipboardEntry::ExternalPaths(paths) => {
                    for path in paths.paths() {
                        if Self::is_image_path(path) {
                            self.add_image_attachment(path.to_path_buf(), cx);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn image_file_name(path: &Path) -> String {
        path.file_name()
            .and_then(|name| name.to_str())
            .map(str::to_owned)
            .unwrap_or_else(|| "image".to_string())
    }

    fn local_image_dimensions(path_or_url: &str) -> Option<(f32, f32)> {
        let path = Path::new(path_or_url);
        if !path.is_absolute() {
            return None;
        }

        let Ok(dimensions) = imagesize::size(path) else {
            return None;
        };
        if dimensions.width == 0 || dimensions.height == 0 {
            return None;
        }

        Some((dimensions.width as f32, dimensions.height as f32))
    }

    fn fit_size_to_bounds(width: f32, height: f32, max_width: f32, max_height: f32) -> (f32, f32) {
        if width <= 0. || height <= 0. {
            return (max_width, max_height);
        }

        let scale = (max_width / width).min(max_height / height).min(1.);
        ((width * scale).max(1.), (height * scale).max(1.))
    }

    fn message_image_preview_size(image_ref: &str, image_count: usize) -> (f32, f32) {
        let (default_width, default_height) = if image_count <= 1 {
            (360., 220.)
        } else {
            (220., 160.)
        };

        let Some((width, height)) = Self::local_image_dimensions(image_ref) else {
            return (default_width, default_height);
        };

        let aspect_ratio = width / height;
        let (max_width, max_height) = if image_count <= 1 {
            if aspect_ratio >= 2. {
                (620., 260.)
            } else if aspect_ratio <= 0.75 {
                (300., 460.)
            } else {
                (460., 320.)
            }
        } else if aspect_ratio >= 2. {
            (360., 170.)
        } else if aspect_ratio <= 0.75 {
            (170., 280.)
        } else {
            (220., 170.)
        };

        Self::fit_size_to_bounds(width, height, max_width, max_height)
    }

    fn composer_attachment_preview_size(image_ref: &str, attachment_count: usize) -> (f32, f32) {
        let (default_width, default_height, max_width, max_height) = if attachment_count <= 1 {
            (150., 96., 220., 140.)
        } else {
            (110., 72., 150., 96.)
        };

        let Some((width, height)) = Self::local_image_dimensions(image_ref) else {
            return (default_width, default_height);
        };

        Self::fit_size_to_bounds(width, height, max_width, max_height)
    }

    fn send_message(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        let text = self.input_state.read(cx).value().to_string();
        let text = text.trim().to_string();
        if self.selected_thread_is_busy() || self.has_pending_approval() {
            return;
        }

        let Some(server) = self._app_server.clone() else {
            self.set_thread_error("Not connected to app-server".to_string(), cx);
            cx.notify();
            return;
        };

        let thread_id = self.selected_thread_id.clone();
        let cwd = self.active_composer_cwd();
        let selected_model = self.selected_model.clone();
        let selected_reasoning_effort = self.selected_reasoning_effort.clone();
        let images = std::mem::take(&mut self.attached_images);
        let image_refs = images
            .iter()
            .filter_map(|path| path.to_str())
            .map(Self::image_ref_for_message)
            .filter(|path| !path.is_empty())
            .collect::<Vec<_>>();
        if text.is_empty() && image_refs.is_empty() {
            self.attached_images = images;
            return;
        }

        if let Some(thread_id) = thread_id.as_deref() {
            self.composing_new_thread_cwd = None;
            self.inflight_threads.insert(thread_id.to_string());
        } else {
            self.pending_new_thread = true;
        }

        self.input_state.update(cx, |state, cx| {
            state.set_value("", window, cx);
        });

        self.selected_thread_messages
            .push(Self::build_thread_message(
                ThreadSpeaker::User,
                text.clone(),
                image_refs,
                cx,
            ));
        self.streaming_message_ix = None;
        cx.notify();

        let skill_items = self.skill_input_items_for_text(&text);
        let turn_input = Self::build_user_turn_input(&text, &images, skill_items);

        cx.spawn(async move |view, cx| {
            let thread_id = if let Some(id) = thread_id {
                id
            } else {
                let mut thread_start_params = serde_json::Map::new();
                thread_start_params.insert("cwd".to_string(), json!(cwd));
                thread_start_params.insert("approvalPolicy".to_string(), json!("on-request"));
                if let Some(model) = selected_model.clone() {
                    thread_start_params.insert("model".to_string(), json!(model));
                }
                if let Some(effort) = selected_reasoning_effort.clone() {
                    thread_start_params.insert("reasoningEffort".to_string(), json!(effort));
                }
                let start_result = server
                    .call(
                        RequestMethod::ThreadStart,
                        Value::Object(thread_start_params),
                    )
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
                                view.pending_new_thread = false;
                                if view.selected_thread_id.is_none() {
                                    view.set_thread_error(
                                        "thread/start returned no thread id".to_string(),
                                        cx,
                                    );
                                }
                                cx.notify();
                            });
                            return;
                        }
                        let _ = view.update(cx, |view, cx| {
                            view.pending_new_thread = false;
                            view.inflight_threads.insert(id.clone());
                            view.composing_new_thread_cwd = None;
                            if view.selected_thread_id.is_none() {
                                view.selected_thread_id = Some(id.clone());
                                view.selected_thread_title = Some("New thread".to_string());
                            }
                            view.load_threads_from_server(Arc::clone(&server), cx);
                            cx.notify();
                        });
                        id
                    }
                    Err(error) => {
                        let _ = view.update(cx, |view, cx| {
                            view.pending_new_thread = false;
                            if view.selected_thread_id.is_none() {
                                view.set_thread_error(format!("thread/start failed: {error}"), cx);
                            }
                            cx.notify();
                        });
                        return;
                    }
                }
            };

            let mut turn_start_params = serde_json::Map::new();
            turn_start_params.insert("threadId".to_string(), json!(thread_id));
            turn_start_params.insert("input".to_string(), json!(turn_input));
            turn_start_params.insert("cwd".to_string(), json!(cwd));
            turn_start_params.insert("approvalPolicy".to_string(), json!("on-request"));
            if let Some(model) = selected_model {
                turn_start_params.insert("model".to_string(), json!(model));
            }
            if let Some(effort) = selected_reasoning_effort {
                turn_start_params.insert("reasoningEffort".to_string(), json!(effort.clone()));
                turn_start_params.insert("effort".to_string(), json!(effort));
            }

            let result = server
                .call(RequestMethod::TurnStart, Value::Object(turn_start_params))
                .await;

            let _ = view.update(cx, |view, cx| {
                match result {
                    Ok(_payload) => {
                        if view.selected_thread_id.as_deref() == Some(thread_id.as_str()) {
                            view.selected_thread_error = None;
                        }
                    }
                    Err(error) => {
                        view.inflight_threads.remove(thread_id.as_str());
                        if view.selected_thread_id.as_deref() == Some(thread_id.as_str()) {
                            view.set_thread_error(format!("turn/start failed: {error}"), cx);
                        }
                    }
                }
                cx.notify();
            });
        })
        .detach();
    }

    fn build_user_turn_input(
        text: &str,
        images: &[PathBuf],
        mut skill_items: Vec<Value>,
    ) -> Vec<Value> {
        let mut turn_input = Vec::new();
        for image_path in images {
            if let Some(path_str) = image_path.to_str() {
                turn_input.push(json!({ "type": "localImage", "path": path_str }));
            }
        }
        if !text.is_empty() {
            turn_input.push(json!({ "type": "text", "text": text }));
        } else if !images.is_empty() {
            turn_input
                .push(json!({ "type": "text", "text": "Please analyze the attached image(s)." }));
        }
        turn_input.append(&mut skill_items);
        turn_input
    }
}

impl Render for AppShell {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        self.maybe_load_threads(cx);
        self.ensure_composer_options_for_active_workspace(cx);
        if self.is_authenticated {
            self.maybe_fetch_selected_thread_from_api(cx);
        }

        let workspace_groups =
            Self::group_threads_by_workspace(&self.threads, &self.known_workspace_cwds);
        let this = cx.entity().downgrade();
        let selected_id = self.selected_thread_id.clone();
        let skill_picker = self.current_skill_picker_state(cx);
        let skill_picker_selected_ix = skill_picker.as_ref().map(|picker| {
            self.skill_picker_selected_ix
                .min(picker.items.len().saturating_sub(1))
        });
        let file_picker = if skill_picker.is_some() {
            None
        } else {
            self.current_file_picker_state(cx)
        };
        let file_picker_selected_ix = file_picker.as_ref().map(|picker| {
            self.file_picker_selected_ix
                .min(picker.items.len().saturating_sub(1))
        });
        let waiting_for_response = self.selected_thread_is_busy();
        let show_streaming_status = waiting_for_response && !self.has_pending_approval();
        let has_input = !self.input_state.read(cx).value().trim().is_empty()
            || !self.attached_images.is_empty();
        let can_send = has_input
            && !waiting_for_response
            && !self.has_pending_approval()
            && self.is_authenticated
            && self._app_server.is_some();
        let attached_images = self.attached_images.clone();
        let model_label = self.selected_model_label();
        let reasoning_label = self.selected_reasoning_label();
        let selected_model = self.selected_model.clone().unwrap_or_default();
        let selected_reasoning_effort = self.selected_reasoning_effort.clone().unwrap_or_default();
        let available_models = self.available_models.clone();
        let reasoning_efforts = self.available_reasoning_efforts();
        let scroll_offset = self.scroll_handle.offset();
        let scroll_max = self.scroll_handle.max_offset();
        let distance_to_bottom = scroll_max.height + scroll_offset.y;
        let show_scroll_to_bottom =
            !self.selected_thread_messages.is_empty() && distance_to_bottom > px(8.);

        let thread_workspace_items = if self.loading_threads && self.threads.is_empty() {
            vec![
                SidebarMenuItem::new("Loading threads...")
                    .icon(IconName::LoaderCircle)
                    .disable(true),
            ]
        } else if self.threads.is_empty() && self.thread_error.is_some() {
            let error = self.thread_error.clone().unwrap_or_default();
            vec![
                SidebarMenuItem::new(Self::truncate(&format!("Failed to load: {error}"), 40))
                    .icon(IconName::TriangleAlert)
                    .disable(true),
            ]
        } else {
            let mut items = Vec::new();
            for group in &workspace_groups {
                let is_workspace_open = self.expanded_workspace_groups.contains(&group.cwd);
                let show_all = self.expanded_workspace_threads.contains(&group.cwd);
                let visible_count = if show_all {
                    group.threads.len()
                } else {
                    group.threads.len().min(Self::MAX_THREADS_PER_WORKSPACE)
                };
                let hidden_count = group.threads.len().saturating_sub(visible_count);

                let mut children = Vec::new();
                children.extend(group.threads.iter().take(visible_count).map(|row| {
                    let is_active = selected_id.as_deref() == Some(row.id.as_str());
                    let is_running = self.inflight_threads.contains(row.id.as_str());
                    let mut item = SidebarMenuItem::new(row.title.clone())
                        .icon(if is_running {
                            IconName::LoaderCircle
                        } else {
                            IconName::Bot
                        })
                        .spin_icon(is_running)
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
                }));

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
                        .icon(if is_workspace_open {
                            IconName::FolderOpen
                        } else {
                            IconName::FolderClosed
                        })
                        .active(false)
                        .default_open(is_workspace_open)
                        .click_to_open(true)
                        .show_caret(false)
                        .on_open_change({
                            let this = this.clone();
                            let workspace_cwd = group.cwd.clone();
                            move |is_open, _, cx| {
                                let _ = this.update(cx, |view, cx| {
                                    if is_open {
                                        view.expanded_workspace_groups
                                            .insert(workspace_cwd.clone());
                                    } else {
                                        view.expanded_workspace_groups.remove(&workspace_cwd);
                                    }
                                    cx.notify();
                                });
                            }
                        })
                        .suffix_visible_on_hover(true)
                        .suffix({
                            let this = this.clone();
                            let workspace_cwd = group.cwd.clone();
                            let button_id =
                                Self::workspace_action_id("workspace-new-thread", &workspace_cwd);
                            move |_, _| {
                                div().child(
                                    Button::new(button_id.clone())
                                        .ghost()
                                        .xsmall()
                                        .icon(IconName::Plus)
                                        .tooltip("Start new thread in workspace")
                                        .on_click({
                                            let this = this.clone();
                                            let workspace_cwd = workspace_cwd.clone();
                                            move |_, _: &mut Window, cx: &mut App| {
                                                cx.stop_propagation();
                                                let _ = this.update(cx, |view, cx| {
                                                    view.start_new_thread_in_workspace(
                                                        workspace_cwd.clone(),
                                                        cx,
                                                    );
                                                });
                                            }
                                        }),
                                )
                            }
                        })
                        .children(children),
                );
            }
            if items.is_empty() {
                vec![
                    SidebarMenuItem::new("No workspaces yet")
                        .icon(IconName::Inbox)
                        .disable(true),
                ]
            } else {
                items
            }
        };

        let app_theme = cx.global::<AppTheme>();
        let is_dark = app_theme.mode.is_dark();
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
        let green_color = app_theme.green;
        let blue_color = app_theme.blue;
        let crust = app_theme.crust;
        let font_sans: gpui::SharedString = app_theme.font_sans.clone();
        let font_mono: gpui::SharedString = app_theme.font_mono.clone();
        let highlight_theme = if is_dark {
            HighlightTheme::default_dark()
        } else {
            HighlightTheme::default_light()
        };

        if !self.is_authenticated {
            let can_start_login =
                self._app_server.is_some() && !self.loading_auth_state && !self.login_in_progress;
            let primary_label = if self.loading_auth_state {
                "Checking account..."
            } else if self.login_in_progress {
                "Waiting for login confirmation..."
            } else {
                "Continue with ChatGPT"
            };
            let login_status = if self.loading_auth_state {
                "Checking your account...".to_string()
            } else if self.login_in_progress {
                "Waiting for login confirmation...".to_string()
            } else if self._app_server.is_none() {
                if let Some(error) = &self.thread_error {
                    format!("Connection error: {}", error)
                } else {
                    "Connecting to Codex app-server...".to_string()
                }
            } else {
                "Log in to Agent Hub to view and use workspaces.".to_string()
            };
            let show_login_url = self.login_url.is_some();
            let auth_error = self.auth_error.clone().unwrap_or_default();
            let show_auth_error = !auth_error.is_empty();
            let account_email = self.account_email.clone().unwrap_or_default();
            let show_account_email = !account_email.is_empty();
            let account_plan_type = self.account_plan_type.clone().unwrap_or_default();
            let show_account_plan_type = !account_plan_type.is_empty();
            let hero_bg = hsl(220., 18., 95.);
            let hero_text = hsl(220., 10., 10.);
            let hero_subtext = hsl(220., 8., 50.);
            let primary_bg = hsl(220., 10., 5.);
            let disabled_bg = hsl(220., 12., 84.);
            let icon_border = hsl(220., 12., 78.);

            return div()
                .size_full()
                .font_family(font_sans)
                .bg(hero_bg)
                .flex()
                .flex_col()
                .child(
                    div()
                        .w_full()
                        .h(px(74.))
                        .flex()
                        .items_center()
                        .justify_center(),
                )
                .child(
                    div()
                        .w_full()
                        .flex_1()
                        .flex()
                        .justify_center()
                        .items_start()
                        .pt(px(90.))
                        .child(
                            div()
                                .w_full()
                                .max_w(px(560.))
                                .mx_auto()
                                .px(px(24.))
                                .flex()
                                .flex_col()
                                .items_center()
                                .gap(px(16.))
                                .child(
                                    div()
                                        .size(px(74.))
                                        .rounded(px(999.))
                                        .border_1()
                                        .border_color(icon_border)
                                        .flex()
                                        .items_center()
                                        .justify_center()
                                        .child(
                                            Icon::new(IconName::Bot)
                                                .size(px(34.))
                                                .text_color(hero_text),
                                        ),
                                )
                                .child(
                                    div()
                                        .mt(px(8.))
                                        .text_lg()
                                        .text_color(hero_text)
                                        .child("Welcome to Agent Hub"),
                                )
                                .child(
                                    div()
                                        .text_sm()
                                        .text_color(hero_subtext)
                                        .child("Your workspace for building with agents"),
                                )
                                .child(
                                    div()
                                        .mt(px(12.))
                                        .w_full()
                                        .max_w(px(380.))
                                        .h(px(56.))
                                        .rounded(px(999.))
                                        .flex()
                                        .items_center()
                                        .justify_center()
                                        .bg(if can_start_login {
                                            primary_bg
                                        } else {
                                            disabled_bg
                                        })
                                        .when(can_start_login, |this| {
                                            this.cursor_pointer().on_mouse_down(
                                                gpui::MouseButton::Left,
                                                cx.listener(|view, _, _, cx| {
                                                    view.start_login(cx);
                                                }),
                                            )
                                        })
                                        .child(
                                            div()
                                                .text_sm()
                                                .text_color(hsl(0., 0., 98.))
                                                .child(primary_label),
                                        ),
                                )
                                .child(
                                    div()
                                        .mt(px(6.))
                                        .text_sm()
                                        .text_color(hero_subtext)
                                        .child(login_status),
                                )
                                .when(show_login_url, |this| {
                                    this.child(
                                        div().text_sm().text_color(hero_subtext).child(
                                            "A browser window was opened to complete sign in.",
                                        ),
                                    )
                                })
                                .when(show_account_email, |this| {
                                    let account_email = account_email.clone();
                                    this.child(
                                        div()
                                            .text_sm()
                                            .text_color(hero_subtext)
                                            .child(format!("Signed in as {account_email}")),
                                    )
                                })
                                .when(show_account_plan_type, |this| {
                                    let account_plan_type = account_plan_type.clone();
                                    this.child(
                                        div()
                                            .text_sm()
                                            .text_color(hero_subtext)
                                            .child(format!("Plan: {account_plan_type}")),
                                    )
                                })
                                .when(show_auth_error, |this| {
                                    let auth_error = auth_error.clone();
                                    this.child(
                                        div().text_sm().text_color(red_color).child(auth_error),
                                    )
                                }),
                        ),
                );
        }

        div()
            .size_full()
            .font_family(font_sans)
            .bg(sidebar_bg)
            .flex()
            .flex_row()
            .capture_action(cx.listener(|view, _: &InputMoveUp, window, cx| {
                if view.has_pending_approval() && view.move_approval_selection(-1) {
                    window.prevent_default();
                    cx.stop_propagation();
                    cx.notify();
                }
            }))
            .capture_action(cx.listener(|view, _: &InputMoveDown, window, cx| {
                if view.has_pending_approval() && view.move_approval_selection(1) {
                    window.prevent_default();
                    cx.stop_propagation();
                    cx.notify();
                }
            }))
            .capture_action(cx.listener(|view, action: &InputEnter, window, cx| {
                if view.has_pending_approval() && !action.secondary {
                    if view.submit_selected_approval(cx) {
                        window.prevent_default();
                        cx.stop_propagation();
                        cx.notify();
                    }
                }
            }))
            .capture_action(cx.listener(|view, _: &NewWorkspace, window, cx| {
                window.prevent_default();
                cx.stop_propagation();
                view.prompt_add_workspace(cx);
            }))
            .child(
                Sidebar::<AnySidebarItem>::new("sidebar")
                    .w(px(300.))
                    .collapsed(self.sidebar_collapsed)
                    .header(div().h_4())
                    .child(
                        SidebarMenu::new()
                            .child(
                                SidebarMenuItem::new("New thread")
                                    .icon(IconName::Plus)
                                    .on_click({
                                        let this = this.clone();
                                        move |_, _, cx| {
                                            let _ = this.update(cx, |view, cx| {
                                                let workspace_cwd = view.active_composer_cwd();
                                                view.start_new_thread_in_workspace(
                                                    workspace_cwd,
                                                    cx,
                                                );
                                            });
                                        }
                                    }),
                            )
                    )
                    .child(
                        SidebarGroup::new("Workspaces")
                            .suffix({
                                let this = this.clone();
                                move |_, _| {
                                    Button::new("workspaces-add-workspace")
                                        .ghost()
                                        .xsmall()
                                        .icon(IconName::Folder)
                                        .tooltip("Add workspace")
                                        .on_click({
                                            let this = this.clone();
                                            move |_, _: &mut Window, cx: &mut App| {
                                                cx.stop_propagation();
                                                let _ = this.update(cx, |view, cx| {
                                                    view.prompt_add_workspace(cx);
                                                });
                                            }
                                        })
                                }
                            })
                            .child(SidebarMenu::new().children(thread_workspace_items)),
                    )
                    .footer({
                        let rate_limits = self.rate_limits.clone();
                        let sidebar_collapsed = self.sidebar_collapsed;

                        div()
                            .w_full()
                            .flex()
                            .flex_col()
                            .gap(px(4.))
                            .when_some(
                                if sidebar_collapsed { None } else { rate_limits },
                                |this, limits| {
                                    this.child(
                                        div()
                                            .w_full()
                                            .flex()
                                            .flex_col()
                                            .gap(px(8.))
                                            .px(px(4.))
                                            .py(px(8.))
                                            .mb(px(4.))
                                            .rounded(px(6.))
                                            .border_1()
                                            .border_color(surface0)
                                            .bg(mantle)
                                            .when_some(
                                                limits.primary.as_ref(),
                                                |this, entry| {
                                                    let label = Self::format_window_label(
                                                        entry.window_duration_mins,
                                                    );
                                                    this.child(Self::render_rate_limit_bar(
                                                        &label,
                                                        entry,
                                                        overlay_color,
                                                        surface0,
                                                        subtext_color,
                                                    ))
                                                },
                                            )
                                            .when_some(
                                                limits.secondary.as_ref(),
                                                |this, entry| {
                                                    let label = Self::format_window_label(
                                                        entry.window_duration_mins,
                                                    );
                                                    this.child(Self::render_rate_limit_bar(
                                                        &label,
                                                        entry,
                                                        overlay_color,
                                                        surface0,
                                                        subtext_color,
                                                    ))
                                                },
                                            ),
                                    )
                                },
                            )
                            .child(
                                SidebarFooter::new()
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
                                                    .gap(px(8.))
                                                    .child(Icon::new(IconName::Settings))
                                                    .when(!sidebar_collapsed, |this| {
                                                        this.child("Settings")
                                                    }),
                                            )
                                            .when(!sidebar_collapsed, |this| {
                                                this.child(
                                                    Icon::new(IconName::ChevronUp).size(px(14.)),
                                                )
                                            }),
                                    )
                                    .dropdown_menu_with_anchor(gpui::Corner::TopLeft, {
                                        let this = this.clone();
                                        let logout_in_progress = self.logout_in_progress;
                                        let account_email = self.account_email.clone();
                                        move |menu: PopupMenu,
                                              _window: &mut Window,
                                              _cx: &mut gpui::Context<PopupMenu>| {
                                            let menu = if let Some(ref email) = account_email {
                                                menu.item(
                                                    PopupMenuItem::new(email.clone())
                                                        .icon(IconName::CircleUser),
                                                )
                                                .separator()
                                            } else {
                                                menu
                                            };

                                            if logout_in_progress {
                                                return menu.separator().label("Logging out...");
                                            }

                                            menu.item(
                                                PopupMenuItem::new("Log out")
                                                    .icon(IconName::ArrowRight)
                                                    .on_click({
                                                        let this = this.clone();
                                                        move |_, _, cx| {
                                                            let _ = this.update(cx, |view, cx| {
                                                                view.start_logout(cx);
                                                            });
                                                        }
                                                    }),
                                            )
                                        }
                                    }),
                            )
                    }),
            )
            .when(!self.sidebar_collapsed, |this| {
                this.child(div().w(px(1.)).h_full().bg(divider_color))
            })
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
                            .h(px(52.))
                            .px(px(16.))
                            .flex()
                            .items_center()
                            .justify_between()
                            .child({
                                let sidebar_collapsed = self.sidebar_collapsed;
                                let entity = this.clone();
                                div()
                                    .flex()
                                    .items_center()
                                    .gap(px(8.))
                                    .when(sidebar_collapsed, |this| {
                                        this.pl(px(80.))
                                    })
                                    .child(
                                        SidebarToggleButton::new()
                                            .collapsed(sidebar_collapsed)
                                            .on_click(move |_, _, cx| {
                                                let _ = entity.update(cx, |view, cx| {
                                                    view.sidebar_collapsed =
                                                        !view.sidebar_collapsed;
                                                    cx.notify();
                                                });
                                            }),
                                    )
                                    .child(
                                        div()
                                            .text_sm()
                                            .text_color(text_color)
                                            .child(
                                                self.selected_thread_title
                                                    .clone()
                                                    .unwrap_or_else(|| "Thread".to_string()),
                                            ),
                                    )
                            }),
                    )
                    .child(div().w_full().h(px(1.)).bg(divider_color))
                    .child(
                        div()
                            .flex_1()
                            .min_h_0()
                            .relative()
                            .child(
                                div()
                                    .id("chat-scroll")
                                    .size_full()
                                    .overflow_y_scroll()
                                    .track_scroll(&self.scroll_handle)
                                    .px(px(20.))
                                    .py(px(16.))
                                    .child({
                                        div().w_full().flex().justify_center().child(
                                            div()
                                                .w_full()
                                                .max_w(px(930.))
                                                .py(px(18.))
                                                .child(if !self.selected_thread_messages.is_empty() {
                                                    let chat_rows = self
                                                        .selected_thread_messages
                                                        .iter()
                                                        .enumerate()
                                                        .map(|(message_ix, message)| {
                                                match message.speaker {
                                                    ThreadSpeaker::User => {
                                                        let image_refs = message.image_refs.clone();
                                                        let has_text =
                                                            !message.content.trim().is_empty();
                                                        div()
                                                            .w_full()
                                                            .mb(px(14.))
                                                            .flex()
                                                            .justify_end()
                                                            .child(
                                                                div()
                                                                    .max_w(px(700.))
                                                                    .flex()
                                                                    .flex_col()
                                                                    .items_end()
                                                                    .gap(px(8.))
                                                                    .when(!image_refs.is_empty(), {
                                                                        let image_count = image_refs.len();
                                                                        let image_refs = image_refs;
                                                                        move |this| {
                                                                            this.child(
                                                                                div()
                                                                                    .w_full()
                                                                                    .max_w(px(if image_count <= 1 {
                                                                                        620.
                                                                                    } else {
                                                                                        390.
                                                                                    }))
                                                                                    .flex()
                                                                                    .flex_wrap()
                                                                                    .justify_end()
                                                                                    .gap(px(6.))
                                                                                    .children(
                                                                                        image_refs
                                                                                            .into_iter()
                                                                                            .map(
                                                                                                |image_ref| {
                                                                                                    let preview_size = AppShell::message_image_preview_size(
                                                                                                        &image_ref,
                                                                                                        image_count,
                                                                                                    );
                                                                                                    let image = if Path::new(&image_ref)
                                                                                                        .is_absolute()
                                                                                                    {
                                                                                                        img(PathBuf::from(
                                                                                                            image_ref
                                                                                                                .clone(),
                                                                                                        ))
                                                                                                    } else {
                                                                                                        img(image_ref)
                                                                                                    };
                                                                                                    div()
                                                                                                        .w(px(preview_size.0))
                                                                                                        .h(px(preview_size.1))
                                                                                                        .rounded(px(10.))
                                                                                                        .overflow_hidden()
                                                                                                        .border_1()
                                                                                                        .border_color(surface1)
                                                                                                        .bg(surface0)
                                                                                                        .child(
                                                                                                            image
                                                                                                                .size_full()
                                                                                                                .object_fit(
                                                                                                                    gpui::ObjectFit::ScaleDown,
                                                                                                                ),
                                                                                                        )
                                                                                                },
                                                                                            ),
                                                                                    ),
                                                                            )
                                                                        }
                                                                    })
                                                                    .when(has_text, |this| {
                                                                        this.child(
                                                                            div()
                                                                                .w_full()
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
                                                                    }),
                                                            )
                                                            .into_any_element()
                                                    }
                                                    ThreadSpeaker::Assistant => {
                                                        match &message.kind {
                                                            ThreadMessageKind::Text => div()
                                                                .w_full()
                                                                .mb(px(16.))
                                                                .flex()
                                                                .justify_start()
                                                                .child(
                                                                    div()
                                                                        .max_w(px(930.))
                                                                        .text_color(text_color)
                                                                        .child(
                                                                            TextView::new(
                                                                                &message.view_state,
                                                                            )
                                                                            .style(
                                                                                TextViewStyle {
                                                                                    highlight_theme: highlight_theme.clone(),
                                                                                    is_dark,
                                                                                    ..TextViewStyle::default()
                                                                                }
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
                                                                    .mb(px(10.))
                                                                    .flex()
                                                                    .justify_start()
                                                                    .child(
                                                                        div()
                                                                            .w_full()
                                                                            .max_w(px(930.))
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
                                                                                                TextViewStyle {
                                                                                                    highlight_theme: highlight_theme.clone(),
                                                                                                    is_dark,
                                                                                                    ..TextViewStyle::default()
                                                                                                }
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
                                                                                                && let ThreadMessageKind::CommandExecution {
                                                                                                    expanded,
                                                                                                    ..
                                                                                                } = &mut message.kind
                                                                                            {
                                                                                                *expanded = open_ixs
                                                                                                    .contains(&0);
                                                                                                cx.notify();
                                                                                            }
                                                                                        },
                                                                                    ),
                                                                                ),
                                                                            ),
                                                                    )
                                                                    .into_any_element()
                                                            }
                                                            ThreadMessageKind::FileChange {
                                                                status_label,
                                                                file_diffs,
                                                                expanded,
                                                            } => {
                                                                let file_change_accordion_id =
                                                                    format!(
                                                                        "file-change-{message_ix}"
                                                                    );
                                                                let status_label = status_label.clone();
                                                                let expanded = *expanded;
                                                                let file_diffs = file_diffs.clone();
                                                                let total_files = file_diffs.len();
                                                                let font_mono = font_mono.clone();

                                                                let added_bg = gpui::Hsla { a: 0.12, ..green_color };
                                                                let removed_bg = gpui::Hsla { a: 0.12, ..red_color };

                                                                div()
                                                                    .w_full()
                                                                    .mb(px(10.))
                                                                    .flex()
                                                                    .justify_start()
                                                                    .child(
                                                                        div()
                                                                            .w_full()
                                                                            .max_w(px(930.))
                                                                            .child(
                                                                                gpui_component::accordion::Accordion::new(
                                                                                    file_change_accordion_id,
                                                                                )
                                                                                .item(|this| {
                                                                                    let header_text = if total_files == 1 {
                                                                                        format!("1 file changed · {status_label}")
                                                                                    } else {
                                                                                        format!("{total_files} files changed · {status_label}")
                                                                                    };

                                                                                    this
                                                                                        .open(expanded)
                                                                                        .title(header_text)
                                                                                        .child(
                                                                                            div()
                                                                                                .flex()
                                                                                                .flex_col()
                                                                                                .gap(px(12.))
                                                                                                .children(
                                                                                                    file_diffs.iter().map(|file_diff| {
                                                                                                        let total_add = file_diff.additions;
                                                                                                        let total_del = file_diff.deletions;
                                                                                                        let path = file_diff.path.clone();
                                                                                                        let font_mono = font_mono.clone();
                                                                                                        let max_lineno = file_diff
                                                                                                            .rows
                                                                                                            .iter()
                                                                                                            .filter_map(|row| match row {
                                                                                                                diff_view::DiffRow::Line(line) => {
                                                                                                                    line.old_lineno.max(line.new_lineno)
                                                                                                                }
                                                                                                                diff_view::DiffRow::HunkHeader(_) => None,
                                                                                                            })
                                                                                                            .max()
                                                                                                            .unwrap_or(0);
                                                                                                        let line_number_digits = max_lineno.max(1).to_string().len() as f32;
                                                                                                        let line_number_column_width =
                                                                                                            (line_number_digits * 9. + 16.).max(44.);

                                                                                                        div()
                                                                                                            .flex()
                                                                                                            .flex_col()
                                                                                                            .rounded(px(8.))
                                                                                                            .border_1()
                                                                                                            .border_color(surface1)
                                                                                                            .overflow_hidden()
                                                                                                            .child(
                                                                                                                div()
                                                                                                                    .w_full()
                                                                                                                    .px(px(12.))
                                                                                                                    .py(px(8.))
                                                                                                                    .bg(surface0)
                                                                                                                    .border_b_1()
                                                                                                                    .border_color(surface1)
                                                                                                                    .flex()
                                                                                                                    .items_center()
                                                                                                                    .gap(px(8.))
                                                                                                                    .text_sm()
                                                                                                                    .child(
                                                                                                                        div()
                                                                                                                            .font_family(font_mono.clone())
                                                                                                                            .text_color(text_color)
                                                                                                                            .child(path),
                                                                                                                    )
                                                                                                                    .child(
                                                                                                                        div()
                                                                                                                            .flex()
                                                                                                                            .gap(px(6.))
                                                                                                                            .child(
                                                                                                                                div()
                                                                                                                                    .text_color(green_color)
                                                                                                                                    .child(format!("+{total_add}")),
                                                                                                                            )
                                                                                                                            .child(
                                                                                                                                div()
                                                                                                                                    .text_color(red_color)
                                                                                                                                    .child(format!("-{total_del}")),
                                                                                                                            ),
                                                                                                                    ),
                                                                                                            )
                                                                                                            .child(
                                                                                                                div()
                                                                                                                    .w_full()
                                                                                                                    .overflow_x_scrollbar()
                                                                                                                    .child(
                                                                                                                        div()
                                                                                                                            .flex()
                                                                                                                            .flex_col()
                                                                                                                            .min_w_full()
                                                                                                                            .font_family(font_mono.clone())
                                                                                                                            .text_sm()
                                                                                                                            .children(
                                                                                                                                file_diff.rows.iter().map(|row| {
                                                                                                                                    match row {
                                                                                                                                        diff_view::DiffRow::HunkHeader(h) => {
                                                                                                                                            div()
                                                                                                                                                .w_full()
                                                                                                                                                .px(px(12.))
                                                                                                                                                .py(px(4.))
                                                                                                                                                .bg(surface0)
                                                                                                                                                .text_color(subtext_color)
                                                                                                                                                .text_xs()
                                                                                                                                                .whitespace_nowrap()
                                                                                                                                                .child(h.raw.clone())
                                                                                                                                                .into_any_element()
                                                                                                                                        }
                                                                                                                                        diff_view::DiffRow::Line(line) => {
                                                                                                                                            let (row_bg, bar_color) = match line.kind {
                                                                                                                                                diff_view::DiffLineKind::Added => (added_bg, green_color),
                                                                                                                                                diff_view::DiffLineKind::Removed => (removed_bg, red_color),
                                                                                                                                                diff_view::DiffLineKind::Context => (gpui::Hsla::transparent_black(), gpui::Hsla::transparent_black()),
                                                                                                                                            };

                                                                                                                                            let old_ln = line.old_lineno.map_or_else(|| "\u{00A0}".to_string(), |n| n.to_string());
                                                                                                                                            let new_ln = line.new_lineno.map_or_else(|| "\u{00A0}".to_string(), |n| n.to_string());

                                                                                                                                            let display_text = if line.text.is_empty() {
                                                                                                                                                "\u{00A0}".to_string()
                                                                                                                                            } else {
                                                                                                                                                line.text.replace(' ', "\u{00A0}")
                                                                                                                                            };

                                                                                                                                            div()
                                                                                                                                                .min_w_full()
                                                                                                                                               .flex()
                                                                                                                                               .flex_row()
                                                                                                                                               .bg(row_bg)
                                                                                                                                               .child(
                                                                                                                                                   div()
                                                                                                                                                        .w(px(3.))
                                                                                                                                                        .h_full()
                                                                                                                                                        .bg(bar_color),
                                                                                                                                                )
                                                                                                                                               .child(
                                                                                                                                                   div()
                                                                                                                                                        .w(px(line_number_column_width))
                                                                                                                                                        .flex_shrink_0()
                                                                                                                                                        .px(px(6.))
                                                                                                                                                        .py(px(1.))
                                                                                                                                                        .text_color(subtext_color)
                                                                                                                                                        .text_right()
                                                                                                                                                        .whitespace_nowrap()
                                                                                                                                                        .child(old_ln),
                                                                                                                                               )
                                                                                                                                               .child(
                                                                                                                                                   div()
                                                                                                                                                        .w(px(line_number_column_width))
                                                                                                                                                        .flex_shrink_0()
                                                                                                                                                        .px(px(6.))
                                                                                                                                                        .py(px(1.))
                                                                                                                                                        .text_color(subtext_color)
                                                                                                                                                        .text_right()
                                                                                                                                                        .whitespace_nowrap()
                                                                                                                                                        .child(new_ln),
                                                                                                                                               )
                                                                                                                                               .child(
                                                                                                                                                   div()
                                                                                                                                                        .flex_1()
                                                                                                                                                        .px(px(8.))
                                                                                                                                                        .py(px(1.))
                                                                                                                                                        .text_color(text_color)
                                                                                                                                                        .whitespace_nowrap()
                                                                                                                                                        .child(display_text),
                                                                                                                                               )
                                                                                                                                               .into_any_element()
                                                                                                                                        }
                                                                                                                                    }
                                                                                                                                }),
                                                                                                                            ),
                                                                                                                    ),
                                                                                                            )
                                                                                                    }),
                                                                                                ),
                                                                                        )
                                                                                })
                                                                                .on_toggle_click(
                                                                                    cx.listener(
                                                                                        move |view, open_ixs: &[usize], _, cx| {
                                                                                            if let Some(message) = view
                                                                                                .selected_thread_messages
                                                                                                .get_mut(message_ix)
                                                                                                && let ThreadMessageKind::FileChange {
                                                                                                    expanded,
                                                                                                    ..
                                                                                                } = &mut message.kind
                                                                                            {
                                                                                                *expanded = open_ixs
                                                                                                    .contains(&0);
                                                                                                cx.notify();
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
                                                    let mut container = div().w_full().children(chat_rows);
                                                    if let Some((_msg, view_state)) = &self.selected_thread_error {
                                                        container = container.child(
                                                            div()
                                                                .mt(px(8.))
                                                                .px(px(16.))
                                                                .py(px(8.))
                                                                .rounded(px(6.))
                                                                .bg(red_color.opacity(0.1))
                                                                .text_sm()
                                                                .text_color(red_color)
                                                                .child(
                                                                    TextView::new(view_state)
                                                                        .text_color(red_color),
                                                                ),
                                                        );
                                                    }
                                                    container.into_any_element()
                                                } else if let Some((_msg, view_state)) = &self.selected_thread_error {
                                                    div()
                                                        .text_sm()
                                                        .text_color(red_color)
                                                        .child(
                                                            TextView::new(view_state)
                                                                .text_color(red_color),
                                                        )
                                                        .into_any_element()
                                                } else if self.loading_threads
                                                    || self.loading_selected_thread
                                                {
                                                    div()
                                                        .text_sm()
                                                        .text_color(subtext_color)
                                                        .child("Loading thread content...")
                                                        .into_any_element()
                                                } else if let Some(cwd) = &self.composing_new_thread_cwd {
                                                    let workspace_name = Self::workspace_name(cwd);
                                                    div()
                                                        .w_full()
                                                        .min_h(px(460.))
                                                        .flex()
                                                        .flex_col()
                                                        .items_center()
                                                        .justify_center()
                                                        .gap(px(12.))
                                                        .child(
                                                            Icon::new(IconName::Bot)
                                                                .size(px(44.))
                                                                .text_color(overlay_color),
                                                        )
                                                        .child(
                                                            div()
                                                                .text_xl()
                                                                .text_color(text_color)
                                                                .child("New thread"),
                                                        )
                                                        .child(
                                                            div()
                                                                .text_sm()
                                                                .text_color(subtext_color)
                                                                .child(format!(
                                                                    "Start your first request in {workspace_name}."
                                                                )),
                                                        )
                                                        .into_any_element()
                                                } else if self.selected_thread_id.is_some() {
                                                    div()
                                                        .text_sm()
                                                        .text_color(subtext_color)
                                                        .child("No messages in this thread yet.")
                                                        .into_any_element()
                                                } else {
                                                    div()
                                                        .text_sm()
                                                        .text_color(subtext_color)
                                                        .child("Select a thread to view its content.")
                                                        .into_any_element()
                                                })
                                        )
                                    }),
                            )
                            .when(show_scroll_to_bottom, |this| {
                                this.child(
                                    div()
                                        .absolute()
                                        .left_0()
                                        .right_0()
                                        .bottom(px(5.))
                                        .flex()
                                        .justify_center()
                                        .child(
                                            div()
                                                .id("scroll-to-bottom")
                                                .h(px(36.))
                                                .px(px(12.))
                                                .rounded(px(999.))
                                                .border_1()
                                                .border_color(surface1)
                                                .bg(mantle)
                                                .cursor_pointer()
                                                .flex()
                                                .items_center()
                                                .gap(px(6.))
                                                .on_mouse_down(
                                                    gpui::MouseButton::Left,
                                                    cx.listener(|view, _, _, cx| {
                                                        view.scroll_handle.scroll_to_bottom();
                                                        cx.notify();
                                                    }),
                                                )
                                                .child(
                                                    Icon::new(IconName::ArrowDown)
                                                        .size(px(14.))
                                                        .text_color(overlay_color),
                                                )
                                                .child(
                                                    div()
                                                        .text_xs()
                                                        .text_color(overlay_color)
                                                        .child("Bottom"),
                                                ),
                                        ),
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
                                    .on_drop(cx.listener(
                                        |view, paths: &ExternalPaths, _, cx| {
                                            view.handle_dropped_paths(paths, cx);
                                        },
                                    ))
                                    .p(px(10.))
                                    .flex()
                                    .flex_col()
                                    .gap(px(10.))
                                    .when_some(
                                        self.render_pending_approval_panel(
                                            text_color,
                                            subtext_color,
                                            overlay_color,
                                            surface0,
                                            surface1,
                                            mantle,
                                            font_mono.clone(),
                                            cx,
                                        ),
                                        |this, panel| this.child(panel),
                                    )
                                    .when(!attached_images.is_empty(), {
                                        let this = this.clone();
                                        let attachment_count = attached_images.len();
                                        move |el| {
                                            el.child(
                                                div()
                                                    .w_full()
                                                    .flex()
                                                    .flex_wrap()
                                                    .gap(px(8.))
                                                    .px(px(4.))
                                                    .children(
                                                        attached_images
                                                            .iter()
                                                            .enumerate()
                                                            .map(|(ix, path)| {
                                                                let file_name =
                                                                    AppShell::image_file_name(path);
                                                                let display_name =
                                                                    AppShell::truncate(
                                                                        &file_name,
                                                                        18,
                                                                    );
                                                                let path_clone = path.clone();
                                                                let image_ref =
                                                                    path.to_string_lossy().to_string();
                                                                let preview_size =
                                                                    AppShell::composer_attachment_preview_size(
                                                                        &image_ref,
                                                                        attachment_count,
                                                                    );
                                                                let this = this.clone();

                                                                div()
                                                                    .id(format!(
                                                                        "attachment-{ix}"
                                                                    ))
                                                                    .flex()
                                                                    .items_center()
                                                                    .gap(px(6.))
                                                                    .px(px(8.))
                                                                    .py(px(4.))
                                                                    .rounded(px(10.))
                                                                    .border_1()
                                                                    .border_color(surface1)
                                                                    .bg(surface0)
                                                                    .child(
                                                                        div()
                                                                            .w(px(preview_size.0))
                                                                            .h(px(preview_size.1))
                                                                            .rounded(px(6.))
                                                                            .overflow_hidden()
                                                                            .border_1()
                                                                            .border_color(surface1)
                                                                            .bg(mantle)
                                                                            .child(
                                                                                img(path_clone)
                                                                                    .size_full()
                                                                                    .object_fit(
                                                                                        gpui::ObjectFit::ScaleDown,
                                                                                    ),
                                                                            ),
                                                                    )
                                                                    .child(
                                                                        div()
                                                                            .text_xs()
                                                                            .text_color(
                                                                                text_color,
                                                                            )
                                                                            .max_w(px(120.))
                                                                            .overflow_hidden()
                                                                            .text_ellipsis()
                                                                            .whitespace_nowrap()
                                                                            .child(
                                                                                display_name,
                                                                            ),
                                                                    )
                                                                    .child(
                                                                        div()
                                                                            .cursor_pointer()
                                                                            .on_mouse_down(
                                                                                gpui::MouseButton::Left,
                                                                                {
                                                                                    let this = this.clone();
                                                                                    move |_, _: &mut Window, cx: &mut App| {
                                                                                        let _ = this.update(cx, |view, cx| {
                                                                                            view.remove_image_attachment(ix, cx);
                                                                                        });
                                                                                    }
                                                                                },
                                                                            )
                                                                            .child(
                                                                                Icon::new(
                                                                                    IconName::Close,
                                                                                )
                                                                                .size(px(12.))
                                                                                .text_color(
                                                                                    overlay_color,
                                                                                ),
                                                                            ),
                                                                    )
                                                            }),
                                                    ),
                                            )
                                        }
                                    })
                                    .when_some(skill_picker.clone(), |this, skill_picker| {
                                        this.child(
                                            div()
                                                .id("skill-picker-scroll")
                                                .w_full()
                                                .max_h(px(290.))
                                                .relative()
                                                .rounded(px(16.))
                                                .bg(mantle)
                                                .border_1()
                                                .border_color(surface1)
                                                .overflow_y_scroll()
                                                .track_scroll(&self.skill_picker_scroll_handle)
                                                .flex()
                                                .flex_col()
                                                .items_stretch()
                                                .children(
                                                    skill_picker
                                                        .items
                                                        .iter()
                                                        .enumerate()
                                                        .map(|(ix, skill)| {
                                                            let skill_name = skill.name.clone();
                                                            let display_name = skill_name.clone();
                                                            let description =
                                                                skill.description.clone();
                                                            let scope = if skill.scope.is_empty() {
                                                                "skill".to_string()
                                                            } else {
                                                                skill.scope.clone()
                                                            };
                                                            let is_primary =
                                                                skill_picker_selected_ix
                                                                    == Some(ix);
                                                            div()
                                                                .w_full()
                                                                .min_w_0()
                                                                .px(px(12.))
                                                                .py(px(9.))
                                                                .flex()
                                                                .items_center()
                                                                .justify_between()
                                                                .gap(px(12.))
                                                                .when(is_primary, |this| {
                                                                    this.bg(surface0)
                                                                })
                                                                .hover(|this| this.bg(surface0))
                                                                .cursor_pointer()
                                                                .on_mouse_move(cx.listener(
                                                                    move |view, _, _, cx| {
                                                                        if view.skill_picker_selected_ix
                                                                            != ix
                                                                        {
                                                                            view.skill_picker_selected_ix =
                                                                                ix;
                                                                            cx.notify();
                                                                        }
                                                                    },
                                                                ))
                                                                .on_mouse_down(
                                                                    gpui::MouseButton::Left,
                                                                    cx.listener(
                                                                        move |view,
                                                                              _,
                                                                              window,
                                                                              cx| {
                                                                            view.apply_skill_completion(
                                                                                &skill_name,
                                                                                window,
                                                                                cx,
                                                                            );
                                                                            cx.notify();
                                                                        },
                                                                    ),
                                                                )
                                                                .child(
                                                                    div()
                                                                        .flex()
                                                                        .items_center()
                                                                        .gap(px(10.))
                                                                        .min_w_0()
                                                                        .flex_1()
                                                                        .overflow_x_hidden()
                                                                        .child(
                                                                            Icon::new(
                                                                                IconName::BookOpen,
                                                                            )
                                                                            .size(px(15.))
                                                                            .text_color(
                                                                                overlay_color,
                                                                            ),
                                                                        )
                                                                        .child(
                                                                            div()
                                                                                .flex()
                                                                                .items_center()
                                                                                .gap(px(10.))
                                                                                .min_w_0()
                                                                                .flex_1()
                                                                                .overflow_x_hidden()
                                                                                .child(
                                                                                    div()
                                                                                        .flex_shrink_0()
                                                                                        .max_w(px(340.))
                                                                                        .overflow_hidden()
                                                                                        .text_ellipsis()
                                                                                        .whitespace_nowrap()
                                                                                        .text_sm()
                                                                                        .font_weight(
                                                                                            gpui::FontWeight::MEDIUM,
                                                                                        )
                                                                                        .text_color(text_color)
                                                                                        .child(display_name),
                                                                                )
                                                                                .child(
                                                                                    div()
                                                                                        .flex_1()
                                                                                        .min_w_0()
                                                                                        .overflow_hidden()
                                                                                        .text_ellipsis()
                                                                                        .whitespace_nowrap()
                                                                                        .text_sm()
                                                                                        .text_color(subtext_color)
                                                                                        .child(description),
                                                                                ),
                                                                        ),
                                                                )
                                                                .child(
                                                                    div()
                                                                        .w(px(72.))
                                                                        .flex_shrink_0()
                                                                        .text_right()
                                                                        .text_sm()
                                                                        .text_color(overlay_color)
                                                                        .child(scope),
                                                                )
                                                        }),
                                                )
                                                .vertical_scrollbar(
                                                    &self.skill_picker_scroll_handle,
                                                ),
                                        )
                                    })
                                    .when_some(file_picker.clone(), |this, file_picker| {
                                        this.child(
                                            div()
                                                .id("file-picker-scroll")
                                                .w_full()
                                                .max_h(px(240.))
                                                .relative()
                                                .rounded(px(16.))
                                                .bg(mantle)
                                                .border_1()
                                                .border_color(surface1)
                                                .overflow_y_scroll()
                                                .track_scroll(&self.file_picker_scroll_handle)
                                                .flex()
                                                .flex_col()
                                                .items_stretch()
                                                .children(
                                                    file_picker
                                                        .items
                                                        .iter()
                                                        .enumerate()
                                                        .map(|(ix, file_path)| {
                                                            let file_path = file_path.clone();
                                                            let display_name = file_path
                                                                .rsplit('/')
                                                                .next()
                                                                .unwrap_or(file_path.as_str())
                                                                .to_string();
                                                            let parent_path = file_path
                                                                .rsplit_once('/')
                                                                .map(|(parent, _)| {
                                                                    parent.to_string()
                                                                })
                                                                .unwrap_or_default();
                                                            let is_primary =
                                                                file_picker_selected_ix
                                                                    == Some(ix);
                                                            div()
                                                                .w_full()
                                                                .min_w_0()
                                                                .px(px(12.))
                                                                .py(px(5.))
                                                                .flex()
                                                                .items_center()
                                                                .gap(px(8.))
                                                                .when(is_primary, |this| {
                                                                    this.bg(surface0)
                                                                })
                                                                .hover(|this| this.bg(surface0))
                                                                .cursor_pointer()
                                                                .on_mouse_move(cx.listener(
                                                                    move |view, _, _, cx| {
                                                                        if view.file_picker_selected_ix
                                                                            != ix
                                                                        {
                                                                            view.file_picker_selected_ix =
                                                                                ix;
                                                                            cx.notify();
                                                                        }
                                                                    },
                                                                ))
                                                                .on_mouse_down(
                                                                    gpui::MouseButton::Left,
                                                                    cx.listener(
                                                                        move |view,
                                                                              _,
                                                                              window,
                                                                              cx| {
                                                                            view.apply_file_completion(
                                                                                &file_path,
                                                                                window,
                                                                                cx,
                                                                            );
                                                                            cx.notify();
                                                                        },
                                                                    ),
                                                                )
                                                                .child(
                                                                    Icon::new(
                                                                        IconName::File,
                                                                    )
                                                                    .size(px(13.))
                                                                    .text_color(overlay_color),
                                                                )
                                                                .child(
                                                                    div()
                                                                        .flex()
                                                                        .items_center()
                                                                        .gap(px(6.))
                                                                        .min_w_0()
                                                                        .flex_1()
                                                                        .overflow_x_hidden()
                                                                        .child(
                                                                            div()
                                                                                .flex_shrink_0()
                                                                                .max_w(px(340.))
                                                                                .overflow_hidden()
                                                                                .text_ellipsis()
                                                                                .whitespace_nowrap()
                                                                                .text_sm()
                                                                                .font_weight(
                                                                                    gpui::FontWeight::MEDIUM,
                                                                                )
                                                                                .text_color(
                                                                                    text_color,
                                                                                )
                                                                                .child(
                                                                                    display_name,
                                                                                ),
                                                                        )
                                                                        .when(
                                                                            !parent_path.is_empty(),
                                                                            |this| {
                                                                                this.child(
                                                                                    div()
                                                                                        .flex_1()
                                                                                        .min_w_0()
                                                                                        .overflow_hidden()
                                                                                        .text_ellipsis()
                                                                                        .whitespace_nowrap()
                                                                                        .text_xs()
                                                                                        .text_color(
                                                                                            subtext_color,
                                                                                        )
                                                                                        .child(
                                                                                            format!(
                                                                                                "· {parent_path}"
                                                                                            ),
                                                                                        ),
                                                                                )
                                                                            },
                                                                        ),
                                                                )
                                                        }),
                                                )
                                                .vertical_scrollbar(
                                                    &self.file_picker_scroll_handle,
                                                ),
                                        )
                                    })
                                    .when(show_streaming_status, |this| {
                                        this.child(
                                            div()
                                                .w_full()
                                                .px(px(4.))
                                                .py(px(2.))
                                                .flex()
                                                .items_center()
                                                .gap(px(8.))
                                                .child(
                                                    Icon::new(IconName::LoaderCircle)
                                                        .size(px(14.))
                                                        .text_color(overlay_color)
                                                        .with_animation(
                                                            "composer-streaming-spinner",
                                                            Animation::new(Duration::from_secs(1))
                                                                .repeat(),
                                                            |icon, delta| {
                                                                icon.transform(
                                                                    Transformation::rotate(
                                                                        percentage(delta),
                                                                    ),
                                                                )
                                                            },
                                                        ),
                                                )
                                                .child(
                                                    div()
                                                        .text_xs()
                                                        .text_color(subtext_color)
                                                        .child("Streaming response ..."),
                                                ),
                                        )
                                    })
                                    .child(
                                        div()
                                            .w_full()
                                            .min_h(px(43.))
                                            .capture_action(cx.listener(
                                                |view, _: &InputMoveUp, window, cx| {
                                                    if view.move_completion_picker_selection(-1, cx)
                                                    {
                                                        window.prevent_default();
                                                        cx.stop_propagation();
                                                    }
                                                },
                                            ))
                                            .capture_action(cx.listener(
                                                |view, _: &InputMoveDown, window, cx| {
                                                    if view.move_completion_picker_selection(1, cx)
                                                    {
                                                        window.prevent_default();
                                                        cx.stop_propagation();
                                                    }
                                                },
                                            ))
                                            .capture_action(cx.listener(
                                                |view, action: &InputEnter, window, cx| {
                                                    if !action.secondary
                                                        && view.apply_selected_completion_if_open(
                                                            window,
                                                            cx,
                                                        )
                                                    {
                                                        window.prevent_default();
                                                        cx.stop_propagation();
                                                    }
                                                },
                                            ))
                                            .capture_action(cx.listener(
                                                |view, _: &InputPaste, window, cx| {
                                                    if let Some(clipboard) =
                                                        cx.read_from_clipboard()
                                                    {
                                                        let has_image =
                                                            clipboard.entries().iter().any(|e| {
                                                                matches!(
                                                                    e,
                                                                    ClipboardEntry::Image(_)
                                                                )
                                                            });
                                                        if has_image {
                                                            view.handle_clipboard_paste(cx);
                                                            window.prevent_default();
                                                            cx.stop_propagation();
                                                        }
                                                    }
                                                },
                                            ))
                                            .child(
                                                Input::new(&self.input_state)
                                                    .appearance(false)
                                                    .bordered(false)
                                                    .focus_bordered(false)
                                                    .h_full()
                                                    .text_color(text_color)
                                                    .disabled(
                                                        waiting_for_response
                                                            || self.has_pending_approval()
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
                                                        div()
                                                            .cursor_pointer()
                                                            .on_mouse_down(
                                                                gpui::MouseButton::Left,
                                                                cx.listener(
                                                                    |_view, _, _, cx| {
                                                                        let paths_rx =
                                                                            cx.prompt_for_paths(
                                                                                gpui::PathPromptOptions {
                                                                                    files: true,
                                                                                    directories: false,
                                                                                    multiple: true,
                                                                                    prompt: None,
                                                                                },
                                                                            );
                                                                        cx.spawn(
                                                                            async move |view, cx| {
                                                                                if let Ok(Ok(Some(selected))) = paths_rx.await {
                                                                                    let _ = view.update(cx, |view, cx| {
                                                                                        for path in selected {
                                                                                            if AppShell::is_image_path(&path) {
                                                                                                view.add_image_attachment(path, cx);
                                                                                            }
                                                                                        }
                                                                                    });
                                                                                }
                                                                            },
                                                                        )
                                                                        .detach();
                                                                    },
                                                                ),
                                                            )
                                                            .child(
                                                                Icon::new(IconName::Plus)
                                                                    .size(px(24.))
                                                                    .text_color(overlay_color),
                                                            ),
                                                    )
                                                    .child(
                                                        Button::new("composer-model-selector")
                                                            .ghost()
                                                            .xsmall()
                                                            .label(model_label)
                                                            .dropdown_caret(true)
                                                            .text_color(overlay_color)
                                                            .disabled(
                                                                self.loading_model_options
                                                                    || available_models.is_empty(),
                                                            )
                                                            .dropdown_menu_with_anchor(
                                                                gpui::Corner::BottomLeft,
                                                                {
                                                                    let this = this.clone();
                                                                    let available_models =
                                                                        available_models.clone();
                                                                    let selected_model =
                                                                        selected_model.clone();
                                                                    move |menu: PopupMenu,
                                                                          _window: &mut Window,
                                                                          _cx: &mut gpui::Context<
                                                                        PopupMenu,
                                                                    >| {
                                                                        let mut menu = menu;
                                                                        if available_models.is_empty() {
                                                                            return menu.label(
                                                                                "No models available",
                                                                            );
                                                                        }

                                                                        for option in &available_models {
                                                                            let model =
                                                                                option.model.clone();
                                                                            let label = option
                                                                                .display_name
                                                                                .clone();
                                                                            let checked = model
                                                                                == selected_model;
                                                                            menu = menu.item(
                                                                                PopupMenuItem::new(
                                                                                    label,
                                                                                )
                                                                                .checked(checked)
                                                                                .on_click({
                                                                                    let this = this
                                                                                        .clone();
                                                                                    move |_, _, cx| {
                                                                                        let _ = this
                                                                                            .update(
                                                                                                cx,
                                                                                                |view, cx| {
                                                                                                    view.selected_model =
                                                                                                        Some(model.clone());
                                                                                                    view.normalize_selected_reasoning_effort();
                                                                                                    cx.notify();
                                                                                                },
                                                                                            );
                                                                                    }
                                                                                }),
                                                                            );
                                                                        }

                                                                        menu
                                                                    }
                                                                },
                                                            ),
                                                    )
                                                    .child(
                                                        Button::new("composer-reasoning-selector")
                                                            .ghost()
                                                            .xsmall()
                                                            .label(reasoning_label)
                                                            .dropdown_caret(true)
                                                            .text_color(overlay_color)
                                                            .disabled(
                                                                self.loading_model_options
                                                                    || reasoning_efforts.is_empty(),
                                                            )
                                                            .dropdown_menu_with_anchor(
                                                                gpui::Corner::BottomLeft,
                                                                {
                                                                    let this = this.clone();
                                                                    let reasoning_efforts =
                                                                        reasoning_efforts.clone();
                                                                    let selected_reasoning_effort =
                                                                        selected_reasoning_effort
                                                                            .clone();
                                                                    move |menu: PopupMenu,
                                                                          _window: &mut Window,
                                                                          _cx: &mut gpui::Context<
                                                                        PopupMenu,
                                                                    >| {
                                                                        let mut menu = menu;
                                                                        if reasoning_efforts.is_empty() {
                                                                            return menu.label(
                                                                                "No reasoning options",
                                                                            );
                                                                        }

                                                                        for effort in &reasoning_efforts {
                                                                            let effort =
                                                                                effort.clone();
                                                                            let checked = effort
                                                                                == selected_reasoning_effort;
                                                                            let label = AppShell::format_reasoning_effort_label(
                                                                                &effort,
                                                                            );
                                                                            menu = menu.item(
                                                                                PopupMenuItem::new(
                                                                                    label,
                                                                                )
                                                                                .checked(checked)
                                                                                .on_click({
                                                                                    let this = this
                                                                                        .clone();
                                                                                    move |_, _, cx| {
                                                                                        let _ = this
                                                                                            .update(
                                                                                                cx,
                                                                                                |view, cx| {
                                                                                                    view.selected_reasoning_effort =
                                                                                                        Some(effort.clone());
                                                                                                    cx.notify();
                                                                                                },
                                                                                            );
                                                                                    }
                                                                                }),
                                                                            );
                                                                        }

                                                                        menu
                                                                    }
                                                                },
                                                            ),
                                                    ),
                                            )
                                            .child(
                                                div()
                                                    .flex()
                                                    .items_center()
                                                    .gap(px(12.))
                                                    .when_some(
                                                        self.thread_token_usage.clone(),
                                                        {
                                                            let overlay_color = overlay_color;
                                                            let surface0 = surface0;
                                                            let blue_color = blue_color;
                                                            move |el, usage: ThreadTokenUsage| {
                                                                let used = usage.used_percent();
                                                                let ring_color = if used > 90.0 {
                                                                    gpui::Hsla {
                                                                        h: 0.0,
                                                                        s: 0.7,
                                                                        l: 0.55,
                                                                        a: 1.0,
                                                                    }
                                                                } else if used > 70.0 {
                                                                    gpui::Hsla {
                                                                        h: 40.0 / 360.0,
                                                                        s: 0.8,
                                                                        l: 0.55,
                                                                        a: 1.0,
                                                                    }
                                                                } else {
                                                                    blue_color
                                                                };
                                                                let used_label =
                                                                    ThreadTokenUsage::format_tokens(
                                                                        usage.total_tokens,
                                                                    );
                                                                let total_label =
                                                                    ThreadTokenUsage::format_tokens(
                                                                        usage.model_context_window,
                                                                    );
                                                                let remaining =
                                                                    usage.remaining_percent();
                                                                el.child(
                                                                    div()
                                                                        .id(
                                                                            "context-window-ring",
                                                                        )
                                                                        .cursor_pointer()
                                                                        .tooltip(move |window, cx| {
                                                                            Tooltip::new(format!(
                                                                                "Context: {used:.0}% used ({remaining:.0}% left) · {used_label} / {total_label}"
                                                                            ))
                                                                            .build(window, cx)
                                                                        })
                                                                        .size(px(24.))
                                                                        .child(
                                                                            Self::render_context_ring(
                                                                                px(20.),
                                                                                px(2.5),
                                                                                used as f32
                                                                                    / 100.0,
                                                                                ring_color,
                                                                                surface0,
                                                                                overlay_color,
                                                                            ),
                                                                        ),
                                                                )
                                                            }
                                                        },
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
        let is_dark = app_theme.mode.is_dark();
        let text_color = app_theme.text;
        let blue_color = app_theme.blue;
        let surface1 = app_theme.surface1;
        if is_dark {
            ComponentTheme::change(gpui_component::theme::ThemeMode::Dark, None, cx);
        }
        let theme = ComponentTheme::global_mut(cx);
        theme.foreground = text_color;
        theme.caret = text_color;
        theme.accent = surface1;
        theme.accent_foreground = text_color;
        theme.link = blue_color;
        theme.link_hover = blue_color;
        theme.font_size = px(15.);
        theme.mono_font_size = px(12.);

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

        cx.bind_keys([
            KeyBinding::new("shift-enter", InputEnter { secondary: true }, Some("Input")),
            KeyBinding::new("cmd-q", Quit, None),
        ]);
        cx.set_menus(app_menus());
        cx.activate(true);
    });
}

#[cfg(test)]
mod tests {
    use super::{AppShell, RenderableMessage, ThreadSpeaker, file_token_before_cursor};
    use serde_json::json;
    use std::path::PathBuf;

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
                assert!(!expanded);
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
    fn file_token_before_cursor_detects_at_trigger_with_path_query() {
        let token = file_token_before_cursor("Please inspect @src/main");
        assert_eq!(token, Some((15, "src/main".to_string())));
    }

    #[test]
    fn file_token_before_cursor_ignores_email_like_text() {
        assert_eq!(file_token_before_cursor("owner@example.com"), None);
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
    fn format_file_change_item_renders_paths_and_diff() {
        let item = json!({
            "type": "fileChange",
            "status": "inProgress",
            "changes": [
                {
                    "path": "/tmp/repo/README.md",
                    "kind": { "type": "add" },
                    "diff": "### Agent Hub\n"
                },
                {
                    "path": "/tmp/repo/src/main.rs",
                    "kind": { "type": "update" },
                    "diff": "@@ -1 +1 @@\n-fn old() {}\n+fn new() {}\n"
                }
            ]
        });

        let rendered = AppShell::format_file_change_item(&item).expect("should render");
        match rendered {
            RenderableMessage::FileChange {
                status_label,
                file_diffs,
                expanded,
            } => {
                assert_eq!(status_label, "Running");
                assert!(expanded);
                assert_eq!(file_diffs.len(), 2);
                assert_eq!(file_diffs[0].path, "/tmp/repo/README.md");
                assert_eq!(file_diffs[1].path, "/tmp/repo/src/main.rs");
                assert_eq!(file_diffs[1].additions, 1);
                assert_eq!(file_diffs[1].deletions, 1);
            }
            other => panic!("expected file change renderable message, got {other:?}"),
        }
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
                                "content": [
                                    { "type": "localImage", "path": "/tmp/repo/screenshot.png" },
                                    { "type": "text", "text": "run ls" }
                                ]
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
                                "type": "fileChange",
                                "id": "f1",
                                "status": "completed",
                                "changes": [
                                    {
                                        "path": "/tmp/repo/README.md",
                                        "kind": { "type": "add" },
                                        "diff": "### Agent Hub\n"
                                    }
                                ]
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
        assert_eq!(messages.len(), 5);
        assert!(matches!(
            &messages[0],
            RenderableMessage::Text {
                speaker: ThreadSpeaker::User,
                content,
                image_refs,
            } if content == "run ls" && image_refs == &vec!["/tmp/repo/screenshot.png".to_string()]
        ));
        assert!(matches!(
            &messages[1],
            RenderableMessage::CommandExecution { expanded, .. } if !expanded
        ));
        assert!(matches!(
            &messages[2],
            RenderableMessage::Text {
                speaker: ThreadSpeaker::Assistant,
                content,
                ..
            } if content.contains("**Web search**")
        ));
        assert!(matches!(&messages[3], RenderableMessage::FileChange { .. }));
        assert!(matches!(
            &messages[4],
            RenderableMessage::Text {
                speaker: ThreadSpeaker::Assistant,
                content,
                ..
            } if content == "done"
        ));
    }

    #[test]
    fn renderable_messages_from_thread_response_renders_image_url_blocks() {
        let payload = json!({
            "thread": {
                "turns": [
                    {
                        "items": [
                            {
                                "type": "userMessage",
                                "id": "u1",
                                "content": [
                                    { "type": "image", "url": "data:image/png;base64,AAA" },
                                    { "type": "text", "text": "testing" }
                                ]
                            }
                        ]
                    }
                ]
            }
        });

        let messages = AppShell::renderable_messages_from_thread_response(&payload)
            .expect("thread response should parse");
        assert_eq!(messages.len(), 1);
        assert!(matches!(
            &messages[0],
            RenderableMessage::Text {
                speaker: ThreadSpeaker::User,
                content,
                image_refs,
            } if content == "testing" && image_refs == &vec!["data:image/png;base64,AAA".to_string()]
        ));
    }

    #[test]
    fn build_user_turn_input_places_images_before_text() {
        let images = vec![PathBuf::from("/tmp/repo/screenshot.png")];
        let turn_input = AppShell::build_user_turn_input("run ls", &images, Vec::new());
        assert_eq!(turn_input.len(), 2);
        assert_eq!(
            turn_input[0],
            json!({ "type": "localImage", "path": "/tmp/repo/screenshot.png" })
        );
        assert_eq!(turn_input[1], json!({ "type": "text", "text": "run ls" }));
    }

    #[test]
    fn image_ref_for_message_keeps_absolute_paths_for_local_files() {
        let image_ref = AppShell::image_ref_for_message("/tmp/repo/screenshot.png");
        assert_eq!(image_ref, "/tmp/repo/screenshot.png");
    }

    #[test]
    fn image_ref_for_message_strips_file_scheme_from_local_file_urls() {
        let image_ref = AppShell::image_ref_for_message("file:///tmp/repo/screenshot.png");
        assert_eq!(image_ref, "/tmp/repo/screenshot.png");
    }

    #[test]
    fn fit_size_to_bounds_scales_into_limits_without_changing_ratio() {
        let (width, height) = AppShell::fit_size_to_bounds(2000., 1000., 620., 260.);
        assert!((width - 520.).abs() < 0.01);
        assert!((height - 260.).abs() < 0.01);
    }

    #[test]
    fn message_image_preview_size_uses_safe_default_for_non_local_images() {
        let (single_width, single_height) =
            AppShell::message_image_preview_size("data:image/png;base64,AAA", 1);
        assert!((single_width - 360.).abs() < 0.01);
        assert!((single_height - 220.).abs() < 0.01);

        let (multi_width, multi_height) =
            AppShell::message_image_preview_size("https://example.com/image.png", 3);
        assert!((multi_width - 220.).abs() < 0.01);
        assert!((multi_height - 160.).abs() < 0.01);
    }

    #[test]
    fn composer_attachment_preview_size_uses_safe_default_for_non_local_images() {
        let (single_width, single_height) =
            AppShell::composer_attachment_preview_size("https://example.com/image.png", 1);
        assert!((single_width - 150.).abs() < 0.01);
        assert!((single_height - 96.).abs() < 0.01);

        let (multi_width, multi_height) =
            AppShell::composer_attachment_preview_size("data:image/png;base64,AAA", 3);
        assert!((multi_width - 110.).abs() < 0.01);
        assert!((multi_height - 72.).abs() < 0.01);
    }
}
