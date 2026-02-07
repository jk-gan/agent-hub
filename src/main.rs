mod codex;

use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use codex::app_server::{AppServer, RequestMethod};
use gpui::prelude::*;
use gpui::{
    App, Application, Bounds, Context, Render, Window, WindowBounds, WindowOptions, div, px, rgb,
    size,
};
use gpui_component::sidebar::{
    Sidebar, SidebarFooter, SidebarGroup, SidebarHeader, SidebarMenu, SidebarMenuItem,
    SidebarToggleButton,
};
use gpui_component::{Icon, IconName};
use gpui_component_assets::Assets;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

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

struct AppShell {
    started: bool,
    loading_threads: bool,
    sidebar_collapsed: bool,
    thread_error: Option<String>,
    cwd: String,
    cache_path: PathBuf,
    raw_cache_path: PathBuf,
    threads: Vec<ThreadRow>,
    expanded_workspace_threads: HashSet<String>,
    _app_server: Option<Arc<AppServer>>,
}

impl AppShell {
    const MAX_THREADS_PER_WORKSPACE: usize = 10;

    fn new() -> Self {
        let cwd = Self::current_cwd();
        let cache_path = Self::thread_cache_path(&cwd);
        let raw_cache_path = Self::raw_thread_cache_path(&cwd);
        let (threads, thread_error) = match Self::read_threads_cache(&cache_path) {
            Ok(rows) => (rows, None),
            Err(error) => (Vec::new(), Some(format!("cache read failed: {error}"))),
        };

        Self {
            started: false,
            loading_threads: false,
            sidebar_collapsed: false,
            thread_error,
            cwd,
            cache_path,
            raw_cache_path,
            threads,
            expanded_workspace_threads: HashSet::new(),
            _app_server: None,
        }
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
                }
            })
            .collect::<Vec<_>>();

        rows.sort_by_key(|row| Reverse(row.updated_at.unwrap_or(0)));
        rows.truncate(50);

        Ok(rows)
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
                        match response {
                            Ok(payload) => match Self::parse_thread_rows(payload.clone(), &cwd) {
                                Ok(rows) => {
                                    view.thread_error = None;
                                    view.threads = rows;
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
}

impl Render for AppShell {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        self.maybe_load_threads(cx);

        let workspace_groups = Self::group_threads_by_workspace(&self.threads, &self.cwd);
        let this = cx.entity().downgrade();

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
                        let mut item = SidebarMenuItem::new(row.title.clone()).icon(IconName::Bot);
                        if let Some(updated_at) = row.updated_at {
                            item =
                                item.suffix(div().text_xs().child(Self::relative_time(updated_at)));
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
                        .suffix(div().text_xs().child(group.threads.len().to_string()))
                        .children(children),
                );
            }
            items
        };

        div()
            .size_full()
            .bg(rgb(0xe5e5e5))
            .flex()
            .flex_row()
            .child(
                Sidebar::left()
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
                                    SidebarToggleButton::left()
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
                                    .suffix(div().text_xs().child("2d")),
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
            .child(div().w(px(1.)).h_full().bg(rgb(0xb2b2b2)))
            .child(
                div().flex_1().h_full().bg(rgb(0xf0f0f0)).child(
                    div()
                        .w_full()
                        .h(px(44.))
                        .px(px(16.))
                        .flex()
                        .items_center()
                        .text_sm()
                        .text_color(rgb(0x262626))
                        .child("New thread"),
                ),
            )
    }
}

fn main() {
    Application::new().with_assets(Assets).run(|cx: &mut App| {
        gpui_component::init(cx);

        let bounds = Bounds::centered(None, size(px(1_520.), px(920.)), cx);
        cx.open_window(
            WindowOptions {
                window_bounds: Some(WindowBounds::Windowed(bounds)),
                titlebar: Some(gpui::TitlebarOptions {
                    title: Some("Agent Hub".into()),
                    appears_transparent: true,
                    traffic_light_position: Some(gpui::point(px(14.), px(12.))),
                }),
                ..Default::default()
            },
            |_window, cx| cx.new(|_| AppShell::new()),
        )
        .unwrap();
    });
}
