use crate::{AppShell, DraggedBranchDiffSidebarResize};
use gpui::prelude::*;
use gpui::{Context, Window, div, px};
use gpui_component::scroll::ScrollableElement;
use gpui_component::{Icon, IconName};

#[allow(clippy::too_many_arguments)]
pub(crate) fn render(
    shell: &AppShell,
    text_color: gpui::Hsla,
    subtext_color: gpui::Hsla,
    overlay_color: gpui::Hsla,
    surface0: gpui::Hsla,
    surface1: gpui::Hsla,
    mantle: gpui::Hsla,
    divider_color: gpui::Hsla,
    red_color: gpui::Hsla,
    green_color: gpui::Hsla,
    font_mono: &gpui::SharedString,
    window: &mut Window,
    cx: &mut Context<AppShell>,
) -> gpui::AnyElement {
    let snapshot = shell.branch_diff_snapshot.as_ref();
    let branch_diff_error = shell.branch_diff_error.clone();
    let branch_diff_loading = shell.branch_diff_loading;
    let selected_path = shell.branch_diff_selected_path.as_deref();

    div()
        .relative()
        .track_focus(&shell.branch_diff_focus_handle)
        .w(shell.branch_diff_panel_width)
        .h_full()
        .flex_shrink_0()
        .bg(mantle)
        .flex()
        .flex_col()
        .on_mouse_down(
            gpui::MouseButton::Left,
            cx.listener(|view, _, window, cx| {
                view.branch_diff_focus_handle.focus(window, cx);
            }),
        )
        .child(
            div()
                .id("branch-diff-resize-handle")
                .absolute()
                .top(px(0.))
                .left(px(-3.))
                .h_full()
                .w(px(6.))
                .cursor_col_resize()
                .occlude()
                .on_mouse_down(
                    gpui::MouseButton::Left,
                    cx.listener(|_, _, _, cx| {
                        cx.stop_propagation();
                    }),
                )
                .on_drag(DraggedBranchDiffSidebarResize, |drag, _, _, cx| {
                    cx.new(|_| drag.clone())
                }),
        )
        .child(
            div()
                .w_full()
                .h(px(52.))
                .px(px(12.))
                .flex()
                .items_center()
                .justify_between()
                .child(
                    div()
                        .flex()
                        .flex_col()
                        .gap(px(2.))
                        .child(
                            div()
                                .text_sm()
                                .text_color(text_color)
                                .child("Workspace changes"),
                        )
                        .child({
                            if let Some(snapshot) = snapshot {
                                let summary = if snapshot.is_git_repository {
                                    format!("{} · {}", snapshot.branch_name, snapshot.base_ref)
                                } else {
                                    "Git not initialized in this workspace".to_string()
                                };

                                div()
                                    .text_xs()
                                    .text_color(subtext_color)
                                    .child(summary)
                                    .into_any_element()
                            } else {
                                div()
                                    .text_xs()
                                    .text_color(subtext_color)
                                    .child("Uncommitted changes")
                                    .into_any_element()
                            }
                        }),
                )
                .child(
                    div()
                        .flex()
                        .items_center()
                        .gap(px(8.))
                        .child(
                            div()
                                .id("branch-diff-refresh-button")
                                .h(px(28.))
                                .px(px(10.))
                                .rounded(px(999.))
                                .flex()
                                .items_center()
                                .justify_center()
                                .cursor_pointer()
                                .text_xs()
                                .text_color(if branch_diff_loading {
                                    subtext_color
                                } else {
                                    text_color
                                })
                                .bg(if branch_diff_loading {
                                    surface0
                                } else {
                                    gpui::Hsla::transparent_black()
                                })
                                .when(!branch_diff_loading, |this| {
                                    this.hover(|this| this.bg(surface0)).on_mouse_down(
                                        gpui::MouseButton::Left,
                                        cx.listener(move |view, _, _, cx| {
                                            view.request_branch_diff_refresh(cx);
                                            cx.notify();
                                        }),
                                    )
                                })
                                .child("Refresh"),
                        )
                        .child(
                            div()
                                .id("branch-diff-close-button")
                                .h(px(28.))
                                .w(px(28.))
                                .rounded(px(999.))
                                .flex()
                                .items_center()
                                .justify_center()
                                .cursor_pointer()
                                .hover(|this| this.bg(surface0))
                                .on_mouse_down(
                                    gpui::MouseButton::Left,
                                    cx.listener(move |view, _, _, cx| {
                                        view.branch_diff_sidebar_open = false;
                                        cx.notify();
                                    }),
                                )
                                .child(
                                    Icon::new(IconName::PanelRightClose)
                                        .size(px(16.))
                                        .text_color(overlay_color),
                                ),
                        ),
                ),
        )
        .child(div().w_full().h(px(1.)).bg(divider_color))
        .child(div().flex_1().min_h_0().px(px(12.)).py(px(12.)).child(
            if branch_diff_loading && snapshot.is_none() {
                div()
                    .text_sm()
                    .text_color(subtext_color)
                    .child("Loading workspace changes...")
                    .into_any_element()
            } else if let Some(error) = branch_diff_error {
                div()
                    .w_full()
                    .rounded(px(8.))
                    .border_1()
                    .border_color(surface1)
                    .bg(surface0)
                    .px(px(10.))
                    .py(px(8.))
                    .text_sm()
                    .text_color(red_color)
                    .child(error)
                    .into_any_element()
            } else if let Some(snapshot) = snapshot {
                if snapshot.file_diffs.is_empty() {
                    let empty_message = if snapshot.is_git_repository {
                        "No uncommitted changes."
                    } else {
                        "No git diff available for this workspace."
                    };

                    div()
                        .w_full()
                        .rounded(px(8.))
                        .border_1()
                        .border_color(surface1)
                        .bg(surface0)
                        .px(px(10.))
                        .py(px(8.))
                        .text_sm()
                        .text_color(subtext_color)
                        .child(empty_message)
                        .into_any_element()
                } else {
                    let file_count = snapshot.file_diffs.len();
                    let file_label = if file_count == 1 {
                        "1 file".to_string()
                    } else {
                        format!("{file_count} files")
                    };
                    let selected_path = selected_path
                        .filter(|path| {
                            snapshot
                                .file_diffs
                                .iter()
                                .any(|file_diff| file_diff.path == *path)
                        })
                        .or_else(|| {
                            snapshot
                                .file_diffs
                                .first()
                                .map(|file_diff| file_diff.path.as_str())
                        });
                    let selected_file = selected_path.and_then(|path| {
                        snapshot
                            .file_diffs
                            .iter()
                            .find(|file_diff| file_diff.path == path)
                    });

                    div()
                        .w_full()
                        .h_full()
                        .min_h_0()
                        .flex()
                        .flex_col()
                        .gap(px(12.))
                        .child(
                            div()
                                .w_full()
                                .rounded(px(8.))
                                .border_1()
                                .border_color(surface1)
                                .bg(surface0)
                                .px(px(10.))
                                .py(px(8.))
                                .flex()
                                .items_center()
                                .gap(px(8.))
                                .text_sm()
                                .text_color(subtext_color)
                                .child(
                                    div()
                                        .flex_1()
                                        .min_w_0()
                                        .overflow_hidden()
                                        .text_ellipsis()
                                        .whitespace_nowrap()
                                        .text_xs()
                                        .child(snapshot.cwd.clone()),
                                )
                                .child(div().flex_shrink_0().text_xs().child(file_label))
                                .child(
                                    div().flex_shrink_0().text_xs().child(format!(
                                        "+{} -{}",
                                        snapshot.total_additions, snapshot.total_deletions
                                    )),
                                ),
                        )
                        .child(
                            div()
                                .w_full()
                                .flex_1()
                                .min_h_0()
                                .rounded(px(8.))
                                .border_1()
                                .border_color(surface1)
                                .bg(surface0)
                                .overflow_hidden()
                                .flex()
                                .child(
                                    div()
                                        .relative()
                                        .w(px(180.))
                                        .min_w(px(140.))
                                        .max_w(px(220.))
                                        .h_full()
                                        .min_h_0()
                                        .child(
                                            div()
                                                .id("branch-diff-file-list-scroll")
                                                .size_full()
                                                .overflow_y_scroll()
                                                .track_scroll(&shell.branch_diff_file_list_scroll_handle)
                                                .py(px(6.))
                                                .children(snapshot.file_diffs.iter().enumerate().map(
                                                    |(ix, file_diff)| {
                                                        let path = file_diff.path.clone();
                                                        let select_path = path.clone();
                                                        let additions = file_diff.additions;
                                                        let deletions = file_diff.deletions;
                                                        let is_selected = selected_path.is_some_and(
                                                            |selected| selected == path.as_str(),
                                                        );
                                                        div()
                                                            .id(("branch-diff-file", ix))
                                                            .w_full()
                                                            .min_w_0()
                                                            .px(px(10.))
                                                            .py(px(6.))
                                                            .flex()
                                                            .items_center()
                                                            .gap(px(8.))
                                                            .when(is_selected, |this| this.bg(mantle))
                                                            .hover(|this| this.bg(mantle))
                                                            .cursor_pointer()
                                                            .on_mouse_down(
                                                                gpui::MouseButton::Left,
                                                                cx.listener(move |view, _, _, cx| {
                                                                    view.branch_diff_selected_path =
                                                                        Some(select_path.clone());
                                                                    cx.notify();
                                                                }),
                                                            )
                                                            .child(
                                                                div()
                                                                    .flex_1()
                                                                    .min_w_0()
                                                                    .overflow_hidden()
                                                                    .text_ellipsis()
                                                                    .whitespace_nowrap()
                                                                    .text_xs()
                                                                    .text_color(text_color)
                                                                    .child(path),
                                                            )
                                                            .child(
                                                                div()
                                                                    .text_xs()
                                                                    .text_color(subtext_color)
                                                                    .child(format!(
                                                                        "+{} -{}",
                                                                        additions, deletions
                                                                    )),
                                                            )
                                                    },
                                                )),
                                        )
                                        .vertical_scrollbar(&shell.branch_diff_file_list_scroll_handle),
                                )
                                .child(div().w(px(1.)).h_full().bg(surface1))
                                .child(
                                    div()
                                        .relative()
                                        .flex_1()
                                        .min_w_0()
                                        .min_h_0()
                                        .child(
                                            div()
                                                .id("branch-diff-scroll")
                                                .size_full()
                                                .overflow_y_scroll()
                                                .track_scroll(&shell.branch_diff_scroll_handle)
                                                .px(px(10.))
                                                .py(px(10.))
                                                .child(if let Some(file_diff) = selected_file {
                                                    div()
                                                        .w_full()
                                                        .flex()
                                                        .flex_col()
                                                        .gap(px(12.))
                                                        .children(super::thread_cards::render_file_diff_cards(
                                                            std::slice::from_ref(file_diff),
                                                            text_color,
                                                            subtext_color,
                                                            surface0,
                                                            surface1,
                                                            mantle,
                                                            green_color,
                                                            red_color,
                                                            font_mono,
                                                            window,
                                                            cx,
                                                        ))
                                                        .into_any_element()
                                                } else {
                                                    div()
                                                        .text_sm()
                                                        .text_color(subtext_color)
                                                        .child("Select a file to view its diff.")
                                                        .into_any_element()
                                                }),
                                        )
                                        .vertical_scrollbar(&shell.branch_diff_scroll_handle),
                                ),
                        )
                        .into_any_element()
                }
            } else {
                div()
                    .text_sm()
                    .text_color(subtext_color)
                    .child("No changes loaded yet.")
                    .into_any_element()
            },
        ))
        .into_any_element()
}
