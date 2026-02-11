use crate::{AppShell, diff_view};
use gpui::prelude::*;
use gpui::{Context, Entity, ScrollHandle, Window, div, px};
use gpui_component::accordion::Accordion;
use gpui_component::highlighter::HighlightTheme;
use gpui_component::scroll::ScrollableElement;
use gpui_component::text::{TextView, TextViewState, TextViewStyle};
use std::sync::Arc;

#[allow(clippy::too_many_arguments)]
pub(crate) fn render_command_execution_card(
    message_ix: usize,
    header: &str,
    status_label: &str,
    expanded: bool,
    view_state: &Entity<TextViewState>,
    highlight_theme: Arc<HighlightTheme>,
    is_dark: bool,
    text_color: gpui::Hsla,
    surface0: gpui::Hsla,
    cx: &mut Context<AppShell>,
) -> gpui::AnyElement {
    let command_row_accordion_id = format!("command-row-{message_ix}");
    let header = header.to_string();
    let status_label = status_label.to_string();

    div()
        .w_full()
        .mb(px(10.))
        .flex()
        .justify_start()
        .child(
            div().w_full().max_w(px(930.)).child(
                Accordion::new(command_row_accordion_id)
                    .item(|this| {
                        this.open(expanded)
                            .title(format!("{header} · {status_label}"))
                            .child(
                                TextView::new(view_state)
                                    .style(
                                        TextViewStyle {
                                            highlight_theme,
                                            is_dark,
                                            ..TextViewStyle::default()
                                        }
                                        .code_block(
                                            gpui::StyleRefinement::default()
                                                .bg(surface0)
                                                .text_color(text_color)
                                                .rounded(px(6.))
                                                .px(px(12.))
                                                .py(px(10.)),
                                        ),
                                    )
                                    .text_color(text_color)
                                    .selectable(true),
                            )
                    })
                    .on_toggle_click(cx.listener(move |view, open_ixs: &[usize], _, cx| {
                        if view.set_command_execution_expanded(message_ix, open_ixs.contains(&0)) {
                            cx.notify();
                        }
                    })),
            ),
        )
        .into_any_element()
}

fn file_diff_line_number_column_width(file_diff: &diff_view::ParsedFileDiff) -> f32 {
    let max_lineno = file_diff
        .rows
        .iter()
        .filter_map(|row| match row {
            diff_view::DiffRow::Line(line) => line.old_lineno.max(line.new_lineno),
            diff_view::DiffRow::HunkHeader(_) => None,
        })
        .max()
        .unwrap_or(0);
    let line_number_digits = max_lineno.max(1).to_string().len() as f32;
    (line_number_digits * 9. + 16.).max(44.)
}

fn file_diff_content_min_width(
    file_diff: &diff_view::ParsedFileDiff,
    line_number_column_width: f32,
) -> f32 {
    let max_text_chars = file_diff
        .rows
        .iter()
        .map(|row| match row {
            diff_view::DiffRow::HunkHeader(header) => header.raw.chars().count(),
            diff_view::DiffRow::Line(line) => line.text.chars().count(),
        })
        .max()
        .unwrap_or(0) as f32;

    // Approximate monospace text width to guarantee horizontal scrolling for long lines.
    let monospace_char_px = 8.2;
    let gutters_px = line_number_column_width * 2. + 35.;
    let estimated_width = gutters_px + max_text_chars * monospace_char_px;
    estimated_width.max(line_number_column_width * 2. + 120.)
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn render_file_diff_cards(
    file_diffs: &[diff_view::ParsedFileDiff],
    text_color: gpui::Hsla,
    subtext_color: gpui::Hsla,
    surface0: gpui::Hsla,
    surface1: gpui::Hsla,
    mantle: gpui::Hsla,
    green_color: gpui::Hsla,
    red_color: gpui::Hsla,
    font_mono: &gpui::SharedString,
    window: &mut Window,
    cx: &mut Context<AppShell>,
) -> Vec<gpui::AnyElement> {
    let added_bg = gpui::Hsla {
        a: 0.12,
        ..green_color
    };
    let removed_bg = gpui::Hsla {
        a: 0.12,
        ..red_color
    };

    file_diffs
        .iter()
        .map(|file_diff| {
            let total_add = file_diff.additions;
            let total_del = file_diff.deletions;
            let path = file_diff.path.clone();
            let horizontal_scroll_id = format!("file-diff-lines-scroll-{}", file_diff.path);
            let horizontal_scroll_state_id =
                format!("file-diff-lines-scroll-state-{}", file_diff.path);
            let horizontal_scroll_handle = window
                .use_keyed_state(horizontal_scroll_state_id, cx, |_, _| ScrollHandle::new())
                .read(cx)
                .clone();
            let line_number_column_width = file_diff_line_number_column_width(file_diff);
            let content_min_width =
                file_diff_content_min_width(file_diff, line_number_column_width);

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
                        .bg(mantle)
                        .flex()
                        .items_center()
                        .justify_between()
                        .child(
                            div()
                                .text_xs()
                                .font_family(font_mono.clone())
                                .text_color(text_color)
                                .child(path),
                        )
                        .child(
                            div()
                                .flex()
                                .gap(px(6.))
                                .child(div().text_color(green_color).child(format!("+{total_add}")))
                                .child(div().text_color(red_color).child(format!("-{total_del}"))),
                        ),
                )
                .child(
                    div()
                        .relative()
                        .w_full()
                        .child(
                            div()
                                .id(horizontal_scroll_id)
                                .w_full()
                                .overflow_x_scroll()
                                .track_scroll(&horizontal_scroll_handle)
                                .map(|mut this| {
                                    // Keep vertical wheel events on the outer vertical scroller.
                                    // Horizontal movement still works via horizontal wheel/gesture
                                    // and dragging the explicit horizontal scrollbar.
                                    this.style().restrict_scroll_to_axis = Some(true);
                                    this
                                })
                                .child(
                                    div()
                                        .flex()
                                        .flex_col()
                                        .items_start()
                                        .w(px(content_min_width))
                                        .min_w_full()
                                        .font_family(font_mono.clone())
                                        .text_sm()
                                        .children(file_diff.rows.iter().map(|row| {
                                            match row {
                                                diff_view::DiffRow::HunkHeader(h) => div()
                                                    .w_full()
                                                    .px(px(12.))
                                                    .py(px(4.))
                                                    .bg(surface0)
                                                    .text_color(subtext_color)
                                                    .text_xs()
                                                    .whitespace_nowrap()
                                                    .child(h.raw.clone())
                                                    .into_any_element(),
                                                diff_view::DiffRow::Line(line) => {
                                                    let (row_bg, bar_color) = match line.kind {
                                                        diff_view::DiffLineKind::Added => {
                                                            (added_bg, green_color)
                                                        }
                                                        diff_view::DiffLineKind::Removed => {
                                                            (removed_bg, red_color)
                                                        }
                                                        diff_view::DiffLineKind::Context => (
                                                            gpui::Hsla::transparent_black(),
                                                            gpui::Hsla::transparent_black(),
                                                        ),
                                                    };
                                                    let old_ln = line.old_lineno.map_or_else(
                                                        || "\u{00A0}".to_string(),
                                                        |n| n.to_string(),
                                                    );
                                                    let new_ln = line.new_lineno.map_or_else(
                                                        || "\u{00A0}".to_string(),
                                                        |n| n.to_string(),
                                                    );
                                                    let display_text = if line.text.is_empty() {
                                                        "\u{00A0}".to_string()
                                                    } else {
                                                        line.text.replace(' ', "\u{00A0}")
                                                    };

                                                    div()
                                                        .w_full()
                                                        .flex()
                                                        .flex_row()
                                                        .bg(row_bg)
                                                        .border_l_3()
                                                        .border_color(bar_color)
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
                                                                .flex_shrink_0()
                                                                .px(px(8.))
                                                                .py(px(1.))
                                                                .text_color(text_color)
                                                                .whitespace_nowrap()
                                                                .child(display_text),
                                                        )
                                                        .into_any_element()
                                                }
                                            }
                                        })),
                                ),
                        )
                        .horizontal_scrollbar(&horizontal_scroll_handle),
                )
                .into_any_element()
        })
        .collect()
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn render_file_change_card(
    message_ix: usize,
    status_label: &str,
    file_diffs: &[diff_view::ParsedFileDiff],
    expanded: bool,
    text_color: gpui::Hsla,
    subtext_color: gpui::Hsla,
    surface0: gpui::Hsla,
    surface1: gpui::Hsla,
    mantle: gpui::Hsla,
    green_color: gpui::Hsla,
    red_color: gpui::Hsla,
    font_mono: &gpui::SharedString,
    window: &mut Window,
    cx: &mut Context<AppShell>,
) -> gpui::AnyElement {
    let file_change_accordion_id = format!("file-change-{message_ix}");
    let status_label = status_label.to_string();
    let total_files = file_diffs.len();

    div()
        .w_full()
        .mb(px(10.))
        .flex()
        .justify_start()
        .child(
            div().w_full().max_w(px(930.)).child(
                Accordion::new(file_change_accordion_id)
                    .item(|this| {
                        let header_text = if total_files == 1 {
                            format!("1 file changed · {status_label}")
                        } else {
                            format!("{total_files} files changed · {status_label}")
                        };

                        this.open(expanded).title(header_text).child(
                            div()
                                .flex()
                                .flex_col()
                                .gap(px(12.))
                                .children(render_file_diff_cards(
                                    file_diffs,
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
                                )),
                        )
                    })
                    .on_toggle_click(cx.listener(move |view, open_ixs: &[usize], _, cx| {
                        if view.set_file_change_expanded(message_ix, open_ixs.contains(&0)) {
                            cx.notify();
                        }
                    })),
            ),
        )
        .into_any_element()
}

fn render_web_search_content_card(
    query: &str,
    queries: &[String],
    url: Option<&str>,
    action_type: &str,
    text_color: gpui::Hsla,
    subtext_color: gpui::Hsla,
    surface0: gpui::Hsla,
    surface1: gpui::Hsla,
    mantle: gpui::Hsla,
    font_mono: &gpui::SharedString,
) -> gpui::AnyElement {
    let mut rows = Vec::new();
    if !query.is_empty() {
        rows.push(
            div()
                .w_full()
                .flex()
                .flex_col()
                .gap(px(2.))
                .child(div().text_xs().text_color(subtext_color).child("Query"))
                .child(
                    div()
                        .text_sm()
                        .font_family(font_mono.clone())
                        .text_color(text_color)
                        .child(query.to_string()),
                )
                .into_any_element(),
        );
    }

    if !queries.is_empty() {
        rows.push(
            div()
                .w_full()
                .flex()
                .flex_col()
                .gap(px(4.))
                .child(div().text_xs().text_color(subtext_color).child("Queries"))
                .child(div().flex().flex_col().gap(px(2.)).children(
                    queries.iter().enumerate().map(|(ix, query)| {
                        div()
                            .text_sm()
                            .font_family(font_mono.clone())
                            .text_color(text_color)
                            .child(format!("{}. {query}", ix + 1))
                            .into_any_element()
                    }),
                ))
                .into_any_element(),
        );
    }

    if let Some(url) = url {
        rows.push(
            div()
                .w_full()
                .flex()
                .flex_col()
                .gap(px(2.))
                .child(div().text_xs().text_color(subtext_color).child("Opened"))
                .child(
                    div()
                        .text_sm()
                        .font_family(font_mono.clone())
                        .text_color(text_color)
                        .child(url.to_string()),
                )
                .into_any_element(),
        );
    }

    rows.push(
        div()
            .w_full()
            .flex()
            .flex_col()
            .gap(px(2.))
            .child(div().text_xs().text_color(subtext_color).child("Action"))
            .child(
                div()
                    .text_sm()
                    .font_family(font_mono.clone())
                    .text_color(text_color)
                    .child(action_type.to_string()),
            )
            .into_any_element(),
    );

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
                .bg(mantle)
                .text_xs()
                .font_family(font_mono.clone())
                .text_color(text_color)
                .child("Web search"),
        )
        .child(
            div()
                .w_full()
                .flex()
                .flex_col()
                .gap(px(10.))
                .bg(surface0)
                .px(px(12.))
                .py(px(10.))
                .children(rows),
        )
        .into_any_element()
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn render_web_search_card(
    message_ix: usize,
    query: &str,
    queries: &[String],
    url: Option<&str>,
    action_type: &str,
    action_label: &str,
    expanded: bool,
    text_color: gpui::Hsla,
    subtext_color: gpui::Hsla,
    surface0: gpui::Hsla,
    surface1: gpui::Hsla,
    mantle: gpui::Hsla,
    font_mono: &gpui::SharedString,
    cx: &mut Context<AppShell>,
) -> gpui::AnyElement {
    let web_search_accordion_id = format!("web-search-{message_ix}");
    let query = query.to_string();
    let queries = queries.to_vec();
    let url = url.map(str::to_string);
    let action_type = action_type.to_string();
    let action_label = action_label.to_string();

    div()
        .w_full()
        .mb(px(10.))
        .flex()
        .justify_start()
        .child(
            div().w_full().max_w(px(930.)).child(
                Accordion::new(web_search_accordion_id)
                    .item(|this| {
                        this.open(expanded)
                            .title(format!("Web search · {action_label}"))
                            .child(render_web_search_content_card(
                                &query,
                                &queries,
                                url.as_deref(),
                                &action_type,
                                text_color,
                                subtext_color,
                                surface0,
                                surface1,
                                mantle,
                                font_mono,
                            ))
                    })
                    .on_toggle_click(cx.listener(move |view, open_ixs: &[usize], _, cx| {
                        if view.set_web_search_expanded(message_ix, open_ixs.contains(&0)) {
                            cx.notify();
                        }
                    })),
            ),
        )
        .into_any_element()
}
