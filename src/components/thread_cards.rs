use crate::{AppShell, diff_view};
use gpui::prelude::*;
use gpui::{Context, ScrollHandle, SharedString, UniformListScrollHandle, Window, div, px};
use gpui_component::PixelsExt;
use gpui_component::accordion::Accordion;
use gpui_component::scroll::ScrollableElement;
use std::sync::Arc;

const COMMAND_OUTPUT_MAX_HEIGHT: f32 = 360.;
const COMMAND_OUTPUT_ROW_HEIGHT: f32 = 24.;
const FILE_CHANGE_CONTENT_MAX_HEIGHT: f32 = 420.;
const FILE_DIFF_CARD_GAP: f32 = 12.;
const FILE_DIFF_HEADER_HEIGHT: f32 = 34.;
const FILE_DIFF_ROW_HEIGHT: f32 = 22.;
const FILE_DIFF_VIRTUAL_OVERDRAW_ROWS: f32 = 24.;

fn command_output_panel_height(line_count: usize) -> f32 {
    (line_count.max(1) as f32 * COMMAND_OUTPUT_ROW_HEIGHT).min(COMMAND_OUTPUT_MAX_HEIGHT)
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn render_command_execution_card(
    message_ix: usize,
    header: &str,
    status_label: &str,
    expanded: bool,
    command_output_lines: Arc<Vec<SharedString>>,
    text_color: gpui::Hsla,
    surface0: gpui::Hsla,
    font_mono: &SharedString,
    window: &mut Window,
    cx: &mut Context<AppShell>,
) -> gpui::AnyElement {
    let command_row_accordion_id = format!("command-row-{message_ix}");
    let command_output_scroll_state_id = format!("command-output-scroll-state-{message_ix}");
    let command_output_line_count = command_output_lines.len().max(1);
    let command_output_scroll_handle = window
        .use_keyed_state(command_output_scroll_state_id, cx, |_, _| {
            UniformListScrollHandle::new()
        })
        .read(cx)
        .clone();
    let command_output_height = command_output_panel_height(command_output_line_count);
    let header = header.to_string();
    let status_label = status_label.to_string();
    let font_mono = font_mono.clone();

    div()
        .w_full()
        .mb(px(10.))
        .flex()
        .justify_start()
        .child(
            div().w_full().min_w_0().child(
                Accordion::new(command_row_accordion_id)
                    .item(|this| {
                        this.open(expanded)
                            .title(format!("{header} · {status_label}"))
                            .child(
                                div()
                                    .w_full()
                                    .rounded(px(6.))
                                    .border_1()
                                    .border_color(surface0)
                                    .bg(surface0)
                                    .overflow_hidden()
                                    .relative()
                                    .child(
                                        gpui::uniform_list(
                                            format!("command-output-list-{message_ix}"),
                                            command_output_line_count,
                                            {
                                            let command_output_lines = command_output_lines.clone();
                                            let font_mono = font_mono.clone();
                                            move |range, _, _| {
                                                range
                                                    .map(|line_ix| {
                                                        let line = command_output_lines
                                                            .get(line_ix)
                                                            .cloned()
                                                            .unwrap_or_default();

                                                        div()
                                                            .id(format!(
                                                                "command-output-line-{message_ix}-{line_ix}"
                                                            ))
                                                            .w_full()
                                                            .h(px(COMMAND_OUTPUT_ROW_HEIGHT))
                                                            .px(px(12.))
                                                            .py(px(2.))
                                                            .font_family(font_mono.clone())
                                                            .text_sm()
                                                            .text_color(text_color)
                                                            .whitespace_nowrap()
                                                            .overflow_hidden()
                                                            .child(line)
                                                            .into_any_element()
                                                    })
                                                    .collect::<Vec<_>>()
                                            }
                                            }
                                        )
                                        .track_scroll(&command_output_scroll_handle)
                                        .w_full()
                                        .h(px(command_output_height)),
                                    )
                                    .on_scroll_wheel(cx.listener(
                                        |_, _: &gpui::ScrollWheelEvent, _, cx| {
                                            cx.stop_propagation();
                                        },
                                    ))
                                    .vertical_scrollbar(&command_output_scroll_handle),
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

fn file_diff_visible_row_range(
    row_count: usize,
    rows_top_in_content: gpui::Pixels,
    viewport_top: gpui::Pixels,
    viewport_bottom: gpui::Pixels,
) -> (usize, usize) {
    if row_count == 0 {
        return (0, 0);
    }

    let row_count_f = row_count as f32;
    let row_height_px = px(FILE_DIFF_ROW_HEIGHT);
    let rows_total_height = px(row_count_f * FILE_DIFF_ROW_HEIGHT);
    let rows_bottom_in_content = rows_top_in_content + rows_total_height;

    if viewport_bottom <= rows_top_in_content {
        return (0, 0);
    }
    if viewport_top >= rows_bottom_in_content {
        return (row_count, row_count);
    }

    let overdraw = px(FILE_DIFF_VIRTUAL_OVERDRAW_ROWS * FILE_DIFF_ROW_HEIGHT);
    let local_visible_top = (viewport_top - rows_top_in_content - overdraw).max(px(0.));
    let local_visible_bottom =
        (viewport_bottom - rows_top_in_content + overdraw).min(rows_total_height);

    let start = (local_visible_top.as_f32() / row_height_px.as_f32()).floor() as usize;
    let end = (local_visible_bottom.as_f32() / row_height_px.as_f32()).ceil() as usize;
    (start.min(row_count), end.min(row_count))
}

#[allow(clippy::too_many_arguments)]
fn render_file_diff_row(
    row: &diff_view::DiffRow,
    line_number_column_width: f32,
    text_color: gpui::Hsla,
    subtext_color: gpui::Hsla,
    surface0: gpui::Hsla,
    added_bg: gpui::Hsla,
    removed_bg: gpui::Hsla,
    green_color: gpui::Hsla,
    red_color: gpui::Hsla,
) -> gpui::AnyElement {
    match row {
        diff_view::DiffRow::HunkHeader(h) => div()
            .w_full()
            .h(px(FILE_DIFF_ROW_HEIGHT))
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
                diff_view::DiffLineKind::Added => (added_bg, green_color),
                diff_view::DiffLineKind::Removed => (removed_bg, red_color),
                diff_view::DiffLineKind::Context => (
                    gpui::Hsla::transparent_black(),
                    gpui::Hsla::transparent_black(),
                ),
            };
            let old_ln = line
                .old_lineno
                .map_or_else(|| "\u{00A0}".to_string(), |n| n.to_string());
            let new_ln = line
                .new_lineno
                .map_or_else(|| "\u{00A0}".to_string(), |n| n.to_string());
            let display_text = if line.text.is_empty() {
                "\u{00A0}".to_string()
            } else {
                line.text.replace(' ', "\u{00A0}")
            };

            div()
                .w_full()
                .h(px(FILE_DIFF_ROW_HEIGHT))
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
    render_file_diff_cards_virtualized(
        file_diffs,
        text_color,
        subtext_color,
        surface0,
        surface1,
        mantle,
        green_color,
        red_color,
        font_mono,
        px(0.),
        px(f32::MAX),
        window,
        cx,
    )
}

#[allow(clippy::too_many_arguments)]
fn render_file_diff_cards_virtualized(
    file_diffs: &[diff_view::ParsedFileDiff],
    text_color: gpui::Hsla,
    subtext_color: gpui::Hsla,
    surface0: gpui::Hsla,
    surface1: gpui::Hsla,
    mantle: gpui::Hsla,
    green_color: gpui::Hsla,
    red_color: gpui::Hsla,
    font_mono: &gpui::SharedString,
    viewport_top: gpui::Pixels,
    viewport_bottom: gpui::Pixels,
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

    let mut content_cursor = px(0.);
    file_diffs
        .iter()
        .enumerate()
        .map(|(file_diff_ix, file_diff)| {
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
            let rows_top_in_content = content_cursor + px(FILE_DIFF_HEADER_HEIGHT);
            let row_count = file_diff.rows.len();
            let rows_total_height = px(row_count as f32 * FILE_DIFF_ROW_HEIGHT);
            let (visible_start, visible_end) = file_diff_visible_row_range(
                row_count,
                rows_top_in_content,
                viewport_top,
                viewport_bottom,
            );
            let before_rows = visible_start;
            let after_rows = row_count.saturating_sub(visible_end);
            let before_spacer_height = px(before_rows as f32 * FILE_DIFF_ROW_HEIGHT);
            let after_spacer_height = px(after_rows as f32 * FILE_DIFF_ROW_HEIGHT);
            let is_last = file_diff_ix + 1 == file_diffs.len();
            content_cursor += px(FILE_DIFF_HEADER_HEIGHT) + rows_total_height;
            if !is_last {
                content_cursor += px(FILE_DIFF_CARD_GAP);
            }

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
                        .h(px(FILE_DIFF_HEADER_HEIGHT))
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
                                        .when(before_spacer_height > px(0.), |this| {
                                            this.child(div().w_full().h(before_spacer_height))
                                        })
                                        .children(
                                            file_diff.rows[visible_start..visible_end].iter().map(
                                                |row| {
                                                    render_file_diff_row(
                                                        row,
                                                        line_number_column_width,
                                                        text_color,
                                                        subtext_color,
                                                        surface0,
                                                        added_bg,
                                                        removed_bg,
                                                        green_color,
                                                        red_color,
                                                    )
                                                },
                                            ),
                                        )
                                        .when(after_spacer_height > px(0.), |this| {
                                            this.child(div().w_full().h(after_spacer_height))
                                        }),
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
    let file_change_scroll_id = format!("file-change-scroll-{message_ix}");
    let file_change_scroll_state_id = format!("file-change-scroll-state-{message_ix}");
    let file_change_scroll_handle = window
        .use_keyed_state(file_change_scroll_state_id, cx, |_, _| ScrollHandle::new())
        .read(cx)
        .clone();
    let viewport_top = -file_change_scroll_handle.offset().y;
    let viewport_bottom = viewport_top + px(FILE_CHANGE_CONTENT_MAX_HEIGHT);
    let status_label = status_label.to_string();
    let total_files = file_diffs.len();

    div()
        .w_full()
        .mb(px(10.))
        .flex()
        .justify_start()
        .child(
            div().w_full().min_w_0().child(
                Accordion::new(file_change_accordion_id)
                    .item(|this| {
                        let header_text = if total_files == 1 {
                            format!("1 file changed · {status_label}")
                        } else {
                            format!("{total_files} files changed · {status_label}")
                        };

                        this.open(expanded).title(header_text).child(
                            div()
                                .w_full()
                                .relative()
                                .child(
                                    div()
                                        .id(file_change_scroll_id)
                                        .w_full()
                                        .max_h(px(FILE_CHANGE_CONTENT_MAX_HEIGHT))
                                        .overflow_y_scroll()
                                        .track_scroll(&file_change_scroll_handle)
                                        .on_scroll_wheel(cx.listener(
                                            |_, _: &gpui::ScrollWheelEvent, _, cx| {
                                                cx.stop_propagation();
                                            },
                                        ))
                                        .child(
                                            div().w_full().flex().flex_col().gap(px(12.)).children(
                                                render_file_diff_cards_virtualized(
                                                    file_diffs,
                                                    text_color,
                                                    subtext_color,
                                                    surface0,
                                                    surface1,
                                                    mantle,
                                                    green_color,
                                                    red_color,
                                                    font_mono,
                                                    viewport_top,
                                                    viewport_bottom,
                                                    window,
                                                    cx,
                                                ),
                                            ),
                                        ),
                                )
                                .vertical_scrollbar(&file_change_scroll_handle),
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
            div().w_full().min_w_0().child(
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
