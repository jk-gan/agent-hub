use crate::AppShell;
use gpui::prelude::*;
use gpui::{Context, SharedString, div, px};
use gpui_component::theme::hsl;
use gpui_component::{Icon, IconName};

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

#[allow(clippy::too_many_arguments)]
pub(crate) fn render(
    shell: &AppShell,
    text_color: gpui::Hsla,
    subtext_color: gpui::Hsla,
    overlay_color: gpui::Hsla,
    surface0: gpui::Hsla,
    surface1: gpui::Hsla,
    mantle: gpui::Hsla,
    font_mono: SharedString,
    cx: &mut Context<AppShell>,
) -> Option<gpui::AnyElement> {
    let approval = shell.pending_approvals.first()?.clone();
    let selected_ix = shell
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
    let queue_hint = if shell.pending_approvals.len() > 1 {
        Some(format!("{} pending", shell.pending_approvals.len()))
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

            div()
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
                )
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
