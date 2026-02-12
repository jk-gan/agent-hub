use crate::{AppShell, ThreadMessageKind, ThreadSpeaker, components};
use gpui::prelude::*;
use gpui::{Context, Window, div, img, px};
use gpui_component::highlighter::HighlightTheme;
use gpui_component::scroll::ScrollableElement;
use gpui_component::text::{TextView, TextViewStyle};
use gpui_component::{Icon, IconName};
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[allow(clippy::too_many_arguments)]
pub(crate) fn render(
    shell: &AppShell,
    highlight_theme: Arc<HighlightTheme>,
    is_dark: bool,
    text_color: gpui::Hsla,
    subtext_color: gpui::Hsla,
    overlay_color: gpui::Hsla,
    surface0: gpui::Hsla,
    surface1: gpui::Hsla,
    mantle: gpui::Hsla,
    red_color: gpui::Hsla,
    green_color: gpui::Hsla,
    font_mono: &gpui::SharedString,
    window: &mut Window,
    cx: &mut Context<AppShell>,
) -> gpui::AnyElement {
    let scroll_offset = shell.scroll_handle.offset();
    let scroll_max = shell.scroll_handle.max_offset();
    let distance_to_bottom = scroll_max.height + scroll_offset.y;
    let show_scroll_to_bottom =
        !shell.selected_thread_messages.is_empty() && distance_to_bottom > px(8.);

    div()
        .flex_1()
        .min_h_0()
        .relative()
        .child(
            div()
                .relative()
                .size_full()
                .min_h_0()
                .child(
                    div()
                        .id("chat-scroll")
                        .size_full()
                        .min_h_0()
                        .overflow_y_scroll()
                        .track_scroll(&shell.scroll_handle)
                        .px(px(20.))
                        .py(px(16.))
                        .pb(px(152.))
                        .child({
                            div().w_full().flex().justify_center().child(
                                div()
                                    .w_full()
                                    .min_w_0()
                                    .py(px(18.))
                                    .child(if !shell.selected_thread_messages.is_empty() {
                                        let chat_rows = shell
                                            .selected_thread_messages
                                            .iter()
                                            .enumerate()
                                            .map(|(message_ix, message)| {
                                                match message.speaker {
                                                    ThreadSpeaker::User => {
                                                        let image_refs = message.image_refs.clone();
                                                        let image_count = image_refs.len();
                                                        let image_previews = image_refs
                                                            .iter()
                                                            .map(|image_ref| {
                                                                let dimensions = shell
                                                                    .image_dimensions_by_ref
                                                                    .get(image_ref)
                                                                    .copied()
                                                                    .or_else(|| {
                                                                        AppShell::local_image_dimensions(
                                                                            image_ref,
                                                                        )
                                                                    });
                                                                let preview_size =
                                                                    AppShell::message_image_preview_size_from_dimensions(
                                                                        dimensions,
                                                                        image_count,
                                                                    );
                                                                (image_ref.clone(), preview_size)
                                                            })
                                                            .collect::<Vec<_>>();
                                                        let has_text =
                                                            !message.content.trim().is_empty();
                                                        div()
                                                            .w_full()
                                                            .mb(px(14.))
                                                            .flex()
                                                            .justify_end()
                                                            .child(
                                                                div()
                                                                    .w_full()
                                                                    .min_w_0()
                                                                    .max_w(px(700.))
                                                                    .flex()
                                                                    .flex_col()
                                                                    .items_end()
                                                                    .gap(px(8.))
                                                                    .when(!image_refs.is_empty(), {
                                                                        let image_previews =
                                                                            image_previews;
                                                                        move |this| {
                                                                            this.child(
                                                                                div()
                                                                                    .w_full()
                                                                                    .max_w(px(
                                                                                        if image_count <= 1
                                                                                        {
                                                                                            620.
                                                                                        } else {
                                                                                            390.
                                                                                        },
                                                                                    ))
                                                                                    .flex()
                                                                                    .flex_wrap()
                                                                                    .justify_end()
                                                                                    .gap(px(6.))
                                                                                    .children(
                                                                                        image_previews
                                                                                            .into_iter()
                                                                                            .map(
                                                                                                |(
                                                                                                    image_ref,
                                                                                                    preview_size,
                                                                                                )| {
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
                                                                                .min_w_0()
                                                                                .bg(surface0)
                                                                                .rounded(px(14.))
                                                                                .px(px(14.))
                                                                                .py(px(10.))
                                                                                .text_color(text_color)
                                                                                .child(
                                                                                    div()
                                                                                        .w_full()
                                                                                        .min_w_0()
                                                                                        .child(
                                                                                            TextView::new(
                                                                                                &message.view_state,
                                                                                            )
                                                                                            .w_full()
                                                                                            .text_color(
                                                                                                text_color,
                                                                                            )
                                                                                            .selectable(true),
                                                                                        ),
                                                                                ),
                                                                        )
                                                                    }),
                                                            )
                                                            .into_any_element()
                                                    }
                                                    ThreadSpeaker::Assistant => match &message.kind {
                                                        ThreadMessageKind::Text => div()
                                                            .w_full()
                                                            .mb(px(16.))
                                                            .flex()
                                                            .justify_start()
                                                            .child(
                                                                div()
                                                                    .w_full()
                                                                    .min_w_0()
                                                                    .text_color(text_color)
                                                                    .child(
                                                                        div()
                                                                            .w_full()
                                                                            .min_w_0()
                                                                            .child(
                                                                                TextView::new(&message.view_state)
                                                                                    .w_full()
                                                                                    .style(
                                                                                        TextViewStyle {
                                                                                            highlight_theme:
                                                                                                highlight_theme
                                                                                                    .clone(),
                                                                                            is_dark,
                                                                                            ..TextViewStyle::default()
                                                                                        }
                                                                                        .code_block(
                                                                                            gpui::StyleRefinement::default()
                                                                                                .bg(mantle)
                                                                                                .text_color(text_color)
                                                                                                .border_1()
                                                                                                .border_color(surface1)
                                                                                                .rounded(px(6.))
                                                                                                .px(px(12.))
                                                                                                .py(px(10.)),
                                                                                        ),
                                                                                    )
                                                                                    .text_color(text_color)
                                                                                    .selectable(true),
                                                                            ),
                                                                    ),
                                                            )
                                                            .into_any_element(),
                                                        ThreadMessageKind::CommandExecution {
                                                            header,
                                                            status_label,
                                                            expanded,
                                                        } => components::thread_cards::render_command_execution_card(
                                                            message_ix,
                                                            header,
                                                            status_label,
                                                            *expanded,
                                                            &message.view_state,
                                                            highlight_theme.clone(),
                                                            is_dark,
                                                            text_color,
                                                            surface0,
                                                            cx,
                                                        ),
                                                        ThreadMessageKind::FileChange {
                                                            status_label,
                                                            file_diffs,
                                                            expanded,
                                                        } => components::thread_cards::render_file_change_card(
                                                            message_ix,
                                                            status_label,
                                                            file_diffs,
                                                            *expanded,
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
                                                        ),
                                                        ThreadMessageKind::WebSearch {
                                                            details,
                                                            expanded,
                                                        } => {
                                                            let action_label =
                                                                AppShell::web_search_action_label(
                                                                    &details.action_type,
                                                                );
                                                            components::thread_cards::render_web_search_card(
                                                                message_ix,
                                                                &details.query,
                                                                &details.queries,
                                                                details.url.as_deref(),
                                                                &details.action_type,
                                                                &action_label,
                                                                *expanded,
                                                                text_color,
                                                                subtext_color,
                                                                surface0,
                                                                surface1,
                                                                mantle,
                                                                font_mono,
                                                                cx,
                                                            )
                                                        }
                                                    },
                                                }
                                            })
                                            .collect::<Vec<_>>();
                                        let mut container = div().w_full().children(chat_rows);
                                        if let Some((_msg, view_state)) = &shell.selected_thread_error {
                                            container = container.child(
                                                div()
                                                    .mt(px(8.))
                                                    .px(px(16.))
                                                    .py(px(8.))
                                                    .rounded(px(6.))
                                                    .bg(red_color.opacity(0.1))
                                                    .text_sm()
                                                    .text_color(red_color)
                                                    .child(TextView::new(view_state).text_color(red_color)),
                                            );
                                        }
                                        container.into_any_element()
                                    } else if let Some((_msg, view_state)) = &shell.selected_thread_error {
                                        div()
                                            .text_sm()
                                            .text_color(red_color)
                                            .child(TextView::new(view_state).text_color(red_color))
                                            .into_any_element()
                                    } else if shell.loading_threads || shell.loading_selected_thread {
                                        div()
                                            .text_sm()
                                            .text_color(subtext_color)
                                            .child("Loading thread content...")
                                            .into_any_element()
                                    } else if let Some(cwd) = &shell.composing_new_thread_cwd {
                                        let workspace_name = AppShell::workspace_name(cwd);
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
                                            .child(div().text_xl().text_color(text_color).child("New thread"))
                                            .child(
                                                div()
                                                    .text_sm()
                                                    .text_color(subtext_color)
                                                    .child(format!(
                                                        "Start your first request in {workspace_name}."
                                                    )),
                                            )
                                            .into_any_element()
                                    } else if shell.selected_thread_id.is_some() {
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
                                    }),
                            )
                        }),
                )
                .vertical_scrollbar(&shell.scroll_handle),
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
                            .child(div().text_xs().text_color(overlay_color).child("Bottom")),
                    ),
            )
        })
        .into_any_element()
}
