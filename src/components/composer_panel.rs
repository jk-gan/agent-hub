use crate::{AppShell, ThreadTokenUsage};
use gpui::prelude::*;
use gpui::{
    Animation, AnimationExt as _, App, ClipboardEntry, Context, ExternalPaths, Transformation,
    Window, div, img, percentage, px,
};
use gpui_component::button::{Button, ButtonVariants as _};
use gpui_component::input::{
    Enter as InputEnter, Input, MoveDown as InputMoveDown, MoveUp as InputMoveUp,
    Paste as InputPaste,
};
use gpui_component::menu::{DropdownMenu as _, PopupMenu, PopupMenuItem};
use gpui_component::scroll::ScrollableElement;
use gpui_component::tooltip::Tooltip;
use gpui_component::{Disableable as _, Icon, IconName, Sizable as _};
use std::time::Duration;

#[allow(clippy::too_many_arguments)]
pub(crate) fn render(
    shell: &AppShell,
    text_color: gpui::Hsla,
    subtext_color: gpui::Hsla,
    overlay_color: gpui::Hsla,
    surface0: gpui::Hsla,
    surface1: gpui::Hsla,
    mantle: gpui::Hsla,
    blue_color: gpui::Hsla,
    red_color: gpui::Hsla,
    crust: gpui::Hsla,
    font_mono: &gpui::SharedString,
    cx: &mut Context<AppShell>,
) -> gpui::AnyElement {
    let this = cx.entity().downgrade();
    let skill_picker = shell.current_skill_picker_state(cx);
    let skill_picker_selected_ix = skill_picker.as_ref().map(|picker| {
        shell
            .skill_picker_selected_ix
            .min(picker.items.len().saturating_sub(1))
    });
    let file_picker = if skill_picker.is_some() {
        None
    } else {
        shell.current_file_picker_state(cx)
    };
    let file_picker_selected_ix = file_picker.as_ref().map(|picker| {
        shell
            .file_picker_selected_ix
            .min(picker.items.len().saturating_sub(1))
    });
    let waiting_for_response = shell.selected_thread_is_busy();
    let show_streaming_status = waiting_for_response && !shell.has_pending_approval();
    let has_text_input = !shell.input_state.read(cx).value().trim().is_empty();
    let has_input = has_text_input || !shell.attached_images.is_empty();
    let can_send = if waiting_for_response {
        has_text_input && shell.can_steer_selected_turn()
    } else {
        has_input
    };
    let can_send = can_send
        && !shell.has_pending_approval()
        && shell.is_authenticated
        && shell._app_server.is_some();
    let show_stop_button = waiting_for_response && !has_text_input;
    let stop_waiting_for_ids = show_stop_button
        && !shell.has_pending_approval()
        && shell.is_authenticated
        && shell._app_server.is_some()
        && !shell.has_interrupt_ids();
    let can_stop = show_stop_button
        && !shell.has_pending_approval()
        && shell.is_authenticated
        && shell._app_server.is_some()
        && shell.has_interrupt_ids();
    let model_label = shell.selected_model_label();
    let reasoning_label = shell.selected_reasoning_label();
    let selected_model = shell.selected_model.clone().unwrap_or_default();
    let selected_reasoning_effort = shell.selected_reasoning_effort.clone().unwrap_or_default();
    let model_menu_items = shell
        .available_models
        .iter()
        .map(|option| (option.model.clone(), option.display_name.clone()))
        .collect::<Vec<_>>();
    let has_model_options = !model_menu_items.is_empty();
    let reasoning_efforts = shell.available_reasoning_efforts();

    div()
        .w_full()
        .flex_shrink_0()
        .px(px(24.))
        .pt(px(10.))
        .pb(px(12.))
        .flex()
        .justify_center()
        .child(
            div()
                .w_full()
                .min_w_0()
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
                .on_drop(cx.listener(|view, paths: &ExternalPaths, _, cx| {
                    view.handle_dropped_paths(paths, cx);
                }))
                .p(px(10.))
                .flex()
                .flex_col()
                .gap(px(10.))
                .when_some(
                    shell.render_pending_approval_panel(
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
                .when(!shell.attached_images.is_empty(), {
                    let this = this.clone();
                    let attached_images = shell.attached_images.clone();
                    move |el| {
                        el.child(
                            div()
                                .w_full()
                                .flex()
                                .flex_wrap()
                                .gap(px(8.))
                                .px(px(4.))
                                .children(attached_images.iter().enumerate().map(|(ix, path)| {
                                    let file_name = AppShell::image_file_name(path);
                                    let display_name = AppShell::truncate(&file_name, 18);
                                    let path_clone = path.clone();
                                    let preview_size = AppShell::composer_attachment_preview_size();
                                    let this = this.clone();

                                    div()
                                        .id(format!("attachment-{ix}"))
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
                                                        .object_fit(gpui::ObjectFit::Cover),
                                                ),
                                        )
                                        .child(
                                            div()
                                                .text_xs()
                                                .text_color(text_color)
                                                .max_w(px(120.))
                                                .overflow_hidden()
                                                .text_ellipsis()
                                                .whitespace_nowrap()
                                                .child(display_name),
                                        )
                                        .child(
                                            div()
                                                .cursor_pointer()
                                                .on_mouse_down(gpui::MouseButton::Left, {
                                                    let this = this.clone();
                                                    move |_, _: &mut Window, cx: &mut App| {
                                                        let _ = this.update(cx, |view, cx| {
                                                            view.remove_image_attachment(ix, cx);
                                                        });
                                                    }
                                                })
                                                .child(
                                                    Icon::new(IconName::Close)
                                                        .size(px(12.))
                                                        .text_color(overlay_color),
                                                ),
                                        )
                                })),
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
                            .track_scroll(&shell.skill_picker_scroll_handle)
                            .flex()
                            .flex_col()
                            .items_stretch()
                            .children(skill_picker.items.iter().enumerate().map(|(ix, skill)| {
                                let skill_name = skill.name.clone();
                                let display_name = skill_name.clone();
                                let description = skill.description.clone();
                                let scope = if skill.scope.is_empty() {
                                    "skill".to_string()
                                } else {
                                    skill.scope.clone()
                                };
                                let is_primary = skill_picker_selected_ix == Some(ix);
                                div()
                                    .w_full()
                                    .min_w_0()
                                    .px(px(12.))
                                    .py(px(9.))
                                    .flex()
                                    .items_center()
                                    .justify_between()
                                    .gap(px(12.))
                                    .when(is_primary, |this| this.bg(surface0))
                                    .hover(|this| this.bg(surface0))
                                    .cursor_pointer()
                                    .on_mouse_move(cx.listener(move |view, _, _, cx| {
                                        if view.skill_picker_selected_ix != ix {
                                            view.skill_picker_selected_ix = ix;
                                            cx.notify();
                                        }
                                    }))
                                    .on_mouse_down(
                                        gpui::MouseButton::Left,
                                        cx.listener(move |view, _, window, cx| {
                                            view.apply_skill_completion(&skill_name, window, cx);
                                            cx.notify();
                                        }),
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
                                                Icon::new(IconName::BookOpen)
                                                    .size(px(15.))
                                                    .text_color(overlay_color),
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
                                                            .font_weight(gpui::FontWeight::MEDIUM)
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
                            }))
                            .vertical_scrollbar(&shell.skill_picker_scroll_handle),
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
                            .track_scroll(&shell.file_picker_scroll_handle)
                            .flex()
                            .flex_col()
                            .items_stretch()
                            .children(file_picker.items.iter().enumerate().map(|(ix, file_path)| {
                                let file_path = file_path.clone();
                                let display_name = file_path
                                    .rsplit('/')
                                    .next()
                                    .unwrap_or(file_path.as_str())
                                    .to_string();
                                let parent_path = file_path
                                    .rsplit_once('/')
                                    .map(|(parent, _)| parent.to_string())
                                    .unwrap_or_default();
                                let is_primary = file_picker_selected_ix == Some(ix);
                                div()
                                    .w_full()
                                    .min_w_0()
                                    .px(px(12.))
                                    .py(px(5.))
                                    .flex()
                                    .items_center()
                                    .gap(px(8.))
                                    .when(is_primary, |this| this.bg(surface0))
                                    .hover(|this| this.bg(surface0))
                                    .cursor_pointer()
                                    .on_mouse_move(cx.listener(move |view, _, _, cx| {
                                        if view.file_picker_selected_ix != ix {
                                            view.file_picker_selected_ix = ix;
                                            cx.notify();
                                        }
                                    }))
                                    .on_mouse_down(
                                        gpui::MouseButton::Left,
                                        cx.listener(move |view, _, window, cx| {
                                            view.apply_file_completion(&file_path, window, cx);
                                            cx.notify();
                                        }),
                                    )
                                    .child(
                                        Icon::new(IconName::File)
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
                                                    .font_weight(gpui::FontWeight::MEDIUM)
                                                    .text_color(text_color)
                                                    .child(display_name),
                                            )
                                            .when(!parent_path.is_empty(), |this| {
                                                this.child(
                                                    div()
                                                        .flex_1()
                                                        .min_w_0()
                                                        .overflow_hidden()
                                                        .text_ellipsis()
                                                        .whitespace_nowrap()
                                                        .text_xs()
                                                        .text_color(subtext_color)
                                                        .child(format!("· {parent_path}")),
                                                )
                                            }),
                                    )
                            }))
                            .vertical_scrollbar(&shell.file_picker_scroll_handle),
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
                                        Animation::new(Duration::from_secs(1)).repeat(),
                                        |icon, delta| {
                                            icon.transform(Transformation::rotate(percentage(delta)))
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
                        .relative()
                        .capture_action(cx.listener(
                            |view, _: &InputMoveUp, window, cx| {
                                if view.move_completion_picker_selection(-1, cx) {
                                    window.prevent_default();
                                    cx.stop_propagation();
                                }
                            },
                        ))
                        .capture_action(cx.listener(
                            |view, _: &InputMoveDown, window, cx| {
                                if view.move_completion_picker_selection(1, cx) {
                                    window.prevent_default();
                                    cx.stop_propagation();
                                }
                            },
                        ))
                        .capture_action(cx.listener(
                            |view, action: &InputEnter, window, cx| {
                                if !action.secondary
                                    && view.apply_selected_completion_if_open(window, cx)
                                {
                                    window.prevent_default();
                                    cx.stop_propagation();
                                }
                            },
                        ))
                        .capture_action(cx.listener(
                            |view, _: &InputPaste, window, cx| {
                                if let Some(clipboard) = cx.read_from_clipboard() {
                                    let has_image = clipboard
                                        .entries()
                                        .iter()
                                        .any(|e| matches!(e, ClipboardEntry::Image(_)));
                                    if has_image {
                                        view.handle_clipboard_paste(cx);
                                        window.prevent_default();
                                        cx.stop_propagation();
                                    }
                                }
                            },
                        ))
                        .child(
                            Input::new(&shell.input_state)
                                .appearance(false)
                                .bordered(false)
                                .focus_bordered(false)
                                .h_full()
                                .text_color(text_color)
                                .disabled(shell.has_pending_approval() || shell._app_server.is_none()),
                        )
                        .child(
                            div()
                                .invisible()
                                .absolute()
                                .top_0()
                                .left_0()
                                .right_0()
                                .bottom_0()
                                .rounded(px(12.))
                                .border_1()
                                .border_dashed()
                                .border_color(blue_color)
                                .bg(gpui::Hsla {
                                    h: blue_color.h,
                                    s: blue_color.s,
                                    l: blue_color.l,
                                    a: 0.14,
                                })
                                .flex()
                                .items_center()
                                .justify_center()
                                .drag_over::<ExternalPaths>(|style, _, _, _| style.visible())
                                .on_drop(cx.listener(|view, paths: &ExternalPaths, _, cx| {
                                    view.handle_dropped_paths(paths, cx);
                                }))
                                .child(
                                    div()
                                        .px(px(10.))
                                        .py(px(4.))
                                        .rounded(px(999.))
                                        .bg(surface0)
                                        .border_1()
                                        .border_color(surface1)
                                        .text_sm()
                                        .font_weight(gpui::FontWeight::MEDIUM)
                                        .text_color(text_color)
                                        .child("Drop image to attach"),
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
                                            cx.listener(|_view, _, _, cx| {
                                                let paths_rx =
                                                    cx.prompt_for_paths(gpui::PathPromptOptions {
                                                        files: true,
                                                        directories: false,
                                                        multiple: true,
                                                        prompt: None,
                                                    });
                                                cx.spawn(async move |view, cx| {
                                                    if let Ok(Ok(Some(selected))) = paths_rx.await {
                                                        let _ = view.update(cx, |view, cx| {
                                                            for path in selected {
                                                                if AppShell::is_image_path(&path) {
                                                                    view.add_image_attachment(
                                                                        path, cx,
                                                                    );
                                                                }
                                                            }
                                                        });
                                                    }
                                                })
                                                .detach();
                                            }),
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
                                            shell.loading_model_options || !has_model_options,
                                        )
                                        .dropdown_menu_with_anchor(gpui::Corner::BottomLeft, {
                                            let this = this.clone();
                                            let model_menu_items = model_menu_items.clone();
                                            let selected_model = selected_model.clone();
                                            move |menu: PopupMenu,
                                                  _window: &mut Window,
                                                  _cx: &mut gpui::Context<PopupMenu>| {
                                                let mut menu = menu;
                                                if model_menu_items.is_empty() {
                                                    return menu.label("No models available");
                                                }

                                                for (model, label) in &model_menu_items {
                                                    let model = model.clone();
                                                    let label = label.clone();
                                                    let checked = model == selected_model;
                                                    menu = menu.item(
                                                        PopupMenuItem::new(label)
                                                            .checked(checked)
                                                            .on_click({
                                                                let this = this.clone();
                                                                move |_, _, cx| {
                                                                    let _ =
                                                                        this.update(cx, |view, cx| {
                                                                            view.selected_model =
                                                                                Some(model.clone());
                                                                            view.normalize_selected_reasoning_effort();
                                                                            cx.notify();
                                                                        });
                                                                }
                                                            }),
                                                    );
                                                }

                                                menu
                                            }
                                        }),
                                )
                                .child(
                                    Button::new("composer-reasoning-selector")
                                        .ghost()
                                        .xsmall()
                                        .label(reasoning_label)
                                        .dropdown_caret(true)
                                        .text_color(overlay_color)
                                        .disabled(
                                            shell.loading_model_options || reasoning_efforts.is_empty(),
                                        )
                                        .dropdown_menu_with_anchor(gpui::Corner::BottomLeft, {
                                            let this = this.clone();
                                            let reasoning_efforts = reasoning_efforts.clone();
                                            let selected_reasoning_effort =
                                                selected_reasoning_effort.clone();
                                            move |menu: PopupMenu,
                                                  _window: &mut Window,
                                                  _cx: &mut gpui::Context<PopupMenu>| {
                                                let mut menu = menu;
                                                if reasoning_efforts.is_empty() {
                                                    return menu.label("No reasoning options");
                                                }

                                                for effort in &reasoning_efforts {
                                                    let effort = effort.clone();
                                                    let checked = effort == selected_reasoning_effort;
                                                    let label =
                                                        AppShell::format_reasoning_effort_label(
                                                            &effort,
                                                        );
                                                    menu = menu.item(
                                                        PopupMenuItem::new(label)
                                                            .checked(checked)
                                                            .on_click({
                                                                let this = this.clone();
                                                                move |_, _, cx| {
                                                                    let _ =
                                                                        this.update(cx, |view, cx| {
                                                                            view.selected_reasoning_effort =
                                                                                Some(effort.clone());
                                                                            cx.notify();
                                                                        });
                                                                }
                                                            }),
                                                    );
                                                }

                                                menu
                                            }
                                        }),
                                ),
                        )
                        .child(
                            div()
                                .flex()
                                .items_center()
                                .gap(px(12.))
                                .when_some(shell.thread_token_usage.clone(), {
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
                                            ThreadTokenUsage::format_tokens(usage.total_tokens);
                                        let total_label = ThreadTokenUsage::format_tokens(
                                            usage.model_context_window,
                                        );
                                        let remaining = usage.remaining_percent();
                                        el.child(
                                            div()
                                                .id("context-window-ring")
                                                .cursor_pointer()
                                                .tooltip(move |window, cx| {
                                                    Tooltip::new(format!(
                                                        "Context: {used:.0}% used ({remaining:.0}% left) · {used_label} / {total_label}"
                                                    ))
                                                    .build(window, cx)
                                                })
                                                .size(px(24.))
                                                .child(AppShell::render_context_ring(
                                                    px(20.),
                                                    px(2.5),
                                                    used as f32 / 100.0,
                                                    ring_color,
                                                    surface0,
                                                    overlay_color,
                                                )),
                                        )
                                    }
                                })
                                .child(if show_stop_button {
                                    if stop_waiting_for_ids {
                                        div()
                                            .size(px(34.))
                                            .rounded(px(999.))
                                            .flex()
                                            .items_center()
                                            .justify_center()
                                            .bg(surface1)
                                            .child(
                                                Icon::new(IconName::LoaderCircle)
                                                    .size(px(18.))
                                                    .text_color(overlay_color)
                                                    .with_animation(
                                                        "stop-button-spinner",
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
                                    } else {
                                        div()
                                            .size(px(34.))
                                            .rounded(px(999.))
                                            .flex()
                                            .items_center()
                                            .justify_center()
                                            .bg(if can_stop { red_color } else { surface1 })
                                            .when(can_stop, |this| {
                                                this.cursor_pointer().on_mouse_down(
                                                    gpui::MouseButton::Left,
                                                    cx.listener(|view, _, _, cx| {
                                                        view.stop_streaming_turn(cx);
                                                    }),
                                                )
                                            })
                                            .child(
                                                Icon::new(IconName::Close)
                                                    .size(px(18.))
                                                    .text_color(crust),
                                            )
                                    }
                                } else {
                                    div()
                                        .size(px(34.))
                                        .rounded(px(999.))
                                        .flex()
                                        .items_center()
                                        .justify_center()
                                        .bg(if can_send { blue_color } else { surface1 })
                                        .when(can_send, |this| {
                                            this.cursor_pointer().on_mouse_down(
                                                gpui::MouseButton::Left,
                                                cx.listener(|view, _, window, cx| {
                                                    view.send_message(window, cx);
                                                }),
                                            )
                                        })
                                        .child(
                                            Icon::new(IconName::ArrowUp)
                                                .size(px(20.))
                                                .text_color(crust),
                                        )
                                }),
                        ),
                ),
        )
        .into_any_element()
}
