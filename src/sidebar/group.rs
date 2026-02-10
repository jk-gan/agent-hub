use super::SidebarItem;
use gpui::{
    AnyElement, App, ElementId, IntoElement, ParentElement, SharedString, Styled as _, Window, div,
    prelude::FluentBuilder as _,
};
use gpui_component::{ActiveTheme, Collapsible, h_flex, v_flex};
use std::rc::Rc;

/// A group of items in the [`super::Sidebar`].
#[derive(Clone)]
pub struct SidebarGroup<E: SidebarItem + 'static> {
    label: SharedString,
    collapsed: bool,
    children: Vec<E>,
    suffix: Option<Rc<dyn Fn(&mut Window, &mut App) -> AnyElement + 'static>>,
}

impl<E: SidebarItem> SidebarGroup<E> {
    /// Create a new [`SidebarGroup`] with the given label.
    pub fn new(label: impl Into<SharedString>) -> Self {
        Self {
            label: label.into(),
            collapsed: false,
            children: Vec::new(),
            suffix: None,
        }
    }

    /// Add a child to the sidebar group, the child should implement [`SidebarItem`].
    pub fn child(mut self, child: E) -> Self {
        self.children.push(child);
        self
    }

    /// Add multiple children to the sidebar group.
    ///
    /// See also [`SidebarGroup::child`].
    pub fn children(mut self, children: impl IntoIterator<Item = E>) -> Self {
        self.children.extend(children);
        self
    }

    /// Set the suffix for the group header.
    pub fn suffix<F, T>(mut self, builder: F) -> Self
    where
        F: Fn(&mut Window, &mut App) -> T + 'static,
        T: IntoElement,
    {
        self.suffix = Some(Rc::new(move |window, cx| {
            builder(window, cx).into_any_element()
        }));
        self
    }
}

impl<E: SidebarItem> Collapsible for SidebarGroup<E> {
    fn is_collapsed(&self) -> bool {
        self.collapsed
    }

    fn collapsed(mut self, collapsed: bool) -> Self {
        self.collapsed = collapsed;
        self
    }
}

impl<E: SidebarItem> SidebarItem for SidebarGroup<E> {
    fn render(
        self,
        id: impl Into<ElementId>,
        window: &mut Window,
        cx: &mut App,
    ) -> impl IntoElement {
        let id = id.into();
        let label = self.label;
        let suffix = self.suffix;

        v_flex()
            .relative()
            .when(!self.collapsed, |this| {
                this.child(
                    h_flex()
                        .flex_shrink_0()
                        .px_2()
                        .justify_between()
                        .items_center()
                        .rounded(cx.theme().radius)
                        .text_xs()
                        .text_color(cx.theme().sidebar_foreground.opacity(0.7))
                        .h_8()
                        .child(label)
                        .when_some(suffix, |this, suffix| this.child(suffix(window, cx))),
                )
            })
            .child(
                div()
                    .gap_2()
                    .flex_col()
                    .children(self.children.into_iter().enumerate().map(|(ix, child)| {
                        child
                            .collapsed(self.collapsed)
                            .render(format!("{}-{}", id, ix), window, cx)
                            .into_any_element()
                    })),
            )
    }
}
