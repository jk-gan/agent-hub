use crate::app_assets::Assets;
use catppuccin::FlavorName;
use gpui::*;

fn color_to_hsla(color: catppuccin::Color) -> Hsla {
    Rgba {
        r: color.rgb.r as f32 / 255.0,
        g: color.rgb.g as f32 / 255.0,
        b: color.rgb.b as f32 / 255.0,
        a: 1.0,
    }
    .into()
}

#[derive(Debug)]
pub enum ThemeMode {
    Light,
    Dark,
}

impl ThemeMode {
    pub fn is_dark(&self) -> bool {
        matches!(self, Self::Dark)
    }
}

#[derive(Debug)]
pub struct Theme {
    pub name: SharedString,
    pub mode: ThemeMode,
    pub font_sans: SharedString,
    pub font_mono: SharedString,
    pub rosewater: Hsla,
    pub flamingo: Hsla,
    pub pink: Hsla,
    pub mauve: Hsla,
    pub red: Hsla,
    pub maroon: Hsla,
    pub peach: Hsla,
    pub yellow: Hsla,
    pub green: Hsla,
    pub teal: Hsla,
    pub sky: Hsla,
    pub sapphire: Hsla,
    pub blue: Hsla,
    pub lavender: Hsla,
    pub text: Hsla,
    pub subtext1: Hsla,
    pub subtext0: Hsla,
    pub overlay2: Hsla,
    pub overlay1: Hsla,
    pub overlay0: Hsla,
    pub surface2: Hsla,
    pub surface1: Hsla,
    pub surface0: Hsla,
    pub base: Hsla,
    pub mantle: Hsla,
    pub crust: Hsla,
}

impl From<&catppuccin::Flavor> for Theme {
    fn from(flavor: &catppuccin::Flavor) -> Self {
        let colors = flavor.colors;
        let name = flavor.name;
        let mode = match name {
            FlavorName::Latte => ThemeMode::Light,
            FlavorName::Frappe | FlavorName::Macchiato | FlavorName::Mocha => ThemeMode::Dark,
        };

        Self {
            name: name.to_string().into(),
            mode,
            font_sans: "Inter".into(),
            // font_sans: "Satoshi".into(),
            font_mono: "Fira Code".into(),
            rosewater: color_to_hsla(colors.rosewater),
            flamingo: color_to_hsla(colors.flamingo),
            pink: color_to_hsla(colors.pink),
            mauve: color_to_hsla(colors.mauve),
            red: color_to_hsla(colors.red),
            maroon: color_to_hsla(colors.maroon),
            peach: color_to_hsla(colors.peach),
            yellow: color_to_hsla(colors.yellow),
            green: color_to_hsla(colors.green),
            teal: color_to_hsla(colors.teal),
            sky: color_to_hsla(colors.sky),
            sapphire: color_to_hsla(colors.sapphire),
            blue: color_to_hsla(colors.blue),
            lavender: color_to_hsla(colors.lavender),
            text: color_to_hsla(colors.text),
            subtext1: color_to_hsla(colors.subtext1),
            subtext0: color_to_hsla(colors.subtext0),
            overlay2: color_to_hsla(colors.overlay2),
            overlay1: color_to_hsla(colors.overlay1),
            overlay0: color_to_hsla(colors.overlay0),
            surface2: color_to_hsla(colors.surface2),
            surface1: color_to_hsla(colors.surface1),
            surface0: color_to_hsla(colors.surface0),
            base: color_to_hsla(colors.base),
            mantle: color_to_hsla(colors.mantle),
            crust: color_to_hsla(colors.crust),
        }
    }
}

impl Global for Theme {}

impl Theme {
    pub fn init(cx: &mut gpui::App) {
        Assets.load_fonts(cx).expect("Failed to load fonts");
        let appearance = cx.window_appearance();

        cx.set_global(Self::load_theme(appearance));
    }

    // pub fn request_method_color(&self, method: &RequestMethod) -> Hsla {
    //     match method {
    //         RequestMethod::Get => self.teal,
    //         RequestMethod::Post => self.blue,
    //         RequestMethod::Put => self.yellow,
    //         RequestMethod::Delete => self.red,
    //     }
    // }

    pub fn load_theme(_appearance: WindowAppearance) -> Theme {
        catppuccin::PALETTE.get_flavor(FlavorName::Latte).into()
        // match appearance {
        //     gpui::WindowAppearance::Light | gpui::WindowAppearance::VibrantLight => {
        //         catppuccin::PALETTE.get_flavor(FlavorName::Latte)
        //     }
        //     gpui::WindowAppearance::Dark | gpui::WindowAppearance::VibrantDark => {
        //         catppuccin::PALETTE.get_flavor(FlavorName::Mocha)
        //     }
        // }
        // .into()
    }

    pub fn sync_system_appearance(cx: &mut impl std::borrow::BorrowMut<App>) {
        let appearance = std::borrow::BorrowMut::borrow_mut(cx).window_appearance();
        cx.update_global::<Theme, _>(|this, _cx| {
            *this = Self::load_theme(appearance);
        });
        std::borrow::BorrowMut::borrow_mut(cx).refresh_windows();
    }
}
