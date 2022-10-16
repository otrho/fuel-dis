use colorful::RGB;

pub(crate) trait RgbScheme {
    fn const_reg() -> RGB;
    fn gp_reg() -> RGB;
    fn label() -> RGB;
    fn literal() -> RGB;
    fn opcode() -> RGB;
    fn constant() -> RGB;
    fn hex_dump() -> RGB;
    fn address() -> RGB;
    fn comment() -> RGB;
}

// Works well on a dark background.
pub(crate) struct Pastel;

impl RgbScheme for Pastel {
    fn const_reg() -> RGB {
        // Yellow.
        RGB::new(255, 250, 129)
    }

    fn gp_reg() -> RGB {
        // Green.
        RGB::new(191, 228, 118)
    }

    fn label() -> RGB {
        // Violet.
        RGB::new(193, 179, 215)
    }

    fn literal() -> RGB {
        // Red.
        RGB::new(250, 170, 174)
    }

    fn opcode() -> RGB {
        // Blue.
        RGB::new(154, 206, 223)
    }

    fn constant() -> RGB {
        // Orange.
        RGB::new(253, 203, 152)
    }

    fn hex_dump() -> RGB {
        // Grey.
        RGB::new(160, 160, 160)
    }

    fn address() -> RGB {
        // Also grey.
        RGB::new(160, 160, 160)
    }

    fn comment() -> RGB {
        // Darker grey.
        RGB::new(100, 100, 100)
    }
}
