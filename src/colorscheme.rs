use colorful::RGB;

pub(crate) trait RgbScheme {
    fn address() -> RGB;
    fn comment() -> RGB;
    fn const_reg() -> RGB;
    fn constant() -> RGB;
    fn error() -> RGB;
    fn gp_reg() -> RGB;
    fn hex_dump() -> RGB;
    fn label() -> RGB;
    fn literal() -> RGB;
    fn opcode() -> RGB;
}

// Works well on a dark background.
pub(crate) struct Pastel;

impl RgbScheme for Pastel {
    fn address() -> RGB {
        // Also grey.
        RGB::new(160, 160, 160)
    }

    fn comment() -> RGB {
        // Darker grey.
        RGB::new(100, 100, 100)
    }

    fn const_reg() -> RGB {
        // Yellow.
        RGB::new(255, 250, 129)
    }

    fn constant() -> RGB {
        // Orange.
        RGB::new(253, 203, 152)
    }

    fn error() -> RGB {
        // Brighter red.
        RGB::new(255, 109, 106)
    }

    fn gp_reg() -> RGB {
        // Green.
        RGB::new(191, 228, 118)
    }

    fn hex_dump() -> RGB {
        // Grey.
        RGB::new(160, 160, 160)
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
}
