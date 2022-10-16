pub(crate) const IS_REG_NUM: usize = 12;
pub(crate) const FLAG_REG_NUM: usize = 15;
pub(crate) const RETA_REG_NUM: usize = 62;
pub(crate) const DS_REG_NUM: usize = 63;

pub(crate) fn const_reg_name(reg: &usize) -> Option<&str> {
    (*reg <= 15 || *reg >= 54).then(|| {
        match reg {
            0 => "$zero",
            1 => "$one",
            2 => "$of",
            3 => "$pc",
            4 => "$ssp",
            5 => "$sp",
            6 => "$fp",
            7 => "$hp",
            8 => "$err",
            9 => "$ggas",
            10 => "$cgas",
            11 => "$bal",
            12 => "$is",
            13 => "$ret",
            14 => "$retl",
            15 => "$flag",
            // GP regs.
            54 => "$arg5",
            55 => "$arg4",
            56 => "$arg3",
            57 => "$arg2",
            58 => "$arg1",
            59 => "$arg0",
            60 => "$tmp",
            61 => "$retv",
            62 => "$reta",
            63 => "$ds",

            _ => unreachable!("Invalid reg number: {reg}"),
        }
    })
}
