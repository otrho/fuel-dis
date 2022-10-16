use std::{
    collections::BTreeMap,
    fmt::{self, Display, Formatter},
    rc::Rc,
};

use colorful::Colorful;
use fuel_asm::Opcode;

use crate::{
    colorscheme::{Pastel, RgbScheme},
    markup::Markup,
    consts::const_reg_name,
};

mod analysis;
mod disasm;

pub(crate) use analysis::analyse;
pub(crate) use disasm::disassemble;

#[derive(Default)]
pub(crate) struct Asm(BTreeMap<usize, Element>);

impl Asm {
    fn get(&self, offset: usize) -> Option<&Element> {
        self.0.get(&offset)
    }

    fn insert(&mut self, offset: usize, element: Element) {
        // Avoiding a clone of element by avoiding .or_insert() on the Entry below.
        if !self.0.contains_key(&offset) {
            self.0.insert(offset, element);
        } else {
            self.0.entry(offset).and_modify(|cur_element| {
                if *cur_element != element {
                    let pair = ElementValue::Pair(Box::new(element), Box::new(cur_element.clone()));
                    cur_element.value = pair;
                }
            });
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Element {
    value: ElementValue,
    comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
enum ElementValue {
    Operation(Opcode),
    Label(String),
    WordDatum(Rc<[u8]>),
    BytesDatum(Rc<[u8]>, usize),
    Unused(Rc<[u8]>, usize),
    Pair(Box<Element>, Box<Element>),
}

impl Element {
    fn len(&self) -> usize {
        use ElementValue::*;
        match &self.value {
            Operation(_) => 4,
            Label(_) => 0,
            WordDatum(_) => 8,
            BytesDatum(_, size) => *size,
            Unused(_, size) => *size,
            Pair(car, cdr) => std::cmp::max(car.len(), cdr.len()),
        }
    }
}

impl Display for Asm {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        fn asm_to_string(offset: usize, element: &Element) -> String {
            if let ElementValue::Unused(bytes, len) = &element.value {
                format!("\n{}", hex_dump(bytes, offset, *len))
            } else {
                Markup::text(format!("{offset:08x}"))
                    .colored(Pastel::address())
                    .padded(10)
                    .append(match &element.value {
                        ElementValue::Operation(opcode) => prettify(opcode),
                        ElementValue::Label(name) => {
                            Markup::text(format!("{name}:")).colored(Pastel::label())
                        }
                        ElementValue::WordDatum(bytes) => {
                            let value =
                                u64::from_be_bytes(bytes[offset..offset + 8].try_into().unwrap());
                            Markup::text("WORD")
                                .colored(Pastel::opcode())
                                .indented()
                                .padded(12)
                                .append(
                                    Markup::text(format!("{value:016x}h"))
                                        .colored(Pastel::literal()),
                                )
                        }
                        ElementValue::BytesDatum(bytes, len) => {
                            // Probably a hash so treat it like a number, but maybe it's a string.
                            let num_str = bytes[offset..offset + len]
                                .iter()
                                .map(|b| format!("{b:02x}"))
                                .collect::<Vec<_>>()
                                .join("");
                            let ascii_str = printable_str(&bytes[offset..offset + len]);
                            Markup::sep(
                                vec![
                                    Markup::text(format!("BYTES[{len}]")).colored(Pastel::opcode()),
                                    Markup::text(num_str).colored(Pastel::literal()),
                                    Markup::text(ascii_str).colored(Pastel::literal()),
                                ],
                                Markup::Space,
                            )
                        }
                        ElementValue::Unused(..) => {
                            unreachable!("Handled above.")
                        }
                        ElementValue::Pair(..) => unreachable!("Handled below in asm_fmtter()."),
                    })
                    .padded(50)
                    .append(
                        element
                            .comment
                            .as_ref()
                            .map(|c| Markup::text(format!("; {c}")).colored(Pastel::comment()))
                            .unwrap_or(Markup::Empty),
                    )
                    .render()
            }
        }

        fn asm_fmtter(
            f: &mut Formatter,
            offset: usize,
            element: &Element,
        ) -> Result<(), fmt::Error> {
            if let ElementValue::Pair(car, cdr) = &element.value {
                asm_fmtter(f, offset, car.as_ref())?;
                asm_fmtter(f, offset, cdr.as_ref())
            } else {
                if let ElementValue::Label(_) = &element.value {
                    // Put a blank between blocks.
                    writeln!(f, "")?;
                }
                writeln!(f, "{}", asm_to_string(offset, element))
            }
        }

        for (offset, element) in self.0.iter() {
            asm_fmtter(f, *offset, element)?;
        }

        Ok(())
    }
}

fn prettify(opcode: &Opcode) -> Markup {
    macro_rules! fmt_op {
        ($op:expr) => {
            Markup::text($op).colored(Pastel::opcode()).indented()
        };

        ($op:expr, $($arg:expr),*) => {
            Markup::text($op)
                .colored(Pastel::opcode())
                .indented()
                .padded(12)
                .append(Markup::sep(vec![ $($arg,)* ], Markup::Space))
        }
    }

    use Opcode::*;
    match opcode {
        ADD(dst_reg, lhs_reg, rhs_reg) => fmt_op!(
            "add",
            name_reg(dst_reg),
            name_reg(lhs_reg),
            name_reg(rhs_reg)
        ),
        ADDI(dst_reg, lhs_reg, imm) => {
            fmt_op!("addi", name_reg(dst_reg), name_reg(lhs_reg), imm_dec(imm))
        }
        ALOC(..) => todo!(),
        AND(..) => todo!(),
        ANDI(..) => todo!(),
        BAL(dst_reg, asset_reg, ctrct_reg) => fmt_op!(
            "bal",
            name_reg(dst_reg),
            name_reg(asset_reg),
            name_reg(ctrct_reg)
        ),
        BHEI(dst_reg) => fmt_op!("bhei", name_reg(dst_reg)),
        BHSH(..) => todo!(),
        BURN(..) => todo!(),
        CALL(to_reg, coins_reg, asset_reg, gas_reg) => fmt_op!(
            "call",
            name_reg(to_reg),
            name_reg(coins_reg),
            name_reg(asset_reg),
            name_reg(gas_reg)
        ),
        CB(..) => todo!(),
        CCP(..) => todo!(),
        CFEI(size) => fmt_op!("cfei", imm_dec(size)),
        CFSI(size) => fmt_op!("cfsi", imm_dec(size)),
        CROO(..) => todo!(),
        CSIZ(..) => todo!(),
        DIV(dst_reg, lhs_reg, rhs_reg) => fmt_op!(
            "div",
            name_reg(dst_reg),
            name_reg(lhs_reg),
            name_reg(rhs_reg)
        ),
        DIVI(..) => todo!(),
        ECR(dst_reg, sig_reg, hash_reg) => fmt_op!(
            "ecr",
            name_reg(dst_reg),
            name_reg(sig_reg),
            name_reg(hash_reg)
        ),
        EQ(dst_reg, lhs_reg, rhs_reg) => fmt_op!(
            "eq",
            name_reg(dst_reg),
            name_reg(lhs_reg),
            name_reg(rhs_reg)
        ),
        EXP(..) => todo!(),
        EXPI(..) => todo!(),
        FLAG(..) => todo!(),
        GM(reg, imm) => fmt_op!(
            "gm",
            name_reg(reg),
            imm_const(match imm {
                1 => "GM_IS_CALLER_EXTERNAL",
                2 => "GM_GET_CALLER",
                3 => "GM_GET_VERIFYING_PREDICATE",
                _ => "UNKNOWN GM CODE",
            })
        ),
        GT(dst_reg, lhs_reg, rhs_reg) => fmt_op!(
            "gt",
            name_reg(dst_reg),
            name_reg(lhs_reg),
            name_reg(rhs_reg)
        ),
        GTF(dst_reg, len_reg, imm) => fmt_op!(
            "gtf",
            name_reg(dst_reg),
            name_reg(len_reg),
            imm_const(match imm {
                0x001 => "GTF_TYPE",
                0x002 => "GTF_SCRIPT_GAS_PRICE",
                0x003 => "GTF_SCRIPT_GAS_LIMIT",
                0x004 => "GTF_SCRIPT_MATURITY",
                0x005 => "GTF_SCRIPT_SCRIPT_LENGTH",
                0x006 => "GTF_SCRIPT_SCRIPT_DATA_LENGTH",
                0x007 => "GTF_SCRIPT_INPUTS_COUNT",
                0x008 => "GTF_SCRIPT_OUTPUTS_COUNT",
                0x009 => "GTF_SCRIPT_WITNESSES_COUNT",
                0x00A => "GTF_SCRIPT_RECEIPTS_ROOT",
                0x00B => "GTF_SCRIPT_SCRIPT",
                0x00C => "GTF_SCRIPT_SCRIPT_DATA",
                0x00D => "GTF_SCRIPT_INPUT_AT_INDEX",
                0x00E => "GTF_SCRIPT_OUTPUT_AT_INDEX",
                0x00F => "GTF_SCRIPT_WITNESS_AT_INDEX",
                0x010 => "GTF_CREATE_GAS_PRICE",
                0x011 => "GTF_CREATE_GAS_LIMIT",
                0x012 => "GTF_CREATE_MATURITY",
                0x013 => "GTF_CREATE_BYTECODE_LENGTH",
                0x014 => "GTF_CREATE_BYTECODE_WITNESS_INDEX",
                0x015 => "GTF_CREATE_STORAGE_SLOTS_COUNT",
                0x016 => "GTF_CREATE_INPUTS_COUNT",
                0x017 => "GTF_CREATE_OUTPUTS_COUNT",
                0x018 => "GTF_CREATE_WITNESSES_COUNT",
                0x019 => "GTF_CREATE_SALT",
                0x01A => "GTF_CREATE_STORAGE_SLOT_AT_INDEX",
                0x01B => "GTF_CREATE_INPUT_AT_INDEX",
                0x01C => "GTF_CREATE_OUTPUT_AT_INDEX",
                0x01D => "GTF_CREATE_WITNESS_AT_INDEX",
                0x101 => "GTF_INPUT_TYPE",
                0x102 => "GTF_INPUT_COIN_TX_ID",
                0x103 => "GTF_INPUT_COIN_OUTPUT_INDEX",
                0x104 => "GTF_INPUT_COIN_OWNER",
                0x105 => "GTF_INPUT_COIN_AMOUNT",
                0x106 => "GTF_INPUT_COIN_ASSET_ID",
                0x107 => "GTF_INPUT_COIN_TX_POINTER",
                0x108 => "GTF_INPUT_COIN_WITNESS_INDEX",
                0x109 => "GTF_INPUT_COIN_MATURITY",
                0x10A => "GTF_INPUT_COIN_PREDICATE_LENGTH",
                0x10B => "GTF_INPUT_COIN_PREDICATE_DATA_LENGTH",
                0x10C => "GTF_INPUT_COIN_PREDICATE",
                0x10D => "GTF_INPUT_COIN_PREDICATE_DATA",
                0x10E => "GTF_INPUT_CONTRACT_TX_ID",
                0x10F => "GTF_INPUT_CONTRACT_OUTPUT_INDEX",
                0x110 => "GTF_INPUT_CONTRACT_BALANCE_ROOT",
                0x111 => "GTF_INPUT_CONTRACT_STATE_ROOT",
                0x112 => "GTF_INPUT_CONTRACT_TX_POINTER",
                0x113 => "GTF_INPUT_CONTRACT_CONTRACT_ID",
                0x114 => "GTF_INPUT_MESSAGE_MESSAGE_ID",
                0x115 => "GTF_INPUT_MESSAGE_SENDER",
                0x116 => "GTF_INPUT_MESSAGE_RECIPIENT",
                0x117 => "GTF_INPUT_MESSAGE_AMOUNT",
                0x118 => "GTF_INPUT_MESSAGE_NONCE",
                0x119 => "GTF_INPUT_MESSAGE_WITNESS_INDEX",
                0x11A => "GTF_INPUT_MESSAGE_DATA_LENGTH",
                0x11B => "GTF_INPUT_MESSAGE_PREDICATE_LENGTH",
                0x11C => "GTF_INPUT_MESSAGE_PREDICATE_DATA_LENGTH",
                0x11D => "GTF_INPUT_MESSAGE_DATA",
                0x11E => "GTF_INPUT_MESSAGE_PREDICATE",
                0x11F => "GTF_INPUT_MESSAGE_PREDICATE_DATA",
                0x201 => "GTF_OUTPUT_TYPE",
                0x202 => "GTF_OUTPUT_COIN_TO",
                0x203 => "GTF_OUTPUT_COIN_AMOUNT",
                0x204 => "GTF_OUTPUT_COIN_ASSET_ID",
                0x205 => "GTF_OUTPUT_CONTRACT_INPUT_INDEX",
                0x206 => "GTF_OUTPUT_CONTRACT_BALANCE_ROOT",
                0x207 => "GTF_OUTPUT_CONTRACT_STATE_ROOT",
                0x208 => "GTF_OUTPUT_MESSAGE_RECIPIENT",
                0x209 => "GTF_OUTPUT_MESSAGE_AMOUNT",
                0x20A => "GTF_OUTPUT_CONTRACT_CREATED_CONTRACT_ID",
                0x20B => "GTF_OUTPUT_CONTRACT_CREATED_STATE_ROOT",
                0x301 => "GTF_WITNESS_DATA_LENGTH",
                0x302 => "GTF_WITNESS_DATA",
                _ => "UNKNOWN GTF CODE",
            })
        ),
        JI(offs) => fmt_op!("ji", imm_addr(offs * 4)),
        JMP(reg) => fmt_op!("jmp", name_reg(reg)),
        JNE(..) => todo!(),
        JNEI(..) => todo!(),
        JNZI(reg, offs) => fmt_op!("jnzi", name_reg(reg), imm_addr(offs * 4)),
        K256(..) => todo!(),
        LB(..) => todo!(),
        LDC(..) => todo!(),
        LOG(..) => todo!(),
        LOGD(reg0, reg1, reg2, reg3) => fmt_op!(
            "logd",
            name_reg(reg0),
            name_reg(reg1),
            name_reg(reg2),
            name_reg(reg3)
        ),
        LT(dst_reg, lhs_reg, rhs_reg) => fmt_op!(
            "lt",
            name_reg(dst_reg),
            name_reg(lhs_reg),
            name_reg(rhs_reg)
        ),
        LW(dst_reg, src_reg, word_offs) => {
            fmt_op!(
                "lw",
                name_reg(dst_reg),
                name_reg(src_reg),
                imm_dec(word_offs)
            )
        }
        MCL(..) => todo!(),
        MCLI(..) => todo!(),
        MCP(dst_reg, src_reg, len_reg) => fmt_op!(
            "mcp",
            name_reg(dst_reg),
            name_reg(src_reg),
            name_reg(len_reg)
        ),
        MCPI(dst_reg, src_reg, len) => {
            fmt_op!("mcpi", name_reg(dst_reg), name_reg(src_reg), imm_dec(len))
        }
        MEQ(dst_reg, lhs_reg, rhs_reg, len_reg) => fmt_op!(
            "meq",
            name_reg(dst_reg),
            name_reg(lhs_reg),
            name_reg(rhs_reg),
            name_reg(len_reg)
        ),
        MINT(..) => todo!(),
        MLOG(..) => todo!(),
        MOD(..) => todo!(),
        MODI(..) => todo!(),
        MOVE(dst_reg, src_reg) => fmt_op!("move", name_reg(dst_reg), name_reg(src_reg)),
        MOVI(dst_reg, imm) => fmt_op!("movi", name_reg(dst_reg), imm_dec(imm)),
        MROO(..) => todo!(),
        MUL(dst_reg, lhs_reg, rhs_reg) => fmt_op!(
            "mul",
            name_reg(dst_reg),
            name_reg(lhs_reg),
            name_reg(rhs_reg)
        ),
        MULI(dst_reg, lhs_reg, imm) => {
            fmt_op!("muli", name_reg(dst_reg), name_reg(lhs_reg), imm_dec(imm))
        }
        NOOP => fmt_op!("noop"),
        NOT(..) => todo!(),
        OR(..) => todo!(),
        ORI(dst_reg, reg, imm) => {
            fmt_op!(
                "ori",
                name_reg(dst_reg),
                name_reg(reg),
                imm_hex(*imm as u64)
            )
        }
        RET(reg) => fmt_op!("ret", name_reg(reg)),
        RETD(dst_reg, len_reg) => fmt_op!("retd", name_reg(dst_reg), name_reg(len_reg)),
        RVRT(reg) => fmt_op!("rvrt", name_reg(reg)),
        S256(dst_reg, src_reg, len_reg) => fmt_op!(
            "s256",
            name_reg(dst_reg),
            name_reg(src_reg),
            name_reg(len_reg)
        ),
        SB(..) => todo!(),
        SLL(..) => todo!(),
        SLLI(dst_reg, reg, imm) => fmt_op!("slli", name_reg(dst_reg), name_reg(reg), imm_dec(imm)),
        SMO(..) => todo!(),
        SRL(..) => todo!(),
        SRLI(..) => todo!(),
        SRW(key_reg, other_reg) => fmt_op!("srw", name_reg(key_reg), name_reg(other_reg)),
        SRWQ(dst_reg, key_reg) => fmt_op!("srwq", name_reg(dst_reg), name_reg(key_reg)),
        SUB(dst_reg, lhs_reg, rhs_reg) => fmt_op!(
            "sub",
            name_reg(dst_reg),
            name_reg(lhs_reg),
            name_reg(rhs_reg)
        ),
        SUBI(dst_reg, reg, imm) => fmt_op!("subi", name_reg(dst_reg), name_reg(reg), imm_dec(imm)),
        SW(dst_reg, reg, offs) => fmt_op!("sw", name_reg(dst_reg), name_reg(reg), imm_dec(offs)),
        SWW(key_reg, reg) => fmt_op!("sww", name_reg(key_reg), name_reg(reg)),
        SWWQ(dst_reg, key_reg) => fmt_op!("swwq", name_reg(dst_reg), name_reg(key_reg)),
        TIME(..) => todo!(),
        TR(dst_reg, len_reg, src_reg) => fmt_op!(
            "tr",
            name_reg(dst_reg),
            name_reg(len_reg),
            name_reg(src_reg)
        ),
        TRO(dst_reg, out_reg, len_reg, addr_reg) => fmt_op!(
            "tro",
            name_reg(dst_reg),
            name_reg(out_reg),
            name_reg(len_reg),
            name_reg(addr_reg)
        ),
        XOR(..) => todo!(),
        XORI(dst_reg, reg, imm) => {
            fmt_op!(
                "xori",
                name_reg(dst_reg),
                name_reg(reg),
                imm_hex(*imm as u64)
            )
        }

        Undefined => todo!(),
    }
}

fn name_reg(reg: &usize) -> Markup {
    const_reg_name(reg)
        .map(|s| Markup::text(s).colored(Pastel::const_reg()))
        .unwrap_or_else(|| Markup::text(format!("$r{}", reg - 16)).colored(Pastel::gp_reg()))
}

fn imm_dec<T: Display>(imm: T) -> Markup {
    Markup::text(imm.to_string()).colored(Pastel::literal())
}

fn imm_hex(imm: u64) -> Markup {
    Markup::text(format!("{imm:x}h")).colored(Pastel::literal())
}

fn imm_const<T: Display>(imm: T) -> Markup {
    Markup::text(imm.to_string()).colored(Pastel::constant())
}

fn imm_addr(addr: u32) -> Markup {
    Markup::text(format!("block_{addr:04x}")).colored(Pastel::label())
}

fn hex_dump(bytes: &Rc<[u8]>, offset: usize, len: usize) -> String {
    bytes[offset..(offset + len)]
        .chunks(16)
        .enumerate()
        .map(|(idx, line_bytes)| {
            let hex_chars = line_bytes
                .iter()
                .map(|byte| format!("{byte:02x}"))
                .collect::<Vec<_>>()
                .join(" ");
            let hex_chars = format!("{hex_chars:<47}").color(Pastel::hex_dump());
            let ascii_chars = printable_str(line_bytes).color(Pastel::hex_dump());
            let clr_offset = format!("{:08x}", offset + idx * 16).color(Pastel::address());
            format!("{}  {hex_chars}  {ascii_chars}\n", clr_offset)
        })
        .collect::<String>()
}

fn printable_str(bytes: &[u8]) -> String {
    bytes
        .iter()
        .map(|byte| {
            format!(
                "{}",
                if byte.is_ascii_graphic() || *byte == b' ' {
                    *byte as char
                } else {
                    '.'
                }
            )
        })
        .collect::<Vec<_>>()
        .join("")
}
