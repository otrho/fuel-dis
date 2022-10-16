use super::{Asm, Element, ElementValue, Opcode};
use crate::{
    consts::{const_reg_name, DS_REG_NUM, FLAG_REG_NUM, IS_REG_NUM},
    error::Error,
};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

pub(crate) fn analyse(mut asm: Asm, bytes: Rc<[u8]>) -> Result<Asm, Error> {
    let mut analyser = Analyser::new(&bytes);
    for (offset, element) in asm.0.iter() {
        if let ElementValue::Operation(opcode) = &element.value {
            analyser.analyse(offset, opcode)
        }
    }

    macro_rules! join_comments {
        ($cmnt_ty:pat, $var:ident, $offset:ident) => {
            analyser.comments.get(&$offset).and_then(|cs| {
                let comments = cs
                    .iter()
                    .filter_map(|c| if let $cmnt_ty = c { Some($var) } else { None })
                    .cloned()
                    .collect::<Vec<_>>();
                (!comments.is_empty()).then(|| comments.join("; "))
            })
        };
    }

    // Add any comments to opcodes.
    asm.0.iter_mut().for_each(|(offset, element)| {
        if let ElementValue::Operation(_opcode) = &element.value {
            let comment = join_comments!(Comment::Other(s), s, offset);
            element.comment = comment;
        }
    });

    // Insert the labels.
    for offset in analyser.jump_offsets.into_iter() {
        let offset = offset * 4;
        let comment = join_comments!(Comment::Label(s), s, offset);

        // A bit basic for now, the label is just 'block_<offset>'.
        asm.insert(
            offset,
            Element {
                value: ElementValue::Label(format!("block_{:04x}", offset)),
                comment,
            },
        );
    }

    // Insert the data.
    for (offset, len) in analyser.data.into_iter() {
        let value = if len == 8 {
            ElementValue::WordDatum(bytes.clone())
        } else {
            ElementValue::BytesDatum(bytes.clone(), len)
        };
        let comment = join_comments!(Comment::Other(s), s, offset);

        asm.insert(offset, Element { value, comment });
    }

    // Fill in any gaps with hex dumps.  (Use an Iterator::scan() instead?)
    let mut cur_offset = 0;
    let gaps = asm
        .0
        .iter()
        .filter_map(|(el_offset, element)| {
            let offset = cur_offset;
            cur_offset = el_offset + element.len();

            (*el_offset > offset).then(|| (offset, el_offset - offset))
        })
        .collect::<Vec<_>>();

    for (offs, len) in gaps {
        asm.insert(
            offs,
            Element {
                value: ElementValue::Unused(bytes.clone(), len),
                comment: None,
            },
        );
    }

    Ok(asm)
}

struct Analyser {
    bytes: Rc<[u8]>,
    jump_offsets: HashSet<usize>,
    regs: HashMap<usize, usize>,
    data: Vec<(usize, usize)>,
    comments: HashMap<usize, Vec<Comment>>,
}

#[derive(Clone)]
enum Comment {
    Label(String),
    Other(String),
}

impl Analyser {
    fn new(bytes: &Rc<[u8]>) -> Self {
        Analyser {
            bytes: bytes.clone(),
            jump_offsets: HashSet::default(),
            regs: HashMap::default(),
            data: Vec::new(),
            comments: HashMap::default(),
        }
    }

    fn analyse(&mut self, op_offset: &usize, opcode: &Opcode) {
        use Opcode::*;

        match opcode {
            // Immediate jumps.
            JI(offset) => self.jump_imm(*offset as usize, op_offset, false),
            JNEI(_, _, offset) => self.jump_imm(*offset as usize, op_offset, true),
            JNZI(_, offset) => self.jump_imm(*offset as usize, op_offset, true),

            // Dynamic jumps.
            JMP(offset_reg) => self.jump_dyn(offset_reg, op_offset, false),
            JNE(_, _, offset_reg) => self.jump_dyn(offset_reg, op_offset, true),

            // Memory R/W ops.
            LW(dst_reg, src_reg, imm12) => self.lw(dst_reg, src_reg, imm12, op_offset),
            MCPI(_dst_reg, src_reg, imm12) => self.mcpi(src_reg, imm12, op_offset),
            MEQ(dst_reg, lhs_reg, rhs_reg, len_reg) => {
                self.meq(dst_reg, lhs_reg, rhs_reg, len_reg, op_offset)
            }
            LB(..) | MCL(..) | MCLI(..) | MCP(..) | SB(..) | SW(..) => {
                // TODO
                if let Some(def_reg) = def_reg(opcode) {
                    self.regs.remove(def_reg);
                }
            }

            // Arithmetic ops.
            ADD(dst_reg, lhs_reg, rhs_reg) => {
                self.bin_op(dst_reg, lhs_reg, rhs_reg, |lhs, rhs| lhs + rhs)
            }
            ADDI(dst_reg, lhs_reg, rhs_imm12) => {
                self.bin_op_imm(dst_reg, lhs_reg, &(*rhs_imm12 as usize), |lhs, rhs| {
                    lhs + rhs
                })
            }
            DIV(..) | DIVI(..) | MOD(..) | MODI(..) | MUL(..) | MULI(..) | SUB(..) | SUBI(..) => {
                // TODO
                if let Some(def_reg) = def_reg(opcode) {
                    self.regs.remove(def_reg);
                }
            }

            MOVE(dst_reg, src_reg) => {
                if let Some(src_val) = self.get_reg_val(src_reg) {
                    self.regs.insert(*dst_reg, *src_val);
                } else {
                    self.regs.remove(dst_reg);
                }
            }
            MOVI(dst_reg, imm24) => {
                self.regs.insert(*dst_reg, *imm24 as usize);
            }

            // Ignore the rest.
            ALOC(..) | AND(..) | ANDI(..) | BAL(..) | BHEI(..) | BHSH(..) | BURN(..) | CALL(..)
            | CB(..) | CCP(..) | CFEI(..) | CFSI(..) | CROO(..) | CSIZ(..) | ECR(..) | EQ(..)
            | EXP(..) | EXPI(..) | FLAG(..) | GM(..) | GT(..) | GTF(..) | K256(..) | LDC(..)
            | LOG(..) | LOGD(..) | LT(..) | MINT(..) | MLOG(..) | MROO(..) | NOOP | NOT(..)
            | OR(..) | ORI(..) | RET(..) | RETD(..) | RVRT(..) | S256(..) | SLL(..) | SLLI(..)
            | SMO(..) | SRL(..) | SRLI(..) | SRW(..) | SRWQ(..) | SWW(..) | SWWQ(..) | TIME(..)
            | TR(..) | TRO(..) | XOR(..) | XORI(..) => {
                if let Some(def_reg) = def_reg(opcode) {
                    self.regs.remove(def_reg);
                }
            }

            Undefined => todo!(),
        };
    }

    fn add_comment(&mut self, el_offset: usize, comment: Comment) {
        self.comments
            .entry(el_offset)
            .and_modify(|c| c.push(comment.clone()))
            .or_insert(vec![comment]);
    }

    fn clear_gp_regs(&mut self) {
        // Really, we only need to preserve $ds.
        self.regs.retain(|&reg, _| reg == DS_REG_NUM);
    }

    fn get_reg_val(&self, src: &usize) -> Option<&usize> {
        match *src {
            IS_REG_NUM => Some(&0),
            _ => self.regs.get(src),
        }
    }

    fn bin_op<F>(&mut self, dst_reg: &usize, lhs_reg: &usize, rhs_reg: &usize, f: F)
    where
        F: FnOnce(&usize, &usize) -> usize,
    {
        if let (Some(lhs_val), Some(rhs_val)) =
            (self.get_reg_val(lhs_reg), self.get_reg_val(rhs_reg))
        {
            self.regs.insert(*dst_reg, f(lhs_val, rhs_val));
        } else {
            self.regs.remove(dst_reg);
        }
    }

    fn bin_op_imm<F>(&mut self, dst_reg: &usize, lhs_reg: &usize, rhs_imm: &usize, f: F)
    where
        F: FnOnce(&usize, &usize) -> usize,
    {
        if let Some(lhs_val) = self.get_reg_val(lhs_reg) {
            self.regs.insert(*dst_reg, f(lhs_val, rhs_imm));
        } else {
            self.regs.remove(dst_reg);
        }
    }

    fn jump_imm(&mut self, dst_offset: usize, from_offset: &usize, clear_regs: bool) {
        self.jump_offsets.insert(dst_offset);
        self.add_comment(
            dst_offset * 4,
            Comment::Label(format!("from {:08x}", from_offset)),
        );
        if clear_regs {
            self.clear_gp_regs();
        }
    }

    fn jump_dyn(&mut self, offset_reg: &usize, from_offset: &usize, clear_regs: bool) {
        if let Some(offset_val) = self.get_reg_val(offset_reg) {
            let dst_offset = *offset_val;
            self.jump_offsets.insert(dst_offset);
            self.add_comment(
                dst_offset as usize * 4,
                Comment::Label(format!("from {:08x}", from_offset)),
            );
        }
        if clear_regs {
            self.clear_gp_regs();
        }
    }

    fn lw(&mut self, dst_reg: &usize, src_reg: &usize, word_offs: &u16, op_offset: &usize) {
        if let Some(src_val) = self.get_reg_val(src_reg) {
            let offset = src_val + *word_offs as usize * 8;
            if offset + 8 <= self.bytes.len() {
                let value = u64::from_be_bytes(self.bytes[offset..offset + 8].try_into().unwrap());
                self.regs.insert(*dst_reg, value as usize);
                self.data.push((offset, 8));

                let reg_name = const_reg_name(dst_reg)
                    .map(|s| s.to_owned())
                    .unwrap_or_else(|| format!("$r{}", dst_reg - 16));
                self.add_comment(
                    *op_offset,
                    Comment::Other(format!("{reg_name} = {value:016x}h")),
                );
                self.add_comment(offset, Comment::Other(format!("load @ {op_offset:08x}")));
            }
        } else {
            self.regs.remove(dst_reg);
        }
    }

    fn mcpi(&mut self, src_reg: &usize, len_imm: &u16, op_offset: &usize) {
        if let Some(src_val) = self.get_reg_val(src_reg) {
            let offset = *src_val;
            let len = *len_imm as usize;

            if offset + len <= self.bytes.len() {
                self.data.push((offset, len));
                self.add_comment(offset, Comment::Other(format!("mcp @ {op_offset:08x}")));
            }
        }
    }

    fn meq(
        &mut self,
        dst_reg: &usize,
        lhs_reg: &usize,
        rhs_reg: &usize,
        len_reg: &usize,
        op_offset: &usize,
    ) {
        if let Some(len_val) = self.get_reg_val(len_reg) {
            let len = *len_val;
            if let Some(lhs_val) = self.get_reg_val(lhs_reg) {
                let offset = *lhs_val;
                if offset + len <= self.bytes.len() {
                    self.data.push((offset, len));
                    self.add_comment(offset, Comment::Other(format!("meq @ {op_offset:08x}")));
                }
            }
            if let Some(rhs_val) = self.get_reg_val(rhs_reg) {
                let offset = *rhs_val;
                if rhs_val + len <= self.bytes.len() {
                    self.data.push((offset, len));
                    self.add_comment(offset, Comment::Other(format!("meq @ {op_offset:08x}")));
                }
            }
        }
        self.regs.remove(dst_reg);
    }
}

fn def_reg(opcode: &Opcode) -> Option<&usize> {
    use Opcode::*;
    match opcode {
        ADD(dst_reg, ..)
        | ADDI(dst_reg, ..)
        | AND(dst_reg, ..)
        | ANDI(dst_reg, ..)
        | DIV(dst_reg, ..)
        | DIVI(dst_reg, ..)
        | EQ(dst_reg, ..)
        | EXP(dst_reg, ..)
        | EXPI(dst_reg, ..)
        | GT(dst_reg, ..)
        | LT(dst_reg, ..)
        | MLOG(dst_reg, ..)
        | MROO(dst_reg, ..)
        | MOD(dst_reg, ..)
        | MODI(dst_reg, ..)
        | MOVE(dst_reg, ..)
        | MOVI(dst_reg, ..)
        | MUL(dst_reg, ..)
        | MULI(dst_reg, ..)
        | NOT(dst_reg, ..)
        | OR(dst_reg, ..)
        | ORI(dst_reg, ..)
        | SLL(dst_reg, ..)
        | SLLI(dst_reg, ..)
        | SRL(dst_reg, ..)
        | SRLI(dst_reg, ..)
        | SUB(dst_reg, ..)
        | SUBI(dst_reg, ..)
        | XOR(dst_reg, ..)
        | XORI(dst_reg, ..) => Some(dst_reg),

        JI(..) | JNEI(..) | JNZI(..) | JMP(..) | JNE(..) | RET(..) | RETD(..) => None,

        CFEI(..) | CFSI(..) => None,
        LB(dst_reg, ..) | LW(dst_reg, ..) => Some(dst_reg),

        ALOC(..) | MCL(..) | MCLI(..) | MCP(..) | MCPI(..) => None,
        MEQ(dst_reg, ..) => Some(dst_reg),
        SB(..) | SW(..) => None,

        BAL(dst_reg, ..) | BHEI(dst_reg, ..) => Some(dst_reg),
        BHSH(..) | BURN(..) | CALL(..) | CCP(..) | CROO(..) => None,
        CSIZ(dst_reg, ..) => Some(dst_reg),
        CB(..) | LDC(..) | LOG(..) | LOGD(..) | MINT(..) | RVRT(..) | SMO(..) => None,
        SRW(dst_reg, ..) => Some(dst_reg),
        SRWQ(..) | SWW(..) | SWWQ(..) => None,
        TIME(dst_reg, ..) => Some(dst_reg),
        TR(..) | TRO(..) | ECR(..) | K256(..) | S256(..) | NOOP => None,

        FLAG(..) => Some(&FLAG_REG_NUM),
        GM(dst_reg, ..) | GTF(dst_reg, ..) => Some(dst_reg),

        Undefined => unreachable!("undefined opcode"),
    }
}
