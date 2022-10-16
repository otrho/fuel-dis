use super::{Asm, Element, ElementValue};
use crate::consts::RETA_REG_NUM;
use fuel_asm::{Instruction, Opcode};
use std::collections::VecDeque;

pub(crate) fn disassemble(bytes: &[u8]) -> Asm {
    Disassembler::new(bytes).disassemble()
}

struct Disassembler<'a> {
    bytes: &'a [u8],
    asm: Asm,
    work_queue: VecDeque<usize>,
}

impl<'a> Disassembler<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Disassembler {
            bytes,
            asm: Asm::default(),
            work_queue: VecDeque::from([0]),
        }
    }

    fn disassemble(mut self) -> Asm {
        while let Some(offset) = self.work_queue.pop_front() {
            let existing = self.asm.get(offset);

            if matches!(
                existing,
                Some(Element {
                    value: ElementValue::Operation(_),
                    comment: _
                })
            ) {
                // We've already disassembled this offset.
                continue;
            }

            let (opcode, next_offsets) = decode(offset, &self.bytes[offset..offset + 4]);
            self.asm.insert(
                offset,
                Element {
                    value: ElementValue::Operation(opcode),
                    comment: None,
                },
            );
            self.work_queue.extend(next_offsets);
        }

        self.asm
    }
}

fn decode(offset: usize, bytes: &[u8]) -> (Opcode, Vec<usize>) {
    assert_eq!(bytes.len(), 4);
    let instruction = Instruction::from(u32::from_be_bytes(bytes.try_into().unwrap()));

    let opcode = Opcode::from(instruction);

    // We care the most about control flow.  All other opcodes will just continue to the next
    // instruction.
    use Opcode::*;
    match opcode {
        ADD(..) | ADDI(..) | ALOC(..) | AND(..) | ANDI(..) | BAL(..) | BHEI(..) | BHSH(..)
        | BURN(..) | CALL(..) | CB(..) | CCP(..) | CFEI(..) | CFSI(..) | CROO(..) | CSIZ(..)
        | DIV(..) | DIVI(..) | ECR(..) | EQ(..) | EXP(..) | EXPI(..) | FLAG(..) | GM(..)
        | GT(..) | GTF(..) | K256(..) | LB(..) | LDC(..) | LOG(..) | LOGD(..) | LT(..) | LW(..)
        | MCL(..) | MCLI(..) | MCP(..) | MCPI(..) | MEQ(..) | MINT(..) | MLOG(..) | MOD(..)
        | MODI(..) | MOVE(..) | MROO(..) | MUL(..) | MULI(..) | NOOP | NOT(..) | OR(..)
        | ORI(..) | S256(..) | SB(..) | SLL(..) | SLLI(..) | SMO(..) | SRL(..) | SRLI(..)
        | SRW(..) | SRWQ(..) | SUB(..) | SUBI(..) | SW(..) | SWW(..) | SWWQ(..) | TIME(..)
        | TR(..) | TRO(..) | XOR(..) | XORI(..) => (opcode, vec![offset + 4]),

        // Jumps.
        JI(dst_offset) => (opcode, vec![dst_offset as usize * 4]),
        JNE(..) => todo!(),
        JNEI(_, _, dst_offset) => (opcode, vec![dst_offset as usize * 4, offset + 4]),
        JNZI(_, dst_offset) => (opcode, vec![dst_offset as usize * 4, offset + 4]),

        // Special case for MOVI and RETA.
        MOVI(dst_reg, dst_offset) => {
            (
                opcode,
                if dst_reg == RETA_REG_NUM {
                    // This is a return address, effectively a jump.
                    vec![dst_offset as usize * 4, offset + 4]
                } else {
                    vec![offset + 4]
                },
            )
        }

        // Terminators, including JMP since we don't know where it goes.
        JMP(_) | RET(..) | RETD(..) | RVRT(..) => (opcode, vec![]),

        Undefined => todo!(),
    }
}
