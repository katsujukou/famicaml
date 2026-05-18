open Stdint

(** CPU6502におけるニーモニック *)
type opcode = 
  (* 転送命令 *)
  | LDA | LDX | LDY | STA | STX | STY 
  | TAX | TAY | TSX | TXA | TXS | TYA
  (* 算術・論理演算命令 *)
  | ADC | AND | ASL | BIT | CMP | CPX 
  | CPY | DEC | DEX | DEY | EOR | INC 
  | INX | INY | LSR | ORA | ROL | ROR
  | SBC 
  (* スタック命令 *)
  | PHA | PHP | PLA | PLP
  (* ジャンプ命令 *)
  | JMP | JSR | RTS | RTI
  (* 分岐命令 *)
  | BCC | BCS | BEQ | BMI | BNE | BPL 
  | BVC | BVS 
  (* フラグ変更命令 *)
  | CLC | CLD | CLI | CLV | SEC | SED 
  | SEI
  (* その他の命令 *)
  | BRK | NOP


type operand_with_mode =
  | Implied
  | Accumulator
  | Immediate of uint8 
  | Zeropage of uint8
  | Zeropage_X of uint8
  | Zeropage_Y of uint8
  | Relative of uint8
  | Absolute of uint16
  | Absolute_X of uint16
  | Absolute_Y of uint16
  | Indirect of uint16
  | Indirect_X of uint8
  | Indirect_Y of uint8

type t = (opcode * operand_with_mode)