open Famicaml_common.Nesint

type opcode =
  | LDA | LDX | LDY | STA | STX | STY
  | TAX | TAY | TSX | TXA | TXS | TYA
  | ADC | AND | ASL | BIT | CMP | CPX
  | CPY | DEC | DEX | DEY | EOR | INC
  | INX | INY | LSR | ORA | ROL | ROR
  | SBC
  | PHA | PHP | PLA | PLP
  | JMP | JSR | RTS | RTI
  | BCC | BCS | BEQ | BMI | BNE | BPL
  | BVC | BVS
  | CLC | CLD | CLI | CLV | SEC | SED | SEI
  | BRK | NOP

type addressing_mode =
  | IMP | ACC | IMD
  | ZP  | ZP_X  | ZP_Y
  | REL
  | ABS | ABS_X | ABS_Y
  | IND | IND_X | IND_Y

type operand_with_mode =
  | Implied
  | Accumulator
  | Immediate   of uint8
  | Zeropage    of uint8
  | Zeropage_X  of uint8
  | Zeropage_Y  of uint8
  | Relative    of uint8
  | Absolute    of uint16
  | Absolute_X  of uint16
  | Absolute_Y  of uint16
  | Indirect    of uint16
  | Indirect_X  of uint8
  | Indirect_Y  of uint8

type t = {
  op     : opcode;
  arg    : operand_with_mode;
  cycles : int;
}

(** 命令ごとのメタ情報。 *)
type spec = {
  op     : opcode;
  mode   : addressing_mode;
  bytes  : int;
  cycles : int;
}

(** オペコードバイトから命令仕様を引く。未定義オペコードは None。 *)
val lookup : uint8 -> spec option
