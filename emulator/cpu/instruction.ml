open Famicaml_common.Nesint

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


type addressing_mode = 
  | IMP
  | ACC
  | IMD
  | ZP
  | ZP_X
  | ZP_Y
  | REL
  | ABS
  | ABS_X
  | ABS_Y
  | IND
  | IND_X
  | IND_Y

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

type t = {
  op : opcode;
  arg: operand_with_mode;
  cycles: int
}

(** 1命令あたりのメタ情報。
    bytes: オペコード込みの命令長(1〜3)
    cycles: 基本サイクル数(ページ跨ぎ等のペナルティは実行時に加算) *)
type spec = {
  op    : opcode;
  mode  : addressing_mode;
  bytes : int;
  cycles : int;
}

(** 公式命令の一覧。 (オペコードバイト, op, addressing_mode, bytes, cycles) *)
let instruction_list = [
  (* --- LDA --- *)
  (0xA9, LDA, IMD,   2, 2); (0xA5, LDA, ZP,    2, 3);
  (0xB5, LDA, ZP_X,  2, 4); (0xAD, LDA, ABS,   3, 4);
  (0xBD, LDA, ABS_X, 3, 4); (0xB9, LDA, ABS_Y, 3, 4);
  (0xA1, LDA, IND_X, 2, 6); (0xB1, LDA, IND_Y, 2, 5);
  (* --- LDX --- *)
  (0xA2, LDX, IMD,   2, 2); (0xA6, LDX, ZP,    2, 3);
  (0xB6, LDX, ZP_Y,  2, 4); (0xAE, LDX, ABS,   3, 4);
  (0xBE, LDX, ABS_Y, 3, 4);
  (* --- LDY --- *)
  (0xA0, LDY, IMD,   2, 2); (0xA4, LDY, ZP,    2, 3);
  (0xB4, LDY, ZP_X,  2, 4); (0xAC, LDY, ABS,   3, 4);
  (0xBC, LDY, ABS_X, 3, 4);
  (* --- STA --- *)
  (0x85, STA, ZP,    2, 3); (0x95, STA, ZP_X,  2, 4);
  (0x8D, STA, ABS,   3, 4); (0x9D, STA, ABS_X, 3, 5);
  (0x99, STA, ABS_Y, 3, 5); (0x81, STA, IND_X, 2, 6);
  (0x91, STA, IND_Y, 2, 6);
  (* --- STX --- *)
  (0x86, STX, ZP,    2, 3); (0x96, STX, ZP_Y,  2, 4);
  (0x8E, STX, ABS,   3, 4);
  (* --- STY --- *)
  (0x84, STY, ZP,    2, 3); (0x94, STY, ZP_X,  2, 4);
  (0x8C, STY, ABS,   3, 4);
  (* --- レジスタ転送 --- *)
  (0xAA, TAX, IMP, 1, 2); (0xA8, TAY, IMP, 1, 2);
  (0xBA, TSX, IMP, 1, 2); (0x8A, TXA, IMP, 1, 2);
  (0x9A, TXS, IMP, 1, 2); (0x98, TYA, IMP, 1, 2);
  (* --- ADC --- *)
  (0x69, ADC, IMD,   2, 2); (0x65, ADC, ZP,    2, 3);
  (0x75, ADC, ZP_X,  2, 4); (0x6D, ADC, ABS,   3, 4);
  (0x7D, ADC, ABS_X, 3, 4); (0x79, ADC, ABS_Y, 3, 4);
  (0x61, ADC, IND_X, 2, 6); (0x71, ADC, IND_Y, 2, 5);
  (* --- SBC --- *)
  (0xE9, SBC, IMD,   2, 2); (0xE5, SBC, ZP,    2, 3);
  (0xF5, SBC, ZP_X,  2, 4); (0xED, SBC, ABS,   3, 4);
  (0xFD, SBC, ABS_X, 3, 4); (0xF9, SBC, ABS_Y, 3, 4);
  (0xE1, SBC, IND_X, 2, 6); (0xF1, SBC, IND_Y, 2, 5);
  (* --- AND --- *)
  (0x29, AND, IMD,   2, 2); (0x25, AND, ZP,    2, 3);
  (0x35, AND, ZP_X,  2, 4); (0x2D, AND, ABS,   3, 4);
  (0x3D, AND, ABS_X, 3, 4); (0x39, AND, ABS_Y, 3, 4);
  (0x21, AND, IND_X, 2, 6); (0x31, AND, IND_Y, 2, 5);
  (* --- ORA --- *)
  (0x09, ORA, IMD,   2, 2); (0x05, ORA, ZP,    2, 3);
  (0x15, ORA, ZP_X,  2, 4); (0x0D, ORA, ABS,   3, 4);
  (0x1D, ORA, ABS_X, 3, 4); (0x19, ORA, ABS_Y, 3, 4);
  (0x01, ORA, IND_X, 2, 6); (0x11, ORA, IND_Y, 2, 5);
  (* --- EOR --- *)
  (0x49, EOR, IMD,   2, 2); (0x45, EOR, ZP,    2, 3);
  (0x55, EOR, ZP_X,  2, 4); (0x4D, EOR, ABS,   3, 4);
  (0x5D, EOR, ABS_X, 3, 4); (0x59, EOR, ABS_Y, 3, 4);
  (0x41, EOR, IND_X, 2, 6); (0x51, EOR, IND_Y, 2, 5);
  (* --- CMP --- *)
  (0xC9, CMP, IMD,   2, 2); (0xC5, CMP, ZP,    2, 3);
  (0xD5, CMP, ZP_X,  2, 4); (0xCD, CMP, ABS,   3, 4);
  (0xDD, CMP, ABS_X, 3, 4); (0xD9, CMP, ABS_Y, 3, 4);
  (0xC1, CMP, IND_X, 2, 6); (0xD1, CMP, IND_Y, 2, 5);
  (* --- CPX --- *)
  (0xE0, CPX, IMD, 2, 2); (0xE4, CPX, ZP,  2, 3);
  (0xEC, CPX, ABS, 3, 4);
  (* --- CPY --- *)
  (0xC0, CPY, IMD, 2, 2); (0xC4, CPY, ZP,  2, 3);
  (0xCC, CPY, ABS, 3, 4);
  (* --- BIT --- *)
  (0x24, BIT, ZP, 2, 3); (0x2C, BIT, ABS, 3, 4);
  (* --- INC / DEC --- *)
  (0xE6, INC, ZP,    2, 5); (0xF6, INC, ZP_X,  2, 6);
  (0xEE, INC, ABS,   3, 6); (0xFE, INC, ABS_X, 3, 7);
  (0xC6, DEC, ZP,    2, 5); (0xD6, DEC, ZP_X,  2, 6);
  (0xCE, DEC, ABS,   3, 6); (0xDE, DEC, ABS_X, 3, 7);
  (* --- INX/INY/DEX/DEY --- *)
  (0xE8, INX, IMP, 1, 2); (0xC8, INY, IMP, 1, 2);
  (0xCA, DEX, IMP, 1, 2); (0x88, DEY, IMP, 1, 2);
  (* --- ASL --- *)
  (0x0A, ASL, ACC,   1, 2); (0x06, ASL, ZP,    2, 5);
  (0x16, ASL, ZP_X,  2, 6); (0x0E, ASL, ABS,   3, 6);
  (0x1E, ASL, ABS_X, 3, 7);
  (* --- LSR --- *)
  (0x4A, LSR, ACC,   1, 2); (0x46, LSR, ZP,    2, 5);
  (0x56, LSR, ZP_X,  2, 6); (0x4E, LSR, ABS,   3, 6);
  (0x5E, LSR, ABS_X, 3, 7);
  (* --- ROL --- *)
  (0x2A, ROL, ACC,   1, 2); (0x26, ROL, ZP,    2, 5);
  (0x36, ROL, ZP_X,  2, 6); (0x2E, ROL, ABS,   3, 6);
  (0x3E, ROL, ABS_X, 3, 7);
  (* --- ROR --- *)
  (0x6A, ROR, ACC,   1, 2); (0x66, ROR, ZP,    2, 5);
  (0x76, ROR, ZP_X,  2, 6); (0x6E, ROR, ABS,   3, 6);
  (0x7E, ROR, ABS_X, 3, 7);
  (* --- ジャンプ --- *)
  (0x4C, JMP, ABS, 3, 3); (0x6C, JMP, IND, 3, 5);
  (0x20, JSR, ABS, 3, 6); (0x60, RTS, IMP, 1, 6);
  (0x40, RTI, IMP, 1, 6);
  (* --- 分岐(すべて REL, 2 bytes, 基本 2 cycles)--- *)
  (0x90, BCC, REL, 2, 2); (0xB0, BCS, REL, 2, 2);
  (0xF0, BEQ, REL, 2, 2); (0x30, BMI, REL, 2, 2);
  (0xD0, BNE, REL, 2, 2); (0x10, BPL, REL, 2, 2);
  (0x50, BVC, REL, 2, 2); (0x70, BVS, REL, 2, 2);
  (* --- スタック --- *)
  (0x48, PHA, IMP, 1, 3); (0x08, PHP, IMP, 1, 3);
  (0x68, PLA, IMP, 1, 4); (0x28, PLP, IMP, 1, 4);
  (* --- フラグ変更 --- *)
  (0x18, CLC, IMP, 1, 2); (0xD8, CLD, IMP, 1, 2);
  (0x58, CLI, IMP, 1, 2); (0xB8, CLV, IMP, 1, 2);
  (0x38, SEC, IMP, 1, 2); (0xF8, SED, IMP, 1, 2);
  (0x78, SEI, IMP, 1, 2);
  (* --- その他 --- *)
  (0x00, BRK, IMP, 1, 7); (0xEA, NOP, IMP, 1, 2);
]

(** opcode バイト(0x00-0xFF)を添字に引く 256 要素の表。
    未定義オペコードは None。 *)
let mnemonic_table : spec option array =
  let tbl = Array.make 256 None in
  List.iter
    (fun (code, op, mode, bytes, cycles) ->
       tbl.(code) <- Some { op; mode; bytes; cycles })
    instruction_list;
  tbl

(** オペコードバイトから命令仕様を引く。未定義なら None。 *)
let lookup (byte : uint8) : spec option =
  mnemonic_table.(Uint8.to_int byte)

