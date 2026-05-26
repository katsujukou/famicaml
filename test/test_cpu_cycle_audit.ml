(* 6502 公式 151 opcode の base cycle 数を audit.
   Reference: NESdev wiki "6502 instruction reference".
   Page-cross / branch-taken / RMW abs,X 等の動的 penalty は別途. *)

open Famicaml_common.Nesint
module Cpu = Emulator.Cpu
module Bus = Emulator.Bus
module PS = Cpu.Register.Processor_status

let make_env () =
  let ram = Bytes.make 0x10000 '\x00' in
  let bus =
    Bus.mk
      ~read:(fun a -> Uint8.of_int (Bytes.get_uint8 ram (Uint16.to_int a)))
      ~write:(fun a v ->
        Bytes.set_uint8 ram (Uint16.to_int a) (Uint8.to_int v))
  in
  let cpu = Cpu.mk () in
  cpu.reg_PC <- Uint16.zero;
  cpu.reg_P <- PS.set_flag PS.I false PS.initial;
  (ram, bus, cpu)

let wr ram start bytes =
  List.iteri (fun i b -> Bytes.set_uint8 ram (start + i) b) bytes


(* (opcode, mnemonic, base cycle) のテーブル. *)
let opcodes =
  [ (* --- Implied / Accumulator: 2 cycle --- *)
    (0xEA, "NOP", 2)
  ; (0x18, "CLC", 2)
  ; (0x38, "SEC", 2)
  ; (0x58, "CLI", 2)
  ; (0x78, "SEI", 2)
  ; (0xB8, "CLV", 2)
  ; (0xD8, "CLD", 2)
  ; (0xF8, "SED", 2)
  ; (0xAA, "TAX", 2)
  ; (0x8A, "TXA", 2)
  ; (0xA8, "TAY", 2)
  ; (0x98, "TYA", 2)
  ; (0xBA, "TSX", 2)
  ; (0x9A, "TXS", 2)
  ; (0xE8, "INX", 2)
  ; (0xC8, "INY", 2)
  ; (0xCA, "DEX", 2)
  ; (0x88, "DEY", 2)
  ; (0x0A, "ASL A", 2)
  ; (0x4A, "LSR A", 2)
  ; (0x2A, "ROL A", 2)
  ; (0x6A, "ROR A", 2)
  ; (* --- Stack --- *)
    (0x48, "PHA", 3)
  ; (0x08, "PHP", 3)
  ; (0x68, "PLA", 4)
  ; (0x28, "PLP", 4)
  ; (* --- Branches: 2 cycle (not taken). Taken/page-cross は別 test --- *)
    (0x10, "BPL", 2)
  ; (0x30, "BMI", 2)
  ; (0x50, "BVC", 2)
  ; (0x70, "BVS", 2)
  ; (0x90, "BCC", 2)
  ; (0xB0, "BCS", 2)
  ; (0xD0, "BNE", 2)
  ; (0xF0, "BEQ", 2)
  ; (* --- Jumps / Returns --- *)
    (0x4C, "JMP abs", 3)
  ; (0x6C, "JMP ind", 5)
  ; (0x20, "JSR", 6)
  ; (0x60, "RTS", 6)
  ; (0x40, "RTI", 6)
  ; (0x00, "BRK", 7)
  ; (* --- Comparison --- *)
    (0xC9, "CMP imm", 2)
  ; (0xC5, "CMP zp", 3)
  ; (0xD5, "CMP zp,X", 4)
  ; (0xCD, "CMP abs", 4)
  ; (0xDD, "CMP abs,X", 4)
  ; (0xD9, "CMP abs,Y", 4)
  ; (0xC1, "CMP (zp,X)", 6)
  ; (0xD1, "CMP (zp),Y", 5)
  ; (0xE0, "CPX imm", 2)
  ; (0xE4, "CPX zp", 3)
  ; (0xEC, "CPX abs", 4)
  ; (0xC0, "CPY imm", 2)
  ; (0xC4, "CPY zp", 3)
  ; (0xCC, "CPY abs", 4)
  ; (* --- BIT --- *)
    (0x24, "BIT zp", 3)
  ; (0x2C, "BIT abs", 4)
  ; (* --- LDA --- *)
    (0xA9, "LDA imm", 2)
  ; (0xA5, "LDA zp", 3)
  ; (0xB5, "LDA zp,X", 4)
  ; (0xAD, "LDA abs", 4)
  ; (0xBD, "LDA abs,X", 4)
  ; (0xB9, "LDA abs,Y", 4)
  ; (0xA1, "LDA (zp,X)", 6)
  ; (0xB1, "LDA (zp),Y", 5)
  ; (* --- LDX --- *)
    (0xA2, "LDX imm", 2)
  ; (0xA6, "LDX zp", 3)
  ; (0xB6, "LDX zp,Y", 4)
  ; (0xAE, "LDX abs", 4)
  ; (0xBE, "LDX abs,Y", 4)
  ; (* --- LDY --- *)
    (0xA0, "LDY imm", 2)
  ; (0xA4, "LDY zp", 3)
  ; (0xB4, "LDY zp,X", 4)
  ; (0xAC, "LDY abs", 4)
  ; (0xBC, "LDY abs,X", 4)
  ; (* --- STA --- *)
    (0x85, "STA zp", 3)
  ; (0x95, "STA zp,X", 4)
  ; (0x8D, "STA abs", 4)
  ; (0x9D, "STA abs,X", 5)
  ; (0x99, "STA abs,Y", 5)
  ; (0x81, "STA (zp,X)", 6)
  ; (0x91, "STA (zp),Y", 6)
  ; (* --- STX --- *)
    (0x86, "STX zp", 3)
  ; (0x96, "STX zp,Y", 4)
  ; (0x8E, "STX abs", 4)
  ; (* --- STY --- *)
    (0x84, "STY zp", 3)
  ; (0x94, "STY zp,X", 4)
  ; (0x8C, "STY abs", 4)
  ; (* --- ADC --- *)
    (0x69, "ADC imm", 2)
  ; (0x65, "ADC zp", 3)
  ; (0x75, "ADC zp,X", 4)
  ; (0x6D, "ADC abs", 4)
  ; (0x7D, "ADC abs,X", 4)
  ; (0x79, "ADC abs,Y", 4)
  ; (0x61, "ADC (zp,X)", 6)
  ; (0x71, "ADC (zp),Y", 5)
  ; (* --- SBC --- *)
    (0xE9, "SBC imm", 2)
  ; (0xE5, "SBC zp", 3)
  ; (0xF5, "SBC zp,X", 4)
  ; (0xED, "SBC abs", 4)
  ; (0xFD, "SBC abs,X", 4)
  ; (0xF9, "SBC abs,Y", 4)
  ; (0xE1, "SBC (zp,X)", 6)
  ; (0xF1, "SBC (zp),Y", 5)
  ; (* --- AND --- *)
    (0x29, "AND imm", 2)
  ; (0x25, "AND zp", 3)
  ; (0x35, "AND zp,X", 4)
  ; (0x2D, "AND abs", 4)
  ; (0x3D, "AND abs,X", 4)
  ; (0x39, "AND abs,Y", 4)
  ; (0x21, "AND (zp,X)", 6)
  ; (0x31, "AND (zp),Y", 5)
  ; (* --- ORA --- *)
    (0x09, "ORA imm", 2)
  ; (0x05, "ORA zp", 3)
  ; (0x15, "ORA zp,X", 4)
  ; (0x0D, "ORA abs", 4)
  ; (0x1D, "ORA abs,X", 4)
  ; (0x19, "ORA abs,Y", 4)
  ; (0x01, "ORA (zp,X)", 6)
  ; (0x11, "ORA (zp),Y", 5)
  ; (* --- EOR --- *)
    (0x49, "EOR imm", 2)
  ; (0x45, "EOR zp", 3)
  ; (0x55, "EOR zp,X", 4)
  ; (0x4D, "EOR abs", 4)
  ; (0x5D, "EOR abs,X", 4)
  ; (0x59, "EOR abs,Y", 4)
  ; (0x41, "EOR (zp,X)", 6)
  ; (0x51, "EOR (zp),Y", 5)
  ; (* --- RMW (Read-Modify-Write) --- *)
    (0x06, "ASL zp", 5)
  ; (0x16, "ASL zp,X", 6)
  ; (0x0E, "ASL abs", 6)
  ; (0x1E, "ASL abs,X", 7)
  ; (0x46, "LSR zp", 5)
  ; (0x56, "LSR zp,X", 6)
  ; (0x4E, "LSR abs", 6)
  ; (0x5E, "LSR abs,X", 7)
  ; (0x26, "ROL zp", 5)
  ; (0x36, "ROL zp,X", 6)
  ; (0x2E, "ROL abs", 6)
  ; (0x3E, "ROL abs,X", 7)
  ; (0x66, "ROR zp", 5)
  ; (0x76, "ROR zp,X", 6)
  ; (0x6E, "ROR abs", 6)
  ; (0x7E, "ROR abs,X", 7)
  ; (0xE6, "INC zp", 5)
  ; (0xF6, "INC zp,X", 6)
  ; (0xEE, "INC abs", 6)
  ; (0xFE, "INC abs,X", 7)
  ; (0xC6, "DEC zp", 5)
  ; (0xD6, "DEC zp,X", 6)
  ; (0xCE, "DEC abs", 6)
  ; (0xDE, "DEC abs,X", 7)
  ]

let test_all_opcodes_base_cycles () =
  List.iter
    (fun (op, name, expected) ->
      let actual = Cpu.opcode_base_cycles op in
      Alcotest.(check int)
        (Printf.sprintf "$%02X %s = %d cyc" op name expected)
        expected
        actual)
    opcodes

let test_count_covered () =
  Alcotest.(check int) "151 公式 opcode 全網羅" 151 (List.length opcodes)

(* ------------------------------------------------------------------ *)
(* Dynamic penalty: page-cross / branch-taken                          *)
(* ------------------------------------------------------------------ *)

let test_branch_not_taken_2cyc () =
  let ram, bus, cpu = make_env () in
  (* CLC ; BCS +5 (BCS = carry set; carry is clear → not taken). *)
  wr ram 0 [ 0x18; 0xB0; 0x05 ];
  let _ = Cpu.step_instruction bus cpu in
  let c = Cpu.step_instruction bus cpu in
  Alcotest.(check int) "branch not taken = 2 cyc" 2 c

let test_branch_taken_with_page_cross () =
  let ram, bus, cpu = make_env () in
  (* CLC at $0500, BCC +$05 at $0501. operand fetch 後 PC=$0503.
     new_pc = $0503 + $05 = $0508 → 同 page. cross test には PC を $00FB
     に置いて BCC operand 後 PC=$00FD, new_pc = $00FD + $05 = $0102 (cross). *)
  cpu.reg_PC <- Uint16.of_int 0x00FA;
  wr ram 0x00FA [ 0x18; 0x90; 0x05 ];
  let _ = Cpu.step_instruction bus cpu in
  (* CLC at $00FA, PC = $00FB *)
  let c = Cpu.step_instruction bus cpu in
  (* BCC at $00FB: opcode → $00FC, operand → $00FD, target $00FD+$05=$0102 cross *)
  Alcotest.(check int) "branch taken + page cross = 4 cyc" 4 c

let test_abs_y_page_cross () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x1100 0xAB;
  wr ram 0 [ 0xA0; 0xFF; 0xB9; 0x01; 0x10 ];
  let _ = Cpu.step_instruction bus cpu in
  let c = Cpu.step_instruction bus cpu in
  Alcotest.(check int) "LDA abs,Y page cross = 5 cyc" 5 c

let test_indirect_y_no_page_cross () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x20 0x40;
  Bytes.set_uint8 ram 0x21 0x30;
  Bytes.set_uint8 ram 0x3040 0xCC;
  wr ram 0 [ 0xA0; 0x00; 0xB1; 0x20 ];
  let _ = Cpu.step_instruction bus cpu in
  let c = Cpu.step_instruction bus cpu in
  Alcotest.(check int) "LDA (zp),Y no cross = 5 cyc" 5 c

let test_indirect_y_page_cross () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x20 0xFF;
  Bytes.set_uint8 ram 0x21 0x30;
  Bytes.set_uint8 ram 0x3100 0xDD;
  wr ram 0 [ 0xA0; 0x01; 0xB1; 0x20 ];
  let _ = Cpu.step_instruction bus cpu in
  let c = Cpu.step_instruction bus cpu in
  Alcotest.(check int) "LDA (zp),Y page cross = 6 cyc" 6 c

let test_sta_abs_y_always_5 () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA0; 0xFF; 0xA9; 0x99; 0x99; 0x01; 0x10 ];
  let _ = Cpu.step_instruction bus cpu in
  let _ = Cpu.step_instruction bus cpu in
  let c = Cpu.step_instruction bus cpu in
  Alcotest.(check int) "STA abs,Y = 5 cyc (always)" 5 c

let test_sta_indirect_y_always_6 () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x20 0xFF;
  Bytes.set_uint8 ram 0x21 0x30;
  wr ram 0 [ 0xA0; 0x01; 0xA9; 0x99; 0x91; 0x20 ];
  let _ = Cpu.step_instruction bus cpu in
  let _ = Cpu.step_instruction bus cpu in
  let c = Cpu.step_instruction bus cpu in
  Alcotest.(check int) "STA (zp),Y = 6 cyc (always)" 6 c

(* ------------------------------------------------------------------ *)
(* IRQ delivery timing (NESdev: 命令 N の penultimate cycle で sample) *)
(*                                                                     *)
(* 帰結: CLI 直後の 1 命令は IRQ で割り込まれない. その次の命令完了後  *)
(*       に IRQ entry が始まる.                                         *)
(* ------------------------------------------------------------------ *)

let test_cli_then_one_instruction_then_irq () =
  let ram, bus, cpu = make_env () in
  (* IRQ vector に $9000 *)
  Bytes.set_uint8 ram 0xFFFE 0x00;
  Bytes.set_uint8 ram 0xFFFF 0x90;
  cpu.reg_P <- PS.set_flag PS.I true cpu.reg_P;
  wr ram 0 [ 0x78; 0x58; 0xEA; 0xEA; 0xEA ];
  (* SEI ; CLI ; NOP ; NOP ; NOP *)
  let _ = Cpu.step_instruction bus cpu in
  (* SEI: I=1 *)
  Cpu.request_irq cpu;
  let _ = Cpu.step_instruction bus cpu in
  (* CLI: I=0 *)
  let _ = Cpu.step_instruction bus cpu in
  (* NOP — 実機仕様により CLI 直後はまだ割り込まれない *)
  Alcotest.(check int)
    "NOP 後はまだ PC は通常コード ($0003 NOP)"
    0x0003
    (Uint16.to_int cpu.reg_PC);
  let _ = Cpu.step_instruction bus cpu in
  (* 2 個目の NOP の fetch 時に IRQ entry が始まる *)
  Alcotest.(check int) "2 命令目開始時 IRQ entry → PC = vector" 0x9000 (Uint16.to_int cpu.reg_PC)

let () =
  Alcotest.run
    "CPU cycle audit"
    [ ( "Opcode count"
      , [ Alcotest.test_case "151 公式 opcode" `Quick test_count_covered ] )
    ; ( "Base cycle counts"
      , [ Alcotest.test_case
            "全 151 opcode の base cycle 数"
            `Quick
            test_all_opcodes_base_cycles
        ] )
    ; ( "Branch penalty"
      , [ Alcotest.test_case "not taken = 2 cyc" `Quick test_branch_not_taken_2cyc
        ; Alcotest.test_case
            "taken + page cross = 4 cyc"
            `Quick
            test_branch_taken_with_page_cross
        ] )
    ; ( "IRQ delivery timing"
      , [ Alcotest.test_case
            "CLI 後 1 命令で IRQ entry (現状 bug 確認)"
            `Quick
            test_cli_then_one_instruction_then_irq
        ] )
    ; ( "abs,Y / (zp),Y page cross"
      , [ Alcotest.test_case
            "LDA abs,Y page cross = 5 cyc"
            `Quick
            test_abs_y_page_cross
        ; Alcotest.test_case
            "LDA (zp),Y no cross = 5 cyc"
            `Quick
            test_indirect_y_no_page_cross
        ; Alcotest.test_case
            "LDA (zp),Y page cross = 6 cyc"
            `Quick
            test_indirect_y_page_cross
        ; Alcotest.test_case
            "STA abs,Y = 5 cyc (always)"
            `Quick
            test_sta_abs_y_always_5
        ; Alcotest.test_case
            "STA (zp),Y = 6 cyc (always)"
            `Quick
            test_sta_indirect_y_always_6
        ] )
    ]
