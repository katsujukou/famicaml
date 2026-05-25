(** Per-cycle CPU パイプライン (Phase A3: 全公式 opcode) のテスト。

    test_cpu.ml (legacy Cpu) と等価な 45 件を Pipeline.step_instruction
    で再現する + per-cycle 特有の検証 (cycle count, page-cross 等)。 *)

open Famicaml_common.Nesint
module P = Emulator.Cpu
module PS = Emulator.Cpu.PS
module Bus = Emulator.Bus

(* ------------------------------------------------------------------ *)
(* テスト環境                                                           *)
(* ------------------------------------------------------------------ *)

let make_env () =
  let ram = Bytes.create 0x10000 in
  let read a = Uint8.of_int (Bytes.get_uint8 ram (Uint16.to_int a)) in
  let write a v = Bytes.set_uint8 ram (Uint16.to_int a) (Uint8.to_int v) in
  (ram, Bus.mk ~read ~write, P.mk ())

let wr ram off bytes =
  List.iteri (fun i b -> Bytes.set_uint8 ram (off + i) b) bytes

(** N 命令進める。 *)
let run bus cpu n =
  for _ = 1 to n do
    ignore (P.step_instruction bus cpu)
  done

let flag (cpu : P.t) f = PS.get_flag f cpu.reg_P
let chk8 msg exp act = Alcotest.(check int) msg exp (Uint8.to_int act)
let chk16 msg exp act = Alcotest.(check int) msg exp (Uint16.to_int act)
let chkb = Alcotest.(check bool)

(* ------------------------------------------------------------------ *)
(* LDA / STA                                                           *)
(* ------------------------------------------------------------------ *)

let test_lda_imm () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x42 ];
  run bus cpu 1;
  chk8 "A" 0x42 cpu.reg_A;
  chkb "Z" false (flag cpu PS.Z);
  chkb "N" false (flag cpu PS.N)

let test_lda_zero_flag () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x00 ];
  run bus cpu 1;
  chk8 "A" 0 cpu.reg_A;
  chkb "Z" true (flag cpu PS.Z);
  chkb "N" false (flag cpu PS.N)

let test_lda_negative_flag () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x80 ];
  run bus cpu 1;
  chk8 "A" 0x80 cpu.reg_A;
  chkb "Z" false (flag cpu PS.Z);
  chkb "N" true (flag cpu PS.N)

let test_lda_zeropage () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x10 0xAB;
  wr ram 0 [ 0xA5; 0x10 ];
  run bus cpu 1;
  chk8 "A" 0xAB cpu.reg_A

let test_sta_zeropage () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x55; 0x85; 0x20 ];
  run bus cpu 2;
  Alcotest.(check int) "mem[$20]" 0x55 (Bytes.get_uint8 ram 0x20)

(* ------------------------------------------------------------------ *)
(* レジスタ転送                                                         *)
(* ------------------------------------------------------------------ *)

let test_tax () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x37; 0xAA ];
  run bus cpu 2;
  chk8 "X" 0x37 cpu.reg_X

let test_txa () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA2; 0x80; 0x8A ];
  run bus cpu 2;
  chk8 "A" 0x80 cpu.reg_A;
  chkb "N" true (flag cpu PS.N)

let test_txs_no_flags () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA2; 0x42 ];
  run bus cpu 1;
  let p_after_ldx = PS.to_uint8 cpu.reg_P in
  wr ram 2 [ 0x9A ];
  run bus cpu 1;
  chk8 "SP" 0x42 cpu.reg_SP;
  chkb "P unchanged by TXS" true (PS.to_uint8 cpu.reg_P = p_after_ldx)

(* ------------------------------------------------------------------ *)
(* ADC / SBC                                                           *)
(* ------------------------------------------------------------------ *)

let test_adc_basic () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x10; 0x69; 0x20 ];
  run bus cpu 2;
  chk8 "A" 0x30 cpu.reg_A;
  chkb "C" false (flag cpu PS.C);
  chkb "V" false (flag cpu PS.V)

let test_adc_carry_out () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0xFF; 0x69; 0x01 ];
  run bus cpu 2;
  chk8 "A" 0 cpu.reg_A;
  chkb "C" true (flag cpu PS.C);
  chkb "Z" true (flag cpu PS.Z)

let test_adc_overflow_pos () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x50; 0x69; 0x50 ];
  run bus cpu 2;
  chk8 "A" 0xA0 cpu.reg_A;
  chkb "V" true (flag cpu PS.V);
  chkb "N" true (flag cpu PS.N);
  chkb "C" false (flag cpu PS.C)

let test_adc_with_carry_in () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0x38; 0xA9; 0x01; 0x69; 0x01 ];
  run bus cpu 3;
  chk8 "A" 0x03 cpu.reg_A

let test_sbc_basic () =
  let ram, bus, cpu = make_env () in
  (* SEC ; LDA #$30 ; SBC #$10  →  A = $20 *)
  wr ram 0 [ 0x38; 0xA9; 0x30; 0xE9; 0x10 ];
  run bus cpu 3;
  chk8 "A" 0x20 cpu.reg_A;
  chkb "C" true (flag cpu PS.C)

let test_sbc_borrow () =
  let ram, bus, cpu = make_env () in
  (* SEC ; LDA #$10 ; SBC #$20  →  A = $F0, C=0 (borrow) *)
  wr ram 0 [ 0x38; 0xA9; 0x10; 0xE9; 0x20 ];
  run bus cpu 3;
  chk8 "A" 0xF0 cpu.reg_A;
  chkb "C borrowed" false (flag cpu PS.C)

(* ------------------------------------------------------------------ *)
(* AND / ORA / EOR                                                     *)
(* ------------------------------------------------------------------ *)

let test_and () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0xF0; 0x29; 0x0F ];
  run bus cpu 2;
  chk8 "A" 0 cpu.reg_A;
  chkb "Z" true (flag cpu PS.Z)

let test_ora () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0xF0; 0x09; 0x0F ];
  run bus cpu 2;
  chk8 "A" 0xFF cpu.reg_A

let test_eor () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0xAA; 0x49; 0xFF ];
  run bus cpu 2;
  chk8 "A" 0x55 cpu.reg_A

(* ------------------------------------------------------------------ *)
(* CMP / CPX / CPY                                                     *)
(* ------------------------------------------------------------------ *)

let test_cmp_equal () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x42; 0xC9; 0x42 ];
  run bus cpu 2;
  chkb "Z" true (flag cpu PS.Z);
  chkb "C" true (flag cpu PS.C);
  chkb "N" false (flag cpu PS.N)

let test_cmp_greater () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x50; 0xC9; 0x40 ];
  run bus cpu 2;
  chkb "Z" false (flag cpu PS.Z);
  chkb "C" true (flag cpu PS.C)

let test_cmp_less () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x10; 0xC9; 0x20 ];
  run bus cpu 2;
  chkb "Z" false (flag cpu PS.Z);
  chkb "C" false (flag cpu PS.C);
  chkb "N" true (flag cpu PS.N)

(* ------------------------------------------------------------------ *)
(* INC / DEC                                                           *)
(* ------------------------------------------------------------------ *)

let test_inx_wrap () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA2; 0xFF; 0xE8 ];
  run bus cpu 2;
  chk8 "X wraps" 0 cpu.reg_X;
  chkb "Z" true (flag cpu PS.Z)

let test_dey () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA0; 0x01; 0x88; 0x88 ];
  run bus cpu 3;
  chk8 "Y = $FF" 0xFF cpu.reg_Y;
  chkb "N" true (flag cpu PS.N)

let test_inc_mem () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x40 0x7F;
  wr ram 0 [ 0xE6; 0x40 ];
  run bus cpu 1;
  Alcotest.(check int) "mem[$40] = $80" 0x80 (Bytes.get_uint8 ram 0x40);
  chkb "N" true (flag cpu PS.N)

let test_dec_mem () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x40 0x01;
  wr ram 0 [ 0xC6; 0x40 ];
  run bus cpu 1;
  Alcotest.(check int) "mem[$40] = 0" 0 (Bytes.get_uint8 ram 0x40);
  chkb "Z" true (flag cpu PS.Z)

(* ------------------------------------------------------------------ *)
(* ASL / LSR / ROL / ROR (accumulator)                                 *)
(* ------------------------------------------------------------------ *)

let test_asl_acc () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0xC0; 0x0A ];
  run bus cpu 2;
  chk8 "A = $80" 0x80 cpu.reg_A;
  chkb "C" true (flag cpu PS.C);
  chkb "N" true (flag cpu PS.N)

let test_lsr_acc () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x03; 0x4A ];
  run bus cpu 2;
  chk8 "A = $01" 0x01 cpu.reg_A;
  chkb "C" true (flag cpu PS.C)

let test_rol_acc () =
  let ram, bus, cpu = make_env () in
  (* SEC ; LDA #$80 ; ROL  →  A = $01, C = 1 *)
  wr ram 0 [ 0x38; 0xA9; 0x80; 0x2A ];
  run bus cpu 3;
  chk8 "A = $01" 0x01 cpu.reg_A;
  chkb "C" true (flag cpu PS.C)

let test_ror_acc () =
  let ram, bus, cpu = make_env () in
  (* SEC ; LDA #$01 ; ROR  →  A = $80, C = 1 *)
  wr ram 0 [ 0x38; 0xA9; 0x01; 0x6A ];
  run bus cpu 3;
  chk8 "A = $80" 0x80 cpu.reg_A;
  chkb "C" true (flag cpu PS.C);
  chkb "N" true (flag cpu PS.N)

(* ------------------------------------------------------------------ *)
(* 分岐                                                                 *)
(* ------------------------------------------------------------------ *)

let test_beq_taken () =
  let ram, bus, cpu = make_env () in
  (* LDA #$00 ; BEQ +2 ; LDA #$FF (skipped) ; LDA #$11 *)
  wr ram 0 [ 0xA9; 0x00; 0xF0; 0x02; 0xA9; 0xFF; 0xA9; 0x11 ];
  run bus cpu 3;
  chk8 "A = $11 (skipped FF)" 0x11 cpu.reg_A

let test_beq_not_taken () =
  let ram, bus, cpu = make_env () in
  (* LDA #$01 ; BEQ +2 ; LDA #$FF *)
  wr ram 0 [ 0xA9; 0x01; 0xF0; 0x02; 0xA9; 0xFF ];
  run bus cpu 3;
  chk8 "A = $FF" 0xFF cpu.reg_A

let test_bne_taken () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA9; 0x01; 0xD0; 0x02; 0xA9; 0xFF; 0xA9; 0x22 ];
  run bus cpu 3;
  chk8 "A = $22" 0x22 cpu.reg_A

let test_bcs_taken () =
  let ram, bus, cpu = make_env () in
  (* SEC ; BCS +2 ; LDA #$FF ; LDA #$33 *)
  wr ram 0 [ 0x38; 0xB0; 0x02; 0xA9; 0xFF; 0xA9; 0x33 ];
  run bus cpu 3;
  chk8 "A = $33" 0x33 cpu.reg_A

let test_bcc_taken () =
  let ram, bus, cpu = make_env () in
  (* CLC ; BCC +2 ; LDA #$FF ; LDA #$44 *)
  wr ram 0 [ 0x18; 0x90; 0x02; 0xA9; 0xFF; 0xA9; 0x44 ];
  run bus cpu 3;
  chk8 "A = $44" 0x44 cpu.reg_A

let test_branch_backward () =
  let ram, bus, cpu = make_env () in
  (* LDX #$03 ; @loop: DEX ; BNE @loop *)
  wr ram 0 [ 0xA2; 0x03; 0xCA; 0xD0; 0xFD ];
  (* LDX = 1 inst, then DEX/BNE pair × 3 = 6 inst, total 7 *)
  run bus cpu 7;
  chk8 "X = 0" 0 cpu.reg_X;
  chkb "Z" true (flag cpu PS.Z)

(* ------------------------------------------------------------------ *)
(* JMP                                                                 *)
(* ------------------------------------------------------------------ *)

let test_jmp_abs () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0x4C; 0x34; 0x12 ];
  run bus cpu 1;
  chk16 "PC = $1234" 0x1234 cpu.reg_PC

(* $10FF → target lo from $10FF, target hi from $1000 (page-cross bug) *)
let test_jmp_indirect_bug () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x10FF 0x34;
  Bytes.set_uint8 ram 0x1000 0x12;
  (* バグで $1100 ではなくここから読まれる *)
  Bytes.set_uint8 ram 0x1100 0xFF;
  wr ram 0 [ 0x6C; 0xFF; 0x10 ];
  run bus cpu 1;
  chk16 "PC = $1234 (not $FF34)" 0x1234 cpu.reg_PC

(* ------------------------------------------------------------------ *)
(* JSR / RTS                                                           *)
(* ------------------------------------------------------------------ *)

let test_jsr_rts () =
  let ram, bus, cpu = make_env () in
  (* $0000: JSR $0010
     $0003: LDA #$AA (戻ったあと実行)
     $0010: LDA #$55 ; RTS *)
  wr ram 0 [ 0x20; 0x10; 0x00; 0xA9; 0xAA ];
  wr ram 0x10 [ 0xA9; 0x55; 0x60 ];
  run bus cpu 1;
  (* JSR *)
  chk16 "PC = $0010" 0x0010 cpu.reg_PC;
  run bus cpu 2;
  (* LDA $55 ; RTS *)
  chk16 "PC = $0003 (back)" 0x0003 cpu.reg_PC;
  run bus cpu 1;
  chk8 "A = $AA" 0xAA cpu.reg_A

(* ------------------------------------------------------------------ *)
(* スタック                                                             *)
(* ------------------------------------------------------------------ *)

let test_pha_pla () =
  let ram, bus, cpu = make_env () in
  (* LDA #$77 ; PHA ; LDA #$00 ; PLA *)
  wr ram 0 [ 0xA9; 0x77; 0x48; 0xA9; 0x00; 0x68 ];
  run bus cpu 4;
  chk8 "A = $77" 0x77 cpu.reg_A;
  chkb "Z" false (flag cpu PS.Z)

let test_php_plp () =
  let ram, bus, cpu = make_env () in
  (* SEC ; PHP ; CLC ; PLP *)
  wr ram 0 [ 0x38; 0x08; 0x18; 0x28 ];
  run bus cpu 4;
  chkb "C restored true" true (flag cpu PS.C)

(* ------------------------------------------------------------------ *)
(* フラグ操作                                                           *)
(* ------------------------------------------------------------------ *)

let test_clc_sec () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0x38; 0x18 ];
  run bus cpu 1;
  chkb "C after SEC" true (flag cpu PS.C);
  run bus cpu 1;
  chkb "C after CLC" false (flag cpu PS.C)

let test_sei_cli () =
  let ram, bus, cpu = make_env () in
  (* CLI ; SEI *)
  wr ram 0 [ 0x58; 0x78 ];
  run bus cpu 1;
  chkb "I after CLI" false (flag cpu PS.I);
  run bus cpu 1;
  chkb "I after SEI" true (flag cpu PS.I)

let test_clv () =
  let ram, bus, cpu = make_env () in
  (* LDA #$50 ; ADC #$50 (V=1) ; CLV *)
  wr ram 0 [ 0xA9; 0x50; 0x69; 0x50; 0xB8 ];
  run bus cpu 3;
  chkb "V cleared" false (flag cpu PS.V)

(* ------------------------------------------------------------------ *)
(* BIT                                                                  *)
(* ------------------------------------------------------------------ *)

let test_bit () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x10 0xC0;
  (* bit 7=1, bit 6=1 *)
  wr ram 0 [ 0xA9; 0x00; 0x24; 0x10 ];
  run bus cpu 2;
  chkb "Z (A & M = 0)" true (flag cpu PS.Z);
  chkb "V (bit6)" true (flag cpu PS.V);
  chkb "N (bit7)" true (flag cpu PS.N)

(* ------------------------------------------------------------------ *)
(* BRK                                                                  *)
(* ------------------------------------------------------------------ *)

let test_brk () =
  let ram, bus, cpu = make_env () in
  (* $FFFE/F に IRQ ベクタを書く *)
  Bytes.set_uint8 ram 0xFFFE 0x00;
  Bytes.set_uint8 ram 0xFFFF 0x90;
  wr ram 0 [ 0x00 ];
  run bus cpu 1;
  chk16 "PC = $9000 (IRQ vector)" 0x9000 cpu.reg_PC;
  chkb "I set" true (flag cpu PS.I)

(* ------------------------------------------------------------------ *)
(* NOP                                                                  *)
(* ------------------------------------------------------------------ *)

let test_nop () =
  let ram, bus, cpu = make_env () in
  let p_before = PS.to_uint8 cpu.reg_P in
  wr ram 0 [ 0xEA ];
  run bus cpu 1;
  chk16 "PC = $0001" 0x0001 cpu.reg_PC;
  chkb "P unchanged" true (PS.to_uint8 cpu.reg_P = p_before)

(* ================================================================== *)
(* 以下、Per-cycle 特有の検証 (cycle count, page-cross, RMW dummy 等)   *)
(* ================================================================== *)

let test_cycle_counts () =
  let ram, bus, cpu = make_env () in
  wr
    ram
    0
    [ 0xEA (* NOP *); 0xA9; 0x42 (* LDA #imm *); 0xA5; 0x10 (* LDA zp *) ];
  Alcotest.(check int) "NOP = 2" 2 (P.step_instruction bus cpu);
  Alcotest.(check int) "LDA #imm = 2" 2 (P.step_instruction bus cpu);
  Alcotest.(check int) "LDA zp = 3" 3 (P.step_instruction bus cpu)

let test_abs_x_no_page_cross () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x1005 0x77;
  wr ram 0 [ 0xA2; 0x05; 0xBD; 0x00; 0x10 ];
  (* LDX #5 ; LDA $1000,X *)
  let _ = P.step_instruction bus cpu in
  (* LDX *)
  let c = P.step_instruction bus cpu in
  (* LDA abs,X *)
  Alcotest.(check int) "no cross = 4 cycle" 4 c;
  chk8 "A = $77" 0x77 cpu.reg_A

let test_abs_x_with_page_cross () =
  let ram, bus, cpu = make_env () in
  Bytes.set_uint8 ram 0x1100 0x88;
  wr ram 0 [ 0xA2; 0xFF; 0xBD; 0x01; 0x10 ];
  (* LDX #$FF ; LDA $1001,X = $1100 *)
  let _ = P.step_instruction bus cpu in
  let c = P.step_instruction bus cpu in
  Alcotest.(check int) "page cross = 5 cycle" 5 c;
  chk8 "A = $88" 0x88 cpu.reg_A

let test_sta_abs_x_always_5 () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xA2; 0x05; 0xA9; 0x99; 0x9D; 0x00; 0x10 ];
  (* LDX 5 ; LDA $99 ; STA $1000,X *)
  let _ = P.step_instruction bus cpu in
  let _ = P.step_instruction bus cpu in
  let c = P.step_instruction bus cpu in
  Alcotest.(check int) "STA abs,X = 5 cycle (always)" 5 c;
  Alcotest.(check int) "mem[$1005] = $99" 0x99 (Bytes.get_uint8 ram 0x1005)

let test_inc_zp_rmw_cycles () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0xE6; 0x40 ];
  (* INC $40 *)
  let c = P.step_instruction bus cpu in
  Alcotest.(check int) "INC zp = 5 cycle" 5 c

let test_branch_no_cross_cycles () =
  let ram, bus, cpu = make_env () in
  (* CLC ; BCC +1 ; NOP — 分岐成立、ページ越えなし → 3 cycle *)
  wr ram 0 [ 0x18; 0x90; 0x01; 0xEA; 0xEA ];
  let _ = P.step_instruction bus cpu in
  (* CLC *)
  let c = P.step_instruction bus cpu in
  Alcotest.(check int) "branch taken, no cross = 3 cycle" 3 c

let test_jsr_pushes_correct_pc () =
  let ram, bus, cpu = make_env () in
  (* PC = $0500, JSR $1234 → push (PC+2)-1 = $0502, RTS pulls and adds 1 *)
  wr ram 0x0500 [ 0x20; 0x34; 0x12 ];
  wr ram 0x1234 [ 0x60 ];
  (* RTS *)
  cpu.reg_PC <- Uint16.of_int 0x0500;
  let _ = P.step_instruction bus cpu in
  (* JSR *)
  let _ = P.step_instruction bus cpu in
  (* RTS *)
  chk16 "PC after RTS = $0503" 0x0503 cpu.reg_PC

let test_illegal_opcode_fails () =
  let ram, bus, cpu = make_env () in
  wr ram 0 [ 0x02 ];
  (* $02 は illegal (KIL) *)
  Alcotest.check_raises
    "illegal opcode"
    (Failure "Cpu.decode: illegal/undefined opcode $02")
    (fun () -> ignore (P.step_instruction bus cpu))

(* ================================================================== *)
(* 割り込み (Phase A5)                                                  *)
(* ================================================================== *)

(* 割り込みベクタ + テスト用 RAM を準備するヘルパー *)
let set_vectors ram ~nmi ~reset ~irq =
  Bytes.set_uint8 ram 0xFFFA (nmi land 0xFF);
  Bytes.set_uint8 ram 0xFFFB ((nmi lsr 8) land 0xFF);
  Bytes.set_uint8 ram 0xFFFC (reset land 0xFF);
  Bytes.set_uint8 ram 0xFFFD ((reset lsr 8) land 0xFF);
  Bytes.set_uint8 ram 0xFFFE (irq land 0xFF);
  Bytes.set_uint8 ram 0xFFFF ((irq lsr 8) land 0xFF)

(* --- NMI --- *)

let test_nmi_jumps_to_vector_and_cycles () =
  let ram, bus, cpu = make_env () in
  set_vectors ram ~nmi:0x9000 ~reset:0 ~irq:0;
  cpu.reg_PC <- Uint16.of_int 0x8000;
  P.request_nmi cpu;
  let cycles = P.step_instruction bus cpu in
  Alcotest.(check int) "NMI = 7 cycle" 7 cycles;
  chk16 "PC = $9000" 0x9000 cpu.reg_PC;
  chkb "I set after NMI" true (flag cpu PS.I);
  chkb "nmi_pending auto-cleared" false cpu.nmi_pending

(* NMI はスタックに PC (上位→下位の順) と P (B=0, R=1) を push する. *)
let test_nmi_pushes_pc_and_p () =
  let ram, bus, cpu = make_env () in
  set_vectors ram ~nmi:0x9000 ~reset:0 ~irq:0;
  cpu.reg_PC <- Uint16.of_int 0x8042;
  let sp_before = Uint8.to_int cpu.reg_SP in
  P.request_nmi cpu;
  let _ = P.step_instruction bus cpu in
  (* スタック: SP_before に PCH=$80, SP_before-1 に PCL=$42, SP_before-2 に P *)
  Alcotest.(check int)
    "PCH pushed"
    0x80
    (Bytes.get_uint8 ram (0x100 + sp_before));
  Alcotest.(check int)
    "PCL pushed"
    0x42
    (Bytes.get_uint8 ram (0x100 + sp_before - 1));
  let pushed_p = Bytes.get_uint8 ram (0x100 + sp_before - 2) in
  Alcotest.(check int) "pushed P bit 4 (B) = 0" 0 (pushed_p land 0x10);
  Alcotest.(check int) "pushed P bit 5 (R) = 1" 0x20 (pushed_p land 0x20);
  Alcotest.(check int)
    "SP decreased by 3"
    (sp_before - 3)
    (Uint8.to_int cpu.reg_SP)

(* 命令実行中に NMI が来ても、現命令完了後にサービスされる. *)
let test_nmi_waits_for_instruction_boundary () =
  let ram, bus, cpu = make_env () in
  set_vectors ram ~nmi:0x9000 ~reset:0 ~irq:0;
  (* LDA $1234 (= 4 cycle): T1 fetch, T2 lo, T3 hi, T4 read *)
  cpu.reg_PC <- Uint16.of_int 0x8000;
  wr ram 0x8000 [ 0xAD; 0x34; 0x12 ];
  Bytes.set_uint8 ram 0x1234 0x55;
  (* 1 cycle 進めて命令の途中にする *)
  P.tick bus cpu;
  chkb "in mid-instruction" true (cpu.pending <> []);
  (* NMI 要求 *)
  P.request_nmi cpu;
  (* 残り 3 cycle で LDA 完了。NMI はまだ発火しない *)
  P.tick bus cpu;
  P.tick bus cpu;
  P.tick bus cpu;
  chk8 "A = $55 (LDA 完了)" 0x55 cpu.reg_A;
  chk16 "PC still at $8003" 0x8003 cpu.reg_PC;
  chkb "nmi still pending" true cpu.nmi_pending;
  (* 次の step で NMI 開始 *)
  let cycles = P.step_instruction bus cpu in
  Alcotest.(check int) "NMI = 7 cycle" 7 cycles;
  chk16 "PC = $9000" 0x9000 cpu.reg_PC

(* --- IRQ --- *)

let test_irq_serviced_when_i_clear () =
  let ram, bus, cpu = make_env () in
  set_vectors ram ~nmi:0 ~reset:0 ~irq:0xA000;
  cpu.reg_PC <- Uint16.of_int 0x8000;
  (* CLI で I=0 にしてから IRQ *)
  wr ram 0x8000 [ 0x58 ];
  let _ = P.step_instruction bus cpu in
  chkb "I=0 after CLI" false (flag cpu PS.I);
  P.request_irq cpu;
  let cycles = P.step_instruction bus cpu in
  Alcotest.(check int) "IRQ = 7 cycle" 7 cycles;
  chk16 "PC = $A000" 0xA000 cpu.reg_PC;
  chkb "I set by IRQ service" true (flag cpu PS.I);
  chkb "irq_pending auto-cleared" false cpu.irq_pending

let test_irq_blocked_when_i_set () =
  let ram, bus, cpu = make_env () in
  set_vectors ram ~nmi:0 ~reset:0 ~irq:0xA000;
  cpu.reg_PC <- Uint16.of_int 0x8000;
  wr ram 0x8000 [ 0x78; 0xEA ];
  (* SEI ; NOP *)
  let _ = P.step_instruction bus cpu in
  chkb "I=1 after SEI" true (flag cpu PS.I);
  P.request_irq cpu;
  let cycles = P.step_instruction bus cpu in
  Alcotest.(check int) "NOP runs normally = 2 cycle" 2 cycles;
  chk16 "PC = $8002 (NOP executed, no IRQ)" 0x8002 cpu.reg_PC;
  chkb "irq still pending" true cpu.irq_pending

(* push される P の B フラグは 0 (BRK と区別). *)
let test_irq_pushed_p_has_b_zero () =
  let ram, bus, cpu = make_env () in
  set_vectors ram ~nmi:0 ~reset:0 ~irq:0xA000;
  cpu.reg_PC <- Uint16.of_int 0x8000;
  wr ram 0x8000 [ 0x58 ];
  let _ = P.step_instruction bus cpu in
  let sp_before = Uint8.to_int cpu.reg_SP in
  P.request_irq cpu;
  let _ = P.step_instruction bus cpu in
  let pushed_p = Bytes.get_uint8 ram (0x100 + sp_before - 2) in
  Alcotest.(check int) "IRQ pushed P bit 4 (B) = 0" 0 (pushed_p land 0x10)

(* --- 優先度 --- *)

let test_nmi_priority_over_irq () =
  let ram, bus, cpu = make_env () in
  set_vectors ram ~nmi:0x9000 ~reset:0 ~irq:0xA000;
  cpu.reg_PC <- Uint16.of_int 0x8000;
  wr ram 0x8000 [ 0x58 ];
  (* CLI で IRQ も通せる状態 *)
  let _ = P.step_instruction bus cpu in
  P.request_nmi cpu;
  P.request_irq cpu;
  let _ = P.step_instruction bus cpu in
  chk16 "PC = $9000 (NMI first)" 0x9000 cpu.reg_PC;
  chkb "nmi cleared" false cpu.nmi_pending;
  chkb "irq still pending (not yet serviced)" true cpu.irq_pending

(* --- RESET --- *)

let test_reset_jumps_to_vector_and_sp_minus_3 () =
  let ram, bus, cpu = make_env () in
  set_vectors ram ~nmi:0 ~reset:0xC000 ~irq:0;
  let sp_before = Uint8.to_int cpu.reg_SP in
  P.request_reset cpu;
  let cycles = P.step_instruction bus cpu in
  Alcotest.(check int) "RESET = 7 cycle" 7 cycles;
  chk16 "PC = $C000" 0xC000 cpu.reg_PC;
  Alcotest.(check int) "SP -= 3" (sp_before - 3) (Uint8.to_int cpu.reg_SP);
  chkb "I set" true (flag cpu PS.I);
  chkb "reset_pending cleared" false cpu.reset_pending

(* RESET は スタックへの write を抑制する (値は変更されない). *)
let test_reset_suppresses_stack_writes () =
  let ram, bus, cpu = make_env () in
  set_vectors ram ~nmi:0 ~reset:0xC000 ~irq:0;
  (* スタック領域に sentinel を仕込む *)
  for i = 0xF8 to 0xFF do
    Bytes.set_uint8 ram (0x100 + i) 0x99
  done;
  P.request_reset cpu;
  let _ = P.step_instruction bus cpu in
  for i = 0xF8 to 0xFF do
    Alcotest.(check int)
      (Printf.sprintf "stack[$%02X] preserved" i)
      0x99
      (Bytes.get_uint8 ram (0x100 + i))
  done

let test_reset_priority_over_nmi_irq () =
  let ram, bus, cpu = make_env () in
  set_vectors ram ~nmi:0x9000 ~reset:0xC000 ~irq:0xA000;
  P.request_nmi cpu;
  P.request_irq cpu;
  P.request_reset cpu;
  let _ = P.step_instruction bus cpu in
  chk16 "PC = $C000 (RESET wins)" 0xC000 cpu.reg_PC;
  chkb "reset cleared" false cpu.reset_pending;
  chkb "nmi still pending" true cpu.nmi_pending;
  chkb "irq still pending" true cpu.irq_pending

(* request_* を呼ばなければ通常実行 (再現性確認) *)
let test_no_interrupt_runs_normal_opcode () =
  let ram, bus, cpu = make_env () in
  cpu.reg_PC <- Uint16.of_int 0x8000;
  wr ram 0x8000 [ 0xA9; 0x77 ];
  (* LDA #$77 *)
  let cycles = P.step_instruction bus cpu in
  Alcotest.(check int) "normal LDA #imm = 2 cycle" 2 cycles;
  chk8 "A = $77" 0x77 cpu.reg_A

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "Cpu Pipeline (Phase A3 — 公式 152 opcode)"
    [ ( "LDA / STA"
      , [ Alcotest.test_case "LDA immediate" `Quick test_lda_imm
        ; Alcotest.test_case "LDA sets Z flag" `Quick test_lda_zero_flag
        ; Alcotest.test_case "LDA sets N flag" `Quick test_lda_negative_flag
        ; Alcotest.test_case "LDA zeropage" `Quick test_lda_zeropage
        ; Alcotest.test_case "STA zeropage" `Quick test_sta_zeropage
        ] )
    ; ( "レジスタ転送"
      , [ Alcotest.test_case "TAX" `Quick test_tax
        ; Alcotest.test_case "TXA" `Quick test_txa
        ; Alcotest.test_case "TXS flags unchanged" `Quick test_txs_no_flags
        ] )
    ; ( "ADC / SBC"
      , [ Alcotest.test_case "ADC basic" `Quick test_adc_basic
        ; Alcotest.test_case "ADC carry out" `Quick test_adc_carry_out
        ; Alcotest.test_case "ADC overflow pos" `Quick test_adc_overflow_pos
        ; Alcotest.test_case "ADC with carry in" `Quick test_adc_with_carry_in
        ; Alcotest.test_case "SBC basic" `Quick test_sbc_basic
        ; Alcotest.test_case "SBC borrow" `Quick test_sbc_borrow
        ] )
    ; ( "AND / ORA / EOR"
      , [ Alcotest.test_case "AND" `Quick test_and
        ; Alcotest.test_case "ORA" `Quick test_ora
        ; Alcotest.test_case "EOR" `Quick test_eor
        ] )
    ; ( "CMP / CPX / CPY"
      , [ Alcotest.test_case "CMP equal" `Quick test_cmp_equal
        ; Alcotest.test_case "CMP greater" `Quick test_cmp_greater
        ; Alcotest.test_case "CMP less" `Quick test_cmp_less
        ] )
    ; ( "INC / DEC"
      , [ Alcotest.test_case "INX wrap $FF→$00" `Quick test_inx_wrap
        ; Alcotest.test_case "DEY" `Quick test_dey
        ; Alcotest.test_case "INC memory" `Quick test_inc_mem
        ; Alcotest.test_case "DEC memory" `Quick test_dec_mem
        ] )
    ; ( "ASL / LSR / ROL / ROR"
      , [ Alcotest.test_case "ASL accumulator" `Quick test_asl_acc
        ; Alcotest.test_case "LSR accumulator" `Quick test_lsr_acc
        ; Alcotest.test_case "ROL accumulator" `Quick test_rol_acc
        ; Alcotest.test_case "ROR accumulator" `Quick test_ror_acc
        ] )
    ; ( "分岐"
      , [ Alcotest.test_case "BEQ taken" `Quick test_beq_taken
        ; Alcotest.test_case "BEQ not taken" `Quick test_beq_not_taken
        ; Alcotest.test_case "BNE taken" `Quick test_bne_taken
        ; Alcotest.test_case "BCS taken" `Quick test_bcs_taken
        ; Alcotest.test_case "BCC taken" `Quick test_bcc_taken
        ; Alcotest.test_case "後方分岐(ループ)" `Quick test_branch_backward
        ] )
    ; ( "JMP"
      , [ Alcotest.test_case "JMP absolute" `Quick test_jmp_abs
        ; Alcotest.test_case "JMP indirect bug" `Quick test_jmp_indirect_bug
        ] )
    ; ( "JSR / RTS"
      , [ Alcotest.test_case "JSR + RTS roundtrip" `Quick test_jsr_rts ] )
    ; ( "スタック"
      , [ Alcotest.test_case "PHA / PLA" `Quick test_pha_pla
        ; Alcotest.test_case "PHP / PLP" `Quick test_php_plp
        ] )
    ; ( "フラグ操作"
      , [ Alcotest.test_case "CLC / SEC" `Quick test_clc_sec
        ; Alcotest.test_case "SEI / CLI" `Quick test_sei_cli
        ; Alcotest.test_case "CLV" `Quick test_clv
        ] )
    ; ("BIT", [ Alcotest.test_case "BIT Z/N/V flags" `Quick test_bit ])
    ; ("BRK", [ Alcotest.test_case "BRK jumps to IRQ vector" `Quick test_brk ])
    ; ("NOP", [ Alcotest.test_case "NOP no side effects" `Quick test_nop ])
    ; ( "Per-cycle 検証"
      , [ Alcotest.test_case
            "cycle count: NOP/LDA imm/LDA zp"
            `Quick
            test_cycle_counts
        ; Alcotest.test_case
            "abs,X no page cross = 4"
            `Quick
            test_abs_x_no_page_cross
        ; Alcotest.test_case
            "abs,X with page cross = 5"
            `Quick
            test_abs_x_with_page_cross
        ; Alcotest.test_case
            "STA abs,X always = 5"
            `Quick
            test_sta_abs_x_always_5
        ; Alcotest.test_case
            "INC zp RMW = 5 cycle"
            `Quick
            test_inc_zp_rmw_cycles
        ; Alcotest.test_case
            "branch taken (no cross) = 3"
            `Quick
            test_branch_no_cross_cycles
        ; Alcotest.test_case
            "JSR/RTS PC = JSR+3"
            `Quick
            test_jsr_pushes_correct_pc
        ; Alcotest.test_case
            "illegal opcode $02 → failwith"
            `Quick
            test_illegal_opcode_fails
        ] )
    ; ( "NMI"
      , [ Alcotest.test_case
            "PC ← $FFFA/B, 7 cycle, I set, auto-clear"
            `Quick
            test_nmi_jumps_to_vector_and_cycles
        ; Alcotest.test_case
            "PC と P (B=0,R=1) を push"
            `Quick
            test_nmi_pushes_pc_and_p
        ; Alcotest.test_case
            "命令境界まで待つ"
            `Quick
            test_nmi_waits_for_instruction_boundary
        ] )
    ; ( "IRQ"
      , [ Alcotest.test_case "I=0 でサービス" `Quick test_irq_serviced_when_i_clear
        ; Alcotest.test_case
            "I=1 でブロック (irq_pending 残る)"
            `Quick
            test_irq_blocked_when_i_set
        ; Alcotest.test_case
            "push される P の B=0"
            `Quick
            test_irq_pushed_p_has_b_zero
        ] )
    ; ( "割り込み優先度"
      , [ Alcotest.test_case "NMI > IRQ" `Quick test_nmi_priority_over_irq
        ; Alcotest.test_case
            "RESET > NMI > IRQ"
            `Quick
            test_reset_priority_over_nmi_irq
        ] )
    ; ( "RESET"
      , [ Alcotest.test_case
            "PC ← $FFFC/D, SP -= 3, I set"
            `Quick
            test_reset_jumps_to_vector_and_sp_minus_3
        ; Alcotest.test_case
            "stack write 抑制"
            `Quick
            test_reset_suppresses_stack_writes
        ] )
    ; ( "通常実行 (回帰確認)"
      , [ Alcotest.test_case
            "request 無しなら通常 opcode"
            `Quick
            test_no_interrupt_runs_normal_opcode
        ] )
    ]
