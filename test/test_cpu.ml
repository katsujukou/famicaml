open Stdint

(* Module aliases
   Emulator.Cpu.R  = Register       (cpu.ml で module R = Register と定義)
   Emulator.Cpu.PS = Processor_status (cpu.ml で module PS = Register.Processor_status と定義) *)
module Cpu = Emulator.Cpu
module Reg = Emulator.Cpu.R
module PS  = Emulator.Cpu.PS
module Bus = Emulator.Bus

(* ------------------------------------------------------------------ *)
(* テスト環境                                                           *)
(* ------------------------------------------------------------------ *)

(** 64KB フラット RAM と Bus を作成する。RAM の参照も返す。 *)
let make_env () =
  let ram = Bytes.create 0x10000 in
  let read  a   = Uint8.of_int @@ Bytes.get_uint8 ram (Uint16.to_int a) in
  let write a v = Bytes.set_uint8 ram (Uint16.to_int a) (Uint8.to_int v) in
  (ram, Bus.mk ~read ~write, Cpu.mk ())

(** RAM の指定オフセットにバイト列を書き込む。 *)
let wr ram offset bytes =
  List.iteri (fun i b -> Bytes.set_uint8 ram (offset + i) b) bytes

(** CPU を n ステップ実行する。ith_irq はデフォルト 0x0000。 *)
let run ?(ith_irq = 0) bus cpu n =
  let iv = Uint16.of_int ith_irq in
  for _ = 1 to n do
    ignore (Cpu.step bus cpu ~ith_irq:iv)
  done

(** フラグを取得する。 *)
let flag (cpu : Cpu.t) f = PS.get_flag f cpu.reg_P

(* ------------------------------------------------------------------ *)
(* アサーションヘルパー                                                 *)
(* ------------------------------------------------------------------ *)

let chk8  msg exp act = Alcotest.(check int) msg exp (Uint8.to_int act)
let chk16 msg exp act = Alcotest.(check int) msg exp (Uint16.to_int act)
let chkb             = Alcotest.(check bool)

(* ------------------------------------------------------------------ *)
(* LDA / STA                                                           *)
(* ------------------------------------------------------------------ *)

let test_lda_imm () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x42];   (* LDA #$42 *)
  run bus cpu 1;
  chk8 "A" 0x42 cpu.reg_A;
  chkb "Z" false (flag cpu PS.Z);
  chkb "N" false (flag cpu PS.N)

let test_lda_zero_flag () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x00];   (* LDA #$00 *)
  run bus cpu 1;
  chk8 "A" 0x00 cpu.reg_A;
  chkb "Z" true  (flag cpu PS.Z);
  chkb "N" false (flag cpu PS.N)

let test_lda_negative_flag () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x80];   (* LDA #$80 *)
  run bus cpu 1;
  chk8 "A" 0x80 cpu.reg_A;
  chkb "Z" false (flag cpu PS.Z);
  chkb "N" true  (flag cpu PS.N)

let test_lda_zeropage () =
  let (ram, bus, cpu) = make_env () in
  Bytes.set_uint8 ram 0x10 0xAB;
  wr ram 0 [0xA5; 0x10];   (* LDA $10 *)
  run bus cpu 1;
  chk8 "A" 0xAB cpu.reg_A

let test_sta_zeropage () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x55; 0x85; 0x20];  (* LDA #$55 ; STA $20 *)
  run bus cpu 2;
  Alcotest.(check int) "mem[$20]" 0x55 (Bytes.get_uint8 ram 0x20)

(* ------------------------------------------------------------------ *)
(* レジスタ転送                                                         *)
(* ------------------------------------------------------------------ *)

let test_tax () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x37; 0xAA];  (* LDA #$37 ; TAX *)
  run bus cpu 2;
  chk8 "X" 0x37 cpu.reg_X;
  chkb "Z" false (flag cpu PS.Z);
  chkb "N" false (flag cpu PS.N)

let test_txa () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA2; 0x80; 0x8A];  (* LDX #$80 ; TXA *)
  run bus cpu 2;
  chk8 "A" 0x80 cpu.reg_A;
  chkb "N" true (flag cpu PS.N)

let test_txs_no_flags () =
  let (ram, bus, cpu) = make_env () in
  (* LDX #$42: Z=0, N=0 → 初期 P と同じなのでフラグ変化なし *)
  wr ram 0 [0xA2; 0x42];  (* LDX #$42 *)
  run bus cpu 1;
  let p_after_ldx = PS.to_uint8 cpu.reg_P in
  wr ram 2 [0x9A];        (* TXS *)
  run bus cpu 1;
  chk8 "SP" 0x42 cpu.reg_SP;
  Alcotest.(check bool) "P unchanged by TXS" true (PS.to_uint8 cpu.reg_P = p_after_ldx)

(* ------------------------------------------------------------------ *)
(* ADC / SBC                                                           *)
(* ------------------------------------------------------------------ *)

let test_adc_basic () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x10; 0x69; 0x20];  (* LDA #$10 ; ADC #$20 *)
  run bus cpu 2;
  chk8 "A" 0x30 cpu.reg_A;
  chkb "C" false (flag cpu PS.C);
  chkb "V" false (flag cpu PS.V)

let test_adc_carry_out () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0xFF; 0x69; 0x01];  (* LDA #$FF ; ADC #$01 *)
  run bus cpu 2;
  chk8 "A" 0x00 cpu.reg_A;
  chkb "C" true (flag cpu PS.C);
  chkb "Z" true (flag cpu PS.Z)

let test_adc_overflow_pos () =
  (* 0x50 + 0x50 = 0xA0: 正 + 正 = 負(符号オーバーフロー) *)
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x50; 0x69; 0x50];
  run bus cpu 2;
  chk8 "A" 0xA0 cpu.reg_A;
  chkb "V" true (flag cpu PS.V);
  chkb "N" true (flag cpu PS.N);
  chkb "C" false (flag cpu PS.C)

let test_adc_with_carry_in () =
  (* SEC してから ADC: 1 + 1 + C(1) = 3 *)
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0x38; 0xA9; 0x01; 0x69; 0x01];  (* SEC ; LDA #$01 ; ADC #$01 *)
  run bus cpu 3;
  chk8 "A" 0x03 cpu.reg_A

let test_sbc_basic () =
  let (ram, bus, cpu) = make_env () in
  (* SEC ; LDA #$50 ; SBC #$30 → A = $20, C=1(no borrow) *)
  wr ram 0 [0x38; 0xA9; 0x50; 0xE9; 0x30];
  run bus cpu 3;
  chk8 "A" 0x20 cpu.reg_A;
  chkb "C" true  (flag cpu PS.C);
  chkb "V" false (flag cpu PS.V)

let test_sbc_borrow () =
  let (ram, bus, cpu) = make_env () in
  (* SEC ; LDA #$00 ; SBC #$01 → A = $FF, C=0(borrow) *)
  wr ram 0 [0x38; 0xA9; 0x00; 0xE9; 0x01];
  run bus cpu 3;
  chk8 "A" 0xFF cpu.reg_A;
  chkb "C" false (flag cpu PS.C)

(* ------------------------------------------------------------------ *)
(* 論理演算 AND / ORA / EOR                                            *)
(* ------------------------------------------------------------------ *)

let test_and () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0xFF; 0x29; 0x0F];  (* LDA #$FF ; AND #$0F *)
  run bus cpu 2;
  chk8 "A" 0x0F cpu.reg_A;
  chkb "Z" false (flag cpu PS.Z)

let test_ora () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x0F; 0x09; 0xF0];  (* LDA #$0F ; ORA #$F0 *)
  run bus cpu 2;
  chk8 "A" 0xFF cpu.reg_A;
  chkb "N" true (flag cpu PS.N)

let test_eor () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0xFF; 0x49; 0xFF];  (* LDA #$FF ; EOR #$FF *)
  run bus cpu 2;
  chk8 "A" 0x00 cpu.reg_A;
  chkb "Z" true (flag cpu PS.Z)

(* ------------------------------------------------------------------ *)
(* CMP / CPX / CPY                                                     *)
(* ------------------------------------------------------------------ *)

let test_cmp_equal () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x42; 0xC9; 0x42];  (* LDA #$42 ; CMP #$42 *)
  run bus cpu 2;
  chkb "Z" true  (flag cpu PS.Z);
  chkb "C" true  (flag cpu PS.C);
  chkb "N" false (flag cpu PS.N)

let test_cmp_greater () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x50; 0xC9; 0x30];  (* LDA #$50 ; CMP #$30 *)
  run bus cpu 2;
  chkb "Z" false (flag cpu PS.Z);
  chkb "C" true  (flag cpu PS.C)

let test_cmp_less () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x10; 0xC9; 0x20];  (* LDA #$10 ; CMP #$20 *)
  run bus cpu 2;
  chkb "Z" false (flag cpu PS.Z);
  chkb "C" false (flag cpu PS.C)

(* ------------------------------------------------------------------ *)
(* INC / DEC / INX / INY / DEX / DEY                                  *)
(* ------------------------------------------------------------------ *)

let test_inx_wrap () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA2; 0xFF; 0xE8];  (* LDX #$FF ; INX → $00 *)
  run bus cpu 2;
  chk8 "X" 0x00 cpu.reg_X;
  chkb "Z" true (flag cpu PS.Z)

let test_dey () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA0; 0x01; 0x88];  (* LDY #$01 ; DEY → $00 *)
  run bus cpu 2;
  chk8 "Y" 0x00 cpu.reg_Y;
  chkb "Z" true (flag cpu PS.Z)

let test_inc_mem () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x09; 0x85; 0x10; 0xE6; 0x10; 0xA5; 0x10];
  (* LDA #$09 ; STA $10 ; INC $10 ; LDA $10 *)
  run bus cpu 4;
  chk8 "A" 0x0A cpu.reg_A

let test_dec_mem () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x01; 0x85; 0x20; 0xC6; 0x20; 0xA5; 0x20];
  (* LDA #$01 ; STA $20 ; DEC $20 ; LDA $20 *)
  run bus cpu 4;
  chk8 "A" 0x00 cpu.reg_A;
  chkb "Z" true (flag cpu PS.Z)

(* ------------------------------------------------------------------ *)
(* ASL / LSR / ROL / ROR                                               *)
(* ------------------------------------------------------------------ *)

let test_asl_acc () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x81; 0x0A];  (* LDA #$81 ; ASL A *)
  run bus cpu 2;
  chk8 "A" 0x02 cpu.reg_A;
  chkb "C" true (flag cpu PS.C)

let test_lsr_acc () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x03; 0x4A];  (* LDA #$03 ; LSR A *)
  run bus cpu 2;
  chk8 "A" 0x01 cpu.reg_A;
  chkb "C" true (flag cpu PS.C)

let test_rol_acc () =
  let (ram, bus, cpu) = make_env () in
  (* SEC でキャリーを立ててから ROL: $01 → $03 (左シフト + C=1 が bit0 に入る) *)
  wr ram 0 [0x38; 0xA9; 0x01; 0x2A];  (* SEC ; LDA #$01 ; ROL A *)
  run bus cpu 3;
  chk8 "A" 0x03 cpu.reg_A;
  chkb "C" false (flag cpu PS.C)

let test_ror_acc () =
  let (ram, bus, cpu) = make_env () in
  (* SEC でキャリーを立ててから ROR: $02 → $81 *)
  wr ram 0 [0x38; 0xA9; 0x02; 0x6A];  (* SEC ; LDA #$02 ; ROR A *)
  run bus cpu 3;
  chk8 "A" 0x81 cpu.reg_A;
  chkb "C" false (flag cpu PS.C);
  chkb "N" true  (flag cpu PS.N)

(* ------------------------------------------------------------------ *)
(* 分岐命令                                                             *)
(* ------------------------------------------------------------------ *)

let test_beq_taken () =
  let (ram, bus, cpu) = make_env () in
  (* LDA #$00 → Z=1 ; BEQ +2(スキップ) ; LDA #$FF ; LDA #$42 *)
  wr ram 0 [0xA9; 0x00; 0xF0; 0x02; 0xA9; 0xFF; 0xA9; 0x42];
  run bus cpu 3;  (* LDA, BEQ(taken), LDA#$42 *)
  chk8 "A" 0x42 cpu.reg_A

let test_beq_not_taken () =
  let (ram, bus, cpu) = make_env () in
  (* LDA #$01 → Z=0 ; BEQ +2(スキップしない) ; LDA #$FF *)
  wr ram 0 [0xA9; 0x01; 0xF0; 0x02; 0xA9; 0xFF];
  run bus cpu 3;
  chk8 "A" 0xFF cpu.reg_A

let test_bne_taken () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x01; 0xD0; 0x02; 0xA9; 0xFF; 0xA9; 0x42];
  run bus cpu 3;
  chk8 "A" 0x42 cpu.reg_A

let test_bcs_taken () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0x38; 0xB0; 0x02; 0xA9; 0xFF; 0xA9; 0x42];
  (* SEC ; BCS +2(taken) ; LDA#$FF ; LDA#$42 *)
  run bus cpu 3;
  chk8 "A" 0x42 cpu.reg_A

let test_bcc_taken () =
  let (ram, bus, cpu) = make_env () in
  (* C=0 (初期値) ; BCC +2(taken) ; LDA#$FF ; LDA#$42 *)
  wr ram 0 [0x90; 0x02; 0xA9; 0xFF; 0xA9; 0x42];
  run bus cpu 2;
  chk8 "A" 0x42 cpu.reg_A

let test_branch_backward () =
  let (ram, bus, cpu) = make_env () in
  (* $0000: LDA #$00
     $0002: INX
     $0003: CPX #$03
     $0005: BNE $0002  (offset = -5 = 0xFB)
     $0007: NOP        *)
  wr ram 0 [0xA9; 0x00; 0xE8; 0xE0; 0x03; 0xD0; 0xFB; 0xEA];
  run bus cpu 10;  (* 十分なステップ数 *)
  chk8 "X" 0x03 cpu.reg_X

(* ------------------------------------------------------------------ *)
(* JMP                                                                  *)
(* ------------------------------------------------------------------ *)

let test_jmp_abs () =
  let (ram, bus, cpu) = make_env () in
  (* JMP $0010 ; (gap) ; $0010: LDA #$42 *)
  wr ram 0    [0x4C; 0x10; 0x00];
  wr ram 0x10 [0xA9; 0x42];
  run bus cpu 2;
  chk8 "A" 0x42 cpu.reg_A

let test_jmp_indirect_bug () =
  (* 6502 のハードウェアバグ: JMP ($01FF) の上位バイトを $0200 ではなく $0100 から読む
     lo = mem[$01FF] = $00
     hi (バグ) = mem[$0100] = $03  → ジャンプ先 $0300
     hi (正常) = mem[$0200] = $FF  → ジャンプ先 $FF00 (到達しないはず) *)
  let (ram, bus, cpu) = make_env () in
  Bytes.set_uint8 ram 0x01FF 0x00;    (* lo = $00 *)
  Bytes.set_uint8 ram 0x0100 0x03;    (* hi (バグ読み) = $03 → $0300 *)
  Bytes.set_uint8 ram 0x0200 0xFF;    (* hi (正常読み) = $FF → $FF00 *)
  wr ram 0x0300 [0xA9; 0x42];         (* バグで到達するアドレス: LDA #$42 *)
  wr ram 0xFF00 [0xA9; 0xAA];         (* 正常動作なら到達するアドレス: LDA #$AA *)
  wr ram 0 [0x6C; 0xFF; 0x01];        (* JMP ($01FF) *)
  run bus cpu 2;
  chk8 "A" 0x42 cpu.reg_A             (* バグ経由で $0300 に到達 *)

(* ------------------------------------------------------------------ *)
(* JSR / RTS                                                           *)
(* ------------------------------------------------------------------ *)

let test_jsr_rts () =
  let (ram, bus, cpu) = make_env () in
  (* $0000: JSR $0100
     $0003: LDA #$01
     $0100: LDA #$FF
     $0102: RTS         *)
  wr ram 0x0000 [0x20; 0x00; 0x01];   (* JSR $0100 *)
  wr ram 0x0003 [0xA9; 0x01];         (* LDA #$01 *)
  wr ram 0x0100 [0xA9; 0xFF; 0x60];   (* LDA #$FF ; RTS *)
  run bus cpu 4;  (* JSR, LDA#$FF, RTS, LDA#$01 *)
  chk8 "A" 0x01 cpu.reg_A

(* ------------------------------------------------------------------ *)
(* スタック PHA / PLA / PHP / PLP                                      *)
(* ------------------------------------------------------------------ *)

let test_pha_pla () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0xA9; 0x42; 0x48; 0xA9; 0x00; 0x68];
  (* LDA #$42 ; PHA ; LDA #$00 ; PLA *)
  run bus cpu 4;
  chk8 "A" 0x42 cpu.reg_A

let test_php_plp () =
  let (ram, bus, cpu) = make_env () in
  (* SEC ; PHP ; CLC ; PLP → C が復元される *)
  wr ram 0 [0x38; 0x08; 0x18; 0x28];
  run bus cpu 4;
  chkb "C" true (flag cpu PS.C)

(* ------------------------------------------------------------------ *)
(* フラグ操作                                                           *)
(* ------------------------------------------------------------------ *)

let test_clc_sec () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0x38; 0x18];  (* SEC ; CLC *)
  run bus cpu 2;
  chkb "C" false (flag cpu PS.C)

let test_sei_cli () =
  let (ram, bus, cpu) = make_env () in
  wr ram 0 [0x58; 0x78];  (* CLI ; SEI *)
  run bus cpu 2;
  chkb "I" true (flag cpu PS.I)

let test_clv () =
  let (ram, bus, cpu) = make_env () in
  (* ADC でオーバーフロー → CLV でクリア *)
  wr ram 0 [0xA9; 0x50; 0x69; 0x50; 0xB8];  (* LDA #$50 ; ADC #$50 ; CLV *)
  run bus cpu 3;
  chkb "V" false (flag cpu PS.V)

(* ------------------------------------------------------------------ *)
(* BIT                                                                  *)
(* ------------------------------------------------------------------ *)

let test_bit () =
  let (ram, bus, cpu) = make_env () in
  (* BIT: Z = (A AND M)==0, N=M[7], V=M[6] *)
  Bytes.set_uint8 ram 0x10 0xC0;    (* M = 1100_0000 *)
  wr ram 0 [0xA9; 0x00; 0x24; 0x10];  (* LDA #$00 ; BIT $10 *)
  run bus cpu 2;
  chkb "Z" true  (flag cpu PS.Z);   (* A AND M = 0 *)
  chkb "N" true  (flag cpu PS.N);   (* M[7] = 1 *)
  chkb "V" true  (flag cpu PS.V)    (* M[6] = 1 *)

(* ------------------------------------------------------------------ *)
(* BRK                                                                  *)
(* ------------------------------------------------------------------ *)

let test_brk () =
  let (ram, bus, cpu) = make_env () in
  (* IRQ ベクタ ($FFFE/$FFFF) を $0200 に設定 *)
  Bytes.set_uint8 ram 0xFFFE 0x00;
  Bytes.set_uint8 ram 0xFFFF 0x02;
  wr ram 0x0200 [0xA9; 0x77];  (* IRQ ハンドラ: LDA #$77 *)
  wr ram 0 [0x00];              (* BRK *)
  run ~ith_irq:0x0200 bus cpu 2;  (* BRK + LDA#$77 *)
  chk8 "A" 0x77 cpu.reg_A;
  chkb "I" true (flag cpu PS.I)   (* I フラグがセットされる *)

(* ------------------------------------------------------------------ *)
(* NOP                                                                  *)
(* ------------------------------------------------------------------ *)

let test_nop () =
  let (ram, bus, cpu) = make_env () in
  let a_before = cpu.reg_A in
  let p_before = PS.to_uint8 cpu.reg_P in
  wr ram 0 [0xEA];  (* NOP *)
  run bus cpu 1;
  chk8 "A unchanged" (Uint8.to_int a_before) cpu.reg_A;
  chk16 "PC advanced" 0x0001 cpu.reg_PC;
  Alcotest.(check bool) "P unchanged" true (PS.to_uint8 cpu.reg_P = p_before)

(* ------------------------------------------------------------------ *)
(* テスト登録                                                           *)
(* ------------------------------------------------------------------ *)

let () = Alcotest.run "CPU" [
  "LDA / STA", [
    Alcotest.test_case "LDA immediate"       `Quick test_lda_imm;
    Alcotest.test_case "LDA sets Z flag"     `Quick test_lda_zero_flag;
    Alcotest.test_case "LDA sets N flag"     `Quick test_lda_negative_flag;
    Alcotest.test_case "LDA zeropage"        `Quick test_lda_zeropage;
    Alcotest.test_case "STA zeropage"        `Quick test_sta_zeropage;
  ];
  "レジスタ転送", [
    Alcotest.test_case "TAX"                 `Quick test_tax;
    Alcotest.test_case "TXA"                 `Quick test_txa;
    Alcotest.test_case "TXS flags unchanged" `Quick test_txs_no_flags;
  ];
  "ADC / SBC", [
    Alcotest.test_case "ADC basic"           `Quick test_adc_basic;
    Alcotest.test_case "ADC carry out"       `Quick test_adc_carry_out;
    Alcotest.test_case "ADC overflow pos"    `Quick test_adc_overflow_pos;
    Alcotest.test_case "ADC with carry in"   `Quick test_adc_with_carry_in;
    Alcotest.test_case "SBC basic"           `Quick test_sbc_basic;
    Alcotest.test_case "SBC borrow"          `Quick test_sbc_borrow;
  ];
  "AND / ORA / EOR", [
    Alcotest.test_case "AND"                 `Quick test_and;
    Alcotest.test_case "ORA"                 `Quick test_ora;
    Alcotest.test_case "EOR"                 `Quick test_eor;
  ];
  "CMP / CPX / CPY", [
    Alcotest.test_case "CMP equal"           `Quick test_cmp_equal;
    Alcotest.test_case "CMP greater"         `Quick test_cmp_greater;
    Alcotest.test_case "CMP less"            `Quick test_cmp_less;
  ];
  "INC / DEC", [
    Alcotest.test_case "INX wrap $FF→$00"    `Quick test_inx_wrap;
    Alcotest.test_case "DEY"                 `Quick test_dey;
    Alcotest.test_case "INC memory"          `Quick test_inc_mem;
    Alcotest.test_case "DEC memory"          `Quick test_dec_mem;
  ];
  "ASL / LSR / ROL / ROR", [
    Alcotest.test_case "ASL accumulator"     `Quick test_asl_acc;
    Alcotest.test_case "LSR accumulator"     `Quick test_lsr_acc;
    Alcotest.test_case "ROL accumulator"     `Quick test_rol_acc;
    Alcotest.test_case "ROR accumulator"     `Quick test_ror_acc;
  ];
  "分岐", [
    Alcotest.test_case "BEQ taken"           `Quick test_beq_taken;
    Alcotest.test_case "BEQ not taken"       `Quick test_beq_not_taken;
    Alcotest.test_case "BNE taken"           `Quick test_bne_taken;
    Alcotest.test_case "BCS taken"           `Quick test_bcs_taken;
    Alcotest.test_case "BCC taken"           `Quick test_bcc_taken;
    Alcotest.test_case "後方分岐(ループ)"    `Quick test_branch_backward;
  ];
  "JMP", [
    Alcotest.test_case "JMP absolute"        `Quick test_jmp_abs;
    Alcotest.test_case "JMP indirect bug"    `Quick test_jmp_indirect_bug;
  ];
  "JSR / RTS", [
    Alcotest.test_case "JSR + RTS roundtrip" `Quick test_jsr_rts;
  ];
  "スタック", [
    Alcotest.test_case "PHA / PLA"           `Quick test_pha_pla;
    Alcotest.test_case "PHP / PLP"           `Quick test_php_plp;
  ];
  "フラグ操作", [
    Alcotest.test_case "CLC / SEC"           `Quick test_clc_sec;
    Alcotest.test_case "SEI / CLI"           `Quick test_sei_cli;
    Alcotest.test_case "CLV"                 `Quick test_clv;
  ];
  "BIT", [
    Alcotest.test_case "BIT Z/N/V flags"     `Quick test_bit;
  ];
  "BRK", [
    Alcotest.test_case "BRK jumps to IRQ vector" `Quick test_brk;
  ];
  "NOP", [
    Alcotest.test_case "NOP no side effects" `Quick test_nop;
  ];
]
