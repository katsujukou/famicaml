open Stdint 
open Register

module Ins = Instruction
module PS = Register.Processor_status

open Logic_util

let u8 n = Uint8.of_int n
let bit7 = Uint8.of_int 0x80
 
(** 値 v を見て Z, N フラグを更新する(多くの命令で共通) *)
let set_zn (cpu:Register.t) (v:uint8) =
  cpu.reg_P <- PS.set_flags
    [ (Z, v = Uint8.zero)
    ; (N, Uint8.logand v bit7 <> Uint8.zero) ]
    cpu.reg_P
 
let get_flag (cpu:Register.t) f = PS.get_flag f cpu.reg_P
let set_flag (cpu: Register.t) f b = cpu.reg_P <- PS.set_flag f b cpu.reg_P
 
(* --- スタック操作($0100 + SP)--- *)
 
let stack_base = Uint16.of_int 0x0100
 
let push (bus:Bus.t) (cpu: Register.t) (v:uint8) =
  bus.write Uint16.(stack_base + Uint16.of_uint8 cpu.reg_SP) v;
  cpu.reg_SP <- Uint8.(cpu.reg_SP - one)
 
let pull (bus:Bus.t) (cpu: Register.t) : uint8 =
  cpu.reg_SP <- Uint8.(cpu.reg_SP + one);
  bus.read Uint16.(stack_base + Uint16.of_uint8 cpu.reg_SP)
 
let push16 (bus:Bus.t) (cpu: Register.t) (v:uint16) =
  push bus cpu (Uint8.of_int (Uint16.to_int (Uint16.shift_right v 8)));   (* hi *)
  push bus cpu (Uint8.of_int (Uint16.to_int (Uint16.logand v (Uint16.of_int 0xFF))))  (* lo *)
 
let pull16 (bus:Bus.t) (cpu: Register.t) : uint16 =
  let lo = pull bus cpu in
  let hi = pull bus cpu in
  Uint16.(shift_left (Uint16.of_uint8 hi) 8 + of_uint8 lo)
 
(** オペランドから「読み出し値」を取得する。
    LDA/AND/ORA/EOR/ADC/SBC/CMP/BIT など read 系命令で共通。 *)
let operand_value (bus:Bus.t) (cpu: Register.t) : Ins.operand_with_mode -> uint8 =
  function
  | Immediate v   -> v
  | Zeropage a    -> peek_zp bus a
  | Zeropage_X a  -> peek_zx bus cpu a
  | Zeropage_Y a  -> peek_zy bus cpu a
  | Absolute a    -> peek bus a
  | Absolute_X a  -> peek_x bus cpu a
  | Absolute_Y a  -> peek_y bus cpu a
  | Indirect_X a  -> peek_ix bus cpu a
  | Indirect_Y a  -> peek_iy bus cpu a
  | Accumulator   -> cpu.reg_A
  | _ -> raise Exn.Invalid_addressing_mode
 
(** オペランドの「実効アドレス」を計算する。
    STA/STX/STY/INC/DEC/ASL/LSR/ROL/ROR などメモリ書き込み命令で使う。
    Accumulator/Immediate/Implied はアドレスを持たない。 *)
let effective_addr (bus:Bus.t) (cpu: Register.t) : Ins.operand_with_mode -> uint16 =
  function
  | Zeropage a    -> Uint16.of_uint8 a
  | Zeropage_X a  -> Uint16.of_uint8 Uint8.(a + cpu.reg_X)
  | Zeropage_Y a  -> Uint16.of_uint8 Uint8.(a + cpu.reg_Y)
  | Absolute a    -> a
  | Absolute_X a  -> Uint16.(a + Uint16.of_uint8 cpu.reg_X)
  | Absolute_Y a  -> Uint16.(a + Uint16.of_uint8 cpu.reg_Y)
  | Indirect_X a  ->
      let ll = bus.read (Uint16.of_uint8 Uint8.(a + cpu.reg_X)) in
      let hh = bus.read (Uint16.of_uint8 Uint8.(a + Uint8.one + cpu.reg_X)) in
      Uint16.(shift_left (Uint16.of_uint8 hh) 8 + of_uint8 ll)
  | Indirect_Y a  ->
      let ll = peek_zp bus a in
      let hh = peek_zp bus Uint8.(a + Uint8.one) in
      let base = Uint16.(shift_left (Uint16.of_uint8 hh) 8 + of_uint8 ll) in
      Uint16.(base + Uint16.of_uint8 cpu.reg_Y)
  | _ -> raise Exn.Invalid_addressing_mode
 
(* ------------------------------------------------------------------ *)
(* 命令実装                                                            *)
(* ------------------------------------------------------------------ *)
 
(* --- ロード/ストア --- *)
 
let run_load (set_reg : uint8 -> unit) bus cpu arg =
  let v = operand_value bus cpu arg in
  set_reg v;
  set_zn cpu v
 
let run_lda bus cpu arg = run_load (fun v -> cpu.reg_A <- v) bus cpu arg
let run_ldx bus cpu arg = run_load (fun v -> cpu.reg_X <- v) bus cpu arg
let run_ldy bus cpu arg = run_load (fun v -> cpu.reg_Y <- v) bus cpu arg
 
let run_store (bus:Bus.t) cpu (v:uint8) arg =
  bus.write (effective_addr bus cpu arg) v
 
let run_sta bus cpu arg = run_store bus cpu cpu.reg_A arg
let run_stx bus cpu arg = run_store bus cpu cpu.reg_X arg
let run_sty bus cpu arg = run_store bus cpu cpu.reg_Y arg
 
(* --- レジスタ間転送 --- *)
 
let run_tax cpu = cpu.reg_X <- cpu.reg_A; set_zn cpu cpu.reg_X
let run_tay cpu = cpu.reg_Y <- cpu.reg_A; set_zn cpu cpu.reg_Y
let run_txa cpu = cpu.reg_A <- cpu.reg_X; set_zn cpu cpu.reg_A
let run_tya cpu = cpu.reg_A <- cpu.reg_Y; set_zn cpu cpu.reg_A
let run_tsx cpu = cpu.reg_X <- cpu.reg_SP; set_zn cpu cpu.reg_X
let run_txs cpu = cpu.reg_SP <- cpu.reg_X  (* TXS はフラグに影響しない *)
 
(* --- 算術演算 --- *)
 
(** ADC: A = A + M + C。V と C を正しく設定する。 *)
let run_adc bus cpu arg =
  let m = operand_value bus cpu arg in
  let a = cpu.reg_A in
  let c = if get_flag cpu C then 1 else 0 in
  let sum = Uint8.to_int a + Uint8.to_int m + c in
  let result = u8 (sum land 0xFF) in
  (* オーバーフロー: A と M の符号が同じで、結果の符号が違うとき *)
  let overflow =
    (Uint8.logand (Uint8.logxor a result) (Uint8.logxor m result)) |> fun x ->
    Uint8.logand x bit7 <> Uint8.zero
  in
  cpu.reg_A <- result;
  cpu.reg_P <- PS.set_flags
    [ (C, sum > 0xFF)
    ; (V, overflow)
    ; (Z, result = Uint8.zero)
    ; (N, Uint8.logand result bit7 <> Uint8.zero) ]
    cpu.reg_P
 
(** SBC: A = A - M - (1-C)。6502 では SBC は ADC に M のビット反転を
    入れたものと等価。 *)
let run_sbc bus cpu arg =
  let m = operand_value bus cpu arg in
  let a = cpu.reg_A in
  let c = if get_flag cpu C then 1 else 0 in
  let m_inv = Uint8.logxor m (u8 0xFF) in
  let sum = Uint8.to_int a + Uint8.to_int m_inv + c in
  let result = u8 (sum land 0xFF) in
  let overflow =
    Uint8.logand (Uint8.logxor a result) (Uint8.logxor m_inv result) |> fun x ->
    Uint8.logand x bit7 <> Uint8.zero
  in
  cpu.reg_A <- result;
  cpu.reg_P <- PS.set_flags
    [ (C, sum > 0xFF)
    ; (V, overflow)
    ; (Z, result = Uint8.zero)
    ; (N, Uint8.logand result bit7 <> Uint8.zero) ]
    cpu.reg_P
 
(* --- 論理演算 --- *)
 
let run_logic (f : uint8 -> uint8 -> uint8) bus cpu arg =
  let m = operand_value bus cpu arg in
  let result = f cpu.reg_A m in
  cpu.reg_A <- result;
  set_zn cpu result
 
let run_and bus cpu arg = run_logic Uint8.logand bus cpu arg
let run_ora bus cpu arg = run_logic Uint8.logor  bus cpu arg
let run_eor bus cpu arg = run_logic Uint8.logxor bus cpu arg
 
(* --- 比較 --- *)
 
let run_compare (reg : uint8) bus cpu arg =
  let m = operand_value bus cpu arg in
  let r = Uint8.to_int reg - Uint8.to_int m in
  let result = u8 (r land 0xFF) in
  cpu.reg_P <- PS.set_flags
    [ (C, Uint8.to_int reg >= Uint8.to_int m)
    ; (Z, reg = m)
    ; (N, Uint8.logand result bit7 <> Uint8.zero) ]
    cpu.reg_P
 
let run_cmp bus cpu arg = run_compare cpu.reg_A bus cpu arg
let run_cpx bus cpu arg = run_compare cpu.reg_X bus cpu arg
let run_cpy bus cpu arg = run_compare cpu.reg_Y bus cpu arg
 
(* --- BIT --- *)
 
let run_bit bus cpu arg =
  let m = operand_value bus cpu arg in
  let r = Uint8.logand cpu.reg_A m in
  cpu.reg_P <- PS.set_flags
    [ (Z, r = Uint8.zero)
    ; (V, Uint8.logand m (u8 0x40) <> Uint8.zero)
    ; (N, Uint8.logand m bit7 <> Uint8.zero) ]
    cpu.reg_P
 
(* --- インクリメント/デクリメント --- *)
 
(** メモリに対する read-modify-write。INC/DEC/ASL/LSR/ROL/ROR で共通。 *)
let modify_mem (bus:Bus.t) cpu arg (f : uint8 -> uint8) =
  match arg with
  | Ins.Accumulator ->
      let r = f cpu.reg_A in
      cpu.reg_A <- r; r
  | _ ->
      let addr = effective_addr bus cpu arg in
      let v = bus.read addr in
      let r = f v in
      bus.write addr r;
      r
 
let run_inc bus cpu arg =
  let r = modify_mem bus cpu arg (fun v -> Uint8.(v + one)) in
  set_zn cpu r
 
let run_dec bus cpu arg =
  let r = modify_mem bus cpu arg (fun v -> Uint8.(v - one)) in
  set_zn cpu r
 
let run_inx cpu = cpu.reg_X <- Uint8.(cpu.reg_X + one); set_zn cpu cpu.reg_X
let run_iny cpu = cpu.reg_Y <- Uint8.(cpu.reg_Y + one); set_zn cpu cpu.reg_Y
let run_dex cpu = cpu.reg_X <- Uint8.(cpu.reg_X - one); set_zn cpu cpu.reg_X
let run_dey cpu = cpu.reg_Y <- Uint8.(cpu.reg_Y - one); set_zn cpu cpu.reg_Y
 
(* --- シフト/ローテート --- *)
 
let run_asl bus cpu arg =
  let carry = ref false in
  let r = modify_mem bus cpu arg (fun v ->
    carry := Uint8.logand v bit7 <> Uint8.zero;
    Uint8.shift_left v 1)
  in
  set_flag cpu C !carry;
  set_zn cpu r
 
let run_lsr bus cpu arg =
  let carry = ref false in
  let r = modify_mem bus cpu arg (fun v ->
    carry := Uint8.logand v Uint8.one <> Uint8.zero;
    Uint8.shift_right v 1)  (* logical: 上位に0 *)
  in
  set_flag cpu C !carry;
  set_zn cpu r
 
let run_rol bus cpu arg =
  let old_c = if get_flag cpu C then Uint8.one else Uint8.zero in
  let carry = ref false in
  let r = modify_mem bus cpu arg (fun v ->
    carry := Uint8.logand v bit7 <> Uint8.zero;
    Uint8.logor (Uint8.shift_left v 1) old_c)
  in
  set_flag cpu C !carry;
  set_zn cpu r
 
let run_ror bus cpu arg =
  let old_c = if get_flag cpu C then bit7 else Uint8.zero in
  let carry = ref false in
  let r = modify_mem bus cpu arg (fun v ->
    carry := Uint8.logand v Uint8.one <> Uint8.zero;
    Uint8.logor (Uint8.shift_right v 1) old_c)
  in
  set_flag cpu C !carry;
  set_zn cpu r
 
(* --- ジャンプ/サブルーチン --- *)
 
let run_jmp cpu arg =
  match arg with
  | Ins.Absolute a -> cpu.reg_PC <- a
  | Ins.Indirect a -> cpu.reg_PC <- a   (* 注: 間接解決は fetch_arg 側で *)
  | _ -> raise Exn.Invalid_addressing_mode
 
let run_jsr bus cpu arg =
  match arg with
  | Ins.Absolute a ->
      (* 戻りアドレスは「次命令の1つ前」。fetch で PC は既に
         JSR の次を指しているので PC-1 を push する。 *)
      push16 bus cpu Uint16.(cpu.reg_PC - one);
      cpu.reg_PC <- a
  | _ -> raise Exn.Invalid_addressing_mode
 
let run_rts bus cpu =
  let addr = pull16 bus cpu in
  cpu.reg_PC <- Uint16.(addr + one)
 
let run_rti bus cpu =
  (* B フラグと R フラグはスタックから無視/固定する *)
  let p = pull bus cpu in
  cpu.reg_P <- PS.of_uint8 (Uint8.logor (Uint8.logand p (u8 0xCF)) (u8 0x20));
  cpu.reg_PC <- pull16 bus cpu
 
(* --- スタック --- *)
 
let run_pha bus cpu = push bus cpu cpu.reg_A
let run_pla bus cpu =
  cpu.reg_A <- pull bus cpu;
  set_zn cpu cpu.reg_A
 
(* PHP: B=1, R=1 を立てた状態で push(ハード仕様) *)
let run_php bus cpu =
  let v = Uint8.logor (PS.to_uint8 cpu.reg_P) (u8 0x30) in
  push bus cpu v
 
(* PLP: B と R は無視(R=1固定、B=0) *)
let run_plp bus cpu =
  let p = pull bus cpu in
  cpu.reg_P <- PS.of_uint8 (Uint8.logor (Uint8.logand p (u8 0xCF)) (u8 0x20))
 
(* --- 分岐 --- *)
 
(** 符号付き8bitオフセットで分岐する。 *)
let branch (cpu: Register.t) (off_u8:uint8) =
  let off = Uint8.to_int off_u8 in
  let signed = if off >= 0x80 then off - 0x100 else off in
  cpu.reg_PC <- Uint16.of_int ((Uint16.to_int cpu.reg_PC + signed) land 0xFFFF)
 
let run_branch (cond:bool) (cpu: Register.t) arg =
  match arg with
  | Ins.Relative off -> if cond then branch cpu off
  | _ -> raise Exn.Invalid_addressing_mode
 
(* --- フラグ操作 --- *)
 
let run_clc cpu = set_flag cpu C false
let run_sec cpu = set_flag cpu C true
let run_cli cpu = set_flag cpu I false
let run_sei cpu = set_flag cpu I true
let run_cld cpu = set_flag cpu D false
let run_sed cpu = set_flag cpu D true
let run_clv cpu = set_flag cpu V false
 
(* --- BRK/NOP --- *)
 
let run_brk (bus:Bus.t) (cpu: Register.t) ~ith_irq =
  push16 bus cpu Uint16.(cpu.reg_PC + one);
  push bus cpu (Uint8.logor (PS.to_uint8 cpu.reg_P) (u8 0x30));  (* B=1 で push *)
  set_flag cpu I true;
  cpu.reg_PC <- ith_irq
 
let run_nop () = ()
 
