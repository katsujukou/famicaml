open Famicaml_common.Nesint

module R = Register 
module PS = Register.Processor_status
module Ins = Instruction
type t = Register.t

let mk () = {
  R.reg_PC = Uint16.of_int 0;
  R.reg_A = Uint8.of_int 0;
  R.reg_X = Uint8.of_int 0;
  R.reg_Y = Uint8.of_int 00;
  R.reg_P = PS.initial;
  R.reg_SP = Uint8.of_int 0xFD;
}

open Logic_util

let fetch_arg (bus:Bus.t) (a:Bus.addr) 
  = Ins.(
  function
  | IMP -> Implied
  | ACC -> Accumulator
  | IMD -> Immediate (peek bus a)
  | ZP -> Zeropage (peek bus a)
  | ZP_X -> Zeropage_X (peek bus a)
  | ZP_Y -> Zeropage_Y (peek bus a)
  | ABS -> Absolute (peek_16 bus a)
  | ABS_X -> Absolute_X (peek_16 bus a)
  | ABS_Y -> Absolute_Y (peek_16 bus a)
  | REL -> Relative (peek bus a)
  | IND -> Indirect (peek_16 bus a)
  | IND_X -> Indirect_X (peek bus a)
  | IND_Y -> Indirect_Y (peek bus a)
  )

let fetch (bus:Bus.t) (cpu: Register.t)
  : Ins.t
  = let code = peek bus cpu.reg_PC in 
    match Ins.lookup code with
    | None -> raise Exn.Undefined_opcode 
    | Some spec ->
      let arg = 
        (* JMP ($XXFF) のハードウェアバグ再現
           See: https://www.nesdev.org/wiki/Instruction_reference#JMP
        *)
        if spec.op = Ins.JMP && spec.mode = Ins.IND
        then
          let vector = peek_16 bus Uint16.(cpu.reg_PC + one) in
          Ins.Absolute (peek_16_buggy bus vector)
        else fetch_arg bus Uint16.(cpu.reg_PC + one) spec.mode
      in
      cpu.reg_PC <- Uint16.(cpu.reg_PC + of_int spec.bytes);
      { op = spec.op; arg; cycles = spec.cycles }

(* ------------------------------------------------------------------ *)
(* ディスパッチャ                                                       *)
(* ------------------------------------------------------------------ *)
open Logic 

let execute (bus:Bus.t) (cpu: Register.t) ~ith_irq (inst:Ins.t) : unit =
  let arg = inst.arg in
  match inst.op with
  (* ロード/ストア *)
  | LDA -> run_lda bus cpu arg
  | LDX -> run_ldx bus cpu arg
  | LDY -> run_ldy bus cpu arg
  | STA -> run_sta bus cpu arg
  | STX -> run_stx bus cpu arg
  | STY -> run_sty bus cpu arg
  (* 転送 *)
  | TAX -> run_tax cpu
  | TAY -> run_tay cpu
  | TXA -> run_txa cpu
  | TYA -> run_tya cpu
  | TSX -> run_tsx cpu
  | TXS -> run_txs cpu
  (* 算術 *)
  | ADC -> run_adc bus cpu arg
  | SBC -> run_sbc bus cpu arg
  (* 論理 *)
  | AND -> run_and bus cpu arg
  | ORA -> run_ora bus cpu arg
  | EOR -> run_eor bus cpu arg
  (* 比較 *)
  | CMP -> run_cmp bus cpu arg
  | CPX -> run_cpx bus cpu arg
  | CPY -> run_cpy bus cpu arg
  | BIT -> run_bit bus cpu arg
  (* インクリメント/デクリメント *)
  | INC -> run_inc bus cpu arg
  | DEC -> run_dec bus cpu arg
  | INX -> run_inx cpu
  | INY -> run_iny cpu
  | DEX -> run_dex cpu
  | DEY -> run_dey cpu
  (* シフト/ローテート *)
  | ASL -> run_asl bus cpu arg
  | LSR -> run_lsr bus cpu arg
  | ROL -> run_rol bus cpu arg
  | ROR -> run_ror bus cpu arg
  (* ジャンプ/サブルーチン *)
  | JMP -> run_jmp cpu arg
  | JSR -> run_jsr bus cpu arg
  | RTS -> run_rts bus cpu
  | RTI -> run_rti bus cpu
  (* スタック *)
  | PHA -> run_pha bus cpu
  | PLA -> run_pla bus cpu
  | PHP -> run_php bus cpu
  | PLP -> run_plp bus cpu
  (* 分岐 *)
  | BCC -> run_branch (not (get_flag cpu C)) cpu arg
  | BCS -> run_branch (get_flag cpu C) cpu arg
  | BEQ -> run_branch (get_flag cpu Z) cpu arg
  | BNE -> run_branch (not (get_flag cpu Z)) cpu arg
  | BMI -> run_branch (get_flag cpu N) cpu arg
  | BPL -> run_branch (not (get_flag cpu N)) cpu arg
  | BVS -> run_branch (get_flag cpu V) cpu arg
  | BVC -> run_branch (not (get_flag cpu V)) cpu arg
  (* フラグ操作 *)
  | CLC -> run_clc cpu
  | SEC -> run_sec cpu
  | CLI -> run_cli cpu
  | SEI -> run_sei cpu
  | CLD -> run_cld cpu
  | SED -> run_sed cpu
  | CLV -> run_clv cpu
  (* その他 *)
  | BRK -> run_brk bus cpu ~ith_irq
  | NOP -> run_nop ()
 
let step (bus:Bus.t) (cpu: t) ~ith_irq : int =
  let inst = fetch bus cpu in
  execute bus cpu ~ith_irq inst;
  inst.cycles