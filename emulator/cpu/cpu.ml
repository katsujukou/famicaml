(* Per-cycle 6502 CPU (Phase A3+A5: 公式 152 opcode + NMI/IRQ/RESET).
   仕様コメントは .mli 参照。

   構造:
   - micro_op: 1 cycle 分の作業を表すクロージャ
   - addressing-mode テンプレ (zp_read/abs_x_write 等) が cycle 列を生成
   - 各命令の本体 (lda_op, asl_transform 等) は (cpu, value) -> ... の単純関数
   - decode は opcode -> (AM, op) を引いて pending に詰める *)

open Famicaml_common.Nesint

(* cpu.ml が dune の自動生成名前空間モジュール役を兼ねるため、
   サブモジュール (Register) を明示 re-export する。 *)
module Register = Register
module PS = Register.Processor_status

(* ------------------------------------------------------------------ *)
(* 型                                                                  *)
(* ------------------------------------------------------------------ *)

type t =
  { mutable reg_PC : uint16
  ; mutable reg_A : uint8
  ; mutable reg_X : uint8
  ; mutable reg_Y : uint8
  ; mutable reg_P : PS.t
  ; mutable reg_SP : uint8
  ; mutable cycles : int
  ; mutable pending : micro_op list
  ; mutable opcode : int
  ; mutable lo : int
  ; mutable hi : int
  ; mutable ptr : int
  ; mutable data : int
  ; mutable addr : int
  ; mutable nmi_pending : bool
  ; mutable irq_pending : bool
  ; mutable reset_pending : bool
    (* 6502 は IRQ を命令の penultimate (= 後ろから 2 番目) cycle で sample
       する. 命令境界 dispatch 時は「2 cycle 前の IRQ 状態」を使う必要がある.
       irq_latch_b は 2 cycle 前の (irq_pending && !I), irq_latch_a は 1 cycle 前.
       同じ仕組みを NMI にも適用. 結果: CLI/SEI/PLP の I flag 変更が IRQ
       delivery に 1 命令分の delay として現れる (実機通り). *)
  ; mutable irq_latch_a : bool
  ; mutable irq_latch_b : bool
  }

and micro_op = t -> Bus.t -> unit

let mk () =
  { reg_PC = Uint16.zero
  ; reg_A = Uint8.zero
  ; reg_X = Uint8.zero
  ; reg_Y = Uint8.zero
  ; reg_P = PS.initial
  ; reg_SP = Uint8.of_int 0xFD
  ; cycles = 0
  ; pending = []
  ; opcode = 0
  ; lo = 0
  ; hi = 0
  ; ptr = 0
  ; data = 0
  ; addr = 0
  ; nmi_pending = false
  ; irq_pending = false
  ; reset_pending = false
  ; irq_latch_a = false
  ; irq_latch_b = false
  }

(* ------------------------------------------------------------------ *)
(* 共通ヘルパー                                                         *)
(* ------------------------------------------------------------------ *)

let set_nz (cpu : t) (v : uint8) =
  let n = Uint8.to_int v land 0x80 <> 0 in
  let z = Uint8.to_int v = 0 in
  cpu.reg_P <- PS.set_flags [ (PS.N, n); (PS.Z, z) ] cpu.reg_P

let stack_addr (cpu : t) = Uint16.of_int (0x100 lor Uint8.to_int cpu.reg_SP)

let push (cpu : t) (bus : Bus.t) (b : uint8) =
  bus.write (stack_addr cpu) b;
  cpu.reg_SP <- Uint8.sub cpu.reg_SP Uint8.one

let pc_inc (cpu : t) = cpu.reg_PC <- Uint16.add cpu.reg_PC Uint16.one

(* 共通 micro_op: PC の指す byte を fetch して cpu.lo / cpu.hi / cpu.ptr に latch *)
let fetch_to_lo : micro_op =
  fun cpu bus ->
  cpu.lo <- Uint8.to_int (bus.read cpu.reg_PC);
  pc_inc cpu

let fetch_to_hi : micro_op =
  fun cpu bus ->
  cpu.hi <- Uint8.to_int (bus.read cpu.reg_PC);
  pc_inc cpu

let fetch_to_ptr : micro_op =
  fun cpu bus ->
  cpu.ptr <- Uint8.to_int (bus.read cpu.reg_PC);
  pc_inc cpu

(* PC の指す byte を dummy read (PC は進めない). 内部 cycle 用. *)
let dummy_read_pc : micro_op =
  fun cpu bus ->
  let _ : uint8 = bus.read cpu.reg_PC in
  ()

(* ------------------------------------------------------------------ *)
(* Addressing-mode テンプレ                                            *)
(*                                                                     *)
(* どのテンプレも「T1 (opcode fetch) を除く残り cycle 列」を返す。      *)
(* op : t -> uint8 -> unit  ←  読み出した値を使う処理 (LDA 等)         *)
(* src : t -> uint8         ←  書き込む値を返す関数 (STA 等)           *)
(* trans : t -> uint8 -> uint8  ←  RMW の値変換 (ASL/INC 等)           *)
(* ------------------------------------------------------------------ *)

(* --- implied / accumulator (2 cycle total) --- *)

let imp_op (op : t -> unit) : micro_op list =
  [ (fun cpu bus ->
      dummy_read_pc cpu bus;
      op cpu)
  ]

let acc_rmw (trans : t -> uint8 -> uint8) : micro_op list =
  [ (fun cpu bus ->
      dummy_read_pc cpu bus;
      cpu.reg_A <- trans cpu cpu.reg_A)
  ]

(* --- immediate (2 cycle total) --- *)

let imm_op (op : t -> uint8 -> unit) : micro_op list =
  [ (fun cpu bus ->
      let v = bus.read cpu.reg_PC in
      pc_inc cpu;
      op cpu v)
  ]

(* --- zero page --- *)

let zp_read (op : t -> uint8 -> unit) : micro_op list =
  [ fetch_to_lo
  ; (fun cpu bus ->
      let v = bus.read (Uint16.of_int cpu.lo) in
      op cpu v)
  ]

let zp_write (src : t -> uint8) : micro_op list =
  [ fetch_to_lo; (fun cpu bus -> bus.write (Uint16.of_int cpu.lo) (src cpu)) ]

let zp_rmw (trans : t -> uint8 -> uint8) : micro_op list =
  [ fetch_to_lo
  ; (fun cpu bus -> cpu.data <- Uint8.to_int (bus.read (Uint16.of_int cpu.lo)))
  ; (fun cpu bus ->
      (* dummy write: 古い値を書き直す *)
      bus.write (Uint16.of_int cpu.lo) (Uint8.of_int cpu.data))
  ; (fun cpu bus ->
      let new_v = trans cpu (Uint8.of_int cpu.data) in
      bus.write (Uint16.of_int cpu.lo) new_v)
  ]

(* --- zero page,X / zero page,Y --- *)

let zp_indexed_read (idx_reg : t -> uint8) (op : t -> uint8 -> unit)
  : micro_op list
  =
  [ fetch_to_lo
  ; (fun cpu bus ->
      let _ : uint8 = bus.read (Uint16.of_int cpu.lo) in
      cpu.addr <- (cpu.lo + Uint8.to_int (idx_reg cpu)) land 0xFF)
  ; (fun cpu bus ->
      let v = bus.read (Uint16.of_int cpu.addr) in
      op cpu v)
  ]

let zp_indexed_write (idx_reg : t -> uint8) (src : t -> uint8) : micro_op list =
  [ fetch_to_lo
  ; (fun cpu bus ->
      let _ : uint8 = bus.read (Uint16.of_int cpu.lo) in
      cpu.addr <- (cpu.lo + Uint8.to_int (idx_reg cpu)) land 0xFF)
  ; (fun cpu bus -> bus.write (Uint16.of_int cpu.addr) (src cpu))
  ]

let zp_x_rmw (trans : t -> uint8 -> uint8) : micro_op list =
  [ fetch_to_lo
  ; (fun cpu bus ->
      let _ : uint8 = bus.read (Uint16.of_int cpu.lo) in
      cpu.addr <- (cpu.lo + Uint8.to_int cpu.reg_X) land 0xFF)
  ; (fun cpu bus ->
      cpu.data <- Uint8.to_int (bus.read (Uint16.of_int cpu.addr)))
  ; (fun cpu bus -> bus.write (Uint16.of_int cpu.addr) (Uint8.of_int cpu.data))
  ; (fun cpu bus ->
      let new_v = trans cpu (Uint8.of_int cpu.data) in
      bus.write (Uint16.of_int cpu.addr) new_v)
  ]

let zp_x_read = zp_indexed_read (fun c -> c.reg_X)
let zp_y_read = zp_indexed_read (fun c -> c.reg_Y)
let zp_x_write = zp_indexed_write (fun c -> c.reg_X)
let zp_y_write = zp_indexed_write (fun c -> c.reg_Y)

(* --- absolute --- *)

let abs_read (op : t -> uint8 -> unit) : micro_op list =
  [ fetch_to_lo
  ; fetch_to_hi
  ; (fun cpu bus ->
      let eff = (cpu.hi lsl 8) lor cpu.lo in
      let v = bus.read (Uint16.of_int eff) in
      op cpu v)
  ]

let abs_write (src : t -> uint8) : micro_op list =
  [ fetch_to_lo
  ; fetch_to_hi
  ; (fun cpu bus ->
      let eff = (cpu.hi lsl 8) lor cpu.lo in
      bus.write (Uint16.of_int eff) (src cpu))
  ]

let abs_rmw (trans : t -> uint8 -> uint8) : micro_op list =
  [ fetch_to_lo
  ; fetch_to_hi
  ; (fun cpu bus ->
      let eff = (cpu.hi lsl 8) lor cpu.lo in
      cpu.addr <- eff;
      cpu.data <- Uint8.to_int (bus.read (Uint16.of_int eff)))
  ; (fun cpu bus -> bus.write (Uint16.of_int cpu.addr) (Uint8.of_int cpu.data))
  ; (fun cpu bus ->
      let new_v = trans cpu (Uint8.of_int cpu.data) in
      bus.write (Uint16.of_int cpu.addr) new_v)
  ]

(* --- absolute,X / absolute,Y ---
   Read: 4 cycle (+1 if page cross)
   Write: 5 cycle (always)
   RMW: 7 cycle (always) *)

let abs_indexed_read (idx_reg : t -> uint8) (op : t -> uint8 -> unit)
  : micro_op list
  =
  [ fetch_to_lo
  ; fetch_to_hi
  ; (fun cpu bus ->
      let idx = Uint8.to_int (idx_reg cpu) in
      let base = (cpu.hi lsl 8) lor cpu.lo in
      let eff = (base + idx) land 0xFFFF in
      let wrong = (cpu.hi lsl 8) lor ((cpu.lo + idx) land 0xFF) in
      cpu.addr <- eff;
      let v = bus.read (Uint16.of_int wrong) in
      if wrong = eff
      then op cpu v
      else
        cpu.pending
        <- (fun cpu bus ->
             let v2 = bus.read (Uint16.of_int cpu.addr) in
             op cpu v2)
           :: cpu.pending)
  ]

let abs_indexed_write (idx_reg : t -> uint8) (src : t -> uint8) : micro_op list =
  [ fetch_to_lo
  ; fetch_to_hi
  ; (fun cpu bus ->
      let idx = Uint8.to_int (idx_reg cpu) in
      let base = (cpu.hi lsl 8) lor cpu.lo in
      let eff = (base + idx) land 0xFFFF in
      let wrong = (cpu.hi lsl 8) lor ((cpu.lo + idx) land 0xFF) in
      cpu.addr <- eff;
      let _ : uint8 = bus.read (Uint16.of_int wrong) in
      ())
  ; (fun cpu bus -> bus.write (Uint16.of_int cpu.addr) (src cpu))
  ]

let abs_x_rmw (trans : t -> uint8 -> uint8) : micro_op list =
  [ fetch_to_lo
  ; fetch_to_hi
  ; (fun cpu bus ->
      let x = Uint8.to_int cpu.reg_X in
      let base = (cpu.hi lsl 8) lor cpu.lo in
      let eff = (base + x) land 0xFFFF in
      let wrong = (cpu.hi lsl 8) lor ((cpu.lo + x) land 0xFF) in
      cpu.addr <- eff;
      let _ : uint8 = bus.read (Uint16.of_int wrong) in
      ())
  ; (fun cpu bus ->
      cpu.data <- Uint8.to_int (bus.read (Uint16.of_int cpu.addr)))
  ; (fun cpu bus -> bus.write (Uint16.of_int cpu.addr) (Uint8.of_int cpu.data))
  ; (fun cpu bus ->
      let new_v = trans cpu (Uint8.of_int cpu.data) in
      bus.write (Uint16.of_int cpu.addr) new_v)
  ]

let abs_x_read = abs_indexed_read (fun c -> c.reg_X)
let abs_y_read = abs_indexed_read (fun c -> c.reg_Y)
let abs_x_write = abs_indexed_write (fun c -> c.reg_X)
let abs_y_write = abs_indexed_write (fun c -> c.reg_Y)

(* --- (zp,X) — pre-indexed indirect (6 cycle) --- *)

let ind_x_common (final : t -> Bus.t -> unit) : micro_op list =
  [ fetch_to_ptr
  ; (fun cpu bus ->
      let _ : uint8 = bus.read (Uint16.of_int cpu.ptr) in
      cpu.ptr <- (cpu.ptr + Uint8.to_int cpu.reg_X) land 0xFF)
  ; (fun cpu bus -> cpu.lo <- Uint8.to_int (bus.read (Uint16.of_int cpu.ptr)))
  ; (fun cpu bus ->
      cpu.hi
      <- Uint8.to_int (bus.read (Uint16.of_int ((cpu.ptr + 1) land 0xFF))))
  ; final
  ]

let ind_x_read (op : t -> uint8 -> unit) : micro_op list =
  ind_x_common (fun cpu bus ->
    let eff = (cpu.hi lsl 8) lor cpu.lo in
    let v = bus.read (Uint16.of_int eff) in
    op cpu v)

let ind_x_write (src : t -> uint8) : micro_op list =
  ind_x_common (fun cpu bus ->
    let eff = (cpu.hi lsl 8) lor cpu.lo in
    bus.write (Uint16.of_int eff) (src cpu))

(* --- (zp),Y — post-indexed indirect ---
   Read: 5 cycle (+1 if page cross)
   Write: 6 cycle (always) *)

let ind_y_read (op : t -> uint8 -> unit) : micro_op list =
  [ fetch_to_ptr
  ; (fun cpu bus -> cpu.lo <- Uint8.to_int (bus.read (Uint16.of_int cpu.ptr)))
  ; (fun cpu bus ->
      cpu.hi
      <- Uint8.to_int (bus.read (Uint16.of_int ((cpu.ptr + 1) land 0xFF))))
  ; (fun cpu bus ->
      let y = Uint8.to_int cpu.reg_Y in
      let base = (cpu.hi lsl 8) lor cpu.lo in
      let eff = (base + y) land 0xFFFF in
      let wrong = (cpu.hi lsl 8) lor ((cpu.lo + y) land 0xFF) in
      cpu.addr <- eff;
      let v = bus.read (Uint16.of_int wrong) in
      if wrong = eff
      then op cpu v
      else
        cpu.pending
        <- (fun cpu bus ->
             let v2 = bus.read (Uint16.of_int cpu.addr) in
             op cpu v2)
           :: cpu.pending)
  ]

let ind_y_write (src : t -> uint8) : micro_op list =
  [ fetch_to_ptr
  ; (fun cpu bus -> cpu.lo <- Uint8.to_int (bus.read (Uint16.of_int cpu.ptr)))
  ; (fun cpu bus ->
      cpu.hi
      <- Uint8.to_int (bus.read (Uint16.of_int ((cpu.ptr + 1) land 0xFF))))
  ; (fun cpu bus ->
      let y = Uint8.to_int cpu.reg_Y in
      let base = (cpu.hi lsl 8) lor cpu.lo in
      let eff = (base + y) land 0xFFFF in
      let wrong = (cpu.hi lsl 8) lor ((cpu.lo + y) land 0xFF) in
      cpu.addr <- eff;
      let _ : uint8 = bus.read (Uint16.of_int wrong) in
      ())
  ; (fun cpu bus -> bus.write (Uint16.of_int cpu.addr) (src cpu))
  ]

(* ------------------------------------------------------------------ *)
(* 個別命令の本体                                                       *)
(* ------------------------------------------------------------------ *)

(* --- ロード/ストア --- *)

let lda_op cpu v =
  cpu.reg_A <- v;
  set_nz cpu v

let ldx_op cpu v =
  cpu.reg_X <- v;
  set_nz cpu v

let ldy_op cpu v =
  cpu.reg_Y <- v;
  set_nz cpu v

let sta_src cpu = cpu.reg_A
let stx_src cpu = cpu.reg_X
let sty_src cpu = cpu.reg_Y

(* --- 算術 --- *)

let adc_op cpu v =
  let a = Uint8.to_int cpu.reg_A in
  let m = Uint8.to_int v in
  let c = if PS.get_flag PS.C cpu.reg_P then 1 else 0 in
  let sum = a + m + c in
  let result = sum land 0xFF in
  (* V: 同符号同士の加算で符号反転が起きたとき *)
  let overflow = a lxor result land (m lxor result) land 0x80 <> 0 in
  cpu.reg_A <- Uint8.of_int result;
  cpu.reg_P
  <- PS.set_flags
       [ (PS.C, sum > 0xFF)
       ; (PS.V, overflow)
       ; (PS.N, result land 0x80 <> 0)
       ; (PS.Z, result = 0)
       ]
       cpu.reg_P

let sbc_op cpu v =
  (* SBC = ADC with inverted operand. carry = "no borrow". *)
  adc_op cpu (Uint8.lognot v)

(* --- 論理 --- *)

let and_op cpu v =
  let r = Uint8.logand cpu.reg_A v in
  cpu.reg_A <- r;
  set_nz cpu r

let ora_op cpu v =
  let r = Uint8.logor cpu.reg_A v in
  cpu.reg_A <- r;
  set_nz cpu r

let eor_op cpu v =
  let r = Uint8.logxor cpu.reg_A v in
  cpu.reg_A <- r;
  set_nz cpu r

(* --- 比較 --- *)

let cmp_with (reg : t -> uint8) cpu v =
  let r = Uint8.to_int (reg cpu) in
  let m = Uint8.to_int v in
  let diff = (r - m) land 0xFF in
  cpu.reg_P
  <- PS.set_flags
       [ (PS.C, r >= m); (PS.N, diff land 0x80 <> 0); (PS.Z, diff = 0) ]
       cpu.reg_P

let cmp_op = cmp_with (fun c -> c.reg_A)
let cpx_op = cmp_with (fun c -> c.reg_X)
let cpy_op = cmp_with (fun c -> c.reg_Y)

let bit_op cpu v =
  let r = Uint8.logand cpu.reg_A v in
  let vi = Uint8.to_int v in
  cpu.reg_P
  <- PS.set_flags
       [ (PS.Z, Uint8.to_int r = 0)
       ; (PS.V, vi land 0x40 <> 0)
       ; (PS.N, vi land 0x80 <> 0)
       ]
       cpu.reg_P

(* --- INC/DEC (RMW + register) --- *)

let inc_trans _cpu v =
  let r = Uint8.add v Uint8.one in
  set_nz _cpu r;
  r

let dec_trans _cpu v =
  let r = Uint8.sub v Uint8.one in
  set_nz _cpu r;
  r

let inx cpu =
  cpu.reg_X <- Uint8.add cpu.reg_X Uint8.one;
  set_nz cpu cpu.reg_X

let iny cpu =
  cpu.reg_Y <- Uint8.add cpu.reg_Y Uint8.one;
  set_nz cpu cpu.reg_Y

let dex cpu =
  cpu.reg_X <- Uint8.sub cpu.reg_X Uint8.one;
  set_nz cpu cpu.reg_X

let dey cpu =
  cpu.reg_Y <- Uint8.sub cpu.reg_Y Uint8.one;
  set_nz cpu cpu.reg_Y

(* --- シフト/ローテート --- *)

let asl_trans cpu v =
  let in_ = Uint8.to_int v in
  let result = (in_ lsl 1) land 0xFF in
  cpu.reg_P
  <- PS.set_flags
       [ (PS.C, in_ land 0x80 <> 0)
       ; (PS.N, result land 0x80 <> 0)
       ; (PS.Z, result = 0)
       ]
       cpu.reg_P;
  Uint8.of_int result

let lsr_trans cpu v =
  let in_ = Uint8.to_int v in
  let result = in_ lsr 1 in
  cpu.reg_P
  <- PS.set_flags
       [ (PS.C, in_ land 1 <> 0); (PS.N, false); (PS.Z, result = 0) ]
       cpu.reg_P;
  Uint8.of_int result

let rol_trans cpu v =
  let in_ = Uint8.to_int v in
  let c_in = if PS.get_flag PS.C cpu.reg_P then 1 else 0 in
  let result = (in_ lsl 1) lor c_in land 0xFF in
  cpu.reg_P
  <- PS.set_flags
       [ (PS.C, in_ land 0x80 <> 0)
       ; (PS.N, result land 0x80 <> 0)
       ; (PS.Z, result = 0)
       ]
       cpu.reg_P;
  Uint8.of_int result

let ror_trans cpu v =
  let in_ = Uint8.to_int v in
  let c_in = if PS.get_flag PS.C cpu.reg_P then 0x80 else 0 in
  let result = (in_ lsr 1) lor c_in in
  cpu.reg_P
  <- PS.set_flags
       [ (PS.C, in_ land 1 <> 0)
       ; (PS.N, result land 0x80 <> 0)
       ; (PS.Z, result = 0)
       ]
       cpu.reg_P;
  Uint8.of_int result

(* --- レジスタ転送 --- *)

let tax cpu =
  cpu.reg_X <- cpu.reg_A;
  set_nz cpu cpu.reg_X

let tay cpu =
  cpu.reg_Y <- cpu.reg_A;
  set_nz cpu cpu.reg_Y

let tsx cpu =
  cpu.reg_X <- cpu.reg_SP;
  set_nz cpu cpu.reg_X

let txa cpu =
  cpu.reg_A <- cpu.reg_X;
  set_nz cpu cpu.reg_A

let tya cpu =
  cpu.reg_A <- cpu.reg_Y;
  set_nz cpu cpu.reg_A

let txs cpu = cpu.reg_SP <- cpu.reg_X (* TXS はフラグ不変 *)

(* --- フラグ操作 --- *)

let clc cpu = cpu.reg_P <- PS.set_flag PS.C false cpu.reg_P
let sec cpu = cpu.reg_P <- PS.set_flag PS.C true cpu.reg_P
let cli cpu = cpu.reg_P <- PS.set_flag PS.I false cpu.reg_P
let sei cpu = cpu.reg_P <- PS.set_flag PS.I true cpu.reg_P
let cld cpu = cpu.reg_P <- PS.set_flag PS.D false cpu.reg_P
let sed cpu = cpu.reg_P <- PS.set_flag PS.D true cpu.reg_P
let clv cpu = cpu.reg_P <- PS.set_flag PS.V false cpu.reg_P

(* --- スタック (PHA/PHP/PLA/PLP) --- *)

let pha_micros : micro_op list =
  [ dummy_read_pc; (fun cpu bus -> push cpu bus cpu.reg_A) ]

let php_micros : micro_op list =
  [ dummy_read_pc
  ; (fun cpu bus ->
      (* PHP は B と R を立てて push する *)
      let p = PS.set_flags [ (PS.B, true); (PS.R, true) ] cpu.reg_P in
      push cpu bus (PS.to_uint8 p))
  ]

let pla_micros : micro_op list =
  [ dummy_read_pc
  ; (fun cpu bus ->
      let _ : uint8 = bus.read (stack_addr cpu) in
      cpu.reg_SP <- Uint8.add cpu.reg_SP Uint8.one)
  ; (fun cpu bus ->
      cpu.reg_A <- bus.read (stack_addr cpu);
      set_nz cpu cpu.reg_A)
  ]

let plp_micros : micro_op list =
  [ dummy_read_pc
  ; (fun cpu bus ->
      let _ : uint8 = bus.read (stack_addr cpu) in
      cpu.reg_SP <- Uint8.add cpu.reg_SP Uint8.one)
  ; (fun cpu bus ->
      let pulled = bus.read (stack_addr cpu) in
      (* B/R は無視 (= 既存の値を残す) *)
      let cur_b = PS.get_flag PS.B cpu.reg_P in
      let cur_r = PS.get_flag PS.R cpu.reg_P in
      cpu.reg_P
      <- PS.set_flags [ (PS.B, cur_b); (PS.R, cur_r) ] (PS.of_uint8 pulled))
  ]

(* --- ジャンプ系 --- *)

let jmp_abs : micro_op list =
  [ fetch_to_lo
  ; (fun cpu bus ->
      cpu.hi <- Uint8.to_int (bus.read cpu.reg_PC);
      cpu.reg_PC <- Uint16.of_int ((cpu.hi lsl 8) lor cpu.lo))
  ]

(* JMP indirect: ハードウェアバグ含む.
   $XXFF の indirect は target 高位が $XX00 から読まれる (キャリーされない). *)
let jmp_ind : micro_op list =
  [ fetch_to_lo
  ; fetch_to_hi
  ; (fun cpu bus ->
      let ptr = (cpu.hi lsl 8) lor cpu.lo in
      cpu.data <- Uint8.to_int (bus.read (Uint16.of_int ptr)))
  ; (fun cpu bus ->
      let ptr_hi_addr = (cpu.hi lsl 8) lor ((cpu.lo + 1) land 0xFF) in
      let th = Uint8.to_int (bus.read (Uint16.of_int ptr_hi_addr)) in
      cpu.reg_PC <- Uint16.of_int ((th lsl 8) lor cpu.data))
  ]

let jsr_micros : micro_op list =
  [ fetch_to_lo
  ; (* T3: internal cycle, dummy read at stack *)
    (fun cpu bus ->
      let _ : uint8 = bus.read (stack_addr cpu) in
      ())
  ; (fun cpu bus ->
      let pch = Uint16.to_int cpu.reg_PC lsr 8 in
      push cpu bus (Uint8.of_int pch))
  ; (fun cpu bus ->
      let pcl = Uint16.to_int cpu.reg_PC land 0xFF in
      push cpu bus (Uint8.of_int pcl))
  ; (fun cpu bus ->
      cpu.hi <- Uint8.to_int (bus.read cpu.reg_PC);
      cpu.reg_PC <- Uint16.of_int ((cpu.hi lsl 8) lor cpu.lo))
  ]

let rts_micros : micro_op list =
  [ dummy_read_pc
  ; (fun cpu bus ->
      let _ : uint8 = bus.read (stack_addr cpu) in
      cpu.reg_SP <- Uint8.add cpu.reg_SP Uint8.one)
  ; (fun cpu bus ->
      cpu.lo <- Uint8.to_int (bus.read (stack_addr cpu));
      cpu.reg_SP <- Uint8.add cpu.reg_SP Uint8.one)
  ; (fun cpu bus -> cpu.hi <- Uint8.to_int (bus.read (stack_addr cpu)))
  ; (fun cpu bus ->
      let pulled = (cpu.hi lsl 8) lor cpu.lo in
      let _ : uint8 = bus.read (Uint16.of_int pulled) in
      cpu.reg_PC <- Uint16.of_int ((pulled + 1) land 0xFFFF))
  ]

let rti_micros : micro_op list =
  [ dummy_read_pc
  ; (fun cpu bus ->
      let _ : uint8 = bus.read (stack_addr cpu) in
      cpu.reg_SP <- Uint8.add cpu.reg_SP Uint8.one)
  ; (fun cpu bus ->
      let pulled = bus.read (stack_addr cpu) in
      let cur_b = PS.get_flag PS.B cpu.reg_P in
      let cur_r = PS.get_flag PS.R cpu.reg_P in
      cpu.reg_P
      <- PS.set_flags [ (PS.B, cur_b); (PS.R, cur_r) ] (PS.of_uint8 pulled);
      cpu.reg_SP <- Uint8.add cpu.reg_SP Uint8.one)
  ; (fun cpu bus ->
      cpu.lo <- Uint8.to_int (bus.read (stack_addr cpu));
      cpu.reg_SP <- Uint8.add cpu.reg_SP Uint8.one)
  ; (fun cpu bus ->
      cpu.hi <- Uint8.to_int (bus.read (stack_addr cpu));
      cpu.reg_PC <- Uint16.of_int ((cpu.hi lsl 8) lor cpu.lo))
  ]

(* BRK: 7 cycle. PC を +1 して push (B フラグ立てる), $FFFE/F ベクタへ. *)
let brk_micros : micro_op list =
  [ (* T2: dummy read at PC, PC++ *)
    (fun cpu bus ->
      let _ : uint8 = bus.read cpu.reg_PC in
      pc_inc cpu)
  ; (fun cpu bus ->
      let pch = Uint16.to_int cpu.reg_PC lsr 8 in
      push cpu bus (Uint8.of_int pch))
  ; (fun cpu bus ->
      let pcl = Uint16.to_int cpu.reg_PC land 0xFF in
      push cpu bus (Uint8.of_int pcl))
  ; (fun cpu bus ->
      let p = PS.set_flags [ (PS.B, true); (PS.R, true) ] cpu.reg_P in
      push cpu bus (PS.to_uint8 p);
      cpu.reg_P <- PS.set_flag PS.I true cpu.reg_P)
  ; (fun cpu bus -> cpu.lo <- Uint8.to_int (bus.read (Uint16.of_int 0xFFFE)))
  ; (fun cpu bus ->
      cpu.hi <- Uint8.to_int (bus.read (Uint16.of_int 0xFFFF));
      cpu.reg_PC <- Uint16.of_int ((cpu.hi lsl 8) lor cpu.lo))
  ]

(* --- 分岐 (相対) --- *)

let branch_t4 : micro_op =
  fun cpu bus ->
  let _ : uint8 = bus.read cpu.reg_PC in
  (* dummy at wrong page *)
  cpu.reg_PC <- Uint16.of_int cpu.addr

let branch_t3 : micro_op =
  fun cpu bus ->
  let _ : uint8 = bus.read cpu.reg_PC in
  let signed = if cpu.lo < 0x80 then cpu.lo else cpu.lo - 0x100 in
  let cur_pc = Uint16.to_int cpu.reg_PC in
  let new_pc = (cur_pc + signed) land 0xFFFF in
  if cur_pc land 0xFF00 = new_pc land 0xFF00
  then cpu.reg_PC <- Uint16.of_int new_pc
  else (
    cpu.addr <- new_pc;
    cpu.pending <- branch_t4 :: cpu.pending)

let rel_branch (cond : t -> bool) : micro_op list =
  [ (* T2: fetch offset; if condition fails, end here. *)
    (fun cpu bus ->
      cpu.lo <- Uint8.to_int (bus.read cpu.reg_PC);
      pc_inc cpu;
      if cond cpu then cpu.pending <- branch_t3 :: cpu.pending)
  ]

(* ------------------------------------------------------------------ *)
(* Decode                                                              *)
(* ------------------------------------------------------------------ *)

let decode (op : int) : micro_op list =
  match op with
  (* --- ロード --- *)
  | 0xA9 -> imm_op lda_op
  | 0xA5 -> zp_read lda_op
  | 0xB5 -> zp_x_read lda_op
  | 0xAD -> abs_read lda_op
  | 0xBD -> abs_x_read lda_op
  | 0xB9 -> abs_y_read lda_op
  | 0xA1 -> ind_x_read lda_op
  | 0xB1 -> ind_y_read lda_op
  | 0xA2 -> imm_op ldx_op
  | 0xA6 -> zp_read ldx_op
  | 0xB6 -> zp_y_read ldx_op
  | 0xAE -> abs_read ldx_op
  | 0xBE -> abs_y_read ldx_op
  | 0xA0 -> imm_op ldy_op
  | 0xA4 -> zp_read ldy_op
  | 0xB4 -> zp_x_read ldy_op
  | 0xAC -> abs_read ldy_op
  | 0xBC -> abs_x_read ldy_op
  (* --- ストア --- *)
  | 0x85 -> zp_write sta_src
  | 0x95 -> zp_x_write sta_src
  | 0x8D -> abs_write sta_src
  | 0x9D -> abs_x_write sta_src
  | 0x99 -> abs_y_write sta_src
  | 0x81 -> ind_x_write sta_src
  | 0x91 -> ind_y_write sta_src
  | 0x86 -> zp_write stx_src
  | 0x96 -> zp_y_write stx_src
  | 0x8E -> abs_write stx_src
  | 0x84 -> zp_write sty_src
  | 0x94 -> zp_x_write sty_src
  | 0x8C -> abs_write sty_src
  (* --- レジスタ転送 --- *)
  | 0xAA -> imp_op tax
  | 0xA8 -> imp_op tay
  | 0xBA -> imp_op tsx
  | 0x8A -> imp_op txa
  | 0x98 -> imp_op tya
  | 0x9A -> imp_op txs
  (* --- スタック --- *)
  | 0x48 -> pha_micros
  | 0x08 -> php_micros
  | 0x68 -> pla_micros
  | 0x28 -> plp_micros
  (* --- 算術 --- *)
  | 0x69 -> imm_op adc_op
  | 0x65 -> zp_read adc_op
  | 0x75 -> zp_x_read adc_op
  | 0x6D -> abs_read adc_op
  | 0x7D -> abs_x_read adc_op
  | 0x79 -> abs_y_read adc_op
  | 0x61 -> ind_x_read adc_op
  | 0x71 -> ind_y_read adc_op
  | 0xE9 -> imm_op sbc_op
  | 0xE5 -> zp_read sbc_op
  | 0xF5 -> zp_x_read sbc_op
  | 0xED -> abs_read sbc_op
  | 0xFD -> abs_x_read sbc_op
  | 0xF9 -> abs_y_read sbc_op
  | 0xE1 -> ind_x_read sbc_op
  | 0xF1 -> ind_y_read sbc_op
  (* --- 論理 --- *)
  | 0x29 -> imm_op and_op
  | 0x25 -> zp_read and_op
  | 0x35 -> zp_x_read and_op
  | 0x2D -> abs_read and_op
  | 0x3D -> abs_x_read and_op
  | 0x39 -> abs_y_read and_op
  | 0x21 -> ind_x_read and_op
  | 0x31 -> ind_y_read and_op
  | 0x09 -> imm_op ora_op
  | 0x05 -> zp_read ora_op
  | 0x15 -> zp_x_read ora_op
  | 0x0D -> abs_read ora_op
  | 0x1D -> abs_x_read ora_op
  | 0x19 -> abs_y_read ora_op
  | 0x01 -> ind_x_read ora_op
  | 0x11 -> ind_y_read ora_op
  | 0x49 -> imm_op eor_op
  | 0x45 -> zp_read eor_op
  | 0x55 -> zp_x_read eor_op
  | 0x4D -> abs_read eor_op
  | 0x5D -> abs_x_read eor_op
  | 0x59 -> abs_y_read eor_op
  | 0x41 -> ind_x_read eor_op
  | 0x51 -> ind_y_read eor_op
  (* --- 比較 --- *)
  | 0xC9 -> imm_op cmp_op
  | 0xC5 -> zp_read cmp_op
  | 0xD5 -> zp_x_read cmp_op
  | 0xCD -> abs_read cmp_op
  | 0xDD -> abs_x_read cmp_op
  | 0xD9 -> abs_y_read cmp_op
  | 0xC1 -> ind_x_read cmp_op
  | 0xD1 -> ind_y_read cmp_op
  | 0xE0 -> imm_op cpx_op
  | 0xE4 -> zp_read cpx_op
  | 0xEC -> abs_read cpx_op
  | 0xC0 -> imm_op cpy_op
  | 0xC4 -> zp_read cpy_op
  | 0xCC -> abs_read cpy_op
  | 0x24 -> zp_read bit_op
  | 0x2C -> abs_read bit_op
  (* --- インクリメント/デクリメント --- *)
  | 0xE6 -> zp_rmw inc_trans
  | 0xF6 -> zp_x_rmw inc_trans
  | 0xEE -> abs_rmw inc_trans
  | 0xFE -> abs_x_rmw inc_trans
  | 0xC6 -> zp_rmw dec_trans
  | 0xD6 -> zp_x_rmw dec_trans
  | 0xCE -> abs_rmw dec_trans
  | 0xDE -> abs_x_rmw dec_trans
  | 0xE8 -> imp_op inx
  | 0xC8 -> imp_op iny
  | 0xCA -> imp_op dex
  | 0x88 -> imp_op dey
  (* --- シフト/ローテート --- *)
  | 0x0A -> acc_rmw asl_trans
  | 0x06 -> zp_rmw asl_trans
  | 0x16 -> zp_x_rmw asl_trans
  | 0x0E -> abs_rmw asl_trans
  | 0x1E -> abs_x_rmw asl_trans
  | 0x4A -> acc_rmw lsr_trans
  | 0x46 -> zp_rmw lsr_trans
  | 0x56 -> zp_x_rmw lsr_trans
  | 0x4E -> abs_rmw lsr_trans
  | 0x5E -> abs_x_rmw lsr_trans
  | 0x2A -> acc_rmw rol_trans
  | 0x26 -> zp_rmw rol_trans
  | 0x36 -> zp_x_rmw rol_trans
  | 0x2E -> abs_rmw rol_trans
  | 0x3E -> abs_x_rmw rol_trans
  | 0x6A -> acc_rmw ror_trans
  | 0x66 -> zp_rmw ror_trans
  | 0x76 -> zp_x_rmw ror_trans
  | 0x6E -> abs_rmw ror_trans
  | 0x7E -> abs_x_rmw ror_trans
  (* --- ジャンプ/サブルーチン --- *)
  | 0x4C -> jmp_abs
  | 0x6C -> jmp_ind
  | 0x20 -> jsr_micros
  | 0x60 -> rts_micros
  | 0x40 -> rti_micros
  (* --- 分岐 --- *)
  | 0x90 -> rel_branch (fun c -> not (PS.get_flag PS.C c.reg_P)) (* BCC *)
  | 0xB0 -> rel_branch (fun c -> PS.get_flag PS.C c.reg_P) (* BCS *)
  | 0xF0 -> rel_branch (fun c -> PS.get_flag PS.Z c.reg_P) (* BEQ *)
  | 0xD0 -> rel_branch (fun c -> not (PS.get_flag PS.Z c.reg_P)) (* BNE *)
  | 0x30 -> rel_branch (fun c -> PS.get_flag PS.N c.reg_P) (* BMI *)
  | 0x10 -> rel_branch (fun c -> not (PS.get_flag PS.N c.reg_P)) (* BPL *)
  | 0x50 -> rel_branch (fun c -> not (PS.get_flag PS.V c.reg_P)) (* BVC *)
  | 0x70 -> rel_branch (fun c -> PS.get_flag PS.V c.reg_P) (* BVS *)
  (* --- フラグ操作 --- *)
  | 0x18 -> imp_op clc
  | 0x38 -> imp_op sec
  | 0x58 -> imp_op cli
  | 0x78 -> imp_op sei
  | 0xD8 -> imp_op cld
  | 0xF8 -> imp_op sed
  | 0xB8 -> imp_op clv
  (* --- その他 --- *)
  | 0xEA -> imp_op (fun _cpu -> ()) (* NOP *)
  | 0x00 -> brk_micros
  (* 未定義 (illegal) opcode は対応なし *)
  | op ->
    failwith (Printf.sprintf "Cpu.decode: illegal/undefined opcode $%02X" op)

(* ------------------------------------------------------------------ *)
(* 割り込みシーケンス (NMI / IRQ / RESET)                                *)
(*                                                                     *)
(* どの割り込みも 7 cycle で構成され、共通の骨格は:                       *)
(*   T1-T2: dummy read at PC                                            *)
(*   T3-T5: PCH / PCL / P を push (RESET は writes 抑制で SP-- のみ)     *)
(*   T6:    read vector lo  ($FFFA NMI / $FFFC RESET / $FFFE IRQ/BRK)   *)
(*   T7:    read vector hi、PC へ反映、I フラグ立て                      *)
(* ------------------------------------------------------------------ *)

(** vector lo/hi のアドレスを指定して 7 cycle の割り込みシーケンスを構築する。
    push_p_b は push される P の B フラグ (NMI/IRQ=false, BRK=true; RESET 不要). *)
let int_micros ~vec_lo ~push_p_b : micro_op list =
  let vec_hi = vec_lo + 1 in
  [ dummy_read_pc
  ; dummy_read_pc
  ; (fun cpu bus ->
      let pch = Uint16.to_int cpu.reg_PC lsr 8 in
      push cpu bus (Uint8.of_int pch))
  ; (fun cpu bus ->
      let pcl = Uint16.to_int cpu.reg_PC land 0xFF in
      push cpu bus (Uint8.of_int pcl))
  ; (fun cpu bus ->
      let p = PS.set_flags [ (PS.B, push_p_b); (PS.R, true) ] cpu.reg_P in
      push cpu bus (PS.to_uint8 p);
      cpu.reg_P <- PS.set_flag PS.I true cpu.reg_P)
  ; (fun cpu bus -> cpu.lo <- Uint8.to_int (bus.read (Uint16.of_int vec_lo)))
  ; (fun cpu bus ->
      cpu.hi <- Uint8.to_int (bus.read (Uint16.of_int vec_hi));
      cpu.reg_PC <- Uint16.of_int ((cpu.hi lsl 8) lor cpu.lo))
  ]

let nmi_micros : micro_op list = int_micros ~vec_lo:0xFFFA ~push_p_b:false
let irq_micros : micro_op list = int_micros ~vec_lo:0xFFFE ~push_p_b:false

(** RESET: NMI/IRQ と同骨格だが、T3-T5 の push は writes が抑制される
    (read として実行され、値は捨てる)。SP-- だけ進む。 *)
let reset_micros : micro_op list =
  let suppressed_push : micro_op =
    fun cpu bus ->
    let _ : uint8 = bus.read (stack_addr cpu) in
    cpu.reg_SP <- Uint8.sub cpu.reg_SP Uint8.one
  in
  [ dummy_read_pc
  ; dummy_read_pc
  ; suppressed_push
  ; suppressed_push
  ; suppressed_push
  ; (fun cpu bus -> cpu.lo <- Uint8.to_int (bus.read (Uint16.of_int 0xFFFC)))
  ; (fun cpu bus ->
      cpu.hi <- Uint8.to_int (bus.read (Uint16.of_int 0xFFFD));
      cpu.reg_PC <- Uint16.of_int ((cpu.hi lsl 8) lor cpu.lo);
      cpu.reg_P <- PS.set_flag PS.I true cpu.reg_P)
  ]

let opcode_base_cycles (op : int) : int = 1 + List.length (decode op)
let request_nmi (cpu : t) = cpu.nmi_pending <- true
let request_irq (cpu : t) = cpu.irq_pending <- true
let request_reset (cpu : t) = cpu.reset_pending <- true

(* ------------------------------------------------------------------ *)
(* tick / step_instruction                                             *)
(* ------------------------------------------------------------------ *)

let fetch_opcode_cycle (cpu : t) (bus : Bus.t) =
  let op = Uint8.to_int (bus.read cpu.reg_PC) in
  cpu.opcode <- op;
  pc_inc cpu;
  cpu.pending <- decode op

(** 命令境界 (pending = []) に達したとき、保留割り込みがあれば
    pending に割り込みシーケンスを差し込む。優先度: RESET > NMI > IRQ.
    IRQ は I フラグが 0 のときだけサービスされる。
    サービス時に対応する pending フラグを auto-clear する。 *)
let dispatch_interrupt_if_pending (cpu : t) : unit =
  if cpu.reset_pending
  then (
    cpu.reset_pending <- false;
    cpu.pending <- reset_micros)
  else if cpu.nmi_pending
  then (
    cpu.nmi_pending <- false;
    cpu.pending <- nmi_micros)
  else if cpu.irq_latch_b
  then (
    cpu.irq_pending <- false;
    cpu.pending <- irq_micros)

let tick (bus : Bus.t) (cpu : t) : unit =
  if cpu.pending = [] then dispatch_interrupt_if_pending cpu;
  (match cpu.pending with
   | [] -> fetch_opcode_cycle cpu bus
   | m :: rest ->
     cpu.pending <- rest;
     m cpu bus);
  (* IRQ sampling: penultimate-cycle polling を 2 段 latch でモデル化.
     dispatch は latch_b を使う (= 2 cycle 前の "irq_pending && !I" 状態).
     これで CLI/SEI 直後の 1 命令分の delay が再現される (実機通り).
     NMI は edge-triggered で CLI/SEI 影響を受けないため即時 dispatch のまま.
     最適化: irq_pending も latch も全 false なら state 変化なし → skip. *)
  if cpu.irq_pending || cpu.irq_latch_a || cpu.irq_latch_b
  then (
    let irq_now =
      cpu.irq_pending && not (PS.get_flag PS.I cpu.reg_P)
    in
    cpu.irq_latch_b <- cpu.irq_latch_a;
    cpu.irq_latch_a <- irq_now);
  cpu.cycles <- cpu.cycles + 1

let step_instruction (bus : Bus.t) (cpu : t) : int =
  let start = cpu.cycles in
  if cpu.pending = [] then tick bus cpu;
  while cpu.pending <> [] do
    tick bus cpu
  done;
  cpu.cycles - start
