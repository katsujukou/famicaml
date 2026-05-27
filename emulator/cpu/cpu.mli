(** Per-cycle 6502 CPU (Phase A5: 公式 152 opcode + NMI/IRQ/RESET).

    アプローチ A (explicit state machine + addressing-mode pipeline) で
    実装した cycle-accurate な 6502 エミュレータ。各 cycle で発生する
    バスアクセスを 1 つずつ明示するため、IDE 用エミュとして必要な
    精度 (sprite 0 hit、NMI 配信タイミング、RMW dummy write 等) を
    満たす。 *)

open Famicaml_common.Nesint

(** レジスタ関連サブモジュールの re-export。 *)
module Register = Register

module PS = Register.Processor_status

(** 1 cycle で行う作業を表すクロージャ。[cpu] と [bus] を受け取って
    1 cycle 分の副作用 (バス read/write, レジスタ更新, latch 設定など)
    を起こす。 *)
type micro_op

(** CPU 状態。可視レジスタ + パイプライン latch + サイクルカウンタ。 *)
type t =
  { (* ----- 6502 可視レジスタ ----- *)
    mutable reg_PC : uint16
  ; mutable reg_A : uint8
  ; mutable reg_X : uint8
  ; mutable reg_Y : uint8
  ; mutable reg_P : Register.Processor_status.t
  ; mutable reg_SP : uint8
    (* ----- 起動からの cycle 数 (デバッガ・計測用) -----
                              wasm_of_ocaml 6.2 の Int64 ↔ JS 変換が不安定なため
                              int (63bit) を使う。NES 1.79 MHz でも 160000 年動かせる. *)
  ; mutable cycles : int (* ----- パイプライン内部状態 ----- *)
  ; mutable pending : micro_op list
    (** 残り cycle の micro_op 列。空のとき: 次 tick が
            fetch_opcode + decode を行い、このリストを埋める. *)
  ; mutable opcode : int (** 現在実行中の opcode ($00..$FF) *)
  ; mutable lo : int (** operand low byte (effective addr の下位) *)
  ; mutable hi : int (** operand high byte (effective addr の上位) *)
  ; mutable ptr : int (** indirect 系で使う pointer *)
  ; mutable data : int (** RMW 系で latch する被演算データ *)
  ; mutable addr : int
    (* ----- 割り込みラッチ -----
       各 request_* で立ち、サービス時 (interrupt sequence 開始時) に自動クリア。
       NMI は edge-triggered、IRQ は本実装では auto-clear で擬似的に
       level-triggered を表現する (詳細は request_irq 参照)。 *)
    (** 計算済み effective address (16 bit) *)
  ; mutable nmi_pending : bool
  ; mutable irq_pending : bool
  ; mutable reset_pending : bool
  ; mutable irq_latch_a : bool
  ; mutable irq_latch_b : bool
  }

(** 初期状態の CPU を生成する。PC=0, A=X=Y=0, SP=$FD, P=I|R,
    cycles=0, pending=[] (= 次 tick で fetch_opcode から始まる). *)
val mk : unit -> t

(** 1 CPU cycle 進める。

    - [cpu.pending = []] のとき: PC からオペコードをフェッチして
      decode、その命令の残り cycle 列を pending に積む。
    - そうでないとき: pending の先頭の micro_op を 1 つ実行する.

    どちらの場合も [cpu.cycles] は 1 つ進む。 *)
val tick : Bus.t -> t -> unit

(** 次の命令境界 ([pending = []] に戻る) まで [tick] し続け、その命令
    (または割り込みシーケンス) 全体の cycle 数を返す。

    呼び出し時点で [pending <> []] (命令の途中) なら、その命令を完了
    させた上で cycle 数を返す。 *)
val step_instruction : Bus.t -> t -> int

(** Opcode の "base" cycle 数 (= 1 cycle opcode fetch + decode が返す
    micro_op list 長). branch taken / page-cross 等の動的 penalty は
    含まない. cycle audit 用. *)
val opcode_base_cycles : int -> int

(** NMI 要求。次の命令境界で 7 cycle の NMI シーケンスが実行され、
    PC ← [$FFFA/B] となる。Edge-triggered: サービス時に自動クリア
    されるので、PPU 等は vblank 突入の瞬間にのみ呼ぶこと
    (毎 cycle 呼ぶと意味が変わる)。 *)
val request_nmi : t -> unit

(** IRQ 要求。次の命令境界で I フラグが 0 なら 7 cycle の IRQ
    シーケンスが実行され、PC ← [$FFFE/F] となる。

    本実装ではサービス時に auto-clear する (簡易化)。
    APU など level-triggered な源は、毎フレームの IRQ ポイントで
    再度 [request_irq] を呼ぶ運用とする。 *)
val request_irq : t -> unit

(** RESET 要求。次の命令境界で 7 cycle の RESET シーケンスが実行され、
    SP -= 3、I フラグ立て、PC ← [$FFFC/D]。push 動作は書き込みが
    抑制される (実機通り、SP のみ進む)。

    [request_reset] は他の割り込みより高い優先度を持つ。 *)
val request_reset : t -> unit

(** State serialize. precondition: pending = [] (= instruction 境界). *)
val serialize : Buffer.t -> t -> unit

(** State deserialize. cursor は Bytes 内の現在位置. pending は [] にリセット. *)
val deserialize : Bytes.t -> int ref -> t -> unit
