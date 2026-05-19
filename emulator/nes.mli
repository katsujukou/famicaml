open Stdint

(** NES 本体の状態。CPU・バス・割り込みベクタを保持する。 *)
type t = {
  mutable power : bool;

  cpu        : Cpu.t;
  memory_bus : Bus.t;

  ith_nmi   : uint16;
  ith_reset : uint16;
  ith_irq   : uint16;
}

(** CPU WRAM (2KB) をマッピングした初期状態の NES を作成する。
    ROM のロードや割り込みベクタの設定は呼び出し元が行う。 *)
val mk : unit -> t
