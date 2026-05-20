open Famicaml_common.Nesint

(** レジスタモジュール。外部からは [Emulator.Cpu.R] でアクセスできる。 *)
module R = Register

(** プロセッサステータスモジュール。外部からは [Emulator.Cpu.PS] でアクセスできる。 *)
module PS = Register.Processor_status

(** CPU 状態型。レジスタ群のレコード。 *)
type t = Register.t

(** 初期状態の CPU を作成する。
    PC=0, A/X/Y=0, SP=$FD, P=I|R ($24)。 *)
val mk : unit -> t

(** PC の示すアドレスから 1 命令をフェッチして PC を進める。 *)
val fetch : Bus.t -> t -> Instruction.t

(** フェッチ済み命令を実行する。[ith_irq] は BRK 命令のジャンプ先。 *)
val execute : Bus.t -> t -> ith_irq:uint16 -> Instruction.t -> unit

(** fetch + execute をまとめて行い、消費サイクル数を返す。 *)
val step : Bus.t -> t -> ith_irq:uint16 -> int
