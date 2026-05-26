(** RP2A03 APU (Phase D1A: Pulse 1+2 + Frame counter + $4015).

    本実装は段階的に進める. 現段階で実装済み:
    - Pulse 1 / Pulse 2: envelope, length counter, sweep, 8-step sequencer
    - Frame counter ($4017): 4-step / 5-step、quarter/half frame clocking
    - Frame IRQ ($4017 bit 6 = 0 のとき)
    - $4015 read/write (チャネル enable + IRQ flag)
    - APU clock = CPU の半分 (= 2 CPU cycle で 1 APU step)

    未実装: Triangle / Noise / DMC、サンプル出力. *)

open Famicaml_common.Nesint

type t

(** Power-up 状態の APU を作る. *)
val mk : unit -> t

(** CPU 1 cycle 進める. 内部で APU clock (= CPU 2 cycle に 1 step) と
    frame counter を駆動し、IRQ 発火条件を更新する. *)
val tick_cpu : t -> unit

(** $4000-$4017 から read. 実装外のレジスタは open bus (= 0) を返す. *)
val cpu_read : t -> uint16 -> uint8

(** $4000-$4017 への write. *)
val cpu_write : t -> uint16 -> uint8 -> unit

(** Frame counter (もしくは DMC) からの保留中 IRQ が立っているか.
    CPU は毎 cycle これを見て、I フラグが 0 なら IRQ シーケンスを開始する. *)
val irq_pending : t -> bool

(** {1 Audio output (Phase D1B)}

    APU は per-CPU-cycle で実機の non-linear mixer 出力を計算し、
    target sample rate に累積平均でダウンサンプル. 出力レンジは [0.0, 1.0]
    付近 (非線形なので厳密に [0, 1] ではない). *)

(** Web Audio の AudioContext.sampleRate を渡して downsample 比率を更新する.
    呼ばない場合のデフォルトは 44100Hz. *)
val set_sample_rate : t -> float -> unit

(** Ring buffer から最大 [max_n] サンプル取り出して新規 array に格納.
    実際取り出した数は max_n か buffer 内残数の小さい方 (返り値の長さ).
    取り出した分は buffer から消費される. *)
val drain_samples : t -> int -> float array

(** {1 DMC DMA (Nes 連携)}

    DMC の memory reader は sample buffer を充填するため CPU bus 経由で
    1 byte 読み出す必要がある. 実機の CPU halt + bus read を再現するため、
    Nes 側から bus reader closure を inject して、APU は output cycle 内で
    同期的にメモリを読む. 発生した CPU stall (typically 4 cycle) は
    {!take_dmc_pending_stall} で取り出して Nes が消化する.

    プロトコル (Nes.mk):
    1. [connect_bus_reader apu (fun addr -> bus.read addr |> Uint8.to_int)]
       で bus reader を inject.

    プロトコル (Nes.tick):
    1. 通常の Cpu/Apu/Ppu tick を実行
    2. {!take_dmc_pending_stall} で stall cycle 数を取得
    3. その cycle 数分 CPU cycles に追加し、PPU と APU を同時並行進行 *)

(** Bus reader closure 型. CPU address を受けて byte (0..255) を返す. *)
type bus_reader = int -> int

(** DMC が CPU bus を読むための closure を inject する.
    呼ばないと DMA で読み出される値は 0 になる. *)
val connect_bus_reader : t -> bus_reader -> unit

(** DMC DMA で発生した CPU stall (CPU cycle 単位) を取り出す.
    取り出すと 0 にリセットされる. *)
val take_dmc_pending_stall : t -> int
