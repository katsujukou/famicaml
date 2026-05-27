(** MMC3 (iNES mapper #4).

    {1 Memory map}

    - $6000-$7FFF: 8KB PRG RAM (enable/protect bit あり)
    - $8000-$FFFF: 4 つの 8KB PRG bank
        - mode 0: R6 / R7 / 2nd-last / last
        - mode 1: 2nd-last / R7 / R6 / last
    - PPU $0000-$1FFF: CHR. A12 inversion ($8000 bit 7) で配置切替
        - inv=0: R0(2K) / R1(2K) / R2(1K) / R3(1K) / R4(1K) / R5(1K)
        - inv=1: R2(1K) / R3(1K) / R4(1K) / R5(1K) / R0(2K) / R1(2K)

    {1 Registers}

    - $8000 (even): bank select. bit 7 = CHR A12 inv, bit 6 = PRG mode, bit 2-0 = R 番号
    - $8001 (odd):  bank data. 直近の bank select で選ばれた R に値を書く
    - $A000 (even): mirroring. bit 0 = 0 → V, 1 → H
    - $A001 (odd):  PRG RAM protect. bit 7 = enable, bit 6 = write protect
    - $C000 (even): IRQ latch
    - $C001 (odd):  IRQ reload (= 次の A12 立ち上がりで counter = latch)
    - $E000 (even): IRQ disable (+ pending IRQ flag clear)
    - $E001 (odd):  IRQ enable

    {1 Scanline IRQ}

    PPU rendering 中の A12 立ち上がり (= sprite pattern fetch のタイミング) で
    内部 counter を decrement. 0 になり enable=true なら IRQ assert. *)

type t

val create
  :  prg:Bytes.t
  -> chr:Bytes.t
  -> chr_is_ram:bool
  -> set_mirroring:(Rom.Cartridge.mirror -> unit)
  -> t

val cpu_read : t -> int -> int
val cpu_write : t -> int -> int -> unit
val chr_read : t -> int -> Famicaml_common.Nesint.uint8
val chr_write : t -> int -> Famicaml_common.Nesint.uint8 -> unit

(** PPU が rendering 中の A12 立ち上がりで呼ぶ. counter decrement + IRQ trigger. *)
val on_a12_rise : t -> unit

(** IRQ pin が low (= active) かどうか. level-triggered. *)
val irq_pending : t -> bool

(** Soft reset. NESdev / Mesen 準拠: no-op. *)
val reset : t -> unit

(** 8KB PRG-RAM ($6000-$7FFF) への直接参照. battery-backed SRAM の load/save に. *)
val prg_ram : t -> Bytes.t

val serialize : Buffer.t -> t -> unit
val deserialize : Bytes.t -> int ref -> t -> unit
