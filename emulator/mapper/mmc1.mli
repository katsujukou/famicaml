(** MMC1 (iNES mapper #1).

    Shift register ベースの bank-switching mapper.

    {1 Memory map}

    - $6000-$7FFF: 8KB PRG RAM (battery-backable; ROM 仕様で有無)
    - $8000-$FFFF: PRG ROM. control register の PRG mode に応じて 16/32KB 単位で切替
    - PPU $0000-$1FFF: CHR ROM/RAM. control register の CHR mode に応じて 4/8KB 単位で切替

    {1 レジスタ書き込みプロトコル}

    $8000-$FFFF write は内部 5-bit shift register に LSB を 1 bit ずつ shift in する.
    5 回目の write で shift register の値が以下のいずれかの target に書かれる
    (target は最後の write address の bits 14-13 で決定):

    - $8000-$9FFF (= 0): Control
      - bit 4-3: PRG mode (0/1: 32KB swap, 2: fix first, 3: fix last)
      - bit 2:   CHR mode (0: 8KB swap, 1: 4KB×2)
      - bit 1-0: mirroring (0: one_screen_lo, 1: one_screen_hi, 2: V, 3: H)
    - $A000-$BFFF (= 1): CHR bank 0 (lo)
    - $C000-$DFFF (= 2): CHR bank 1 (hi, 8KB mode では未使用)
    - $E000-$FFFF (= 3): PRG bank
      - bit 4: PRG RAM enable (0=enabled)
      - bit 3-0: PRG bank number

    write の bit 7 = 1 なら shift register reset + control register OR'd with $0C
    (= PRG mode 3 を強制 = 起動時状態). *)

type t

(** [create ~prg ~chr ~chr_is_ram ~set_mirroring] で MMC1 state を作る.
    [set_mirroring] は control register write 時に PPU の mirroring を更新する
    callback. 作成直後に control register の初期値 ($0C) に基づく mirroring
    (= One_screen_lo) を即 apply する. *)
val create
  :  prg:Bytes.t
  -> chr:Bytes.t
  -> chr_is_ram:bool
  -> set_mirroring:(Rom.Cartridge.mirror -> unit)
  -> t

(** CPU bus $6000-$FFFF からの read. それ以外の address は 0 を返す. *)
val cpu_read : t -> int -> int

(** CPU bus への write. $6000-$7FFF は PRG RAM, $8000-$FFFF は shift register. *)
val cpu_write : t -> int -> int -> unit

(** PPU bus $0000-$1FFF (CHR) からの read. *)
val chr_read : t -> int -> Famicaml_common.Nesint.uint8

(** PPU bus への write. CHR-RAM のみ有効、CHR-ROM では無視. *)
val chr_write : t -> int -> Famicaml_common.Nesint.uint8 -> unit

(** Soft reset. NESdev / Mesen 準拠: no-op (mapper は CPU RESET 影響を受けない). *)
val reset : t -> unit

(** 8KB PRG-RAM ($6000-$7FFF) への直接参照. battery-backed SRAM の load/save に. *)
val prg_ram : t -> Bytes.t
