type mirror =
  | H
  | V
  | One_screen_lo
  | One_screen_hi

type cart_spec =
  { mirroring : mirror
  ; has_battery : bool
  ; has_trainer : bool
  }

type rom_spec =
  | NROM of
      { prg : Bytes.t
      ; chr : Bytes.t
      } (** PRG: 16KB or 32KB 固定。CHR: 8KB 固定 ROM or RAM。 *)
  | UNROM of
      { prg : Bytes.t
      ; chr_ram : Bytes.t
      } (** PRG: 16KB×N バンク (下位切替可・上位最終バンク固定)。CHR: 8KB RAM。 *)
  | CNROM of
      { prg : Bytes.t
      ; chr : Bytes.t
      } (** PRG: 16KB or 32KB 固定。CHR: 8KB×N バンク切替可。 *)
  | MMC1 of
      { prg : Bytes.t
      ; chr : Bytes.t
      ; chr_is_ram : bool
      }
  (** Mapper 1: shift register 経由で PRG/CHR バンク切替 + 動的 mirroring。
            CHR は ROM または RAM (chr_is_ram で区別)。 *)

type t =
  { spec : cart_spec
  ; rom : rom_spec
  }

(** カートリッジの CHR バイト列を取り出す。
    NROM/CNROM では CHR-ROM、UNROM/MMC1 では CHR (ROM or RAM)。
    どのマッパーでも長さは 8KB 以上が保証される (iNES パーサが担保)。 *)
val chr_bytes : t -> Bytes.t
