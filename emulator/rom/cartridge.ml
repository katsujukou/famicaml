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
      } (** Mapper 1: shift register 経由で PRG/CHR バンク切替 + 動的 mirroring。 *)
  | MMC3 of
      { prg : Bytes.t
      ; chr : Bytes.t
      ; chr_is_ram : bool
      }
  (** Mapper 4: 8KB PRG bank × 2 + fixed last/2nd-last, 1/2KB CHR bank,
            動的 H/V mirroring, scanline IRQ. *)

type t =
  { spec : cart_spec
  ; rom : rom_spec
  }

let chr_bytes cart =
  match cart.rom with
  | NROM { chr; _ } -> chr
  | UNROM { chr_ram; _ } -> chr_ram
  | CNROM { chr; _ } -> chr
  | MMC1 { chr; _ } -> chr
  | MMC3 { chr; _ } -> chr
