open Famicaml_common.Nesint

module type PPUINTERNAL = sig
  type t =
    { mutable v : uint16
    ; mutable t : uint16
    ; mutable x : uint8
    ; mutable w : bool
    }

  val initial : unit -> t
end

module Ppu_internal : PPUINTERNAL = struct
  type t =
    { mutable v : uint16
    ; mutable t : uint16
    ; mutable x : uint8
    ; mutable w : bool
    }

  (* PPU 内部レジスタ power-up: v/t/x = 0, w = false (write toggle cleared). *)
  let initial () =
    { v = Uint16.zero; t = Uint16.zero; x = Uint8.zero; w = false }
end

module type PPUCTRL = sig
  type sprsize =
    | Spr_8x8
    | Spr_8x16

  type pattern_alignment =
    | L
    | R

  type vram_addr_incr =
    | VAI_01
    | VAI_32

  type t =
    { enable_nmi : bool
    ; ppu_master_slave : bool (* true: slave mode (unusual!) *)
    ; sprite_size : sprsize
    ; spr_alignment : pattern_alignment
    ; bg_alignment : pattern_alignment
    ; addr_incr : vram_addr_incr
    ; base_nmtbl : Nametable.nmtbl_idx
    }

  val of_uint8 : uint8 -> t
  val to_uint8 : t -> uint8
  val initial : unit -> t
end

module Ppu_control : PPUCTRL = struct
  type sprsize =
    | Spr_8x8
    | Spr_8x16

  type pattern_alignment =
    | L
    | R

  type vram_addr_incr =
    | VAI_01
    | VAI_32

  type t =
    { enable_nmi : bool
    ; ppu_master_slave : bool (* true: slave mode (unusual!) *)
    ; sprite_size : sprsize
    ; spr_alignment : pattern_alignment
    ; bg_alignment : pattern_alignment
    ; addr_incr : vram_addr_incr
    ; base_nmtbl : Nametable.nmtbl_idx
    }

  let of_uint8 x =
    Uint8.
      { enable_nmi = test_bit 7 x
      ; ppu_master_slave = test_bit 6 x
      ; sprite_size = (if test_bit 5 x then Spr_8x16 else Spr_8x8)
      ; bg_alignment = (if test_bit 4 x then R else L)
      ; spr_alignment = (if test_bit 3 x then R else L)
      ; addr_incr = (if test_bit 2 x then VAI_32 else VAI_01)
      ; base_nmtbl =
          Nametable.(
            match Int.logand 3 (to_int x) with
            | 0 -> Nmtbl_2000
            | 1 -> Nmtbl_2400
            | 2 -> Nmtbl_2800
            | 3 -> Nmtbl_2C00
            | _ -> raise Exn.Impossible)
      }

  let to_uint8 s =
    Uint8.(
      let v = if s.enable_nmi then u8_80 else zero in
      let p = if s.ppu_master_slave then u8_40 else zero in
      let h =
        match s.sprite_size with
        | Spr_8x8 -> zero
        | Spr_8x16 -> u8_20
      in
      let ba =
        match s.bg_alignment with
        | R -> u8_10
        | L -> zero
      in
      let sa =
        match s.spr_alignment with
        | R -> u8_08
        | L -> zero
      in
      let i =
        match s.addr_incr with
        | VAI_01 -> zero
        | VAI_32 -> u8_04
      in
      let nn =
        Nametable.(
          match s.base_nmtbl with
          | Nmtbl_2000 -> u8_00
          | Nmtbl_2400 -> u8_01
          | Nmtbl_2800 -> u8_02
          | Nmtbl_2C00 -> logor u8_02 u8_01)
      in
      logor v (logor p (logor h (logor ba (logor sa (logor i nn))))))

  (* PPUCTRL power-up = $00 (NesDev "PPU power up state").
     of_uint8 経由で組み立てることで、ビット意味のずれが起きない. *)
  let initial () = of_uint8 Uint8.zero
end

module type PPUMASK = sig
  type color_emphasis =
    { red : bool
    ; green : bool
    ; blue : bool
    }

  type t =
    { color_emphasis : color_emphasis
    ; enable_sprite : bool
    ; enable_spr_left_column : bool
    ; enable_bg : bool
    ; enable_bg_left_column : bool
    ; gray_scale : bool
    }

  val to_uint8 : t -> uint8
  val of_uint8 : uint8 -> t
  val initial : unit -> t
end

module Ppu_mask : PPUMASK = struct
  type color_emphasis =
    { red : bool
    ; green : bool
    ; blue : bool
    }

  type t =
    { color_emphasis : color_emphasis
    ; enable_sprite : bool
    ; enable_spr_left_column : bool
    ; enable_bg : bool
    ; enable_bg_left_column : bool
    ; gray_scale : bool
    }

  let of_uint8 x =
    Uint8.
      { color_emphasis =
          { red = test_bit 5 x; green = test_bit 6 x; blue = test_bit 7 x }
      ; enable_sprite = test_bit 4 x
      ; enable_bg = test_bit 3 x
      ; enable_spr_left_column = test_bit 2 x
      ; enable_bg_left_column = test_bit 1 x
      ; gray_scale = test_bit 0 x
      }

  let to_uint8 v =
    Uint8.(
      logor (if v.color_emphasis.blue then u8_80 else zero)
      @@ logor (if v.color_emphasis.green then u8_40 else zero)
      @@ logor (if v.color_emphasis.red then u8_20 else zero)
      @@ logor (if v.enable_sprite then u8_10 else zero)
      @@ logor (if v.enable_bg then u8_08 else zero)
      @@ logor (if v.enable_spr_left_column then u8_04 else zero)
      @@ logor (if v.enable_bg_left_column then u8_02 else zero)
      @@ if v.gray_scale then u8_01 else zero)

  (* PPUMASK power-up = $00。レンダリング無効・色強調なし。 *)
  let initial () = of_uint8 Uint8.zero
end

module type PPUSTATUS = sig
  type t =
    { vblank_flag : bool
    ; sprite_0_hit : bool
    ; sprite_overflow : bool
    }

  val to_uint8 : t -> uint8
  val of_uint8 : uint8 -> t
  val initial : unit -> t
end

module Ppu_status : PPUSTATUS = struct
  type t =
    { vblank_flag : bool
    ; sprite_0_hit : bool
    ; sprite_overflow : bool
    }

  let to_uint8 v =
    Uint8.(
      logor (if v.vblank_flag then u8_80 else u8_00)
      @@ logor (if v.sprite_0_hit then u8_40 else u8_00)
      @@ if v.sprite_overflow then u8_20 else u8_00)

  let of_uint8 x =
    Uint8.
      { vblank_flag = test_bit 7 x
      ; sprite_0_hit = test_bit 6 x
      ; sprite_overflow = test_bit 5 x
      }

  (* PPUSTATUS power-up = $00。実機では V (bit 7) と O (bit 5) は
     しばしば 1 だが保証なし (NesDev "PPU power up state": "+0+x xxxx")。
     エミュレータでは安全側に倒して 0 で初期化する。 *)
  let initial () = of_uint8 Uint8.zero
end
