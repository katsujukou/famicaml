open Famicaml_common.Nesint

(* open Famicaml_common.Nesint *)

(*
|PPUCTRL |$2000	| VPHB SINN	| W	NMI enable (V), PPU master/slave (P), sprite height (H), background tile select (B), sprite tile select (S), increment mode (I), nametable select / X and Y scroll bit 8 (NN)
|PPUMASK	$2001	BGRs bMmG	W	color emphasis (BGR), sprite enable (s), background enable (b), sprite left column enable (M), background left column enable (m), greyscale (G)
PPUSTATUS	$2002	VSO- ----	R	vblank (V), sprite 0 hit (S), sprite overflow (O); read resets write pair for $2005/$2006
OAMADDR	$2003	AAAA AAAA	W	OAM read/write address
OAMDATA	$2004	DDDD DDDD	RW	OAM data read/write
PPUSCROLL	$2005	XXXX XXXX YYYY YYYY	Wx2	X and Y scroll bits 7-0 (two writes: X scroll, then Y scroll)
PPUADDR	$2006	..AA AAAA AAAA AAAA	Wx2	VRAM address (two writes: most significant byte, then least significant byte)
PPUDATA	$2007	DDDD DDDD	RW	VRAM data read/write
OAMDMA	$4014	AAAA AAAA	W	OAM DMA high address
*)

module Ppu_internal : sig
  type t =
    { mutable v : uint16
    ; mutable t : uint16
    ; mutable x : uint8
    ; mutable w : bool
    }

  (** Power-up 状態。$2005/$2006 の write toggle は cleared、
      v/t/x は 0。 *)
  val initial : unit -> t
end

module Ppu_control : sig
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

  (** Power-up 状態 (PPUCTRL = $00)。NesDev "PPU power up state" 参照。
      呼び出しごとに新しいインスタンスを返す。 *)
  val initial : unit -> t
end

module Ppu_mask : sig
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

  (** Power-up 状態 (PPUMASK = $00)。レンダリング無効・色強調なし。 *)
  val initial : unit -> t
end

module Ppu_status : sig
  type t =
    { vblank_flag : bool
    ; sprite_0_hit : bool
    ; sprite_overflow : bool
    }

  val to_uint8 : t -> uint8
  val of_uint8 : uint8 -> t

  (** Power-up 状態 (PPUSTATUS = $00)。
      実機では bit 7 (V) と bit 5 (O) はしばしば 1 だが保証なし
      (NesDev wiki の表記 "+0+x xxxx" 参照)。エミュレータでは安全側に
      倒してすべて 0 として返す。 *)
  val initial : unit -> t
end
