module IO = Io_port

type nmtbl_idx =
  | Nmtbl_2000
  | Nmtbl_2400
  | Nmtbl_2800
  | Nmtbl_2C00

type vram_addr_incr =
  | By_01
  | By_32

type pattern_alignment =
  | L
  | R

val bits_of_nmtbl_idx : nmtbl_idx -> Bus.byte

module type PPU_CONFIG = sig
  val get_nmtbl : unit -> nmtbl_idx
  val set_nmtbl : nmtbl_idx -> unit
  val set_vram_addr_incr : vram_addr_incr -> unit
end

val mk : port:Bus.t -> (module PPU_CONFIG)
