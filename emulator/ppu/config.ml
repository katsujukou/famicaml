open Famicaml_common.Nesint
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

let bits_of_nmtbl_idx ni =
  Uint8.of_int
    (match ni with
     | Nmtbl_2000 -> 0b0000_0000
     | Nmtbl_2400 -> 0b0000_0001
     | Nmtbl_2800 -> 0b0000_0010
     | Nmtbl_2C00 -> 0b0000_0011)

let unsafe_nmtbl_idx_of_int = function
  | 0b0000_0000 -> Nmtbl_2000
  | 0b0000_0001 -> Nmtbl_2400
  | 0b0000_0010 -> Nmtbl_2800
  | 0b0000_0011 -> Nmtbl_2C00
  | _ -> raise Exn.Out_of_range

module type PPU_CONFIG = sig
  val get_nmtbl : unit -> nmtbl_idx
  val set_nmtbl : nmtbl_idx -> unit
  val set_vram_addr_incr : vram_addr_incr -> unit
end

let mk ~(port : Bus.t) =
  let mk_port reg =
    ((fun () -> port.read reg), fun f -> port.write reg (f @@ port.read reg))
  in
  let read_2000, update_2000 = mk_port IO.ppu_reg_2000 in
  (module struct
    let get_nmtbl () =
      unsafe_nmtbl_idx_of_int
      @@ Int.logand 0b0000_0011
      @@ Uint8.to_int
      @@ read_2000 ()

    let set_nmtbl ni = update_2000 (Uint8.logor (bits_of_nmtbl_idx ni))
    let set_vram_addr_incr incr = update_2000 (Uint8.set_bit 2 (incr = By_32))
  end : PPU_CONFIG)
