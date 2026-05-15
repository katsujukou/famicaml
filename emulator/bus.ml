open Stdint 

type t = {
  (* 2KB WRAM *)
  wram : bytes;
  (* ppu  : Ppu.t;
  apu  : Apu.t;
  cart : Cartridge.t; *)
}

let read (bus : t) (addr : uint16) : uint8 =
  let a = Uint16.to_int addr in
  match a with
  | a when a < 0x2000 ->
      (* WRAM with mirroring *)
      Uint8.of_int (Bytes.get_uint8 bus.wram (a land 0x07FF))
  (* | a when a < 0x4000 ->
      (* PPU registers, mirrored every 8 bytes *)
      Ppu.read_register bus.ppu (a land 0x0007)
  | a when a < 0x4018 ->
      Apu.read_register bus.apu a
  | a ->
      Cartridge.read bus.cart a *)
  | _ -> raise Exn.Not_implemented

let write (bus : t) (addr : uint16) (value : uint8) : unit =
  let a = Uint16.to_int addr in
  match a with
  | a when a < 0x2000 ->
      Bytes.set_uint8 bus.wram (a land 0x07FF) (Uint8.to_int value)
  (* | a when a < 0x4000 ->
      Ppu.write_register bus.ppu (a land 0x0007) value
  | 0x4014 ->
      (* OAM DMA: 256バイト転送のトリガ *)
      Ppu.oam_dma bus.ppu bus value
  | a when a < 0x4018 ->
      Apu.write_register bus.apu a value
  | a ->
      Cartridge.write bus.cart a value *)
  | _ -> raise Exn.Not_implemented