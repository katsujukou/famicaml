
type mirror = H | V 

type cart_spec = {
  mirroring: mirror;
  has_battery: bool;
  has_trainer: bool;
}

type rom_spec = 
  | NROM of { prg: Bytes.t; chr: Bytes.t }

type t = {
  spec: cart_spec; 
  rom: rom_spec
}