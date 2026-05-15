open Bytes

type mirror = H | V 

type cart_spec = {
  mirroring: mirror;
  has_battery: bool;
  has_trainer: bool;
}

type rom = 
  | NROM of { prg: Bytes.t; chr: Bytes.t }

type t = cart_spec * rom
