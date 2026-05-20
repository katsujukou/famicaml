open Famicaml_common.Nesint 

type addr = uint16 

type byte = Memory.byte

(** Interface to shared main memory. Some part of it is mapped to other hardware component
    including PPU/APU register and GamePad interface. *)
type t = { 
  read: addr -> byte;
  write: addr -> byte -> unit;
}

let mk ~read ~write = { read; write; }
