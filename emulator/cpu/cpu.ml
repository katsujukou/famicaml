open Stdint

(** Processor Status register *)

module type PS = sig 
  type t = private uint8
  type flag = N | V | R | B | D| I | Z | C

  val set_flags : (flag * bool) list -> t -> t
  val get_flag: flag -> t -> bool
end

module Processor_status : PS = struct
  type t = uint8  
  type flag = N | V | R | B | D| I | Z | C

  let u8_0xFF = Uint8.of_int 0b1111_1111

  let bit_of_flag = function
    | N -> Uint8.of_int 0b1000_0000
    | V -> Uint8.of_int 0b0100_0000
    | R -> Uint8.of_int 0b0010_0000
    | B -> Uint8.of_int 0b0001_0000
    | D -> Uint8.of_int 0b0000_1000
    | I -> Uint8.of_int 0b0000_0100
    | Z -> Uint8.of_int 0b0000_0010
    | C -> Uint8.of_int 0b0000_0001

  let get_flag f v = Uint8.logand (bit_of_flag f) v != Uint8.zero

  let set_flag f b v = 
      if b then Uint8.logor (bit_of_flag f) v
      else Uint8.logand (Uint8.logxor u8_0xFF (bit_of_flag f)) v

  let set_flags flags p = 
    List.fold_left (fun v (f, b) -> set_flag f b v) p flags
end

type t = {
  (* program counter *)
  mutable reg_pc : uint16;

  (* general purpose registers *)
  mutable reg_a : uint8;
  mutable reg_x : uint8;
  mutable reg_y : uint8;

  (* processor status register *)
  mutable reg_p : Processor_status.t;

  (* stack pointer *)
  mutable reg_sp : uint8;
}
