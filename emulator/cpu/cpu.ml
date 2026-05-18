open Stdint

(** Processor Status register *)

module type PS = sig 
  type t = private uint8
  type flag = N | V | R | B | D| I | Z | C
  val initial : t
  val set_flags : (flag * bool) list -> t -> t
  val get_flag: flag -> t -> bool
  val of_uint8: uint8 -> t
  val to_uint8: uint8 -> t
end

module Processor_status : PS = struct
  type t = uint8  
  type flag = N | V | R | B | D| I | Z | C

  let initial = Uint8.of_int 0b0000_0100
  let of_uint8 v = v
  let to_uint8 v = v
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

  let get_flag f v = Uint8.logand (bit_of_flag f) v <> Uint8.zero

  let set_flag f b v = 
      if b then Uint8.logor (bit_of_flag f) v
      else Uint8.logand (Uint8.logxor u8_0xFF (bit_of_flag f)) v

  let set_flags flags p = 
    List.fold_left (fun v (f, b) -> set_flag f b v) p flags
end

type t = {
  (* program counter *)
  mutable reg_PC : uint16;

  (* general purpose registers *)
  mutable reg_A : uint8;
  mutable reg_X : uint8;
  mutable reg_Y : uint8;

  (* processor status register *)
  mutable reg_P : Processor_status.t;

  (* stack pointer *)
  mutable reg_SP : uint8;
}

let mk () = {
  reg_PC = Uint16.of_int 0;
  reg_A = Uint8.of_int 0;
  reg_X = Uint8.of_int 0;
  reg_Y = Uint8.of_int 00;
  reg_P = Processor_status.initial;
  reg_SP = Uint8.of_int 0xFF;
}