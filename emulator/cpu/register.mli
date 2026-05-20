open Famicaml_common.Nesint

module type PS = sig
  type t = private uint8
  type flag = N | V | R | B | D | I | Z | C

  val initial   : t
  val set_flag  : flag -> bool -> t -> t
  val set_flags : (flag * bool) list -> t -> t
  val get_flag  : flag -> t -> bool
  val of_uint8  : uint8 -> t
  val to_uint8  : t -> uint8
end

module Processor_status : PS

(** 6502 レジスタセット。フィールドはすべてミュータブル。 *)
type t = {
  mutable reg_PC : uint16;
  mutable reg_A  : uint8;
  mutable reg_X  : uint8;
  mutable reg_Y  : uint8;
  mutable reg_P  : Processor_status.t;
  mutable reg_SP : uint8;
}
