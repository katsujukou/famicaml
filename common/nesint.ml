type uint8 = int
type uint16 = int
type int8 = int

let m8 = 0xFF
let m16 = 0xFFFF

module Uint8 = struct
  type t = uint8

  let zero = 0
  let one = 1
  let max_int = 0xFF
  let of_int x = x land m8
  let to_int x = x
  let u8_00 = 0x00
  let u8_01 = 0x01
  let u8_02 = 0x02
  let u8_04 = 0x04
  let u8_08 = 0x08
  let u8_10 = 0x10
  let u8_20 = 0x20
  let u8_40 = 0x40
  let u8_80 = 0x80
  let to_signed (x : t) : int8 = if x >= 0x80 then x - 0x100 else x
  let add a b = (a + b) land m8
  let sub a b = (a - b) land m8
  let mul a b = a * b land m8
  let succ x = (x + 1) land m8
  let pred x = (x - 1) land m8
  let logand = ( land )
  let logor = ( lor )
  let logxor = ( lxor )
  let lognot x = lnot x land m8
  let shift_left a n = (a lsl n) land m8
  let shift_right_logical a n = a lsr n
  let shift_right = shift_right_logical
  let equal (a : t) (b : t) = a = b
  let compare (a : t) (b : t) = Stdlib.compare a b
  let bit n x = (x lsr n) land 1
  let test_bit n x = bit n x = 1

  let set_bit n b x =
    if b then logor (one lsl n) x else logand x (lognot (one lsl n))

  let pp ppf x = Format.fprintf ppf "0x%02X" x
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( = ) = equal
  let ( <> ) a b = not (equal a b)
  let ( < ) (a : t) (b : t) = Stdlib.( < ) a b
  let ( > ) (a : t) (b : t) = Stdlib.( > ) a b
  let ( <= ) (a : t) (b : t) = Stdlib.( <= ) a b
  let ( >= ) (a : t) (b : t) = Stdlib.( >= ) a b
end

module Uint16 = struct
  type t = uint16

  let zero = 0
  let one = 1
  let max_int = 0xFFFF
  let of_int x = x land m16
  let to_int x = x
  let of_uint8 (x : uint8) : t = x (* 0..255 は uint16 にそのまま入る *)
  let hi w = (w lsr 8) land m8
  let lo w = w land m8

  let of_bytes ~(lo : uint8) ~(hi : uint8) : t =
    ((hi land m8) lsl 8) lor (lo land m8)

  let add a b = (a + b) land m16
  let sub a b = (a - b) land m16
  let succ x = (x + 1) land m16
  let pred x = (x - 1) land m16
  let add_signed (w : t) (off : int8) : t = (w + off) land m16
  let logand = ( land )
  let logor = ( lor )
  let logxor = ( lxor )
  let lognot x = lnot x land m8
  let shift_left a n = (a lsl n) land m16
  let shift_right_logical a n = a lsr n
  let shift_right = shift_right_logical
  let equal (a : t) (b : t) = a = b
  let compare (a : t) (b : t) = Stdlib.compare a b

  let set_bit n b x =
    if b then logor (one lsl n) x else logand x (lognot (one lsl n))

  let pp ppf x = Format.fprintf ppf "0x%04X" x
  let ( + ) = add
  let ( - ) = sub
  let ( = ) = equal
  let ( <> ) a b = not (equal a b)
  let ( < ) (a : t) (b : t) = Stdlib.( < ) a b
  let ( > ) (a : t) (b : t) = Stdlib.( > ) a b
  let ( <= ) (a : t) (b : t) = Stdlib.( <= ) a b
  let ( >= ) (a : t) (b : t) = Stdlib.( >= ) a b
end

module Int8 = struct
  type t = int8

  let of_uint8 (x : uint8) : t = if x >= 0x80 then x - 0x100 else x
  let to_int (x : t) = x
end
