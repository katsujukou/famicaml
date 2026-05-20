type uint8  = private int
type uint16 = private int
type int8   = private int

module Uint8 : sig
  type t = uint8

  val zero    : t
  val one     : t
  val max_int : t                       (* 0xFF *)

  val of_int : int -> t                 (* 0xFF mask *)
  val to_int : t -> int

  val to_signed : t -> uint8

  (* 算術: すべて 8-bit でラップアラウンド *)
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
  val succ : t -> t
  val pred : t -> t

  val logand : t -> t -> t
  val logor  : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val shift_left          : t -> int -> t
  val shift_right_logical : t -> int -> t
  val shift_right         : t -> int -> t

  val equal   : t -> t -> bool
  val compare : t -> t -> int

  val bit      : int -> t -> int
  val test_bit : int -> t -> bool

  (** デバッグ表示用 (例: "0xA9")。 *)
  val pp : Format.formatter -> t -> unit

  val ( +  ) : t -> t -> t
  val ( -  ) : t -> t -> t
  val ( *  ) : t -> t -> t
  val ( =  ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( <  ) : t -> t -> bool
  val ( >  ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
end

module Uint16 : sig
  type t = uint16

  val zero    : t
  val one     : t
  val max_int : t                       (** 0xFFFF *)

  val of_int   : int -> t
  val to_int   : t -> int

  val of_uint8 : uint8 -> t

  (** 上位/下位バイト。 *)
  val hi : t -> uint8
  val lo : t -> uint8

  (** lo/hi の 2 バイトから 16-bit を組み立てる (6502 はリトルエンディアン)。 *)
  val of_bytes : lo:uint8 -> hi:uint8 -> t

  (* 算術: 16-bit でラップアラウンド *)
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val succ : t -> t
  val pred : t -> t

  (** [int8] のオフセットを加算する (相対分岐の PC 計算)。 *)
  val add_signed : t -> int8 -> t

  val logand : t -> t -> t
  val logor  : t -> t -> t
  val logxor : t -> t -> t

  val shift_left          : t -> int -> t
  val shift_right_logical : t -> int -> t
  val shift_right         : t -> int -> t

  val equal   : t -> t -> bool
  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit

  val ( +  ) : t -> t -> t
  val ( -  ) : t -> t -> t
  val ( =  ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( <  ) : t -> t -> bool
  val ( >  ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
end

module Int8 : sig
  type t = int8
  val of_uint8 : uint8 -> t   
  val to_int   : t -> int
end