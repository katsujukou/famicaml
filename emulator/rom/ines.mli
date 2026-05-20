type error =
  | Invalid_magic (** 先頭4バイトが "NES\x1A" でない *)
  | Unsupported_mapper of int (** 対応していないマッパー番号 *)
  | Truncated_data (** データが必要サイズに満たない *)

val error_to_string : error -> string

(** iNES 形式のバイト列をパースして Cartridge.t を返す。 対応マッパー:
    - 0: NROM (PRG 固定、CHR 固定 or CHR-RAM)
    - 2: UNROM (PRG バンク切替、CHR-RAM)
    - 3: CNROM (PRG 固定、CHR バンク切替) *)
val parse : bytes -> (Cartridge.t, error) result
