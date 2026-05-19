type error =
  | Invalid_magic          (** 先頭4バイトが "NES\x1A" でない *)
  | Unsupported_mapper of int  (** 対応していないマッパー番号 *)
  | Truncated_data         (** データが必要サイズに満たない *)

val error_to_string : error -> string

(** iNES 形式のバイト列をパースして Cartridge.t を返す。
    現在は Mapper 0 (NROM) のみ対応。
    CHR バンク数が 0 の場合は 8KB の CHR-RAM を用意する。 *)
val parse : bytes -> (Cartridge.t, error) result
