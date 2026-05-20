let header_size = 16
let prg_bank_size = 16384 (* 16 KB *)
let chr_bank_size = 8192 (* 8 KB *)
let chr_ram_size = chr_bank_size
let trainer_size = 512

type error =
  | Invalid_magic
  | Unsupported_mapper of int
  | Truncated_data

let error_to_string = function
  | Invalid_magic -> "invalid iNES magic bytes"
  | Unsupported_mapper n -> Printf.sprintf "unsupported mapper: %d" n
  | Truncated_data -> "file is too short"

let ( let* ) = Result.bind
let need data n = if Bytes.length data >= n then Ok () else Error Truncated_data

(* ------------------------------------------------------------------ *)
(* マッパーごとのパーサ                                                 *)
(* ------------------------------------------------------------------ *)

let parse_nrom ~ofs ~prg_banks ~chr_banks data =
  let prg_size = prg_banks * prg_bank_size in
  let chr_size = chr_banks * chr_bank_size in
  let* () = need data (ofs + prg_size + chr_size) in
  let prg = Bytes.sub data ofs prg_size in
  let chr =
    if chr_banks = 0 then Bytes.create chr_ram_size else Bytes.sub data (ofs + prg_size) chr_size
  in
  Ok (Cartridge.NROM { prg; chr })

(** UNROM (mapper #2): PRG バンク切替、CHR-RAM 固定。 iNES ヘッダの chr_banks は通常 0 だが、値に関わらず CHR-RAM を用意する。 *)
let parse_unrom ~ofs ~prg_banks data =
  let prg_size = prg_banks * prg_bank_size in
  let* () = need data (ofs + prg_size) in
  let prg = Bytes.sub data ofs prg_size in
  let chr_ram = Bytes.create chr_ram_size in
  Ok (Cartridge.UNROM { prg; chr_ram })

(** CNROM (mapper #3): PRG 固定、CHR バンク切替。 *)
let parse_cnrom ~ofs ~prg_banks ~chr_banks data =
  let prg_size = prg_banks * prg_bank_size in
  let chr_size = chr_banks * chr_bank_size in
  let* () = need data (ofs + prg_size + chr_size) in
  let prg = Bytes.sub data ofs prg_size in
  let chr =
    if chr_banks = 0 then Bytes.create chr_ram_size else Bytes.sub data (ofs + prg_size) chr_size
  in
  Ok (Cartridge.CNROM { prg; chr })

(* ------------------------------------------------------------------ *)
(* 公開エントリポイント                                                 *)
(* ------------------------------------------------------------------ *)

let parse (data : bytes) : (Cartridge.t, error) result =
  let* () = need data header_size in
  (* マジック確認: "NES\x1A" *)
  if
    Bytes.get_uint8 data 0 <> 0x4E (* N *)
    || Bytes.get_uint8 data 1 <> 0x45 (* E *)
    || Bytes.get_uint8 data 2 <> 0x53 (* S *)
    || Bytes.get_uint8 data 3 <> 0x1A (* ^Z *)
  then Error Invalid_magic
  else (
    let prg_banks = Bytes.get_uint8 data 4 in
    let chr_banks = Bytes.get_uint8 data 5 in
    let flags6 = Bytes.get_uint8 data 6 in
    let flags7 = Bytes.get_uint8 data 7 in
    let mirroring = if flags6 land 0x01 <> 0 then Cartridge.V else Cartridge.H in
    let has_battery = flags6 land 0x02 <> 0 in
    let has_trainer = flags6 land 0x04 <> 0 in
    (* NES 2.0 は flags7[3:2] = 0b10。その場合もニブルの扱いは同じ。 *)
    let mapper = flags7 land 0xF0 lor (flags6 lsr 4) in
    let ofs = header_size + if has_trainer then trainer_size else 0 in
    let* rom =
      match mapper with
      | 0 -> parse_nrom ~ofs ~prg_banks ~chr_banks data
      | 2 -> parse_unrom ~ofs ~prg_banks data
      | 3 -> parse_cnrom ~ofs ~prg_banks ~chr_banks data
      | n -> Error (Unsupported_mapper n)
    in
    Ok { Cartridge.spec = { mirroring; has_battery; has_trainer }; Cartridge.rom })
