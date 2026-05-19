let header_size   = 16
let prg_bank_size = 16384  (* 16 KB *)
let chr_bank_size = 8192   (* 8 KB *)
let chr_ram_size  = chr_bank_size
let trainer_size  = 512

type error =
  | Invalid_magic
  | Unsupported_mapper of int
  | Truncated_data

let error_to_string = function
  | Invalid_magic          -> "invalid iNES magic bytes"
  | Unsupported_mapper n   -> Printf.sprintf "unsupported mapper: %d" n
  | Truncated_data         -> "file is too short"

let parse (data : bytes) : (Cartridge.t, error) result =
  if Bytes.length data < header_size then Error Truncated_data else

  (* マジック確認: "NES\x1A" *)
  if Bytes.get_uint8 data 0 <> 0x4E   (* N *)
  || Bytes.get_uint8 data 1 <> 0x45   (* E *)
  || Bytes.get_uint8 data 2 <> 0x53   (* S *)
  || Bytes.get_uint8 data 3 <> 0x1A   (* ^Z *)
  then Error Invalid_magic else

  let prg_banks = Bytes.get_uint8 data 4 in
  let chr_banks = Bytes.get_uint8 data 5 in
  let flags6    = Bytes.get_uint8 data 6 in
  let flags7    = Bytes.get_uint8 data 7 in

  let mirroring   = if flags6 land 0x01 <> 0 then Cartridge.V else Cartridge.H in
  let has_battery = flags6 land 0x02 <> 0 in
  let has_trainer = flags6 land 0x04 <> 0 in

  (* NES 2.0 は flags7[3:2] = 0b10。その場合も mapper 下位/上位ニブルの扱いは同じ *)
  let mapper = (flags7 land 0xF0) lor (flags6 lsr 4) in
  if mapper <> 0 then Error (Unsupported_mapper mapper) else

  let data_offset = header_size + (if has_trainer then trainer_size else 0) in
  let prg_size    = prg_banks * prg_bank_size in
  let chr_size    = chr_banks * chr_bank_size in
  if Bytes.length data < data_offset + prg_size + chr_size then Error Truncated_data else

  let prg = Bytes.sub data data_offset prg_size in
  let chr =
    if chr_banks = 0
    then Bytes.create chr_ram_size   (* CHR-RAM: ゲームが書き込む *)
    else Bytes.sub data (data_offset + prg_size) chr_size
  in
  Ok { Cartridge.spec = { mirroring; has_battery; has_trainer }
     ; Cartridge.rom  = Cartridge.NROM { prg; chr }
     }
