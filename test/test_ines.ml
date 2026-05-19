module Ines = Emulator.Rom.Ines
module Cart = Emulator.Rom.Cartridge

(* ------------------------------------------------------------------ *)
(* テスト用ヘルパー                                                     *)
(* ------------------------------------------------------------------ *)

let make_rom ?(flags6 = 0) ?(flags7 = 0) ~prg_banks ~chr_banks () =
  let header_size  = 16 in
  let trainer_size = if flags6 land 0x04 <> 0 then 512 else 0 in
  let prg_size     = prg_banks * 16384 in
  let chr_size     = chr_banks * 8192 in
  let total        = header_size + trainer_size + prg_size + chr_size in
  let data = Bytes.create total in
  Bytes.set_uint8 data 0 0x4E;
  Bytes.set_uint8 data 1 0x45;
  Bytes.set_uint8 data 2 0x53;
  Bytes.set_uint8 data 3 0x1A;
  Bytes.set_uint8 data 4 prg_banks;
  Bytes.set_uint8 data 5 chr_banks;
  Bytes.set_uint8 data 6 flags6;
  Bytes.set_uint8 data 7 flags7;
  data

let ok_or_fail = function
  | Ok c    -> c
  | Error e -> Alcotest.failf "unexpected error: %s" (Ines.error_to_string e)

(** インラインレコードのフィールドをタプルに変換する。 *)
let nrom_fields cart = match cart.Cart.rom with
  | Cart.NROM { prg; chr } -> (prg, chr)
  | _ -> Alcotest.fail "expected NROM"

let unrom_fields cart = match cart.Cart.rom with
  | Cart.UNROM { prg; chr_ram } -> (prg, chr_ram)
  | _ -> Alcotest.fail "expected UNROM"

let cnrom_fields cart = match cart.Cart.rom with
  | Cart.CNROM { prg; chr } -> (prg, chr)
  | _ -> Alcotest.fail "expected CNROM"

(* ------------------------------------------------------------------ *)
(* NROM (mapper 0)                                                      *)
(* ------------------------------------------------------------------ *)

let test_nrom_1prg_no_chr () =
  let data = make_rom ~prg_banks:1 ~chr_banks:0 () in
  let cart = ok_or_fail (Ines.parse data) in
  let (prg, chr) = nrom_fields cart in
  Alcotest.(check int) "PRG size"      16384 (Bytes.length prg);
  Alcotest.(check int) "CHR-RAM size"   8192 (Bytes.length chr)

let test_nrom_2prg_1chr () =
  let data = make_rom ~prg_banks:2 ~chr_banks:1 () in
  let cart = ok_or_fail (Ines.parse data) in
  let (prg, chr) = nrom_fields cart in
  Alcotest.(check int) "PRG size" 32768 (Bytes.length prg);
  Alcotest.(check int) "CHR size"  8192 (Bytes.length chr)

let test_mirroring_vertical () =
  let data = make_rom ~flags6:0x01 ~prg_banks:1 ~chr_banks:0 () in
  let cart = ok_or_fail (Ines.parse data) in
  Alcotest.(check bool) "mirroring V" true (cart.Cart.spec.mirroring = Cart.V)

let test_mirroring_horizontal () =
  let data = make_rom ~flags6:0x00 ~prg_banks:1 ~chr_banks:0 () in
  let cart = ok_or_fail (Ines.parse data) in
  Alcotest.(check bool) "mirroring H" true (cart.Cart.spec.mirroring = Cart.H)

let test_battery_flag () =
  let data = make_rom ~flags6:0x02 ~prg_banks:1 ~chr_banks:0 () in
  let cart = ok_or_fail (Ines.parse data) in
  Alcotest.(check bool) "has_battery" true cart.Cart.spec.has_battery

let test_trainer_present () =
  let data = make_rom ~flags6:0x04 ~prg_banks:1 ~chr_banks:0 () in
  let cart = ok_or_fail (Ines.parse data) in
  Alcotest.(check bool) "has_trainer" true cart.Cart.spec.has_trainer;
  let (prg, _) = nrom_fields cart in
  Alcotest.(check int) "PRG size" 16384 (Bytes.length prg)

let test_prg_data_preserved () =
  let data = make_rom ~prg_banks:1 ~chr_banks:0 () in
  Bytes.set_uint8 data 16 0xEA;
  Bytes.set_uint8 data 17 0xA9;
  let cart = ok_or_fail (Ines.parse data) in
  let (prg, _) = nrom_fields cart in
  Alcotest.(check int) "PRG[0]" 0xEA (Bytes.get_uint8 prg 0);
  Alcotest.(check int) "PRG[1]" 0xA9 (Bytes.get_uint8 prg 1)

(* ------------------------------------------------------------------ *)
(* UNROM (mapper 2)  flags6 = 0x20                                     *)
(* ------------------------------------------------------------------ *)

let test_unrom_basic () =
  let data = make_rom ~flags6:0x20 ~prg_banks:4 ~chr_banks:0 () in
  let cart = ok_or_fail (Ines.parse data) in
  let (prg, chr_ram) = unrom_fields cart in
  Alcotest.(check int) "PRG size"      65536 (Bytes.length prg);
  Alcotest.(check int) "CHR-RAM size"   8192 (Bytes.length chr_ram)

let test_unrom_ignores_chr_banks () =
  (* iNES ヘッダが chr_banks=1 でも UNROM は CHR-RAM を使う *)
  let data = make_rom ~flags6:0x20 ~prg_banks:4 ~chr_banks:1 () in
  let cart = ok_or_fail (Ines.parse data) in
  let (_, chr_ram) = unrom_fields cart in
  Alcotest.(check int) "CHR-RAM size" 8192 (Bytes.length chr_ram)

let test_unrom_prg_data () =
  let data = make_rom ~flags6:0x20 ~prg_banks:2 ~chr_banks:0 () in
  Bytes.set_uint8 data 16 0x4C;
  let cart = ok_or_fail (Ines.parse data) in
  let (prg, _) = unrom_fields cart in
  Alcotest.(check int) "PRG size"  32768 (Bytes.length prg);
  Alcotest.(check int) "PRG[0]"     0x4C (Bytes.get_uint8 prg 0)

(* ------------------------------------------------------------------ *)
(* CNROM (mapper 3)  flags6 = 0x30                                     *)
(* ------------------------------------------------------------------ *)

let test_cnrom_basic () =
  let data = make_rom ~flags6:0x30 ~prg_banks:2 ~chr_banks:4 () in
  let cart = ok_or_fail (Ines.parse data) in
  let (prg, chr) = cnrom_fields cart in
  Alcotest.(check int) "PRG size" 32768 (Bytes.length prg);
  Alcotest.(check int) "CHR size" 32768 (Bytes.length chr)

let test_cnrom_chr_data () =
  let data = make_rom ~flags6:0x30 ~prg_banks:1 ~chr_banks:2 () in
  let prg_end = 16 + 16384 in
  Bytes.set_uint8 data prg_end 0xAB;
  Bytes.set_uint8 data (prg_end + 8192) 0xCD;
  let cart = ok_or_fail (Ines.parse data) in
  let (_, chr) = cnrom_fields cart in
  Alcotest.(check int) "CHR size"      16384 (Bytes.length chr);
  Alcotest.(check int) "CHR bank0[0]"   0xAB (Bytes.get_uint8 chr 0);
  Alcotest.(check int) "CHR bank1[0]"   0xCD (Bytes.get_uint8 chr 8192)

(* ------------------------------------------------------------------ *)
(* エラー系                                                             *)
(* ------------------------------------------------------------------ *)

let test_error_invalid_magic () =
  let data = Bytes.of_string "NOTNES\x1A\x00\x01\x00\x00\x00\x00\x00\x00\x00" in
  match Ines.parse data with
  | Error Ines.Invalid_magic -> ()
  | _ -> Alcotest.fail "expected Invalid_magic"

let test_error_truncated_no_header () =
  let data = Bytes.create 8 in
  match Ines.parse data with
  | Error Ines.Truncated_data -> ()
  | _ -> Alcotest.fail "expected Truncated_data"

let test_error_truncated_short_prg () =
  let data = make_rom ~prg_banks:2 ~chr_banks:0 () in
  let short = Bytes.sub data 0 (Bytes.length data - 100) in
  match Ines.parse short with
  | Error Ines.Truncated_data -> ()
  | _ -> Alcotest.fail "expected Truncated_data"

let test_error_unsupported_mapper () =
  (* mapper 4 = MMC3: flags6[7:4]=4 → flags6=0x40 *)
  let data = make_rom ~flags6:0x40 ~prg_banks:1 ~chr_banks:0 () in
  match Ines.parse data with
  | Error (Ines.Unsupported_mapper 4) -> ()
  | _ -> Alcotest.fail "expected Unsupported_mapper 4"

(* ------------------------------------------------------------------ *)
(* テスト登録                                                           *)
(* ------------------------------------------------------------------ *)

let () = Alcotest.run "iNES parser" [
  "NROM (mapper 0)", [
    Alcotest.test_case "1 PRG / CHR-RAM"       `Quick test_nrom_1prg_no_chr;
    Alcotest.test_case "2 PRG / 1 CHR"         `Quick test_nrom_2prg_1chr;
    Alcotest.test_case "mirroring vertical"     `Quick test_mirroring_vertical;
    Alcotest.test_case "mirroring horizontal"   `Quick test_mirroring_horizontal;
    Alcotest.test_case "battery flag"           `Quick test_battery_flag;
    Alcotest.test_case "trainer present"        `Quick test_trainer_present;
    Alcotest.test_case "PRG data preserved"     `Quick test_prg_data_preserved;
  ];
  "UNROM (mapper 2)", [
    Alcotest.test_case "basic"                  `Quick test_unrom_basic;
    Alcotest.test_case "chr_banks 無視"         `Quick test_unrom_ignores_chr_banks;
    Alcotest.test_case "PRG data"               `Quick test_unrom_prg_data;
  ];
  "CNROM (mapper 3)", [
    Alcotest.test_case "basic"                  `Quick test_cnrom_basic;
    Alcotest.test_case "CHR data preserved"     `Quick test_cnrom_chr_data;
  ];
  "エラー系", [
    Alcotest.test_case "invalid magic"          `Quick test_error_invalid_magic;
    Alcotest.test_case "truncated (no header)"  `Quick test_error_truncated_no_header;
    Alcotest.test_case "truncated (short PRG)"  `Quick test_error_truncated_short_prg;
    Alcotest.test_case "unsupported mapper"     `Quick test_error_unsupported_mapper;
  ];
]
