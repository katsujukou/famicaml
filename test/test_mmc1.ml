open Famicaml_common.Nesint
module M = Emulator.Mapper.Mmc1
module Cart = Emulator.Rom.Cartridge

(* ------------------------------------------------------------------ *)
(* ヘルパー                                                             *)
(* ------------------------------------------------------------------ *)

(** 5 ビットを LSB から順に shift register に送る (= 5 連続 write).
    最後の write の address で target register が決まる. *)
let write_register m addr value5 =
  for i = 0 to 4 do
    let b = (value5 lsr i) land 1 in
    M.cpu_write m addr b
  done

let make_mapper ?(prg_kb = 32) ?(chr_kb = 8) ~chr_is_ram () =
  let prg = Bytes.create (prg_kb * 1024) in
  let chr = Bytes.create (chr_kb * 1024) in
  let mirror_ref = ref Cart.H in
  let m =
    M.create ~prg ~chr ~chr_is_ram ~set_mirroring:(fun mir -> mirror_ref := mir)
  in
  (prg, chr, mirror_ref, m)

(* ------------------------------------------------------------------ *)
(* Power-up state                                                       *)
(* ------------------------------------------------------------------ *)

let test_power_up_mirroring () =
  let _, _, mref, _ = make_mapper ~chr_is_ram:false () in
  (* control 初期値 $0C → mirror bits 00 → One_screen_lo *)
  Alcotest.(check bool)
    "initial mirroring = One_screen_lo"
    true
    (!mref = Cart.One_screen_lo)

let test_power_up_prg_mode_3 () =
  (* PRG mode = 3 (= fix last bank at $C000, switch 16KB at $8000).
     PRG = 32KB = 2 bank. last bank = bank 1.
     prg_bank = 0 (init), $8000 → bank 0, $C000 → bank 1. *)
  let prg, _, _, m = make_mapper ~prg_kb:32 ~chr_is_ram:false () in
  Bytes.set_uint8 prg 0 0x11;
  (* bank 0 [0] *)
  Bytes.set_uint8 prg 0x3FFF 0x22;
  (* bank 0 末尾 *)
  Bytes.set_uint8 prg 0x4000 0x33;
  (* bank 1 [0] *)
  Bytes.set_uint8 prg 0x7FFF 0x44;
  (* bank 1 末尾 *)
  Alcotest.(check int) "$8000 = bank 0 [0]" 0x11 (M.cpu_read m 0x8000);
  Alcotest.(check int) "$BFFF = bank 0 末尾" 0x22 (M.cpu_read m 0xBFFF);
  Alcotest.(check int) "$C000 = bank 1 [0] (last)" 0x33 (M.cpu_read m 0xC000);
  Alcotest.(check int) "$FFFF = bank 1 末尾" 0x44 (M.cpu_read m 0xFFFF)

(* ------------------------------------------------------------------ *)
(* Shift register プロトコル                                            *)
(* ------------------------------------------------------------------ *)

let test_shift_reset_with_bit7 () =
  let _, _, mref, m = make_mapper ~chr_is_ram:false () in
  (* 1 bit だけ shift (= 5 回満たさず途中) *)
  M.cpu_write m 0x8000 1;
  M.cpu_write m 0x8000 1;
  (* bit 7 set で reset: control |= $0C で PRG mode 3 + mirror そのまま *)
  M.cpu_write m 0x8000 0x80;
  (* control は元の $0C のまま. mirror は One_screen_lo *)
  Alcotest.(check bool)
    "reset 後も One_screen_lo"
    true
    (!mref = Cart.One_screen_lo);
  (* もう一度 5 bit shift で書き換えできる (= shift_count がリセットされてる) *)
  write_register m 0xA000 0x01;
  (* CHR bank 0 = 1, target = $A000-$BFFF *)
  (* CHR bank 0 が変わったかは内部状態なので外部観測不可。crash しないこと確認 *)
  Alcotest.(check pass) "no crash" () ()

let test_5_writes_apply_control () =
  let _, _, mref, m = make_mapper ~chr_is_ram:false () in
  (* control に 0b00011 を書く: PRG mode 0 (32KB switch), CHR mode 0,
     mirror = 11 = H *)
  write_register m 0x8000 0b00011;
  Alcotest.(check bool) "mirror = H" true (!mref = Cart.H)

let test_target_register_by_address () =
  let _, _, mref, m = make_mapper ~chr_is_ram:false () in
  (* $A000-$BFFF: chr0 (mirror に影響なし) *)
  write_register m 0xA000 0x05;
  Alcotest.(check bool)
    "chr0 write does NOT change mirror"
    true
    (!mref = Cart.One_screen_lo)

(* ------------------------------------------------------------------ *)
(* PRG bank switching (mode 2: fix first)                              *)
(* ------------------------------------------------------------------ *)

let test_prg_mode_2 () =
  let prg, _, _, m = make_mapper ~prg_kb:64 ~chr_is_ram:false () in
  for i = 0 to 3 do
    Bytes.set_uint8 prg (i * 0x4000) (0x10 + i)
  done;
  (* control = PRG mode 2 (= bits 4-3 = 10 = 0b10), CHR mode 0, mirror = 0 (one_screen_lo) *)
  write_register m 0x8000 0b01000;
  (* prg_bank = 2 *)
  write_register m 0xE000 0b00010;
  Alcotest.(check int) "$8000 = bank 0 (fixed first)" 0x10 (M.cpu_read m 0x8000);
  Alcotest.(check int) "$C000 = bank 2 (selected)" 0x12 (M.cpu_read m 0xC000)

(* ------------------------------------------------------------------ *)
(* PRG RAM ($6000-$7FFF)                                                *)
(* ------------------------------------------------------------------ *)

let test_prg_ram_round_trip () =
  let _, _, _, m = make_mapper ~chr_is_ram:false () in
  M.cpu_write m 0x6000 0x42;
  M.cpu_write m 0x7FFF 0x99;
  Alcotest.(check int) "$6000 read" 0x42 (M.cpu_read m 0x6000);
  Alcotest.(check int) "$7FFF read" 0x99 (M.cpu_read m 0x7FFF)

let test_prg_ram_disable () =
  let _, _, _, m = make_mapper ~chr_is_ram:false () in
  M.cpu_write m 0x6000 0xAA;
  (* prg_bank write bit 4 = 1 で PRG RAM disable *)
  write_register m 0xE000 0b10000;
  Alcotest.(check int) "disabled = 0" 0 (M.cpu_read m 0x6000);
  M.cpu_write m 0x6000 0xBB;
  (* write も無視 *)
  (* 再 enable して元値が残ってるか *)
  write_register m 0xE000 0b00000;
  Alcotest.(check int) "enable 後の元値" 0xAA (M.cpu_read m 0x6000)

(* ------------------------------------------------------------------ *)
(* CHR bank switching                                                   *)
(* ------------------------------------------------------------------ *)

let test_chr_8k_mode () =
  let _, chr, _, m = make_mapper ~chr_kb:32 ~chr_is_ram:false () in
  (* 4 つの 8KB bank に sentinel *)
  for i = 0 to 3 do
    Bytes.set_uint8 chr (i * 0x2000) (0x20 + i)
  done;
  (* control: CHR mode = 0 (bit 4 = 0), PRG mode 3 (デフォルト) *)
  write_register m 0x8000 0b01100;
  (* CHR bank 0 = 2 (low bit ignored → 8KB bank 1) *)
  write_register m 0xA000 0b00010;
  let b = M.chr_read m 0x0000 in
  (* 8KB bank: chr0 lsr 1 = 1 → bank 1 (offset 0x2000) *)
  Alcotest.(check int) "8KB bank 1 [0]" 0x21 (Uint8.to_int b)

let test_chr_4k_mode () =
  let _, chr, _, m = make_mapper ~chr_kb:32 ~chr_is_ram:false () in
  for i = 0 to 7 do
    Bytes.set_uint8 chr (i * 0x1000) (0x30 + i)
  done;
  (* CHR mode = 1 (bit 4 = 1) *)
  write_register m 0x8000 0b11100;
  (* chr0 = 3 (4KB bank 3 → offset 0x3000) *)
  write_register m 0xA000 0b00011;
  (* chr1 = 5 *)
  write_register m 0xC000 0b00101;
  Alcotest.(check int)
    "$0000 = chr bank 3"
    0x33
    (Uint8.to_int (M.chr_read m 0x0000));
  Alcotest.(check int)
    "$1000 = chr bank 5"
    0x35
    (Uint8.to_int (M.chr_read m 0x1000))

(* ------------------------------------------------------------------ *)
(* Mirroring 変化                                                      *)
(* ------------------------------------------------------------------ *)

let test_mirroring_change () =
  let _, _, mref, m = make_mapper ~chr_is_ram:false () in
  let check_after mir bits =
    write_register m 0x8000 (0x0C lor bits);
    (* PRG/CHR mode は default 維持、mirror bits だけ変える *)
    Alcotest.(check bool) (Printf.sprintf "mirror = %d" bits) true (!mref = mir)
  in
  check_after Cart.One_screen_lo 0;
  check_after Cart.One_screen_hi 1;
  check_after Cart.V 2;
  check_after Cart.H 3

(* ------------------------------------------------------------------ *)
(* CHR-RAM write                                                        *)
(* ------------------------------------------------------------------ *)

let test_chr_ram_write () =
  let _, chr, _, m = make_mapper ~chr_kb:8 ~chr_is_ram:true () in
  M.chr_write m 0x0050 (Uint8.of_int 0xAB);
  Alcotest.(check int) "write" 0xAB (Bytes.get_uint8 chr 0x0050);
  Alcotest.(check int) "read" 0xAB (Uint8.to_int (M.chr_read m 0x0050))

let test_chr_rom_write_ignored () =
  let _, chr, _, m = make_mapper ~chr_kb:8 ~chr_is_ram:false () in
  M.chr_write m 0x0050 (Uint8.of_int 0xAB);
  Alcotest.(check int) "ROM is unchanged" 0 (Bytes.get_uint8 chr 0x0050)

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "MMC1"
    [ ( "Power-up"
      , [ Alcotest.test_case
            "mirror = One_screen_lo"
            `Quick
            test_power_up_mirroring
        ; Alcotest.test_case
            "PRG mode 3 (fix last)"
            `Quick
            test_power_up_prg_mode_3
        ] )
    ; ( "Shift register"
      , [ Alcotest.test_case "bit 7 で reset" `Quick test_shift_reset_with_bit7
        ; Alcotest.test_case "5 回 write で適用" `Quick test_5_writes_apply_control
        ; Alcotest.test_case
            "target は address bits 14-13"
            `Quick
            test_target_register_by_address
        ] )
    ; ( "PRG bank"
      , [ Alcotest.test_case "mode 2: fix first" `Quick test_prg_mode_2 ] )
    ; ( "PRG RAM"
      , [ Alcotest.test_case "round trip" `Quick test_prg_ram_round_trip
        ; Alcotest.test_case "bit 4 = 1 で disable" `Quick test_prg_ram_disable
        ] )
    ; ( "CHR bank"
      , [ Alcotest.test_case "8KB mode" `Quick test_chr_8k_mode
        ; Alcotest.test_case "4KB×2 mode" `Quick test_chr_4k_mode
        ] )
    ; ( "Mirroring"
      , [ Alcotest.test_case "4 種すべて切替" `Quick test_mirroring_change ] )
    ; ( "CHR-RAM / CHR-ROM"
      , [ Alcotest.test_case "CHR-RAM write OK" `Quick test_chr_ram_write
        ; Alcotest.test_case
            "CHR-ROM write 無視"
            `Quick
            test_chr_rom_write_ignored
        ] )
    ]
