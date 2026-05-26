open Famicaml_common.Nesint
module M = Emulator.Mapper.Mmc3
module Cart = Emulator.Rom.Cartridge

(* ------------------------------------------------------------------ *)
(* ヘルパー                                                             *)
(* ------------------------------------------------------------------ *)

let make_mapper ?(prg_kb = 128) ?(chr_kb = 64) ~chr_is_ram () =
  let prg = Bytes.create (prg_kb * 1024) in
  let chr = Bytes.create (chr_kb * 1024) in
  let mirror_ref = ref Cart.H in
  let m =
    M.create ~prg ~chr ~chr_is_ram ~set_mirroring:(fun mir -> mirror_ref := mir)
  in
  (prg, chr, mirror_ref, m)

(* ------------------------------------------------------------------ *)
(* PRG bank mode 0 (default): R6 @ $8000, R7 @ $A000, fixed C/E         *)
(* ------------------------------------------------------------------ *)

let test_prg_mode_0_layout () =
  let prg, _, _, m = make_mapper ~prg_kb:128 ~chr_is_ram:false () in
  (* 16 個の 8KB bank に sentinel *)
  for i = 0 to 15 do
    Bytes.set_uint8 prg (i * 0x2000) (0x10 + i)
  done;
  (* bank select R6 ($8000 even) = 6 *)
  M.cpu_write m 0x8000 6;
  M.cpu_write m 0x8001 3;
  (* R6 = bank 3 *)
  (* bank select R7 = 7 *)
  M.cpu_write m 0x8000 7;
  M.cpu_write m 0x8001 5;
  (* R7 = bank 5 *)
  Alcotest.(check int) "$8000 = R6 (bank 3)" 0x13 (M.cpu_read m 0x8000);
  Alcotest.(check int) "$A000 = R7 (bank 5)" 0x15 (M.cpu_read m 0xA000);
  Alcotest.(check int) "$C000 = 2nd-last (bank 14)" 0x1E (M.cpu_read m 0xC000);
  Alcotest.(check int) "$E000 = last (bank 15)" 0x1F (M.cpu_read m 0xE000)

let test_prg_mode_1_swap () =
  let prg, _, _, m = make_mapper ~prg_kb:128 ~chr_is_ram:false () in
  for i = 0 to 15 do
    Bytes.set_uint8 prg (i * 0x2000) (0x20 + i)
  done;
  (* bank select R6 = 6、PRG mode = 1 (bit 6 set) *)
  M.cpu_write m 0x8000 (0x40 lor 6);
  M.cpu_write m 0x8001 3;
  (* mode 1: $8000 = 2nd-last (14), $A000 = R7 (default 0), $C000 = R6 (3), $E000 = last (15) *)
  Alcotest.(check int) "$8000 = 2nd-last" 0x2E (M.cpu_read m 0x8000);
  Alcotest.(check int) "$C000 = R6 (3)" 0x23 (M.cpu_read m 0xC000);
  Alcotest.(check int) "$E000 = last" 0x2F (M.cpu_read m 0xE000)

(* ------------------------------------------------------------------ *)
(* CHR bank                                                             *)
(* ------------------------------------------------------------------ *)

let test_chr_default_layout () =
  let _, chr, _, m = make_mapper ~chr_kb:64 ~chr_is_ram:false () in
  (* 64 個の 1KB bank に sentinel *)
  for i = 0 to 63 do
    Bytes.set_uint8 chr (i * 0x400) (0x30 + i)
  done;
  (* R0 = 2K bank index 4 (= 1KB banks 4 & 5) *)
  M.cpu_write m 0x8000 0;
  M.cpu_write m 0x8001 4;
  (* R2 = 1KB bank 10 *)
  M.cpu_write m 0x8000 2;
  M.cpu_write m 0x8001 10;
  (* inv = 0 のとき: PPU $0000-$03FF = R0 前半 = bank 4 *)
  Alcotest.(check int)
    "$0000 = bank 4"
    0x34
    (Uint8.to_int (M.chr_read m 0x0000));
  (* $0400 = R0 後半 = bank 5 *)
  Alcotest.(check int)
    "$0400 = bank 5"
    0x35
    (Uint8.to_int (M.chr_read m 0x0400));
  (* $1000 = R2 = bank 10 *)
  Alcotest.(check int)
    "$1000 = bank 10"
    (0x30 + 10)
    (Uint8.to_int (M.chr_read m 0x1000))

let test_chr_inversion () =
  let _, chr, _, m = make_mapper ~chr_kb:64 ~chr_is_ram:false () in
  for i = 0 to 63 do
    Bytes.set_uint8 chr (i * 0x400) (0x40 + i)
  done;
  (* R0 = 4, R2 = 10. inv = 1 のとき: $0000 = R2 (1K), $1000 = R0 (2K 前半) *)
  M.cpu_write m 0x8000 0;
  M.cpu_write m 0x8001 4;
  M.cpu_write m 0x8000 2;
  M.cpu_write m 0x8001 10;
  (* inv 切替 *)
  M.cpu_write m 0x8000 (0x80 lor 0);
  M.cpu_write m 0x8001 4;
  (* R0 = 4 を再書き込み *)
  Alcotest.(check int)
    "$0000 = R2 (bank 10) under inv"
    (0x40 + 10)
    (Uint8.to_int (M.chr_read m 0x0000));
  Alcotest.(check int)
    "$1000 = R0 前半 (bank 4) under inv"
    (0x40 + 4)
    (Uint8.to_int (M.chr_read m 0x1000))

(* ------------------------------------------------------------------ *)
(* Mirroring                                                            *)
(* ------------------------------------------------------------------ *)

let test_mirroring_dynamic () =
  let _, _, mref, m = make_mapper ~chr_is_ram:false () in
  M.cpu_write m 0xA000 0;
  (* bit 0 = 0 → V *)
  Alcotest.(check bool) "V mirror" true (!mref = Cart.V);
  M.cpu_write m 0xA000 1;
  (* H *)
  Alcotest.(check bool) "H mirror" true (!mref = Cart.H)

(* ------------------------------------------------------------------ *)
(* PRG RAM                                                              *)
(* ------------------------------------------------------------------ *)

let test_prg_ram () =
  let _, _, _, m = make_mapper ~chr_is_ram:false () in
  M.cpu_write m 0x6000 0x77;
  Alcotest.(check int) "round trip" 0x77 (M.cpu_read m 0x6000);
  (* write protect *)
  M.cpu_write m 0xA001 (0x80 lor 0x40);
  (* enable + protect *)
  M.cpu_write m 0x6000 0xAA;
  Alcotest.(check int) "write protected" 0x77 (M.cpu_read m 0x6000);
  (* disable *)
  M.cpu_write m 0xA001 0x00;
  Alcotest.(check int) "disabled returns 0" 0 (M.cpu_read m 0x6000)

(* ------------------------------------------------------------------ *)
(* IRQ                                                                  *)
(* ------------------------------------------------------------------ *)

let test_irq_basic () =
  let _, _, _, m = make_mapper ~chr_is_ram:false () in
  M.cpu_write m 0xC000 3;
  (* latch = 3 *)
  M.cpu_write m 0xC001 0;
  (* reload flag set *)
  M.cpu_write m 0xE001 0;
  (* enable *)
  M.on_a12_rise m;
  (* counter = latch (3), reload clear *)
  Alcotest.(check bool) "no IRQ yet" false (M.irq_pending m);
  M.on_a12_rise m;
  (* 3 → 2 *)
  Alcotest.(check bool) "no IRQ" false (M.irq_pending m);
  M.on_a12_rise m;
  (* 2 → 1 *)
  M.on_a12_rise m;
  (* 1 → 0, IRQ trigger *)
  Alcotest.(check bool) "IRQ pending" true (M.irq_pending m)

let test_irq_disable () =
  let _, _, _, m = make_mapper ~chr_is_ram:false () in
  M.cpu_write m 0xC000 1;
  M.cpu_write m 0xC001 0;
  M.cpu_write m 0xE001 0;
  (* enable *)
  M.on_a12_rise m;
  (* reload to 1 *)
  M.on_a12_rise m;
  (* 1 → 0, IRQ *)
  Alcotest.(check bool) "IRQ" true (M.irq_pending m);
  (* $E000 で clear + disable *)
  M.cpu_write m 0xE000 0;
  Alcotest.(check bool) "cleared" false (M.irq_pending m)

let test_irq_reload () =
  let _, _, _, m = make_mapper ~chr_is_ram:false () in
  M.cpu_write m 0xC000 5;
  M.cpu_write m 0xC001 0;
  M.cpu_write m 0xE001 0;
  for _ = 1 to 6 do
    M.on_a12_rise m
  done;
  Alcotest.(check bool) "IRQ after 6 rises" true (M.irq_pending m);
  M.cpu_write m 0xE000 0;
  (* clear + disable *)
  M.cpu_write m 0xE001 0;
  (* re-enable *)
  (* counter は 0 のまま. 次の rise で latch (5) で reload *)
  M.on_a12_rise m;
  (* counter = 5 (reload, counter==0 で trigger) *)
  Alcotest.(check bool) "no IRQ right after reload" false (M.irq_pending m);
  for _ = 1 to 5 do
    M.on_a12_rise m
  done;
  Alcotest.(check bool) "IRQ after 5 more" true (M.irq_pending m)

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "MMC3"
    [ ( "PRG bank"
      , [ Alcotest.test_case "mode 0 layout" `Quick test_prg_mode_0_layout
        ; Alcotest.test_case "mode 1 (PRG swap)" `Quick test_prg_mode_1_swap
        ] )
    ; ( "CHR bank"
      , [ Alcotest.test_case
            "default layout (inv=0)"
            `Quick
            test_chr_default_layout
        ; Alcotest.test_case "A12 inversion (inv=1)" `Quick test_chr_inversion
        ] )
    ; ( "Mirroring"
      , [ Alcotest.test_case "$A000 で H/V 切替" `Quick test_mirroring_dynamic ] )
    ; ( "PRG RAM"
      , [ Alcotest.test_case
            "round trip / protect / disable"
            `Quick
            test_prg_ram
        ] )
    ; ( "Scanline IRQ"
      , [ Alcotest.test_case "counter decrement → IRQ" `Quick test_irq_basic
        ; Alcotest.test_case "$E000 で disable + clear" `Quick test_irq_disable
        ; Alcotest.test_case "$C001 で reload" `Quick test_irq_reload
        ] )
    ]
