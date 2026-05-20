open Famicaml_common.Nesint

module Cart = Emulator.Rom.Cartridge
module Nes  = Emulator.Nes
module Exn  = Emulator.Exn

(* ------------------------------------------------------------------ *)
(* テスト用ヘルパー                                                     *)
(* ------------------------------------------------------------------ *)

let default_spec : Cart.cart_spec =
  { mirroring = H; has_battery = false; has_trainer = false }

let make_nrom_cart ?(spec = default_spec) ~prg_kb () =
  let prg = Bytes.create (prg_kb * 1024) in
  let chr = Bytes.create 8192 in
  prg, { Cart.spec; rom = NROM { prg; chr } }

let make_unrom_cart ?(spec = default_spec) ~prg_kb () =
  let prg     = Bytes.create (prg_kb * 1024) in
  let chr_ram = Bytes.create 8192 in
  prg, { Cart.spec; rom = UNROM { prg; chr_ram } }

(** PRG 末尾 6 バイトに NMI/RESET/IRQ ベクタを埋める。 *)
let set_vectors prg ~nmi ~reset ~irq =
  let n = Bytes.length prg in
  Bytes.set_uint8 prg (n - 6) (nmi land 0xFF);
  Bytes.set_uint8 prg (n - 5) ((nmi lsr 8) land 0xFF);
  Bytes.set_uint8 prg (n - 4) (reset land 0xFF);
  Bytes.set_uint8 prg (n - 3) ((reset lsr 8) land 0xFF);
  Bytes.set_uint8 prg (n - 2) (irq land 0xFF);
  Bytes.set_uint8 prg (n - 1) ((irq lsr 8) land 0xFF)

(** Cartridge を装填済みの NES を返すヘルパ。 *)
let nes_with cart =
  let nes = Nes.mk () in
  Nes.connect_cartridge nes cart;
  nes

let bus_read_u8 nes addr =
  Uint8.to_int (nes.Nes.memory_bus.read (Uint16.of_int addr))

let bus_write_u8 nes addr v =
  nes.Nes.memory_bus.write (Uint16.of_int addr) (Uint8.of_int v)

(* ------------------------------------------------------------------ *)
(* mk: 空の NES (カセットなし・電源 off)                                *)
(* ------------------------------------------------------------------ *)

let test_mk_empty () =
  let nes = Nes.mk () in
  Alcotest.(check bool) "power off"            false (nes.power);
  Alcotest.(check bool) "cart none"            true  (nes.cart = None);
  (* WRAM はアクセス可能 *)
  bus_write_u8 nes 0x0010 0x55;
  Alcotest.(check int)  "WRAM read"            0x55  (bus_read_u8 nes 0x0010);
  (* カセットなし時の $8000 は open bus = 0 *)
  Alcotest.(check int)  "$8000 open bus"       0     (bus_read_u8 nes 0x8000)

(* ------------------------------------------------------------------ *)
(* PRG-ROM マッピング (NROM)                                            *)
(* ------------------------------------------------------------------ *)

let test_nrom128_mirrors () =
  let prg, cart = make_nrom_cart ~prg_kb:16 () in
  Bytes.set_uint8 prg 0       0xAA;
  Bytes.set_uint8 prg 0x3FFF  0xBB;
  let nes = nes_with cart in
  Alcotest.(check int) "$8000"  0xAA (bus_read_u8 nes 0x8000);
  Alcotest.(check int) "$BFFF"  0xBB (bus_read_u8 nes 0xBFFF);
  Alcotest.(check int) "$C000"  0xAA (bus_read_u8 nes 0xC000);
  Alcotest.(check int) "$FFFF"  0xBB (bus_read_u8 nes 0xFFFF)

let test_nrom256_no_mirror () =
  let prg, cart = make_nrom_cart ~prg_kb:32 () in
  Bytes.set_uint8 prg 0       0x11;
  Bytes.set_uint8 prg 0x4000  0x22;
  let nes = nes_with cart in
  Alcotest.(check int) "$8000" 0x11 (bus_read_u8 nes 0x8000);
  Alcotest.(check int) "$C000" 0x22 (bus_read_u8 nes 0xC000)

(* ------------------------------------------------------------------ *)
(* PRG-ROM マッピング (UNROM)                                           *)
(* ------------------------------------------------------------------ *)

let test_unrom_last_bank_fixed () =
  let prg, cart = make_unrom_cart ~prg_kb:64 () in
  Bytes.set_uint8 prg 0          0x01;
  Bytes.set_uint8 prg (3*0x4000) 0x04;
  let nes = nes_with cart in
  Alcotest.(check int) "$8000=bank0" 0x01 (bus_read_u8 nes 0x8000);
  Alcotest.(check int) "$C000=last"  0x04 (bus_read_u8 nes 0xC000)

let test_unrom_bank_switch () =
  let prg, cart = make_unrom_cart ~prg_kb:64 () in
  Bytes.set_uint8 prg 0          0x01;
  Bytes.set_uint8 prg (1*0x4000) 0x02;
  Bytes.set_uint8 prg (2*0x4000) 0x03;
  let nes = nes_with cart in
  bus_write_u8 nes 0x8000 2;
  Alcotest.(check int) "$8000=bank2" 0x03 (bus_read_u8 nes 0x8000);
  bus_write_u8 nes 0x8000 1;
  Alcotest.(check int) "$8000=bank1" 0x02 (bus_read_u8 nes 0x8000)

(* ------------------------------------------------------------------ *)
(* 接続時のベクタ読み込みと RESET                                        *)
(* ------------------------------------------------------------------ *)

let test_vectors_on_connect () =
  let prg, cart = make_nrom_cart ~prg_kb:16 () in
  set_vectors prg ~nmi:0x8010 ~reset:0xC034 ~irq:0xABCD;
  let nes = nes_with cart in
  Alcotest.(check int) "NMI vec"   0x8010 (Uint16.to_int nes.ith_nmi);
  Alcotest.(check int) "RESET vec" 0xC034 (Uint16.to_int nes.ith_reset);
  Alcotest.(check int) "IRQ vec"   0xABCD (Uint16.to_int nes.ith_irq);
  (* connect では PC は触らない (実機相当) *)
  Alcotest.(check int) "PC untouched" 0 (Uint16.to_int nes.cpu.reg_PC);
  Nes.reset nes;
  Alcotest.(check int) "PC after reset" 0xC034 (Uint16.to_int nes.cpu.reg_PC)

let test_power_on_resets_pc () =
  let prg, cart = make_nrom_cart ~prg_kb:16 () in
  set_vectors prg ~nmi:0 ~reset:0x8123 ~irq:0;
  let nes = nes_with cart in
  Alcotest.(check bool) "off before" false nes.power;
  Nes.power_on nes;
  Alcotest.(check bool) "on after"   true  nes.power;
  Alcotest.(check int)  "PC = reset" 0x8123 (Uint16.to_int nes.cpu.reg_PC)

(* ------------------------------------------------------------------ *)
(* WRAM とマップ外                                                      *)
(* ------------------------------------------------------------------ *)

let test_wram_roundtrip () =
  let _, cart = make_nrom_cart ~prg_kb:16 () in
  let nes = nes_with cart in
  bus_write_u8 nes 0x0123 0x77;
  Alcotest.(check int) "wram readback"   0x77 (bus_read_u8 nes 0x0123);
  Alcotest.(check int) "wram mirror $0923" 0x77 (bus_read_u8 nes 0x0923)

let test_unmapped_raises () =
  let _, cart = make_nrom_cart ~prg_kb:16 () in
  let nes = nes_with cart in
  Alcotest.check_raises "$2000 read" Exn.Out_of_range
    (fun () -> ignore (bus_read_u8 nes 0x2000));
  Alcotest.check_raises "$5000 write" Exn.Out_of_range
    (fun () -> bus_write_u8 nes 0x5000 0)

(* ------------------------------------------------------------------ *)
(* eject / connect サイクル — テニス+SMB の 256W グリッチ的シナリオ      *)
(* http://smb.lsi.harisen.jp/howtoplay.html                            *)
(* ------------------------------------------------------------------ *)

let test_eject_preserves_wram () =
  let _, cart = make_nrom_cart ~prg_kb:16 () in
  let nes = nes_with cart in
  bus_write_u8 nes 0x0042 0x99;
  Nes.eject nes;
  Alcotest.(check bool) "cart removed" true (nes.cart = None);
  (* WRAM は引き続き読める *)
  Alcotest.(check int)  "WRAM 残存"    0x99 (bus_read_u8 nes 0x0042);
  (* カセットなしなので $8000 は open bus *)
  Alcotest.(check int)  "$8000 = 0"    0    (bus_read_u8 nes 0x8000)

let test_swap_carts_preserves_wram () =
  (* カートリッジ A (テニス相当) *)
  let prg_a, cart_a = make_nrom_cart ~prg_kb:16 () in
  set_vectors prg_a ~nmi:0 ~reset:0x8000 ~irq:0;
  (* カートリッジ B (SMB 相当) *)
  let prg_b, cart_b = make_nrom_cart ~prg_kb:16 () in
  set_vectors prg_b ~nmi:0 ~reset:0xC123 ~irq:0;

  let nes = Nes.mk () in
  Nes.connect_cartridge nes cart_a;
  Nes.power_on nes;
  Alcotest.(check int) "A reset vec" 0x8000 (Uint16.to_int nes.cpu.reg_PC);

  (* ゲーム動作中に WRAM へ書き込む (テニスでスコアを操作する想定) *)
  bus_write_u8 nes 0x0700 0xDE;
  bus_write_u8 nes 0x0701 0xAD;
  bus_write_u8 nes 0x0702 0xBE;
  bus_write_u8 nes 0x0703 0xEF;

  (* 電源を入れたままカートリッジ A をイジェクト *)
  Nes.eject nes;
  Alcotest.(check int) "WRAM after eject [0]" 0xDE (bus_read_u8 nes 0x0700);

  (* カートリッジ B を挿入 *)
  Nes.connect_cartridge nes cart_b;
  Alcotest.(check int) "B reset vec stored" 0xC123 (Uint16.to_int nes.ith_reset);

  (* リセットボタン *)
  Nes.reset nes;
  Alcotest.(check int) "PC at B reset" 0xC123 (Uint16.to_int nes.cpu.reg_PC);

  (* 肝心の点: WRAM はカートリッジ抜き差し+リセットを越えても保持される *)
  Alcotest.(check int) "WRAM [0] survived" 0xDE (bus_read_u8 nes 0x0700);
  Alcotest.(check int) "WRAM [1] survived" 0xAD (bus_read_u8 nes 0x0701);
  Alcotest.(check int) "WRAM [2] survived" 0xBE (bus_read_u8 nes 0x0702);
  Alcotest.(check int) "WRAM [3] survived" 0xEF (bus_read_u8 nes 0x0703)

(* ------------------------------------------------------------------ *)
(* connect (bytes 経由)                                                 *)
(* ------------------------------------------------------------------ *)

let test_connect_from_bytes () =
  (* 16 バイトヘッダ + 16KB PRG + 0 CHR の NROM iNES バイト列 *)
  let total = 16 + 16384 in
  let buf = Bytes.create total in
  Bytes.set_uint8 buf 0 0x4E;
  Bytes.set_uint8 buf 1 0x45;
  Bytes.set_uint8 buf 2 0x53;
  Bytes.set_uint8 buf 3 0x1A;
  Bytes.set_uint8 buf 4 1;     (* PRG banks *)
  Bytes.set_uint8 buf 5 0;     (* CHR banks *)
  (* PRG 末尾 6 バイトにベクタ。PRG オフセット = 16 + 16384 - 6 *)
  let v_ofs = 16 + 16384 - 6 in
  Bytes.set_uint8 buf (v_ofs + 2) 0x42;  (* RESET lo *)
  Bytes.set_uint8 buf (v_ofs + 3) 0x80;  (* RESET hi -> $8042 *)
  let nes = Nes.mk () in
  (match Nes.connect nes buf with
   | Ok () -> ()
   | Error e -> Alcotest.failf "connect error: %s" (Emulator.Rom.Ines.error_to_string e));
  Alcotest.(check bool) "cart attached" true (nes.cart <> None);
  Alcotest.(check int)  "RESET vec"     0x8042 (Uint16.to_int nes.ith_reset)

let test_connect_invalid_magic () =
  let buf = Bytes.create 32 in
  let nes = Nes.mk () in
  match Nes.connect nes buf with
  | Error Emulator.Rom.Ines.Invalid_magic ->
    Alcotest.(check bool) "still no cart" true (nes.cart = None)
  | _ -> Alcotest.fail "expected Invalid_magic"

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () = Alcotest.run "Nes" [
  "初期状態", [
    Alcotest.test_case "mk: 空の NES"            `Quick test_mk_empty;
  ];
  "PRG mapping (NROM)", [
    Alcotest.test_case "NROM-128 mirror"         `Quick test_nrom128_mirrors;
    Alcotest.test_case "NROM-256 no mirror"      `Quick test_nrom256_no_mirror;
  ];
  "PRG mapping (UNROM)", [
    Alcotest.test_case "last bank fixed"         `Quick test_unrom_last_bank_fixed;
    Alcotest.test_case "bank switch"             `Quick test_unrom_bank_switch;
  ];
  "ベクタ / RESET", [
    Alcotest.test_case "connect でベクタロード"   `Quick test_vectors_on_connect;
    Alcotest.test_case "power_on は reset を呼ぶ" `Quick test_power_on_resets_pc;
  ];
  "WRAM / 未マップ領域", [
    Alcotest.test_case "WRAM round trip"         `Quick test_wram_roundtrip;
    Alcotest.test_case "$2000/$5000 raises"      `Quick test_unmapped_raises;
  ];
  "eject / cart 差し替え", [
    Alcotest.test_case "eject は WRAM を保持"     `Quick test_eject_preserves_wram;
    Alcotest.test_case "256W: A→eject→B→reset 後も WRAM 残存"
                                                 `Quick test_swap_carts_preserves_wram;
  ];
  "Ines.parse 経由の connect", [
    Alcotest.test_case "正常 iNES"               `Quick test_connect_from_bytes;
    Alcotest.test_case "invalid magic"           `Quick test_connect_invalid_magic;
  ];
]
