open Famicaml_common.Nesint
module Cart = Emulator.Rom.Cartridge
module Nes = Emulator.Nes
module Cpu = Emulator.Cpu
module Exn = Emulator.Exn

(* ------------------------------------------------------------------ *)
(* テスト用ヘルパー                                                     *)
(* ------------------------------------------------------------------ *)

let default_spec : Cart.cart_spec =
  { mirroring = H; has_battery = false; has_trainer = false }

let make_nrom_cart ?(spec = default_spec) ~prg_kb () =
  let prg = Bytes.create (prg_kb * 1024) in
  let chr = Bytes.create 8192 in
  (prg, { Cart.spec; rom = NROM { prg; chr } })

let make_unrom_cart ?(spec = default_spec) ~prg_kb () =
  let prg = Bytes.create (prg_kb * 1024) in
  let chr_ram = Bytes.create 8192 in
  (prg, { Cart.spec; rom = UNROM { prg; chr_ram } })

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
  Alcotest.(check bool) "power off" false nes.power;
  Alcotest.(check bool) "cart none" true (nes.cart = None);
  (* WRAM はアクセス可能 *)
  bus_write_u8 nes 0x0010 0x55;
  Alcotest.(check int) "WRAM read" 0x55 (bus_read_u8 nes 0x0010);
  (* カセットなし時の $8000 は open bus = 0 *)
  Alcotest.(check int) "$8000 open bus" 0 (bus_read_u8 nes 0x8000)

(* ------------------------------------------------------------------ *)
(* PRG-ROM マッピング (NROM)                                            *)
(* ------------------------------------------------------------------ *)

let test_nrom128_mirrors () =
  let prg, cart = make_nrom_cart ~prg_kb:16 () in
  Bytes.set_uint8 prg 0 0xAA;
  Bytes.set_uint8 prg 0x3FFF 0xBB;
  let nes = nes_with cart in
  Alcotest.(check int) "$8000" 0xAA (bus_read_u8 nes 0x8000);
  Alcotest.(check int) "$BFFF" 0xBB (bus_read_u8 nes 0xBFFF);
  Alcotest.(check int) "$C000" 0xAA (bus_read_u8 nes 0xC000);
  Alcotest.(check int) "$FFFF" 0xBB (bus_read_u8 nes 0xFFFF)

let test_nrom256_no_mirror () =
  let prg, cart = make_nrom_cart ~prg_kb:32 () in
  Bytes.set_uint8 prg 0 0x11;
  Bytes.set_uint8 prg 0x4000 0x22;
  let nes = nes_with cart in
  Alcotest.(check int) "$8000" 0x11 (bus_read_u8 nes 0x8000);
  Alcotest.(check int) "$C000" 0x22 (bus_read_u8 nes 0xC000)

(* ------------------------------------------------------------------ *)
(* PRG-ROM マッピング (UNROM)                                           *)
(* ------------------------------------------------------------------ *)

let test_unrom_last_bank_fixed () =
  let prg, cart = make_unrom_cart ~prg_kb:64 () in
  Bytes.set_uint8 prg 0 0x01;
  Bytes.set_uint8 prg (3 * 0x4000) 0x04;
  let nes = nes_with cart in
  Alcotest.(check int) "$8000=bank0" 0x01 (bus_read_u8 nes 0x8000);
  Alcotest.(check int) "$C000=last" 0x04 (bus_read_u8 nes 0xC000)

let test_unrom_bank_switch () =
  let prg, cart = make_unrom_cart ~prg_kb:64 () in
  Bytes.set_uint8 prg 0 0x01;
  Bytes.set_uint8 prg (1 * 0x4000) 0x02;
  Bytes.set_uint8 prg (2 * 0x4000) 0x03;
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
  Alcotest.(check int) "NMI vec" 0x8010 (Uint16.to_int nes.ith_nmi);
  Alcotest.(check int) "RESET vec" 0xC034 (Uint16.to_int nes.ith_reset);
  Alcotest.(check int) "IRQ vec" 0xABCD (Uint16.to_int nes.ith_irq);
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
  Alcotest.(check bool) "on after" true nes.power;
  Alcotest.(check int) "PC = reset" 0x8123 (Uint16.to_int nes.cpu.reg_PC)

(* ------------------------------------------------------------------ *)
(* WRAM とマップ外                                                      *)
(* ------------------------------------------------------------------ *)

let test_wram_roundtrip () =
  let _, cart = make_nrom_cart ~prg_kb:16 () in
  let nes = nes_with cart in
  bus_write_u8 nes 0x0123 0x77;
  Alcotest.(check int) "wram readback" 0x77 (bus_read_u8 nes 0x0123);
  Alcotest.(check int) "wram mirror $0923" 0x77 (bus_read_u8 nes 0x0923)

let test_unmapped_returns_zero () =
  let _, cart = make_nrom_cart ~prg_kb:16 () in
  let nes = nes_with cart in
  (* $4000-$401F (APU/IO) や $4020-$7FFF (expansion / SRAM) は
     現段階では未実装で no-op スタブ: read = 0, write = 捨て.
     例外を投げると ROM の reset routine ($4017 等への書き込み) で死ぬので
     とりあえず通す方針. *)
  Alcotest.(check int) "$4017 read = 0" 0 (bus_read_u8 nes 0x4017);
  Alcotest.(check int) "$4020 read = 0" 0 (bus_read_u8 nes 0x4020);
  Alcotest.(check int) "$6000 read = 0" 0 (bus_read_u8 nes 0x6000);
  (* write は raise しない *)
  bus_write_u8 nes 0x4017 0xFF;
  bus_write_u8 nes 0x5000 0xAA;
  bus_write_u8 nes 0x6000 0xBB

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
  Alcotest.(check int) "WRAM 残存" 0x99 (bus_read_u8 nes 0x0042);
  (* カセットなしなので $8000 は open bus *)
  Alcotest.(check int) "$8000 = 0" 0 (bus_read_u8 nes 0x8000)

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
  Bytes.set_uint8 buf 4 1;
  (* PRG banks *)
  Bytes.set_uint8 buf 5 0;
  (* CHR banks *)
  (* PRG 末尾 6 バイトにベクタ。PRG オフセット = 16 + 16384 - 6 *)
  let v_ofs = 16 + 16384 - 6 in
  Bytes.set_uint8 buf (v_ofs + 2) 0x42;
  (* RESET lo *)
  Bytes.set_uint8 buf (v_ofs + 3) 0x80;
  (* RESET hi -> $8042 *)
  let nes = Nes.mk () in
  (match Nes.connect nes buf with
   | Ok () -> ()
   | Error e ->
     Alcotest.failf "connect error: %s" (Emulator.Rom.Ines.error_to_string e));
  Alcotest.(check bool) "cart attached" true (nes.cart <> None);
  Alcotest.(check int) "RESET vec" 0x8042 (Uint16.to_int nes.ith_reset)

let test_connect_invalid_magic () =
  let buf = Bytes.create 32 in
  let nes = Nes.mk () in
  match Nes.connect nes buf with
  | Error Emulator.Rom.Ines.Invalid_magic ->
    Alcotest.(check bool) "still no cart" true (nes.cart = None)
  | _ -> Alcotest.fail "expected Invalid_magic"

(* ------------------------------------------------------------------ *)
(* CPU↔PPU lockstep (Phase A6)                                         *)
(* ------------------------------------------------------------------ *)

(* PRG に NOP を埋め尽くした 16KB を作る。reset vector も埋めておく. *)
let nrom_with_nop_loop ~reset_vec =
  let prg, cart = make_nrom_cart ~prg_kb:16 () in
  Bytes.fill prg 0 (Bytes.length prg) '\xEA' (* NOP *);
  set_vectors prg ~nmi:0 ~reset:reset_vec ~irq:0;
  cart

let test_tick_advances_cpu_1_ppu_3 () =
  let cart = nrom_with_nop_loop ~reset_vec:0x8000 in
  let nes = Nes.mk () in
  Nes.connect_cartridge nes cart;
  Nes.power_on nes;
  let cpu_start = nes.cpu.cycles in
  let ppu_dot_start = nes.ppu.dot in
  let ppu_sl_start = nes.ppu.scanline in
  Nes.tick nes;
  Alcotest.(check int) "CPU +1 cycle" 1 (nes.cpu.cycles - cpu_start);
  (* PPU が 3 dot 進む (scanline 越えも考慮) *)
  let advanced =
    ((nes.ppu.scanline - ppu_sl_start) * 341) + (nes.ppu.dot - ppu_dot_start)
  in
  Alcotest.(check int) "PPU +3 dot" 3 advanced

(* vblank に達すると frame_complete + nmi_request が立つ.
   PPUCTRL.V=1 で NMI enable した状態で 1 フレーム回す. *)
let test_run_until_frame_fires_vblank () =
  let cart = nrom_with_nop_loop ~reset_vec:0x8000 in
  let nes = Nes.mk () in
  Nes.connect_cartridge nes cart;
  Nes.power_on nes;
  (* PPUCTRL.V を立てる (LDA #$80 ; STA $2000) を直接バス経由で書き込む *)
  nes.memory_bus.write (Uint16.of_int 0x2000) (Uint8.of_int 0x80);
  Nes.run_until_frame nes;
  Alcotest.(check bool)
    "frame_complete (set on vblank entry)"
    true
    nes.ppu.frame_complete;
  Alcotest.(check bool) "vblank flag set" true nes.ppu.status.vblank_flag;
  Alcotest.(check int) "scanline = 241" 241 nes.ppu.scanline;
  Alcotest.(check int) "dot = 1" 1 nes.ppu.dot

(* 1 フレームあたり ≒ 29780 CPU cycle (89342 PPU dot / 3) が消費される. *)
let test_run_until_frame_cycle_budget () =
  let cart = nrom_with_nop_loop ~reset_vec:0x8000 in
  let nes = Nes.mk () in
  Nes.connect_cartridge nes cart;
  Nes.power_on nes;
  let c_start = nes.cpu.cycles in
  Nes.run_until_frame nes;
  let consumed = nes.cpu.cycles - c_start in
  (* vblank 開始までは (241 * 341 + 1) dot = 82182 dot ≒ 27394 CPU cycle.
     スタート位置 (dot=0, sl=0) からなので、ピッタリでない場合があるが
     誤差は十分小さい. *)
  Alcotest.(check bool)
    (Printf.sprintf "27000 < cycles=%d < 28000" consumed)
    true
    (consumed > 27000 && consumed < 28000)

(* PPU が NMI を上げたら次の CPU 命令境界で NMI シーケンスが走る.
   reset vector の手前に LDA #$80 ; STA $2000 を仕込んでから run_until_frame. *)
let test_ppu_nmi_propagates_to_cpu () =
  let prg, cart = make_nrom_cart ~prg_kb:16 () in
  (* PRG の先頭に: LDA #$80 ($A9 $80) ; STA $2000 ($8D $00 $20) ; loop NOP *)
  Bytes.set_uint8 prg 0 0xA9;
  Bytes.set_uint8 prg 1 0x80;
  Bytes.set_uint8 prg 2 0x8D;
  Bytes.set_uint8 prg 3 0x00;
  Bytes.set_uint8 prg 4 0x20;
  for i = 5 to Bytes.length prg - 7 do
    Bytes.set_uint8 prg i 0xEA
  done;
  (* reset vector: $8000 *)
  set_vectors prg ~nmi:0x9000 ~reset:0x8000 ~irq:0;
  let nes = Nes.mk () in
  Nes.connect_cartridge nes cart;
  Nes.power_on nes;
  (* run 1 frame: NMI が発火、PC が NMI ベクタ ($9000) に飛んでいるはず *)
  Nes.run_until_frame nes;
  (* run_until_frame は vblank 開始で抜けるが、まだ CPU が NMI を実行する
     には次の命令境界まで進む必要がある。さらに 1 命令進める. *)
  let _ : int = Cpu.step_instruction nes.memory_bus nes.cpu in
  Alcotest.(check int)
    "PC = NMI vector $9000"
    0x9000
    (Uint16.to_int nes.cpu.reg_PC)

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "Nes"
    [ ("初期状態", [ Alcotest.test_case "mk: 空の NES" `Quick test_mk_empty ])
    ; ( "PRG mapping (NROM)"
      , [ Alcotest.test_case "NROM-128 mirror" `Quick test_nrom128_mirrors
        ; Alcotest.test_case "NROM-256 no mirror" `Quick test_nrom256_no_mirror
        ] )
    ; ( "PRG mapping (UNROM)"
      , [ Alcotest.test_case "last bank fixed" `Quick test_unrom_last_bank_fixed
        ; Alcotest.test_case "bank switch" `Quick test_unrom_bank_switch
        ] )
    ; ( "ベクタ / RESET"
      , [ Alcotest.test_case "connect でベクタロード" `Quick test_vectors_on_connect
        ; Alcotest.test_case
            "power_on は reset を呼ぶ"
            `Quick
            test_power_on_resets_pc
        ] )
    ; ( "WRAM / 未マップ領域"
      , [ Alcotest.test_case "WRAM round trip" `Quick test_wram_roundtrip
        ; Alcotest.test_case
            "APU/IO/SRAM 領域は no-op スタブ"
            `Quick
            test_unmapped_returns_zero
        ] )
    ; ( "eject / cart 差し替え"
      , [ Alcotest.test_case "eject は WRAM を保持" `Quick test_eject_preserves_wram
        ; Alcotest.test_case
            "256W: A→eject→B→reset 後も WRAM 残存"
            `Quick
            test_swap_carts_preserves_wram
        ] )
    ; ( "Ines.parse 経由の connect"
      , [ Alcotest.test_case "正常 iNES" `Quick test_connect_from_bytes
        ; Alcotest.test_case "invalid magic" `Quick test_connect_invalid_magic
        ] )
    ; ( "PPU メモリ統合 (Phase B2)"
      , [ Alcotest.test_case "connect で PPU mirroring 注入" `Quick (fun () ->
            let _, cart_h = make_nrom_cart ~prg_kb:16 () in
            let nes = nes_with cart_h in
            Alcotest.(check bool) "H mirroring" true (nes.ppu.mirroring = Cart.H);
            let _, cart_v =
              make_nrom_cart
                ~spec:
                  { mirroring = V; has_battery = false; has_trainer = false }
                ~prg_kb:16
                ()
            in
            let nes2 = nes_with cart_v in
            Alcotest.(check bool)
              "V mirroring"
              true
              (nes2.ppu.mirroring = Cart.V))
        ; Alcotest.test_case "$2007 経由で CHR を read できる" `Quick (fun () ->
            let _, cart = make_nrom_cart ~prg_kb:16 () in
            (* CHR の特定 offset に sentinel を仕込む *)
            (match cart.rom with
             | NROM { chr; _ } -> Bytes.set_uint8 chr 0x0100 0xBE
             | _ -> ());
            let nes = nes_with cart in
            (* $2006 で v = $0100 をセット *)
            bus_write_u8 nes 0x2006 0x01;
            bus_write_u8 nes 0x2006 0x00;
            (* buffer 経由なので 1 回 read で捨て、もう一度セットして読む *)
            let _ = bus_read_u8 nes 0x2007 in
            bus_write_u8 nes 0x2006 0x01;
            bus_write_u8 nes 0x2006 0x00;
            let _ = bus_read_u8 nes 0x2007 in
            let v = bus_read_u8 nes 0x2007 in
            Alcotest.(check int) "CHR[$0100] = $BE" 0xBE v)
        ; Alcotest.test_case "eject で PPU CHR は empty に戻る" `Quick (fun () ->
            let _, cart = make_nrom_cart ~prg_kb:16 () in
            (match cart.rom with
             | NROM { chr; _ } -> Bytes.set_uint8 chr 0x0050 0xAB
             | _ -> ());
            let nes = nes_with cart in
            Nes.eject nes;
            Alcotest.(check bool)
              "mirroring reset to H"
              true
              (nes.ppu.mirroring = Cart.H);
            bus_write_u8 nes 0x2006 0x00;
            bus_write_u8 nes 0x2006 0x50;
            let _ = bus_read_u8 nes 0x2007 in
            bus_write_u8 nes 0x2006 0x00;
            bus_write_u8 nes 0x2006 0x50;
            let _ = bus_read_u8 nes 0x2007 in
            let v = bus_read_u8 nes 0x2007 in
            Alcotest.(check int) "CHR は empty (0)" 0 v)
        ] )
    ; ( "Controller (Phase C)"
      , [ Alcotest.test_case
            "$4016 write は P1/P2 両方 strobe、$4016 read = P1"
            `Quick
            (fun () ->
               let _, cart = make_nrom_cart ~prg_kb:16 () in
               let nes = nes_with cart in
               Emulator.Controller.set_button nes.controller1 A true;
               Emulator.Controller.set_button nes.controller2 B true;
               (* strobe: 1 → 0 で latch *)
               bus_write_u8 nes 0x4016 1;
               bus_write_u8 nes 0x4016 0;
               (* P1: A=1, あと 7 bit *)
               Alcotest.(check int) "P1 A" 1 (bus_read_u8 nes 0x4016);
               Alcotest.(check int) "P1 B" 0 (bus_read_u8 nes 0x4016);
               (* P2: A=0, B=1, あと... *)
               Alcotest.(check int) "P2 A" 0 (bus_read_u8 nes 0x4017);
               Alcotest.(check int) "P2 B" 1 (bus_read_u8 nes 0x4017))
        ; Alcotest.test_case
            "$4017 write は controller の strobe に影響しない (APU 用)"
            `Quick
            (fun () ->
               let _, cart = make_nrom_cart ~prg_kb:16 () in
               let nes = nes_with cart in
               Emulator.Controller.set_button nes.controller1 Start true;
               (* 通常の strobe 経由で latch *)
               bus_write_u8 nes 0x4016 1;
               bus_write_u8 nes 0x4016 0;
               let _ = bus_read_u8 nes 0x4016 in
               (* A *)
               let _ = bus_read_u8 nes 0x4016 in
               (* B *)
               let _ = bus_read_u8 nes 0x4016 in
               (* Select *)
               (* ここで $4017 に 1 を書いてみる. もし誤って strobe に影響すれば
              shift register が崩れて Start = 1 が読めないはず. *)
               bus_write_u8 nes 0x4017 1;
               Alcotest.(check int)
                 "Start still readable"
                 1
                 (bus_read_u8 nes 0x4016))
        ] )
    ; ( "OAMDMA (Phase B3)"
      , [ Alcotest.test_case
            "$4014 で WRAM 256 byte が OAM にコピーされる"
            `Quick
            (fun () ->
               let _, cart = make_nrom_cart ~prg_kb:16 () in
               let nes = nes_with cart in
               (* WRAM $0200-$02FF にパターンを書く *)
               for i = 0 to 255 do
                 bus_write_u8 nes (0x0200 + i) (i * 7 land 0xFF)
               done;
               (* $4014 write で DMA pending を立てる *)
               bus_write_u8 nes 0x4014 0x02;
               Alcotest.(check bool)
                 "dma_source pending"
                 true
                 (nes.dma_source = Some 0x02);
               (* Nes.tick が DMA を消化する *)
               let prev_cycles = nes.cpu.cycles in
               Nes.tick nes;
               Alcotest.(check bool)
                 "dma_source cleared"
                 true
                 (nes.dma_source = None);
               (* OAM の中身が WRAM のパターンと一致 *)
               for i = 0 to 255 do
                 Alcotest.(check int)
                   (Printf.sprintf "OAM[%d]" i)
                   (i * 7 land 0xFF)
                   (Bytes.get_uint8 nes.ppu.oam i)
               done;
               (* CPU cycle は 513 or 514 進む *)
               let diff = nes.cpu.cycles - prev_cycles in
               Alcotest.(check bool)
                 "CPU stall 513 or 514"
                 true
                 (diff = 513 || diff = 514))
        ] )
    ; ( "CPU↔PPU lockstep"
      , [ Alcotest.test_case
            "tick: CPU +1 cycle, PPU +3 dot"
            `Quick
            test_tick_advances_cpu_1_ppu_3
        ; Alcotest.test_case
            "run_until_frame で vblank 突入"
            `Quick
            test_run_until_frame_fires_vblank
        ; Alcotest.test_case
            "1 フレーム ≒ 29780 CPU cycle"
            `Quick
            test_run_until_frame_cycle_budget
        ; Alcotest.test_case
            "PPU の NMI が CPU PC を NMI ベクタへ"
            `Quick
            test_ppu_nmi_propagates_to_cpu
        ] )
    ]
