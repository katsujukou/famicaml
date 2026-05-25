open Famicaml_common.Nesint
module R = Emulator.Ppu.Register
module N = Emulator.Ppu.Nametable
module Ctrl = R.Ppu_control
module Mask = R.Ppu_mask
module Status = R.Ppu_status

(* ------------------------------------------------------------------ *)
(* テスト用ヘルパー                                                     *)
(* ------------------------------------------------------------------ *)

let hex = Printf.sprintf "$%02X"

(* 256 通りすべてのバイト値を回す。エラー時は失敗したバイトが分かる. *)
let for_each_byte f =
  for i = 0 to 0xFF do
    f (Uint8.of_int i)
  done

(* ------------------------------------------------------------------ *)
(* PPUCTRL ($2000)  VPHB SINN                                          *)
(*   V: NMI enable (bit 7)                                             *)
(*   P: master/slave (bit 6)                                           *)
(*   H: sprite size (bit 5)                                            *)
(*   B: bg pattern table (bit 4)                                       *)
(*   S: sprite pattern table (bit 3)                                   *)
(*   I: VRAM addr increment (bit 2)                                    *)
(*   NN: nametable select (bits 1-0)                                   *)
(* ------------------------------------------------------------------ *)

let test_ppuctrl_bit_assignment () =
  (* 1 つだけビットを立てたバイトから of_uint8 して、対応フィールドだけが
     セットされることを確認する。 *)
  let r = Ctrl.of_uint8 (Uint8.of_int 0x80) in
  Alcotest.(check bool) "bit7 = enable_nmi" true r.enable_nmi;
  let r = Ctrl.of_uint8 (Uint8.of_int 0x40) in
  Alcotest.(check bool) "bit6 = ppu_master_slave" true r.ppu_master_slave;
  let r = Ctrl.of_uint8 (Uint8.of_int 0x20) in
  Alcotest.(check bool) "bit5 = sprite_size 8x16" true (r.sprite_size = Spr_8x16);
  let r = Ctrl.of_uint8 (Uint8.of_int 0x10) in
  Alcotest.(check bool) "bit4 = bg_alignment R" true (r.bg_alignment = R);
  let r = Ctrl.of_uint8 (Uint8.of_int 0x08) in
  Alcotest.(check bool) "bit3 = spr_alignment R" true (r.spr_alignment = R);
  let r = Ctrl.of_uint8 (Uint8.of_int 0x04) in
  Alcotest.(check bool) "bit2 = addr_incr VAI_32" true (r.addr_incr = VAI_32);
  let r = Ctrl.of_uint8 (Uint8.of_int 0x03) in
  Alcotest.(check bool) "bits1-0 = Nmtbl_2C00" true (r.base_nmtbl = N.Nmtbl_2C00)

let test_ppuctrl_zero () =
  let r = Ctrl.of_uint8 Uint8.zero in
  Alcotest.(check bool) "enable_nmi" false r.enable_nmi;
  Alcotest.(check bool) "master_slave" false r.ppu_master_slave;
  Alcotest.(check bool) "sprite_size 8x8" true (r.sprite_size = Spr_8x8);
  Alcotest.(check bool) "bg_alignment L" true (r.bg_alignment = L);
  Alcotest.(check bool) "spr_alignment L" true (r.spr_alignment = L);
  Alcotest.(check bool) "addr_incr VAI_01" true (r.addr_incr = VAI_01);
  Alcotest.(check bool) "Nmtbl_2000" true (r.base_nmtbl = N.Nmtbl_2000)

let test_ppuctrl_nametable_mapping () =
  let cases =
    [ (0x00, N.Nmtbl_2000)
    ; (0x01, N.Nmtbl_2400)
    ; (0x02, N.Nmtbl_2800)
    ; (0x03, N.Nmtbl_2C00)
    ]
  in
  List.iter
    (fun (byte, expected) ->
       let r = Ctrl.of_uint8 (Uint8.of_int byte) in
       Alcotest.(check bool) (hex byte) true (r.base_nmtbl = expected))
    cases

(* of_uint8 -> to_uint8 が恒等になっていることを 0..255 全数で確認。
   PPUCTRL は 8 bit すべてが意味を持つので、完全な round trip が成立する。 *)
let test_ppuctrl_roundtrip () =
  for_each_byte (fun b ->
    let r = Ctrl.of_uint8 b in
    let back = Ctrl.to_uint8 r in
    Alcotest.(check int)
      (Printf.sprintf "roundtrip %s" (hex (Uint8.to_int b)))
      (Uint8.to_int b)
      (Uint8.to_int back))

(* ------------------------------------------------------------------ *)
(* PPUMASK ($2001)  BGRs bMmG                                          *)
(*   B/G/R: color emphasis blue/green/red (bits 7/6/5)                 *)
(*   s: sprite enable (bit 4)                                          *)
(*   b: bg enable (bit 3)                                              *)
(*   M: sprite left column (bit 2)                                     *)
(*   m: bg left column (bit 1)                                         *)
(*   G: greyscale (bit 0)                                              *)
(* ------------------------------------------------------------------ *)

let test_ppumask_bit_assignment () =
  let r = Mask.of_uint8 (Uint8.of_int 0x80) in
  Alcotest.(check bool) "bit7 = blue" true r.color_emphasis.blue;
  let r = Mask.of_uint8 (Uint8.of_int 0x40) in
  Alcotest.(check bool) "bit6 = green" true r.color_emphasis.green;
  let r = Mask.of_uint8 (Uint8.of_int 0x20) in
  Alcotest.(check bool) "bit5 = red" true r.color_emphasis.red;
  let r = Mask.of_uint8 (Uint8.of_int 0x10) in
  Alcotest.(check bool) "bit4 = enable_sprite" true r.enable_sprite;
  let r = Mask.of_uint8 (Uint8.of_int 0x08) in
  Alcotest.(check bool) "bit3 = enable_bg" true r.enable_bg;
  let r = Mask.of_uint8 (Uint8.of_int 0x04) in
  Alcotest.(check bool)
    "bit2 = enable_spr_left_column"
    true
    r.enable_spr_left_column;
  let r = Mask.of_uint8 (Uint8.of_int 0x02) in
  Alcotest.(check bool)
    "bit1 = enable_bg_left_column"
    true
    r.enable_bg_left_column;
  let r = Mask.of_uint8 (Uint8.of_int 0x01) in
  Alcotest.(check bool) "bit0 = gray_scale" true r.gray_scale

let test_ppumask_roundtrip () =
  for_each_byte (fun b ->
    let r = Mask.of_uint8 b in
    let back = Mask.to_uint8 r in
    Alcotest.(check int)
      (Printf.sprintf "roundtrip %s" (hex (Uint8.to_int b)))
      (Uint8.to_int b)
      (Uint8.to_int back))

(* ------------------------------------------------------------------ *)
(* PPUSTATUS ($2002)  VSO- ----                                        *)
(*   V: vblank (bit 7)                                                 *)
(*   S: sprite 0 hit (bit 6)                                           *)
(*   O: sprite overflow (bit 5)                                        *)
(*   bits 4-0: open bus / undefined                                    *)
(* ------------------------------------------------------------------ *)

let test_ppustatus_bit_assignment () =
  let r = Status.of_uint8 (Uint8.of_int 0x80) in
  Alcotest.(check bool) "bit7 = vblank" true r.vblank_flag;
  let r = Status.of_uint8 (Uint8.of_int 0x40) in
  Alcotest.(check bool) "bit6 = sprite_0_hit" true r.sprite_0_hit;
  let r = Status.of_uint8 (Uint8.of_int 0x20) in
  Alcotest.(check bool) "bit5 = sprite_overflow" true r.sprite_overflow

(* 上位 3 bit (0xE0) だけが round trip。下位 5 bit は捨てられる. *)
let test_ppustatus_roundtrip_upper_bits () =
  for_each_byte (fun b ->
    let r = Status.of_uint8 b in
    let back = Status.to_uint8 r in
    Alcotest.(check int)
      (Printf.sprintf "roundtrip %s (upper 3 bits)" (hex (Uint8.to_int b)))
      (Uint8.to_int b land 0xE0)
      (Uint8.to_int back))

(* 下位 5 bit が立っていても、of_uint8 はそれを 3 つのフラグに反映しない. *)
let test_ppustatus_low_bits_ignored () =
  let r = Status.of_uint8 (Uint8.of_int 0x1F) in
  Alcotest.(check bool) "vblank false" false r.vblank_flag;
  Alcotest.(check bool) "sprite_0_hit false" false r.sprite_0_hit;
  Alcotest.(check bool) "sprite_overflow false" false r.sprite_overflow

(* ------------------------------------------------------------------ *)
(* Power-up 初期値                                                      *)
(* NesDev "PPU power up state": PPUCTRL=$00, PPUMASK=$00, PPUSTATUS=$00 *)
(* internal は v=t=x=0, w=false                                         *)
(* ------------------------------------------------------------------ *)

let test_ppuctrl_initial () =
  let r = Ctrl.initial () in
  Alcotest.(check int)
    "PPUCTRL initial = $00"
    0x00
    (Uint8.to_int (Ctrl.to_uint8 r));
  Alcotest.(check bool) "enable_nmi" false r.enable_nmi;
  Alcotest.(check bool) "master_slave" false r.ppu_master_slave;
  Alcotest.(check bool) "sprite_size 8x8" true (r.sprite_size = Spr_8x8);
  Alcotest.(check bool) "bg_alignment L" true (r.bg_alignment = L);
  Alcotest.(check bool) "spr_alignment L" true (r.spr_alignment = L);
  Alcotest.(check bool) "addr_incr VAI_01" true (r.addr_incr = VAI_01);
  Alcotest.(check bool) "Nmtbl_2000" true (r.base_nmtbl = N.Nmtbl_2000)

let test_ppumask_initial () =
  let r = Mask.initial () in
  Alcotest.(check int)
    "PPUMASK initial = $00"
    0x00
    (Uint8.to_int (Mask.to_uint8 r));
  Alcotest.(check bool) "emphasis red" false r.color_emphasis.red;
  Alcotest.(check bool) "emphasis green" false r.color_emphasis.green;
  Alcotest.(check bool) "emphasis blue" false r.color_emphasis.blue;
  Alcotest.(check bool) "enable_sprite" false r.enable_sprite;
  Alcotest.(check bool) "enable_bg" false r.enable_bg;
  Alcotest.(check bool) "spr left column" false r.enable_spr_left_column;
  Alcotest.(check bool) "bg left column" false r.enable_bg_left_column;
  Alcotest.(check bool) "gray_scale" false r.gray_scale

let test_ppustatus_initial () =
  let r = Status.initial () in
  Alcotest.(check int)
    "PPUSTATUS initial (upper 3 bits) = 0"
    0x00
    (Uint8.to_int (Status.to_uint8 r) land 0xE0);
  Alcotest.(check bool) "vblank_flag" false r.vblank_flag;
  Alcotest.(check bool) "sprite_0_hit" false r.sprite_0_hit;
  Alcotest.(check bool) "sprite_overflow" false r.sprite_overflow

let test_ppu_internal_initial () =
  let r = R.Ppu_internal.initial () in
  Alcotest.(check int) "v = 0" 0 (Uint16.to_int r.v);
  Alcotest.(check int) "t = 0" 0 (Uint16.to_int r.t);
  Alcotest.(check int) "x = 0" 0 (Uint8.to_int r.x);
  Alcotest.(check bool) "w = false" false r.w

(* Ppu_internal は mutable record なので、initial () が呼び出しごとに
   独立した record を返すことを確認 (共有事故防止)。
   Ppu_control / Ppu_mask / Ppu_status は immutable record なので
   共有されても害がない (関数型データ)。 *)
let test_internal_initial_returns_fresh_instance () =
  let r1 = R.Ppu_internal.initial () in
  let r2 = R.Ppu_internal.initial () in
  r1.v <- Uint16.of_int 0x1234;
  Alcotest.(check int) "r1 mutated" 0x1234 (Uint16.to_int r1.v);
  Alcotest.(check int) "r2 NOT mutated" 0 (Uint16.to_int r2.v)

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "PPU Register"
    [ ( "PPUCTRL ($2000)"
      , [ Alcotest.test_case "個別ビット → フィールド" `Quick test_ppuctrl_bit_assignment
        ; Alcotest.test_case "0x00 = 既定値" `Quick test_ppuctrl_zero
        ; Alcotest.test_case
            "nametable マッピング"
            `Quick
            test_ppuctrl_nametable_mapping
        ; Alcotest.test_case "round trip (0..255)" `Quick test_ppuctrl_roundtrip
        ] )
    ; ( "PPUMASK ($2001)"
      , [ Alcotest.test_case "個別ビット → フィールド" `Quick test_ppumask_bit_assignment
        ; Alcotest.test_case "round trip (0..255)" `Quick test_ppumask_roundtrip
        ] )
    ; ( "PPUSTATUS ($2002)"
      , [ Alcotest.test_case
            "個別ビット → フィールド"
            `Quick
            test_ppustatus_bit_assignment
        ; Alcotest.test_case
            "上位 3 bit round trip"
            `Quick
            test_ppustatus_roundtrip_upper_bits
        ; Alcotest.test_case
            "下位 5 bit は open bus"
            `Quick
            test_ppustatus_low_bits_ignored
        ] )
    ; ( "Power-up 初期値"
      , [ Alcotest.test_case "PPUCTRL.initial = $00" `Quick test_ppuctrl_initial
        ; Alcotest.test_case "PPUMASK.initial = $00" `Quick test_ppumask_initial
        ; Alcotest.test_case
            "PPUSTATUS.initial = $00"
            `Quick
            test_ppustatus_initial
        ; Alcotest.test_case
            "Ppu_internal.initial 全 0"
            `Quick
            test_ppu_internal_initial
        ; Alcotest.test_case
            "Ppu_internal.initial () は毎回新インスタンス"
            `Quick
            test_internal_initial_returns_fresh_instance
        ] )
    ]
