(* Per-pixel PPU renderer (Phase B6/B7) のテスト.
   - Loopy v register operations (inc_coarse_x / inc_y / copy_horizontal / copy_vertical)
   - Sprite eval の 8 sprite/scanline 上限 + overflow flag
   - mid-frame CHR bank swap が framebuffer に反映 *)

open Famicaml_common.Nesint
module Ppu = Emulator.Ppu
module Cart = Emulator.Rom.Cartridge

let u16 = Uint16.of_int

let make_ppu_with_chr ~chr_io () =
  let ppu = Ppu.mk () in
  Ppu.connect_cart ppu ~mirroring:Cart.H ~chr_io;
  ppu

let empty_chr_io () : Ppu.chr_io =
  { chr_read = (fun _ -> Uint8.zero)
  ; chr_write = (fun _ _ -> ())
  ; a12_rise = (fun () -> ())
  }

let step_n ppu n =
  for _ = 1 to n do
    Ppu.step ppu
  done

(* ------------------------------------------------------------------ *)
(* Loopy v register operations                                         *)
(*                                                                     *)
(* NESdev "PPU scrolling":                                             *)
(*   dot 8/16/.../256:    inc coarse X (BG fetch ごと)                 *)
(*   dot 256:             inc Y                                        *)
(*   dot 257:             hori(v) = hori(t)                            *)
(*   pre-render dot 280-304: vert(v) = vert(t)                         *)
(* ------------------------------------------------------------------ *)

let test_horizontal_copy_at_dot_257 () =
  let ppu = make_ppu_with_chr ~chr_io:(empty_chr_io ()) () in
  (* rendering on で v register update が走る *)
  ppu.mask <- { ppu.mask with enable_bg = true };
  (* t: NT_x=1, coarse_X=$1F → t bit 10 + bit 0..4 = 0x041F *)
  ppu.internal.t <- u16 0x041F;
  ppu.internal.v <- u16 0;
  (* scanline 0 dot 257 まで step. dot は 0 から始まり、PPU.step で
     dot を進める. PPU.mk の初期 dot は 0、scanline は 0. step 1 回で dot=1. *)
  step_n ppu 257;
  let v = Uint16.to_int ppu.internal.v in
  Alcotest.(check int)
    "v[NT_x + coarse_X] = t[NT_x + coarse_X]"
    0x041F
    (v land 0x041F)

let test_y_increment_at_dot_256 () =
  let ppu = make_ppu_with_chr ~chr_io:(empty_chr_io ()) () in
  ppu.mask <- { ppu.mask with enable_bg = true };
  (* v: fine_y=0, coarse_Y=0 → 0. dot 256 で fine_y++ *)
  ppu.internal.v <- u16 0;
  ppu.internal.t <- u16 0;
  step_n ppu 256;
  let v = Uint16.to_int ppu.internal.v in
  Alcotest.(check int) "fine_y incremented to 1" 0x1000 (v land 0x7000)

let test_vertical_copy_in_pre_render () =
  let ppu = make_ppu_with_chr ~chr_io:(empty_chr_io ()) () in
  ppu.mask <- { ppu.mask with enable_bg = true };
  (* t: fine_y=7, coarse_Y=29, NT_y=1 → t bits 14-12 (fine_y) + 11 (NT_y) + 9-5 (coarse_Y) *)
  let t_v =
    (7 lsl 12) (* fine_y = 7 *)
    lor (1 lsl 11) (* NT_y = 1 *)
    lor (29 lsl 5) (* coarse_Y = 29 *)
  in
  ppu.internal.t <- u16 t_v;
  ppu.internal.v <- u16 0;
  (* Step まで: scanline 261 (pre-render) dot 280 = (261 * 341) + 280 *)
  step_n ppu ((261 * 341) + 280);
  let v = Uint16.to_int ppu.internal.v in
  let mask = 0x7BE0 in
  Alcotest.(check int) "v[vert bits] = t[vert bits]" (t_v land mask) (v land mask)

let test_no_v_update_when_rendering_off () =
  let ppu = make_ppu_with_chr ~chr_io:(empty_chr_io ()) () in
  ppu.mask <- { ppu.mask with enable_bg = false; enable_sprite = false };
  ppu.internal.t <- u16 0x041F;
  ppu.internal.v <- u16 0;
  step_n ppu 300;
  Alcotest.(check int) "v unchanged" 0 (Uint16.to_int ppu.internal.v)

(* ------------------------------------------------------------------ *)
(* Sprite evaluation                                                   *)
(* ------------------------------------------------------------------ *)

let test_sprite_overflow_when_more_than_8_in_range () =
  let ppu = make_ppu_with_chr ~chr_io:(empty_chr_io ()) () in
  ppu.mask <- { ppu.mask with enable_bg = true; enable_sprite = true };
  (* OAM: 9 sprite を y = 20 に配置 (8x8 mode → scanline 20..27 で in range) *)
  for i = 0 to 8 do
    Bytes.set_uint8 ppu.oam (i * 4) 20;
    Bytes.set_uint8 ppu.oam ((i * 4) + 1) 0;
    Bytes.set_uint8 ppu.oam ((i * 4) + 2) 0;
    Bytes.set_uint8 ppu.oam ((i * 4) + 3) (i * 8)
  done;
  (* scanline 19 dot 257 で evaluate_sprites_for scanline=20 が走る. *)
  let target = (19 * 341) + 257 in
  step_n ppu target;
  Alcotest.(check bool) "overflow set" true ppu.status.sprite_overflow;
  Alcotest.(check int) "sprite_count = 8" 8 ppu.sprite_count

let test_no_overflow_when_8_or_fewer () =
  let ppu = make_ppu_with_chr ~chr_io:(empty_chr_io ()) () in
  ppu.mask <- { ppu.mask with enable_bg = true; enable_sprite = true };
  for i = 0 to 7 do
    Bytes.set_uint8 ppu.oam (i * 4) 20;
    Bytes.set_uint8 ppu.oam ((i * 4) + 1) 0;
    Bytes.set_uint8 ppu.oam ((i * 4) + 2) 0;
    Bytes.set_uint8 ppu.oam ((i * 4) + 3) (i * 8)
  done;
  (* 残り 56 sprite は y = $F0 (off-screen) で範囲外 *)
  for i = 8 to 63 do
    Bytes.set_uint8 ppu.oam (i * 4) 0xF0
  done;
  let target = (19 * 341) + 257 in
  step_n ppu target;
  Alcotest.(check bool) "no overflow" false ppu.status.sprite_overflow;
  Alcotest.(check int) "sprite_count = 8" 8 ppu.sprite_count

(* ------------------------------------------------------------------ *)
(* Mid-frame CHR bank swap が framebuffer に反映                       *)
(*                                                                     *)
(* chr_io closure 内に mutable bank ref を持ち、scanline 120 を超えた  *)
(* タイミングで CHR の中身を変える. visible scanline 0..119 と         *)
(* 120..239 で framebuffer の色が変わることを確認.                     *)
(* ------------------------------------------------------------------ *)

let test_mid_frame_chr_swap () =
  let ppu = Ppu.mk () in
  (* CHR: pattern 0 = 全 color=1 (lo=$FF, hi=$00) で upper. swap 後 全 color=2. *)
  let bank = ref 0 in
  let chr_io : Ppu.chr_io =
    { chr_read =
        (fun a ->
          if a < 8
          then
            (* tile 0 lo plane: !bank=0 で $FF, =1 で $00 *)
            if !bank = 0 then Uint8.of_int 0xFF else Uint8.zero
          else if a < 16
          then
            (* tile 0 hi plane: !bank=0 で $00, =1 で $FF *)
            if !bank = 0 then Uint8.zero else Uint8.of_int 0xFF
          else Uint8.zero)
    ; chr_write = (fun _ _ -> ())
    ; a12_rise = (fun () -> ())
    }
  in
  Ppu.connect_cart ppu ~mirroring:Cart.H ~chr_io;
  (* nametable 全部 tile 0 (既に $00 で初期化済み). palette: bg=$0F, $01=$06, $02=$1A *)
  Bytes.set_uint8 ppu.palette_ram 0 0x0F;
  Bytes.set_uint8 ppu.palette_ram 1 0x06;
  Bytes.set_uint8 ppu.palette_ram 2 0x1A;
  ppu.mask <- { ppu.mask with enable_bg = true; enable_bg_left_column = true };
  (* scanline 120 dot 256 まで step (= 上半分の最終 fetch まで) *)
  step_n ppu ((120 * 341) + 256);
  (* bank swap *)
  bank := 1;
  (* 残り frame を step (scanline 261 dot 340 まで) *)
  step_n ppu ((262 * 341) - ((120 * 341) + 256));
  (* 上半分 pixel: color=1, palette idx = (0*4)+1 = 1 → master[$06] *)
  let exp_top_r, exp_top_g, exp_top_b = Ppu.master_rgb ppu 0x06 in
  (* 下半分 pixel: color=2, palette idx = (0*4)+2 = 2 → master[$1A] *)
  let exp_bot_r, exp_bot_g, exp_bot_b = Ppu.master_rgb ppu 0x1A in
  let read_px x y =
    let off = ((y * 256) + x) * 4 in
    ( Bytes.get_uint8 ppu.framebuffer off
    , Bytes.get_uint8 ppu.framebuffer (off + 1)
    , Bytes.get_uint8 ppu.framebuffer (off + 2) )
  in
  let top_r, top_g, top_b = read_px 0 50 in
  let bot_r, bot_g, bot_b = read_px 0 200 in
  Alcotest.(check int) "top R = master[$06].R" exp_top_r top_r;
  Alcotest.(check int) "top G" exp_top_g top_g;
  Alcotest.(check int) "top B" exp_top_b top_b;
  Alcotest.(check int) "bottom R = master[$1A].R" exp_bot_r bot_r;
  Alcotest.(check int) "bottom G" exp_bot_g bot_g;
  Alcotest.(check int) "bottom B" exp_bot_b bot_b

(* ------------------------------------------------------------------ *)
(* Palette cache の lazy invalidation                                  *)
(* ------------------------------------------------------------------ *)

let test_palette_write_invalidates_cache () =
  let ppu = make_ppu_with_chr ~chr_io:(empty_chr_io ()) () in
  ppu.mask <- { ppu.mask with enable_bg = true; enable_bg_left_column = true };
  (* CHR は全 0 → bg color 0 = palette[0] *)
  (* palette[0] = $30 (白っぽい) で 1 frame *)
  Bytes.set_uint8 ppu.palette_ram 0 0x30;
  step_n ppu (262 * 341);
  let r1 = Bytes.get_uint8 ppu.framebuffer 0 in
  (* $2007 (PPUDATA write) 経由で palette[0] を $0F (黒) に変更. *)
  Ppu.cpu_write ppu (u16 0x2006) (Uint8.of_int 0x3F);
  Ppu.cpu_write ppu (u16 0x2006) (Uint8.of_int 0x00);
  Ppu.cpu_write ppu (u16 0x2007) (Uint8.of_int 0x0F);
  step_n ppu (262 * 341);
  let r2 = Bytes.get_uint8 ppu.framebuffer 0 in
  Alcotest.(check bool) "palette write 後 色が変わる" true (r1 <> r2)

(* ------------------------------------------------------------------ *)
(* テスト登録                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "PPU per-pixel renderer"
    [ ( "Loopy v register operations"
      , [ Alcotest.test_case
            "horizontal copy at dot 257"
            `Quick
            test_horizontal_copy_at_dot_257
        ; Alcotest.test_case "Y inc at dot 256" `Quick test_y_increment_at_dot_256
        ; Alcotest.test_case
            "vertical copy at pre-render dot 280"
            `Quick
            test_vertical_copy_in_pre_render
        ; Alcotest.test_case
            "rendering disabled なら v 不変"
            `Quick
            test_no_v_update_when_rendering_off
        ] )
    ; ( "Sprite evaluation"
      , [ Alcotest.test_case
            "9 sprite 同一行 → overflow"
            `Quick
            test_sprite_overflow_when_more_than_8_in_range
        ; Alcotest.test_case
            "8 sprite 以下 → overflow なし"
            `Quick
            test_no_overflow_when_8_or_fewer
        ] )
    ; ( "Per-pixel rendering behaviors"
      , [ Alcotest.test_case
            "mid-frame CHR bank swap が framebuffer に反映"
            `Quick
            test_mid_frame_chr_swap
        ; Alcotest.test_case
            "palette write が cache を invalidate"
            `Quick
            test_palette_write_invalidates_cache
        ] )
    ]
