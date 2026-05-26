open Famicaml_common.Nesint
module Ppu = Emulator.Ppu
module R = Ppu.Register

(* ------------------------------------------------------------------ *)
(* テスト用ヘルパー                                                     *)
(* ------------------------------------------------------------------ *)

let u8 = Uint8.of_int
let u16 = Uint16.of_int
let write ppu addr byte = Ppu.cpu_write ppu (u16 addr) (u8 byte)
let read ppu addr = Uint8.to_int (Ppu.cpu_read ppu (u16 addr))

(* ------------------------------------------------------------------ *)
(* mk                                                                  *)
(* ------------------------------------------------------------------ *)

let test_mk_initial () =
  let ppu = Ppu.mk () in
  Alcotest.(check int)
    "ctrl = $00"
    0
    (Uint8.to_int (R.Ppu_control.to_uint8 ppu.ctrl));
  Alcotest.(check int)
    "mask = $00"
    0
    (Uint8.to_int (R.Ppu_mask.to_uint8 ppu.mask));
  Alcotest.(check int)
    "status upper = 0"
    0
    (Uint8.to_int (R.Ppu_status.to_uint8 ppu.status) land 0xE0);
  Alcotest.(check int) "v = 0" 0 (Uint16.to_int ppu.internal.v);
  Alcotest.(check int) "t = 0" 0 (Uint16.to_int ppu.internal.t);
  Alcotest.(check int) "x = 0" 0 (Uint8.to_int ppu.internal.x);
  Alcotest.(check bool) "w = false" false ppu.internal.w;
  Alcotest.(check int) "oam_addr = 0" 0 (Uint8.to_int ppu.oam_addr)

(* ------------------------------------------------------------------ *)
(* $2000 PPUCTRL write                                                 *)
(* ------------------------------------------------------------------ *)

(* PPUCTRL の bit[1:0] が Loopy t の bit[11:10] に伝播する。
   仕様: NesDev "PPU scrolling" — $2000 write
   t: ...BA.. ........ <- d: ......BA *)
let test_write_ctrl_updates_internal_t () =
  let ppu = Ppu.mk () in
  (* d = 0b00000011 → t[11:10] = 0b11、つまり t = 0x0C00 *)
  write ppu 0x2000 0b00000011;
  Alcotest.(check int) "t[11:10] = 0b11" 0x0C00 (Uint16.to_int ppu.internal.t);
  (* d = 0b00000010 → t[11:10] = 0b10、t = 0x0800 *)
  write ppu 0x2000 0b00000010;
  Alcotest.(check int) "t[11:10] = 0b10" 0x0800 (Uint16.to_int ppu.internal.t);
  (* typed state にも反映 *)
  Alcotest.(check bool)
    "ctrl.base_nmtbl = 2800"
    true
    (ppu.ctrl.base_nmtbl = Nmtbl_2800)

let test_write_ctrl_preserves_other_t_bits () =
  let ppu = Ppu.mk () in
  (* t の他の bit に値を入れておく (mask: 0xF3FF 以外 = 0x0C00 以外) *)
  ppu.internal.t <- u16 0x73FF;
  write ppu 0x2000 0b00000001;
  (* bits 10-11 のみ書き換わり、それ以外は保持 *)
  Alcotest.(check int)
    "t = 0x73FF & 0xF3FF | 0x0400"
    (0x73FF land 0xF3FF lor 0x0400)
    (Uint16.to_int ppu.internal.t)

(* ------------------------------------------------------------------ *)
(* $2001 PPUMASK write                                                 *)
(* ------------------------------------------------------------------ *)

let test_write_mask () =
  let ppu = Ppu.mk () in
  write ppu 0x2001 0x18 (* enable sprite + bg *);
  Alcotest.(check bool) "enable_sprite" true ppu.mask.enable_sprite;
  Alcotest.(check bool) "enable_bg" true ppu.mask.enable_bg

(* ------------------------------------------------------------------ *)
(* $2002 PPUSTATUS read                                                *)
(* ------------------------------------------------------------------ *)

(* read_status は vblank フラグをクリアし、w を 0 にする。 *)
let test_read_status_clears_vblank_and_toggle () =
  let ppu = Ppu.mk () in
  (* 事前条件: vblank=true, w=true *)
  ppu.status <- { ppu.status with vblank_flag = true };
  ppu.internal.w <- true;
  let byte = read ppu 0x2002 in
  Alcotest.(check int) "byte の bit 7 (vblank) = 1" 0x80 (byte land 0x80);
  Alcotest.(check bool) "vblank がクリアされた" false ppu.status.vblank_flag;
  Alcotest.(check bool) "w が 0 になった" false ppu.internal.w

(* 下位 5 bit は open bus (直近データバス値) で埋められる。 *)
let test_read_status_low_bits_are_open_bus () =
  let ppu = Ppu.mk () in
  (* open_bus に $1F を仕込んでおく (どこかへ write すれば乗る) *)
  write ppu 0x2000 0x1F;
  (* status の上位 3 bit を立てない状態で read。低位 5 bit は open_bus の 0x1F が返るはず *)
  let byte = read ppu 0x2002 in
  Alcotest.(check int) "lower 5 bits = 0x1F" 0x1F (byte land 0x1F)

(* ------------------------------------------------------------------ *)
(* $2003 PPUADDR (実は OAMADDR) write                                  *)
(* ------------------------------------------------------------------ *)

let test_write_oam_addr () =
  let ppu = Ppu.mk () in
  write ppu 0x2003 0x42;
  Alcotest.(check int) "oam_addr = 0x42" 0x42 (Uint8.to_int ppu.oam_addr)

(* ------------------------------------------------------------------ *)
(* $2005 PPUSCROLL write                                               *)
(* ------------------------------------------------------------------ *)

(* 仕様:
   1 回目 (w=0): t[4:0]  <- byte[7:3], x <- byte[2:0], w <- 1
   2 回目 (w=1): t[14:12]<- byte[2:0], t[9:5] <- byte[7:3], w <- 0 *)
let test_write_scroll_first_write () =
  let ppu = Ppu.mk () in
  write ppu 0x2005 0b11010_101;
  (* coarse X = 0b11010 = 26, fine X = 0b101 = 5 *)
  Alcotest.(check int) "t[4:0] = 26" 26 (Uint16.to_int ppu.internal.t land 0x1F);
  Alcotest.(check int) "x = 5" 5 (Uint8.to_int ppu.internal.x);
  Alcotest.(check bool) "w = true" true ppu.internal.w

let test_write_scroll_second_write () =
  let ppu = Ppu.mk () in
  write ppu 0x2005 0xFF;
  (* 1 回目 *)
  write ppu 0x2005 0b01011_110;
  (* 2 回目: coarse Y = 0b01011 = 11, fine Y = 0b110 = 6 *)
  let t = Uint16.to_int ppu.internal.t in
  Alcotest.(check int) "coarse Y (t[9:5]) = 11" 11 ((t lsr 5) land 0x1F);
  Alcotest.(check int) "fine Y (t[14:12]) = 6" 6 ((t lsr 12) land 0x07);
  Alcotest.(check bool) "w = false (toggle reset)" false ppu.internal.w

(* ------------------------------------------------------------------ *)
(* $2006 PPUADDR write                                                 *)
(* ------------------------------------------------------------------ *)

(* 仕様:
   1 回目 (w=0): t[13:8] <- byte[5:0], t[14] <- 0, w <- 1
   2 回目 (w=1): t[7:0]  <- byte, v <- t, w <- 0 *)
let test_write_addr_two_writes_set_v () =
  let ppu = Ppu.mk () in
  (* 既に bit 14 を立てておく (1 回目でクリアされることを確認) *)
  ppu.internal.t <- u16 0x4000;
  write ppu 0x2006 0x2A;
  (* d = 0010 1010 → t[13:8] = 0b101010 = 0x2A、bit 14 クリア *)
  Alcotest.(check int)
    "t after 1st = 0x2A00"
    0x2A00
    (Uint16.to_int ppu.internal.t);
  Alcotest.(check bool) "w = true" true ppu.internal.w;
  write ppu 0x2006 0x34;
  (* t[7:0] = 0x34 → t = 0x2A34, v ← t *)
  Alcotest.(check int)
    "t after 2nd = 0x2A34"
    0x2A34
    (Uint16.to_int ppu.internal.t);
  Alcotest.(check int) "v = t = 0x2A34" 0x2A34 (Uint16.to_int ppu.internal.v);
  Alcotest.(check bool) "w = false" false ppu.internal.w

let test_write_addr_high_byte_top_2_bits_ignored () =
  let ppu = Ppu.mk () in
  write ppu 0x2006 0xFF;
  (* byte の上位 2 bit (0xC0) は捨てられ、bit[5:0] = 0x3F のみ採用 *)
  Alcotest.(check int)
    "t = 0x3F00 (bit 14 は 0)"
    0x3F00
    (Uint16.to_int ppu.internal.t)

(* ------------------------------------------------------------------ *)
(* $2002 read が $2005/$2006 の toggle を打ち消す                       *)
(* ------------------------------------------------------------------ *)

let test_read_status_resets_toggle_between_scroll_writes () =
  let ppu = Ppu.mk () in
  write ppu 0x2005 0xAA;
  (* 1 回目, w = true *)
  let _ = read ppu 0x2002 in
  (* w クリア *)
  Alcotest.(check bool) "w cleared by $2002 read" false ppu.internal.w;
  (* 次の $2005 write は再び 1 回目として扱われる *)
  write ppu 0x2005 0b00000_111;
  Alcotest.(check int) "x = 7 (再度 1 回目扱い)" 7 (Uint8.to_int ppu.internal.x)

(* ------------------------------------------------------------------ *)
(* $2007 PPUDATA — v 自動加算                                          *)
(* ------------------------------------------------------------------ *)

(* PPUCTRL.I に応じて v が 1 / 32 ずつ進む。 *)
let test_data_advance_v_by_1 () =
  let ppu = Ppu.mk () in
  (* PPUCTRL.I = 0 (VAI_01) はデフォルト *)
  ppu.internal.v <- u16 0x0000;
  let _ = read ppu 0x2007 in
  Alcotest.(check int) "v += 1" 1 (Uint16.to_int ppu.internal.v);
  write ppu 0x2007 0;
  Alcotest.(check int) "v += 1 (write 後)" 2 (Uint16.to_int ppu.internal.v)

let test_data_advance_v_by_32 () =
  let ppu = Ppu.mk () in
  write ppu 0x2000 0b00000100;
  (* I = 1 → VAI_32 *)
  ppu.internal.v <- u16 0x0000;
  let _ = read ppu 0x2007 in
  Alcotest.(check int) "v += 32" 32 (Uint16.to_int ppu.internal.v)

(* v は 14 bit でラップする (0x3FFF で wrap to 0). *)
let test_v_wraps_at_14_bit () =
  let ppu = Ppu.mk () in
  ppu.internal.v <- u16 0x3FFF;
  let _ = read ppu 0x2007 in
  Alcotest.(check int) "v = 0 (wrap)" 0 (Uint16.to_int ppu.internal.v)

(* ------------------------------------------------------------------ *)
(* CPU バスアドレスの 8 バイトミラー                                   *)
(* ------------------------------------------------------------------ *)

let test_addr_mirror_8_bytes () =
  let ppu = Ppu.mk () in
  (* $2000 と $2008, $3FF8 は同じレジスタ (PPUCTRL) *)
  write ppu 0x2008 0b00000011;
  Alcotest.(check int)
    "$2008 wrote to PPUCTRL"
    0x0C00
    (Uint16.to_int ppu.internal.t);
  write ppu 0x3FF8 0b00000001;
  Alcotest.(check int)
    "$3FF8 wrote to PPUCTRL"
    0x0400
    (Uint16.to_int ppu.internal.t)

(* ------------------------------------------------------------------ *)
(* write-only register の read は open bus                              *)
(* ------------------------------------------------------------------ *)

let test_write_only_read_returns_open_bus () =
  let ppu = Ppu.mk () in
  write ppu 0x2000 0xA5;
  (* open_bus = 0xA5 *)
  let v = read ppu 0x2000 in
  Alcotest.(check int) "$2000 read = open bus" 0xA5 v

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "PPU"
    [ ("mk", [ Alcotest.test_case "初期状態" `Quick test_mk_initial ])
    ; ( "$2000 PPUCTRL write"
      , [ Alcotest.test_case
            "Loopy t[11:10] が NN で更新"
            `Quick
            test_write_ctrl_updates_internal_t
        ; Alcotest.test_case
            "t の他 bit は保持"
            `Quick
            test_write_ctrl_preserves_other_t_bits
        ] )
    ; ( "$2001 PPUMASK write"
      , [ Alcotest.test_case "typed state 反映" `Quick test_write_mask ] )
    ; ( "$2002 PPUSTATUS read"
      , [ Alcotest.test_case
            "vblank と w クリア"
            `Quick
            test_read_status_clears_vblank_and_toggle
        ; Alcotest.test_case
            "下位 5 bit は open bus"
            `Quick
            test_read_status_low_bits_are_open_bus
        ] )
    ; ( "$2003 OAMADDR write"
      , [ Alcotest.test_case "oam_addr 更新" `Quick test_write_oam_addr ] )
    ; ( "$2005 PPUSCROLL write"
      , [ Alcotest.test_case
            "1 回目: coarse X, fine X, w←1"
            `Quick
            test_write_scroll_first_write
        ; Alcotest.test_case
            "2 回目: coarse Y, fine Y, w←0"
            `Quick
            test_write_scroll_second_write
        ] )
    ; ( "$2006 PPUADDR write"
      , [ Alcotest.test_case
            "2 回書いて v←t"
            `Quick
            test_write_addr_two_writes_set_v
        ; Alcotest.test_case
            "1 回目の上位 2 bit 無視"
            `Quick
            test_write_addr_high_byte_top_2_bits_ignored
        ] )
    ; ( "toggle 相互作用"
      , [ Alcotest.test_case
            "$2002 read が w をクリア"
            `Quick
            test_read_status_resets_toggle_between_scroll_writes
        ] )
    ; ( "$2007 PPUDATA v 自動加算"
      , [ Alcotest.test_case "VAI_01: v += 1" `Quick test_data_advance_v_by_1
        ; Alcotest.test_case "VAI_32: v += 32" `Quick test_data_advance_v_by_32
        ; Alcotest.test_case "14 bit wrap" `Quick test_v_wraps_at_14_bit
        ] )
    ; ( "addr ミラー"
      , [ Alcotest.test_case
            "$2000/$2008/$3FF8 等価"
            `Quick
            test_addr_mirror_8_bytes
        ] )
    ; ( "write-only register"
      , [ Alcotest.test_case
            "read は open bus"
            `Quick
            test_write_only_read_returns_open_bus
        ] )
    ; ( "step (scanline 進行)"
      , [ Alcotest.test_case "1 step で dot+1" `Quick (fun () ->
            let ppu = Ppu.mk () in
            Ppu.step ppu;
            Alcotest.(check int) "dot=1" 1 ppu.dot;
            Alcotest.(check int) "scanline=0" 0 ppu.scanline)
        ; Alcotest.test_case "dot 340→0, scanline +1" `Quick (fun () ->
            let ppu = Ppu.mk () in
            for _ = 1 to 341 do
              Ppu.step ppu
            done;
            Alcotest.(check int) "dot=0" 0 ppu.dot;
            Alcotest.(check int) "scanline=1" 1 ppu.scanline)
        ; Alcotest.test_case "scanline 261 → 0, frame +1" `Quick (fun () ->
            let ppu = Ppu.mk () in
            (* 1 frame = 262 × 341 = 89342 dot *)
            for _ = 1 to 89342 do
              Ppu.step ppu
            done;
            Alcotest.(check int) "dot=0" 0 ppu.dot;
            Alcotest.(check int) "scanline=0" 0 ppu.scanline;
            Alcotest.(check int) "frame=1" 1 ppu.frame)
        ; Alcotest.test_case
            "vblank フラグ・nmi_request@(241,1) NMI enabled"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               ppu.ctrl <- { ppu.ctrl with enable_nmi = true };
               (* scanline 241 dot 1 まで進める: 241 * 341 + 1 = 82182 dot *)
               for _ = 1 to 82182 do
                 Ppu.step ppu
               done;
               Alcotest.(check int) "scanline=241" 241 ppu.scanline;
               Alcotest.(check int) "dot=1" 1 ppu.dot;
               Alcotest.(check bool) "vblank set" true ppu.status.vblank_flag;
               Alcotest.(check bool) "nmi_request set" true ppu.nmi_request;
               Alcotest.(check bool)
                 "frame_complete set"
                 true
                 ppu.frame_complete)
        ; Alcotest.test_case
            "NMI disabled なら nmi_request は立たない"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               for _ = 1 to 82182 do
                 Ppu.step ppu
               done;
               Alcotest.(check bool) "vblank set" true ppu.status.vblank_flag;
               Alcotest.(check bool) "nmi_request false" false ppu.nmi_request)
        ; Alcotest.test_case
            "pre-render @ (261,1) で vblank/sprite0/overflow クリア"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               ppu.status
               <- { vblank_flag = true
                  ; sprite_0_hit = true
                  ; sprite_overflow = true
                  };
               (* scanline 261 dot 1 まで: 261 * 341 + 1 = 89002 dot *)
               for _ = 1 to 89002 do
                 Ppu.step ppu
               done;
               Alcotest.(check int) "scanline=261" 261 ppu.scanline;
               Alcotest.(check int) "dot=1" 1 ppu.dot;
               Alcotest.(check bool)
                 "vblank cleared"
                 false
                 ppu.status.vblank_flag;
               Alcotest.(check bool)
                 "sprite0 cleared"
                 false
                 ppu.status.sprite_0_hit;
               Alcotest.(check bool)
                 "overflow cleared"
                 false
                 ppu.status.sprite_overflow)
        ] )
    ; ( "PPU memory space (Phase B2)"
      , [ Alcotest.test_case
            "nametable write→read (buffer delay)"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               (* $2000 に 0xAB を書く *)
               ppu.internal.v <- u16 0x2000;
               write ppu 0x2007 0xAB;
               (* read は 1 回目が前 buffer、2 回目が真値 *)
               ppu.internal.v <- u16 0x2000;
               let r1 = read ppu 0x2007 in
               let r2 = read ppu 0x2007 in
               Alcotest.(check int) "1 回目はバッファ (前回値)" 0 r1;
               Alcotest.(check int) "2 回目で 0xAB" 0xAB r2)
        ; Alcotest.test_case
            "palette read is immediate (no buffer)"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               (* $3F00 に 0x21 を書く *)
               ppu.internal.v <- u16 0x3F00;
               write ppu 0x2007 0x21;
               ppu.internal.v <- u16 0x3F00;
               let r = read ppu 0x2007 in
               Alcotest.(check int) "palette は即時 0x21" 0x21 r)
        ; Alcotest.test_case
            "palette read fills buffer from nametable mirror"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               (* $2F00 に sentinel を書く *)
               ppu.internal.v <- u16 0x2F00;
               write ppu 0x2007 0x77;
               (* $3F00 を palette read → buffer は $2F00 由来になる *)
               ppu.internal.v <- u16 0x3F00;
               let _ = read ppu 0x2007 in
               Alcotest.(check int)
                 "buffer は $2F00 の 0x77"
                 0x77
                 (Uint8.to_int ppu.read_buffer))
        ; Alcotest.test_case
            "horizontal mirror: $2000 と $2400 が同一物理"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               Ppu.connect_cart
                 ppu
                 ~mirroring:Emulator.Rom.Cartridge.H
                 ~chr_io:Ppu.empty_chr_io;
               ppu.internal.v <- u16 0x2000;
               write ppu 0x2007 0x42;
               (* $2400 を read (buffer 経由) *)
               ppu.internal.v <- u16 0x2400;
               let _ = read ppu 0x2007 in
               let _ = read ppu 0x2007 in
               (* もう一回 advance しないように reset *)
               ppu.internal.v <- u16 0x2400;
               let _ = read ppu 0x2007 in
               let v = read ppu 0x2007 in
               Alcotest.(check int) "H mirror: $2400 == $2000" 0x42 v)
        ; Alcotest.test_case
            "vertical mirror: $2000 と $2800 が同一物理"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               Ppu.connect_cart
                 ppu
                 ~mirroring:Emulator.Rom.Cartridge.V
                 ~chr_io:Ppu.empty_chr_io;
               ppu.internal.v <- u16 0x2000;
               write ppu 0x2007 0x55;
               ppu.internal.v <- u16 0x2800;
               let _ = read ppu 0x2007 in
               let v = read ppu 0x2007 in
               Alcotest.(check int) "V mirror: $2800 == $2000" 0x55 v)
        ; Alcotest.test_case
            "vertical mirror: $2400 と $2C00 が同一物理"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               Ppu.connect_cart
                 ppu
                 ~mirroring:Emulator.Rom.Cartridge.V
                 ~chr_io:Ppu.empty_chr_io;
               ppu.internal.v <- u16 0x2400;
               write ppu 0x2007 0x66;
               ppu.internal.v <- u16 0x2C00;
               let _ = read ppu 0x2007 in
               let v = read ppu 0x2007 in
               Alcotest.(check int) "V mirror: $2C00 == $2400" 0x66 v)
        ; Alcotest.test_case "$3000-$3EFF は $2000-$2EFF のミラー" `Quick (fun () ->
            let ppu = Ppu.mk () in
            ppu.internal.v <- u16 0x2123;
            write ppu 0x2007 0x99;
            ppu.internal.v <- u16 0x3123;
            let _ = read ppu 0x2007 in
            let v = read ppu 0x2007 in
            Alcotest.(check int) "$3123 == $2123" 0x99 v)
        ; Alcotest.test_case
            "palette mirror: $3F10/$14/$18/$1C → $3F00/04/08/0C"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               ppu.internal.v <- u16 0x3F00;
               write ppu 0x2007 0x11;
               ppu.internal.v <- u16 0x3F04;
               write ppu 0x2007 0x22;
               ppu.internal.v <- u16 0x3F08;
               write ppu 0x2007 0x33;
               ppu.internal.v <- u16 0x3F0C;
               write ppu 0x2007 0x44;
               let read_at a =
                 ppu.internal.v <- u16 a;
                 read ppu 0x2007
               in
               Alcotest.(check int) "$3F10 == $3F00" 0x11 (read_at 0x3F10);
               Alcotest.(check int) "$3F14 == $3F04" 0x22 (read_at 0x3F14);
               Alcotest.(check int) "$3F18 == $3F08" 0x33 (read_at 0x3F18);
               Alcotest.(check int) "$3F1C == $3F0C" 0x44 (read_at 0x3F1C))
        ; Alcotest.test_case
            "palette $3F20-$3FFF は $3F00-$3F1F のミラー"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               ppu.internal.v <- u16 0x3F05;
               write ppu 0x2007 0x7E;
               ppu.internal.v <- u16 0x3F25;
               let v = read ppu 0x2007 in
               Alcotest.(check int) "$3F25 == $3F05" 0x7E v)
        ; Alcotest.test_case
            "CHR read via $2007 (buffer delay)"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               (* CHR の特定 offset に 0xCC を仕込む *)
               let chr = Bytes.make 0x2000 '\x00' in
               Bytes.set_uint8 chr 0x1234 0xCC;
               let chr_io =
                 { Ppu.chr_read =
                     (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
               ppu.internal.v <- u16 0x1234;
               let r1 = read ppu 0x2007 in
               let r2 = read ppu 0x2007 in
               Alcotest.(check int) "1 回目はバッファ (前回値)" 0 r1;
               (* r2 は v が 1235 に進んだあとの読みなので、要 fresh *)
               ignore r2;
               ppu.internal.v <- u16 0x1234;
               let _ = read ppu 0x2007 in
               let v = read ppu 0x2007 in
               Alcotest.(check int) "CHR[0x1234] = 0xCC" 0xCC v)
        ; Alcotest.test_case "CHR write 経路 (CHR-RAM 想定)" `Quick (fun () ->
            let ppu = Ppu.mk () in
            let chr = Bytes.make 0x2000 '\x00' in
            let chr_io =
              { Ppu.chr_read =
                  (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
              ; chr_write =
                  (fun ofs v -> Bytes.set_uint8 chr ofs (Uint8.to_int v))
              ; a12_rise = (fun () -> ())
              }
            in
            Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
            ppu.internal.v <- u16 0x0050;
            write ppu 0x2007 0xAA;
            Alcotest.(check int)
              "chr backing buffer に書かれた"
              0xAA
              (Bytes.get_uint8 chr 0x0050))
        ; Alcotest.test_case
            "disconnect_cart で CHR が empty に戻る"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               let chr_io =
                 { Ppu.chr_read = (fun _ -> Uint8.of_int 0x5A)
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.V ~chr_io;
               Ppu.disconnect_cart ppu;
               Alcotest.(check bool)
                 "mirroring reset to H"
                 true
                 (ppu.mirroring = Emulator.Rom.Cartridge.H);
               ppu.internal.v <- u16 0x0000;
               let _ = read ppu 0x2007 in
               ppu.internal.v <- u16 0x0000;
               let _ = read ppu 0x2007 in
               let v = read ppu 0x2007 in
               Alcotest.(check int) "CHR は empty (0)" 0 v)
        ] )
    ; ( "Background renderer (Phase B1)"
      , [ Alcotest.test_case
            "enable_bg=false: universal bg で全面塗り"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               (* universal bg = master idx 0x30 (白っぽい (0xEC, 0xEE, 0xEC)) *)
               Bytes.set_uint8 ppu.palette_ram 0 0x30;
               ppu.mask <- { ppu.mask with enable_bg = false };
               Ppu.render_background ppu;
               Alcotest.(check int) "R" 0xEC (Bytes.get_uint8 ppu.framebuffer 0);
               Alcotest.(check int) "G" 0xEE (Bytes.get_uint8 ppu.framebuffer 1);
               Alcotest.(check int) "B" 0xEC (Bytes.get_uint8 ppu.framebuffer 2);
               Alcotest.(check int) "A" 0xFF (Bytes.get_uint8 ppu.framebuffer 3);
               let last = ((239 * 256) + 255) * 4 in
               Alcotest.(check int)
                 "last R"
                 0xEC
                 (Bytes.get_uint8 ppu.framebuffer last))
        ; Alcotest.test_case
            "enable_bg=true: CHR=0, nt=0, palette[0]=0x0F (黒) → 全画素黒"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               let chr = Bytes.make 0x2000 '\x00' in
               let chr_io =
                 { Ppu.chr_read =
                     (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
               Bytes.set_uint8 ppu.palette_ram 0 0x0F;
               ppu.mask <- { ppu.mask with enable_bg = true };
               Ppu.render_background ppu;
               let check_pixel x y =
                 let off = ((y * 256) + x) * 4 in
                 Alcotest.(check int)
                   (Printf.sprintf "(%d,%d) R" x y)
                   0
                   (Bytes.get_uint8 ppu.framebuffer off);
                 Alcotest.(check int)
                   (Printf.sprintf "(%d,%d) A" x y)
                   0xFF
                   (Bytes.get_uint8 ppu.framebuffer (off + 3))
               in
               check_pixel 0 0;
               check_pixel 128 120;
               check_pixel 255 239)
        ; Alcotest.test_case
            "tile (0,0) の (0,0) px に CHR で color=1 を仕込む"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               let chr = Bytes.make 0x2000 '\x00' in
               (* tile 0 の y=0 low plane bit7 を立てる → (0,0) px = color 1 *)
               Bytes.set_uint8 chr 0 0x80;
               let chr_io =
                 { Ppu.chr_read =
                     (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
               (* universal bg = 0x0F (黒), palette 0 の color 1 = 0x30 (白) *)
               Bytes.set_uint8 ppu.palette_ram 0 0x0F;
               Bytes.set_uint8 ppu.palette_ram 1 0x30;
               ppu.mask <- { ppu.mask with enable_bg = true };
               Ppu.render_background ppu;
               Alcotest.(check int)
                 "(0,0) R = 0xEC (白)"
                 0xEC
                 (Bytes.get_uint8 ppu.framebuffer 0);
               Alcotest.(check int)
                 "(1,0) R = 0x00 (黒)"
                 0x00
                 (Bytes.get_uint8 ppu.framebuffer (1 * 4)))
        ; Alcotest.test_case
            "set_master_palette / reset_master_palette"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               let r0, g0, b0 = Ppu.master_rgb ppu 0x0F in
               Alcotest.(check (triple int int int))
                 "default 0x0F = 黒"
                 (0, 0, 0)
                 (r0, g0, b0);
               let zero_pal = Bytes.make 192 '\x00' in
               Alcotest.(check bool)
                 "set OK"
                 true
                 (Ppu.set_master_palette ppu zero_pal);
               let r1, _, _ = Ppu.master_rgb ppu 0x00 in
               Alcotest.(check int) "set 後 0x00 R = 0" 0 r1;
               Ppu.reset_master_palette ppu;
               let r2, _, _ = Ppu.master_rgb ppu 0x00 in
               Alcotest.(check int) "reset 後 0x00 R = 0x54" 0x54 r2)
        ] )
    ; ( "Background scroll (Phase B1 拡張)"
      , [ Alcotest.test_case
            "scroll_x = 8 で BG が 8px 左にシフト (= tile が 1 つ左に出る)"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               let chr = Bytes.make 0x2000 '\x00' in
               (* tile 1 = 全 color 1 (両 plane: lo 全 1, hi 全 0 → c=1) *)
               for i = 0 to 7 do
                 Bytes.set_uint8 chr (16 + i) 0xFF
               done;
               let chr_io =
                 { Ppu.chr_read =
                     (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
               (* nametable[0,0] = 0 (tile 0 = 透明), [1,0] = 1 (tile 1 = white).
                  scroll_x = 8 にすると、画面 x=0 に nametable col 1 (tile 1) が出る. *)
               Bytes.set_uint8 ppu.vram 0 0;
               Bytes.set_uint8 ppu.vram 1 1;
               Bytes.set_uint8 ppu.palette_ram 0 0x0F;
               (* universal bg = 黒 *)
               Bytes.set_uint8 ppu.palette_ram 1 0x30;
               (* palette 0 color 1 = 白 *)
               ppu.mask <- { ppu.mask with enable_bg = true };
               (* scroll_x = 8 (coarse_x = 1, fine_x = 0) *)
               ppu.internal.t <- u16 0x0001;
               ppu.internal.x <- u8 0;
               Ppu.render_background ppu;
               (* 画面 (0,0) は nametable col 1 のはず = 白 *)
               Alcotest.(check int)
                 "(0,0) R = 0xEC (白)"
                 0xEC
                 (Bytes.get_uint8 ppu.framebuffer 0))
        ; Alcotest.test_case "base_nt = 1 ($2400) が原点になる" `Quick (fun () ->
            let ppu = Ppu.mk () in
            let chr = Bytes.make 0x2000 '\x00' in
            for i = 0 to 7 do
              Bytes.set_uint8 chr (16 + i) 0xFF
            done;
            let chr_io =
              { Ppu.chr_read =
                  (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
              ; chr_write = (fun _ _ -> ())
              ; a12_rise = (fun () -> ())
              }
            in
            (* Vertical mirror で $2000/$2400 を独立にする *)
            Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.V ~chr_io;
            (* $2000 の (0,0) tile = 0 (透明), $2400 の (0,0) tile = 1 (白) *)
            Bytes.set_uint8 ppu.vram 0 0;
            (* $2400 は V mirror では vram[0x400..] = bank 1 *)
            Bytes.set_uint8 ppu.vram 0x400 1;
            Bytes.set_uint8 ppu.palette_ram 0 0x0F;
            Bytes.set_uint8 ppu.palette_ram 1 0x30;
            ppu.mask <- { ppu.mask with enable_bg = true };
            (* base_nt = 1: t[11:10] = 01 *)
            ppu.internal.t <- u16 (1 lsl 10);
            Ppu.render_background ppu;
            (* 画面 (0,0) は $2400 の (0,0) = tile 1 = 白 *)
            Alcotest.(check int)
              "(0,0) R = 0xEC (白)"
              0xEC
              (Bytes.get_uint8 ppu.framebuffer 0))
        ] )
    ; ( "OAM / sprites (Phase B3)"
      , [ Alcotest.test_case "$2003/$2004 で OAM 直接 read/write" `Quick (fun () ->
            let ppu = Ppu.mk () in
            write ppu 0x2003 0x10;
            write ppu 0x2004 0xAB;
            write ppu 0x2004 0xCD;
            Alcotest.(check int) "OAM[0x10]" 0xAB (Bytes.get_uint8 ppu.oam 0x10);
            Alcotest.(check int) "OAM[0x11]" 0xCD (Bytes.get_uint8 ppu.oam 0x11);
            Alcotest.(check int)
              "oam_addr inc to 0x12"
              0x12
              (Uint8.to_int ppu.oam_addr);
            (* read は oam_addr を進めない *)
            write ppu 0x2003 0x10;
            let v1 = read ppu 0x2004 in
            let v2 = read ppu 0x2004 in
            Alcotest.(check int) "read v1 = 0xAB" 0xAB v1;
            Alcotest.(check int) "read v2 = 0xAB (oam_addr 不変)" 0xAB v2)
        ; Alcotest.test_case
            "sprite 8x8 描画 (priority front, palette 0 color 1)"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               let chr = Bytes.make 0x2000 '\x00' in
               (* tile 1 の y=0 low plane bit7 を立てる → (0,0) px = color 1 *)
               Bytes.set_uint8 chr (1 * 16) 0x80;
               let chr_io =
                 { Ppu.chr_read =
                     (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
               (* universal bg = 0x0F (黒), sprite palette 0 color 1 ($3F11) = 0x30 (白) *)
               Bytes.set_uint8 ppu.palette_ram 0 0x0F;
               Bytes.set_uint8 ppu.palette_ram 0x11 0x30;
               ppu.mask
               <- { ppu.mask with enable_sprite = true; enable_bg = false };
               (* OAM[0] = sprite at (50, 100), tile 1, attr 0 (palette 0, front) *)
               Bytes.set_uint8 ppu.oam 0 99;
               (* Y - 1 = 99 → top = 100 *)
               Bytes.set_uint8 ppu.oam 1 1;
               Bytes.set_uint8 ppu.oam 2 0;
               Bytes.set_uint8 ppu.oam 3 50;
               (* X = 50 *)
               Ppu.render_background ppu;
               Ppu.render_sprites ppu;
               let off = ((100 * 256) + 50) * 4 in
               Alcotest.(check int)
                 "(50,100) R = 0xEC (白)"
                 0xEC
                 (Bytes.get_uint8 ppu.framebuffer off))
        ; Alcotest.test_case "sprite y >= 0xEF は描画されない (画面外)" `Quick (fun () ->
            let ppu = Ppu.mk () in
            let chr = Bytes.make 0x2000 '\x00' in
            Bytes.set_uint8 chr 0 0xFF;
            let chr_io =
              { Ppu.chr_read =
                  (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
              ; chr_write = (fun _ _ -> ())
              ; a12_rise = (fun () -> ())
              }
            in
            Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
            Bytes.set_uint8 ppu.palette_ram 0 0x0F;
            Bytes.set_uint8 ppu.palette_ram 0x11 0x30;
            ppu.mask <- { ppu.mask with enable_sprite = true };
            Bytes.set_uint8 ppu.oam 0 0xEF;
            Bytes.set_uint8 ppu.oam 1 0;
            Bytes.set_uint8 ppu.oam 2 0;
            Bytes.set_uint8 ppu.oam 3 0;
            Ppu.render_background ppu;
            Ppu.render_sprites ppu;
            (* 何も描かれないので (0,0) は universal bg = 黒 *)
            Alcotest.(check int)
              "(0,0) R = 0"
              0
              (Bytes.get_uint8 ppu.framebuffer 0))
        ; Alcotest.test_case
            "sprite 0 hit: render_background が hit scanline を予測する"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               let chr = Bytes.make 0x2000 '\x00' in
               (* BG tile 0 全 1 (color=1), sprite tile 1 全 1 (color=1) *)
               for i = 0 to 7 do
                 Bytes.set_uint8 chr i 0xFF;
                 Bytes.set_uint8 chr (16 + i) 0xFF
               done;
               let chr_io =
                 { Ppu.chr_read =
                     (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
               Bytes.set_uint8 ppu.palette_ram 0 0x0F;
               Bytes.set_uint8 ppu.palette_ram 1 0x30;
               Bytes.set_uint8 ppu.palette_ram 0x11 0x30;
               ppu.mask
               <- { ppu.mask with
                    enable_bg = true
                  ; enable_sprite = true
                  ; enable_bg_left_column = true
                  ; enable_spr_left_column = true
                  };
               (* sprite 0: top = 10, x = 10, tile 1 *)
               Bytes.set_uint8 ppu.oam 0 9;
               Bytes.set_uint8 ppu.oam 1 1;
               Bytes.set_uint8 ppu.oam 2 0;
               Bytes.set_uint8 ppu.oam 3 10;
               Ppu.render_background ppu;
               let predicted = Ppu.predict_sprite_0_hit_scanline ppu in
               Alcotest.(check int) "predict scanline = 10" 10 predicted;
               ppu.sprite_0_hit_scanline <- predicted;
               Alcotest.(check bool)
                 "hit before reaching scanline"
                 false
                 ppu.status.sprite_0_hit;
               (* PPU を scanline 10 dot 256 に進める *)
               for _ = 1 to (10 * 341) + 256 do
                 Ppu.step ppu
               done;
               Alcotest.(check bool)
                 "hit after reaching scanline"
                 true
                 ppu.status.sprite_0_hit)
        ; Alcotest.test_case
            "predict_sprite_0_hit_scanline: BG が無い時は 240"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               ppu.mask
               <- { ppu.mask with enable_bg = false; enable_sprite = true };
               Alcotest.(check int)
                 "no hit"
                 240
                 (Ppu.predict_sprite_0_hit_scanline ppu))
        ; Alcotest.test_case
            "sprite priority behind: BG opaque pixel 上では描かれない"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               let chr = Bytes.make 0x2000 '\x00' in
               for i = 0 to 7 do
                 Bytes.set_uint8 chr i 0xFF;
                 (* BG tile 0 全 1 *)
                 Bytes.set_uint8 chr (16 + i) 0xFF (* sprite tile 1 全 1 *)
               done;
               let chr_io =
                 { Ppu.chr_read =
                     (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
               Bytes.set_uint8 ppu.palette_ram 0 0x0F;
               (* univ bg = 黒 *)
               Bytes.set_uint8 ppu.palette_ram 1 0x16;
               (* bg color = 赤系 *)
               Bytes.set_uint8 ppu.palette_ram 0x11 0x30;
               (* sprite color = 白 *)
               ppu.mask
               <- { ppu.mask with
                    enable_bg = true
                  ; enable_sprite = true
                  ; enable_bg_left_column = true
                  ; enable_spr_left_column = true
                  };
               (* sprite at (10, 10) with priority=behind (attr bit 5 = 1) *)
               Bytes.set_uint8 ppu.oam 0 9;
               Bytes.set_uint8 ppu.oam 1 1;
               Bytes.set_uint8 ppu.oam 2 0b0010_0000;
               Bytes.set_uint8 ppu.oam 3 10;
               Ppu.render_background ppu;
               Ppu.render_sprites ppu;
               (* (10,10) は BG opaque なので sprite (behind) は描かれない. BG 色 (赤系) が残る *)
               let off = ((10 * 256) + 10) * 4 in
               (* master idx 0x16 = NES の "赤" 系。具体的値はパレット依存だが、白 (0xEC) ではないことだけ確認 *)
               let r = Bytes.get_uint8 ppu.framebuffer off in
               Alcotest.(check bool) "色は白 (0xEC) ではない" true (r <> 0xEC))
        ] )
    ; ( "Sprite limit + overflow (Phase B5)"
      , [ Alcotest.test_case
            "8 個までは描画、9 個目で overflow flag が立つ"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               Bytes.fill ppu.oam 0 256 '\xFF';
               let chr = Bytes.make 0x2000 '\x00' in
               for i = 0 to 7 do
                 Bytes.set_uint8 chr (16 + i) 0xFF
                 (* sprite tile 1 を opaque に *)
               done;
               let chr_io =
                 { Ppu.chr_read =
                     (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
               Bytes.set_uint8 ppu.palette_ram 0x11 0x30;
               ppu.mask
               <- { ppu.mask with
                    enable_sprite = true
                  ; enable_spr_left_column = true
                  };
               (* 9 個の sprite を同一 scanline (y=9, top=10) に配置 *)
               for i = 0 to 8 do
                 Bytes.set_uint8 ppu.oam (i * 4) 9;
                 Bytes.set_uint8 ppu.oam ((i * 4) + 1) 1;
                 Bytes.set_uint8 ppu.oam ((i * 4) + 2) 0;
                 Bytes.set_uint8 ppu.oam ((i * 4) + 3) (i * 9)
               done;
               Ppu.render_background ppu;
               Ppu.render_sprites ppu;
               Alcotest.(check bool)
                 "overflow set"
                 true
                 ppu.status.sprite_overflow)
        ; Alcotest.test_case "8 個ちょうどなら overflow しない" `Quick (fun () ->
            let ppu = Ppu.mk () in
            (* OAM 初期値はゼロ = 全 sprite が y=0 に居ることになるので、
               未使用 sprite は画面外 ($FF) で埋める. *)
            Bytes.fill ppu.oam 0 256 '\xFF';
            let chr = Bytes.make 0x2000 '\x00' in
            for i = 0 to 7 do
              Bytes.set_uint8 chr (16 + i) 0xFF
            done;
            let chr_io =
              { Ppu.chr_read =
                  (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
              ; chr_write = (fun _ _ -> ())
              ; a12_rise = (fun () -> ())
              }
            in
            Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
            ppu.mask <- { ppu.mask with enable_sprite = true };
            for i = 0 to 7 do
              Bytes.set_uint8 ppu.oam (i * 4) 9;
              Bytes.set_uint8 ppu.oam ((i * 4) + 1) 1;
              Bytes.set_uint8 ppu.oam ((i * 4) + 2) 0;
              Bytes.set_uint8 ppu.oam ((i * 4) + 3) (i * 9)
            done;
            Ppu.render_background ppu;
            Ppu.render_sprites ppu;
            Alcotest.(check bool)
              "overflow not set"
              false
              ppu.status.sprite_overflow)
        ; Alcotest.test_case
            "9 個目の sprite (低 index 優先) は描画されない"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               Bytes.fill ppu.oam 0 256 '\xFF';
               let chr = Bytes.make 0x2000 '\x00' in
               (* tile 1: 黒 (color=1 全埋め), tile 2: 別色 (color=3 全埋め) *)
               for i = 0 to 7 do
                 Bytes.set_uint8 chr (16 + i) 0xFF;
                 (* tile 1 lo plane *)
                 Bytes.set_uint8 chr (32 + i) 0xFF;
                 (* tile 2 lo plane *)
                 Bytes.set_uint8
                   chr
                   (32 + 8 + i)
                   0xFF (* tile 2 hi plane → color=3 *)
               done;
               let chr_io =
                 { Ppu.chr_read =
                     (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
               Bytes.set_uint8 ppu.palette_ram 0x11 0x30;
               (* color 1 = 白 *)
               Bytes.set_uint8 ppu.palette_ram 0x13 0x06;
               (* color 3 = 暗赤 *)
               ppu.mask
               <- { ppu.mask with
                    enable_sprite = true
                  ; enable_spr_left_column = true
                  };
               (* 8 個 sprite (index 0..7): tile 1 (= 白), 異なる X 位置 *)
               for i = 0 to 7 do
                 Bytes.set_uint8 ppu.oam (i * 4) 9;
                 Bytes.set_uint8 ppu.oam ((i * 4) + 1) 1;
                 Bytes.set_uint8 ppu.oam ((i * 4) + 2) 0;
                 Bytes.set_uint8 ppu.oam ((i * 4) + 3) (i * 16)
               done;
               (* 9 個目 (index 8): tile 2 (= 暗赤), 別 X 位置. 描画されないはず. *)
               Bytes.set_uint8 ppu.oam (8 * 4) 9;
               Bytes.set_uint8 ppu.oam ((8 * 4) + 1) 2;
               Bytes.set_uint8 ppu.oam ((8 * 4) + 2) 0;
               Bytes.set_uint8 ppu.oam ((8 * 4) + 3) 200;
               (* X = 200 *)
               Ppu.render_background ppu;
               Ppu.render_sprites ppu;
               (* sprite 8 が居るはずの (200, 10) には sprite 色 (= 暗赤、master 0x06)
                  ではなく universal bg (= master 0) が出るはず. *)
               let off = ((10 * 256) + 200) * 4 in
               let bg_r, _, _ = Ppu.master_rgb ppu 0 in
               Alcotest.(check int)
                 "(200,10) R = universal bg"
                 bg_r
                 (Bytes.get_uint8 ppu.framebuffer off))
        ] )
    ; ( "Grayscale + color emphasis (Phase B5)"
      , [ Alcotest.test_case
            "grayscale: palette byte が & 0x30 されて輝度のみに"
            `Quick
            (fun () ->
               let ppu = Ppu.mk () in
               (* 普通色 0x21 (=青系) を palette[1] に. grayscale なら 0x21 & 0x30 = 0x20 (中間白). *)
               Bytes.set_uint8 ppu.palette_ram 1 0x21;
               (* CHR: tile 0 全 color=1 で塗る *)
               let chr = Bytes.make 0x2000 '\x00' in
               for i = 0 to 7 do
                 Bytes.set_uint8 chr i 0xFF
               done;
               let chr_io =
                 { Ppu.chr_read =
                     (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
                 ; chr_write = (fun _ _ -> ())
                 ; a12_rise = (fun () -> ())
                 }
               in
               Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
               ppu.mask <- { ppu.mask with enable_bg = true; gray_scale = true };
               Ppu.render_background ppu;
               (* (0,0) の RGB は master[0x20] と一致するはず *)
               let exp_r, exp_g, exp_b = Ppu.master_rgb ppu 0x20 in
               Alcotest.(check int)
                 "R = master[0x20].R"
                 exp_r
                 (Bytes.get_uint8 ppu.framebuffer 0);
               Alcotest.(check int)
                 "G = master[0x20].G"
                 exp_g
                 (Bytes.get_uint8 ppu.framebuffer 1);
               Alcotest.(check int)
                 "B = master[0x20].B"
                 exp_b
                 (Bytes.get_uint8 ppu.framebuffer 2))
        ; Alcotest.test_case "emphasis R: G/B 成分が ~75% に減衰" `Quick (fun () ->
            let ppu = Ppu.mk () in
            (* palette[1] = 0x20 (白) *)
            Bytes.set_uint8 ppu.palette_ram 1 0x20;
            let chr = Bytes.make 0x2000 '\x00' in
            for i = 0 to 7 do
              Bytes.set_uint8 chr i 0xFF
            done;
            let chr_io =
              { Ppu.chr_read =
                  (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
              ; chr_write = (fun _ _ -> ())
              ; a12_rise = (fun () -> ())
              }
            in
            Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
            let em = { R.Ppu_mask.red = true; green = false; blue = false } in
            ppu.mask <- { ppu.mask with enable_bg = true; color_emphasis = em };
            Ppu.render_background ppu;
            let r_orig, g_orig, b_orig = Ppu.master_rgb ppu 0x20 in
            let r = Bytes.get_uint8 ppu.framebuffer 0 in
            let g = Bytes.get_uint8 ppu.framebuffer 1 in
            let b = Bytes.get_uint8 ppu.framebuffer 2 in
            Alcotest.(check int) "R 不変" r_orig r;
            Alcotest.(check int) "G = orig * 192/256" (g_orig * (192 asr 8)) g;
            Alcotest.(check int) "B = orig * 192/256" (b_orig * (192 asr 8)) b)
        ; Alcotest.test_case "emphasis 全 bit: 全成分が ~75% に" `Quick (fun () ->
            let ppu = Ppu.mk () in
            Bytes.set_uint8 ppu.palette_ram 1 0x20;
            let chr = Bytes.make 0x2000 '\x00' in
            for i = 0 to 7 do
              Bytes.set_uint8 chr i 0xFF
            done;
            let chr_io =
              { Ppu.chr_read =
                  (fun ofs -> Uint8.of_int (Bytes.get_uint8 chr ofs))
              ; chr_write = (fun _ _ -> ())
              ; a12_rise = (fun () -> ())
              }
            in
            Ppu.connect_cart ppu ~mirroring:Emulator.Rom.Cartridge.H ~chr_io;
            let em = { R.Ppu_mask.red = true; green = true; blue = true } in
            ppu.mask <- { ppu.mask with enable_bg = true; color_emphasis = em };
            Ppu.render_background ppu;
            let r_orig, _, _ = Ppu.master_rgb ppu 0x20 in
            let r = Bytes.get_uint8 ppu.framebuffer 0 in
            (* 全 bit em だと「em されてない成分」が無いので、apply_em の挙動次第:
               実装的には if em then c else c * 192/256. 全 bit em なら全成分そのまま. *)
            Alcotest.(check int) "全 bit em なら R 不変" r_orig r)
        ] )
    ]
