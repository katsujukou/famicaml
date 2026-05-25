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
    ]
