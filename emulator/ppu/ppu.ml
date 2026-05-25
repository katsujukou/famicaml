open Famicaml_common.Nesint

(* サブモジュールを外向きに公開する。ppu.ml が存在することで
   dune の自動生成名前空間が上書きされ、Register / Nametable / Config
   等が外から見えなくなるため明示的に re-export する。 *)
module Register = Register
module Nametable = Nametable
module Pattern_table = Pattern_table
module Palette = Palette

(* ------------------------------------------------------------------ *)
(* Ppu.t — PPU 本体の状態                                              *)
(* ------------------------------------------------------------------ *)

type t =
  { mutable ctrl : Register.Ppu_control.t (* $2000 latch (typed) *)
  ; mutable mask : Register.Ppu_mask.t (* $2001 latch (typed) *)
  ; mutable status : Register.Ppu_status.t (* $2002 latch (typed) *)
  ; internal : Register.Ppu_internal.t (* Loopy v/t/x/w *)
  ; mutable oam_addr : uint8 (* $2003 *)
  ; mutable read_buffer : uint8 (* $2007 の 1 バイト遅延バッファ *)
  ; mutable open_bus : uint8
    (* 直近 CPU データバス値 *)
    (* ----- スキャンライン進行 (Phase A6) ----- *)
  ; mutable dot : int (* 0..340 *)
  ; mutable scanline : int (* 0..261 (261 は pre-render) *)
  ; mutable frame : int (* 起動からのフレーム数 *)
  ; mutable nmi_request : bool
    (** vblank 突入時かつ PPUCTRL.V=1 なら立つ。CPU へ転写されたら
            呼び出し側 (Nes) がクリアする想定。 *)
  ; mutable frame_complete : bool
    (** vblank 突入 (scanline 241 dot 1) に達した瞬間に立つ。
            run_until_frame が観測してクリアする. *)
  }

let mk () =
  { ctrl = Register.Ppu_control.initial ()
  ; mask = Register.Ppu_mask.initial ()
  ; status = Register.Ppu_status.initial ()
  ; internal = Register.Ppu_internal.initial ()
  ; oam_addr = Uint8.zero
  ; read_buffer = Uint8.zero
  ; open_bus = Uint8.zero
  ; dot = 0
  ; scanline = 0
  ; frame = 0
  ; nmi_request = false
  ; frame_complete = false
  }

(* ------------------------------------------------------------------ *)
(* 各レジスタの CPU バス側ハンドラ                                       *)
(*                                                                     *)
(* 仕様は NesDev wiki "PPU registers" / "PPU scrolling" を参照。        *)
(* ------------------------------------------------------------------ *)

(** $2000 write (PPUCTRL):
    - typed state を新しいバイトで丸ごと差し替え
    - Loopy: t[11..10] <- byte[1..0] (nametable select) *)
let write_ctrl ppu byte =
  ppu.ctrl <- Register.Ppu_control.of_uint8 byte;
  let li = ppu.internal in
  let nn = (Uint8.to_int byte land 0b11) lsl 10 in
  let cleared = Uint16.logand li.t (Uint16.of_int 0xF3FF) in
  li.t <- Uint16.logor cleared (Uint16.of_int nn)

(** $2001 write (PPUMASK): typed state を差し替えるだけ。 *)
let write_mask ppu byte = ppu.mask <- Register.Ppu_mask.of_uint8 byte

(** $2002 read (PPUSTATUS):
    - 上位 3 bit (V/S/O) を返す
    - 下位 5 bit は open bus (直近データバス値の下位 5 bit)
    - 副作用: vblank フラグをクリア、write toggle (w) を 0 にする *)
let read_status ppu =
  let upper =
    Uint8.logand (Register.Ppu_status.to_uint8 ppu.status) (Uint8.of_int 0xE0)
  in
  let lower = Uint8.logand ppu.open_bus (Uint8.of_int 0x1F) in
  let byte = Uint8.logor upper lower in
  ppu.status <- { ppu.status with vblank_flag = false };
  ppu.internal.w <- false;
  byte

(** $2003 write (OAMADDR): OAM 書き込みアドレスを設定。 *)
let write_oam_addr ppu byte = ppu.oam_addr <- byte

(** $2004 read (OAMDATA): OAM[oam_addr] を返す。
    OAM 未実装なので open bus を返すスタブ。 *)
let read_oam_data ppu = ppu.open_bus

(** $2004 write (OAMDATA): OAM[oam_addr++] に書き込む。
    OAM 未実装なので no-op スタブ。 *)
let write_oam_data ppu _byte = ppu.oam_addr <- Uint8.succ ppu.oam_addr

(** $2005 write (PPUSCROLL): 2 回書き込みで scroll を設定。
    1 回目 (w=0): coarse X (byte[7..3]) → t[4..0], fine X (byte[2..0]) → x
    2 回目 (w=1): coarse Y (byte[7..3]) → t[9..5], fine Y (byte[2..0]) → t[14..12] *)
let write_scroll ppu byte =
  let d = Uint8.to_int byte in
  let li = ppu.internal in
  if not li.w
  then (
    let coarse_x = d lsr 3 in
    let cleared = Uint16.logand li.t (Uint16.of_int 0xFFE0) in
    li.t <- Uint16.logor cleared (Uint16.of_int coarse_x);
    li.x <- Uint8.of_int (d land 0b111);
    li.w <- true)
  else (
    let coarse_y = d lsr 3 in
    let fine_y = d land 0b111 in
    let cleared = Uint16.logand li.t (Uint16.of_int 0x0C1F) in
    let payload = (coarse_y lsl 5) lor (fine_y lsl 12) in
    li.t <- Uint16.logor cleared (Uint16.of_int payload);
    li.w <- false)

(** $2006 write (PPUADDR): 2 回書き込みで VRAM アドレスを設定。
    1 回目 (w=0): 上位 6 bit (byte[5..0]) → t[13..8], bit 14 はクリア
                  (アドレスは 14 bit なので bit 14 の上位 2 bit は無視される)
    2 回目 (w=1): 下位 8 bit (byte[7..0]) → t[7..0]、その後 v ← t *)
let write_addr ppu byte =
  let d = Uint8.to_int byte in
  let li = ppu.internal in
  if not li.w
  then (
    let hi = d land 0x3F in
    let cleared = Uint16.logand li.t (Uint16.of_int 0x00FF) in
    li.t <- Uint16.logor cleared (Uint16.of_int (hi lsl 8));
    li.w <- true)
  else (
    let cleared = Uint16.logand li.t (Uint16.of_int 0xFF00) in
    li.t <- Uint16.logor cleared (Uint16.of_int d);
    li.v <- li.t;
    li.w <- false)

(** $2007 アクセス後の v 自動加算量。PPUCTRL.I に依存。 *)
let v_increment ppu =
  match ppu.ctrl.addr_incr with
  | Register.Ppu_control.VAI_01 -> 1
  | Register.Ppu_control.VAI_32 -> 32

let advance_v ppu =
  let next = Uint16.add ppu.internal.v (Uint16.of_int (v_increment ppu)) in
  ppu.internal.v <- Uint16.logand next (Uint16.of_int 0x3FFF)

(** $2007 read (PPUDATA): バッファ経由の 1 バイト遅延読み + v 自動加算。
    VRAM 未実装なので read_buffer のみ更新する暫定スタブ。 *)
let read_data ppu =
  let returned = ppu.read_buffer in
  ppu.read_buffer <- Uint8.zero;
  (* TODO: VRAM[v] を読んで read_buffer に格納 *)
  advance_v ppu;
  returned

(** $2007 write (PPUDATA): VRAM[v] への書き込み + v 自動加算。
    VRAM 未実装なので v 加算のみ。 *)
let write_data ppu _byte =
  (* TODO: VRAM[v] <- byte *)
  advance_v ppu

(* ------------------------------------------------------------------ *)
(* CPU バス入口                                                         *)
(*                                                                     *)
(* CPU バス $2000-$3FFF への read/write はすべてここを通る。            *)
(* addr land 0x07 で 8 つのレジスタへ dispatch する (8 バイトミラー)。  *)
(* ------------------------------------------------------------------ *)

let cpu_read ppu (addr : uint16) : uint8 =
  let reg = Uint16.to_int addr land 0x07 in
  let byte =
    match reg with
    | 2 -> read_status ppu
    | 4 -> read_oam_data ppu
    | 7 -> read_data ppu
    | _ ->
      (* $2000 / $2001 / $2003 / $2005 / $2006 は write-only。
         実機の read は open bus を返す。 *)
      ppu.open_bus
  in
  ppu.open_bus <- byte;
  byte

let cpu_write ppu (addr : uint16) (byte : uint8) : unit =
  (* どんなレジスタ宛の write でもデータバスに値が乗る *)
  ppu.open_bus <- byte;
  let reg = Uint16.to_int addr land 0x07 in
  match reg with
  | 0 -> write_ctrl ppu byte
  | 1 -> write_mask ppu byte
  | 3 -> write_oam_addr ppu byte
  | 4 -> write_oam_data ppu byte
  | 5 -> write_scroll ppu byte
  | 6 -> write_addr ppu byte
  | 7 -> write_data ppu byte
  | _ -> ()
(* $2002 への write は無視 (open_bus 更新は冒頭で済み) *)

(* ------------------------------------------------------------------ *)
(* スキャンライン進行 (Phase A6)                                        *)
(*                                                                     *)
(* NES PPU は 262 scanline × 341 dot = 89342 dot/frame で進む。        *)
(*   scanline 0..239: 可視 (rendering)                                  *)
(*   scanline 240:    post-render (idle)                                *)
(*   scanline 241:    vblank 開始 (dot 1 で vblank flag セット)         *)
(*   scanline 241..260: vblank                                          *)
(*   scanline 261:    pre-render (dot 1 で vblank/sprite0/overflow クリア) *)
(*                                                                     *)
(* 描画ピクセルの生成はまだ実装しない (Phase B 以降)。                  *)
(* ------------------------------------------------------------------ *)

(** PPU を 1 dot 進める。タイミングイベント (vblank 突入・終了) を発火する。 *)
let step (ppu : t) : unit =
  (* (1) dot/scanline 進行 *)
  let next_dot = ppu.dot + 1 in
  if next_dot < 341
  then ppu.dot <- next_dot
  else (
    ppu.dot <- 0;
    let next_sl = ppu.scanline + 1 in
    if next_sl < 262
    then ppu.scanline <- next_sl
    else (
      ppu.scanline <- 0;
      ppu.frame <- ppu.frame + 1));
  (* (2) edge events @ new position *)
  if ppu.scanline = 241 && ppu.dot = 1
  then (
    ppu.status <- { ppu.status with vblank_flag = true };
    if ppu.ctrl.enable_nmi then ppu.nmi_request <- true;
    ppu.frame_complete <- true);
  if ppu.scanline = 261 && ppu.dot = 1
  then
    ppu.status
    <- { vblank_flag = false; sprite_0_hit = false; sprite_overflow = false }
