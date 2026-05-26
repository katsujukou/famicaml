open Famicaml_common.Nesint

(* サブモジュールを外向きに公開する。ppu.ml が存在することで
   dune の自動生成名前空間が上書きされ、Register / Nametable / Config
   等が外から見えなくなるため明示的に re-export する。 *)
module Register = Register
module Nametable = Nametable
module Pattern_table = Pattern_table
module Palette = Palette

(* ------------------------------------------------------------------ *)
(* CHR バス入出力 (cartridge 由来) を抽象化するインターフェース。      *)
(*                                                                     *)
(* PPU 内部からは「pattern table 領域 ($0000-$1FFF) の read/write」 *)
(* としてしか見えない。bank 切替やミラー、CHR-ROM/RAM の区別は       *)
(* 注入された関数側 (= mapper) の責務. *)
(* ------------------------------------------------------------------ *)

type chr_io =
  { chr_read : int -> uint8
  ; chr_write : int -> uint8 -> unit
  ; a12_rise : unit -> unit
    (** PPU rendering 中の A12 (= PPU bus address bit 12) 立ち上がりを
        mapper に通知するコールバック. MMC3 の scanline IRQ counter は
        これで decrement される. NROM 等は no-op. *)
  }

(** カートリッジ未接続時の CHR バス. read=0, write=捨て, a12_rise=no-op. *)
let empty_chr_io : chr_io =
  { chr_read = (fun _ -> Uint8.zero)
  ; chr_write = (fun _ _ -> ())
  ; a12_rise = (fun () -> ())
  }

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
    (* ----- PPU メモリ空間 (Phase B2) -----
       $0000-$1FFF: pattern table (chr_io 経由で cartridge へ)
       $2000-$2FFF: nametable RAM (2KB 物理、mirroring に従って配置)
       $3000-$3EFF: $2000-$2EFF のミラー
       $3F00-$3F1F: palette RAM (32 byte)
       $3F20-$3FFF: $3F00-$3F1F のミラー *)
  ; vram : Bytes.t (** 2KB 物理 nametable RAM. *)
  ; palette_ram : Bytes.t (** 32 byte palette RAM. *)
  ; mutable mirroring : Rom.Cartridge.mirror
  ; mutable chr_io : chr_io (* ----- Background renderer (Phase B1) ----- *)
  ; framebuffer : Bytes.t
    (** 256×240 × 4 byte RGBA. vblank 突入時に
            render_background が書き込む. *)
  ; bg_mask : Bytes.t
    (** 256×240 byte. render_background が各 pixel の
            BG opacity を記録 (0 = transparent, 1 = opaque).
            render_sprites の priority 判定と sprite 0 hit に使う. *)
  ; mutable sprite_0_hit_scanline : int
    (** vblank で render_background が予測した「次フレームの visible scanline 中
            で sprite 0 と BG が最初に重なる scanline」 (0..239)。240 = no hit。
            step で scanline がここに達した瞬間に status.sprite_0_hit を立てる. *)
  ; mutable initial_scroll : (int * int) option
    (** Visible scanline 0 開始時 (scanline 0 dot 0) にスナップした (t, fine_x).
            render_background で「scanline 0 の scroll」として使う。
            None = step がまだ走っていない (boot/test). *)
  ; scroll_log : int array
    (** Visible 中の $2000/$2005/$2006 書き換えを (scanline, t, x) で記録.
            scroll_log[3i] = scanline、scroll_log[3i+1] = t、scroll_log[3i+2] = x。
            mid-frame split scroll (sprite 0 hit でステータスバー固定する SMB 型)
            を per-scanline 精度で扱うため. *)
  ; mutable scroll_log_n : int
  ; mutable master_palette : Palette.t
    (** NES master palette (64 色 × RGB). UI から差し替え可能. *)
  ; oam : Bytes.t (* ----- スキャンライン進行 (Phase A6) ----- *)
    (** Sprite 用 256 byte OAM (64 sprite × 4 byte: Y/tile/attr/X). *)
  ; mutable dot : int (* 0..340 *)
  ; mutable scanline : int (* 0..261 (261 は pre-render) *)
  ; mutable frame : int (* 起動からのフレーム数 *)
  ; mutable nmi_request : bool
    (** vblank 突入時かつ PPUCTRL.V=1 なら立つ。CPU へ転写されたら
            呼び出し側 (Nes) がクリアする想定。 *)
  ; mutable frame_complete : bool
    (** vblank 突入 (scanline 241 dot 1) に達した瞬間に立つ。
            run_until_frame が観測してクリアする. *)
    (* ----- BG fetch pipeline (Phase B6: Mesen-style per-pixel) -----
       Visible/pre-render scanline の dot 1-256 + 321-336 で 8 dot 1 周期の
       fetch cycle を回す:
         dot %8 = 1-2: NT byte fetch  → nt_latch
         dot %8 = 3-4: AT byte fetch  → at_latch
         dot %8 = 5-6: PT lo byte     → pt_lo_latch
         dot %8 = 7-8: PT hi byte     → pt_hi_latch + reload shift regs
       BG pixel は bg_shift_{lo,hi} を fine_x で選んで出力. *)
  ; mutable nt_latch : int
  ; mutable at_latch : int
  ; mutable pt_lo_latch : int
  ; mutable pt_hi_latch : int
  ; mutable bg_shift_lo : int (* 16-bit shift register *)
  ; mutable bg_shift_hi : int
  ; mutable at_shift_lo : int (* 8-bit shift register, replicated bit *)
  ; mutable at_shift_hi : int
  ; mutable at_next_lo : int (* 次タイルの attribute bit (reload 時に shift reg に拡散) *)
  ; mutable at_next_hi : int
  ; palette_r_cache : int array (** Lazy cache: palette + grayscale + emphasis 適用済み (32 entries). *)
  ; palette_g_cache : int array
  ; palette_b_cache : int array
  ; mutable palette_cache_dirty : bool
    (** palette_ram / PPUMASK の書き換えで true. draw 時に再計算→false. *)
    (* ----- Sprite pipeline (Phase B7) -----
       各 scanline 終了時に "次 scanline 用の 8 sprites" を eval + pattern fetch して
       下記配列に積む. 次 scanline の dot 1-256 で各 dot ごとに 8 個を x 範囲走査. *)
  ; sprite_pattern_lo : int array (** 8 entries *)
  ; sprite_pattern_hi : int array
  ; sprite_x : int array
  ; sprite_attr : int array
  ; sprite_is_zero : bool array
  ; mutable sprite_count : int (* 0..8 *)
    (** Per-scanline resolved sprite line. 256 entries each.
        sprite_line_color[x] = -1 (transparent) or 16..31 (sprite palette cache idx).
        scanline 開始時に resolve_sprite_line で埋め、draw_sprite_pixel は lookup. *)
  ; sprite_line_color : int array
  ; sprite_line_behind : bool array
  ; sprite_line_is_zero : bool array
  }

let mk () =
  { ctrl = Register.Ppu_control.initial ()
  ; mask = Register.Ppu_mask.initial ()
  ; status = Register.Ppu_status.initial ()
  ; internal = Register.Ppu_internal.initial ()
  ; oam_addr = Uint8.zero
  ; read_buffer = Uint8.zero
  ; open_bus = Uint8.zero
  ; vram = Bytes.make 0x0800 '\x00'
  ; palette_ram = Bytes.make 0x20 '\x00'
  ; mirroring = Rom.Cartridge.H
  ; chr_io = empty_chr_io
  ; framebuffer = Bytes.make (256 * 240 * 4) '\x00'
  ; bg_mask = Bytes.make (256 * 240) '\x00'
  ; sprite_0_hit_scanline = 240
  ; initial_scroll = None
  ; scroll_log = Array.make (3 * 256) 0
  ; scroll_log_n = 0
  ; master_palette = Palette.default ()
  ; oam = Bytes.make 256 '\x00'
  ; dot = 0
  ; scanline = 0
  ; frame = 0
  ; nmi_request = false
  ; frame_complete = false
  ; nt_latch = 0
  ; at_latch = 0
  ; pt_lo_latch = 0
  ; pt_hi_latch = 0
  ; bg_shift_lo = 0
  ; bg_shift_hi = 0
  ; at_shift_lo = 0
  ; at_shift_hi = 0
  ; at_next_lo = 0
  ; at_next_hi = 0
  ; palette_r_cache = Array.make 32 0
  ; palette_g_cache = Array.make 32 0
  ; palette_b_cache = Array.make 32 0
  ; palette_cache_dirty = true
  ; sprite_pattern_lo = Array.make 8 0
  ; sprite_pattern_hi = Array.make 8 0
  ; sprite_x = Array.make 8 0
  ; sprite_attr = Array.make 8 0
  ; sprite_is_zero = Array.make 8 false
  ; sprite_count = 0
  ; sprite_line_color = Array.make 256 (-1)
  ; sprite_line_behind = Array.make 256 false
  ; sprite_line_is_zero = Array.make 256 false
  }

(** カートリッジ接続時に mirroring と CHR バスを注入する。 *)
let connect_cart
      (ppu : t)
      ~(mirroring : Rom.Cartridge.mirror)
      ~(chr_io : chr_io)
  : unit
  =
  ppu.mirroring <- mirroring;
  ppu.chr_io <- chr_io

(** カートリッジ取り外し。mirroring/chr_io はデフォルトに戻す。
    vram / palette_ram の中身は保持する (実機の SRAM ライク振る舞い). *)
let disconnect_cart (ppu : t) : unit =
  ppu.mirroring <- Rom.Cartridge.H;
  ppu.chr_io <- empty_chr_io

(** Mirroring を動的に変更する. MMC1 等の bank-switching mapper が
    control register write 時に呼ぶ. *)
let set_mirroring (ppu : t) (m : Rom.Cartridge.mirror) : unit =
  ppu.mirroring <- m

(* ------------------------------------------------------------------ *)
(* Master palette アクセス (UI 連携用)                                  *)
(* ------------------------------------------------------------------ *)

(** Master palette を .pal 形式 (192 byte) で差し替える。
    サイズ不正なら false. *)
let set_master_palette (ppu : t) (b : Bytes.t) : bool =
  match Palette.of_pal_bytes b with
  | Ok p ->
    ppu.master_palette <- p;
    true
  | Error _ -> false

(** Master palette を .pal 形式 (192 byte) で取得する (新規 bytes). *)
let get_master_palette (ppu : t) : Bytes.t =
  Palette.to_pal_bytes ppu.master_palette

(** Master palette を内蔵デフォルトに戻す. *)
let reset_master_palette (ppu : t) : unit =
  ppu.master_palette <- Palette.default ()

(** Master palette の idx 番目 (0..63) に RGB を書く. 範囲外は無視. *)
let set_master_color (ppu : t) (idx : int) ~(r : int) ~(g : int) ~(b : int)
  : unit
  =
  if idx >= 0 && idx < 64 then Palette.set_color ppu.master_palette idx ~r ~g ~b

(* ------------------------------------------------------------------ *)
(* PPU メモリ空間アドレッシング                                         *)
(* ------------------------------------------------------------------ *)

(** Nametable $2000-$2FFF のアドレスを物理 vram (2KB) のオフセットに
    変換する。$3000-$3EFF は呼び出し側で $2000-$2EFF にミラー済み前提.

    - Vertical mirror: 左右並び(0,1,0,1) → 上下がミラー
    - Horizontal mirror: 上下並び(0,0,1,1) → 左右がミラー
    - One_screen_lo: 全 NT が vram bank 0 ($0000-$03FF)
    - One_screen_hi: 全 NT が vram bank 1 ($0400-$07FF) *)
let nt_offset ~(mirroring : Rom.Cartridge.mirror) (addr : int) : int =
  let a = addr land 0x0FFF in
  let nt = a lsr 10 in
  let ofs = a land 0x03FF in
  let bank =
    match mirroring with
    | Rom.Cartridge.V -> nt land 1
    | Rom.Cartridge.H -> nt lsr 1
    | Rom.Cartridge.One_screen_lo -> 0
    | Rom.Cartridge.One_screen_hi -> 1
  in
  (bank * 0x0400) + ofs

(** Palette RAM のアドレスを物理 palette_ram (32 byte) のオフセットに
    変換する。$3F10/$14/$18/$1C は $3F00/$04/$08/$0C のミラー
    (sprite palette の universal background entry). *)
let palette_offset (addr : int) : int =
  let a = addr land 0x1F in
  match a with
  | 0x10 | 0x14 | 0x18 | 0x1C -> a - 0x10
  | _ -> a

(** PPU メモリ空間からの read ($0000-$3FFF, 上位 2 bit は無視). *)
let ppu_bus_read (ppu : t) (addr : int) : uint8 =
  let a = addr land 0x3FFF in
  if a < 0x2000
  then ppu.chr_io.chr_read a
  else if a < 0x3F00
  then (
    let nt_a = a land 0x2FFF in
    Uint8.of_int
      (Bytes.get_uint8 ppu.vram (nt_offset ~mirroring:ppu.mirroring nt_a)))
  else Uint8.of_int (Bytes.get_uint8 ppu.palette_ram (palette_offset a))

(** PPU メモリ空間への write. *)
let ppu_bus_write (ppu : t) (addr : int) (byte : uint8) : unit =
  let a = addr land 0x3FFF in
  if a < 0x2000
  then ppu.chr_io.chr_write a byte
  else if a < 0x3F00
  then (
    let nt_a = a land 0x2FFF in
    Bytes.set_uint8
      ppu.vram
      (nt_offset ~mirroring:ppu.mirroring nt_a)
      (Uint8.to_int byte))
  else (
    Bytes.set_uint8 ppu.palette_ram (palette_offset a) (Uint8.to_int byte);
    ppu.palette_cache_dirty <- true)

(* ------------------------------------------------------------------ *)
(* Scroll log (Phase B1+: mid-frame split scroll 対応)                  *)
(*                                                                     *)
(* CPU が $2000/$2005/$2006 で t/x を書き換えるたびに現 PPU 位置と     *)
(* 新 t/x を記録する. render_background が visible scanline ごとに      *)
(* 適切な scroll を引き当てる. *)
(* ------------------------------------------------------------------ *)

let log_scroll_change (ppu : t) : unit =
  let n = ppu.scroll_log_n in
  let cap = Array.length ppu.scroll_log / 3 in
  if n < cap
  then (
    let off = n * 3 in
    Array.unsafe_set ppu.scroll_log off ppu.scanline;
    Array.unsafe_set ppu.scroll_log (off + 1) (Uint16.to_int ppu.internal.t);
    Array.unsafe_set ppu.scroll_log (off + 2) (Uint8.to_int ppu.internal.x);
    ppu.scroll_log_n <- n + 1)

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
  li.t <- Uint16.logor cleared (Uint16.of_int nn);
  log_scroll_change ppu

(** $2001 write (PPUMASK): typed state を差し替えるだけ。 *)
let write_mask ppu byte =
  ppu.mask <- Register.Ppu_mask.of_uint8 byte;
  (* grayscale / emphasis が変わると cache 内容も変わる. *)
  ppu.palette_cache_dirty <- true

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

(** $2004 read (OAMDATA): OAM[oam_addr] を返す。oam_addr は inc されない. *)
let read_oam_data ppu =
  Uint8.of_int (Bytes.get_uint8 ppu.oam (Uint8.to_int ppu.oam_addr))

(** $2004 write (OAMDATA): OAM[oam_addr] に書き込んで oam_addr++. *)
let write_oam_data ppu byte =
  Bytes.set_uint8 ppu.oam (Uint8.to_int ppu.oam_addr) (Uint8.to_int byte);
  ppu.oam_addr <- Uint8.succ ppu.oam_addr

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
    li.w <- false);
  log_scroll_change ppu

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
    li.w <- false);
  log_scroll_change ppu

(** $2007 アクセス後の v 自動加算量。PPUCTRL.I に依存。 *)
let v_increment ppu =
  match ppu.ctrl.addr_incr with
  | Register.Ppu_control.VAI_01 -> 1
  | Register.Ppu_control.VAI_32 -> 32

let advance_v ppu =
  let next = Uint16.add ppu.internal.v (Uint16.of_int (v_increment ppu)) in
  ppu.internal.v <- Uint16.logand next (Uint16.of_int 0x3FFF)

(** $2007 read (PPUDATA): NesDev 仕様準拠の遅延 read + v 自動加算.

    - v < $3F00 (pattern table / nametable): 1 バイト遅延.
      前回 buffer の値を返し、buffer を VRAM[v] で更新する.
    - v >= $3F00 (palette): 即時 VRAM[v] を返す.
      buffer は同時に nametable mirror (v - $1000) から更新される
      (= $2F00-$2FFF の値が入る). *)
let read_data ppu =
  let v = Uint16.to_int ppu.internal.v land 0x3FFF in
  let returned =
    if v < 0x3F00
    then (
      let prev = ppu.read_buffer in
      ppu.read_buffer <- ppu_bus_read ppu v;
      prev)
    else (
      let immediate = ppu_bus_read ppu v in
      (* buffer には nametable mirror ($2F00-$2FFF) の値が入る *)
      ppu.read_buffer <- ppu_bus_read ppu (v - 0x1000);
      immediate)
  in
  advance_v ppu;
  returned

(** $2007 write (PPUDATA): VRAM[v] への書き込み + v 自動加算. *)
let write_data ppu byte =
  let v = Uint16.to_int ppu.internal.v in
  ppu_bus_write ppu v byte;
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

(* ------------------------------------------------------------------ *)
(* Background renderer (Phase B1)                                       *)
(*                                                                     *)
(* 簡易版: scroll 無視、PPUCTRL.NN が指す nametable 1 枚と             *)
(* PPUCTRL.bg_alignment が指す pattern table を使って 256×240 px の    *)
(* frame buffer (RGBA8) を生成する。enable_bg = false なら universal   *)
(* bg ($3F00) で全面塗り。本実装は per-frame (= vblank 突入時にまとめて *)
(* 呼ぶ) で、scanline-based ではない。                                 *)
(* ------------------------------------------------------------------ *)

let nt_base_addr (ppu : t) : int =
  match ppu.ctrl.base_nmtbl with
  | Nametable.Nmtbl_2000 -> 0x2000
  | Nametable.Nmtbl_2400 -> 0x2400
  | Nametable.Nmtbl_2800 -> 0x2800
  | Nametable.Nmtbl_2C00 -> 0x2C00

let bg_pattern_base (ppu : t) : int =
  match ppu.ctrl.bg_alignment with
  | Register.Ppu_control.L -> 0x0000
  | Register.Ppu_control.R -> 0x1000

(** Master palette の master_idx 番から RGB を取り出す. *)
let master_rgb (ppu : t) (master_idx : int) : int * int * int =
  Palette.color ppu.master_palette (master_idx land 0x3F)

let put_pixel (ppu : t) ~(x : int) ~(y : int) ~(r : int) ~(g : int) ~(b : int)
  : unit
  =
  let off = ((y * 256) + x) * 4 in
  Bytes.set_uint8 ppu.framebuffer off r;
  Bytes.set_uint8 ppu.framebuffer (off + 1) g;
  Bytes.set_uint8 ppu.framebuffer (off + 2) b;
  Bytes.set_uint8 ppu.framebuffer (off + 3) 0xFF

let fill_framebuffer (ppu : t) ~(r : int) ~(g : int) ~(b : int) : unit =
  for y = 0 to 239 do
    for x = 0 to 255 do
      put_pixel ppu ~x ~y ~r ~g ~b
    done
  done

let clear_bg_mask (ppu : t) : unit = Bytes.fill ppu.bg_mask 0 (256 * 240) '\x00'

(* ================================================================== *)
(* Per-pixel BG fetch pipeline (Phase B6: Mesen-style)                 *)
(*                                                                     *)
(* Loopy v register layout:                                            *)
(*   yyy NN YYYYY XXXXX                                                *)
(*   ||| || ||||| +++++-- coarse X                                     *)
(*   ||| || +++++-------- coarse Y                                     *)
(*   ||| ++--------------  NT select (NT_x, NT_y)                      *)
(*   +++------------------ fine Y                                      *)
(*                                                                     *)
(* Visible / pre-render scanline 中の dot timing:                      *)
(*   1-256, 321-336: 8 dot 周期で NT/AT/PT_lo/PT_hi fetch              *)
(*   256:            v: Y increment                                    *)
(*   257:            v: horizontal copy from t                          *)
(*   280-304:        v: vertical copy from t (pre-render のみ)         *)
(*   每 dot:          BG shift register を 1 bit 左 shift              *)
(*                                                                     *)
(* DrawPixel は visible scanline (0..239) の dot 1-256 で 1 pixel 出力.*)
(* ================================================================== *)

(* v register operations (NESdev wiki "PPU scrolling") *)
let inc_coarse_x (ppu : t) : unit =
  let v = Uint16.to_int ppu.internal.v in
  let v =
    if v land 0x001F = 31
    then (v land lnot 0x001F) lxor 0x0400 (* coarse X=0, switch H NT *)
    else v + 1
  in
  ppu.internal.v <- Uint16.of_int (v land 0x7FFF)

let inc_y (ppu : t) : unit =
  let v = Uint16.to_int ppu.internal.v in
  let v =
    if v land 0x7000 <> 0x7000
    then v + 0x1000 (* fine Y < 7: increment fine Y *)
    else (
      let v = v land lnot 0x7000 in
      let y = (v land 0x03E0) lsr 5 in
      let v, y =
        if y = 29
        then (v lxor 0x0800, 0) (* coarse Y=0, switch V NT *)
        else if y = 31
        then (v, 0) (* coarse Y=0, no NT switch *)
        else (v, y + 1)
      in
      (v land lnot 0x03E0) lor (y lsl 5))
  in
  ppu.internal.v <- Uint16.of_int (v land 0x7FFF)

let copy_horizontal (ppu : t) : unit =
  (* v[0,4..10] = t[0,4..10] = NT_x (bit 10) + coarse X (bits 0-4) *)
  let v = Uint16.to_int ppu.internal.v in
  let t = Uint16.to_int ppu.internal.t in
  let mask = 0x041F in
  ppu.internal.v <- Uint16.of_int ((v land lnot mask) lor (t land mask))

let copy_vertical (ppu : t) : unit =
  (* v[5..9,11..14] = t[5..9,11..14] *)
  let v = Uint16.to_int ppu.internal.v in
  let t = Uint16.to_int ppu.internal.t in
  let mask = 0x7BE0 in
  ppu.internal.v <- Uint16.of_int ((v land lnot mask) lor (t land mask))

(* BG fetch helpers *)
let fetch_nt_byte (ppu : t) : int =
  let v = Uint16.to_int ppu.internal.v in
  let addr = 0x2000 lor (v land 0x0FFF) in
  Uint8.to_int (ppu_bus_read ppu addr)

let fetch_at_byte (ppu : t) : int =
  let v = Uint16.to_int ppu.internal.v in
  let nt = v land 0x0C00 in
  let coarse_x = v land 0x001F in
  let coarse_y = (v land 0x03E0) lsr 5 in
  let addr = 0x23C0 lor nt lor ((coarse_y / 4) * 8) lor (coarse_x / 4) in
  let at = Uint8.to_int (ppu_bus_read ppu addr) in
  let shift = ((coarse_y land 2) lsl 1) lor (coarse_x land 2) in
  (at lsr shift) land 0b11

let fetch_pt_lo_byte (ppu : t) : int =
  let v = Uint16.to_int ppu.internal.v in
  let fine_y = (v lsr 12) land 0x07 in
  let base = bg_pattern_base ppu in
  let addr = base + (ppu.nt_latch * 16) + fine_y in
  Uint8.to_int (ppu_bus_read ppu addr)

let fetch_pt_hi_byte (ppu : t) : int =
  let v = Uint16.to_int ppu.internal.v in
  let fine_y = (v lsr 12) land 0x07 in
  let base = bg_pattern_base ppu in
  let addr = base + (ppu.nt_latch * 16) + fine_y + 8 in
  Uint8.to_int (ppu_bus_read ppu addr)

let reload_bg_shifts (ppu : t) : unit =
  ppu.bg_shift_lo <- (ppu.bg_shift_lo land 0xFF00) lor ppu.pt_lo_latch;
  ppu.bg_shift_hi <- (ppu.bg_shift_hi land 0xFF00) lor ppu.pt_hi_latch;
  ppu.at_next_lo <- ppu.at_latch land 1;
  ppu.at_next_hi <- (ppu.at_latch lsr 1) land 1

let shift_bg_regs (ppu : t) : unit =
  ppu.bg_shift_lo <- (ppu.bg_shift_lo lsl 1) land 0xFFFF;
  ppu.bg_shift_hi <- (ppu.bg_shift_hi lsl 1) land 0xFFFF;
  ppu.at_shift_lo <- ((ppu.at_shift_lo lsl 1) land 0xFF) lor ppu.at_next_lo;
  ppu.at_shift_hi <- ((ppu.at_shift_hi lsl 1) land 0xFF) lor ppu.at_next_hi

(* 8 dot 周期の fetch: dot %8 = 1 NT, 3 AT, 5 PT_lo, 7 PT_hi reload.
   dot 257 で reload もう一発 (= dot 256 で fetched data の最後の reload). *)
let bg_fetch_at_dot (ppu : t) : unit =
  let d = ppu.dot in
  if d >= 1 && d <= 256
  then (
    shift_bg_regs ppu;
    (match d land 7 with
     | 1 -> ppu.nt_latch <- fetch_nt_byte ppu
     | 3 -> ppu.at_latch <- fetch_at_byte ppu
     | 5 -> ppu.pt_lo_latch <- fetch_pt_lo_byte ppu
     | 7 -> ppu.pt_hi_latch <- fetch_pt_hi_byte ppu
     | 0 ->
       reload_bg_shifts ppu;
       inc_coarse_x ppu
     | _ -> ());
    if d = 256 then inc_y ppu)
  else if d = 257
  then copy_horizontal ppu
  else if d >= 321 && d <= 336
  then (
    shift_bg_regs ppu;
    match d land 7 with
    | 1 -> ppu.nt_latch <- fetch_nt_byte ppu
    | 3 -> ppu.at_latch <- fetch_at_byte ppu
    | 5 -> ppu.pt_lo_latch <- fetch_pt_lo_byte ppu
    | 7 -> ppu.pt_hi_latch <- fetch_pt_hi_byte ppu
    | 0 ->
      reload_bg_shifts ppu;
      inc_coarse_x ppu
    | _ -> ())

(* Pre-render scanline 261 でのみ: dot 280-304 で vertical copy. *)
let pre_render_vertical_copy (ppu : t) : unit =
  if ppu.dot >= 280 && ppu.dot <= 304 then copy_vertical ppu

(* Per-scanline palette cache refresh. state の 32-entry array に in-place 書き込み. *)
let refresh_palette_cache_inplace (ppu : t) : unit =
  let gs = ppu.mask.gray_scale in
  let em_r = ppu.mask.color_emphasis.red in
  let em_g = ppu.mask.color_emphasis.green in
  let em_b = ppu.mask.color_emphasis.blue in
  let any_em = em_r || em_g || em_b in
  let apply_em c em = if em then c else (c * 192) asr 8 in
  for i = 0 to 31 do
    let pal_byte =
      Char.code (Bytes.unsafe_get ppu.palette_ram (palette_offset (0x3F00 + i)))
    in
    let pal_byte = if gs then pal_byte land 0x30 else pal_byte in
    let r, g, b = master_rgb ppu pal_byte in
    let r, g, b =
      if any_em
      then (apply_em r em_r, apply_em g em_g, apply_em b em_b)
      else (r, g, b)
    in
    Array.unsafe_set ppu.palette_r_cache i r;
    Array.unsafe_set ppu.palette_g_cache i g;
    Array.unsafe_set ppu.palette_b_cache i b
  done

(* Draw pixel for visible scanline 0..239 at dot 1..256. Per-pixel BG output. *)
let draw_bg_pixel (ppu : t) : unit =
  if ppu.palette_cache_dirty then (
    refresh_palette_cache_inplace ppu;
    ppu.palette_cache_dirty <- false);
  let r_cache = ppu.palette_r_cache in
  let g_cache = ppu.palette_g_cache in
  let b_cache = ppu.palette_b_cache in
  let x = ppu.dot - 1 in
  let y = ppu.scanline in
  let fx = Uint8.to_int ppu.internal.x land 7 in
  let bit_pos = 15 - fx in
  let bg_lo = (ppu.bg_shift_lo lsr bit_pos) land 1 in
  let bg_hi = (ppu.bg_shift_hi lsr bit_pos) land 1 in
  let bg_color = (bg_hi lsl 1) lor bg_lo in
  let at_bit = 7 - fx in
  let at_lo = (ppu.at_shift_lo lsr at_bit) land 1 in
  let at_hi = (ppu.at_shift_hi lsr at_bit) land 1 in
  let bg_palette = (at_hi lsl 1) lor at_lo in
  (* Leftmost 8 px BG clip *)
  let clipped = x < 8 && not ppu.mask.enable_bg_left_column in
  let visible = ppu.mask.enable_bg && not clipped in
  let pal_idx = if (not visible) || bg_color = 0 then 0 else (bg_palette * 4) + bg_color in
  let r = Array.unsafe_get r_cache pal_idx in
  let g = Array.unsafe_get g_cache pal_idx in
  let b = Array.unsafe_get b_cache pal_idx in
  let off = ((y * 256) + x) * 4 in
  Bytes.unsafe_set ppu.framebuffer off (Char.unsafe_chr r);
  Bytes.unsafe_set ppu.framebuffer (off + 1) (Char.unsafe_chr g);
  Bytes.unsafe_set ppu.framebuffer (off + 2) (Char.unsafe_chr b);
  Bytes.unsafe_set ppu.framebuffer (off + 3) '\xFF';
  (* bg_mask: BG opacity (sprite priority 判定用). visible で非透明なら 1 *)
  let mask_off = (y * 256) + x in
  if visible && bg_color <> 0
  then Bytes.unsafe_set ppu.bg_mask mask_off '\x01'
  else Bytes.unsafe_set ppu.bg_mask mask_off '\x00'

(** Palette RAM 32 byte を frame 開始時にまとめて RGB に解決して
    キャッシュする (16 bg + 16 sprite). 描画ループ内では 1 pixel あたり
    palette RAM read を行わず、この配列の index 参照で済む.
    palette mirror ($3F10/14/18/1C → $3F00/04/08/0C) も解決済み.

    Phase B5: PPUMASK の grayscale / color emphasis を適用する.
    - grayscale: palette byte を [land 0x30] (= 下位 4 bit クリア) で輝度のみに
    - emphasis: emphasis されていないチャネルを ~75% に減衰 *)
let resolve_palette_cache (ppu : t) : int array * int array * int array =
  let r_arr = Array.make 32 0 in
  let g_arr = Array.make 32 0 in
  let b_arr = Array.make 32 0 in
  let gs = ppu.mask.gray_scale in
  let em_r = ppu.mask.color_emphasis.red in
  let em_g = ppu.mask.color_emphasis.green in
  let em_b = ppu.mask.color_emphasis.blue in
  let any_em = em_r || em_g || em_b in
  let apply_em c em = if em then c else c * (192 asr 8) in
  for i = 0 to 31 do
    let pal_byte =
      Char.code (Bytes.unsafe_get ppu.palette_ram (palette_offset (0x3F00 + i)))
    in
    let pal_byte = if gs then pal_byte land 0x30 else pal_byte in
    let r, g, b = master_rgb ppu pal_byte in
    let r, g, b =
      if any_em
      then (apply_em r em_r, apply_em g em_g, apply_em b em_b)
      else (r, g, b)
    in
    Array.unsafe_set r_arr i r;
    Array.unsafe_set g_arr i g;
    Array.unsafe_set b_arr i b
  done;
  (r_arr, g_arr, b_arr)

(** 1 frame 分の background を framebuffer に書き出す.

    Loopy t と x をフレーム全体のスクロール基準として使う:
    - scroll_x = (coarse_x << 3) | fine_x  ← t[4:0] と register x
    - scroll_y = (coarse_y << 3) | fine_y  ← t[9:5] と t[14:12]
    - base nametable: t[11:10]

    実機は scanline 単位で v を進めるが、本実装は per-frame で
    1 フレーム = 1 つの scroll 状態として扱う (mid-frame split は
    非対応; sprite 0 hit 系のステータスバー固定は別途 Phase B4 で).

    最適化:
    - per-pixel ループではなく per-tile (33 tiles/scanline × 240 scanline) で
      nametable/attribute/CHR を fetch
    - palette は frame 冒頭で 32 色 → RGB に pre-resolve
    - 内部ループは Bytes.unsafe_set_uint8 と Array.unsafe_get で
      bounds check を排除 *)
let render_background (ppu : t) : unit =
  clear_bg_mask ppu;
  let r_cache, g_cache, b_cache = resolve_palette_cache ppu in
  if not ppu.mask.enable_bg
  then (
    let r = Array.unsafe_get r_cache 0 in
    let g = Array.unsafe_get g_cache 0 in
    let b = Array.unsafe_get b_cache 0 in
    fill_framebuffer ppu ~r ~g ~b)
  else (
    let pattern_base = bg_pattern_base ppu in
    let fb = ppu.framebuffer in
    let mask = ppu.bg_mask in
    let chr_read = ppu.chr_io.chr_read in
    (* scanline 単位の scroll: initial_scroll を起点に、scroll_log を時間順に
       walk して各 visible scanline で有効な (t, x) を引き当てる. *)
    let log = ppu.scroll_log in
    let log_n = ppu.scroll_log_n in
    let writes_idx = ref 0 in
    let init_t, init_x =
      match ppu.initial_scroll with
      | Some (t, x) -> (t, x)
      | None -> (Uint16.to_int ppu.internal.t, Uint8.to_int ppu.internal.x)
    in
    let cur_t = ref init_t in
    let cur_x = ref init_x in
    for sy = 0 to 239 do
      (* scanline sy の rendering 前に「sy より前 (= sl < sy) の visible 書き換え」
         および「vblank/pre-render での書き換え (sl >= 240)」を反映する.
         同一 scanline 中の write は次 scanline で反映 (= 簡易 v ↔ t 更新). *)
      while
        !writes_idx < log_n
        &&
        let sl = Array.unsafe_get log (!writes_idx * 3) in
        sl >= 240 || sl < sy
      do
        let off = !writes_idx * 3 in
        cur_t := Array.unsafe_get log (off + 1);
        cur_x := Array.unsafe_get log (off + 2);
        incr writes_idx
      done;
      let t = !cur_t in
      let base_nt = (t lsr 10) land 0x03 in
      let coarse_x = t land 0x1F in
      let coarse_y = (t lsr 5) land 0x1F in
      let fine_x_reg = !cur_x land 0x07 in
      let fine_y_t = (t lsr 12) land 0x07 in
      let scroll_x = (coarse_x lsl 3) lor fine_x_reg in
      let scroll_y = (coarse_y lsl 3) lor fine_y_t in
      let nt_origin_x = base_nt land 1 * 256 in
      let nt_origin_y = (base_nt lsr 1) land 1 * 240 in
      let vy_raw = nt_origin_y + sy + scroll_y in
      let vy = ((vy_raw mod 480) + 480) mod 480 in
      let nt_y = vy / 240 in
      let local_y = vy mod 240 in
      let tile_row = local_y / 8 in
      let fine_y = local_y land 7 in
      let vx_base = nt_origin_x + scroll_x in
      let fine_x = vx_base land 7 in
      (* 33 タイル: 最初のタイルは fine_x..7 のみ画面に出る *)
      for tn = 0 to 32 do
        let vx_tile = vx_base - fine_x + (tn * 8) in
        let vx = vx_tile land 0x1FF in
        let nt_x = (vx lsr 8) land 1 in
        let local_x = vx land 0xFF in
        let tile_col = local_x lsr 3 in
        let nt_idx = (nt_y * 2) + nt_x in
        let nt_base = 0x2000 + (nt_idx * 0x0400) in
        let tile_idx =
          Uint8.to_int (ppu_bus_read ppu (nt_base + (tile_row * 32) + tile_col))
        in
        let at_byte =
          Uint8.to_int
            (ppu_bus_read
               ppu
               (nt_base + 0x3C0 + (tile_row / 4 * 8) + (tile_col / 4)))
        in
        let qx = (tile_col lsr 1) land 1 in
        let qy = (tile_row lsr 1) land 1 in
        let shift = ((qy * 2) + qx) * 2 in
        let palette_idx = (at_byte lsr shift) land 0b11 in
        let chr_addr = pattern_base + (tile_idx * 16) + fine_y in
        let lo = Uint8.to_int (chr_read chr_addr) in
        let hi = Uint8.to_int (chr_read (chr_addr + 8)) in
        for fx = 0 to 7 do
          let screen_x = (tn * 8) + fx - fine_x in
          if screen_x >= 0 && screen_x < 256
          then (
            let bit = 7 - fx in
            let lo_b = (lo lsr bit) land 1 in
            let hi_b = (hi lsr bit) land 1 in
            let c = (hi_b lsl 1) lor lo_b in
            let pal_cache_idx = if c = 0 then 0 else (palette_idx * 4) + c in
            let r = Array.unsafe_get r_cache pal_cache_idx in
            let g = Array.unsafe_get g_cache pal_cache_idx in
            let b = Array.unsafe_get b_cache pal_cache_idx in
            let fb_off = ((sy * 256) + screen_x) * 4 in
            Bytes.unsafe_set fb fb_off (Char.unsafe_chr r);
            Bytes.unsafe_set fb (fb_off + 1) (Char.unsafe_chr g);
            Bytes.unsafe_set fb (fb_off + 2) (Char.unsafe_chr b);
            Bytes.unsafe_set fb (fb_off + 3) '\xFF';
            if c <> 0 then Bytes.unsafe_set mask ((sy * 256) + screen_x) '\x01')
        done
      done
    done)

(* ------------------------------------------------------------------ *)
(* Sprite renderer (Phase B3)                                          *)
(*                                                                     *)
(* per-frame の簡易実装: vblank で render_background の直後に呼ばれ、   *)
(* 64 sprite を逆順 (高 index → 低 index) で合成する。                  *)
(* これで OAM 内の低 index sprite が前面に来る。                         *)
(*                                                                     *)
(* 仕様:                                                                *)
(* - byte 0 = Y - 1 (実際は y+1 から 8 (or 16) 行)                      *)
(* - byte 1 = tile (8x16 では bit 0 = pattern table, bits 7-1 = top)    *)
(* - byte 2 = attr: pal (0-1), priority (5), flip H/V (6/7)             *)
(* - byte 3 = X                                                         *)
(* - sprite palette は $3F11-$3F1F (color 0 は透明; 描かない)            *)
(* - priority 1 (behind) は BG の透明ピクセル上にだけ描く                *)
(* - leftmost 8 px clip は PPUMASK.M で制御                              *)
(* - sprite 0 hit: sprite #0 の opaque pixel と BG の opaque pixel が   *)
(*   重なる最初の位置で status.sprite_0_hit を立てる (x=255 は除外)      *)
(* ------------------------------------------------------------------ *)

(** sprite tile の (tile_byte, fine_y) から CHR offset を計算.
    8x16 のときは tile_byte の bit 0 が pattern table、bits 7-1 が top tile,
    bit が立ってる方が bottom tile (連続). flip_v はここでは見ない (呼び手で
    fine_y を予め反転しておく). *)
(* ================================================================== *)
(* Per-pixel Sprite pipeline (Phase B7)                                *)
(*                                                                     *)
(* 各 visible scanline 終了時 (dot 257-320 相当) に「次 scanline 用の  *)
(* 8 sprite」を eval + pattern fetch して下記配列に積む.                *)
(* 次 scanline の dot 1-256 で sprite_x[i] からの 8 pixel 範囲を walk. *)
(* ================================================================== *)

let sprite_height (ppu : t) : int =
  match ppu.ctrl.sprite_size with
  | Register.Ppu_control.Spr_8x8 -> 8
  | Register.Ppu_control.Spr_8x16 -> 16

(* 256 entry reverse-bit lookup table (horizontal flip 用). *)
let reverse_bits_table : int array =
  let tbl = Array.make 256 0 in
  for b = 0 to 255 do
    let r = ref 0 in
    for k = 0 to 7 do
      r := !r lor (((b lsr k) land 1) lsl (7 - k))
    done;
    tbl.(b) <- !r
  done;
  tbl

(* sprite_pattern_offset を inline (forward ref 不可なので). *)
let sprite_pat_addr (ppu : t) ~(tile_byte : int) ~(fine_y : int) : int =
  match ppu.ctrl.sprite_size with
  | Register.Ppu_control.Spr_8x8 ->
    let base =
      match ppu.ctrl.spr_alignment with
      | Register.Ppu_control.L -> 0x0000
      | Register.Ppu_control.R -> 0x1000
    in
    base + (tile_byte * 16) + fine_y
  | Register.Ppu_control.Spr_8x16 ->
    let base = if tile_byte land 1 = 0 then 0x0000 else 0x1000 in
    let bt = tile_byte land 0xFE in
    let st, sy =
      if fine_y < 8 then (bt, fine_y) else (bt + 1, fine_y - 8)
    in
    base + (st * 16) + sy

(* Evaluate sprites for scanline [next_sl] (0..239). Populate sprite_*[]
   arrays + sprite_count. Set sprite_overflow flag if > 8 sprites in range. *)
let evaluate_sprites_for (ppu : t) ~(next_sl : int) : unit =
  ppu.sprite_count <- 0;
  let height = sprite_height ppu in
  let i = ref 0 in
  let overflow = ref false in
  while !i < 64 && not !overflow do
    let base = !i * 4 in
    let y = Char.code (Bytes.unsafe_get ppu.oam base) in
    if next_sl >= y && next_sl < y + height
    then (
      if ppu.sprite_count < 8
      then (
        let slot = ppu.sprite_count in
        let tile = Char.code (Bytes.unsafe_get ppu.oam (base + 1)) in
        let attr = Char.code (Bytes.unsafe_get ppu.oam (base + 2)) in
        let x = Char.code (Bytes.unsafe_get ppu.oam (base + 3)) in
        let flip_v = attr land 0x80 <> 0 in
        let fy = next_sl - y in
        let fy = if flip_v then height - 1 - fy else fy in
        let chr_ofs = sprite_pat_addr ppu ~tile_byte:tile ~fine_y:fy in
        let pat_lo = Uint8.to_int (ppu.chr_io.chr_read chr_ofs) in
        let pat_hi = Uint8.to_int (ppu.chr_io.chr_read (chr_ofs + 8)) in
        (* horizontal flip: bit 順を反転して保存 *)
        let flip_h = attr land 0x40 <> 0 in
        let pat_lo =
          if flip_h then Array.unsafe_get reverse_bits_table pat_lo else pat_lo
        in
        let pat_hi =
          if flip_h then Array.unsafe_get reverse_bits_table pat_hi else pat_hi
        in
        Array.unsafe_set ppu.sprite_pattern_lo slot pat_lo;
        Array.unsafe_set ppu.sprite_pattern_hi slot pat_hi;
        Array.unsafe_set ppu.sprite_x slot x;
        Array.unsafe_set ppu.sprite_attr slot attr;
        Array.unsafe_set ppu.sprite_is_zero slot (!i = 0);
        ppu.sprite_count <- ppu.sprite_count + 1)
      else (
        (* > 8 sprites in range → set overflow. 実機の hardware bug 再現は省略. *)
        ppu.status <- { ppu.status with sprite_overflow = true };
        overflow := true));
    incr i
  done

(* Per-scanline sprite line resolve. Scanline 開始時 (dot 0 or 1) に呼び、
   その行 256 pixel 分の sprite 出力を sprite_line_* 配列に書く.
   draw_sprite_pixel は配列 lookup だけで済む. *)
let resolve_sprite_line (ppu : t) : unit =
  (* Clear line buffer. *)
  Array.fill ppu.sprite_line_color 0 256 (-1);
  if not ppu.mask.enable_sprite || ppu.sprite_count = 0 then ()
  else (
    (* 低 index sprite が前面 → 高 index から書いて低 index で上書きする. *)
    for s = ppu.sprite_count - 1 downto 0 do
      let sx = Array.unsafe_get ppu.sprite_x s in
      let pat_lo = Array.unsafe_get ppu.sprite_pattern_lo s in
      let pat_hi = Array.unsafe_get ppu.sprite_pattern_hi s in
      let attr = Array.unsafe_get ppu.sprite_attr s in
      let pal = attr land 0b11 in
      let behind = attr land 0x20 <> 0 in
      let is_zero = Array.unsafe_get ppu.sprite_is_zero s in
      for fx = 0 to 7 do
        let x = sx + fx in
        if x < 256
        then (
          let bit = 7 - fx in
          let lo = (pat_lo lsr bit) land 1 in
          let hi = (pat_hi lsr bit) land 1 in
          let color = (hi lsl 1) lor lo in
          if color <> 0
          then (
            Array.unsafe_set ppu.sprite_line_color x (16 + (pal * 4) + color);
            Array.unsafe_set ppu.sprite_line_behind x behind;
            Array.unsafe_set ppu.sprite_line_is_zero x is_zero))
      done
    done)

(* DrawPixel sprite (called at visible dot 1..256, scanline 0..239).
   BG は draw_bg_pixel が既に framebuffer + bg_mask に書いてる. *)
let draw_sprite_pixel (ppu : t) : unit =
  if not ppu.mask.enable_sprite then ()
  else (
    let x = ppu.dot - 1 in
    let pal_idx = Array.unsafe_get ppu.sprite_line_color x in
    if pal_idx < 0 then () (* transparent *)
    else if x < 8 && not ppu.mask.enable_spr_left_column then ()
    else (
      let y = ppu.scanline in
      let bg_off = (y * 256) + x in
      let bg_opaque = Bytes.unsafe_get ppu.bg_mask bg_off = '\x01' in
      let is_zero = Array.unsafe_get ppu.sprite_line_is_zero x in
      let behind = Array.unsafe_get ppu.sprite_line_behind x in
      (* Sprite 0 hit *)
      if
        is_zero
        && bg_opaque
        && x <> 255
        && ppu.mask.enable_bg
        && not ppu.status.sprite_0_hit
        && not
             (x < 8
              && (not ppu.mask.enable_bg_left_column
                  || not ppu.mask.enable_spr_left_column))
      then ppu.status <- { ppu.status with sprite_0_hit = true };
      (* Priority: front か bg transparent なら sprite 描画 *)
      if (not behind) || not bg_opaque
      then (
        if ppu.palette_cache_dirty
        then (
          refresh_palette_cache_inplace ppu;
          ppu.palette_cache_dirty <- false);
        let r = Array.unsafe_get ppu.palette_r_cache pal_idx in
        let g = Array.unsafe_get ppu.palette_g_cache pal_idx in
        let b = Array.unsafe_get ppu.palette_b_cache pal_idx in
        let off = ((y * 256) + x) * 4 in
        Bytes.unsafe_set ppu.framebuffer off (Char.unsafe_chr r);
        Bytes.unsafe_set ppu.framebuffer (off + 1) (Char.unsafe_chr g);
        Bytes.unsafe_set ppu.framebuffer (off + 2) (Char.unsafe_chr b))))

(* ================================================================== *)

let sprite_pattern_offset (ppu : t) ~(tile_byte : int) ~(fine_y : int) : int =
  match ppu.ctrl.sprite_size with
  | Register.Ppu_control.Spr_8x8 ->
    let pattern_base =
      match ppu.ctrl.spr_alignment with
      | Register.Ppu_control.L -> 0x0000
      | Register.Ppu_control.R -> 0x1000
    in
    pattern_base + (tile_byte * 16) + fine_y
  | Register.Ppu_control.Spr_8x16 ->
    let pattern_base = if tile_byte land 1 = 0 then 0x0000 else 0x1000 in
    let base_tile = tile_byte land 0xFE in
    let sub_tile, sub_y =
      if fine_y < 8 then (base_tile, fine_y) else (base_tile + 1, fine_y - 8)
    in
    pattern_base + (sub_tile * 16) + sub_y

(** Sprite 0 と BG (bg_mask 経由) の最初の overlap scanline を予測する.
    render_background の直後に呼ぶこと (bg_mask が確定している前提).
    結果は 0..239 (hit scanline) または 240 (no hit) を返す.

    x = 255 は仕様上除外、leftmost 8px clip も考慮.
    enable_bg=false or enable_sprite=false なら hit 起きないので 240. *)
let predict_sprite_0_hit_scanline (ppu : t) : int =
  if (not ppu.mask.enable_bg) || not ppu.mask.enable_sprite
  then 240
  else (
    let y = Char.code (Bytes.unsafe_get ppu.oam 0) in
    let tile_byte = Char.code (Bytes.unsafe_get ppu.oam 1) in
    let attr = Char.code (Bytes.unsafe_get ppu.oam 2) in
    let x = Char.code (Bytes.unsafe_get ppu.oam 3) in
    if y >= 0xEF
    then 240
    else (
      let flip_h = attr land 0b0100_0000 <> 0 in
      let flip_v = attr land 0b1000_0000 <> 0 in
      let height =
        match ppu.ctrl.sprite_size with
        | Register.Ppu_control.Spr_8x8 -> 8
        | Register.Ppu_control.Spr_8x16 -> 16
      in
      let top = y + 1 in
      let chr_read = ppu.chr_io.chr_read in
      let mask = ppu.bg_mask in
      let left_bg = ppu.mask.enable_bg_left_column in
      let left_spr = ppu.mask.enable_spr_left_column in
      let result = ref 240 in
      (try
         for py = 0 to height - 1 do
           let dst_y = top + py in
           if dst_y >= 240 then raise Exit;
           let fine_y = if flip_v then height - 1 - py else py in
           let chr_ofs = sprite_pattern_offset ppu ~tile_byte ~fine_y in
           let lo = Uint8.to_int (chr_read chr_ofs) in
           let hi = Uint8.to_int (chr_read (chr_ofs + 8)) in
           for px = 0 to 7 do
             let dst_x = x + px in
             if dst_x < 256 && dst_x <> 255
             then (
               let bit_x = if flip_h then px else 7 - px in
               let lo_b = (lo lsr bit_x) land 1 in
               let hi_b = (hi lsr bit_x) land 1 in
               let c = (hi_b lsl 1) lor lo_b in
               if c <> 0
               then (
                 let bg_op =
                   Bytes.unsafe_get mask ((dst_y * 256) + dst_x) <> '\x00'
                 in
                 if bg_op
                 then (
                   let clipped = dst_x < 8 && ((not left_bg) || not left_spr) in
                   if not clipped
                   then (
                     result := dst_y;
                     raise Exit))))
           done
         done
       with
       | Exit -> ());
      !result))

(** 1 frame 分の sprite を framebuffer に上書き合成する.

    Phase B5 から per-scanline 評価:
    - 各 visible scanline で OAM を低 index から走査
    - 現 scanline をカバーする sprite を最大 8 個まで secondary OAM に集める
    - 9 個目を見つけた瞬間 PPUSTATUS.O (sprite_overflow) を set
    - 集めた 8 sprite を逆順 (高 index → 低 index) で描画 → 低 index が前面 *)
let render_sprites (ppu : t) : unit =
  if not ppu.mask.enable_sprite
  then ()
  else (
    let r_cache, g_cache, b_cache = resolve_palette_cache ppu in
    let height =
      match ppu.ctrl.sprite_size with
      | Register.Ppu_control.Spr_8x8 -> 8
      | Register.Ppu_control.Spr_8x16 -> 16
    in
    let fb = ppu.framebuffer in
    let mask = ppu.bg_mask in
    let chr_read = ppu.chr_io.chr_read in
    let left_spr = ppu.mask.enable_spr_left_column in
    let secondary = Array.make 8 0 in
    for sy = 0 to 239 do
      let n_found = ref 0 in
      for s = 0 to 63 do
        let off = s * 4 in
        let y = Char.code (Bytes.unsafe_get ppu.oam off) in
        if y < 0xEF
        then (
          let top = y + 1 in
          if sy >= top && sy < top + height
          then
            if !n_found < 8
            then (
              Array.unsafe_set secondary !n_found s;
              incr n_found)
            else
              (* 9 個目以降: overflow flag を立てる (実機のバグ動作は再現しない) *)
              ppu.status <- { ppu.status with sprite_overflow = true })
      done;
      (* secondary OAM を逆順に描画 (低 index が前面) *)
      for i = !n_found - 1 downto 0 do
        let sprite_idx = Array.unsafe_get secondary i in
        let off = sprite_idx * 4 in
        let y = Char.code (Bytes.unsafe_get ppu.oam off) in
        let tile_byte = Char.code (Bytes.unsafe_get ppu.oam (off + 1)) in
        let attr = Char.code (Bytes.unsafe_get ppu.oam (off + 2)) in
        let x = Char.code (Bytes.unsafe_get ppu.oam (off + 3)) in
        let palette_idx = attr land 0b11 in
        let behind = attr land 0b0010_0000 <> 0 in
        let flip_h = attr land 0b0100_0000 <> 0 in
        let flip_v = attr land 0b1000_0000 <> 0 in
        let top = y + 1 in
        let py = sy - top in
        let fine_y = if flip_v then height - 1 - py else py in
        let chr_ofs = sprite_pattern_offset ppu ~tile_byte ~fine_y in
        let lo = Uint8.to_int (chr_read chr_ofs) in
        let hi = Uint8.to_int (chr_read (chr_ofs + 8)) in
        for px = 0 to 7 do
          let dst_x = x + px in
          if dst_x < 256
          then (
            let bit_x = if flip_h then px else 7 - px in
            let lo_b = (lo lsr bit_x) land 1 in
            let hi_b = (hi lsr bit_x) land 1 in
            let c = (hi_b lsl 1) lor lo_b in
            if c <> 0
            then (
              let mask_off = (sy * 256) + dst_x in
              let bg_opaque = Bytes.unsafe_get mask mask_off <> '\x00' in
              let clipped = dst_x < 8 && not left_spr in
              let should_draw =
                (not clipped) && ((not behind) || not bg_opaque)
              in
              if should_draw
              then (
                let pal_cache_idx = 16 + (palette_idx * 4) + c in
                let r = Array.unsafe_get r_cache pal_cache_idx in
                let g = Array.unsafe_get g_cache pal_cache_idx in
                let b = Array.unsafe_get b_cache pal_cache_idx in
                let fb_off = mask_off * 4 in
                Bytes.unsafe_set fb fb_off (Char.unsafe_chr r);
                Bytes.unsafe_set fb (fb_off + 1) (Char.unsafe_chr g);
                Bytes.unsafe_set fb (fb_off + 2) (Char.unsafe_chr b);
                Bytes.unsafe_set fb (fb_off + 3) '\xFF')))
        done
      done
    done)

(** PPU を 1 dot 進める。Per-pixel renderer (Mesen-style).
    Visible/pre-render scanline では BG fetch pipeline + DrawPixel を回す.
    Sprite renderer は当面 per-frame (vblank で render_sprites を呼ぶ). *)
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
  let sl = ppu.scanline in
  let d = ppu.dot in
  let rendering_enabled = ppu.mask.enable_bg || ppu.mask.enable_sprite in
  (* (2) フレーム開始: scanline 0 dot 0 で bg_mask クリア *)
  if sl = 0 && d = 0 then clear_bg_mask ppu;
  (* (3) Visible scanline (0..239): draw + fetch pipeline *)
  if sl < 240 && rendering_enabled
  then (
    (* dot 0 で sprite line を一気に解決. 以降 dot は配列 lookup のみ. *)
    if d = 0 then resolve_sprite_line ppu;
    if d >= 1 && d <= 256 then (
      draw_bg_pixel ppu;
      draw_sprite_pixel ppu);
    bg_fetch_at_dot ppu);
  (* (3.5) Sprite eval for NEXT scanline. dot 257 で実行 (実機 dot 257-320 の代表). *)
  if d = 257 && rendering_enabled
  then (
    if sl < 239 then evaluate_sprites_for ppu ~next_sl:(sl + 1)
    else if sl = 261 then evaluate_sprites_for ppu ~next_sl:0);
  (* (4) Pre-render scanline (261): fetch pipeline + vertical copy *)
  if sl = 261 && rendering_enabled
  then (
    bg_fetch_at_dot ppu;
    pre_render_vertical_copy ppu);
  (* (5) Vblank 突入 (scanline 241 dot 1) *)
  if sl = 241 && d = 1
  then (
    ppu.status <- { ppu.status with vblank_flag = true };
    if ppu.ctrl.enable_nmi then ppu.nmi_request <- true;
    ppu.frame_complete <- true);
  (* (6) Pre-render 開始でフラグクリア *)
  if sl = 261 && d = 1
  then
    ppu.status
    <- { vblank_flag = false; sprite_0_hit = false; sprite_overflow = false };
  (* (8) MMC3 A12 rise 通知. 簡易: dot 260 で 1 回. *)
  if
    rendering_enabled
    && (sl < 240 || sl = 261)
    && d = 260
  then ppu.chr_io.a12_rise ()
