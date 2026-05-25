open Js_of_ocaml
open Famicaml_common.Nesint
module Ines = Emulator.Rom.Ines
module Cart = Emulator.Rom.Cartridge
module Nes = Emulator.Nes
module Pattern_table = Emulator.Ppu.Pattern_table
module Palette = Emulator.Ppu.Palette

(* ------------------------------------------------------------------ *)
(* ヘルパー                                                             *)
(* ------------------------------------------------------------------ *)

let bytes_of_uint8array (arr : Typed_array.uint8Array Js.t) : bytes =
  let len = arr##.length in
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set_uint8 b i (Typed_array.unsafe_get arr i)
  done;
  b

(** OCaml の bytes を JS の Uint8Array に変換する。
    ImageData コンストラクタは Uint8ClampedArray を要求するが、
    js_of_ocaml 6.2 の Typed_array は ClampedArray を公開していないので、
    Uint8Array で返して JS 側 (ReScript) で
    [new Uint8ClampedArray(arr.buffer)] に巻き直す。 *)
let uint8array_of_bytes (b : bytes) : Typed_array.uint8Array Js.t =
  let len = Bytes.length b in
  let arr = new%js Typed_array.uint8Array len in
  for i = 0 to len - 1 do
    Typed_array.set arr i (Bytes.get_uint8 b i)
  done;
  arr

let mirror_to_string = function
  | Cart.H -> "horizontal"
  | Cart.V -> "vertical"

let rom_summary = function
  | Cart.NROM { prg; chr } -> ("NROM", Bytes.length prg, Bytes.length chr)
  | Cart.UNROM { prg; chr_ram } ->
    ("UNROM", Bytes.length prg, Bytes.length chr_ram)
  | Cart.CNROM { prg; chr } -> ("CNROM", Bytes.length prg, Bytes.length chr)

(* ------------------------------------------------------------------ *)
(* グローバル NES インスタンス                                          *)
(* ------------------------------------------------------------------ *)

let nes : Nes.t = Nes.mk ()

(* ------------------------------------------------------------------ *)
(* グローバル Palette 状態                                              *)
(*                                                                     *)
(* マスターパレット (64 色) と pattern viewer 用 sub palette (4 色)。   *)
(* どちらもブラウザ側 UI から編集される。将来 PPU の palette RAM が     *)
(* 実装されたら sub の動的選択ロジックはそちらへ移し、ここは display    *)
(* レイヤ専用 (master だけ) になる想定。 *)
(* ------------------------------------------------------------------ *)

let master_palette : Palette.t ref = ref (Palette.default ())
let viewer_sub : Palette.sub ref = ref (Palette.default_sub ())

(* ------------------------------------------------------------------ *)
(* JS object 構築                                                       *)
(* ------------------------------------------------------------------ *)

let cart_summary_js (cart : Cart.t) =
  let mapper, prg_size, chr_size = rom_summary cart.rom in
  object%js
    val mapper = Js.string mapper
    val mirroring = Js.string (mirror_to_string cart.spec.mirroring)
    val hasBattery = Js.bool cart.spec.has_battery
    val hasTrainer = Js.bool cart.spec.has_trainer
    val prgSize = prg_size
    val chrSize = chr_size
  end

let cart_field () =
  match nes.cart with
  | None -> Js.null
  | Some c -> Js.some (cart_summary_js c)

let state_js () =
  object%js
    val power = Js.bool nes.power
    val cart = cart_field ()
    val resetVector = Uint16.to_int nes.ith_reset
    val nmiVector = Uint16.to_int nes.ith_nmi
    val irqVector = Uint16.to_int nes.ith_irq
    val pc = Uint16.to_int nes.cpu.reg_PC
    val cpuCycles = nes.cpu.cycles
    val ppuFrame = nes.ppu.frame
    val ppuScanline = nes.ppu.scanline
    val ppuDot = nes.ppu.dot
  end

(** パターンテーブル ($0000-$0FFF または $1000-$1FFF) を 128×128 RGBA に
    デコードして JS object として返す。
    - idx = 0: $0000-$0FFF (left)
    - idx = 1: $1000-$1FFF (right)
    カートリッジ未挿入や CHR が足りない場合は null を返す。 *)
let pattern_table_js (idx : int) =
  match nes.cart with
  | None -> Js.null
  | Some cart ->
    let chr = Cart.chr_bytes cart in
    let table_ofs = idx * 0x1000 in
    if Bytes.length chr < table_ofs + 0x1000
    then Js.null
    else (
      let pixels = Pattern_table.decode_table ~chr ~table_ofs in
      let rgba =
        Palette.pixels_to_rgba pixels ~master:!master_palette ~sub:!viewer_sub
      in
      Js.some
        (object%js
           val width = 128
           val height = 128
           val rgba = uint8array_of_bytes rgba
        end))

(* ------------------------------------------------------------------ *)
(* Palette API                                                          *)
(* ------------------------------------------------------------------ *)

(** マスターパレットを .pal 形式 (192 byte) で取得する。 *)
let get_master_palette () =
  uint8array_of_bytes (Palette.to_pal_bytes !master_palette)

(** .pal バイト列を読み込んでマスターパレットを差し替える。
    成功なら true、失敗 (サイズ不正等) なら false。 *)
let set_master_palette (arr : Typed_array.uint8Array Js.t) =
  let b = bytes_of_uint8array arr in
  match Palette.of_pal_bytes b with
  | Ok m ->
    master_palette := m;
    Js._true
  | Error _ -> Js._false

(** マスターパレットを内蔵デフォルトに戻す。 *)
let reset_master_palette () = master_palette := Palette.default ()

(** マスターインデックス [idx] (0..63) の色を更新する。 *)
let set_master_color (idx : int) (r : int) (g : int) (b : int) =
  if idx >= 0 && idx < 64 then Palette.set_color !master_palette idx ~r ~g ~b

(** Pattern viewer 用 sub palette の 4 スロットを 4 byte で取得する。
    各 byte がマスターインデックス 0..63。 *)
let get_viewer_sub () =
  let b = Bytes.create 4 in
  Array.iteri (fun i x -> Bytes.set_uint8 b i (x land 0x3F)) !viewer_sub;
  uint8array_of_bytes b

(** Pattern viewer 用 sub palette のスロット [slot] (0..3) に
    マスターインデックス [master_idx] (0..63) をアサインする。 *)
let set_viewer_sub_slot (slot : int) (master_idx : int) =
  if slot >= 0 && slot < 4 && master_idx >= 0 && master_idx < 64
  then !viewer_sub.(slot) <- master_idx

let load_rom (arr : Typed_array.uint8Array Js.t) =
  let data = bytes_of_uint8array arr in
  match Nes.connect nes data with
  | Ok () ->
    object%js
      val ok = Js._true
      val state = Js.some (state_js ())
      val error = Js.null
    end
  | Error e ->
    object%js
      val ok = Js._false
      val state = Js.null
      val error = Js.some (Js.string (Ines.error_to_string e))
    end

let () =
  Js.export
    "FamiCaml"
    (object%js
       val loadRom = Js.wrap_callback load_rom
       val eject = Js.wrap_callback (fun () -> Nes.eject nes)
       val reset = Js.wrap_callback (fun () -> Nes.reset nes)
       val powerOn = Js.wrap_callback (fun () -> Nes.power_on nes)
       val powerOff = Js.wrap_callback (fun () -> Nes.power_off nes)
       val state = Js.wrap_callback state_js
       val patternTable = Js.wrap_callback pattern_table_js
       val runFrame = Js.wrap_callback (fun () -> Nes.run_until_frame nes)
       val tick = Js.wrap_callback (fun () -> Nes.tick nes)
       val getMasterPalette = Js.wrap_callback get_master_palette
       val setMasterPalette = Js.wrap_callback set_master_palette
       val resetMasterPalette = Js.wrap_callback reset_master_palette

       val setMasterColor =
         Js.wrap_callback (fun idx r g b -> set_master_color idx r g b)

       val getViewerSub = Js.wrap_callback get_viewer_sub

       val setViewerSubSlot =
         Js.wrap_callback (fun slot master_idx ->
           set_viewer_sub_slot slot master_idx)
    end)
