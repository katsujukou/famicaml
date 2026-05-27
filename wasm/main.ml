open Js_of_ocaml
open Famicaml_common.Nesint
module Ines = Emulator.Rom.Ines
module Cart = Emulator.Rom.Cartridge
module Nes = Emulator.Nes
module Pattern_table = Emulator.Ppu.Pattern_table
module Palette = Emulator.Ppu.Palette
module Controller = Emulator.Controller

(* ------------------------------------------------------------------ *)
(* ヘルパー                                                             *)
(* ------------------------------------------------------------------ *)

(* js_of_ocaml 6.2 の primitive で zero-copy. ROM ロード等で使用. *)
let bytes_of_uint8array (arr : Typed_array.uint8Array Js.t) : bytes =
  Typed_array.Bytes.of_uint8Array arr

(** OCaml の bytes を JS の Uint8Array に変換する。
    ImageData コンストラクタは Uint8ClampedArray を要求するが、
    js_of_ocaml 6.2 の Typed_array は ClampedArray を公開していないので、
    Uint8Array で返して JS 側 (ReScript) で
    [new Uint8ClampedArray(arr.buffer)] に巻き直す。 *)
(* js_of_ocaml 6.2 の primitive で zero-copy. OCaml bytes 内容を直接 JS
   Uint8Array として view (= 値共有). フレームバッファ転送のホットパス
   (毎フレーム 245760 byte) で per-byte JS interop loop を解消. *)
let uint8array_of_bytes (b : bytes) : Typed_array.uint8Array Js.t =
  Typed_array.Bytes.to_uint8Array b

let mirror_to_string = function
  | Cart.H -> "horizontal"
  | Cart.V -> "vertical"
  | Cart.One_screen_lo -> "one-screen-lo"
  | Cart.One_screen_hi -> "one-screen-hi"

let rom_summary = function
  | Cart.NROM { prg; chr } -> ("NROM", Bytes.length prg, Bytes.length chr)
  | Cart.UNROM { prg; chr_ram } ->
    ("UNROM", Bytes.length prg, Bytes.length chr_ram)
  | Cart.CNROM { prg; chr } -> ("CNROM", Bytes.length prg, Bytes.length chr)
  | Cart.MMC1 { prg; chr; _ } -> ("MMC1", Bytes.length prg, Bytes.length chr)
  | Cart.MMC3 { prg; chr; _ } -> ("MMC3", Bytes.length prg, Bytes.length chr)

(* ------------------------------------------------------------------ *)
(* グローバル NES インスタンス                                          *)
(* ------------------------------------------------------------------ *)

let nes : Nes.t = Nes.mk ()

(* ------------------------------------------------------------------ *)
(* Pattern viewer 用 sub palette (4 色)。                              *)
(*                                                                     *)
(* マスターパレット (64 色) は Phase B1 以降 PPU 内部 (nes.ppu) に     *)
(* 統合されたので、viewer/emulator で同じ master を共有する。          *)
(* sub palette は viewer 専用 (デバッグ用) なので wasm 側で保持する。   *)
(* ------------------------------------------------------------------ *)

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
  | Some _ ->
    (* PPU の chr_io 経由で現在の bank 状態 ($0000-$0FFF or $1000-$1FFF) を
       読む. これにより MMC1/MMC3 等の bank 切替に追従する. *)
    let chr = Bytes.create 0x1000 in
    let base = idx * 0x1000 in
    for i = 0 to 0x0FFF do
      Bytes.set_uint8
        chr
        i
        (Famicaml_common.Nesint.Uint8.to_int
           (nes.ppu.chr_io.chr_read (base + i)))
    done;
    let pixels = Pattern_table.decode_table ~chr ~table_ofs:0 in
    let master =
      match Palette.of_pal_bytes (Emulator.Ppu.get_master_palette nes.ppu) with
      | Ok p -> p
      | Error _ -> Palette.default ()
    in
    let rgba = Palette.pixels_to_rgba pixels ~master ~sub:!viewer_sub in
    Js.some
      (object%js
         val width = 128
         val height = 128
         val rgba = uint8array_of_bytes rgba
      end)

(* ------------------------------------------------------------------ *)
(* Palette API                                                          *)
(* ------------------------------------------------------------------ *)

(** マスターパレットを .pal 形式 (192 byte) で取得する。 *)
let get_master_palette () =
  uint8array_of_bytes (Emulator.Ppu.get_master_palette nes.ppu)

(** .pal バイト列を読み込んでマスターパレットを差し替える。
    成功なら true、失敗 (サイズ不正等) なら false。 *)
let set_master_palette (arr : Typed_array.uint8Array Js.t) =
  let b = bytes_of_uint8array arr in
  if Emulator.Ppu.set_master_palette nes.ppu b then Js._true else Js._false

(** マスターパレットを内蔵デフォルトに戻す。 *)
let reset_master_palette () = Emulator.Ppu.reset_master_palette nes.ppu

(** マスターインデックス [idx] (0..63) の色を更新する。 *)
let set_master_color (idx : int) (r : int) (g : int) (b : int) =
  Emulator.Ppu.set_master_color nes.ppu idx ~r ~g ~b

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

(** 256×240 RGBA の frame buffer を Uint8Array として取得する.
    毎フレーム呼ばれる前提でコピーは避けたいが、Js_of_ocaml 6.2 では
    Bytes と Uint8Array のゼロコピー連携が無いので毎回コピーする. *)
let get_framebuffer () = uint8array_of_bytes nes.ppu.framebuffer

(* ------------------------------------------------------------------ *)
(* Audio output                                                         *)
(*                                                                     *)
(* AudioContext.sampleRate を渡して downsample 比率を更新し、毎フレーム  *)
(* drain_audio_samples で蓄積サンプルを Float32Array で取り出す.       *)
(* ------------------------------------------------------------------ *)

(* JS の number は wasm_of_ocaml では Js.number_t として渡ってくるので
   明示的に float へ変換する必要がある (= 自動変換は無い). *)
let set_audio_sample_rate (rate : Js.number_t) : unit =
  Emulator.Apu.set_sample_rate nes.apu (Js.float_of_number rate)

(** APU の ring buffer から最大 [max_n] サンプルを取り出して Float32Array に
    詰めて返す. 値域は概ね [0.0, 0.3] (実機 mixer の non-linear 出力). *)
let drain_audio_samples (max_n : int) : Typed_array.float32Array Js.t =
  let arr = Emulator.Apu.drain_samples nes.apu max_n in
  let n = Array.length arr in
  let ja = new%js Typed_array.float32Array n in
  for i = 0 to n - 1 do
    Typed_array.set ja i (Js.number_of_float arr.(i))
  done;
  ja

(* ------------------------------------------------------------------ *)
(* Controller                                                           *)
(*                                                                     *)
(* JS 側からは player (0 = P1, 1 = P2) と button (0..7) を渡す.        *)
(* button index: 0=A, 1=B, 2=Select, 3=Start, 4=Up, 5=Down, 6=Left,    *)
(*                7=Right (Controller.button の variant 順と一致).      *)
(* ------------------------------------------------------------------ *)

let controller_of_player (p : int) : Controller.t option =
  match p with
  | 0 -> Some nes.controller1
  | 1 -> Some nes.controller2
  | _ -> None

let button_of_int (i : int) : Controller.button option =
  match i with
  | 0 -> Some A
  | 1 -> Some B
  | 2 -> Some Select
  | 3 -> Some Start
  | 4 -> Some Up
  | 5 -> Some Down
  | 6 -> Some Left
  | 7 -> Some Right
  | _ -> None

let set_button (player : int) (button : int) (pressed : bool Js.t) : unit =
  match (controller_of_player player, button_of_int button) with
  | Some c, Some b -> Controller.set_button c b (Js.to_bool pressed)
  | _ -> ()

let release_all_buttons () =
  Controller.release_all nes.controller1;
  Controller.release_all nes.controller2

(** OCaml の例外を JS 側に投げる前に Printexc.to_string で名前を
    取り出して console.error に出す。これがないと JS では
    "WebAssembly.Exception {stack: undefined}" としか見えない. *)
let with_error_logging (label : string) (f : unit -> 'a) : 'a =
  try f () with
  | e ->
    let msg = Printexc.to_string e in
    Console.console##error (Js.string ("[" ^ label ^ "] " ^ msg));
    raise e

let run_frame_safe () =
  with_error_logging "runFrame" (fun () -> Nes.run_until_frame nes)

let tick_safe () = with_error_logging "tick" (fun () -> Nes.tick nes)
let reset_safe () = with_error_logging "reset" (fun () -> Nes.reset nes)
let power_on_safe () = with_error_logging "powerOn" (fun () -> Nes.power_on nes)

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

(* battery-backed SRAM の load/save. UI から呼ぶ.
   has_sram は cart info ベース (mapper が prg_ram を持っているか). *)
let has_sram () : bool Js.t =
  match Nes.sram nes with
  | Some _ -> Js._true
  | None -> Js._false

let load_sram (arr : Typed_array.uint8Array Js.t) : bool Js.t =
  let b = bytes_of_uint8array arr in
  if Nes.load_sram nes b then Js._true else Js._false

let save_sram () =
  match Nes.sram nes with
  | None -> Js.null
  | Some b -> Js.some (uint8array_of_bytes b)

(* Quick save/load: 状態全体を Uint8Array で受け渡す. *)
let save_state () : Typed_array.uint8Array Js.t =
  let b = Nes.save_state nes in
  uint8array_of_bytes b

let load_state (arr : Typed_array.uint8Array Js.t) : bool Js.t =
  let b = bytes_of_uint8array arr in
  if Nes.load_state nes b then Js._true else Js._false

let () = Printexc.record_backtrace true

let () =
  Js.export
    "FamiCaml"
    (object%js
       val loadRom = Js.wrap_callback load_rom
       val hasSram = Js.wrap_callback has_sram
       val loadSram = Js.wrap_callback load_sram
       val saveSram = Js.wrap_callback save_sram
       val saveState = Js.wrap_callback save_state
       val loadState = Js.wrap_callback load_state
       val eject = Js.wrap_callback (fun () -> Nes.eject nes)
       val reset = Js.wrap_callback reset_safe
       val powerOn = Js.wrap_callback power_on_safe
       val powerOff = Js.wrap_callback (fun () -> Nes.power_off nes)
       val state = Js.wrap_callback state_js
       val patternTable = Js.wrap_callback pattern_table_js
       val runFrame = Js.wrap_callback run_frame_safe
       val tick = Js.wrap_callback tick_safe
       val getFramebuffer = Js.wrap_callback get_framebuffer
       val setAudioSampleRate = Js.wrap_callback set_audio_sample_rate
       val drainAudioSamples = Js.wrap_callback drain_audio_samples

       val setButton =
         Js.wrap_callback (fun player button pressed ->
           set_button player button pressed)

       val releaseAllButtons = Js.wrap_callback release_all_buttons
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
