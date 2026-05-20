open Js_of_ocaml
open Famicaml_common.Nesint
module Ines = Emulator.Rom.Ines
module Cart = Emulator.Rom.Cartridge
module Nes = Emulator.Nes

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

let mirror_to_string = function
  | Cart.H -> "horizontal"
  | Cart.V -> "vertical"

let rom_summary = function
  | Cart.NROM { prg; chr } -> ("NROM", Bytes.length prg, Bytes.length chr)
  | Cart.UNROM { prg; chr_ram } -> ("UNROM", Bytes.length prg, Bytes.length chr_ram)
  | Cart.CNROM { prg; chr } -> ("CNROM", Bytes.length prg, Bytes.length chr)

(* ------------------------------------------------------------------ *)
(* グローバル NES インスタンス                                          *)
(* ------------------------------------------------------------------ *)

let nes : Nes.t = Nes.mk ()

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
  end

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
    end)
