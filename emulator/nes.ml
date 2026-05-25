open Famicaml_common.Nesint

(** マッパーの read/write 関数ペア。CPU バスアドレス (0x8000-0xFFFF) を 受け取り、対応するバイトを読み書きする。 *)
type mapper_io =
  { read : int -> int
  ; write : int -> int -> unit
  }

(** カートリッジが挿さっていない時の状態。Open bus は実機では浮動値だが、 本実装では 0 で代用する。 *)
let empty_mapper : mapper_io = { read = (fun _ -> 0); write = (fun _ _ -> ()) }

type t =
  { mutable power : bool
  ; mutable cart : Rom.Cartridge.t option
  ; mutable mapper : mapper_io
  ; cpu : Cpu.t
  ; ppu : Ppu.t
  ; memory_bus : Bus.t
  ; wram : Bytes.t
  ; mutable ith_nmi : uint16
  ; mutable ith_reset : uint16
  ; mutable ith_irq : uint16
  }

(* ------------------------------------------------------------------ *)
(* マッパー: PRG-ROM の読み書きを担当                                   *)
(* ------------------------------------------------------------------ *)

(** NROM / CNROM 共通の PRG リーダ。16KB ならミラー、32KB は素通し。 *)
let prg_read_fixed prg cpu_addr =
  let prg_len = Bytes.length prg in
  let ofs = (cpu_addr - 0x8000) mod prg_len in
  Bytes.get_uint8 prg ofs

(** UNROM: $8000-$BFFF は切替可能バンク、$C000-$FFFF は最終バンク固定。 *)
let prg_read_unrom prg ~bank_lo cpu_addr =
  let n_banks = Bytes.length prg / 0x4000 in
  let bank = if cpu_addr < 0xC000 then bank_lo else n_banks - 1 in
  let ofs = (bank * 0x4000) + (cpu_addr land 0x3FFF) in
  Bytes.get_uint8 prg ofs

(** Cartridge の中身に応じた mapper_io を構築する。 PPU 実装まで CNROM の CHR バンク切替は無視。 *)
let make_mapper (cart : Rom.Cartridge.t) : mapper_io =
  match cart.rom with
  | Rom.Cartridge.NROM { prg; _ } ->
    { read = prg_read_fixed prg; write = (fun _ _ -> ()) }
  | Rom.Cartridge.CNROM { prg; _ } ->
    { read = prg_read_fixed prg; write = (fun _ _ -> ()) }
  | Rom.Cartridge.UNROM { prg; _ } ->
    let bank_lo = ref 0 in
    let n_banks = Bytes.length prg / 0x4000 in
    { read = (fun a -> prg_read_unrom prg ~bank_lo:!bank_lo a)
    ; write = (fun _ x -> bank_lo := x mod n_banks)
    }

(* ------------------------------------------------------------------ *)
(* メモリマップ                                                         *)
(*                                                                     *)
(* CPU bus address space:                                              *)
(*   $0000-$1FFF: WRAM (2KB, mirrored every 2KB)                       *)
(*   $2000-$3FFF: PPU registers (8 bytes, mirrored every 8 bytes)      *)
(*   $4000-$401F: APU & I/O — 未実装、Out_of_range                     *)
(*   $4020-$5FFF: cart expansion — 未実装、Out_of_range                *)
(*   $6000-$7FFF: cart SRAM   — 未実装、Out_of_range                   *)
(*   $8000-$FFFF: cart PRG-ROM (mapper 経由)                           *)
(* ------------------------------------------------------------------ *)

(* ------------------------------------------------------------------ *)
(* バス組み立て                                                         *)
(* ------------------------------------------------------------------ *)

let mk () =
  let wram = Bytes.create 0x0800 in
  (* バスのクロージャから自分自身 (nes.mapper, nes.ppu) を参照するために、
     初期化の二相パターンで自参照を実現する。 *)
  let nes_ref : t option ref = ref None in
  let current_nes () =
    match !nes_ref with
    | Some n -> n
    | None ->
      failwith
        "Nes.mk: bus closure called before nes_ref was set (should not happen)"
  in
  let bus_read_access p =
    let u = Uint16.to_int p in
    if u < 0x2000
    then
      (* WRAM (2KB mirror) *)
      Uint8.of_int (Bytes.get_uint8 wram (u land 0x07FF))
    else if u < 0x4000
    then
      (* PPU registers (8 byte mirror; addr land 0x07 で dispatch) *)
      Ppu.cpu_read (current_nes ()).ppu p
    else if u >= 0x8000
    then Uint8.of_int ((current_nes ()).mapper.read u)
    else raise Exn.Out_of_range
  in
  let bus_write_access p x =
    let u = Uint16.to_int p in
    if u < 0x2000
    then Bytes.set_uint8 wram (u land 0x07FF) (Uint8.to_int x)
    else if u < 0x4000
    then Ppu.cpu_write (current_nes ()).ppu p x
    else if u >= 0x8000
    then (current_nes ()).mapper.write u (Uint8.to_int x)
    else raise Exn.Out_of_range
  in
  let bus = Bus.mk ~read:bus_read_access ~write:bus_write_access in
  let cpu = Cpu.mk () in
  let ppu = Ppu.mk () in
  let nes =
    { power = false
    ; cart = None
    ; mapper = empty_mapper
    ; cpu
    ; ppu
    ; memory_bus = bus
    ; wram
    ; ith_nmi = Uint16.zero
    ; ith_reset = Uint16.zero
    ; ith_irq = Uint16.zero
    }
  in
  nes_ref := Some nes;
  nes

(* ------------------------------------------------------------------ *)
(* カートリッジ操作と RESET                                              *)
(* ------------------------------------------------------------------ *)

let read16_via_bus (nes : t) addr =
  let lo = Uint8.to_int (nes.memory_bus.read (Uint16.of_int addr)) in
  let hi = Uint8.to_int (nes.memory_bus.read (Uint16.of_int (addr + 1))) in
  Uint16.of_int (lo lor (hi lsl 8))

(** $FFFA/$FFFC/$FFFE のベクタを (現在のマッパー経由で) 読み直す。 *)
let refresh_vectors (nes : t) =
  nes.ith_nmi <- read16_via_bus nes 0xFFFA;
  nes.ith_reset <- read16_via_bus nes 0xFFFC;
  nes.ith_irq <- read16_via_bus nes 0xFFFE

let connect_cartridge (nes : t) (cart : Rom.Cartridge.t) =
  nes.cart <- Some cart;
  nes.mapper <- make_mapper cart;
  refresh_vectors nes

let connect (nes : t) (data : bytes) =
  match Rom.Ines.parse data with
  | Error e -> Error e
  | Ok cart ->
    connect_cartridge nes cart;
    Ok ()

let eject (nes : t) =
  nes.cart <- None;
  nes.mapper <- empty_mapper;
  nes.ith_nmi <- Uint16.zero;
  nes.ith_reset <- Uint16.zero;
  nes.ith_irq <- Uint16.zero

(* WRAM および CPU レジスタは敢えて触らない: 256W グリッチを再現するには
     ここで状態を残す必要がある。 *)

let reset (nes : t) =
  refresh_vectors nes;
  (* Pipeline の 7 cycle RESET シーケンスを同期的に消化する。
     A/X/Y/SP/P (の I 以外) と WRAM はそのまま保持される。 *)
  Cpu.request_reset nes.cpu;
  let _ : int = Cpu.step_instruction nes.memory_bus nes.cpu in
  ()

let power_on (nes : t) =
  nes.power <- true;
  reset nes

let power_off (nes : t) = nes.power <- false

(* ------------------------------------------------------------------ *)
(* ロックステップ実行 (Phase A6)                                        *)
(*                                                                     *)
(* CPU 1 cycle = PPU 3 dot。PPU が vblank で nmi_request を立てたら    *)
(* CPU に転写する。run_until_frame は次の vblank 開始まで tick し続ける. *)
(* ------------------------------------------------------------------ *)

(** 1 CPU cycle 進める。PPU も 3 dot 進む。NMI 発火を CPU に転写する。 *)
let tick (nes : t) : unit =
  Cpu.tick nes.memory_bus nes.cpu;
  Ppu.step nes.ppu;
  Ppu.step nes.ppu;
  Ppu.step nes.ppu;
  if nes.ppu.nmi_request
  then (
    nes.ppu.nmi_request <- false;
    Cpu.request_nmi nes.cpu)

(** 次の vblank 開始 (= 1 フレーム完了) まで tick し続ける。 *)
let run_until_frame (nes : t) : unit =
  nes.ppu.frame_complete <- false;
  while not nes.ppu.frame_complete do
    tick nes
  done
