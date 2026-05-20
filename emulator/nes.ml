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
  | Rom.Cartridge.NROM { prg; _ } -> { read = prg_read_fixed prg; write = (fun _ _ -> ()) }
  | Rom.Cartridge.CNROM { prg; _ } -> { read = prg_read_fixed prg; write = (fun _ _ -> ()) }
  | Rom.Cartridge.UNROM { prg; _ } ->
    let bank_lo = ref 0 in
    let n_banks = Bytes.length prg / 0x4000 in
    { read = (fun a -> prg_read_unrom prg ~bank_lo:!bank_lo a)
    ; write = (fun _ x -> bank_lo := x mod n_banks)
    }

(* ------------------------------------------------------------------ *)
(* メモリマップ                                                         *)
(* ------------------------------------------------------------------ *)

type memory_section =
  { from : Memory.usize_t (* 0x0000 は u16 上自明なため未読 *)
  ; to_ : Memory.usize_t
  }
[@@warning "-69"]

type memory_map_t =
  { wram : memory_section
  ; prg_rom : memory_section
  }

let memory_map =
  { wram = { from = 0x0000; to_ = 0x1FFF }; prg_rom = { from = 0x8000; to_ = 0xFFFF } }

(* ------------------------------------------------------------------ *)
(* バス組み立て                                                         *)
(* ------------------------------------------------------------------ *)

let mk () =
  let wram = Bytes.create 0x0800 in
  (* バスのクロージャから自分自身 (nes.mapper) を参照するために、
     初期化の二相パターンで自参照を実現する。 *)
  let nes_ref : t option ref = ref None in
  let current_mapper () =
    match !nes_ref with
    | Some n -> n.mapper
    | None -> empty_mapper (* mk の中での read/write は呼ばれない *)
  in
  let bus_read_access p =
    let u = Uint16.to_int p in
    if u <= memory_map.wram.to_
    then Uint8.of_int (Bytes.get_uint8 wram (Uint16.to_int p land 0x07FF))
    else if u >= memory_map.prg_rom.from
    then Uint8.of_int ((current_mapper ()).read (Uint16.to_int p))
    else raise Exn.Out_of_range
  in
  let bus_write_access p x =
    let u = Uint16.to_int p in
    if u <= memory_map.wram.to_
    then Bytes.set_uint8 wram (Uint16.to_int p land 0x07FF) (Uint8.to_int x)
    else if u >= memory_map.prg_rom.from
    then (current_mapper ()).write (Uint16.to_int p) (Uint8.to_int x)
    else raise Exn.Out_of_range
  in
  let bus = Bus.mk ~read:bus_read_access ~write:bus_write_access in
  let cpu = Cpu.mk () in
  let nes =
    { power = false
    ; cart = None
    ; mapper = empty_mapper
    ; cpu
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
  (* 実機ではさらに SP -= 3, P |= I が起きるが、ゲームのリセットコードが
     ほぼ必ず SP/P を初期化し直すため、本実装では PC のみ更新する。
     A/X/Y/SP/P と WRAM はそのまま保持される。 *)
  refresh_vectors nes;
  nes.cpu.reg_PC <- nes.ith_reset

let power_on (nes : t) =
  nes.power <- true;
  reset nes

let power_off (nes : t) = nes.power <- false
