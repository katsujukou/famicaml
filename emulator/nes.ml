open Famicaml_common.Nesint

(** マッパーの CPU 側 / PPU 側 read/write 関数群。
    - [read]/[write]: CPU バス $8000-$FFFF (PRG-ROM および bank select レジスタ)
    - [chr_read]/[chr_write]: PPU バス $0000-$1FFF (pattern table; CHR-ROM/RAM) *)
type mapper_io =
  { read : int -> int
  ; write : int -> int -> unit
  ; chr_read : int -> uint8
  ; chr_write : int -> uint8 -> unit
  }

(** カートリッジが挿さっていない時の状態。Open bus は実機では浮動値だが、 本実装では 0 で代用する。 *)
let empty_mapper : mapper_io =
  { read = (fun _ -> 0)
  ; write = (fun _ _ -> ())
  ; chr_read = (fun _ -> Uint8.zero)
  ; chr_write = (fun _ _ -> ())
  }

type t =
  { mutable power : bool
  ; mutable cart : Rom.Cartridge.t option
  ; mutable mapper : mapper_io
  ; cpu : Cpu.t
  ; ppu : Ppu.t
  ; apu : Apu.t
  ; controller1 : Controller.t
  ; controller2 : Controller.t
  ; memory_bus : Bus.t
  ; wram : Bytes.t
  ; mutable ith_nmi : uint16
  ; mutable ith_reset : uint16
  ; mutable ith_irq : uint16
  ; mutable dma_source : int option
    (** Some high の間 OAMDMA 待ち。次の {!tick} で消化される。 *)
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

(* CHR 8KB を ofs (任意のbit幅) で読む。実体長で wrap させる. *)
let chr_read_fixed chr ofs =
  let len = Bytes.length chr in
  if len = 0
  then Uint8.zero
  else Uint8.of_int (Bytes.get_uint8 chr (ofs mod len))

let chr_write_fixed chr ofs v =
  let len = Bytes.length chr in
  if len > 0 then Bytes.set_uint8 chr (ofs mod len) (Uint8.to_int v)

let chr_write_noop _ _ = ()

(** Cartridge の中身に応じた mapper_io を構築する。

    - NROM: PRG は 16/32KB 固定、CHR は 8KB ROM (固定).
    - CNROM: PRG 固定、CHR は 8KB×N bank を $8000 write で選択.
    - UNROM: PRG の下位 bank を $8000 write で選択、CHR は 8KB RAM.
    - MMC1: shift register 経由で PRG/CHR バンク切替 + 動的 mirroring +
            8KB PRG RAM ($6000-$7FFF).

    read/write は CPU bus $6000-$FFFF 全域を受け取る (= NROM/CNROM/UNROM 等
    PRG RAM を持たないマッパーでは $6000-$7FFF range を無視する).

    [set_mirroring] は MMC1 等の動的 mirroring 用 callback. *)
let make_mapper
      (cart : Rom.Cartridge.t)
      ~(set_mirroring : Rom.Cartridge.mirror -> unit)
  : mapper_io
  =
  match cart.rom with
  | Rom.Cartridge.NROM { prg; chr } ->
    { read = (fun a -> if a < 0x8000 then 0 else prg_read_fixed prg a)
    ; write = (fun _ _ -> ())
    ; chr_read = chr_read_fixed chr
    ; chr_write = chr_write_noop
    }
  | Rom.Cartridge.CNROM { prg; chr } ->
    let bank_size = 0x2000 in
    let n_banks = max 1 (Bytes.length chr / bank_size) in
    let chr_bank = ref 0 in
    { read = (fun a -> if a < 0x8000 then 0 else prg_read_fixed prg a)
    ; write = (fun a x -> if a >= 0x8000 then chr_bank := x mod n_banks)
    ; chr_read =
        (fun ofs ->
          let off = (!chr_bank * bank_size) + (ofs land 0x1FFF) in
          chr_read_fixed chr off)
    ; chr_write = chr_write_noop
    }
  | Rom.Cartridge.UNROM { prg; chr_ram } ->
    let bank_lo = ref 0 in
    let n_banks = Bytes.length prg / 0x4000 in
    { read =
        (fun a ->
          if a < 0x8000 then 0 else prg_read_unrom prg ~bank_lo:!bank_lo a)
    ; write = (fun a x -> if a >= 0x8000 then bank_lo := x mod n_banks)
    ; chr_read = chr_read_fixed chr_ram
    ; chr_write = chr_write_fixed chr_ram
    }
  | Rom.Cartridge.MMC1 { prg; chr; chr_is_ram } ->
    let m = Mapper.Mmc1.create ~prg ~chr ~chr_is_ram ~set_mirroring in
    { read = (fun a -> Mapper.Mmc1.cpu_read m a)
    ; write = (fun a x -> Mapper.Mmc1.cpu_write m a x)
    ; chr_read = (fun ofs -> Mapper.Mmc1.chr_read m ofs)
    ; chr_write = (fun ofs v -> Mapper.Mmc1.chr_write m ofs v)
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
    else if u = 0x4016
    then Uint8.of_int (Controller.read (current_nes ()).controller1)
    else if u = 0x4017
    then
      (* $4017 read は P2 controller. APU frame counter は write only. *)
      Uint8.of_int (Controller.read (current_nes ()).controller2)
    else if u = 0x4015
    then Apu.cpu_read (current_nes ()).apu p
    else if u < 0x4020
    then
      (* $4000-$4014, $4018-$401F は read 不可 → open bus = 0 *)
      Uint8.zero
    else if u < 0x6000
    then
      (* cart expansion ($4020-$5FFF) — 未実装スタブ *)
      Uint8.zero
    else
      (* $6000-$FFFF: mapper へ (PRG RAM + PRG ROM) *)
      Uint8.of_int ((current_nes ()).mapper.read u)
  in
  let bus_write_access p x =
    let u = Uint16.to_int p in
    if u < 0x2000
    then Bytes.set_uint8 wram (u land 0x07FF) (Uint8.to_int x)
    else if u < 0x4000
    then Ppu.cpu_write (current_nes ()).ppu p x
    else if u = 0x4014
    then
      (* OAMDMA: 次の tick で 256 byte コピー + CPU stall *)
      (current_nes ()).dma_source <- Some (Uint8.to_int x)
    else if u = 0x4016
    then (
      (* $4016 write は P1/P2 共通の strobe ライン. 両方 wire. *)
      let n = current_nes () in
      Controller.write_strobe n.controller1 (Uint8.to_int x);
      Controller.write_strobe n.controller2 (Uint8.to_int x))
    else if u < 0x4018
    then
      (* $4000-$4013, $4015, $4017 はすべて APU へ. *)
      Apu.cpu_write (current_nes ()).apu p x
    else if u < 0x4020
    then ()
    else if u < 0x6000
    then ()
    else (current_nes ()).mapper.write u (Uint8.to_int x)
  in
  let bus = Bus.mk ~read:bus_read_access ~write:bus_write_access in
  let cpu = Cpu.mk () in
  let ppu = Ppu.mk () in
  let apu = Apu.mk () in
  (* APU が DMC DMA で CPU bus を同期的に読むための closure を inject.
     bus_read_access は uint16/uint8 を扱うので int で wrap する. *)
  Apu.connect_bus_reader apu (fun addr ->
    Uint8.to_int (bus_read_access (Uint16.of_int addr)));
  let controller1 = Controller.mk () in
  let controller2 = Controller.mk () in
  let nes =
    { power = false
    ; cart = None
    ; mapper = empty_mapper
    ; cpu
    ; ppu
    ; apu
    ; controller1
    ; controller2
    ; memory_bus = bus
    ; wram
    ; ith_nmi = Uint16.zero
    ; ith_reset = Uint16.zero
    ; ith_irq = Uint16.zero
    ; dma_source = None
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
  let mapper =
    make_mapper cart ~set_mirroring:(fun m -> Ppu.set_mirroring nes.ppu m)
  in
  nes.mapper <- mapper;
  Ppu.connect_cart
    nes.ppu
    ~mirroring:cart.spec.mirroring
    ~chr_io:{ Ppu.chr_read = mapper.chr_read; Ppu.chr_write = mapper.chr_write };
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
  Ppu.disconnect_cart nes.ppu;
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

(** 1 CPU cycle 進める。PPU も 3 dot 進む。NMI 発火を CPU に転写する。

    OAMDMA 待ち (dma_source = Some _) があればこの tick でまとめて消化する:
    - $XX00-$XXFF の 256 byte を順に bus.read → $2004 write で OAM へコピー
    - CPU を 513 (奇数 cycle 開始なら 514) cycle stall させる
    - PPU は stall 中も走る (513 cycle × 3 dot 進める) *)
let tick (nes : t) : unit =
  match nes.dma_source with
  | Some high ->
    nes.dma_source <- None;
    let src_base = high lsl 8 in
    for i = 0 to 255 do
      let b = nes.memory_bus.read (Uint16.of_int (src_base + i)) in
      Ppu.cpu_write nes.ppu (Uint16.of_int 0x2004) b
    done;
    let stall = if nes.cpu.cycles land 1 = 1 then 514 else 513 in
    nes.cpu.cycles <- nes.cpu.cycles + stall;
    for _ = 1 to stall do
      Apu.tick_cpu nes.apu;
      Ppu.step nes.ppu;
      Ppu.step nes.ppu;
      Ppu.step nes.ppu
    done;
    if nes.ppu.nmi_request
    then (
      nes.ppu.nmi_request <- false;
      Cpu.request_nmi nes.cpu);
    if Apu.irq_pending nes.apu then Cpu.request_irq nes.cpu
  | None ->
    Cpu.tick nes.memory_bus nes.cpu;
    Apu.tick_cpu nes.apu;
    Ppu.step nes.ppu;
    Ppu.step nes.ppu;
    Ppu.step nes.ppu;
    if nes.ppu.nmi_request
    then (
      nes.ppu.nmi_request <- false;
      Cpu.request_nmi nes.cpu);
    (* DMC DMA: APU は output cycle 内で bus_reader 経由で同期的に
       byte を取得し、その間の CPU stall (4 cycle) を pending として
       通知してくる. ここで cycle 数を消化する (= CPU cycles 加算 +
       APU/PPU を stall 中も並行進行). *)
    let stall = Apu.take_dmc_pending_stall nes.apu in
    if stall > 0
    then (
      nes.cpu.cycles <- nes.cpu.cycles + stall;
      for _ = 1 to stall do
        Apu.tick_cpu nes.apu;
        Ppu.step nes.ppu;
        Ppu.step nes.ppu;
        Ppu.step nes.ppu
      done);
    (* Frame counter / DMC IRQ は level-triggered (実機通り). CPU の I フラグが
       立っていればブロックされる. Cpu.request_irq は次の命令境界でサービス. *)
    if Apu.irq_pending nes.apu then Cpu.request_irq nes.cpu

(** 次の vblank 開始 (= 1 フレーム完了) まで tick し続ける。 *)
let run_until_frame (nes : t) : unit =
  nes.ppu.frame_complete <- false;
  while not nes.ppu.frame_complete do
    tick nes
  done
