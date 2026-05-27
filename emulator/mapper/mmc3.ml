open Famicaml_common.Nesint

type t =
  { prg : Bytes.t
  ; chr : Bytes.t
  ; chr_is_ram : bool
  ; prg_ram : Bytes.t (* 8KB, $6000-$7FFF *)
  ; (* $8000 bank select *)
    mutable bank_select : int (* 3-bit (R0..R7 を選ぶ) *)
  ; mutable prg_mode : int (* 0: R6@$8000/R7@$A000, 1: 2nd-last@$8000 *)
  ; mutable chr_inv : bool (* CHR A12 inversion *)
  ; r : int array (* R0..R7 (= 8 entry; 各要素を直接 mutate) *)
  ; (* $A001 PRG RAM protect *)
    mutable prg_ram_enable : bool
  ; mutable prg_ram_write_protect : bool
  ; (* IRQ *)
    mutable irq_latch : int
  ; mutable irq_counter : int
  ; mutable irq_reload : bool
  ; mutable irq_enable : bool
  ; mutable irq_flag : bool
  ; set_mirroring : Rom.Cartridge.mirror -> unit
  }

let create ~prg ~chr ~chr_is_ram ~set_mirroring =
  let t =
    { prg
    ; chr
    ; chr_is_ram
    ; prg_ram = Bytes.make 0x2000 '\x00'
    ; bank_select = 0
    ; prg_mode = 0
    ; chr_inv = false
    ; r = Array.make 8 0
    ; prg_ram_enable = true
    ; prg_ram_write_protect = false
    ; irq_latch = 0
    ; irq_counter = 0
    ; irq_reload = false
    ; irq_enable = false
    ; irq_flag = false
    ; set_mirroring
    }
  in
  (* 起動時 mirroring は cartridge spec によるが、念のため V を default で
     apply しない (= Nes.connect_cart で spec.mirroring が apply されるので
     ここでは何もしなくて良い). *)
  t

(* ------------------------------------------------------------------ *)
(* PRG bank resolution                                                  *)
(* ------------------------------------------------------------------ *)

let prg_n_8k_banks t = Bytes.length t.prg / 0x2000

(** CPU $8000-$FFFF → physical PRG offset. *)
let prg_offset t addr =
  let n = prg_n_8k_banks t in
  let last = n - 1 in
  let second_last = n - 2 in
  let r6 = t.r.(6) mod n in
  let r7 = t.r.(7) mod n in
  let bank8k =
    if t.prg_mode = 0
    then (
      (* R6 @ $8000, R7 @ $A000, 2nd-last @ $C000, last @ $E000 *)
      match (addr lsr 13) land 0b11 with
      | 0 -> r6 (* $8000-$9FFF *)
      | 1 -> r7 (* $A000-$BFFF *)
      | 2 -> second_last (* $C000-$DFFF *)
      | _ -> last (* $E000-$FFFF *))
    else (
      (* 2nd-last @ $8000, R7 @ $A000, R6 @ $C000, last @ $E000 *)
      match (addr lsr 13) land 0b11 with
      | 0 -> second_last
      | 1 -> r7
      | 2 -> r6
      | _ -> last)
  in
  (bank8k * 0x2000) + (addr land 0x1FFF)

(* ------------------------------------------------------------------ *)
(* CHR bank resolution                                                  *)
(* ------------------------------------------------------------------ *)

(** PPU $0000-$1FFF → physical CHR offset.
    inv=0: 2K(R0) @ $0000, 2K(R1) @ $0800, 1K(R2..R5) @ $1000..
    inv=1: 1K(R2..R5) @ $0000.., 2K(R0) @ $1000, 2K(R1) @ $1800.

    R0/R1 (2KB) の場合は bit 0 を無視 (= 1KB unit で考えて偶数アドレスに alignment).
    具体的には: 2KB bank b は (b & ~1) と (b & ~1)+1 の連続 1KB bank として扱う. *)
let chr_offset t addr =
  let a = addr land 0x1FFF in
  let chr_len = Bytes.length t.chr in
  (* どの 1KB スロット (= 0..7) かを得る. *)
  let slot = a lsr 10 in
  (* slot に対応する R 番号と 1KB bank 番号 (R に格納されてる値) を返す.
     R0/R1 は 2KB なので 2 slots を占有. *)
  let bank1k =
    match (t.chr_inv, slot) with
    | false, 0 -> t.r.(0) land lnot 1 (* R0 (2K) 前半 *)
    | false, 1 -> t.r.(0) land lnot 1 lor 1 (* R0 (2K) 後半 *)
    | false, 2 -> t.r.(1) land lnot 1
    | false, 3 -> t.r.(1) land lnot 1 lor 1
    | false, 4 -> t.r.(2)
    | false, 5 -> t.r.(3)
    | false, 6 -> t.r.(4)
    | false, 7 -> t.r.(5)
    | true, 0 -> t.r.(2)
    | true, 1 -> t.r.(3)
    | true, 2 -> t.r.(4)
    | true, 3 -> t.r.(5)
    | true, 4 -> t.r.(0) land lnot 1
    | true, 5 -> t.r.(0) land lnot 1 lor 1
    | true, 6 -> t.r.(1) land lnot 1
    | true, 7 -> t.r.(1) land lnot 1 lor 1
    | _ -> 0
  in
  let off = (bank1k * 0x400) + (a land 0x3FF) in
  if chr_len = 0 then 0 else off mod chr_len

(* ------------------------------------------------------------------ *)
(* Register writes                                                      *)
(* ------------------------------------------------------------------ *)

let write_bank_select t value =
  t.chr_inv <- value land 0x80 <> 0;
  t.prg_mode <- (value lsr 6) land 1;
  t.bank_select <- value land 0b111

let write_bank_data t value =
  (* R0/R1 (2KB) は bit 0 を無視 *)
  let v =
    if t.bank_select = 0 || t.bank_select = 1 then value land 0xFE else value
  in
  t.r.(t.bank_select) <- v

let write_mirror t value =
  let m = if value land 1 = 0 then Rom.Cartridge.V else Rom.Cartridge.H in
  t.set_mirroring m

let write_prg_ram_protect t value =
  t.prg_ram_enable <- value land 0x80 <> 0;
  t.prg_ram_write_protect <- value land 0x40 <> 0

let write_irq_latch t value = t.irq_latch <- value land 0xFF
let write_irq_reload t _value = t.irq_reload <- true

let write_irq_disable t _value =
  t.irq_enable <- false;
  t.irq_flag <- false

let write_irq_enable t _value = t.irq_enable <- true

(* ------------------------------------------------------------------ *)
(* CPU bus                                                              *)
(* ------------------------------------------------------------------ *)

let cpu_read t addr =
  if addr >= 0x6000 && addr < 0x8000
  then
    if t.prg_ram_enable
    then Bytes.get_uint8 t.prg_ram (addr - 0x6000)
    else
      (* PRG-RAM disabled: 実機は open bus (= 直前 data bus 値) を返す.
            近似として「アドレスの上位 byte」を返す (大抵の場合これが
            opcode fetch 直後の bus 値と一致する). SMB3 の IRQ handler が
            $7AFF を open bus で参照してるため必要. *)
      (addr lsr 8) land 0xFF
  else if addr >= 0x8000
  then Bytes.get_uint8 t.prg (prg_offset t addr)
  else 0

let cpu_write t addr byte =
  if addr >= 0x6000 && addr < 0x8000
  then (
    if t.prg_ram_enable && not t.prg_ram_write_protect
    then Bytes.set_uint8 t.prg_ram (addr - 0x6000) byte)
  else if addr >= 0x8000
  then (
    (* address bit 13-14 で register pair を選ぶ、bit 0 で even/odd *)
    let pair = (addr lsr 13) land 0b11 in
    let odd = addr land 1 = 1 in
    match (pair, odd) with
    | 0, false -> write_bank_select t byte
    | 0, true -> write_bank_data t byte
    | 1, false -> write_mirror t byte
    | 1, true -> write_prg_ram_protect t byte
    | 2, false -> write_irq_latch t byte
    | 2, true -> write_irq_reload t byte
    | 3, false -> write_irq_disable t byte
    | _ -> write_irq_enable t byte)

(* ------------------------------------------------------------------ *)
(* PPU bus (CHR)                                                        *)
(* ------------------------------------------------------------------ *)

let chr_read t addr =
  if Bytes.length t.chr = 0
  then Uint8.zero
  else Uint8.of_int (Bytes.get_uint8 t.chr (chr_offset t addr))

let chr_write t addr byte =
  if t.chr_is_ram && Bytes.length t.chr > 0
  then Bytes.set_uint8 t.chr (chr_offset t addr) (Uint8.to_int byte)

(* ------------------------------------------------------------------ *)
(* IRQ counter                                                          *)
(* ------------------------------------------------------------------ *)

(** A12 立ち上がりで呼ばれる. NesDev 仕様:
    - counter == 0 OR reload flag set → counter = latch, reload flag clear
    - else → counter--
    - その結果 counter == 0 AND enable=true → irq flag set *)
let on_a12_rise t =
  if t.irq_counter = 0 || t.irq_reload
  then (
    t.irq_counter <- t.irq_latch;
    t.irq_reload <- false)
  else t.irq_counter <- t.irq_counter - 1;
  if t.irq_counter = 0 && t.irq_enable then t.irq_flag <- true

let irq_pending t = t.irq_flag

(** Soft reset. NESdev / Mesen 準拠: MMC3 soft reset は no-op.
    IRQ enable/flag、counter、PRG/CHR bank、mirroring は CPU RESET 信号で
    影響を受けない. ゲーム自身が $E000 等で制御. *)
let reset (_t : t) : unit = ()

(** 8KB PRG-RAM ($6000-$7FFF) への直接参照. SRAM load/save 用. *)
let prg_ram (t : t) : Bytes.t = t.prg_ram
