open Famicaml_common.Nesint

type t =
  { prg : Bytes.t
  ; chr : Bytes.t
  ; chr_is_ram : bool
  ; prg_ram : Bytes.t (* 8KB, $6000-$7FFF *)
  ; mutable shift : int (* 5-bit shift register *)
  ; mutable shift_count : int (* 0..4 (= 5 で apply) *)
  ; mutable control : int
  ; (* 5-bit:
       bit 4-3: PRG mode (0/1: 32KB switch, 2: fix first, 3: fix last)
       bit 2:   CHR mode (0: 8KB switch, 1: 4KB×2)
       bit 1-0: mirroring *)
    mutable chr0 : int (* CHR bank 0 (5-bit) *)
  ; mutable chr1 : int (* CHR bank 1 (5-bit) *)
  ; mutable prg_bank : int (* PRG bank (4-bit) *)
  ; mutable prg_ram_enable : bool
  ; set_mirroring : Rom.Cartridge.mirror -> unit
  }

let mirroring_of_bits b : Rom.Cartridge.mirror =
  match b land 0b11 with
  | 0 -> One_screen_lo
  | 1 -> One_screen_hi
  | 2 -> V
  | 3 -> H
  | _ -> assert false

let apply_control t = t.set_mirroring (mirroring_of_bits t.control)

let create ~prg ~chr ~chr_is_ram ~set_mirroring =
  let t =
    { prg
    ; chr
    ; chr_is_ram
    ; prg_ram = Bytes.make 0x2000 '\x00'
    ; shift = 0
    ; shift_count = 0
    ; (* Power-up: PRG mode 3 (= fix last bank, switch 16KB at $8000).
         control = $0C = bits 4-3 = 11. mirror bits = 00 = One_screen_lo. *)
      control = 0x0C
    ; chr0 = 0
    ; chr1 = 0
    ; prg_bank = 0
    ; prg_ram_enable = true
    ; set_mirroring
    }
  in
  (* 起動時の mirroring を即 apply *)
  apply_control t;
  t

(* ------------------------------------------------------------------ *)
(* Shift register protocol                                              *)
(* ------------------------------------------------------------------ *)

let reset_shift t =
  t.shift <- 0;
  t.shift_count <- 0;
  t.control <- t.control lor 0x0C;
  apply_control t

(** Soft reset. shift register + control を起動時状態 (PRG mode 3) へ. *)
let reset t = reset_shift t

(** 5 回目の write で確定した value を target register に書き込む.
    target は最後の write address の bits 14-13 で決定. *)
let apply_register t addr value =
  let target = (addr lsr 13) land 0b11 in
  match target with
  | 0 ->
    t.control <- value land 0x1F;
    apply_control t
  | 1 -> t.chr0 <- value land 0x1F
  | 2 -> t.chr1 <- value land 0x1F
  | 3 ->
    t.prg_bank <- value land 0x0F;
    t.prg_ram_enable <- value land 0x10 = 0
  | _ -> assert false

let load_register t addr byte =
  if byte land 0x80 <> 0
  then reset_shift t
  else (
    t.shift <- t.shift lor ((byte land 1) lsl t.shift_count);
    t.shift_count <- t.shift_count + 1;
    if t.shift_count = 5
    then (
      apply_register t addr t.shift;
      t.shift <- 0;
      t.shift_count <- 0))

(* ------------------------------------------------------------------ *)
(* PRG address resolution                                               *)
(* ------------------------------------------------------------------ *)

let prg_n_banks_16k t = Bytes.length t.prg / 0x4000

(** CPU $8000-$FFFF address → physical PRG offset. *)
let prg_offset t addr =
  let prg_mode = (t.control lsr 2) land 0b11 in
  let n = prg_n_banks_16k t in
  match prg_mode with
  | 0 | 1 ->
    (* 32KB switching, ignore low bit of prg_bank *)
    let bank32 = (t.prg_bank lsr 1) land 0x07 in
    let off = (bank32 * 0x8000) + (addr - 0x8000) in
    off mod Bytes.length t.prg
  | 2 ->
    (* Fix first bank at $8000, switch 16KB at $C000 *)
    let bank = if addr < 0xC000 then 0 else t.prg_bank in
    let off = (bank mod n * 0x4000) + (addr land 0x3FFF) in
    off
  | 3 ->
    (* Fix last bank at $C000, switch 16KB at $8000 *)
    let bank = if addr < 0xC000 then t.prg_bank else n - 1 in
    let off = (bank mod n * 0x4000) + (addr land 0x3FFF) in
    off
  | _ -> assert false

(* ------------------------------------------------------------------ *)
(* CHR address resolution                                               *)
(* ------------------------------------------------------------------ *)

(** PPU $0000-$1FFF address → physical CHR offset. *)
let chr_offset t addr =
  let chr_mode = (t.control lsr 4) land 1 in
  let chr_len = Bytes.length t.chr in
  let off =
    if chr_mode = 0
    then (
      (* 8KB mode: chr0 の bit 0 は無視。8KB bank を一気に切替. *)
      let bank8 = t.chr0 lsr 1 in
      (bank8 * 0x2000) + (addr land 0x1FFF))
    else if
      (* 4KB mode *)
      addr < 0x1000
    then (t.chr0 * 0x1000) + (addr land 0x0FFF)
    else (t.chr1 * 0x1000) + (addr land 0x0FFF)
  in
  if chr_len = 0 then 0 else off mod chr_len

(* ------------------------------------------------------------------ *)
(* CPU bus API                                                          *)
(* ------------------------------------------------------------------ *)

let cpu_read t addr =
  if addr >= 0x6000 && addr < 0x8000
  then if t.prg_ram_enable then Bytes.get_uint8 t.prg_ram (addr - 0x6000) else 0
  else if addr >= 0x8000
  then Bytes.get_uint8 t.prg (prg_offset t addr)
  else 0

let cpu_write t addr byte =
  if addr >= 0x6000 && addr < 0x8000
  then (if t.prg_ram_enable then Bytes.set_uint8 t.prg_ram (addr - 0x6000) byte)
  else if addr >= 0x8000
  then load_register t addr byte

(* ------------------------------------------------------------------ *)
(* PPU bus API (CHR)                                                    *)
(* ------------------------------------------------------------------ *)

let chr_read t addr =
  if Bytes.length t.chr = 0
  then Uint8.zero
  else Uint8.of_int (Bytes.get_uint8 t.chr (chr_offset t addr))

let chr_write t addr byte =
  if t.chr_is_ram && Bytes.length t.chr > 0
  then Bytes.set_uint8 t.chr (chr_offset t addr) (Uint8.to_int byte)
