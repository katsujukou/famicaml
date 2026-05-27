type button =
  | A
  | B
  | Select
  | Start
  | Up
  | Down
  | Left
  | Right

type t =
  { mutable a : bool
  ; mutable b : bool
  ; mutable select : bool
  ; mutable start : bool
  ; mutable up : bool
  ; mutable down : bool
  ; mutable left : bool
  ; mutable right : bool
  ; mutable strobe : bool (* 直近の $4016 bit 0. high の間は latch 継続 *)
  ; mutable shift : int (* 8-bit shift register (LSB 先に出る) *)
  ; mutable read_count : int (* 既に取り出した bit 数 (8 を超えたら 1 を返す) *)
  }

let mk () =
  { a = false
  ; b = false
  ; select = false
  ; start = false
  ; up = false
  ; down = false
  ; left = false
  ; right = false
  ; strobe = false
  ; shift = 0
  ; read_count = 0
  }

let set_button c btn pressed =
  match btn with
  | A -> c.a <- pressed
  | B -> c.b <- pressed
  | Select -> c.select <- pressed
  | Start -> c.start <- pressed
  | Up -> c.up <- pressed
  | Down -> c.down <- pressed
  | Left -> c.left <- pressed
  | Right -> c.right <- pressed

let get_button c btn =
  match btn with
  | A -> c.a
  | B -> c.b
  | Select -> c.select
  | Start -> c.start
  | Up -> c.up
  | Down -> c.down
  | Left -> c.left
  | Right -> c.right

let release_all c =
  c.a <- false;
  c.b <- false;
  c.select <- false;
  c.start <- false;
  c.up <- false;
  c.down <- false;
  c.left <- false;
  c.right <- false

(** 現在のボタン状態をシフトレジスタ用の 8 bit にパックする.
    bit 0 = A, bit 1 = B, bit 2 = Select, bit 3 = Start,
    bit 4 = Up, bit 5 = Down, bit 6 = Left, bit 7 = Right. *)
let pack (c : t) : int =
  let bit b s = if b then 1 lsl s else 0 in
  bit c.a 0
  lor bit c.b 1
  lor bit c.select 2
  lor bit c.start 3
  lor bit c.up 4
  lor bit c.down 5
  lor bit c.left 6
  lor bit c.right 7

let write_strobe (c : t) (byte : int) : unit =
  let new_strobe = byte land 1 = 1 in
  (* high → low の falling edge で latch する. high 中も実機は
     リアルタイムに latch しているので strobe true 時の read は
     常に A 現状態を返す (下記 read 参照). *)
  if c.strobe && not new_strobe
  then (
    c.shift <- pack c;
    c.read_count <- 0);
  c.strobe <- new_strobe

let read (c : t) : int =
  if c.strobe
  then
    (* strobe high: A button の現状態が継続的に返る *)
    if c.a then 1 else 0
  else if c.read_count >= 8
  then 1 (* 9 回目以降は 1 (実機 open bus) *)
  else (
    let bit = c.shift land 1 in
    c.shift <- c.shift lsr 1;
    c.read_count <- c.read_count + 1;
    bit)

(* ------------------------------------------------------------------ *)
(* Quick save/load 用 state serialization                              *)
(* ------------------------------------------------------------------ *)

let serialize (buf : Buffer.t) (c : t) : unit =
  let bit b s = if b then 1 lsl s else 0 in
  let btns =
    bit c.a 0
    lor bit c.b 1
    lor bit c.select 2
    lor bit c.start 3
    lor bit c.up 4
    lor bit c.down 5
    lor bit c.left 6
    lor bit c.right 7
  in
  Buffer.add_char buf (Char.chr btns);
  Buffer.add_char buf (if c.strobe then '\x01' else '\x00');
  Buffer.add_char buf (Char.chr (c.shift land 0xFF));
  Buffer.add_char buf (Char.chr (c.read_count land 0xFF))

let deserialize (b : Bytes.t) (cursor : int ref) (c : t) : unit =
  let get () =
    let v = Bytes.get_uint8 b !cursor in
    incr cursor;
    v
  in
  let btns = get () in
  c.a <- btns land 1 <> 0;
  c.b <- btns land 2 <> 0;
  c.select <- btns land 4 <> 0;
  c.start <- btns land 8 <> 0;
  c.up <- btns land 0x10 <> 0;
  c.down <- btns land 0x20 <> 0;
  c.left <- btns land 0x40 <> 0;
  c.right <- btns land 0x80 <> 0;
  c.strobe <- get () <> 0;
  c.shift <- get ();
  c.read_count <- get ()
