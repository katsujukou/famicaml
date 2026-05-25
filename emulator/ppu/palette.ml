(* NES マスターパレット (64 色) と 4 色サブパレット。仕様は .mli 参照。 *)

type t = bytes (* 192 byte; RGB triple × 64 *)

let n_colors = 64
let pal_bytes = n_colors * 3

(* 既定パレット。NesDev wiki "PPU palettes" のレファレンス値ベース。
   各行 16 色 × 4 行 = 64 色、各色 R G B の 3 byte。
   コメント側は通し index (16 進)。 *)
let default_pal_string =
  String.concat
    ""
    [ (* 0x00 .. 0x0F *)
      "\x54\x54\x54\x00\x1E\x74\x08\x10\x90\x30\x00\x88"
    ; "\x44\x00\x64\x5C\x00\x30\x54\x04\x00\x3C\x18\x00"
    ; "\x20\x2A\x00\x08\x3A\x00\x00\x40\x00\x00\x3C\x00"
    ; "\x00\x32\x3C\x00\x00\x00\x00\x00\x00\x00\x00\x00"
    ; (* 0x10 .. 0x1F *)
      "\x98\x96\x98\x08\x4C\xC4\x30\x32\xEC\x5C\x1E\xE4"
    ; "\x88\x14\xB0\xA0\x14\x64\x98\x22\x20\x78\x3C\x00"
    ; "\x54\x5A\x00\x28\x72\x00\x08\x7C\x00\x00\x76\x28"
    ; "\x00\x66\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00"
    ; (* 0x20 .. 0x2F *)
      "\xEC\xEE\xEC\x4C\x9A\xEC\x78\x7C\xEC\xB0\x62\xEC"
    ; "\xE4\x54\xEC\xEC\x58\xB4\xEC\x6A\x64\xD4\x88\x20"
    ; "\xA0\xAA\x00\x74\xC4\x00\x4C\xD0\x20\x38\xCC\x6C"
    ; "\x38\xB4\xCC\x3C\x3C\x3C\x00\x00\x00\x00\x00\x00"
    ; (* 0x30 .. 0x3F *)
      "\xEC\xEE\xEC\xA8\xCC\xEC\xBC\xBC\xEC\xD4\xB2\xEC"
    ; "\xEC\xAE\xEC\xEC\xAE\xD4\xEC\xB4\xB0\xE4\xC4\x90"
    ; "\xCC\xD2\x78\xB4\xDE\x78\xA8\xE2\x90\x98\xE2\xB4"
    ; "\xA0\xD6\xE4\xA0\xA2\xA0\x00\x00\x00\x00\x00\x00"
    ]

let () = assert (String.length default_pal_string = pal_bytes)
let default () = Bytes.of_string default_pal_string

let check_idx idx =
  if idx < 0 || idx >= n_colors
  then
    invalid_arg
      (Printf.sprintf "Palette: index %d out of range [0,%d)" idx n_colors)

let color m idx =
  check_idx idx;
  let off = idx * 3 in
  let r = Bytes.get_uint8 m off in
  let g = Bytes.get_uint8 m (off + 1) in
  let b = Bytes.get_uint8 m (off + 2) in
  (r, g, b)

let clip x = if x < 0 then 0 else if x > 255 then 255 else x

let set_color m idx ~r ~g ~b =
  check_idx idx;
  let off = idx * 3 in
  Bytes.set_uint8 m off (clip r);
  Bytes.set_uint8 m (off + 1) (clip g);
  Bytes.set_uint8 m (off + 2) (clip b)

let of_pal_bytes b =
  let len = Bytes.length b in
  if len <> pal_bytes
  then
    Error
      (Printf.sprintf ".pal must be exactly %d bytes (got %d)" pal_bytes len)
  else
    (* 入力をコピーして所有権を切る *)
    Ok (Bytes.copy b)

let to_pal_bytes m = Bytes.copy m

type sub = int array

let default_sub () = [| 0x0F; 0x00; 0x10; 0x30 |]

let pixels_to_rgba pixels ~master ~sub =
  let n = Array.length pixels in
  let out = Bytes.create (n * 4) in
  for i = 0 to n - 1 do
    let p = pixels.(i) land 0b11 in
    let master_idx = sub.(p) land 0x3F in
    let off = master_idx * 3 in
    let r = Bytes.get_uint8 master off in
    let g = Bytes.get_uint8 master (off + 1) in
    let b = Bytes.get_uint8 master (off + 2) in
    let o = i * 4 in
    Bytes.set_uint8 out o r;
    Bytes.set_uint8 out (o + 1) g;
    Bytes.set_uint8 out (o + 2) b;
    Bytes.set_uint8 out (o + 3) 0xFF
  done;
  out
