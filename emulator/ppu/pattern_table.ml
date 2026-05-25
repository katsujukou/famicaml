(* パターンテーブル (CHR) のデコード。仕様コメントは .mli 参照。 *)

type tile = int array

let tile_bytes = 16
let tile_pixels = 64
let table_bytes = 4096
let table_dim = 128 (* pixel; = 16 tiles × 8 pixel *)

let decode_tile ~chr ~base_ofs =
  if Bytes.length chr < base_ofs + tile_bytes
  then
    invalid_arg
      (Printf.sprintf
         "Pattern_table.decode_tile: chr length %d < base_ofs %d + %d"
         (Bytes.length chr)
         base_ofs
         tile_bytes);
  let t = Array.make tile_pixels 0 in
  for y = 0 to 7 do
    let lo = Bytes.get_uint8 chr (base_ofs + y) in
    let hi = Bytes.get_uint8 chr (base_ofs + 8 + y) in
    for x = 0 to 7 do
      let shift = 7 - x in
      let lo_bit = (lo lsr shift) land 1 in
      let hi_bit = (hi lsr shift) land 1 in
      t.((y * 8) + x) <- lo_bit lor (hi_bit lsl 1)
    done
  done;
  t

let decode_table ~chr ~table_ofs =
  if Bytes.length chr < table_ofs + table_bytes
  then
    invalid_arg
      (Printf.sprintf
         "Pattern_table.decode_table: chr length %d < table_ofs %d + %d"
         (Bytes.length chr)
         table_ofs
         table_bytes);
  let img = Array.make (table_dim * table_dim) 0 in
  for tile_y = 0 to 15 do
    for tile_x = 0 to 15 do
      let tile_idx = (tile_y * 16) + tile_x in
      let tile =
        decode_tile ~chr ~base_ofs:(table_ofs + (tile_idx * tile_bytes))
      in
      (* タイルを画像座標 (px_x, px_y) = (tile_x*8+x, tile_y*8+y) に貼る *)
      let base_x = tile_x * 8 in
      let base_y = tile_y * 8 in
      for y = 0 to 7 do
        for x = 0 to 7 do
          let dst = ((base_y + y) * table_dim) + base_x + x in
          img.(dst) <- tile.((y * 8) + x)
        done
      done
    done
  done;
  img

(* greyscale: 0 → 0x00, 1 → 0x55, 2 → 0xAA, 3 → 0xFF *)
let greyscale_lut = [| 0x00; 0x55; 0xAA; 0xFF |]

let to_rgba_greyscale pixels =
  let n = Array.length pixels in
  let buf = Bytes.create (n * 4) in
  for i = 0 to n - 1 do
    let p = pixels.(i) land 0b11 in
    let v = greyscale_lut.(p) in
    let o = i * 4 in
    Bytes.set_uint8 buf o v;
    Bytes.set_uint8 buf (o + 1) v;
    Bytes.set_uint8 buf (o + 2) v;
    Bytes.set_uint8 buf (o + 3) 0xFF
  done;
  buf
