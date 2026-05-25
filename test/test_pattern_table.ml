module PT = Emulator.Ppu.Pattern_table

(* ------------------------------------------------------------------ *)
(* ヘルパー                                                             *)
(* ------------------------------------------------------------------ *)

(** バイト列を生成する: int list → bytes. *)
let bytes_of_ints xs =
  let b = Bytes.create (List.length xs) in
  List.iteri (fun i x -> Bytes.set_uint8 b i x) xs;
  b

(** 8×8 grid を可読な行リストとして表示するための簡易表現 (デバッグ用)。
    実際の比較は flat array で行う。 *)
let _show_tile tile =
  let buf = Buffer.create 80 in
  for y = 0 to 7 do
    for x = 0 to 7 do
      Buffer.add_char buf (Char.chr (Char.code '0' + tile.((y * 8) + x)))
    done;
    Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* decode_tile                                                         *)
(* ------------------------------------------------------------------ *)

(* 全 0 のタイルは全 0 の画素になる *)
let test_decode_tile_all_zero () =
  let chr = Bytes.make 16 '\x00' in
  let tile = PT.decode_tile ~chr ~base_ofs:0 in
  Alcotest.(check int) "length 64" 64 (Array.length tile);
  Array.iter (fun p -> Alcotest.(check int) "pixel = 0" 0 p) tile

(* 低位プレーンだけ全 1、高位 0 → 全画素 = 1 *)
let test_decode_tile_low_plane_only () =
  let chr =
    bytes_of_ints
      [ 0xFF
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ; (* low *)
        0x00
      ; 0x00
      ; 0x00
      ; 0x00
      ; 0x00
      ; 0x00
      ; 0x00
      ; 0x00 (* high *)
      ]
  in
  let tile = PT.decode_tile ~chr ~base_ofs:0 in
  Array.iter (fun p -> Alcotest.(check int) "pixel = 1" 1 p) tile

(* 高位プレーンだけ全 1、低位 0 → 全画素 = 2 *)
let test_decode_tile_high_plane_only () =
  let chr =
    bytes_of_ints
      [ 0x00
      ; 0x00
      ; 0x00
      ; 0x00
      ; 0x00
      ; 0x00
      ; 0x00
      ; 0x00
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ; 0xFF
      ]
  in
  let tile = PT.decode_tile ~chr ~base_ofs:0 in
  Array.iter (fun p -> Alcotest.(check int) "pixel = 2" 2 p) tile

(* 両プレーン全 1 → 全画素 = 3 *)
let test_decode_tile_both_planes () =
  let chr = Bytes.make 16 '\xFF' in
  let tile = PT.decode_tile ~chr ~base_ofs:0 in
  Array.iter (fun p -> Alcotest.(check int) "pixel = 3" 3 p) tile

(* NesDev wiki "PPU pattern tables" の説明例:
     bytes 0-7 (low):  $41 $C2 $44 $48 $10 $20 $40 $80
     bytes 8-15(high): $01 $02 $04 $08 $16 $21 $42 $87
   が以下の画素マップ (.=0, 1, 2, 3) になる:
     .1.....3
     11....3.
     .1...3..
     .1..3...
     ...3.22.
     ..3....2
     .3....2.
     3....222
*)
let test_decode_tile_nesdev_example () =
  let chr =
    bytes_of_ints
      [ 0x41
      ; 0xC2
      ; 0x44
      ; 0x48
      ; 0x10
      ; 0x20
      ; 0x40
      ; 0x80
      ; 0x01
      ; 0x02
      ; 0x04
      ; 0x08
      ; 0x16
      ; 0x21
      ; 0x42
      ; 0x87
      ]
  in
  let tile = PT.decode_tile ~chr ~base_ofs:0 in
  let expected =
    [| 0
     ; 1
     ; 0
     ; 0
     ; 0
     ; 0
     ; 0
     ; 3
     ; 1
     ; 1
     ; 0
     ; 0
     ; 0
     ; 0
     ; 3
     ; 0
     ; 0
     ; 1
     ; 0
     ; 0
     ; 0
     ; 3
     ; 0
     ; 0
     ; 0
     ; 1
     ; 0
     ; 0
     ; 3
     ; 0
     ; 0
     ; 0
     ; 0
     ; 0
     ; 0
     ; 3
     ; 0
     ; 2
     ; 2
     ; 0
     ; 0
     ; 0
     ; 3
     ; 0
     ; 0
     ; 0
     ; 0
     ; 2
     ; 0
     ; 3
     ; 0
     ; 0
     ; 0
     ; 0
     ; 2
     ; 0
     ; 3
     ; 0
     ; 0
     ; 0
     ; 0
     ; 2
     ; 2
     ; 2
    |]
  in
  for i = 0 to 63 do
    Alcotest.(check int)
      (Printf.sprintf "pixel %d (%d,%d)" i (i mod 8) (i / 8))
      expected.(i)
      tile.(i)
  done

(* base_ofs を指定して 2 番目のタイルを取り出せる *)
let test_decode_tile_with_offset () =
  let chr =
    Bytes.cat (Bytes.make 16 '\x00') (* 1 タイル目: 全 0 *) (Bytes.make 16 '\xFF')
    (* 2 タイル目: 全 1 (両プレーン) *)
  in
  let tile = PT.decode_tile ~chr ~base_ofs:16 in
  Array.iter (fun p -> Alcotest.(check int) "pixel = 3" 3 p) tile

(* 範囲外オフセットは Invalid_argument を投げる *)
let test_decode_tile_out_of_range () =
  let chr = Bytes.make 8 '\x00' in
  Alcotest.check_raises
    "out of range"
    (Invalid_argument
       "Pattern_table.decode_tile: chr length 8 < base_ofs 0 + 16")
    (fun () -> ignore (PT.decode_tile ~chr ~base_ofs:0))

(* ------------------------------------------------------------------ *)
(* decode_table                                                        *)
(* ------------------------------------------------------------------ *)

let test_decode_table_size () =
  let chr = Bytes.make 4096 '\x00' in
  let img = PT.decode_table ~chr ~table_ofs:0 in
  Alcotest.(check int) "128x128 = 16384" 16384 (Array.length img);
  Array.iter (fun p -> Alcotest.(check int) "all 0" 0 p) img

(* タイル (0,0) と (15,15) が画像のどこに対応するかを検証する。
   タイル N をすべて palette index 3 で埋めて、画像座標 [tile_y*8 .. +7]
   × [tile_x*8 .. +7] が 3 になることを確認する。 *)
let test_decode_table_tile_placement () =
  let chr = Bytes.make 4096 '\x00' in
  (* tile (3, 5) = タイル番号 3*16 + 5 = 53 のオフセット = 53 * 16 = 848 *)
  let tile_x, tile_y = (5, 3) in
  let tile_idx = (tile_y * 16) + tile_x in
  let off = tile_idx * 16 in
  (* 両プレーン全 1 → 全画素 = 3 *)
  for i = 0 to 15 do
    Bytes.set_uint8 chr (off + i) 0xFF
  done;
  let img = PT.decode_table ~chr ~table_ofs:0 in
  (* 画像内の対応領域 *)
  for y = 0 to 7 do
    for x = 0 to 7 do
      let px = (tile_x * 8) + x in
      let py = (tile_y * 8) + y in
      let v = img.((py * 128) + px) in
      Alcotest.(check int) (Printf.sprintf "img[%d,%d]" px py) 3 v
    done
  done;
  (* 周囲のタイルは 0 のまま *)
  Alcotest.(check int) "img[0,0]" 0 img.(0);
  Alcotest.(check int) "img[127,127]" 0 img.(127 + (127 * 128))

let test_decode_table_with_offset () =
  let chr = Bytes.make 8192 '\x00' in
  (* 2 番目のテーブル (table_ofs = 4096) の先頭タイルを全 3 にする *)
  for i = 0 to 15 do
    Bytes.set_uint8 chr (4096 + i) 0xFF
  done;
  let img = PT.decode_table ~chr ~table_ofs:4096 in
  for i = 0 to 63 do
    let y = i / 8
    and x = i mod 8 in
    Alcotest.(check int) (Printf.sprintf "img[%d,%d]" x y) 3 img.((y * 128) + x)
  done

(* ------------------------------------------------------------------ *)
(* to_rgba_greyscale                                                   *)
(* ------------------------------------------------------------------ *)

let test_rgba_greyscale_palette () =
  let pixels = [| 0; 1; 2; 3 |] in
  let rgba = PT.to_rgba_greyscale pixels in
  Alcotest.(check int) "length = 16" 16 (Bytes.length rgba);
  (* palette 0 → (0,0,0,255) *)
  Alcotest.(check int) "0 R" 0x00 (Bytes.get_uint8 rgba 0);
  Alcotest.(check int) "0 A" 0xFF (Bytes.get_uint8 rgba 3);
  (* palette 1 → (0x55, 0x55, 0x55, 255) *)
  Alcotest.(check int) "1 R" 0x55 (Bytes.get_uint8 rgba 4);
  Alcotest.(check int) "1 G" 0x55 (Bytes.get_uint8 rgba 5);
  Alcotest.(check int) "1 B" 0x55 (Bytes.get_uint8 rgba 6);
  Alcotest.(check int) "1 A" 0xFF (Bytes.get_uint8 rgba 7);
  (* palette 2 → 0xAA *)
  Alcotest.(check int) "2 R" 0xAA (Bytes.get_uint8 rgba 8);
  (* palette 3 → 0xFF *)
  Alcotest.(check int) "3 R" 0xFF (Bytes.get_uint8 rgba 12);
  Alcotest.(check int) "3 G" 0xFF (Bytes.get_uint8 rgba 13);
  Alcotest.(check int) "3 B" 0xFF (Bytes.get_uint8 rgba 14);
  Alcotest.(check int) "3 A" 0xFF (Bytes.get_uint8 rgba 15)

let test_rgba_greyscale_table_size () =
  let chr = Bytes.make 4096 '\x00' in
  let img = PT.decode_table ~chr ~table_ofs:0 in
  let rgba = PT.to_rgba_greyscale img in
  Alcotest.(check int) "128*128*4 = 65536" 65536 (Bytes.length rgba)

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "Pattern Table"
    [ ( "decode_tile"
      , [ Alcotest.test_case
            "全 0 → 全 palette 0"
            `Quick
            test_decode_tile_all_zero
        ; Alcotest.test_case
            "低位だけ 1 → 全 palette 1"
            `Quick
            test_decode_tile_low_plane_only
        ; Alcotest.test_case
            "高位だけ 1 → 全 palette 2"
            `Quick
            test_decode_tile_high_plane_only
        ; Alcotest.test_case
            "両プレーン 1 → 全 palette 3"
            `Quick
            test_decode_tile_both_planes
        ; Alcotest.test_case
            "NesDev wiki 例"
            `Quick
            test_decode_tile_nesdev_example
        ; Alcotest.test_case
            "offset 指定 (2 タイル目)"
            `Quick
            test_decode_tile_with_offset
        ; Alcotest.test_case
            "範囲外で Invalid_argument"
            `Quick
            test_decode_tile_out_of_range
        ] )
    ; ( "decode_table"
      , [ Alcotest.test_case "128×128 = 16384 要素" `Quick test_decode_table_size
        ; Alcotest.test_case
            "タイル配置 (3,5)"
            `Quick
            test_decode_table_tile_placement
        ; Alcotest.test_case "table_ofs 指定" `Quick test_decode_table_with_offset
        ] )
    ; ( "to_rgba_greyscale"
      , [ Alcotest.test_case
            "palette → RGBA 色対応"
            `Quick
            test_rgba_greyscale_palette
        ; Alcotest.test_case
            "128×128 → 65536 bytes"
            `Quick
            test_rgba_greyscale_table_size
        ] )
    ]
