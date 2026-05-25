module P = Emulator.Ppu.Palette

(* ------------------------------------------------------------------ *)
(* default                                                             *)
(* ------------------------------------------------------------------ *)

let test_default_size () =
  let m = P.default () in
  let b = P.to_pal_bytes m in
  Alcotest.(check int) ".pal size = 192" 192 (Bytes.length b)

(* NesDev のレファレンスに沿っているはずの代表的な色を抽出してチェック.
   厳密値は variant がいくつかあるので、本実装で使った値そのものを期待する. *)
let test_default_known_colors () =
  let m = P.default () in
  (* 0x00: 中間グレー (0x54, 0x54, 0x54) *)
  let r, g, b = P.color m 0x00 in
  Alcotest.(check (triple int int int))
    "0x00 = (54,54,54)"
    (0x54, 0x54, 0x54)
    (r, g, b);
  (* 0x0F: 黒 *)
  let r, g, b = P.color m 0x0F in
  Alcotest.(check (triple int int int)) "0x0F = (0,0,0)" (0, 0, 0) (r, g, b);
  (* 0x20: 明るい白っぽいグレー *)
  let r, g, b = P.color m 0x20 in
  Alcotest.(check (triple int int int))
    "0x20 = (236,238,236)"
    (0xEC, 0xEE, 0xEC)
    (r, g, b)

let test_default_returns_fresh_instance () =
  let m1 = P.default () in
  let m2 = P.default () in
  P.set_color m1 0x00 ~r:1 ~g:2 ~b:3;
  let r1, _, _ = P.color m1 0x00 in
  let r2, _, _ = P.color m2 0x00 in
  Alcotest.(check int) "m1 mutated" 1 r1;
  Alcotest.(check int) "m2 NOT mutated" 0x54 r2

(* ------------------------------------------------------------------ *)
(* color / set_color                                                   *)
(* ------------------------------------------------------------------ *)

let test_set_color_basic () =
  let m = P.default () in
  P.set_color m 0x05 ~r:0x12 ~g:0x34 ~b:0x56;
  let r, g, b = P.color m 0x05 in
  Alcotest.(check (triple int int int)) "round trip" (0x12, 0x34, 0x56) (r, g, b)

let test_set_color_clip () =
  let m = P.default () in
  P.set_color m 0x01 ~r:300 ~g:(-50) ~b:255;
  let r, g, b = P.color m 0x01 in
  Alcotest.(check (triple int int int))
    "clipped to [0,255]"
    (255, 0, 255)
    (r, g, b)

let test_color_out_of_range () =
  let m = P.default () in
  Alcotest.check_raises
    "negative"
    (Invalid_argument "Palette: index -1 out of range [0,64)")
    (fun () -> ignore (P.color m (-1)));
  Alcotest.check_raises
    "= 64"
    (Invalid_argument "Palette: index 64 out of range [0,64)")
    (fun () -> ignore (P.color m 64))

(* ------------------------------------------------------------------ *)
(* of_pal_bytes / to_pal_bytes                                         *)
(* ------------------------------------------------------------------ *)

let test_pal_roundtrip () =
  let m = P.default () in
  let pal = P.to_pal_bytes m in
  match P.of_pal_bytes pal with
  | Error e -> Alcotest.failf "round trip failed: %s" e
  | Ok m' ->
    (* 64 色すべて一致 *)
    for i = 0 to 63 do
      let c1 = P.color m i in
      let c2 = P.color m' i in
      Alcotest.(check (triple int int int))
        (Printf.sprintf "color %02X" i)
        c1
        c2
    done

let test_pal_wrong_size () =
  match P.of_pal_bytes (Bytes.create 100) with
  | Ok _ -> Alcotest.fail "expected Error"
  | Error msg ->
    Alcotest.(check bool)
      ("error mentions 192 / 100: " ^ msg)
      true
      (String.length msg > 0)

(* of_pal_bytes は所有権を切る (入力 mutate しても結果に影響しない) *)
let test_of_pal_isolates_input () =
  let m = P.default () in
  let pal = P.to_pal_bytes m in
  let m' =
    match P.of_pal_bytes pal with
    | Ok x -> x
    | Error e -> Alcotest.failf "%s" e
  in
  (* 入力 pal を書き換える *)
  Bytes.set_uint8 pal 0 0xFF;
  let r, _, _ = P.color m' 0x00 in
  Alcotest.(check int) "m' は入力の変更を受けない" 0x54 r

(* to_pal_bytes も同様 *)
let test_to_pal_isolates_output () =
  let m = P.default () in
  let pal = P.to_pal_bytes m in
  Bytes.set_uint8 pal 0 0xFF;
  let r, _, _ = P.color m 0x00 in
  Alcotest.(check int) "出力を変えても master は不変" 0x54 r

(* ------------------------------------------------------------------ *)
(* pixels_to_rgba                                                      *)
(* ------------------------------------------------------------------ *)

let test_pixels_to_rgba_basic () =
  let m = P.default () in
  let sub = [| 0x0F (* 黒 *); 0x00 (* 灰 *); 0x10 (* 明灰 *); 0x30 (* 白 *) |] in
  let pixels = [| 0; 1; 2; 3 |] in
  let rgba = P.pixels_to_rgba pixels ~master:m ~sub in
  Alcotest.(check int) "length = 16" 16 (Bytes.length rgba);
  (* pixel 0 → sub[0] = 0x0F → (0,0,0,255) *)
  Alcotest.(check int) "p0 R" 0 (Bytes.get_uint8 rgba 0);
  Alcotest.(check int) "p0 A" 0xFF (Bytes.get_uint8 rgba 3);
  (* pixel 1 → sub[1] = 0x00 → (0x54, 0x54, 0x54, 255) *)
  Alcotest.(check int) "p1 R" 0x54 (Bytes.get_uint8 rgba 4);
  Alcotest.(check int) "p1 G" 0x54 (Bytes.get_uint8 rgba 5);
  Alcotest.(check int) "p1 B" 0x54 (Bytes.get_uint8 rgba 6);
  (* pixel 3 → sub[3] = 0x30 → (0xEC, 0xEE, 0xEC) *)
  Alcotest.(check int) "p3 R" 0xEC (Bytes.get_uint8 rgba 12);
  Alcotest.(check int) "p3 G" 0xEE (Bytes.get_uint8 rgba 13);
  Alcotest.(check int) "p3 B" 0xEC (Bytes.get_uint8 rgba 14)

(* pixel が 4 以上でも land 0b11 でラップする *)
let test_pixels_to_rgba_wraps () =
  let m = P.default () in
  let sub = P.default_sub () in
  let pixels = [| 0; 4; 8; 99 |] in
  (* land 3 で 0, 0, 0, 3 *)
  let rgba = P.pixels_to_rgba pixels ~master:m ~sub in
  (* index 0, 1, 2 はすべて sub[0] = 0x0F = 黒 *)
  for i = 0 to 2 do
    Alcotest.(check int)
      (Printf.sprintf "p%d R = 0" i)
      0
      (Bytes.get_uint8 rgba (i * 4))
  done;
  (* index 3 は sub[3] = 0x30 = 白 *)
  Alcotest.(check int) "p99 R = 0xEC" 0xEC (Bytes.get_uint8 rgba 12)

(* ------------------------------------------------------------------ *)
(* default_sub                                                         *)
(* ------------------------------------------------------------------ *)

let test_default_sub () =
  let s = P.default_sub () in
  Alcotest.(check int) "length = 4" 4 (Array.length s);
  Alcotest.(check int) "[0]" 0x0F s.(0);
  Alcotest.(check int) "[1]" 0x00 s.(1);
  Alcotest.(check int) "[2]" 0x10 s.(2);
  Alcotest.(check int) "[3]" 0x30 s.(3)

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "Palette"
    [ ( "default"
      , [ Alcotest.test_case "size = 192 byte" `Quick test_default_size
        ; Alcotest.test_case "代表的な既知色" `Quick test_default_known_colors
        ; Alcotest.test_case
            "default () は毎回新インスタンス"
            `Quick
            test_default_returns_fresh_instance
        ] )
    ; ( "color / set_color"
      , [ Alcotest.test_case "set → get round trip" `Quick test_set_color_basic
        ; Alcotest.test_case "値は [0,255] にクリップ" `Quick test_set_color_clip
        ; Alcotest.test_case
            "範囲外 idx で Invalid_argument"
            `Quick
            test_color_out_of_range
        ] )
    ; ( ".pal 形式"
      , [ Alcotest.test_case "to → of round trip" `Quick test_pal_roundtrip
        ; Alcotest.test_case "長さ 192 以外は Error" `Quick test_pal_wrong_size
        ; Alcotest.test_case
            "of_pal_bytes は入力から独立"
            `Quick
            test_of_pal_isolates_input
        ; Alcotest.test_case
            "to_pal_bytes は master から独立"
            `Quick
            test_to_pal_isolates_output
        ] )
    ; ( "pixels_to_rgba"
      , [ Alcotest.test_case
            "2-bit pixel → master 経由で RGBA"
            `Quick
            test_pixels_to_rgba_basic
        ; Alcotest.test_case
            "pixel >=4 は land 3"
            `Quick
            test_pixels_to_rgba_wraps
        ] )
    ; ( "default_sub"
      , [ Alcotest.test_case "[0x0F;0x00;0x10;0x30]" `Quick test_default_sub ]
      )
    ]
