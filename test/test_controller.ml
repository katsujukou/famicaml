module C = Emulator.Controller
open C

(* ------------------------------------------------------------------ *)
(* 初期状態                                                            *)
(* ------------------------------------------------------------------ *)

let test_mk_all_released () =
  let c = C.mk () in
  Alcotest.(check bool) "A" false (C.get_button c A);
  Alcotest.(check bool) "B" false (C.get_button c B);
  Alcotest.(check bool) "Select" false (C.get_button c Select);
  Alcotest.(check bool) "Start" false (C.get_button c Start);
  Alcotest.(check bool) "Up" false (C.get_button c Up);
  Alcotest.(check bool) "Down" false (C.get_button c Down);
  Alcotest.(check bool) "Left" false (C.get_button c Left);
  Alcotest.(check bool) "Right" false (C.get_button c Right)

(* ------------------------------------------------------------------ *)
(* set_button / get_button                                            *)
(* ------------------------------------------------------------------ *)

let test_set_get_button () =
  let c = C.mk () in
  C.set_button c A true;
  C.set_button c Right true;
  Alcotest.(check bool) "A pressed" true (C.get_button c A);
  Alcotest.(check bool) "Right pressed" true (C.get_button c Right);
  Alcotest.(check bool) "B not pressed" false (C.get_button c B);
  C.set_button c A false;
  Alcotest.(check bool) "A released" false (C.get_button c A)

let test_release_all () =
  let c = C.mk () in
  C.set_button c A true;
  C.set_button c B true;
  C.set_button c Start true;
  C.release_all c;
  Alcotest.(check bool) "A" false (C.get_button c A);
  Alcotest.(check bool) "B" false (C.get_button c B);
  Alcotest.(check bool) "Start" false (C.get_button c Start)

(* ------------------------------------------------------------------ *)
(* Strobe + shift register protocol                                    *)
(*                                                                     *)
(* 標準的なシーケンス:                                                  *)
(*   write $4016 = 1   (strobe high)                                   *)
(*   write $4016 = 0   (strobe low → latch)                            *)
(*   read $4016 × 8 → A B Select Start Up Down Left Right の順         *)
(* ------------------------------------------------------------------ *)

let test_shift_sequence () =
  let c = C.mk () in
  C.set_button c A true;
  C.set_button c Start true;
  C.set_button c Right true;
  C.write_strobe c 1;
  C.write_strobe c 0;
  (* read 8 回: A B Select Start Up Down Left Right の順 *)
  Alcotest.(check int) "A" 1 (C.read c);
  Alcotest.(check int) "B" 0 (C.read c);
  Alcotest.(check int) "Select" 0 (C.read c);
  Alcotest.(check int) "Start" 1 (C.read c);
  Alcotest.(check int) "Up" 0 (C.read c);
  Alcotest.(check int) "Down" 0 (C.read c);
  Alcotest.(check int) "Left" 0 (C.read c);
  Alcotest.(check int) "Right" 1 (C.read c)

let test_overshoot_returns_1 () =
  let c = C.mk () in
  C.write_strobe c 1;
  C.write_strobe c 0;
  (* 8 回読んでから先 *)
  for _ = 1 to 8 do
    let _ = C.read c in
    ()
  done;
  Alcotest.(check int) "9th read = 1" 1 (C.read c);
  Alcotest.(check int) "10th read = 1" 1 (C.read c)

let test_strobe_high_returns_a () =
  let c = C.mk () in
  C.set_button c A true;
  C.write_strobe c 1;
  (* strobe high の間は A 現状態を継続的に返す *)
  Alcotest.(check int) "A=1" 1 (C.read c);
  Alcotest.(check int) "A=1 again" 1 (C.read c);
  C.set_button c A false;
  Alcotest.(check int) "A=0 (real-time)" 0 (C.read c)

let test_latch_is_snapshot () =
  let c = C.mk () in
  C.set_button c A true;
  C.write_strobe c 1;
  C.write_strobe c 0;
  (* 1 bit 取り出した後で A を離しても、latch された値はそのまま *)
  Alcotest.(check int) "first A=1" 1 (C.read c);
  C.set_button c A false;
  (* B を押してもシフトレジスタには影響しない (= 既に latch 済み) *)
  C.set_button c B true;
  Alcotest.(check int) "second B=0 (latched snapshot)" 0 (C.read c)

let test_strobe_only_bit_0 () =
  let c = C.mk () in
  C.set_button c A true;
  (* bit 0 以外は無視されるはず *)
  C.write_strobe c 0xFE;
  (* これは strobe = 0 とみなされる *)
  (* 直前まで strobe = false なので falling edge なし、latch されない *)
  C.write_strobe c 1;
  (* strobe = 1 (high) *)
  C.write_strobe c 0xFE;
  (* high → low なら latch、$FE は bit 0 = 0 *)
  Alcotest.(check int) "A latched after FE write" 1 (C.read c)

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "Controller"
    [ ( "初期 / set / release"
      , [ Alcotest.test_case "mk: 全 release" `Quick test_mk_all_released
        ; Alcotest.test_case "set/get round trip" `Quick test_set_get_button
        ; Alcotest.test_case "release_all" `Quick test_release_all
        ] )
    ; ( "Strobe / shift register"
      , [ Alcotest.test_case "8 bit shift order" `Quick test_shift_sequence
        ; Alcotest.test_case "9 回目以降は 1" `Quick test_overshoot_returns_1
        ; Alcotest.test_case
            "strobe high で A 現状態"
            `Quick
            test_strobe_high_returns_a
        ; Alcotest.test_case "latch 後の押し直しは反映されない" `Quick test_latch_is_snapshot
        ; Alcotest.test_case "bit 0 以外は無視" `Quick test_strobe_only_bit_0
        ] )
    ]
