open Famicaml_common.Nesint
module A = Emulator.Apu

let u8 = Uint8.of_int
let u16 = Uint16.of_int
let write apu addr byte = A.cpu_write apu (u16 addr) (u8 byte)
let read apu addr = Uint8.to_int (A.cpu_read apu (u16 addr))

(* ------------------------------------------------------------------ *)
(* mk: 初期状態は全 channel disabled、IRQ なし                          *)
(* ------------------------------------------------------------------ *)

let test_mk_initial () =
  let apu = A.mk () in
  Alcotest.(check bool) "no IRQ pending" false (A.irq_pending apu);
  Alcotest.(check int) "$4015 = 0" 0 (read apu 0x4015)

(* ------------------------------------------------------------------ *)
(* $4015 write で Pulse の length counter が enable/disable で reset    *)
(* ------------------------------------------------------------------ *)

let test_enable_pulse1_via_4015 () =
  let apu = A.mk () in
  (* Pulse 1 を enable してから timer_hi (= length load) を書く *)
  write apu 0x4015 0x01;
  (* enable pulse1 *)
  write apu 0x4003 0b0000_1000;
  (* length idx = 1 → length = 254 *)
  let s = read apu 0x4015 in
  Alcotest.(check int) "$4015 bit 0 (pulse1 length > 0)" 1 (s land 1);
  (* disable で length counter が即 0 になる *)
  write apu 0x4015 0x00;
  let s2 = read apu 0x4015 in
  Alcotest.(check int) "$4015 bit 0 = 0 after disable" 0 (s2 land 1)

let test_enable_pulse2_via_4015 () =
  let apu = A.mk () in
  write apu 0x4015 0x02;
  write apu 0x4007 0b0000_1000;
  let s = read apu 0x4015 in
  Alcotest.(check int) "$4015 bit 1 (pulse2)" 0b10 (s land 0b10)

(* ------------------------------------------------------------------ *)
(* Frame counter IRQ (4-step mode)                                     *)
(*                                                                     *)
(* 29830 CPU cycle で IRQ flag が立つはず. inhibit (bit 6) で抑止可.   *)
(* ------------------------------------------------------------------ *)

let test_frame_irq_4step () =
  let apu = A.mk () in
  (* mode = 4-step, IRQ inhibit = 0 *)
  write apu 0x4017 0x00;
  (* 29830 cycle 進める *)
  for _ = 1 to 29830 do
    A.tick_cpu apu
  done;
  Alcotest.(check bool) "IRQ pending @ 29830" true (A.irq_pending apu);
  (* $4015 read で IRQ flag クリア *)
  let s = read apu 0x4015 in
  Alcotest.(check int) "bit 6 was set" 0b0100_0000 (s land 0b0100_0000);
  Alcotest.(check bool) "IRQ cleared after $4015 read" false (A.irq_pending apu)

let test_frame_irq_inhibit () =
  let apu = A.mk () in
  (* IRQ inhibit = 1 *)
  write apu 0x4017 0b0100_0000;
  for _ = 1 to 30000 do
    A.tick_cpu apu
  done;
  Alcotest.(check bool) "no IRQ (inhibit)" false (A.irq_pending apu)

let test_frame_irq_inhibit_clears_flag () =
  let apu = A.mk () in
  write apu 0x4017 0x00;
  for _ = 1 to 29830 do
    A.tick_cpu apu
  done;
  Alcotest.(check bool) "IRQ pending" true (A.irq_pending apu);
  (* inhibit set すると即 IRQ flag クリア *)
  write apu 0x4017 0b0100_0000;
  Alcotest.(check bool) "IRQ cleared by inhibit" false (A.irq_pending apu)

(* ------------------------------------------------------------------ *)
(* 5-step mode: IRQ なし                                                *)
(* ------------------------------------------------------------------ *)

let test_5step_no_irq () =
  let apu = A.mk () in
  write apu 0x4017 0b1000_0000;
  (* 5-step *)
  for _ = 1 to 40000 do
    A.tick_cpu apu
  done;
  Alcotest.(check bool) "no IRQ in 5-step" false (A.irq_pending apu)

(* ------------------------------------------------------------------ *)
(* Length counter が half frame ごとに decrement される                *)
(* ------------------------------------------------------------------ *)

let test_length_counter_decrement () =
  let apu = A.mk () in
  write apu 0x4015 0x01;
  (* pulse1 enable *)
  (* length_halt = false (= デクリメントされる), envelope ctrl はそのまま *)
  write apu 0x4000 0x00;
  write apu 0x4003 0b0000_0000;
  (* length idx = 0 → length = 10 *)
  let initial_bit = read apu 0x4015 land 1 in
  Alcotest.(check int) "length > 0 initially" 1 initial_bit;
  (* 1 half frame = 14913 cycle 経過まで進める. 14914 cycle くらいで 1 回 dec *)
  for _ = 1 to 14914 do
    A.tick_cpu apu
  done;
  (* length: 10 → 9, まだ > 0 *)
  let bit = read apu 0x4015 land 1 in
  Alcotest.(check int) "length > 0 after 1 half frame" 1 bit

let test_length_halt () =
  let apu = A.mk () in
  write apu 0x4015 0x01;
  (* length_halt = true (bit 5) *)
  write apu 0x4000 0b0010_0000;
  write apu 0x4003 0b0000_0000;
  (* length = 10 *)
  (* 100 half frame 分進めても length は decrement されない *)
  for _ = 1 to 14913 * 30 do
    A.tick_cpu apu
  done;
  Alcotest.(check int) "length > 0 with halt" 1 (read apu 0x4015 land 1)

(* ------------------------------------------------------------------ *)
(* Triangle channel                                                    *)
(* ------------------------------------------------------------------ *)

let test_triangle_enable_via_4015 () =
  let apu = A.mk () in
  write apu 0x4015 0x04;
  (* triangle enable *)
  write apu 0x4008 0x7F;
  (* linear counter reload = 127, control = 0 *)
  write apu 0x400A 0x10;
  (* timer lo *)
  write apu 0x400B 0b0000_0000;
  (* length idx = 0 → length = 10, timer hi = 0 *)
  let s = read apu 0x4015 in
  Alcotest.(check int) "$4015 bit 2 (triangle length > 0)" 0b100 (s land 0b100);
  write apu 0x4015 0x00;
  let s2 = read apu 0x4015 in
  Alcotest.(check int) "$4015 bit 2 = 0 after disable" 0 (s2 land 0b100)

let test_triangle_linear_counter_decrement () =
  let apu = A.mk () in
  write apu 0x4015 0x04;
  (* control = 0 (= reload flag を 1 回 reload 後にクリア → decrement モード),
     linear reload = 10 *)
  write apu 0x4008 0x0A;
  write apu 0x400A 0xFF;
  write apu 0x400B 0b0000_0111;
  (* length=10, timer hi = 7 *)
  (* 1 quarter frame = 7457 cycle で reload (10), control=0 なので reload flag
     はその後 clear される. 2 quarter で 10 → 9. *)
  for _ = 1 to 14913 do
    A.tick_cpu apu
  done;
  (* linear counter は外から見えないので、ここでは「ちゃんと clock されていれば
     timer は進んでいるはず」だけ verify. ここは smoke test. *)
  let _ = read apu 0x4015 in
  Alcotest.(check pass) "no crash" () ()

let test_triangle_short_period_mute () =
  let apu = A.mk () in
  write apu 0x4015 0x04;
  write apu 0x4008 0x7F;
  (* control = 0, linear reload = 127 *)
  write apu 0x400A 0x01;
  (* timer lo = 1 *)
  write apu 0x400B 0b0000_0000;
  (* timer hi = 0 → timer_period = 1 (< 2). length idx = 0 → length = 10. *)
  (* triangle_output は 0 (= mute) のはず. ただし API 経由では verify できないので
     smoke. *)
  for _ = 1 to 100 do
    A.tick_cpu apu
  done;
  Alcotest.(check pass) "no crash" () ()

(* ------------------------------------------------------------------ *)
(* Noise channel                                                       *)
(* ------------------------------------------------------------------ *)

let test_noise_enable_via_4015 () =
  let apu = A.mk () in
  write apu 0x4015 0x08;
  (* noise enable *)
  write apu 0x400C 0b0001_0001;
  (* const vol = 1, vol = 1 *)
  write apu 0x400E 0x00;
  (* mode 0, period idx 0 *)
  write apu 0x400F 0b0000_1000;
  (* length idx = 1 → 254 *)
  let s = read apu 0x4015 in
  Alcotest.(check int) "$4015 bit 3 (noise length > 0)" 0b1000 (s land 0b1000);
  write apu 0x4015 0x00;
  let s2 = read apu 0x4015 in
  Alcotest.(check int) "$4015 bit 3 = 0 after disable" 0 (s2 land 0b1000)

let test_noise_lfsr_changes () =
  let apu = A.mk () in
  write apu 0x4015 0x08;
  write apu 0x400C 0b0001_1111;
  write apu 0x400E 0x00;
  (* mode 0, period idx 0 = 4 APU clocks = 8 CPU cycles *)
  write apu 0x400F 0b0000_1000;
  (* 大量 tick で LFSR が動いて出力が変化するはず. smoke. *)
  for _ = 1 to 1000 do
    A.tick_cpu apu
  done;
  Alcotest.(check pass) "no crash" () ()

(* ------------------------------------------------------------------ *)
(* DMC channel                                                         *)
(* ------------------------------------------------------------------ *)

let test_dmc_direct_dac_write () =
  let apu = A.mk () in
  (* $4011 は DAC counter を直接書き換える. mixer の出力に影響. *)
  write apu 0x4011 0x40;
  (* 値は内部だが、$4015 read で activity は分からないので smoke *)
  Alcotest.(check pass) "no crash" () ()

(* Bus reader を inject して DMC を動かすヘルパ. fixed byte を返す. *)
let with_bus_reader apu byte = A.connect_bus_reader apu (fun _addr -> byte)

let test_dmc_enable_and_dma_request () =
  let apu = A.mk () in
  with_bus_reader apu 0xAA;
  write apu 0x4012 0x00;
  write apu 0x4013 0x00;
  write apu 0x4010 0x00;
  write apu 0x4015 0x10;
  (* enable → write_status の memory reader 起動で即 1 byte DMA + 4 cycle stall *)
  let stall = A.take_dmc_pending_stall apu in
  Alcotest.(check int) "stall = 4" 4 stall;
  Alcotest.(check int) "stall reset" 0 (A.take_dmc_pending_stall apu)

let test_dmc_disable_clears_active () =
  let apu = A.mk () in
  write apu 0x4012 0x00;
  write apu 0x4013 0x10;
  (* length=257 *)
  write apu 0x4010 0x00;
  write apu 0x4015 0x10;
  Alcotest.(check int) "active" 0b1_0000 (read apu 0x4015 land 0b1_0000);
  (* disable で stop *)
  write apu 0x4015 0x00;
  Alcotest.(check int) "stopped" 0 (read apu 0x4015 land 0b1_0000)

let test_dmc_irq_on_sample_end () =
  let apu = A.mk () in
  with_bus_reader apu 0xAA;
  write apu 0x4012 0x00;
  write apu 0x4013 0x00;
  (* L=0 → length=1 *)
  write apu 0x4010 0b1000_0000;
  (* IRQ enable=1, loop=0, rate idx 0 = period 428 *)
  write apu 0x4015 0x10;
  (* enable → 即 1 byte DMA → length=0 → IRQ *)
  Alcotest.(check bool) "DMC IRQ pending" true (A.irq_pending apu);
  (* $4015 read では bit 7 が立つが、clear はしない. *)
  let s = read apu 0x4015 in
  Alcotest.(check int) "bit 7 set" 0b1000_0000 (s land 0b1000_0000);
  Alcotest.(check bool) "IRQ still pending" true (A.irq_pending apu);
  (* $4015 write で IRQ flag clear *)
  write apu 0x4015 0x00;
  Alcotest.(check bool) "IRQ cleared by $4015 write" false (A.irq_pending apu)

let test_dmc_irq_disable_clears_flag () =
  let apu = A.mk () in
  with_bus_reader apu 0xAA;
  write apu 0x4012 0x00;
  write apu 0x4013 0x00;
  write apu 0x4010 0b1000_0000;
  (* IRQ enable=1 *)
  write apu 0x4015 0x10;
  Alcotest.(check bool) "IRQ set" true (A.irq_pending apu);
  (* $4010 で IRQ enable を 0 にすると flag も clear *)
  write apu 0x4010 0x00;
  Alcotest.(check bool)
    "IRQ cleared by $4010 enable=0"
    false
    (A.irq_pending apu)

(* ------------------------------------------------------------------ *)
(* $4000-$4007 へ write しても tick していない時は IRQ 立たない         *)
(* ------------------------------------------------------------------ *)

let test_write_without_tick_no_irq () =
  let apu = A.mk () in
  write apu 0x4000 0xFF;
  write apu 0x4001 0xFF;
  write apu 0x4002 0xFF;
  write apu 0x4003 0xFF;
  Alcotest.(check bool) "no IRQ" false (A.irq_pending apu)

(* ------------------------------------------------------------------ *)
(* 登録                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "APU"
    [ ( "初期 / $4015"
      , [ Alcotest.test_case "mk: 初期状態" `Quick test_mk_initial
        ; Alcotest.test_case
            "$4015 で pulse1 enable / disable"
            `Quick
            test_enable_pulse1_via_4015
        ; Alcotest.test_case
            "$4015 で pulse2 enable"
            `Quick
            test_enable_pulse2_via_4015
        ; Alcotest.test_case
            "write のみで IRQ は立たない"
            `Quick
            test_write_without_tick_no_irq
        ] )
    ; ( "Frame counter IRQ"
      , [ Alcotest.test_case
            "4-step: 29830 cycle で IRQ"
            `Quick
            test_frame_irq_4step
        ; Alcotest.test_case "inhibit で IRQ 立たない" `Quick test_frame_irq_inhibit
        ; Alcotest.test_case
            "inhibit set で既存 flag クリア"
            `Quick
            test_frame_irq_inhibit_clears_flag
        ; Alcotest.test_case "5-step は IRQ なし" `Quick test_5step_no_irq
        ] )
    ; ( "Length counter"
      , [ Alcotest.test_case
            "half frame で decrement"
            `Quick
            test_length_counter_decrement
        ; Alcotest.test_case "halt フラグで停止" `Quick test_length_halt
        ] )
    ; ( "Triangle"
      , [ Alcotest.test_case
            "$4015 で enable / disable"
            `Quick
            test_triangle_enable_via_4015
        ; Alcotest.test_case
            "linear counter は frame で進行 (smoke)"
            `Quick
            test_triangle_linear_counter_decrement
        ; Alcotest.test_case
            "timer_period < 2 は mute (smoke)"
            `Quick
            test_triangle_short_period_mute
        ] )
    ; ( "Noise"
      , [ Alcotest.test_case
            "$4015 で enable / disable"
            `Quick
            test_noise_enable_via_4015
        ; Alcotest.test_case
            "LFSR shift で出力変化 (smoke)"
            `Quick
            test_noise_lfsr_changes
        ] )
    ; ( "DMC"
      , [ Alcotest.test_case
            "$4011 direct DAC write"
            `Quick
            test_dmc_direct_dac_write
        ; Alcotest.test_case
            "enable で DMA request 起動"
            `Quick
            test_dmc_enable_and_dma_request
        ; Alcotest.test_case
            "disable で active クリア"
            `Quick
            test_dmc_disable_clears_active
        ; Alcotest.test_case
            "sample 終端で IRQ + $4015 read は clear せず write が clear"
            `Quick
            test_dmc_irq_on_sample_end
        ; Alcotest.test_case
            "$4010 IRQ enable=0 で flag クリア"
            `Quick
            test_dmc_irq_disable_clears_flag
        ] )
    ]
