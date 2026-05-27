open Famicaml_common.Nesint

(* ------------------------------------------------------------------ *)
(* 定数                                                                 *)
(* ------------------------------------------------------------------ *)

(** Pulse の duty パターン (8 step × 4 duty). 1 = high, 0 = low. *)
let pulse_duty_table =
  [| [| 0; 1; 0; 0; 0; 0; 0; 0 |]
   ; [| 0; 1; 1; 0; 0; 0; 0; 0 |]
   ; [| 0; 1; 1; 1; 1; 0; 0; 0 |]
   ; [| 1; 0; 0; 1; 1; 1; 1; 1 |]
  |]

(** length counter load の index → length 値. *)
let length_table =
  [| 10
   ; 254
   ; 20
   ; 2
   ; 40
   ; 4
   ; 80
   ; 6
   ; 160
   ; 8
   ; 60
   ; 10
   ; 14
   ; 12
   ; 26
   ; 14
   ; 12
   ; 16
   ; 24
   ; 18
   ; 48
   ; 20
   ; 96
   ; 22
   ; 192
   ; 24
   ; 72
   ; 26
   ; 16
   ; 28
   ; 32
   ; 30
  |]

(* ------------------------------------------------------------------ *)
(* Pulse channel                                                       *)
(*                                                                     *)
(* $4000/$4004 DDLC.VVVV                                               *)
(* $4001/$4005 EPPP.NSSS  (sweep)                                      *)
(* $4002/$4006 TTTT.TTTT  (timer low)                                  *)
(* $4003/$4007 LLLL.LTTT  (length load + timer high)                   *)
(* ------------------------------------------------------------------ *)

type pulse =
  { mutable enabled : bool
  ; (* $4000 *)
    mutable duty : int
  ; mutable length_halt : bool
  ; (* = envelope loop *)
    mutable constant_volume : bool
  ; mutable envelope_period : int
  ; (* and constant volume value *)
    (* $4001 sweep *)
    mutable sweep_enable : bool
  ; mutable sweep_period : int
  ; mutable sweep_negate : bool
  ; mutable sweep_shift : int
  ; mutable sweep_reload : bool
  ; mutable sweep_divider : int
  ; (* timer (11 bit) *)
    mutable timer_period : int
  ; mutable timer_value : int
  ; (* sequencer *)
    mutable sequencer_step : int
  ; (* length counter *)
    mutable length_counter : int
  ; (* envelope *)
    mutable envelope_start : bool
  ; mutable envelope_divider : int
  ; mutable envelope_decay : int
  ; (* sweep の負数モード: pulse 1 は target = period - shifted - 1、
       pulse 2 は target = period - shifted (= 1 の差).
       実機の one's complement 由来. *)
    is_pulse2 : bool
  }

let make_pulse ~is_pulse2 =
  { enabled = false
  ; duty = 0
  ; length_halt = false
  ; constant_volume = false
  ; envelope_period = 0
  ; sweep_enable = false
  ; sweep_period = 0
  ; sweep_negate = false
  ; sweep_shift = 0
  ; sweep_reload = false
  ; sweep_divider = 0
  ; timer_period = 0
  ; timer_value = 0
  ; sequencer_step = 0
  ; length_counter = 0
  ; envelope_start = false
  ; envelope_divider = 0
  ; envelope_decay = 0
  ; is_pulse2
  }

let pulse_write_ctrl p byte =
  let d = Uint8.to_int byte in
  p.duty <- (d lsr 6) land 0b11;
  p.length_halt <- d land 0b0010_0000 <> 0;
  p.constant_volume <- d land 0b0001_0000 <> 0;
  p.envelope_period <- d land 0b1111

let pulse_write_sweep p byte =
  let d = Uint8.to_int byte in
  p.sweep_enable <- d land 0b1000_0000 <> 0;
  p.sweep_period <- (d lsr 4) land 0b111;
  p.sweep_negate <- d land 0b1000 <> 0;
  p.sweep_shift <- d land 0b111;
  p.sweep_reload <- true

let pulse_write_timer_lo p byte =
  let d = Uint8.to_int byte in
  p.timer_period <- p.timer_period land 0xFF00 lor d

let pulse_write_timer_hi p byte =
  let d = Uint8.to_int byte in
  let hi = d land 0b111 in
  p.timer_period <- p.timer_period land 0x00FF lor (hi lsl 8);
  if p.enabled
  then (
    let len_idx = (d lsr 3) land 0b1_1111 in
    p.length_counter <- length_table.(len_idx));
  p.sequencer_step <- 0;
  p.envelope_start <- true

(** Sweep の target period 計算. *)
let pulse_sweep_target p =
  let shifted = p.timer_period lsr p.sweep_shift in
  if p.sweep_negate
  then (
    let delta = if p.is_pulse2 then shifted else shifted + 1 in
    let t = p.timer_period - delta in
    if t < 0 then 0 else t)
  else p.timer_period + shifted

(** Sweep が channel を mute するか. *)
let pulse_sweep_muting p = p.timer_period < 8 || pulse_sweep_target p > 0x7FF

(** APU half frame で呼ばれる: sweep を 1 step 進める. *)
let pulse_clock_sweep p =
  let target = pulse_sweep_target p in
  let mute = pulse_sweep_muting p in
  if p.sweep_divider = 0 && p.sweep_enable && (not mute) && p.sweep_shift > 0
  then p.timer_period <- target;
  if p.sweep_divider = 0 || p.sweep_reload
  then (
    p.sweep_divider <- p.sweep_period;
    p.sweep_reload <- false)
  else p.sweep_divider <- p.sweep_divider - 1

(** APU half frame で呼ばれる: length counter を 1 step 進める. *)
let pulse_clock_length p =
  if (not p.length_halt) && p.length_counter > 0
  then p.length_counter <- p.length_counter - 1

(** APU quarter frame で呼ばれる: envelope を 1 step 進める. *)
let pulse_clock_envelope p =
  if p.envelope_start
  then (
    p.envelope_decay <- 15;
    p.envelope_divider <- p.envelope_period;
    p.envelope_start <- false)
  else if p.envelope_divider = 0
  then (
    p.envelope_divider <- p.envelope_period;
    if p.envelope_decay > 0
    then p.envelope_decay <- p.envelope_decay - 1
    else if p.length_halt
    then p.envelope_decay <- 15)
  else p.envelope_divider <- p.envelope_divider - 1

(** APU clock (= 2 CPU cycle ごと) で呼ばれる: timer を 1 step 進め、
    0 になったら period 再ロード + sequencer を進める. *)
let pulse_clock_timer p =
  if p.timer_value = 0
  then (
    p.timer_value <- p.timer_period;
    p.sequencer_step <- (p.sequencer_step + 1) land 0b111)
  else p.timer_value <- p.timer_value - 1

(** 現在の Pulse 出力 (0..15) を返す. mute 条件:
    - length counter == 0
    - timer_period < 8 (= 高周波 mute)
    - sweep target > 0x7FF
    - duty の現 step が 0 *)
let pulse_output p =
  if
    (not p.enabled)
    || p.length_counter = 0
    || pulse_sweep_muting p
    || pulse_duty_table.(p.duty).(p.sequencer_step) = 0
  then 0
  else if p.constant_volume
  then p.envelope_period
  else p.envelope_decay

(* ------------------------------------------------------------------ *)
(* Triangle channel                                                    *)
(*                                                                     *)
(* $4008 CRRR.RRRR  (C=control/halt, R=linear counter reload)          *)
(* $4009 未使用                                                          *)
(* $400A TTTT.TTTT  (timer low)                                        *)
(* $400B LLLL.LTTT  (length load + timer high)                         *)
(*                                                                     *)
(* Triangle は他チャネルと違い CPU clock で timer 駆動 (= APU clock     *)
(* の 2 倍速). 32-step 固定 sequencer で正三角波を生成.                *)
(* ------------------------------------------------------------------ *)

(** 32-step 固定の三角波シーケンス (15 → 0 → 15). *)
let triangle_sequence =
  [| 15
   ; 14
   ; 13
   ; 12
   ; 11
   ; 10
   ; 9
   ; 8
   ; 7
   ; 6
   ; 5
   ; 4
   ; 3
   ; 2
   ; 1
   ; 0
   ; 0
   ; 1
   ; 2
   ; 3
   ; 4
   ; 5
   ; 6
   ; 7
   ; 8
   ; 9
   ; 10
   ; 11
   ; 12
   ; 13
   ; 14
   ; 15
  |]

type triangle =
  { mutable enabled : bool
  ; (* $4008 *)
    mutable control_halt : bool
  ; (* = length counter halt かつ linear counter "control" *)
    mutable linear_counter_reload : int
  ; (* timer (11 bit, CPU clock domain) *)
    mutable timer_period : int
  ; mutable timer_value : int
  ; (* sequencer (5 bit, 0..31) *)
    mutable sequencer_step : int
  ; (* linear counter (7 bit) *)
    mutable linear_counter : int
  ; mutable linear_reload_flag : bool
  ; (* length counter *)
    mutable length_counter : int
  }

let make_triangle () =
  { enabled = false
  ; control_halt = false
  ; linear_counter_reload = 0
  ; timer_period = 0
  ; timer_value = 0
  ; sequencer_step = 0
  ; linear_counter = 0
  ; linear_reload_flag = false
  ; length_counter = 0
  }

let triangle_write_ctrl t byte =
  let d = Uint8.to_int byte in
  t.control_halt <- d land 0b1000_0000 <> 0;
  t.linear_counter_reload <- d land 0b0111_1111

let triangle_write_timer_lo t byte =
  let d = Uint8.to_int byte in
  t.timer_period <- t.timer_period land 0xFF00 lor d

let triangle_write_timer_hi t byte =
  let d = Uint8.to_int byte in
  let hi = d land 0b111 in
  t.timer_period <- t.timer_period land 0x00FF lor (hi lsl 8);
  if t.enabled
  then (
    let len_idx = (d lsr 3) land 0b1_1111 in
    t.length_counter <- length_table.(len_idx));
  t.linear_reload_flag <- true

(** CPU clock 毎回呼ばれる: timer を 1 step 進める. 0 → period reload +
    sequencer 進行. ただし sequencer は length と linear が両方
    非ゼロのときのみ進む (実機通り). *)
let triangle_clock_timer t =
  if t.timer_value = 0
  then (
    t.timer_value <- t.timer_period;
    if t.length_counter > 0 && t.linear_counter > 0
    then t.sequencer_step <- (t.sequencer_step + 1) land 0b1_1111)
  else t.timer_value <- t.timer_value - 1

(** quarter frame で呼ばれる: linear counter を更新.
    reload flag が立っていれば reload 値で再ロード、そうでなければ
    1 decrement. control flag が 0 なら reload flag をクリア
    (= 1 度 reload したらクリア). *)
let triangle_clock_linear t =
  if t.linear_reload_flag
  then t.linear_counter <- t.linear_counter_reload
  else if t.linear_counter > 0
  then t.linear_counter <- t.linear_counter - 1;
  if not t.control_halt then t.linear_reload_flag <- false

(** half frame で呼ばれる: length counter を 1 step 進める. *)
let triangle_clock_length t =
  if (not t.control_halt) && t.length_counter > 0
  then t.length_counter <- t.length_counter - 1

(** 現在の triangle 出力 (0..15).
    timer_period < 2 は超高周波で実機でもほぼ DC = mute 扱い. *)
let triangle_output t =
  if t.timer_period < 2 then 0 else triangle_sequence.(t.sequencer_step)

(* ------------------------------------------------------------------ *)
(* Noise channel                                                       *)
(*                                                                     *)
(* $400C --LC.VVVV  (L=length halt/envelope loop, C=const vol, VVVV=env) *)
(* $400D 未使用                                                          *)
(* $400E M---.PPPP  (M=mode, PPPP=period index)                        *)
(* $400F LLLL.L---  (length load)                                       *)
(*                                                                     *)
(* 15-bit LFSR. mode=0: feedback = bit0 XOR bit1.                      *)
(*              mode=1: feedback = bit0 XOR bit6. (短周期, "音が篭る")  *)
(* output mute: LFSR bit 0 = 1 のとき.                                  *)
(* ------------------------------------------------------------------ *)

(** NTSC noise timer period table (APU clock 単位). *)
let noise_period_table_ntsc =
  [| 4
   ; 8
   ; 16
   ; 32
   ; 64
   ; 96
   ; 128
   ; 160
   ; 202
   ; 254
   ; 380
   ; 508
   ; 762
   ; 1016
   ; 2034
   ; 4068
  |]

type noise =
  { mutable enabled : bool
  ; (* $400C *)
    mutable length_halt : bool
  ; mutable constant_volume : bool
  ; mutable envelope_period : int
  ; (* $400E *)
    mutable mode : bool
  ; mutable timer_period : int
  ; mutable timer_value : int
  ; (* LFSR (15 bit, 初期値 1) *)
    mutable lfsr : int
  ; mutable length_counter : int
  ; (* envelope *)
    mutable envelope_start : bool
  ; mutable envelope_divider : int
  ; mutable envelope_decay : int
  }

let make_noise () =
  { enabled = false
  ; length_halt = false
  ; constant_volume = false
  ; envelope_period = 0
  ; mode = false
  ; timer_period = noise_period_table_ntsc.(0)
  ; timer_value = 0
  ; lfsr = 1
  ; length_counter = 0
  ; envelope_start = false
  ; envelope_divider = 0
  ; envelope_decay = 0
  }

let noise_write_ctrl n byte =
  let d = Uint8.to_int byte in
  n.length_halt <- d land 0b0010_0000 <> 0;
  n.constant_volume <- d land 0b0001_0000 <> 0;
  n.envelope_period <- d land 0b1111

let noise_write_mode n byte =
  let d = Uint8.to_int byte in
  n.mode <- d land 0b1000_0000 <> 0;
  n.timer_period <- noise_period_table_ntsc.(d land 0b1111)

let noise_write_length n byte =
  let d = Uint8.to_int byte in
  if n.enabled
  then (
    let len_idx = (d lsr 3) land 0b1_1111 in
    n.length_counter <- length_table.(len_idx));
  n.envelope_start <- true

(** APU clock (= 2 CPU cycle ごと) で呼ばれる: timer + LFSR shift. *)
let noise_clock_timer n =
  if n.timer_value = 0
  then (
    n.timer_value <- n.timer_period;
    let bit0 = n.lfsr land 1 in
    let other =
      if n.mode then (n.lfsr lsr 6) land 1 else (n.lfsr lsr 1) land 1
    in
    let feedback = bit0 lxor other in
    n.lfsr <- (n.lfsr lsr 1) lor (feedback lsl 14))
  else n.timer_value <- n.timer_value - 1

let noise_clock_length n =
  if (not n.length_halt) && n.length_counter > 0
  then n.length_counter <- n.length_counter - 1

(** Pulse とまったく同じ envelope ロジック. *)
let noise_clock_envelope n =
  if n.envelope_start
  then (
    n.envelope_decay <- 15;
    n.envelope_divider <- n.envelope_period;
    n.envelope_start <- false)
  else if n.envelope_divider = 0
  then (
    n.envelope_divider <- n.envelope_period;
    if n.envelope_decay > 0
    then n.envelope_decay <- n.envelope_decay - 1
    else if n.length_halt
    then n.envelope_decay <- 15)
  else n.envelope_divider <- n.envelope_divider - 1

(** 現在の noise 出力 (0..15). mute 条件:
    - disabled
    - length_counter == 0
    - LFSR bit 0 == 1 *)
let noise_output n =
  if (not n.enabled) || n.length_counter = 0 || n.lfsr land 1 = 1
  then 0
  else if n.constant_volume
  then n.envelope_period
  else n.envelope_decay

(* ------------------------------------------------------------------ *)
(* DMC channel (Delta Modulation Channel)                              *)
(*                                                                     *)
(* $4010 IL--.RRRR  (I=IRQ enable, L=loop, RRRR=rate index)            *)
(* $4011 -DDD.DDDD  (DAC direct load, 7 bit)                            *)
(* $4012 AAAA.AAAA  (sample address = $C000 + A*64)                    *)
(* $4013 LLLL.LLLL  (sample length = L*16 + 1 byte)                    *)
(*                                                                     *)
(* APU clock で timer 駆動. timer 0 → output unit が 1 bit 出す:        *)
(*   shift_reg bit 0 = 1 → DAC += 2 (上限 127)                          *)
(*                       0 → DAC -= 2 (下限 0)                          *)
(*   shift right, 8 bit 出し終わったら sample buffer から再ロード.       *)
(*                                                                     *)
(* Memory reader: sample buffer 空かつ length > 0 で起動. Nes 側に      *)
(* DMA request を出して CPU を 4 cycle stall させ、bus.read で 1 byte    *)
(* を取得して buffer に入れる. length 0 になったら loop or IRQ.          *)
(* ------------------------------------------------------------------ *)

(** NTSC DMC rate table: CPU cycles per output bit. *)
let dmc_rate_table_ntsc =
  [| 428
   ; 380
   ; 340
   ; 320
   ; 286
   ; 254
   ; 226
   ; 214
   ; 190
   ; 160
   ; 142
   ; 128
   ; 106
   ; 84
   ; 72
   ; 54
  |]

(** DMC memory reader が CPU bus を読むための closure. Nes 側から inject される.
    引数: CPU address (16 bit), 戻り値: byte (0..255). *)
type bus_reader = int -> int

type dmc =
  { (* $4010 *)
    mutable irq_enable : bool
  ; mutable loop_flag : bool
  ; mutable timer_period : int
  ; mutable timer_value : int
  ; (* $4011 DAC counter (7 bit, 0..127) *)
    mutable dac : int
  ; (* $4012/$4013 sample 開始情報 *)
    mutable sample_addr_start : int
  ; mutable sample_len_start : int
  ; (* runtime *)
    mutable current_addr : int
  ; mutable bytes_remaining : int
  ; (* sample buffer (1 byte FIFO). -1 = empty. *)
    mutable sample_buffer : int
  ; (* output shift register *)
    mutable shift_reg : int
  ; mutable shift_count : int
  ; mutable silence : bool
  ; (* IRQ flag (clear: $4015 write, $4010 で IRQ enable=0) *)
    mutable irq_flag : bool
  ; (* Bus reader (Nes 側から inject). 接続前は dummy (常に 0 を返す). *)
    mutable bus_reader : bus_reader
  ; (* DMC DMA で発生した CPU stall (CPU cycle 単位). Nes が tick で消化. *)
    mutable pending_stall : int
  }

let make_dmc () =
  { irq_enable = false
  ; loop_flag = false
  ; timer_period = dmc_rate_table_ntsc.(0)
  ; timer_value = 0
  ; dac = 0
  ; sample_addr_start = 0xC000
  ; sample_len_start = 1
  ; current_addr = 0xC000
  ; bytes_remaining = 0
  ; sample_buffer = -1
  ; shift_reg = 0
  ; shift_count = 0
  ; silence = true
  ; irq_flag = false
  ; bus_reader = (fun _ -> 0)
  ; pending_stall = 0
  }

(** Sample buffer 充填: buffer empty かつ bytes_remaining > 0 のとき
    bus_reader で同期的に 1 byte 取得 + CPU stall (4 cycle) を pending に追加.
    実機は CPU を halt して DMA で読むので、output unit から見ると
    「buffer 充填は瞬時に終わる」.

    後で stall は Nes.tick が消化するが、buffer はその場で充填されている
    ので、同じ event 内の output cycle は新 byte で動ける. *)
let dmc_request_dma d =
  if d.sample_buffer < 0 && d.bytes_remaining > 0
  then (
    let byte = d.bus_reader d.current_addr land 0xFF in
    d.sample_buffer <- byte;
    let next_addr =
      if d.current_addr = 0xFFFF then 0x8000 else d.current_addr + 1
    in
    d.current_addr <- next_addr;
    d.bytes_remaining <- d.bytes_remaining - 1;
    d.pending_stall <- d.pending_stall + 4;
    if d.bytes_remaining = 0
    then
      if d.loop_flag
      then (
        d.current_addr <- d.sample_addr_start;
        d.bytes_remaining <- d.sample_len_start)
      else if d.irq_enable
      then d.irq_flag <- true)

let dmc_write_ctrl d byte =
  let b = Uint8.to_int byte in
  d.irq_enable <- b land 0b1000_0000 <> 0;
  d.loop_flag <- b land 0b0100_0000 <> 0;
  d.timer_period <- dmc_rate_table_ntsc.(b land 0b1111);
  (* IRQ disable 時は IRQ flag も clear *)
  if not d.irq_enable then d.irq_flag <- false

let dmc_write_dac d byte =
  let b = Uint8.to_int byte in
  d.dac <- b land 0b0111_1111

let dmc_write_addr d byte =
  let b = Uint8.to_int byte in
  d.sample_addr_start <- 0xC000 + (b * 64)

let dmc_write_length d byte =
  let b = Uint8.to_int byte in
  d.sample_len_start <- (b * 16) + 1

(** Nes 側が DMC bus reader を inject する. *)
let dmc_connect_bus_reader (d : dmc) (f : bus_reader) : unit = d.bus_reader <- f

(** Nes 側が dmc 由来の CPU stall (cycle 数) を取り出す. 取り出すと 0 リセット. *)
let dmc_take_pending_stall (d : dmc) : int =
  let n = d.pending_stall in
  d.pending_stall <- 0;
  n

(** CPU clock で呼ばれる: output unit を 1 step 進める.
    実機の操作順序:
    1. bits-remaining が 0 なら new output cycle:
       - sample buffer が空なら memory reader を起動 (= 同期 DMA, buffer 充填)
       - buffer 充填済みなら shift register にロード + silence clear
       - 充填できなければ silence set
    2. 同 event 内で 1 bit 出力 + shift right + bits-remaining decrement

    この順序により 1 byte 消費にちょうど 8 event = 8 * period CPU cycle. *)
let dmc_clock_output d =
  if d.shift_count = 0
  then (
    (* buffer 空なら memory reader 起動して同期 DMA で充填を試みる *)
    if d.sample_buffer < 0 then dmc_request_dma d;
    if d.sample_buffer < 0
    then d.silence <- true
    else (
      d.silence <- false;
      d.shift_reg <- d.sample_buffer;
      d.sample_buffer <- -1);
    d.shift_count <- 8);
  (* 1 bit 出力 (silence なら DAC 変化なし) *)
  if not d.silence
  then
    if d.shift_reg land 1 = 1
    then (if d.dac <= 125 then d.dac <- d.dac + 2)
    else if d.dac >= 2
    then d.dac <- d.dac - 2;
  d.shift_reg <- d.shift_reg lsr 1;
  d.shift_count <- d.shift_count - 1

(** CPU clock で呼ばれる: timer を 1 step 進める. *)
let dmc_clock_timer d =
  if d.timer_value = 0
  then (
    d.timer_value <- d.timer_period;
    dmc_clock_output d)
  else d.timer_value <- d.timer_value - 1

(** DMC の現在 DAC 値 (0..127). *)
let dmc_output d = d.dac

(* ------------------------------------------------------------------ *)
(* Frame counter ($4017)                                               *)
(*                                                                     *)
(* CPU cycle ベースで step / half / quarter frame イベントを発火する.   *)
(*   4-step mode: quarter @ 7457, 14913, 22371, 29829                  *)
(*                half @ 14913, 29829                                  *)
(*                IRQ @ 29830 (inhibit でなければ)                      *)
(*   5-step mode: quarter @ 7457, 14913, 22371, 37281                  *)
(*                half @ 14913, 37281                                  *)
(*                (IRQ なし)                                            *)
(*                                                                     *)
(* 実機は APU clock (= CPU cycle / 2) の半分単位だが、本実装は CPU      *)
(* cycle で記録する (= 上記の値はそのまま使う). *)
(* ------------------------------------------------------------------ *)

type frame_mode =
  | Mode_4_step
  | Mode_5_step

type frame_counter =
  { mutable mode : frame_mode
  ; mutable irq_inhibit : bool
  ; mutable cycle_in_seq : int (* シーケンス内のサイクル数 *)
  ; mutable irq_flag : bool
  }

let make_frame_counter () =
  (* 実機 power-on は 4-step + IRQ enable (NESdev wiki). *)
  { mode = Mode_4_step
  ; irq_inhibit = false
  ; cycle_in_seq = 0
  ; irq_flag = false
  }

(* ------------------------------------------------------------------ *)
(* Apu.t                                                                *)
(* ------------------------------------------------------------------ *)

(* ------------------------------------------------------------------ *)
(* Sample buffer (ring) と downsampler                                  *)
(*                                                                     *)
(* CPU clock (NTSC 1.789773 MHz) で per-cycle に mix した瞬時値を       *)
(* sample_rate (default 44100Hz) にダウンサンプル.                      *)
(* 連続 N サンプルを「累積平均」で 1 サンプルにまとめる (anti-aliasing  *)
(* 効果). Ring buffer は overflow 時に oldest を捨てる.                 *)
(* ------------------------------------------------------------------ *)

let cpu_clock_hz = 1_789_773.0
let default_sample_rate = 44_100.0
let sample_buffer_capacity = 16384

type t =
  { pulse1 : pulse
  ; pulse2 : pulse
  ; triangle : triangle
  ; noise : noise
  ; dmc : dmc
  ; frame_counter : frame_counter
  ; mutable cpu_cycles : int
    (* 起動からの CPU cycle 数. APU clock を 2 CPU cycle ごとに進めるため. *)
  ; mutable apu_step_phase : int
    (* 0 or 1. 0 のとき APU clock を進める (= 2 CPU cycle 周期). *)
    (* ----- sample 生成 (Phase D1B) ----- *)
  ; mutable cycles_per_sample : float
  ; mutable sample_cycle_acc : float
  ; mutable accumulator : float
  ; mutable accumulator_n : int
  ; sample_buffer : float array
  ; mutable buffer_w : int
  ; mutable buffer_r : int
  }

let mk () =
  { pulse1 = make_pulse ~is_pulse2:false
  ; pulse2 = make_pulse ~is_pulse2:true
  ; triangle = make_triangle ()
  ; noise = make_noise ()
  ; dmc = make_dmc ()
  ; frame_counter = make_frame_counter ()
  ; cpu_cycles = 0
  ; apu_step_phase = 0
  ; cycles_per_sample = cpu_clock_hz /. default_sample_rate
  ; sample_cycle_acc = 0.0
  ; accumulator = 0.0
  ; accumulator_n = 0
  ; sample_buffer = Array.make sample_buffer_capacity 0.0
  ; buffer_w = 0
  ; buffer_r = 0
  }

(** Web Audio の AudioContext.sampleRate を渡して downsample 比率を更新する. *)
let set_sample_rate (apu : t) (rate : float) : unit =
  if rate > 0.0
  then (
    apu.cycles_per_sample <- cpu_clock_hz /. rate;
    (* accumulator も reset (rate 変更時にズレを残さない) *)
    apu.sample_cycle_acc <- 0.0;
    apu.accumulator <- 0.0;
    apu.accumulator_n <- 0)

(** Ring buffer に sample を push. overflow なら oldest を捨てる. *)
let push_sample (apu : t) (s : float) : unit =
  Array.unsafe_set apu.sample_buffer apu.buffer_w s;
  let next_w = (apu.buffer_w + 1) mod sample_buffer_capacity in
  apu.buffer_w <- next_w;
  if next_w = apu.buffer_r
  then apu.buffer_r <- (apu.buffer_r + 1) mod sample_buffer_capacity

(** Drain up to [max_n] samples into a freshly-allocated array.
    実際取り出した数 (= 配列長) は max_n か buffer 内に残ってる数の小さい方. *)
let drain_samples (apu : t) (max_n : int) : float array =
  let avail =
    (apu.buffer_w - apu.buffer_r + sample_buffer_capacity)
    mod sample_buffer_capacity
  in
  let n = min max_n avail in
  let out = Array.make n 0.0 in
  for i = 0 to n - 1 do
    Array.unsafe_set
      out
      i
      (Array.unsafe_get
         apu.sample_buffer
         ((apu.buffer_r + i) mod sample_buffer_capacity))
  done;
  apu.buffer_r <- (apu.buffer_r + n) mod sample_buffer_capacity;
  out

(** 現在の APU 出力を実機の non-linear mixer (NesDev wiki) で計算.
    pulse 部分と triangle/noise/DMC 部分を別式で算出して合算.
    現状 DMC は 0 (未実装). 出力レンジは概ね [0.0, 1.0] 弱. *)
let mix_sample (apu : t) : float =
  let p1 = pulse_output apu.pulse1 in
  let p2 = pulse_output apu.pulse2 in
  let tri = triangle_output apu.triangle in
  let nz = noise_output apu.noise in
  let dm = dmc_output apu.dmc in
  let pulse_out =
    let s = p1 + p2 in
    if s = 0 then 0.0 else 95.88 /. ((8128.0 /. float_of_int s) +. 100.0)
  in
  let tnd_out =
    (* 159.79 / (1 / (T/8227 + N/12241 + D/22638) + 100). *)
    let t_term = if tri = 0 then 0.0 else float_of_int tri /. 8227.0 in
    let n_term = if nz = 0 then 0.0 else float_of_int nz /. 12241.0 in
    let d_term = if dm = 0 then 0.0 else float_of_int dm /. 22638.0 in
    let denom = t_term +. n_term +. d_term in
    if denom = 0.0 then 0.0 else 159.79 /. ((1.0 /. denom) +. 100.0)
  in
  pulse_out +. tnd_out

(** 各 CPU cycle 末尾で呼ばれる: 瞬時値を accumulator に加算し、
    target sample rate のタイミングで平均を ring buffer に push. *)
let accumulate_sample (apu : t) : unit =
  apu.accumulator <- apu.accumulator +. mix_sample apu;
  apu.accumulator_n <- apu.accumulator_n + 1;
  apu.sample_cycle_acc <- apu.sample_cycle_acc +. 1.0;
  if apu.sample_cycle_acc >= apu.cycles_per_sample
  then (
    let avg = apu.accumulator /. float_of_int apu.accumulator_n in
    push_sample apu avg;
    apu.accumulator <- 0.0;
    apu.accumulator_n <- 0;
    apu.sample_cycle_acc <- apu.sample_cycle_acc -. apu.cycles_per_sample)

(** quarter frame: envelope + triangle linear counter を clock *)
let clock_quarter_frame apu =
  pulse_clock_envelope apu.pulse1;
  pulse_clock_envelope apu.pulse2;
  triangle_clock_linear apu.triangle;
  noise_clock_envelope apu.noise

(** half frame: length counter + sweep を clock *)
let clock_half_frame apu =
  pulse_clock_length apu.pulse1;
  pulse_clock_length apu.pulse2;
  pulse_clock_sweep apu.pulse1;
  pulse_clock_sweep apu.pulse2;
  triangle_clock_length apu.triangle;
  noise_clock_length apu.noise

(** Frame counter を CPU 1 cycle 分進める. quarter/half frame の発火、
    4-step mode の IRQ flag セット、シーケンス末尾での wrap を処理する. *)
let frame_counter_tick apu =
  let fc = apu.frame_counter in
  fc.cycle_in_seq <- fc.cycle_in_seq + 1;
  let c = fc.cycle_in_seq in
  match fc.mode with
  | Mode_4_step ->
    if c = 7457
    then (
      clock_quarter_frame apu;
      ())
    else if c = 14913
    then (
      clock_quarter_frame apu;
      clock_half_frame apu)
    else if c = 22371
    then clock_quarter_frame apu
    else if c = 29829
    then (
      if not fc.irq_inhibit then fc.irq_flag <- true;
      clock_quarter_frame apu;
      clock_half_frame apu)
    else if c >= 29830
    then (
      (* 実機では IRQ は 29828, 29829, 29830 で立つが、本実装は 1 か所で
         代表化. 29830 で立てた後 wrap. *)
      if not fc.irq_inhibit then fc.irq_flag <- true;
      fc.cycle_in_seq <- 0)
  | Mode_5_step ->
    if c = 7457
    then clock_quarter_frame apu
    else if c = 14913
    then (
      clock_quarter_frame apu;
      clock_half_frame apu)
    else if c = 22371
    then clock_quarter_frame apu
    else if c = 37281
    then (
      clock_quarter_frame apu;
      clock_half_frame apu)
    else if c >= 37282
    then fc.cycle_in_seq <- 0

(** $4017 write. *)
let frame_counter_write apu byte =
  let d = Uint8.to_int byte in
  let fc = apu.frame_counter in
  fc.mode <- (if d land 0b1000_0000 <> 0 then Mode_5_step else Mode_4_step);
  fc.irq_inhibit <- d land 0b0100_0000 <> 0;
  (* IRQ inhibit が立ったら現 IRQ flag をクリア *)
  if fc.irq_inhibit then fc.irq_flag <- false;
  fc.cycle_in_seq <- 0;
  (* 5-step mode の write 直後は quarter + half frame が即時走る *)
  if fc.mode = Mode_5_step
  then (
    clock_quarter_frame apu;
    clock_half_frame apu)

(* ------------------------------------------------------------------ *)
(* $4015 status register                                                *)
(* ------------------------------------------------------------------ *)

(** $4015 read:
    - bit 0: pulse1 length > 0
    - bit 1: pulse2 length > 0
    - bit 2,3,4: 未実装 (triangle/noise/DMC は 0)
    - bit 6: frame IRQ flag (read で clear)
    - bit 7: DMC IRQ flag (未実装) *)
let read_status apu =
  let bit b s = if b then 1 lsl s else 0 in
  let byte =
    bit (apu.pulse1.length_counter > 0) 0
    lor bit (apu.pulse2.length_counter > 0) 1
    lor bit (apu.triangle.length_counter > 0) 2
    lor bit (apu.noise.length_counter > 0) 3
    lor bit (apu.dmc.bytes_remaining > 0) 4
    lor bit apu.frame_counter.irq_flag 6
    lor bit apu.dmc.irq_flag 7
  in
  (* frame IRQ flag は read で clear、DMC IRQ flag は clear しない (実機通り) *)
  apu.frame_counter.irq_flag <- false;
  Uint8.of_int byte

(** $4015 write: チャネル enable.
    pulse/triangle/noise の bit を 0 にすると対応 length counter が即 0.
    DMC bit 4 を 0 にすると bytes_remaining = 0 (= sample 再生停止).
    1 にすると、bytes_remaining が 0 のときだけ sample を新規に開始.
    任意の write で DMC IRQ flag を clear. *)
let write_status apu byte =
  let d = Uint8.to_int byte in
  apu.pulse1.enabled <- d land 0b0001 <> 0;
  if not apu.pulse1.enabled then apu.pulse1.length_counter <- 0;
  apu.pulse2.enabled <- d land 0b0010 <> 0;
  if not apu.pulse2.enabled then apu.pulse2.length_counter <- 0;
  apu.triangle.enabled <- d land 0b0100 <> 0;
  if not apu.triangle.enabled then apu.triangle.length_counter <- 0;
  apu.noise.enabled <- d land 0b1000 <> 0;
  if not apu.noise.enabled then apu.noise.length_counter <- 0;
  (* 実機仕様: $4015 write は DMC IRQ flag を clear する (= write の副作用). *)
  apu.dmc.irq_flag <- false;
  let dmc_en = d land 0b1_0000 <> 0 in
  if not dmc_en
  then apu.dmc.bytes_remaining <- 0
  else if apu.dmc.bytes_remaining = 0
  then (
    apu.dmc.current_addr <- apu.dmc.sample_addr_start;
    apu.dmc.bytes_remaining <- apu.dmc.sample_len_start;
    (* memory reader を即起動 (= "buffer immediately filled" 仕様,
       Blargg dmc_basics test #19 準拠). Buffer empty で bytes_remaining > 0
       なら 1 byte を同期 DMA で取得する. *)
    if apu.dmc.sample_buffer < 0 then dmc_request_dma apu.dmc)

(* ------------------------------------------------------------------ *)
(* CPU バス入口                                                         *)
(* ------------------------------------------------------------------ *)

let cpu_read (apu : t) (addr : uint16) : uint8 =
  let a = Uint16.to_int addr in
  if a = 0x4015 then read_status apu else Uint8.zero

let cpu_write (apu : t) (addr : uint16) (byte : uint8) : unit =
  let a = Uint16.to_int addr in
  match a with
  | 0x4000 -> pulse_write_ctrl apu.pulse1 byte
  | 0x4001 -> pulse_write_sweep apu.pulse1 byte
  | 0x4002 -> pulse_write_timer_lo apu.pulse1 byte
  | 0x4003 -> pulse_write_timer_hi apu.pulse1 byte
  | 0x4004 -> pulse_write_ctrl apu.pulse2 byte
  | 0x4005 -> pulse_write_sweep apu.pulse2 byte
  | 0x4006 -> pulse_write_timer_lo apu.pulse2 byte
  | 0x4007 -> pulse_write_timer_hi apu.pulse2 byte
  | 0x4008 -> triangle_write_ctrl apu.triangle byte
  | 0x400A -> triangle_write_timer_lo apu.triangle byte
  | 0x400B -> triangle_write_timer_hi apu.triangle byte
  | 0x400C -> noise_write_ctrl apu.noise byte
  | 0x400E -> noise_write_mode apu.noise byte
  | 0x400F -> noise_write_length apu.noise byte
  | 0x4010 -> dmc_write_ctrl apu.dmc byte
  | 0x4011 -> dmc_write_dac apu.dmc byte
  | 0x4012 -> dmc_write_addr apu.dmc byte
  | 0x4013 -> dmc_write_length apu.dmc byte
  | 0x4015 -> write_status apu byte
  | 0x4017 -> frame_counter_write apu byte
  | _ -> ()

(* ------------------------------------------------------------------ *)
(* tick                                                                 *)
(* ------------------------------------------------------------------ *)

let tick_cpu (apu : t) : unit =
  apu.cpu_cycles <- apu.cpu_cycles + 1;
  (* APU clock は CPU の半分 (= 2 CPU cycle で 1 step). pulse timer も
     APU clock domain. *)
  apu.apu_step_phase <- 1 - apu.apu_step_phase;
  if apu.apu_step_phase = 0
  then (
    pulse_clock_timer apu.pulse1;
    pulse_clock_timer apu.pulse2;
    noise_clock_timer apu.noise);
  (* Triangle と DMC の timer は CPU clock domain (= APU clock の倍速).
     特に DMC の rate table の値は CPU cycle 単位 (NesDev wiki "APU DMC":
     "Rate values determine how many CPU cycles to wait between each bit"). *)
  triangle_clock_timer apu.triangle;
  dmc_clock_timer apu.dmc;
  (* Frame counter は CPU cycle domain. *)
  frame_counter_tick apu;
  (* Sample 生成: 各 CPU cycle 末尾で 1 sample 累積. *)
  accumulate_sample apu

let irq_pending (apu : t) : bool =
  apu.frame_counter.irq_flag || apu.dmc.irq_flag

(** Soft reset. NESdev / Mesen 準拠:
    - 各 channel の enabled=false ($4015 write 0 と同等)
    - non-Triangle channel (pulse1/2/noise) の length_counter clear
    - Triangle length_counter は preserved ("triangle unaffected" 仕様)
    - DMC: bytes_remaining clear + IRQ flag clear. sample_addr/sample_len は
      preserved ($4012/$4013 は CPU RESET で影響受けない)
    - Frame counter: IRQ flag clear, cycle reset. mode/inhibit は preserved. *)
let reset (apu : t) : unit =
  apu.pulse1.enabled <- false;
  apu.pulse1.length_counter <- 0;
  apu.pulse2.enabled <- false;
  apu.pulse2.length_counter <- 0;
  apu.triangle.enabled <- false;
  (* Triangle length_counter は preserved *)
  apu.noise.enabled <- false;
  apu.noise.length_counter <- 0;
  apu.dmc.bytes_remaining <- 0;
  apu.dmc.irq_flag <- false;
  (* DMC sample_addr_start / sample_len_start は preserved *)
  apu.frame_counter.irq_flag <- false;
  apu.frame_counter.cycle_in_seq <- 0

(* DMC bus reader inject と pending stall 取り出し. Apu.t 経由のラッパ. *)
let connect_bus_reader (apu : t) (f : bus_reader) : unit =
  dmc_connect_bus_reader apu.dmc f

let take_dmc_pending_stall (apu : t) : int = dmc_take_pending_stall apu.dmc
