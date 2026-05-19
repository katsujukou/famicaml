open Stdint

open Register

let peek_zp (bus:Bus.t) (a: Bus.byte)
  = bus.read (Uint16.of_uint8 a)

let peek_zx (bus:Bus.t) (cpu: t) (a:Bus.byte)
  = bus.read (Uint16.of_uint8 Uint8.(a + cpu.reg_X))

let peek_zy (bus:Bus.t) (cpu: t) (a:Bus.byte)
  = bus.read (Uint16.of_uint8 Uint8.(a + cpu.reg_Y))

let peek (bus: Bus.t) (a: Bus.addr) 
  = bus.read a

let peek_x (bus: Bus.t) (cpu: t) (a:Bus.addr) 
  = bus.read Uint16.(a + (Uint16.of_uint8 cpu.reg_X))

let peek_y (bus: Bus.t) (cpu: t) (a:Bus.addr)
  = bus.read Uint16.(a + (Uint16.of_uint8 cpu.reg_Y))

let peek_ix (bus:Bus.t) (cpu:t) (a:Bus.byte)
  = let ll = bus.read (Uint16.of_uint8 Uint8.(a + cpu.reg_X)) in 
    let hh = bus.read (Uint16.of_uint8 Uint8.(a + Uint8.one + cpu.reg_X)) in
    peek bus Uint16.(shift_left (Uint16.of_uint8 hh) 8 + of_uint8 ll)

let peek_iy (bus:Bus.t) (cpu:t) (a:Bus.byte)
  = let ll = peek_zp bus a in 
    let hh = peek_zp bus Uint8.(a + Uint8.one) in 
    peek_y bus cpu Uint16.(shift_left (Uint16.of_uint8 hh) 8 + of_uint8 ll)

let peek_16 (bus:Bus.t) (a:Bus.addr)
  = let ll = peek bus a in 
    let hh = peek bus Uint16.(a + Uint16.one) in 
    Uint16.(shift_left (Uint16.of_uint8 hh) 8 + of_uint8 ll)

let peek_16_buggy (bus:Bus.t) (a:Bus.addr) =
  let ll = peek bus a in
  let hi_addr =
    Uint16.logor
      (Uint16.logand a (Uint16.of_int 0xFF00))
      (Uint16.logand Uint16.(a + one) (Uint16.of_int 0x00FF))
  in
  let hh = peek bus hi_addr in
  Uint16.(shift_left (Uint16.of_uint8 hh) 8 + of_uint8 ll)