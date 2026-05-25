open Famicaml_common.Nesint

let ppu_reg a : uint16 =
  Uint16.of_int
  @@
  if a >= 0x2000 && a <= 0x3FFF
  then Int.logand a 0x2007
  else raise Exn.Out_of_range

let ppu_reg_2000 = ppu_reg 0x2000
let ppu_reg_2001 = ppu_reg 0x2001
let ppu_reg_2002 = ppu_reg 0x2002
let ppu_reg_2003 = ppu_reg 0x2003
let ppu_reg_2004 = ppu_reg 0x2004
let ppu_reg_2005 = ppu_reg 0x2005
let ppu_reg_2006 = ppu_reg 0x2006
let ppu_reg_2007 = ppu_reg 0x2007
