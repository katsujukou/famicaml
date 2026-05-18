open Stdint

(** Interface to shared main memory. Some part of it is mapped to other hardware component
    including PPU/APU register and GamePad interface. *)
type bus = { 
  read: int -> uint8;
  write: int -> uint8 -> unit;
}

let mk_bus ~read ~write = { read; write; }

(** The main type of MPU6502 emulator. *)
type t = {
  mutable power: bool;
  
  cpu: Cpu.t;
  (* ppu: Ppu.t; *)

  memory_bus: bus;

  (* Interrupt handler vector *)
  ith_nmi   : uint16;
  ith_reset : uint16;
  ith_irq   : uint16; 
}

type memory_section = { 
  from : Memory.usize_t; 
  to_ : Memory.usize_t; 
}

type memory_map_t = {
  wram : memory_section;  
}

let memory_map = Uint32.({
  wram = { from = of_int 0x0000; to_ = of_int 0x1FFF };
})


let mk () = 
  let cpu_wram = Memory.mk ~sz:MS_2KB ~ofs:0x0000 in 

  let bus_read_access n = Uint32.(
    let module Cpu_wram = (val cpu_wram : Memory.MEMORY) in
    let u = of_int n in 
    if compare u memory_map.wram.to_ < 0 then Cpu_wram.read u 
    else raise Exn.Out_of_range
  )
  in
  let bus_write_access n x = Uint32.(
    let module Cpu_wram = (val cpu_wram : Memory.MEMORY) in
    let u = of_int n in      
    if u <= memory_map.wram.to_ then Cpu_wram.write u x
    else raise Exn.Out_of_range
  )
  in
  {
    power = false;
    cpu = Cpu.mk ();
    memory_bus = mk_bus ~read:bus_read_access ~write:bus_write_access;
    ith_nmi = Uint16.of_int 0x8000;
    ith_reset = Uint16.of_int 0x8000;
    ith_irq = Uint16.of_int 0x8000;
  }