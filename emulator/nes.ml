open Stdint


(** The main type of MPU6502 emulator. *)
type t = {
  mutable power: bool;
  
  cpu: Cpu.t;
  (* ppu: Ppu.t; *)

  memory_bus: Bus.t;

  (* Interrupt handler vector *)
  ith_nmi   : uint16;
  ith_reset : uint16;
  ith_irq   : uint16; 
}

type memory_section = {
  from : Memory.usize_t;  (* 0x0000 は u16 上自明なため未読 *)
  to_ : Memory.usize_t;
} [@@warning "-69"]

type memory_map_t = {
  wram : memory_section;  
}

let memory_map = Uint32.({
  wram = { from = of_int 0x0000; to_ = of_int 0x1FFF };
})

let mk () = 
  let module Cpu_wram = (val (Memory.mk ~sz:MS_2KB ~ofs:0x0000) : Memory.MEMORY) in

  let bus_read_access p = Uint32.(
    let u = of_int (Uint16.to_int p) in
    if u <= memory_map.wram.to_ then Cpu_wram.read u
    else raise Exn.Out_of_range
  )
  in
  let bus_write_access p x = Uint32.(
    let u = of_int (Uint16.to_int p) in
    if u <= memory_map.wram.to_ then Cpu_wram.write u x
    else raise Exn.Out_of_range
  )
  in
  {
    power = false;
    cpu = Cpu.mk ();
    memory_bus = Bus.mk ~read:bus_read_access ~write:bus_write_access;
    ith_nmi = Uint16.of_int 0x8000;
    ith_reset = Uint16.of_int 0x8000;
    ith_irq = Uint16.of_int 0x8000;
  }
