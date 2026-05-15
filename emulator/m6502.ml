open Stdint

(** The main type of MPU6502 emulator. *)
type t = {
  power: bool;
  
  cpu: Cpu.t;
  (* ppu: Ppu.t; *)

  cartridge: Rom.Cartridge.t;

  (* Interrupt handler vector *)
  ith_nmi   : uint16;
  ith_reset : uint16;
  ith_irq   : uint16; 
}