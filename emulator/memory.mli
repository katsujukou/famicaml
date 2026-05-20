open Famicaml_common.Nesint 

type byte = uint8

type usize_t = int

type memory_size = 
  | MS_1KB
  | MS_2KB 
  | MS_4KB 
  | MS_8KB 
  | MS_16KB 
  | MS_32KB 

val size_of : memory_size -> usize_t 

module type MEMORY = sig
  val size : usize_t
  val offset : usize_t 
  val read :  int -> byte
  val write : int -> byte -> unit  
end

val mk : sz:memory_size -> ofs:int -> (module MEMORY)