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

let size_of sz =
  match sz with
  | MS_1KB -> 0x0400
  | MS_2KB -> 0x0800
  | MS_4KB -> 0x1000
  | MS_8KB -> 0x2000
  | MS_16KB -> 0x4000
  | MS_32KB -> 0x8000

module type MEMORY = sig
  val size : usize_t
  val offset : usize_t
  val read : int -> byte
  val write : int -> byte -> unit
end

let mk ~sz ~ofs =
  let usize = size_of sz in
  let repr = Bytes.create usize in
  (module struct
    let repr = repr
    let size = usize
    let offset = ofs
    let int_of_addr u = Int.rem (u - offset) usize
    let read u = Uint8.of_int @@ Bytes.get_uint8 repr (int_of_addr u)
    let write u x = Bytes.set_uint8 repr (int_of_addr u) (Uint8.to_int x)
  end : MEMORY)
