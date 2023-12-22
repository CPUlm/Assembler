let _ =
  if Sys.int_size < 33 then
    failwith "The size of the integers available is not large enough."

(** First possible address : 0 *)
let first_address = 0

(** First possible address : 2^32 - 1 *)
let last_address = (1 lsl 32) - 1

let is_uint16 =
  let two_16 = 1 lsl 16 in
  fun i -> 0 <= i && i < two_16

let is_int24 =
  let two_23 = 1 lsl 23 in
  let neg_two_23 = -two_23 in
  fun i -> neg_two_23 <= i && i < two_23

let is_int32 =
  let two_31 = 1 lsl 31 in
  let neg_two_31 = -(1 lsl 31) in
  fun i -> neg_two_31 <= i && i < two_31

let is_uint32 i = 0 <= i && i <= last_address

(** Module type of unsigned 16bit integers *)
module type UInt16 = sig
  type t
  type res = Single of t | Multiple of { low : t; high : t }

  val zero : t
  val of_int32 : int32 -> res
end

module UInt16 = struct
  type t = int32
  type res = Single of t | Multiple of { low : t; high : t }

  let two_16 = Int32.(shift_left one 16)
  let is_uint16 imm = 0l <= imm && imm < two_16
  let zero = 0l

  let of_int32 i =
    if is_uint16 i then Single i
    else
      let low, high =
        (Int32.(logand i 0xffffl), Int32.(shift_right_logical i 16))
      in
      Multiple { low; high }
end

(** Module type of 24bit signed integers *)
module type Int24 = sig
  type t

  val zero : t
  val of_int : int -> t option
end

module Int24 = struct
  type t = int

  let zero = 0
  let of_int i = if is_int24 i then Some i else None
end

(** Module type of an offset in the program *)
module type Offset = sig
  type t

  val of_int : int -> t option
  (** Build an offset from an integer *)

  val fit_in_int24 : t -> bool
  val to_int24 : t -> Int24.t option
  val pp : Format.formatter -> t -> unit
end

module Offset = struct
  type t = int

  let min_offset = first_address - last_address
  let max_offset = last_address - first_address
  let is_valid i = min_offset <= i && i <= max_offset
  let of_int i = if is_valid i then Some i else None
  let fit_in_int24 t = is_int24 t
  let to_int24 t = Int24.of_int t
  let pp ppf t = Format.fprintf ppf "%d" t
end

(** Module type of an immediate value in the program, ie. a 32bit constant. *)
module type Immediate = sig
  type t

  val of_int : int -> t option
  (** Build an immediate from an integer *)

  val to_uint16 : t -> UInt16.res
  (** Convert it to a UInt16 *)

  val to_word : t -> bytes
  (** Convert it to a word *)
end

module Immediate = struct
  type t = int

  let of_int i = if is_int32 i || is_uint32 i then Some i else None
  let to_uint16 i = UInt16.of_int32 (Int32.of_int i) (* TODO : Check This ! *)

  let to_word i =
    let b = Bytes.create 4 in
    Bytes.set_int32_le b 0 (Int32.of_int i);
    b
end

(** A 32bit address in the program *)
module type Address = sig
  type t

  val first : t
  val last : t
  val of_int : int -> t option
  val next : t -> t option
  val to_uint16 : t -> UInt16.res
  val fit_in_uint16 : t -> int -> bool
  val diff : t -> t -> Offset.t
  val with_offset : t -> Offset.t -> t
  val well_defined : t -> Offset.t -> bool
  val pp : Format.formatter -> t -> unit

  module Set : Set.S with type elt = t

  type set = Set.t

  module Map : Map.S with type key = t

  type 'a map = 'a Map.t
end

module Address = struct
  type t = int

  let first = first_address
  let last = last_address
  let of_int i = if first_address <= i && i <= last then Some i else None
  let next t = of_int (t + 1)
  let to_uint16 i = UInt16.of_int32 (Int32.of_int i)

  let fit_in_uint16 t i =
    match of_int (t + i) with Some i -> is_uint16 i | None -> false

  let diff a1 a2 = a1 - a2

  (** Note: The normalization of the address is implementations defined.
       Here we take the address modulo (first_address - last_address).
       But other implementations are possibles ! *)
  let rec normalize_addr addr =
    if addr < first_address then
      (* The address too small, we wrap-around to have a (very) large address *)
      normalize_addr (addr + last_address)
    else if addr > last_address then
      (* The address too large, we wrap-around to have a (very) small address *)
      normalize_addr (addr - last_address)
    else addr

  let with_offset addr offs = normalize_addr (addr + offs)
  let well_defined addr offs = Option.is_some (of_int (addr + offs))
  let pp ppf t = Format.fprintf ppf "%x" t

  module Set = Set.Make (Int)

  type set = Set.t

  module Map = Map.Make (Int)

  type 'a map = 'a Map.t
end

module ProgramAddress = Address
module MemoryAddress = Address
