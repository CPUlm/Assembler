module UInt16 : sig
  type t
  type res = Single of t | Multiple of { low : t; high : t }

  val zero : t
  val of_int32 : int32 -> res
end = struct
  type t = int32
  type res = Single of t | Multiple of { low : t; high : t }

  let two_16 = Int32.(shift_left one 16)
  let is_16bit imm = 0l <= imm && imm < two_16
  let zero = 0l

  let of_int32 i =
    if is_16bit i then Single i
    else
      let low, high =
        (Int32.(logand i 0xffffl), Int32.(shift_right_logical i 16))
      in
      Multiple { low; high }
end

module Int24 : sig
  type t

  val zero : t
  val of_int : int -> t option
end = struct
  type t = int

  let two_23 = 1 lsl 23
  let neg_two_23 = -two_23
  let is_int24 i = neg_two_23 <= i && i < two_23
  let zero = 0
  let of_int i = if is_int24 i then Some i else None
end

module Immediate : sig
  type t

  val of_int : int -> t option
  (** Build an immediate from an integer *)

  val to_uint16 : t -> UInt16.res
  (** Convert it to a UInt16 *)

  val to_word : t -> bytes
  (** Convert it to a word *)
end = struct
  type t = int

  let two_32 = 1 lsl 32
  let two_31 = 1 lsl 31
  let neg_two_31 = -two_31
  let is_valid i = neg_two_31 <= i && i < two_32
  let of_int i = if is_valid i then Some i else None
  let to_uint16 i = UInt16.of_int32 (Int32.of_int i)

  let to_word i =
    let b = Bytes.create 4 in
    Bytes.set_int32_le b 0 (Int32.of_int i);
    b
end

module Offset : sig
  type t

  val of_int : int -> t option
  (** Build an offset from an integer *)

  val to_int : t -> int
end = struct
  type t = int

  let two_31 = 1 lsl 31
  let neg_two_31 = -two_31
  let is_valid i = neg_two_31 <= i && i < two_31
  let of_int i = if is_valid i then Some i else None
  let to_int = Fun.id
end

module type Address = sig
  type t

  val zero : t
  val last : t
  val add_section : t -> int -> t option
  val next : t -> t option
  val with_offset : t -> Offset.t -> t option
  val to_uint16 : t -> UInt16.res

  module Set : Set.S with type elt = t

  type set = Set.t

  module Map : Map.S with type key = t

  type 'a map = 'a Map.t
end

module Address : Address = struct
  type t = int

  let zero = 0
  let two_32 = 1 lsl 32
  let last = two_32 - 1
  let is_valid i = 0 <= i && i <= last

  let add_section t ssize =
    let new_addr = t + ssize in
    if is_valid new_addr then Some new_addr else None

  let next t =
    let new_addr = t + 1 in
    if is_valid new_addr then Some new_addr else None

  let with_offset t offset =
    let new_addr = t + Offset.to_int offset in
    if is_valid new_addr then Some new_addr else None

  let to_uint16 i = UInt16.of_int32 (Int32.of_int i)

  module Set = Set.Make (Int)

  type set = Set.t

  module Map = Map.Make (Int)

  type 'a map = 'a Map.t
end
