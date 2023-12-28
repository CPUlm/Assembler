let _ =
  if Sys.int_size < 33 then
    failwith "The size of the integers available is not large enough."

(** First possible address : 0 *)
let first_address = 0

(** First possible address : 2^32 - 1 = 0xffffffff*)
let last_address = 0xffffffff

(** [is_uint16 i] tests if i in [\[0; 2^16 - 1\]]
    with [2^16 - 1 = 0xffff] *)
let is_uint16 i = 0 <= i && i <= 0xffff

(** [is_int24 i] tests if i in [\[-2^23; 2^23 - 1\]],
    with [-2^23 = -0x800000] and [2^23 - 1 = 0x7fffff]*)
let is_int24 i = -0x800000 <= i && i <= 0x7fffff

(** [is_int32 i] tests if i in [\[-2^31; 2^31 - 1\]],
    with [-2^31 = -0x80000000] and [2^31 - 1 = 0x7fffffff]*)
let is_int32 i = -0x80000000 <= i && i < 0x7fffffff

let is_uint32 i = 0 <= i && i <= last_address

module type Word = sig
  type t

  val zero : t

  val add_at : t -> int32 -> int -> t

  val to_bytes : t -> bytes
end

module Word = struct
  type t = int32

  let zero = 0l

  let add_at v c pos = Int32.(logor v (shift_left c pos))

  let to_bytes v =
    let b = Bytes.create 4 in
    Bytes.set_int32_le b 0 v ; b
end

(** Module type of unsigned 16bit integers *)
module type UInt16 = sig
  type t

  type res = Single of t | Multiple of {low: t; high: t}

  val zero : t

  val of_int32 : int32 -> res

  val encode : t -> int32
end

module UInt16 = struct
  type t = int32

  type res = Single of t | Multiple of {low: t; high: t}

  let is_uint16 imm = 0l <= imm && imm <= 0xffffl

  let zero = 0l

  let of_int32 i =
    if is_uint16 i then Single i
    else
      let low, high =
        (Int32.(logand i 0xffffl), Int32.(shift_right_logical i 16))
      in
      Multiple {low; high}

  let encode = Fun.id
end

(** Module type of 24bit signed integers *)
module type Int24 = sig
  type t

  val zero : t

  val of_int : int -> t option

  val encode : t -> int32
end

module Int24 = struct
  type t = int32

  let zero = 0l

  let of_int i = if is_int24 i then Some (Int32.of_int i) else None

  let encode = Fun.id
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

(** Module type of an integer value in the program or the data, ie. a 32bit constant. *)
module type IntConstant = sig
  type t

  val of_int : int -> t option
  (** Build an immediate from an integer *)

  val to_uint16 : t -> UInt16.res
  (** Convert it to a UInt16 *)

  val to_word : t -> Word.t
  (** Convert it to a word *)
end

module IntConstant = struct
  type t = int

  let of_int i = if is_int32 i || is_uint32 i then Some i else None

  let to_uint16 i = UInt16.of_int32 (Int32.of_int i)

  let to_word i = Int32.of_int i
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

  val offset_from_to : t -> t -> Offset.t

  val with_offset : t -> Offset.t -> t option

  val well_defined : t -> Offset.t -> bool

  val pp : Format.formatter -> t -> unit

  val is_after : t -> t -> bool

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

  (** [offset_from_to a1 a2] returns an offset [o] such as [a1 + o = a2]*)
  let offset_from_to a1 a2 = a2 - a1

  let with_offset addr offs = of_int (addr + offs)

  let well_defined addr offs = Option.is_some (with_offset addr offs)

  let pp ppf t = Format.fprintf ppf "%x" t

  (** [is_after a1 a2] checks if [a1] is after [a2] *)
  let is_after a1 a2 = a1 >= a2

  module Set = Set.Make (Int)

  type set = Set.t

  module Map = Map.Make (Int)

  type 'a map = 'a Map.t
end

module ProgramAddress = Address
module MemoryAddress = Address
