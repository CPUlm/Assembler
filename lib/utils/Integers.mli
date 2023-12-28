module type Word = sig
  type t

  val zero : t

  val add_at : t -> int32 -> int -> t

  val to_bytes : t -> bytes
end

(** A word in our machine *)
module Word : Word

(** Module type of unsigned 16bit integers *)
module type UInt16 = sig
  type t

  type res = Single of t | Multiple of {low: t; high: t}

  val zero : t

  val of_int32 : int32 -> res

  val encode : t -> int32
end

(** 16bit unsigned integer *)
module UInt16 : UInt16

(** Module type of 24bit signed integers *)
module type Int24 = sig
  type t

  val zero : t

  val of_int : int -> t option

  val encode : t -> int32
end

(** 16bit signed integer *)
module Int24 : Int24

(** Module type of an offset in the program *)
module type Offset = sig
  type t

  val of_int : int -> t option
  (** Build an offset from an integer *)

  val fit_in_int24 : t -> bool
  (** [fit_in_int24 o] checks if [o] can be expressed as a 24bit signed integer *)

  val to_int24 : t -> Int24.t option
  (** [to_int24 o] returns the offset [o] expressed as an Int24 integer if possible *)

  val pp : Format.formatter -> t -> unit
end

(** Offsets in the program *)
module Offset : Offset

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

(** Integer constant *)
module IntConstant : IntConstant

(** A 32bit address in the program *)
module type Address = sig
  type t

  val first : t
  (** The first address *)

  val last : t
  (** The last address *)

  val of_int : int -> t option
  (** Convert an integer to an address *)

  val next : t -> t option
  (** The next adress (if it exists) *)

  val to_uint16 : t -> UInt16.res
  (** Export the address to UInt16 *)

  val fit_in_uint16 : t -> int -> bool
  (** Checks if the address shifted by an integer fit in a UInt16 integer *)

  val offset_from_to : t -> t -> Offset.t
  (** [diff a1 a2] : computes the offset [o] between the address [a1] and the
      address [a2]. The result is such : [a1 + o = a2] *)

  val with_offset : t -> Offset.t -> t option
  (** [with_offset addr off] returns the address corresponding to [addr + off]. *)

  val well_defined : t -> Offset.t -> bool
  (** [with_offset addr off] checks if the address [addr + off] is well-defined. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print the address *)

  val is_after : t -> t -> bool
  (** [is_after a1 a2] checks if [a1] is after [a2] *)

  module Set : Set.S with type elt = t

  type set = Set.t

  module Map : Map.S with type key = t

  type 'a map = 'a Map.t
end

(** Addresses in the Program space *)
module ProgramAddress : Address

(** Addresses in the Memory space *)
module MemoryAddress : Address
