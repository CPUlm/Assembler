(** Module type of unsigned 16bit integers *)
module type UInt16 = sig
  type t
  type res = Single of t | Multiple of { low : t; high : t }

  val zero : t
  val of_int32 : int32 -> res
end

module UInt16 : UInt16
(** 16bit unsigned integer *)

(** Module type of 24bit signed integers *)
module type Int24 = sig
  type t

  val zero : t
  val of_int : int -> t option
end

module Int24 : Int24
(** 16bit signed integer *)

(** Module type of an offset in the program *)
module type Offset = sig
  type t

  val of_int : int -> t option
  (** Build an offset from an integer *)

  val fit_in_int24 : t -> bool
  (** [fit_in_int24 o] checks if [o] can be expressed as a 24bit signed integer *)

  val to_int24 : t -> Int24.t option
  (** [to_int24 o] returns the offset [o] expressed as an Int24 integer if possible *)
end

module Offset : Offset
(** Offsets in the program *)

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

module Immediate : Immediate
(** Immediates in the program *)

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

  val diff : t -> t -> Offset.t
  (** [diff a1 a2] : computes the offset between the address [a1] and the
      address [a2]. If not None, the result is such : [a1 + diff a1 a2 = a2] *)

  val with_offset : t -> Offset.t -> t
  (** [with_offset addr off] returns the address corresponding to [addr + off].
      Beware! Some normalisation could occur, the address could be ill-defined.
      Check with the following method. *)

  val well_defined : t -> Offset.t -> bool
  (** [with_offset addr off] checks if the address [addr + off] is well-defined. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print the address *)

  module Set : Set.S with type elt = t

  type set = Set.t

  module Map : Map.S with type key = t

  type 'a map = 'a Map.t
end

module ProgramAddress : Address
(** Addresses in the Program space *)

module MemoryAddress : Address
(** Addresses in the Memory space *)
