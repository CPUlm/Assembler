module type UniqueLabel = sig
  type t

  val fresh : string -> PositionUtils.position -> t

  val name : t -> string

  val position : t -> PositionUtils.position

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t

  type 'a map = 'a Map.t

  type set = Set.t
end

module ProgramLabel : UniqueLabel

module DataLabel : UniqueLabel

module SMap : Map.S with type key = string

module SSet : Set.S with type elt = string
