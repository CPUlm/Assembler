open PositionUtils

module type UniqueLabel = sig
  type t

  val fresh : string -> position -> t
  val name : t -> string
  val position : t -> position

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

  type 'a map = 'a Map.t
  type set = Set.t
end

(** A module to manipulate named unique ids. *)
module UniqueLabel : UniqueLabel = struct
  module CMP = struct
    type t = int * string * position

    let compare ((x, _, _) : t) ((y, _, _) : t) = Stdlib.compare x y
  end

  type t = CMP.t

  let fresh =
    let cpt = ref 0 in
    fun f pos ->
      incr cpt;
      (!cpt, f, pos)

  let name (_, name, _) = name
  let position (_, _, pos) = pos

  module Map = Map.Make (CMP)
  module Set = Set.Make (CMP)

  type 'a map = 'a Map.t
  type set = Set.t
end

module ProgramLabel = UniqueLabel
module DataLabel = UniqueLabel
module SMap = Map.Make (String)
module SSet = Set.Make (String)
