open Ast
open CompileString

module type Id = sig
  type t

  val fresh : unit -> t
  val compare : t -> t -> int

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

  type 'a map = 'a Map.t
  type set = Set.t
end

(** A module to manipulate unique ids. *)
module Label : Id = struct
  type t = int

  let fresh =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      !cpt

  let compare (x : t) (y : t) = Stdlib.compare x y

  module Map = Map.Make (Int)
  module Set = Set.Make (Int)

  type 'a map = 'a Map.t
  type set = Set.t
end

type instr =
  (* Logical Operations *)
  | TAnd of reg * reg * reg
  | TOr of reg * reg * reg
  | TNor of reg * reg * reg
  | TXor of reg * reg * reg
  (* Arithmetic operation *)
  | TAdd of reg * reg * reg
  | TSub of reg * reg * reg
  | TMul of reg * reg * reg
  | TDiv of reg * reg * reg
  (* Shifts operations *)
  | TShiftLeftLogical of reg * reg * reg
  | TShiftRightArith of reg * reg * reg
  | TShiftRightLogical of reg * reg * reg
  (* Memory operations *)
  | TLoad of reg * reg
  | TLoadImmediateAdd of reg * int32 * bool * reg
  | TLoadLabelAdd of reg * Label.t * bool * reg
  | TStore of reg * reg
  (* Flow instructions *)
  | TJmpLabel of Label.t
  | TJmpLabelCond of flag * Label.t
  | TJmpAddr of reg
  | TJmpAddrCond of flag * reg
  | TJmpOffset of int32
  | TJmpOffsetCond of flag * int32
  | TJmpImmediate of int32
  | TJmpImmediateCond of flag * int32
  (* Function Call *)
  | TCallAddr of reg
  | TCallLabel of Label.t

type data = TString of str | TInt of int32

module SSet = Set.Make (String)
module SMap = Map.Make (String)

(** [ret_reg] is the register erased when jumping back to the function. *)
let ret_reg = R19

(** [halt_reg] is the register erased when halting the program. *)
let halt_reg = R19
