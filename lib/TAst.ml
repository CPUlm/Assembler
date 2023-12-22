open PositionUtils
open Ast
open Integers

module type Id = sig
  type t

  val fresh : string option -> position -> t
  val name : t -> string option
  val position : t -> position

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

  type 'a map = 'a Map.t
  type set = Set.t
end

(** A module to manipulate named unique ids. *)
module ProgramLabel : Id = struct
  module CMP = struct
    type t = int * string option * position

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

(** The mode of the load *)
type load_mode = HighHalf | LowHalf

type reg = Ast.reg_kind

type tinstr = tinstr_kind pos
(** Type of a typed instruction, with its position in the source file. *)

and tinstr_kind =
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
  | TLoadImmediateAdd of reg * UInt16.t * load_mode * reg
  | TLoadProgLabelAdd of reg * ProgramLabel.t * reg option
  | TStore of reg * reg
  (* Flow instructions *)
  | TJmpAddr of flag option * reg
  | TJmpOffset of flag option * offset
  | TJmpImmediate of flag option * ProgramAddress.t pos
  (* Function Call *)
  | TCallAddr of reg
  | TCallLabel of ProgramLabel.t

type data = TString of (int * bytes) | TInt of Immediate.t

module SSet = Set.Make (String)
module SMap = Map.Make (String)

(** [default_text_color] : Default Text Color *)
let default_text_color = Black

(** [default_background_color] : Default Background Color *)
let default_background_color = White

type data_file = {
  data_bytes : bytes;
  mem_next_free : MemoryAddress.t;
  data_mapping : (MemoryAddress.t * position) SMap.t;
}

type instr_section = {
  label : ProgramLabel.t;
  body : tinstr Monoid.t;
  pos : position;
}

let get_instr i = i.v