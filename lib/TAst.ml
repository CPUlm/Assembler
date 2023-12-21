open Ast
open EncodeCommon

module type Id = sig
  type t

  val fresh : string option -> t
  val name : t -> string option

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

  type 'a map = 'a Map.t
  type set = Set.t
end

(** A module to manipulate named unique ids. *)
module ProgramLabel : Id = struct
  module CMP = struct
    type t = int * string option

    let compare (x : t) (y : t) = Stdlib.compare (fst x) (fst y)
  end

  type t = CMP.t

  let fresh =
    let cpt = ref 0 in
    fun f ->
      incr cpt;
      (!cpt, f)

  let name = snd

  module Map = Map.Make (CMP)
  module Set = Set.Make (CMP)

  type 'a map = 'a Map.t
  type set = Set.t
end

module ProgramAddress : Address = Address32Bit
module MemoryAddress : Address = Address32Bit

(** The mode of the load *)
type load_mode = HighHalf | LowHalf

type tinstr =
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
  | TLoadImmediateAdd of reg * Int16.t * load_mode * reg
  | TLoadProgLabelAdd of reg * ProgramLabel.t * reg
  | TLoadDataLabelAdd of reg * MemoryAddress.t * reg
  | TStore of reg * reg
  (* Flow instructions *)
  | TJmpAddr of reg
  | TJmpAddrCond of flag * reg
  | TJmpOffset of int * position
  | TJmpOffsetCond of flag * int * position
  | TJmpImmediate of ProgramAddress.t * position
  | TJmpImmediateCond of flag * ProgramAddress.t * position
  (* Function Call *)
  | TCallAddr of reg * position
  | TCallLabel of ProgramLabel.t * position

type pos_instr =
  (* Logical Operations *)
  | PAnd of reg * reg * reg
  | POr of reg * reg * reg
  | PNor of reg * reg * reg
  | PXor of reg * reg * reg
  (* Arithmetic operation *)
  | PAdd of reg * reg * reg
  | PSub of reg * reg * reg
  | PMul of reg * reg * reg
  | PDiv of reg * reg * reg
  (* Shifts operations *)
  | PShiftLeftLogical of reg * reg * reg
  | PShiftRightArith of reg * reg * reg
  | PShiftRightLogical of reg * reg * reg
  (* Memory operations *)
  | PLoad of reg * reg
  | PLoadImmediateAdd of reg * Int16.t * load_mode * reg
  | PLoadProgLabelAdd of reg * ProgramLabel.t * reg
  | PStore of reg * reg
  (* Flow instructions *)
  | PJmpLabel of ProgramLabel.t
  | PJmpLabelCond of flag * ProgramLabel.t
  | PJmpAddr of reg
  | PJmpAddrCond of flag * reg
  | PJmpImmediate of ProgramAddress.t
  | PJmpImmediateCond of flag * ProgramAddress.t

type data = TString of (int * bytes) | TInt of int32

module SSet = Set.Make (String)
module SMap = Map.Make (String)

(** [default_text_color] : Default Text Color *)
let default_text_color = Black

(** [default_background_color] : Default Background Color *)
let default_background_color = White

type data_section = {
  data_bytes : bytes;
  next_free : MemoryAddress.t;
  mapping : (MemoryAddress.t * position) SMap.t;
}

type instr_section = {
  instr_bytes : bytes;
  next_free : MemoryAddress.t;
  mapping : (MemoryAddress.t * position) SMap.t;
}
