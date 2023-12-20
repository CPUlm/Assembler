open Ast

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
module ProgrammLabel : Id = struct
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

module Int16 : sig
  type t
  type res = Single of t | Multiple of { low : t; high : t }

  val of_int32 : int32 -> res
end = struct
  type t = int32
  type res = Single of t | Multiple of { low : t; high : t }

  let two_16 = Int32.(shift_left one 16)
  let is_16bit imm = 0l <= imm && imm < two_16

  let of_int32 i =
    if is_16bit i then Single i
    else
      let low, high =
        (Int32.(logand i 0xffffl), Int32.(shift_right_logical i 16))
      in
      Multiple { low; high }
end

module ProgrammAddress = struct
  type t = int32
end

module DataAddress = struct
  type t = int32
end

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
  | TLoadProgLabelAdd of reg * ProgrammLabel.t * reg
  | TLoadDataLabelAdd of reg * DataAddress.t * reg
  | TStore of reg * reg
  (* Flow instructions *)
  | TJmpLabel of ProgrammLabel.t
  | TJmpLabelCond of flag * ProgrammLabel.t
  | TJmpAddr of reg
  | TJmpAddrCond of flag * reg
  | TJmpOffset of ProgrammAddress.t
  | TJmpOffsetCond of flag * ProgrammAddress.t
  | TJmpImmediate of ProgrammAddress.t
  | TJmpImmediateCond of flag * ProgrammAddress.t
  (* Function Call *)
  | TCallAddr of reg
  | TCallLabel of ProgrammLabel.t

type data = TString of (int32 * bytes) | TInt of int32

module SSet = Set.Make (String)
module SMap = Map.Make (String)

(** [ret_reg] is the register erased when jumping back to the function. *)
let ret_reg = PrivateReg

(** [halt_reg] is the register erased when halting the program. *)
let halt_reg = PrivateReg

(** [default_text_color] : Default Text Color *)
let default_text_color = Black

(** [default_background_color] : Default Background Color *)
let default_background_color = White

type data_section = { data : bytes; size : int32; mapping : int32 SMap.t }
