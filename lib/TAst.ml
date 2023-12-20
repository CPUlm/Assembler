open Ast

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
  | TLoadImmediateAdd of reg * int32 * bool * reg
  | TLoadProgLabelAdd of reg * string * bool * reg
  | TLoadDataLabelAdd of reg * int32 * bool * reg
  | TStore of reg * reg
  (* Flow instructions *)
  | TJmpLabel of string
  | TJmpLabelCond of flag * string
  | TJmpAddr of reg
  | TJmpAddrCond of flag * reg
  | TJmpOffset of int32
  | TJmpOffsetCond of flag * int32
  | TJmpImmediate of int32
  | TJmpImmediateCond of flag * int32
  (* Function Call *)
  | TCallAddr of reg
  | TCallLabel of string

type data = TString of (int32 * bytes) | TInt of int32

module SSet = Set.Make (String)
module SMap = Map.Make (String)

(** [ret_reg] is the register erased when jumping back to the function. *)
let ret_reg = R19

(** [halt_reg] is the register erased when halting the program. *)
let halt_reg = R19

(** [default_text_color] : Default Text Color *)
let default_text_color = Black

(** [default_background_color] : Default Background Color *)
let default_background_color = White

type data_section = { data : bytes; size : int32; mapping : int32 SMap.t }
