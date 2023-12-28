open Labels
open Integers
open Isa
open PositionUtils

(** Type of a typed instruction, with its position in the source file. *)
type tinstr = tinstr_kind pos

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
  | TJmpLabel of flag option * ProgramLabel.t
  | TJmpRegister of flag option * reg
  | TJmpOffset of flag option * Offset.t pos
  | TJmpAddress of flag option * ProgramAddress.t pos
  (* Function Call *)
  | TCallAddr of reg
  | TCallLabel of ProgramLabel.t

type data = TString of (int * Word.t Monoid.t) | TInt of IntConstant.t

type instr_section =
  {label: ProgramLabel.t; body: tinstr Monoid.t; pos: position}

type tprog_instr =
  { prog_sections: instr_section Monoid.t
  ; prog_label_mapping: ProgramLabel.t SMap.t }

let get_instr i = i.v
