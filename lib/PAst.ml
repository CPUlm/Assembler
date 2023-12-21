open Ast
open TAst
open Integers

(** Positioned instruction *)
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
  | PLoadImmediateAdd of reg * UInt16.t * load_mode * reg
  | PLoadProgLabelAdd of reg * ProgramLabel.t * reg
  | PStore of reg * reg
  (* Flow instructions *)
  | PJmpLabel of ProgramLabel.t
  | PJmpLabelCond of flag * ProgramLabel.t
  | PJmpAddr of reg
  | PJmpAddrCond of flag * reg
  | PJmpOffset of int
  | PJmpOffsetCond of flag * int
  | PJmpImmediate of ProgramAddress.t
  | PJmpImmediateCond of flag * ProgramAddress.t
