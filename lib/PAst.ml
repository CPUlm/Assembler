open Ast
open TAst
open EncodeCommon

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
  | PLoadImmediateAdd of reg * Int16.t * load_mode * reg
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

(** Final instruction : ie. the ISA of the target processor. *)
type finstr =
  (* Logical Operations *)
  | FAnd of reg * reg * reg
  | FOr of reg * reg * reg
  | FNor of reg * reg * reg
  | FXor of reg * reg * reg
  (* Arithmetic operation *)
  | FAdd of reg * reg * reg
  | FSub of reg * reg * reg
  | FMul of reg * reg * reg
  | FDiv of reg * reg * reg
  (* Shifts operations *)
  | FShiftLeftLogical of reg * reg * reg
  | FShiftRightArith of reg * reg * reg
  | FShiftRightLogical of reg * reg * reg
  (* Memory operations *)
  | FLoad of reg * reg
  | FLoadImmediateAdd of reg * Int16.t * load_mode * reg
  | FStore of reg * reg
  (* Flow instructions *)
  | FJmpAddr of reg
  | FJmpAddrCond of flag * reg
  | FJmpImmediate of Int16.t (* Relative *)
  | FJmpImmediateCond of flag * Int16.t (* Relative *)
