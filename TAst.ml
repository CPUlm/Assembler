open Ast

type text = Bytes.t
type data = Text of text | UInt of int | Int of int

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
  | TLoadImmediateAdd of reg * int * bool * reg
  | TStore of reg * reg
  (* Flow instructions *)
  | TJmpLabel of string
  | TJmpLabelCond of flag * string
  | TJmpAddr of reg
  | TJmpAddrCond of flag * reg
  | TJmpOffset of int
  | TJmpOffsetCond of flag * int
  | TJmpImmediate of int
  | TJmpImmediateCond of flag * int
