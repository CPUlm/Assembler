open Integers

(** The mode of the load *)
type load_mode = HighHalf | LowHalf

(** Available Registers *)
type reg =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | R16
  | R17
  | R18
  | R19
  | R20
  | R21
  | R22
  | R23
  | R24
  | R25
  | R26
  | R27
  | ROut
  | SP
  | FP
  | PrivateReg

(** Available Flags *)
type flag = Zero | Negative | UnsignedUnderflowFlag | SignedOverflowFlag

(** Instructions Set of our Processor *)
type isa =
  (* Logical Operations *)
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Nor of reg * reg * reg
  | Xor of reg * reg * reg
  (* Arithmetic operation *)
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  (* Shifts operations *)
  | ShiftLeftLogical of reg * reg * reg
  | ShiftRightArith of reg * reg * reg
  | ShiftRightLogical of reg * reg * reg
  (* Memory operations *)
  | Load of reg * reg
  | LoadImmediateAdd of reg * reg * UInt16.t * load_mode
  | Store of reg * reg
  (* Flow instructions *)
  | Jmp of reg
  | JmpCond of reg * flag
  | JmpImmediate of Int24.t
  | JmpImmediateCond of Int24.t * flag
