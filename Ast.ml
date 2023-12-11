exception Except of string

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
  | R28
  | ROut
  | SP
  | FP
(* PC est caché *)


let int_to_reg i = match i with
| 0 -> R0
| 1 -> R1
| 2 -> R2
| 3 -> R3
| 4 -> R4
| 5 -> R5
| 6 -> R6
| 7 -> R7
| 8 -> R8
| 9 -> R9
| 10 -> R10
| 11 -> R11
| 12 -> R12
| 13 -> R13
| 14 -> R14
| 15 -> R15
| 16 -> R16
| 17 -> R17
| 18 -> R18
| 19 -> R19
| 20 -> R20
| 21 -> R21
| 22 -> R22
| 23 -> R23
| 24 -> R24
| 25 -> R25 
| 26 -> R26
| 27 -> R27
| 28 -> R28
| _ -> raise (Except ("Unknown register : " ^ (string_of_int i)))
type flag = Zero | Negative | UnsignedOverflowFlag | SignedOverflowFlag

type inst =
  | Nop (* Pseudo instr *)
  (* Logical Operations *)
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Nor of reg * reg * reg
  | Xor of reg * reg * reg
  | Not of reg * reg (* Pseudo instr *)
  (* Arthmetic operation *)
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Neg of reg * reg (* Pseudo instr *)
  (* Shifts operations *)
  | ShiftLeftLogical of reg * reg * reg
  | ShiftRightArith of reg * reg * reg
  | ShiftRightLogical of reg * reg * reg
  (* Stack operation *)
  | Push of reg (* Pseudo instr *)
  | Pop of reg (* Pseudo instr *)
  (* Memory operations *)
  | Load of reg * reg
  | LoadImmediate of reg * int * bool
  | LoadImmediateAdd of reg * int * bool * reg
  | Store of reg * reg
  | Mov of reg * reg (* Pseudo instr *)
  (* Flow instructions *)
  | Test of reg (* Pseudo instr *)
  | JmpLabel of string (* Pseudo instr *)
  | JmpLabelCond of flag * string (* Pseudo instr *)
  | JmpAddr of reg
  | JmpAddrCond of flag * reg
  | JmpOffset of int
  | JmpOffsetCond of flag * int
  (* Functions *)
  | CallLabel of string (* Pseudo instr *)
  | CallAddr of reg (* Pseudo instr *)
  | CallOffer of int (* Pseudo instr *)
  | Ret (* Pseudo instr *)

and file = (string option * inst) list
