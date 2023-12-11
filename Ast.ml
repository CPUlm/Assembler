(** Allowed Registers *)
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

(** Available Flags *)
type flag = Zero | Negative | UnsignedOverflowFlag | SignedOverflowFlag

(** Possible color of the text *)
type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BrightBlack
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite

(** Style of the text *)
type text_style =
  | Bold
  | Faint
  | Italic
  | Underline
  | Blinking
  | Hide
  | Crossed
  | Overline

(** A text constant *)
type text =
  | Concat of text * text
  | TextColor of color * text
  | BackColor of color * text
  | Style of text_style * text

(** All possible instructions *)
type inst =
  | Nop (* Pseudo instr *)
  (* Logical Operations *)
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Nor of reg * reg * reg
  | Xor of reg * reg * reg
  | Not of reg * reg (* Pseudo instr *)
  (* Arithmetic operation *)
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
  | Ret (* Pseudo instr *)

(** All possible data *)
type data = Text of text | UInt of Int32.t | Int of int

and file = {
  text : (string option * inst) list;
  data : (string option * data) list;
}
(** An assembly file, with its data and its text sections. *)
