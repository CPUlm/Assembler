type position = {
  beg_col : int;
  beg_line : int;
  end_col : int;
  end_line : int;
  file : string;
}

type 'a pos = { v : 'a; pos : position }
type label = string pos

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
  | ROut
  | SP
  | FP
  | PrivateReg

(** Available Flags *)
type flag = Zero | Negative | UnsignedUnderflowFlag | SignedOverflowFlag

(** Possible color of the text *)
and color =
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
and text_style =
  | Bold
  | Faint
  | Italic
  | Underline
  | Blinking
  | Hide
  | Crossed
  | Overline
  | Default

(** A text constant *)
and text =
  | Concat of text * text
  | TextColor of color * text
  | BackColor of color * text
  | Style of text_style * text
  | Text of string

type immediate = int pos
(** An immediate with position *)

type inst = inst_kind pos
(** All possible instructions *)

and inst_kind =
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
  | Incr of reg * reg (* Pseudo instr *)
  | Decr of reg * reg (* Pseudo instr *)
  (* Shifts operations *)
  | ShiftLeftLogical of reg * reg * reg
  | ShiftRightArith of reg * reg * reg
  | ShiftRightLogical of reg * reg * reg
  (* Stack operation *)
  | Push of reg (* Pseudo instr *)
  | Pop of reg (* Pseudo instr *)
  (* Memory operations *)
  | Load of reg * reg
  | LoadImmediate of reg * immediate
  | LoadImmediateLabel of reg * label
  | LoadImmediateAdd of reg * immediate * reg
  | LoadImmediateAddLabel of reg * label * reg
  | Store of reg * reg
  | Mov of reg * reg (* Pseudo instr *)
  (* Flow instructions *)
  | Test of reg (* Pseudo instr *)
  | JmpLabel of label (* Pseudo instr *)
  | JmpLabelCond of flag * label (* Pseudo instr *)
  | JmpAddr of reg
  | JmpAddrCond of flag * reg
  | JmpOffset of immediate
  | JmpOffsetCond of flag * immediate
  | JmpImmediate of immediate
  | JmpImmediateCond of flag * immediate
  | Halt
  (* Functions *)
  | CallLabel of label (* Pseudo instr *)
  | CallAddr of reg (* Pseudo instr *)
  | Ret (* Pseudo instr *)

type data = data_kind pos
(** All possible data *)

and data_kind = Ascii of text | Str of text | Int of immediate

type file = {
  text : (label option * inst) list;
  data : (label option * data) list;
}
(** An assembly file, with its data and its text sections. *)
