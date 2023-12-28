open Integers
open Isa
open PositionUtils

(** A immediate with a position *)
type immediate = Immediate.t pos

(** A immediate with a position *)
type program_address = ProgramAddress.t pos

(** A immediate with a position *)
type offset = Offset.t pos

(** A string with a position *)
type label = string pos

(** Available Registers *)
type reg = Isa.reg pos

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

(** [default_text_color] : Default Text Color *)
let default_text_color = White

(** [default_background_color] : Default Background Color *)
let default_background_color = Black

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
  | Default

(** A text constant *)
type text =
  | Concat of text * text
  | TextColor of color * text
  | BackColor of color * text
  | Style of text_style * text
  | Text of string

(** All possible instructions *)
type inst = inst_kind pos

and inst_kind =
  | AstNop (* Pseudo instr *)
  (* Logical Operations *)
  | AstAnd of reg * reg * reg
  | AstOr of reg * reg * reg
  | AstNor of reg * reg * reg
  | AstXor of reg * reg * reg
  | AstNot of reg * reg (* Pseudo instr *)
  (* Arithmetic operation *)
  | AstAdd of reg * reg * reg
  | AstSub of reg * reg * reg
  | AstMul of reg * reg * reg
  | AstDiv of reg * reg * reg
  | AstNeg of reg * reg (* Pseudo instr *)
  | AstIncr of reg * reg (* Pseudo instr *)
  | AstDecr of reg * reg (* Pseudo instr *)
  (* Shifts operations *)
  | AstShiftLeftLogical of reg * reg * reg
  | AstShiftRightArith of reg * reg * reg
  | AstShiftRightLogical of reg * reg * reg
  (* Stack operation *)
  | AstPush of reg (* Pseudo instr *)
  | AstPop of reg (* Pseudo instr *)
  (* Memory operations *)
  | AstLoad of reg * reg
  | AstLoadImmediateAdd of reg * immediate * reg option
  | AstLoadImmediateAddLabel of reg * label * reg option
  | AstStore of reg * reg
  | AstMov of reg * reg (* Pseudo instr *)
  (* Flow instructions *)
  | AstTest of reg (* Pseudo instr *)
  | AstJmpLabel of flag option * label (* Pseudo instr *)
  | AstJmpAddr of flag option * reg
  | AstJmpOffset of flag option * offset
  | AstJmpImmediate of flag option * program_address
  | AstHalt
  (* Functions *)
  | AstCallLabel of label (* Pseudo instr *)
  | AstCallAddr of reg (* Pseudo instr *)
  | AstRet (* Pseudo instr *)

(** All possible data *)
type data = data_kind pos

and data_kind = Str of text | Int of immediate

(** An assembly file, with its data and its text sections. *)
type file = {text: (label option * inst) list; data: (label option * data) list}
