open Ast
open TAst
open ErrorUtils

let two_32 = Int.shift_left 1 32
let two_31 = Int.shift_left 1 31
let neg_two_31 = -two_31
let signed i = neg_two_31 <= i && i < two_31
let unsigned i = 0 <= i && i < two_32

let check_immediate imm =
  if signed imm.v || unsigned imm.v then imm.v
  else
    let txt =
      Format.sprintf "The value '%i' cannot be represented on 16 bit." imm.v
    in
    type_error txt imm.pos

let check_signed_immediate imm =
  if signed imm.v then imm.v
  else
    let txt =
      Format.sprintf
        "The value '%i' cannot be represented as a 16-bit signed integer." imm.v
    in
    type_error txt imm.pos

let check_unsigned_immediate imm =
  if unsigned imm.v then imm.v
  else
    let txt =
      Format.sprintf
        "The value '%i' cannot be represented as a 16-bit unsigned integer."
        imm.v
    in
    type_error txt imm.pos

let check_writable_reg r p =
  match r with
  | R0 | R1 | SP | FP ->
      let txt =
        Format.sprintf "Writing to read-only register '%s'."
          (match r with
          | R0 -> "r0"
          | R1 -> "r1"
          | SP -> "sp"
          | FP -> "fp"
          | _ -> failwith "Impossible case.")
      in
      warning txt p.pos
  | _ -> ()

let check_label labels label =
  match TAst.SMap.find_opt label.v labels with
  | Some i -> i
  | None ->
      let txt = Format.sprintf "The label '%s' is not defined." label.v in
      type_error txt label.pos

let process_pseudo labels instr =
  match instr.v with
  | Nop -> [ TAnd (R0, R0, R0) ]
  | And (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TAnd (r1, r2, r3) ]
  | Or (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TOr (r1, r2, r3) ]
  | Nor (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TNor (r1, r2, r3) ]
  | Xor (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TXor (r1, r2, r3) ]
  | Not (r1, r2) ->
      check_writable_reg r1 instr;
      [ TXor (r1, r2, R0) ]
  | Add (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TAdd (r1, r2, r3) ]
  | Sub (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TSub (r1, r2, r3) ]
  | Mul (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TMul (r1, r2, r3) ]
  | Div (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TDiv (r1, r2, r3) ]
  | Neg (r1, r2) ->
      check_writable_reg r1 instr;
      [ TSub (r1, R0, r2) ]
  | Incr (r1, r2) ->
      check_writable_reg r1 instr;
      [ TAdd (r1, r2, R1) ]
  | Decr (r1, r2) ->
      check_writable_reg r1 instr;
      [ TSub (r1, r2, R1) ]
  | ShiftLeftLogical (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TShiftLeftLogical (r1, r2, r3) ]
  | ShiftRightArith (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TShiftRightArith (r1, r2, r3) ]
  | ShiftRightLogical (r1, r2, r3) ->
      check_writable_reg r1 instr;
      [ TShiftRightLogical (r1, r2, r3) ]
  | Push r1 -> [ TStore (SP, r1); TAdd (SP, SP, R1) ]
  | Pop r1 ->
      check_writable_reg r1 instr;
      [ TLoad (r1, SP); TSub (SP, SP, R1) ]
  | Load (r1, r2) ->
      check_writable_reg r1 instr;
      [ TLoad (r1, r2) ]
  | LoadImmediate (r1, imm, lhw) ->
      check_writable_reg r1 instr;
      let imm = check_immediate imm in
      [ TLoadImmediateAdd (r1, imm, lhw, R0) ]
  | LoadImmediateLabel (r1, label, lhw) ->
      check_writable_reg r1 instr;
      let lid = check_label labels label in
      [ TLoadLabelAdd (r1, lid, lhw, R0) ]
  | LoadImmediateAdd (r1, imm, lhw, r2) ->
      check_writable_reg r1 instr;
      let imm = check_immediate imm in
      [ TLoadImmediateAdd (r1, imm, lhw, r2) ]
  | LoadImmediateAddLabel (r1, label, lhw, r2) ->
      check_writable_reg r1 instr;
      let lid = check_label labels label in
      [ TLoadLabelAdd (r1, lid, lhw, r2) ]
  | Store (r1, r2) ->
      check_writable_reg r1 instr;
      [ TStore (r1, r2) ]
  | Mov (r1, r2) ->
      check_writable_reg r1 instr;
      [ TAdd (r1, r2, R0) ]
  | Test r1 -> [ TAdd (R0, r1, R0) ]
  | JmpLabel label ->
      let lid = check_label labels label in
      [ TJmpLabel lid ]
  | JmpLabelCond (f, label) ->
      let lid = check_label labels label in
      [ TJmpLabelCond (f, lid) ]
  | JmpAddr r1 -> [ TJmpAddr r1 ]
  | JmpAddrCond (f, r1) -> [ TJmpAddrCond (f, r1) ]
  | JmpOffset imm ->
      let imm = check_signed_immediate imm in
      [ TJmpOffset imm ]
  | JmpOffsetCond (f, imm) ->
      let imm = check_signed_immediate imm in
      [ TJmpOffsetCond (f, imm) ]
  | JmpImmediate imm ->
      let imm = check_unsigned_immediate imm in
      [ TJmpImmediate imm ]
  | JmpImmediateCond (f, imm) ->
      let imm = check_unsigned_immediate imm in
      [ TJmpImmediateCond (f, imm) ]
  | Halt ->
      [
        (* We set [halt_reg] to 0xffff *)
        TLoadImmediateAdd (halt_reg, 0xffff, false, R0);
        (* We add 0xffff0000 to [halt_reg] *)
        TLoadImmediateAdd (halt_reg, 0xffff, true, halt_reg);
        (* We Jump to 0xffffffff (the value of [halt_reg]) *)
        TJmpAddr halt_reg;
      ]
  | CallAddr r -> [ TCallAddr r ]
  | CallLabel label ->
      let lid = check_label labels label in
      [ TCallLabel lid ]
  | Ret ->
      [
        (* We move SP to FP. *)
        TAdd (SP, FP, R0);
        (* We read the value pointed by SP (old FP) into FP. *)
        TLoad (FP, SP);
        (* We let SP point to the return address. *)
        TSub (SP, SP, R1);
        (* We read the value pointed by SP (ret address) into a temporary register : [ret_reg]. *)
        TLoad (ret_reg, SP);
        (* We restore SP to the original state before the call. *)
        TSub (SP, SP, R1);
        (* We jump to the return address. *)
        TJmpAddr ret_reg;
      ]

let check_file (_ : Ast.file) :
    (Label.t * TAst.instr list) list * (Label.t * TAst.data list) list =
  assert false
