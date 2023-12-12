open Ast
open TAst

let check_immediate imm = assert false

let check_writable_reg r =
  match r with
  | R0 | R1 | SP | FP -> assert false (* TODO : Print Warning here !*)
  | _ -> ()

let process_pseudo instr =
  match instr with
  | Nop -> TAnd (R0, R0, R0) |> Monoid.from_elm
  | And (r1, r2, r3) ->
      check_writable_reg r1;
      TAnd (r1, r2, r3) |> Monoid.from_elm
  | Or (r1, r2, r3) ->
      check_writable_reg r1;
      TOr (r1, r2, r3) |> Monoid.from_elm
  | Nor (r1, r2, r3) ->
      check_writable_reg r1;
      TNor (r1, r2, r3) |> Monoid.from_elm
  | Xor (r1, r2, r3) ->
      check_writable_reg r1;
      TXor (r1, r2, r3) |> Monoid.from_elm
  | Not (r1, r2) ->
      check_writable_reg r1;
      TXor (r1, r2, R0) |> Monoid.from_elm
  | Add (r1, r2, r3) ->
      check_writable_reg r1;
      TAdd (r1, r2, r3) |> Monoid.from_elm
  | Sub (r1, r2, r3) ->
      check_writable_reg r1;
      TSub (r1, r2, r3) |> Monoid.from_elm
  | Mul (r1, r2, r3) ->
      check_writable_reg r1;
      TMul (r1, r2, r3) |> Monoid.from_elm
  | Div (r1, r2, r3) ->
      check_writable_reg r1;
      TDiv (r1, r2, r3) |> Monoid.from_elm
  | Neg (r1, r2) ->
      check_writable_reg r1;
      TSub (r1, R0, r2) |> Monoid.from_elm
  | Incr (r1, r2) ->
      check_writable_reg r1;
      TAdd (r1, r2, R1) |> Monoid.from_elm
  | Decr (r1, r2) ->
      check_writable_reg r1;
      TSub (r1, r2, R1) |> Monoid.from_elm
  | ShiftLeftLogical (r1, r2, r3) ->
      check_writable_reg r1;
      TShiftLeftLogical (r1, r2, r3) |> Monoid.from_elm
  | ShiftRightArith (r1, r2, r3) ->
      check_writable_reg r1;
      TShiftRightArith (r1, r2, r3) |> Monoid.from_elm
  | ShiftRightLogical (r1, r2, r3) ->
      check_writable_reg r1;
      TShiftRightLogical (r1, r2, r3) |> Monoid.from_elm
  | Push r1 -> [ TStore (SP, r1); TAdd (SP, SP, R1) ] |> Monoid.from_list
  | Pop r1 ->
      check_writable_reg r1;
      [ TLoad (r1, SP); TSub (SP, SP, R1) ] |> Monoid.from_list
  | Load (r1, r2) ->
      check_writable_reg r1;
      TLoad (r1, r2) |> Monoid.from_elm
  | LoadImmediate (r1, imm, lhw) ->
      check_writable_reg r1;
      let imm = check_immediate imm in
      TLoadImmediateAdd (r1, imm, lhw, R0) |> Monoid.from_elm

(* | Load of reg * reg *)
(* | LoadImmediate of reg * int * bool *)
(* | LoadImmediateLabel of reg * label * bool *)
(* | LoadImmediateAdd of reg * int * bool * reg *)
(* | Store of reg * reg *)
(* | Mov of reg * reg Pseudo instr *)

(* | Nop (* Pseudo instr *)

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
      | JmpImmediate of int
      | JmpImmediateCond of flag * int
      | Halt
      (* Functions *)
      | CallLabel of string (* Pseudo instr *)
      | CallAddr of reg (* Pseudo instr *)
      | Ret (* Pseudo instr *)
*)
