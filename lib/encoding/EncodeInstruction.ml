open Ast
open TAst
open EncodeCommon
open ErrorUtils

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
      warning txt (Some p.pos)
  | _ -> ()

let check_prog_label labels label =
  if SSet.mem label.v labels then label.v
  else
    let txt = Format.sprintf "The label '%s' is not defined." label.v in
    type_error txt (Some label.pos)

let process_load_label data_sections prog_labels l =
  match SMap.find_opt l.v data_sections.mapping with
  | Some i -> Either.left i
  | None -> check_prog_label prog_labels l |> Either.right

(** [process_instr data_sections prog_labels instr] : Check that the instruction
    is wellformed and return a siplified version of it, with the program label
    used if so, and the data label used if so.

    The boolean flag returned with the potentionally used program label, is here
    to mark if its associated address must be stored in memory. *)
let process_instr data_sections prog_labels instr =
  let inst r = (r, None, None) in
  match instr.v with
  | Nop -> inst [ TAnd (R0, R0, R0) ]
  | And (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TAnd (r1, r2, r3) ]
  | Or (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TOr (r1, r2, r3) ]
  | Nor (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TNor (r1, r2, r3) ]
  | Xor (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TXor (r1, r2, r3) ]
  | Not (r1, r2) ->
      check_writable_reg r1 instr;
      inst [ TXor (r1, r2, R0) ]
  | Add (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TAdd (r1, r2, r3) ]
  | Sub (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TSub (r1, r2, r3) ]
  | Mul (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TMul (r1, r2, r3) ]
  | Div (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TDiv (r1, r2, r3) ]
  | Neg (r1, r2) ->
      check_writable_reg r1 instr;
      inst [ TSub (r1, R0, r2) ]
  | Incr (r1, r2) ->
      check_writable_reg r1 instr;
      inst [ TAdd (r1, r2, R1) ]
  | Decr (r1, r2) ->
      check_writable_reg r1 instr;
      inst [ TSub (r1, r2, R1) ]
  | ShiftLeftLogical (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TShiftLeftLogical (r1, r2, r3) ]
  | ShiftRightArith (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TShiftRightArith (r1, r2, r3) ]
  | ShiftRightLogical (r1, r2, r3) ->
      check_writable_reg r1 instr;
      inst [ TShiftRightLogical (r1, r2, r3) ]
  | Push r1 -> inst [ TStore (SP, r1); TAdd (SP, SP, R1) ]
  | Pop r1 ->
      check_writable_reg r1 instr;
      inst [ TLoad (r1, SP); TSub (SP, SP, R1) ]
  | Load (r1, r2) ->
      check_writable_reg r1 instr;
      inst [ TLoad (r1, r2) ]
  | LoadImmediate (r1, imm, lhw) ->
      check_writable_reg r1 instr;
      let imm = check_immediate imm in
      inst [ TLoadImmediateAdd (r1, imm, lhw, R0) ]
  | LoadImmediateLabel (r1, label, lhw) -> (
      check_writable_reg r1 instr;
      match process_load_label data_sections prog_labels label with
      | Left lid ->
          ([ TLoadDataLabelAdd (r1, lid, lhw, R0) ], None, Some label.v)
      | Right lid ->
          ([ TLoadProgLabelAdd (r1, lid, lhw, R0) ], Some (true, lid), None))
  | LoadImmediateAdd (r1, imm, lhw, r2) ->
      check_writable_reg r1 instr;
      let imm = check_immediate imm in
      inst [ TLoadImmediateAdd (r1, imm, lhw, r2) ]
  | LoadImmediateAddLabel (r1, label, lhw, r2) -> (
      check_writable_reg r1 instr;
      match process_load_label data_sections prog_labels label with
      | Left lid ->
          ([ TLoadDataLabelAdd (r1, lid, lhw, r2) ], None, Some label.v)
      | Right lid ->
          ([ TLoadProgLabelAdd (r1, lid, lhw, r2) ], Some (true, lid), None))
  | Store (r1, r2) ->
      check_writable_reg r1 instr;
      inst [ TStore (r1, r2) ]
  | Mov (r1, r2) ->
      check_writable_reg r1 instr;
      inst [ TAdd (r1, r2, R0) ]
  | Test r1 -> inst [ TAdd (R0, r1, R0) ]
  | JmpLabel label ->
      let lid = check_prog_label prog_labels label in
      ([ TJmpLabel lid ], Some (false, lid), None)
  | JmpLabelCond (f, label) ->
      let lid = check_prog_label prog_labels label in
      ([ TJmpLabelCond (f, lid) ], Some (false, lid), None)
  | JmpAddr r1 -> inst [ TJmpAddr r1 ]
  | JmpAddrCond (f, r1) -> inst [ TJmpAddrCond (f, r1) ]
  | JmpOffset imm ->
      let imm = check_signed_immediate imm in
      inst [ TJmpOffset imm ]
  | JmpOffsetCond (f, imm) ->
      let imm = check_signed_immediate imm in
      inst [ TJmpOffsetCond (f, imm) ]
  | JmpImmediate imm ->
      let imm = check_unsigned_immediate imm in
      inst [ TJmpImmediate imm ]
  | JmpImmediateCond (f, imm) ->
      let imm = check_unsigned_immediate imm in
      inst [ TJmpImmediateCond (f, imm) ]
  | Halt ->
      inst
        [
          (* We set [halt_reg] to 0xffff *)
          TLoadImmediateAdd (halt_reg, 0xffffl, false, R0);
          (* We add 0xffff0000 to [halt_reg] *)
          TLoadImmediateAdd (halt_reg, 0xffffl, true, halt_reg);
          (* We Jump to 0xffffffff (the value of [halt_reg]) *)
          TJmpAddr halt_reg;
        ]
  | CallAddr r -> inst [ TCallAddr r ]
  | CallLabel label ->
      let lid = check_prog_label prog_labels label in
      ([ TCallLabel lid ], Some (false, lid), None)
  | Ret ->
      inst
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

let pre_encode_instr data_sections f =
  let prog_sections, prog_labels = split_by_label f.text in
  let (prog_lbl_memory, used_prog_lbl, used_mem_lbl), prog_sections =
    List.fold_left_map
      (fun (pls_mem, pls, dls) (lid, insts) ->
        let pls_mem, pls, dls, tinsts =
          List.fold_left
            (fun (pls_mem, pls, dls, acc) i ->
              let a, pl, dl = process_instr data_sections prog_labels i in
              let acc = acc @ a in
              let pls_mem, pls =
                match pl with
                | None -> (pls_mem, pls)
                | Some (true, pl) -> (SSet.add pl pls_mem, SSet.add pl pls)
                | Some (false, pl) -> (pls_mem, SSet.add pl pls)
              in
              let dls =
                match dl with None -> dls | Some dl -> SSet.add dl dls
              in
              (pls_mem, pls, dls, acc))
            (pls_mem, pls, dls, []) insts
        in
        ((pls_mem, pls, dls), (lid, tinsts)))
      (SSet.empty, SSet.empty, SSet.empty)
      prog_sections
  in
  let unused_prog_label =
    SSet.filter (fun i -> SSet.mem i used_prog_lbl |> not) prog_labels
  in
  (if not (SSet.is_empty unused_prog_label) then
     let txt =
       Format.asprintf "Program labels %a are not used." pp_sset
         unused_prog_label
     in
     warning txt None
   else
     let unused_mem_label =
       SMap.filter
         (fun i _ -> SSet.mem i used_mem_lbl |> not)
         data_sections.mapping
     in
     if not (SMap.is_empty unused_mem_label) then
       let txt =
         Format.asprintf "Data labels %a are not used." pp_smap unused_mem_label
       in
       warning txt None);
  (prog_sections, prog_lbl_memory)
