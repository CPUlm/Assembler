open Ast
open TAst
open EncodeCommon
open ErrorUtils

let check_offset imm =
  if is_signed imm.v then imm.v
  else
    let txt =
      Format.sprintf
        "The value '%d' does not represent a valid 32-bit program address \
         offset."
        imm.v
    in
    type_error txt imm.pos

(* let check_address imm =
   if is_unsigned imm.v then
   else
     let txt =
       Format.sprintf
         "The value '%d' does not represent a valid 32-bit program address."
         imm.v
     in
     type_error txt (Some imm.pos)
*)
let return r = (r, None, None)

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

let check_prog_label labels label =
  match SMap.find_opt label.v labels with
  | Some (i, _) -> i
  | None ->
      let txt = Format.sprintf "The label '%s' is not defined." label.v in
      type_error txt label.pos

let process_load_label data_sections prog_labels l =
  match SMap.find_opt l.v data_sections.mapping with
  | Some i -> Either.left i
  | None -> check_prog_label prog_labels l |> Either.right

let compile_load_imm r1 imm r2 =
  match Int16.of_int32 imm with
  | Int16.Single imm -> [ TLoadImmediateAdd (r1, imm, LowHalf, r2) ]
  | Int16.Multiple i ->
      [
        TLoadImmediateAdd (r1, i.low, LowHalf, r2);
        TLoadImmediateAdd (r1, i.high, HighHalf, r1);
      ]

(** [process_instr data_sections prog_labels instr] : Check that the instruction
    is wellformed and return a siplified version of it, with the program label
    used if so, and the data label used if so.

    The boolean flag returned with the potentionally used program label, is here
    to mark if its associated address must be stored in memory. *)
let process_instr data_sections prog_labels instr =
  match instr.v with
  | Nop -> return [ TAnd (R0, R0, R0) ]
  | And (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TAnd (r1, r2, r3) ]
  | Or (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TOr (r1, r2, r3) ]
  | Nor (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TNor (r1, r2, r3) ]
  | Xor (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TXor (r1, r2, r3) ]
  | Not (r1, r2) ->
      check_writable_reg r1 instr;
      return [ TXor (r1, r2, R0) ]
  | Add (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TAdd (r1, r2, r3) ]
  | Sub (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TSub (r1, r2, r3) ]
  | Mul (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TMul (r1, r2, r3) ]
  | Div (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TDiv (r1, r2, r3) ]
  | Neg (r1, r2) ->
      check_writable_reg r1 instr;
      return [ TSub (r1, R0, r2) ]
  | Incr (r1, r2) ->
      check_writable_reg r1 instr;
      return [ TAdd (r1, r2, R1) ]
  | Decr (r1, r2) ->
      check_writable_reg r1 instr;
      return [ TSub (r1, r2, R1) ]
  | ShiftLeftLogical (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TShiftLeftLogical (r1, r2, r3) ]
  | ShiftRightArith (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TShiftRightArith (r1, r2, r3) ]
  | ShiftRightLogical (r1, r2, r3) ->
      check_writable_reg r1 instr;
      return [ TShiftRightLogical (r1, r2, r3) ]
  | Push r1 -> return [ TStore (SP, r1); TAdd (SP, SP, R1) ]
  | Pop r1 ->
      check_writable_reg r1 instr;
      return [ TLoad (r1, SP); TSub (SP, SP, R1) ]
  | Load (r1, r2) ->
      check_writable_reg r1 instr;
      return [ TLoad (r1, r2) ]
  | LoadImmediate (r1, imm) ->
      check_writable_reg r1 instr;
      let imm = check_immediate imm in
      (compile_load_imm r1 imm R0, None, None)
  | LoadImmediateAdd (r1, imm, r2) ->
      check_writable_reg r1 instr;
      let imm = check_immediate imm in
      (compile_load_imm r1 imm r2, None, None)
  | LoadImmediateLabel (r1, label) -> (
      check_writable_reg r1 instr;
      match process_load_label data_sections prog_labels label with
      | Left (lid, _) ->
          ([ TLoadDataLabelAdd (r1, lid, R0) ], None, Some label.v)
      | Right lid ->
          ([ TLoadProgLabelAdd (r1, lid, R0) ], Some (true, label.v, lid), None)
      )
  | LoadImmediateAddLabel (r1, label, r2) -> (
      check_writable_reg r1 instr;
      match process_load_label data_sections prog_labels label with
      | Left (lid, _) ->
          ([ TLoadDataLabelAdd (r1, lid, r2) ], None, Some label.v)
      | Right lid ->
          ([ TLoadProgLabelAdd (r1, lid, r2) ], Some (true, label.v, lid), None)
      )
  | Store (r1, r2) ->
      check_writable_reg r1 instr;
      return [ TStore (r1, r2) ]
  | Mov (r1, r2) ->
      check_writable_reg r1 instr;
      return [ TAdd (r1, r2, R0) ]
  | Test r1 -> return [ TAdd (R0, r1, R0) ]
  | JmpLabel label ->
      let lid = check_prog_label prog_labels label in
      ([ TJmpLabel lid ], Some (false, label.v, lid), None)
  | JmpLabelCond (f, label) ->
      let lid = check_prog_label prog_labels label in
      ([ TJmpLabelCond (f, lid) ], Some (false, label.v, lid), None)
  | JmpAddr r1 -> return [ TJmpAddr r1 ]
  | JmpAddrCond (f, r1) -> return [ TJmpAddrCond (f, r1) ]
  | JmpOffset imm ->
      let imm = check_offset imm in
      return [ TJmpOffset imm ]
  | JmpOffsetCond (f, imm) ->
      let imm = check_offset imm in
      return [ TJmpOffsetCond (f, imm) ]
  | JmpImmediate imm ->
      let imm = ProgramAddress.of_imm imm in
      return [ TJmpImmediate imm ]
  | JmpImmediateCond (f, imm) ->
      let imm = ProgramAddress.of_imm imm in
      return [ TJmpImmediateCond (f, imm) ]
  | Halt ->
      (* We set [halt_reg] to 0xffffffff *)
      let l = compile_load_imm halt_reg 0xffffffffl R0 in
      (* And we jump *)
      return (l @ [ TJmpAddr halt_reg ])
  | CallAddr r -> return [ TCallAddr r ]
  | CallLabel label ->
      let lid = check_prog_label prog_labels label in
      ([ TCallLabel lid ], Some (false, label.v, lid), None)
  | Ret ->
      return
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
  let prog_sections, prog_labels =
    split_by_label
      ( SMap.empty,
        (fun l -> ProgramLabel.fresh (Some l.v)),
        (fun l id -> SMap.add l.v (id, l.pos)),
        SMap.mem )
      f.text
  in
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
                | Some (true, pl, lid) ->
                    (ProgramLabel.Set.add lid pls_mem, SSet.add pl pls)
                | Some (false, pl, _) -> (pls_mem, SSet.add pl pls)
              in
              let dls =
                match dl with None -> dls | Some dl -> SSet.add dl dls
              in
              (pls_mem, pls, dls, acc))
            (pls_mem, pls, dls, []) insts
        in
        ((pls_mem, pls, dls), (lid, tinsts)))
      (ProgramLabel.Set.empty, SSet.empty, SSet.empty)
      prog_sections
  in
  let unused_prog_label =
    SMap.filter (fun i _ -> SSet.mem i used_prog_lbl |> not) prog_labels
  in
  let unused_mem_label =
    SMap.filter
      (fun i _ -> SSet.mem i used_mem_lbl |> not)
      data_sections.mapping
  in

  SMap.iter
    (fun label (_, pos) ->
      let txt = Format.asprintf "The program label %s is not used." label in
      warning txt pos)
    unused_prog_label;
  SMap.iter
    (fun label (_, pos) ->
      let txt = Format.asprintf "The data label %s is not used." label in
      warning txt pos)
    unused_mem_label;
  (prog_sections, prog_lbl_memory)
