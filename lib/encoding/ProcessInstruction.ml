open Ast
open Isa
open EncodingStruct
open TAst
open Integers
open Labels
open PositionUtils
open SplitFile
open ErrorUtils

let mk_pos_l p l = List.map (mk_pos p.pos) l
let return p r = (Monoid.of_elm (mk_pos p.pos r), None, None)
let return_l p l = (Monoid.of_list (mk_pos_l p l), None, None)

let check_readable_register r =
  match r.v with
  | PrivateReg ->
      warning "The register 'rpriv' should not be used here." r.pos;
      r.v
  | _ -> r.v

let check_writable_reg r =
  match r.v with
  | R0 | R1 | SP | FP | PrivateReg ->
      let txt =
        Format.sprintf "Writing to read-only register '%s'."
          (match r.v with
          | R0 -> "r0"
          | R1 -> "r1"
          | SP -> "sp"
          | FP -> "fp"
          | PrivateReg -> "rpriv"
          | _ -> failwith "Impossible case.")
      in
      warning txt r.pos;
      r.v
  | _ -> r.v

let check_prog_label labels label =
  match SMap.find_opt label.v labels with
  | Some i -> i
  | None ->
      let txt =
        Format.sprintf "The program label '%s' is not defined." label.v
      in
      error txt label.pos

let process_load_label data_sections prog_labels l =
  match SMap.find_opt l.v data_sections.data_label_mapping with
  | Some label -> Either.left label
  | None -> check_prog_label prog_labels l |> Either.right

let compile_load instr r1 int16 r2 =
  let r2 = match r2 with Some r -> r | None -> R0 in
  match int16 with
  | UInt16.Single imm ->
      let i = TLoadImmediateAdd (r1, imm, LowHalf, r2) in
      Monoid.of_elm (mk_pos instr.pos i)
  | UInt16.Multiple i ->
      let l =
        [
          TLoadImmediateAdd (r1, i.low, LowHalf, r2);
          TLoadImmediateAdd (r1, i.high, HighHalf, r1);
        ]
      in
      Monoid.of_list (List.map (mk_pos instr.pos) l)

(** [process_instr data_sections prog_labels instr] : Check that the instruction
       is wellformed and return a siplified version of it, with the program label
       used if so, and the data label used if so.

       The boolean flag returned with the potentionally used program label, is here
       to mark if its associated address must be stored in memory. *)
let process_instr data_sections prog_labels instr =
  match instr.v with
  | AstNop -> return instr (TAnd (R0, R0, R0))
  | AstAnd (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TAnd (r1, r2, r3))
  | AstOr (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TOr (r1, r2, r3))
  | AstNor (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TNor (r1, r2, r3))
  | AstXor (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TXor (r1, r2, r3))
  | AstNot (r1, r2) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      return instr (TXor (r1, r2, R0))
  | AstAdd (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TAdd (r1, r2, r3))
  | AstSub (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TSub (r1, r2, r3))
  | AstMul (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TMul (r1, r2, r3))
  | AstDiv (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TDiv (r1, r2, r3))
  | AstNeg (r1, r2) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      return instr (TSub (r1, R0, r2))
  | AstIncr (r1, r2) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      return instr (TAdd (r1, r2, R1))
  | AstDecr (r1, r2) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      return instr (TSub (r1, r2, R1))
  | AstShiftLeftLogical (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TShiftLeftLogical (r1, r2, r3))
  | AstShiftRightArith (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TShiftRightArith (r1, r2, r3))
  | AstShiftRightLogical (r1, r2, r3) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      let r3 = check_readable_register r3 in
      return instr (TShiftRightLogical (r1, r2, r3))
  | AstPush r1 ->
      let r1 = check_readable_register r1 in
      return_l instr [ TStore (SP, r1); TAdd (SP, SP, R1) ]
  | AstPop r1 ->
      let r1 = check_writable_reg r1 in
      return_l instr [ TLoad (r1, SP); TSub (SP, SP, R1) ]
  | AstLoad (r1, r2) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      return instr (TLoad (r1, r2))
  | AstLoadImmediateAdd (r1, imm, r2) ->
      let r1 = check_writable_reg r1 in
      let r2 =
        match r2 with
        | Some r -> Some (check_readable_register r)
        | None -> None
      in
      let imm = Immediate.to_uint16 imm.v in
      (compile_load instr r1 imm r2, None, None)
  | AstLoadImmediateAddLabel (r1, label, r2) -> (
      let r1 = check_writable_reg r1 in
      let r2 =
        match r2 with
        | Some r -> Some (check_readable_register r)
        | None -> None
      in
      match process_load_label data_sections prog_labels label with
      | Left data_lid ->
          let addr =
            DataLabel.Map.find data_lid data_sections.data_label_position
          in
          let l = compile_load instr r1 (MemoryAddress.to_uint16 addr) r2 in
          (l, None, Some data_lid)
      | Right lid ->
          let i = TLoadProgLabelAdd (r1, lid, r2) in
          (Monoid.of_elm (mk_pos instr.pos i), Some lid, None))
  | AstStore (r1, r2) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      return instr (TStore (r1, r2))
  | AstMov (r1, r2) ->
      let r1 = check_writable_reg r1 in
      let r2 = check_readable_register r2 in
      return instr (TAdd (r1, r2, R0))
  | AstTest r1 ->
      let r1 = check_readable_register r1 in
      return instr (TAdd (R0, r1, R0))
  | AstJmpLabel (f, label) ->
      let lid = check_prog_label prog_labels label in
      let m = Monoid.of_elm (mk_pos instr.pos (TJmpLabel (f, lid))) in
      (m, Some lid, None)
  | AstJmpAddr (f, r1) ->
      let r1 = check_readable_register r1 in
      return instr (TJmpAddr (f, r1))
  | AstJmpOffset (f, imm) -> return instr (TJmpOffset (f, imm))
  | AstJmpImmediate (f, imm) -> return instr (TJmpImmediate (f, imm))
  | AstHalt ->
      (* We set [PrivateReg] to the last address *)
      let l =
        compile_load instr PrivateReg ProgramAddress.(to_uint16 last) None
      in
      (* And we jump *)
      let j = Monoid.of_elm (mk_pos instr.pos (TJmpAddr (None, PrivateReg))) in
      (Monoid.(l @@ j), None, None)
  | AstCallAddr r1 ->
      let r1 = check_readable_register r1 in
      return instr (TCallAddr r1)
  | AstCallLabel label ->
      let lid = check_prog_label prog_labels label in
      let m = Monoid.of_elm (mk_pos instr.pos (TCallLabel lid)) in
      (m, Some lid, None)
  | AstRet ->
      return_l instr
        [
          (* We move SP to FP. *)
          TAdd (SP, FP, R0);
          (* We read the value pointed by SP (old FP) into FP. *)
          TLoad (FP, SP);
          (* We let SP point to the return address. *)
          TSub (SP, SP, R1);
          (* We read the value pointed by SP (ret address) into [PrivateReg]. *)
          TLoad (PrivateReg, SP);
          (* We restore SP to the original state before the call. *)
          TSub (SP, SP, R1);
          (* We jump to the return address. *)
          TJmpAddr (None, PrivateReg);
        ]

let pre_encode_instr data_sections f =
  let prog_sections, prog_label_mapping =
    split_by_label
      (fun l ->
        match SMap.find_opt l.v data_sections.data_label_mapping with
        | None -> ProgramLabel.fresh l.v l.pos
        | Some lbl ->
            let txt =
              Format.asprintf
                "The label '%s' has already been declared as a data label here \
                 : %a"
                l.v pp_pos (DataLabel.position lbl)
            in
            error txt l.pos)
      f.text
  in
  match SMap.find_opt "main" prog_label_mapping with
  | None ->
      file_warning "No label 'main' found, skipping all instructions.";
      { prog_sections = Monoid.empty; prog_label_mapping = SMap.empty }
  | Some mainid ->
      let used_prog_lbl, used_mem_lbl, prog_sections =
        Monoid.fold_left
          (fun (pls, dls, acc) instr_sec ->
            let label, insts = instr_sec.v in
            let pls, dls, body =
              Monoid.fold_left
                (fun (pls, dls, acc) i ->
                  let a, pl, dl =
                    process_instr data_sections prog_label_mapping i
                  in
                  let acc = Monoid.(acc @@ a) in
                  let pls =
                    match pl with
                    | None -> pls
                    | Some pl -> ProgramLabel.Set.add pl pls
                  in
                  let dls =
                    match dl with
                    | None -> dls
                    | Some dl -> DataLabel.Set.add dl dls
                  in
                  (pls, dls, acc))
                (pls, dls, Monoid.empty) insts
            in
            let new_sec = { label; body; pos = instr_sec.pos } in
            (pls, dls, Monoid.(acc @@ of_elm new_sec)))
          (ProgramLabel.Set.empty, DataLabel.Set.empty, Monoid.empty)
          prog_sections
      in
      (* Mark main label as used to avoid useless warnings about it. It it always
         used implictly as the entry point of the program. *)
      let used_prog_lbl = ProgramLabel.Set.add mainid used_prog_lbl in
      let unused_prog_label =
        SMap.filter
          (fun _ lid -> ProgramLabel.Set.mem lid used_prog_lbl |> not)
          prog_label_mapping
      in
      let unused_mem_label =
        SMap.filter
          (fun _ lid -> DataLabel.Set.mem lid used_mem_lbl |> not)
          data_sections.data_label_mapping
      in
      SMap.iter
        (fun label lbl_id ->
          let txt = Format.asprintf "The program label %s is not used." label in
          warning txt (ProgramLabel.position lbl_id))
        unused_prog_label;
      SMap.iter
        (fun label lbl_id ->
          let txt = Format.asprintf "The data label %s is not used." label in
          warning txt (DataLabel.position lbl_id))
        unused_mem_label;
      { prog_sections; prog_label_mapping }
