open Ast
open TAst
open Integers
open PositionUtils
open EncodeCommon
open ErrorUtils

let mk_pos_l p l = List.map (mk_pos p.pos) l
let return p r = (Monoid.of_elm (mk_pos p.pos r), None, None)
let return_l p l = (Monoid.of_list (mk_pos_l p l), None, None)

let check_readable_register r p =
  match r with
  | PrivateReg ->
      warning "The register 'rpriv' should not be used here." p.pos;
      r
  | _ -> r

let check_writable_reg r p =
  match r with
  | R0 | R1 | SP | FP | PrivateReg ->
      let txt =
        Format.sprintf "Writing to read-only register '%s'."
          (match r with
          | R0 -> "r0"
          | R1 -> "r1"
          | SP -> "sp"
          | FP -> "fp"
          | PrivateReg -> "rpriv"
          | _ -> failwith "Impossible case.")
      in
      warning txt p.pos;
      r
  | _ -> r

let check_prog_label labels label =
  match SMap.find_opt label.v labels with
  | Some (i, _) -> i
  | None ->
      let txt = Format.sprintf "The label '%s' is not defined." label.v in
      error txt label.pos

let process_load_label (data_sections : data_section) prog_labels l =
  match SMap.find_opt l.v data_sections.mapping with
  | Some i -> Either.left i
  | None -> check_prog_label prog_labels l |> Either.right

let compile_load instr r1 int16 r2 =
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
let process_instr data_sections
    (prog_labels : (ProgramLabel.t * position) SMap.t) instr =
  match instr.v with
  | Nop -> return instr (TAnd (R0, R0, R0))
  | And (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TAnd (r1, r2, r3))
  | Or (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TOr (r1, r2, r3))
  | Nor (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TNor (r1, r2, r3))
  | Xor (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TXor (r1, r2, r3))
  | Not (r1, r2) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      return instr (TXor (r1, r2, R0))
  | Add (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TAdd (r1, r2, r3))
  | Sub (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TSub (r1, r2, r3))
  | Mul (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TMul (r1, r2, r3))
  | Div (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TDiv (r1, r2, r3))
  | Neg (r1, r2) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      return instr (TSub (r1, R0, r2))
  | Incr (r1, r2) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      return instr (TAdd (r1, r2, R1))
  | Decr (r1, r2) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      return instr (TSub (r1, r2, R1))
  | ShiftLeftLogical (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TShiftLeftLogical (r1, r2, r3))
  | ShiftRightArith (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TShiftRightArith (r1, r2, r3))
  | ShiftRightLogical (r1, r2, r3) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let r3 = check_readable_register r3 instr in
      return instr (TShiftRightLogical (r1, r2, r3))
  | Push r1 ->
      let r1 = check_readable_register r1 instr in
      return_l instr [ TStore (SP, r1); TAdd (SP, SP, R1) ]
  | Pop r1 ->
      let r1 = check_writable_reg r1 instr in
      return_l instr [ TLoad (r1, SP); TSub (SP, SP, R1) ]
  | Load (r1, r2) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      return instr (TLoad (r1, r2))
  | LoadImmediate (r1, imm) ->
      let r1 = check_writable_reg r1 instr in
      let imm = Immediate.to_uint16 imm in
      (compile_load instr r1 imm R0, None, None)
  | LoadImmediateAdd (r1, imm, r2) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      let imm = Immediate.to_uint16 imm in
      (compile_load instr r1 imm r2, None, None)
  | LoadImmediateLabel (r1, label) -> (
      let r1 = check_writable_reg r1 instr in
      match process_load_label data_sections prog_labels label with
      | Left (lid, _) ->
          let l = compile_load instr r1 (MemoryAddress.to_uint16 lid) R0 in
          (l, None, Some lid)
      | Right lid ->
          let i = TLoadProgLabelAdd (r1, lid, R0) in
          (Monoid.of_elm (mk_pos instr.pos i), Some lid, None))
  | LoadImmediateAddLabel (r1, label, r2) -> (
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      match process_load_label data_sections prog_labels label with
      | Left (lid, _) ->
          let l = compile_load instr r1 (MemoryAddress.to_uint16 lid) r2 in
          (l, None, Some lid)
      | Right lid ->
          let i = TLoadProgLabelAdd (r1, lid, r2) in
          (Monoid.of_elm (mk_pos instr.pos i), Some lid, None))
  | Store (r1, r2) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      return instr (TStore (r1, r2))
  | Mov (r1, r2) ->
      let r1 = check_writable_reg r1 instr in
      let r2 = check_readable_register r2 instr in
      return instr (TAdd (r1, r2, R0))
  | Test r1 ->
      let r1 = check_readable_register r1 instr in
      return instr (TAdd (R0, r1, R0))
  | JmpLabel label ->
      let lid = check_prog_label prog_labels label in
      let l =
        [ TLoadProgLabelAdd (PrivateReg, lid, R0); TJmpAddr PrivateReg ]
      in
      (Monoid.of_list (mk_pos_l instr l), Some lid, None)
  | JmpLabelCond (f, label) ->
      let lid = check_prog_label prog_labels label in
      let l =
        [
          TLoadProgLabelAdd (PrivateReg, lid, R0); TJmpAddrCond (f, PrivateReg);
        ]
      in
      (Monoid.of_list (mk_pos_l instr l), Some lid, None)
  | JmpAddr r1 ->
      let r1 = check_readable_register r1 instr in
      return instr (TJmpAddr r1)
  | JmpAddrCond (f, r1) ->
      let r1 = check_readable_register r1 instr in
      return instr (TJmpAddrCond (f, r1))
  | JmpOffset imm -> return instr (TJmpOffset imm)
  | JmpOffsetCond (f, imm) -> return instr (TJmpOffsetCond (f, imm))
  | JmpImmediate imm -> return instr (TJmpImmediate imm)
  | JmpImmediateCond (f, imm) -> return instr (TJmpImmediateCond (f, imm))
  | Halt ->
      (* We set [PrivateReg] to 0xffffffff *)
      let l = compile_load instr PrivateReg (UInt16.of_int32 0xffffffffl) R0 in
      (* And we jump *)
      let j = Monoid.of_elm (mk_pos instr.pos (TJmpAddr PrivateReg)) in
      (Monoid.(l @@ j), None, None)
  | CallAddr r1 ->
      let r1 = check_readable_register r1 instr in
      return instr (TCallAddr r1)
  | CallLabel label ->
      let lid = check_prog_label prog_labels label in
      let m = Monoid.of_elm (mk_pos instr.pos (TCallLabel lid)) in
      (m, Some lid, None)
  | Ret ->
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
          TJmpAddr PrivateReg;
        ]

let pre_encode_instr (data_sections : data_section) f =
  let prog_sections, prog_labels =
    split_by_label
      ( SMap.empty,
        (fun l ->
          match SMap.find_opt l.v data_sections.mapping with
          | None -> ProgramLabel.fresh (Some l.v)
          | Some (_, decl_pos) ->
              let txt =
                Format.asprintf
                  "The label '%s' has already been declared as a data label \
                   here : %a"
                  l.v pp_pos decl_pos
              in
              error txt l.pos),
        (fun l id -> SMap.add l.v (id, l.pos)),
        SMap.mem )
      f.text
  in
  match SMap.find_opt "main" prog_labels with
  | None ->
      file_warning "No label 'main' found, skipping all instructions.";
      []
  | Some (mainid, _) ->
      let (used_prog_lbl, used_mem_lbl), prog_sections =
        List.fold_left_map
          (fun (pls, dls) (lid, insts) ->
            let pls, dls, tinsts =
              List.fold_left
                (fun (pls, dls, acc) i ->
                  let a, pl, dl = process_instr data_sections prog_labels i in
                  let acc = Monoid.(acc @@ a) in
                  let pls =
                    match pl with
                    | None -> pls
                    | Some pl -> ProgramLabel.Set.add pl pls
                  in
                  let dls =
                    match dl with
                    | None -> dls
                    | Some dl -> MemoryAddress.Set.add dl dls
                  in
                  (pls, dls, acc))
                (pls, dls, Monoid.empty) insts
            in
            ((pls, dls), (lid, tinsts)))
          (ProgramLabel.Set.empty, MemoryAddress.Set.empty)
          prog_sections
      in
      (* Mark main label as used to avoid useless warnings about it. It it always
         used implictly as the entry point of the program. *)
      let used_prog_lbl = ProgramLabel.Set.add mainid used_prog_lbl in
      let unused_prog_label =
        SMap.filter
          (fun _ (lid, _) -> ProgramLabel.Set.mem lid used_prog_lbl |> not)
          prog_labels
      in
      let unused_mem_label =
        SMap.filter
          (fun _ (lid, _) -> MemoryAddress.Set.mem lid used_mem_lbl |> not)
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
      prog_sections
