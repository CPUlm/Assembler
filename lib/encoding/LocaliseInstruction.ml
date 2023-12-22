open Ast
open TAst
open PAst
open ErrorUtils
open PositionUtils
open Integers

let next addr =
  (* We can force because we have a majoration *)
  Option.get (ProgramAddress.next addr)

let add_instr (cur_addr, acc) v = (next cur_addr, Monoid.(acc @@ of_elm v))
let reserve_address (cur_addr, acc) = (next cur_addr, acc)
let current_address = fst

(** Load the program label [label] plus the register [r2] to [r1].
    [acc] is the current position in the section. *)
let load_label (label_estim, label_positioned) acc r1 label r2 =
  let r2 = match r2 with Some r -> r | None -> R0 in
  let label_in_one_load =
    match ProgramLabel.Map.find_opt label label_positioned with
    | Some pos ->
        (* The label occurs before our label so we have its final position !
           We test if this address can be encoded in a uint16 to test if one
           load if enougt *)
        ProgramAddress.fit_in_uint16 pos 0
    | None ->
        (* The label occurs after our label, so we look at his position
           estimation. Same as before *)
        let estim_pos = ProgramLabel.Map.find label label_estim in
        ProgramAddress.fit_in_uint16 estim_pos 0
  in
  let acc = add_instr acc (PLoadProgLabelAdd (r1, label, r2)) in
  if label_in_one_load then acc else reserve_address acc

(** Load the address [addr] in the register [r1]. If [r2] is not [None], it is
    added to the result *)
let load_address acc r1 addr r2 =
  let r2 = match r2 with Some r -> r | None -> R0 in
  match ProgramAddress.to_uint16 addr with
  | Single imm -> add_instr acc (PLoadImmediateAdd (r1, imm, LowHalf, r2))
  | Multiple imms ->
      let acc = add_instr acc (PLoadImmediateAdd (r1, imms.low, LowHalf, r2)) in
      add_instr acc (PLoadImmediateAdd (r1, imms.high, HighHalf, r1))

(** Jump to the offset [ofs]. Jump is performed every time if [f] is [None]
    Otherwise, [f] is the flag used to known if we jump. *)
let jump_offset acc ofs f =
  (* This is the absolute address we will jump at *)
  let target_addr = ProgramAddress.with_offset (current_address acc) ofs in
  match Offset.to_int24 ofs with
  | Some ofs -> (
      (* We can use the jmpi instruction directly :)
         No need to used the absolute address. *)
      match f with
      | None -> (add_instr acc (PJmpImmediate ofs), target_addr)
      | Some f -> (add_instr acc (PJmpImmediateCond (f, ofs)), target_addr))
  | None ->
      (* We can't use jmpi :/ *)
      (* We load the target_address into rpriv *)
      let acc = load_address acc PrivateReg target_addr None in
      (* And jump to rpriv *)
      (add_instr acc (PJmpAddr PrivateReg), target_addr)

let setup_stack acc nb_op =
  let ret_addr =
    let ofs =
      (* nb of op needed to setup the stack without the load of the return address *)
      nb_op + 5
    in
    let ofs =
      if ProgramAddress.fit_in_uint16 (current_address acc) ofs then
        (* only one load needed for the return address *)
        Offset.of_int (ofs + 1)
      else (* two load needed for the return address *) Offset.of_int (ofs + 2)
    in
    (* This *should* be safe, if nb_op is not 100000 *)
    let ofs = Option.get ofs in
    ProgramAddress.with_offset (current_address acc) ofs
  in
  (* We load [ret_addr] into [PrivateReg] *)
  let acc = load_address acc PrivateReg ret_addr None in
  (* And push it to the stack *)
  let acc = add_instr acc (PStore (SP, PrivateReg)) in
  (* Update the stack pointer *)
  let acc = add_instr acc (PAdd (SP, SP, R1)) in
  (* We add the current FP to the stack *)
  let acc = add_instr acc (PStore (SP, FP)) in
  (* Copy SP into FP *)
  let acc = add_instr acc (PAdd (FP, SP, R0)) in
  (* Update the stack pointer *)
  let acc = add_instr acc (PAdd (SP, SP, R1)) in
  acc

let localise_section labels_pos begin_addr sec =
  let incr_and_ret accc v =
    let acc, a2c = accc in
    let acc = add_instr acc v in
    (acc, a2c)
  in
  Monoid.fold_left
    (fun ((acc, a2c) as accc) instr ->
      match instr.v with
      | TAnd (r1, r2, r3) -> incr_and_ret accc (PAnd (r1, r2, r3))
      | TOr (r1, r2, r3) -> incr_and_ret accc (POr (r1, r2, r3))
      | TNor (r1, r2, r3) -> incr_and_ret accc (PNor (r1, r2, r3))
      | TXor (r1, r2, r3) -> incr_and_ret accc (PXor (r1, r2, r3))
      | TAdd (r1, r2, r3) -> incr_and_ret accc (PAdd (r1, r2, r3))
      | TSub (r1, r2, r3) -> incr_and_ret accc (PSub (r1, r2, r3))
      | TMul (r1, r2, r3) -> incr_and_ret accc (PMul (r1, r2, r3))
      | TDiv (r1, r2, r3) -> incr_and_ret accc (PDiv (r1, r2, r3))
      | TShiftLeftLogical (r1, r2, r3) ->
          incr_and_ret accc (PShiftLeftLogical (r1, r2, r3))
      | TShiftRightArith (r1, r2, r3) ->
          incr_and_ret accc (PShiftRightArith (r1, r2, r3))
      | TShiftRightLogical (r1, r2, r3) ->
          incr_and_ret accc (PShiftRightLogical (r1, r2, r3))
      | TLoad (r1, r2) -> incr_and_ret accc (PLoad (r1, r2))
      | TLoadImmediateAdd (r1, imm, mode, r2) ->
          incr_and_ret accc (PLoadImmediateAdd (r1, imm, mode, r2))
      | TLoadProgLabelAdd (r1, prg_lbl, r2) ->
          let acc = load_label labels_pos acc r1 prg_lbl r2 in
          (acc, a2c)
      | TStore (r1, r2) -> incr_and_ret accc (PStore (r1, r2))
      | TJmpAddr (None, r1) -> incr_and_ret accc (PJmpAddr r1)
      | TJmpAddr (Some f, r1) -> incr_and_ret accc (PJmpAddrCond (f, r1))
      | TJmpOffset (f, offset) ->
          (if not (ProgramAddress.well_defined (current_address acc) offset.v)
           then
             let txt =
               Format.asprintf
                 "The address to which the program jumps, calculated from the  \
                  offset '%a' of the current address, is not well defined. In \
                  fact, this address is outside the permitted range (from %a \
                  to %a). The resulting address is implementation defined, \
                  please fix this."
                 Offset.pp offset.v ProgramAddress.pp ProgramAddress.first
                 ProgramAddress.pp ProgramAddress.last
             in
             warning txt offset.pos);
          let acc, target_addr = jump_offset acc offset.v f in
          (acc, ProgramAddress.Map.add target_addr offset.pos a2c)
      | TJmpImmediate (f, target_addr) ->
          let offset =
            ProgramAddress.diff (current_address acc) target_addr.v
          in
          let acc, _ = jump_offset acc offset f in
          (acc, ProgramAddress.Map.add target_addr.v target_addr.pos a2c)
      | TCallAddr reg ->
          let acc = setup_stack acc 1 in
          let acc = add_instr acc (PJmpAddr reg) in
          (acc, a2c)
      | TCallLabel _ -> (
          (* let offset = ProgramAddress.diff (current_address acc) label in
          match Offset.to_int24 with
          | Some i ->
              (* The jump will be a jumpi ! *)
              let acc = setup_stack acc 1 in
              let acc = add_instr acc (PJmpFutureOffset i) in
              (acc, a2c)
          | None -> *)
              (* We need to load the address in rpriv to jump *)
              assert false))
    ((begin_addr, Monoid.empty), ProgramAddress.Map.empty)
    sec.body
