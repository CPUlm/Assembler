open TAst
open PAst
open Integers

let add_instr_p pos (addr, l) v =
  (ProgramAddress.next addr pos (assert false), l @ [ (addr, v) ])

let encode_load curr_addr pos instr r1 (i : UInt16.res) r2 =
  match i with
  | Single imm ->
      let p1 = curr_addr in
      let l = [ (p1, PLoadImmediateAdd (r1, imm, LowHalf, r2)) ] in
      let next_addr = ProgramAddress.next p1 pos instr in
      (next_addr, l)
  | Multiple imms ->
      let p1 = curr_addr in
      let p2 = ProgramAddress.next p1 pos instr in
      let l =
        [
          (p1, PLoadImmediateAdd (r1, imms.low, LowHalf, r2));
          (p2, PLoadImmediateAdd (r1, imms.high, HighHalf, r1));
        ]
      in
      let next_addr = ProgramAddress.next p2 pos instr in
      (next_addr, l)

let compile_jump accc pos instr target_addr =
  let curr_addr, acc, a2c = accc in
  let next_addr, l =
    encode_load curr_addr pos instr PrivateReg
      (ProgramAddress.to_uint16 target_addr)
      R0
  in
  let l = l @ [ (next_addr, PJmpAddr PrivateReg) ] in
  let next_addr = ProgramAddress.next next_addr pos in
  (next_addr, acc @ l, ProgramAddress.Map.add target_addr instr.pos a2c)

let compile_jump_cond accc pos instr f target_addr =
  let curr_addr, acc, a2c = accc in
  let next_addr, l =
    encode_load curr_addr pos instr PrivateReg
      (ProgramAddress.to_uint16 target_addr)
      R0
  in
  let l = l @ [ (next_addr, PJmpAddrCond (f, PrivateReg)) ] in
  let next_addr = ProgramAddress.next next_addr pos in
  (next_addr, acc @ l, ProgramAddress.Map.add target_addr instr.pos a2c)

let setup_stack curr_addr pos (instr : tinstr) nb_op =
  let ret_addr =
    let ofs = nb_op + 5 (* nb of op without load to setup the stack *) in
    (* let ofs =
         if ProgramAddress.fit_16bit curr_addr (ofs, instr.pos) then
           ofs + 1 (* only one load needed *)
         else ofs + 2 (* two load here *)
       in *)
    ProgramAddress.with_offset curr_addr (ofs, instr.pos)
  in
  (* We load [ret_addr] into [PrivateReg] *)
  let na, l =
    encode_load curr_addr pos instr PrivateReg
      (ProgramAddress.to_uint16 ret_addr)
      R0
  in
  (* And push it to the stack *)
  let na, l = add_instr_p pos (na, l) (PStore (SP, PrivateReg)) in
  (* Update the stack pointer *)
  let na, l = add_instr_p pos (na, l) (PAdd (SP, SP, R1)) in
  (* We add the current FP to the stack *)
  let na, l = add_instr_p pos (na, l) (PStore (SP, FP)) in
  (* Copy SP into FP *)
  let na, l = add_instr_p pos (na, l) (PAdd (FP, SP, R0)) in
  (* Update the stack pointer *)
  let na, l = add_instr_p pos (na, l) (PAdd (SP, SP, R1)) in
  (na, l)

let localise_section begin_addr instrs pos =
  let add_instr = add_instr_p pos in
  let incr_and_ret accc v =
    let curr_addr, acc, a2c = accc in
    let na, l = add_instr (curr_addr, acc) v in
    (na, l, a2c)
  in
  Monoid.fold
    (fun ((curr_addr, acc, a2c) as accc) (instr : tinstr) ->
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
          let na, l =
            add_instr (curr_addr, acc) (PLoadProgLabelAdd (r1, prg_lbl, r2))
          in
          (* We reserve one operation for the load *)
          let na = ProgramAddress.next na pos instr in
          (na, l, a2c)
      | TStore (r1, r2) -> incr_and_ret accc (PStore (r1, r2))
      | TJmpAddr r1 -> incr_and_ret accc (PJmpAddr r1)
      | TJmpAddrCond (f, r1) -> incr_and_ret accc (PJmpAddrCond (f, r1))
      | TJmpOffset offset ->
          let target_addr = ProgramAddress.with_offset curr_addr offset in
          (* compile_jump accc pos instr target_addr *)
          ignore target_addr;
          assert false
      | TJmpOffsetCond (f, offset) ->
          let target_addr = ProgramAddress.with_offset curr_addr offset in
          (* compile_jump_cond accc pos (assert false) f target_addr (assert false) *)
          ignore (f, target_addr);
          assert false
      | TJmpImmediate target_addr ->
          ignore target_addr;
          assert false (* compile_jump accc pos target_addr (assert false) *)
      | TJmpImmediateCond (f, target_addr) ->
          ignore (f, target_addr);
          assert false
          (* compile_jump_cond accc pos f target_addr (assert false) *)
      | TCallAddr r ->
          let na, l = setup_stack curr_addr pos instr 1 in
          let na, l = add_instr (na, l) (PJmpAddr r) in
          (na, l, a2c)
      | TCallLabel lbl ->
          let na, l = setup_stack curr_addr pos instr 3 in
          (* We load the value of the label in memory : 2 operations *)
          let na, l =
            add_instr (na, l) (PLoadProgLabelAdd (PrivateReg, lbl, R0))
          in
          (* We reserve one operation for the load *)
          let na = ProgramAddress.next na pos instr in
          (* And now, we (finally) jump ! *)
          let na, l = add_instr (na, l) (PJmpAddr PrivateReg) in
          (na, l, a2c))
    (begin_addr, [], ProgramAddress.Map.empty)
    instrs
