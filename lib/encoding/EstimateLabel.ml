open TAst

let max_section_size start_addr sec_instrs pos =
  Monoid.fold
    (fun curr_size (i : tinstr) ->
      match get_instr i with
      (* Logical Operations *)
      | TAnd _ -> ProgramAddress.next curr_size pos i
      | TOr _ -> ProgramAddress.next curr_size pos i
      | TNor _ -> ProgramAddress.next curr_size pos i
      | TXor _ -> ProgramAddress.next curr_size pos i
      (* Arithmetic operation *)
      | TAdd _ -> ProgramAddress.next curr_size pos i
      | TSub _ -> ProgramAddress.next curr_size pos i
      | TMul _ -> ProgramAddress.next curr_size pos i
      | TDiv _ -> ProgramAddress.next curr_size pos i
      (* Shifts operations *)
      | TShiftLeftLogical _ -> ProgramAddress.next curr_size pos i
      | TShiftRightArith _ -> ProgramAddress.next curr_size pos i
      | TShiftRightLogical _ -> ProgramAddress.next curr_size pos i
      (* Memory operations *)
      | TLoad _ -> ProgramAddress.next curr_size pos i
      | TLoadImmediateAdd _ -> ProgramAddress.next curr_size pos i
      | TLoadProgLabelAdd _ -> ProgramAddress.next curr_size pos i
      | TStore _ -> ProgramAddress.next curr_size pos i
      (* Flow instructions *)
      | TJmpAddr _ -> ProgramAddress.next curr_size pos i
      | TJmpAddrCond _ -> ProgramAddress.next curr_size pos i
      | TJmpOffset _ -> assert false
      | TJmpOffsetCond _ -> assert false
      | TJmpImmediate _ -> assert false
      | TJmpImmediateCond _ -> assert false
      (* Function Call *)
      | TCallAddr _ -> assert false
      | TCallLabel _ -> assert false)
    start_addr sec_instrs
