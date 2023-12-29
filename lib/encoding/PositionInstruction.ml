open TAst
open Isa
open PAst
open ErrorUtils
open Labels
open PositionUtils
open Integers

(** Apply the function f [times] times on arg. *)
let rec repeat f arg times =
  if times = 0 then arg else repeat f (f arg) (times - 1)

let next addr =
  (* We can force because we have a majoration *)
  Option.get (ProgramAddress.next addr)

let add_instr (cur_addr, acc) v =
  (next cur_addr, Monoid.(acc @@ of_elm (cur_addr, Either.Left v)))

let add_future (cur_addr, acc) res v =
  if res <= 0 then
    raise (Invalid_argument "The number of operation reserved must be > 0")
  else
    let next_addr = repeat next cur_addr res in
    ( next_addr
    , Monoid.(acc @@ of_elm (cur_addr, Either.Right {res_space= res; op= v})) )

let reserve_address (cur_addr, acc) = (next cur_addr, acc)

let current_address = fst

let computed_address (_, label_positioned) label =
  ProgramLabel.Map.find_opt label label_positioned

let estimated_address (label_estim, _) label =
  ProgramLabel.Map.find label label_estim

(** Load the program label [label] plus the register [r2] to [r1].
    [acc] is the current position in the section. *)
let load_label labels_pos acc r1 label r2 =
  let r2 = match r2 with Some r -> r | None -> R0 in
  let label_in_one_load =
    match computed_address labels_pos label with
    | Some pos ->
        (* The label occurs before our label so we have its final position !
           We test if this address can be encoded in a uint16 to test if one
           load if enough *)
        ProgramAddress.fit_in_uint16 pos 0
    | None ->
        (* The label occurs after our label, so we look at his position
           estimation. Same as before *)
        let estim_pos = estimated_address labels_pos label in
        ProgramAddress.fit_in_uint16 estim_pos 0
  in
  let res_space = if label_in_one_load then 1 else 2 in
  let acc = add_future acc res_space (FuturePLabelLoad (r1, label, r2)) in
  if label_in_one_load then acc else reserve_address acc

(** Load the address [addr] in the register [r1]. If [r2] is not [None], it is
    added to the result *)
let load_address acc r1 addr r2 =
  let r2 = match r2 with Some r -> r | None -> R0 in
  match ProgramAddress.to_uint16 addr with
  | Single imm ->
      add_instr acc (LoadImmediateAdd (r1, r2, imm, LowHalf))
  | Multiple imms ->
      let acc = add_instr acc (LoadImmediateAdd (r1, r2, imms.low, LowHalf)) in
      add_instr acc (LoadImmediateAdd (r1, r1, imms.high, HighHalf))

(** Jump to the offset [ofs]. Jump is performed every time if [f] is [None]
    Otherwise, [f] is the flag used to known if we jump. *)
let jump_offset acc ofs f pos =
  let cur_addr = current_address acc in
  (* This is the absolute address we will jump at *)
  let target_addr =
    match ProgramAddress.with_offset cur_addr ofs with
    | Some addr ->
        addr
    | None ->
        let txt =
          Format.asprintf
            "The address %a shifted by the offset %a is not a well-defined \
             address. This result does not fall in the valid address range: \
             [%a; %a]"
            ProgramAddress.pp cur_addr Offset.pp ofs ProgramAddress.pp
            ProgramAddress.first ProgramAddress.pp ProgramAddress.last
        in
        error txt pos
  in
  match Offset.to_int24 ofs with
  | Some ofs -> (
    (* We can use the jmpi instruction directly :)
       No need to used the absolute address. *)
    match f with
    | None ->
        (add_instr acc (JmpImmediate ofs), target_addr)
    | Some f ->
        (add_instr acc (JmpImmediateCond (ofs, f)), target_addr) )
  | None ->
      (* We can't use jmpi :/ *)
      (* We load the target_address into rpriv *)
      let acc = load_address acc PrivateReg target_addr None in
      (* And jump to rpriv *)
      (add_instr acc (Jmp PrivateReg), target_addr)

(** Setup the stack for an function call *)
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
    Option.get (ProgramAddress.with_offset (current_address acc) ofs)
  in
  (* We load [ret_addr] into [PrivateReg] *)
  let acc = load_address acc PrivateReg ret_addr None in
  (* And push it to the stack *)
  let acc = add_instr acc (Store (SP, PrivateReg)) in
  (* Update the stack pointer *)
  let acc = add_instr acc (Add (SP, SP, R1)) in
  (* We add the current FP to the stack *)
  let acc = add_instr acc (Store (SP, FP)) in
  (* Copy SP into FP *)
  let acc = add_instr acc (Add (FP, SP, R0)) in
  (* Update the stack pointer *)
  let acc = add_instr acc (Add (SP, SP, R1)) in
  acc

(** This function determines the type of jump we have to deal with. Three label jump are possible:
    - We jump to the target address with jmpi
    - We jump to the target address by loading it in rpriv and by jumping to rpriv.
      - The load can take 1 operation to be performed
      - Or 2 if the address is >= 2^16.
    That why this function returns, the number of operation needed and [None] if
    the jump can be done with jmpi or [Some i] with i the number of load needed *)
let label_jump_kind labels_pos acc label =
  (* [pseudo_addr] is the address we reason with. Its defined as:
     - If the label is before us, we know its final address, we return it.
     - Otherwise, it is after us, we return its estimation (witch is >= than his final address) *)
  let pseudo_addr =
    match computed_address labels_pos label with
    | Some addr ->
        addr
    | None ->
        estimated_address labels_pos label
  in
  (* We compute the offset between now and the pseudo address. This offset is:
     - Exact if negative (because pseudo_addr is exact if [ofs] < 0)
     - If positive its >= than the exact one. *)
  let ofs = ProgramAddress.offset_from_to (current_address acc) pseudo_addr in
  (* so we just check that we can jump at it in one operation jmpi *)
  if Offset.fit_in_int24 ofs then (* We can, 1 operation needed *)
    (1, None)
  else if
    (* We cant, we have to load the address in rpriv *)
    ProgramAddress.fit_in_uint16 pseudo_addr 0
  then
    (* Only one load needed for that ! So 2 operation with the jump *)
    (2, Some 1)
  else (* Two loads needed, so 3 operations with the jump *)
    (3, Some 2)

(** Perform the jump to the label [label] with flag [f]. [jump_kind] is what is
    returned by the function [label_jump_kind]. *)
let jump_label acc label f jump_kind =
  match jump_kind with
  | None -> (
    match f with
    | Some f ->
        add_future acc 1 (FutureJumpOffsetCond (f, label))
    | None ->
        add_future acc 1 (FutureJumpOffset label) )
  | Some i -> (
      let acc = add_future acc i (FuturePLabelLoad (PrivateReg, label, R0)) in
      match f with
      | Some f ->
          add_instr acc (JmpCond (PrivateReg, f))
      | None ->
          add_instr acc (Jmp PrivateReg) )

let position_section labels_pos begin_addr sec =
  let incr_and_ret accc v =
    let acc, a2c = accc in
    let acc = add_instr acc v in
    (acc, a2c)
  in
  let (next_addr, instrs), addr2check =
    Monoid.fold_left
      (fun ((acc, a2c) as accc) instr ->
        match instr.v with
        | TAnd (r1, r2, r3) ->
            incr_and_ret accc (And (r1, r2, r3))
        | TOr (r1, r2, r3) ->
            incr_and_ret accc (Or (r1, r2, r3))
        | TNor (r1, r2, r3) ->
            incr_and_ret accc (Nor (r1, r2, r3))
        | TXor (r1, r2, r3) ->
            incr_and_ret accc (Xor (r1, r2, r3))
        | TAdd (r1, r2, r3) ->
            incr_and_ret accc (Add (r1, r2, r3))
        | TSub (r1, r2, r3) ->
            incr_and_ret accc (Sub (r1, r2, r3))
        | TMul (r1, r2, r3) ->
            incr_and_ret accc (Mul (r1, r2, r3))
        | TDiv (r1, r2, r3) ->
            incr_and_ret accc (Div (r1, r2, r3))
        | TShiftLeftLogical (r1, r2, r3) ->
            incr_and_ret accc (ShiftLeftLogical (r1, r2, r3))
        | TShiftRightArith (r1, r2, r3) ->
            incr_and_ret accc (ShiftRightArith (r1, r2, r3))
        | TShiftRightLogical (r1, r2, r3) ->
            incr_and_ret accc (ShiftRightLogical (r1, r2, r3))
        | TLoad (r1, r2) ->
            incr_and_ret accc (Load (r1, r2))
        | TLoadImmediateAdd (r1, imm, mode, r2) ->
            incr_and_ret accc (LoadImmediateAdd (r1, r2, imm, mode))
        | TLoadProgLabelAdd (r1, prg_lbl, r2) ->
            let acc = load_label labels_pos acc r1 prg_lbl r2 in
            (acc, a2c)
        | TStore (r1, r2) ->
            incr_and_ret accc (Store (r1, r2))
        | TJmpLabel (f, label) ->
            let _, jump_kind = label_jump_kind labels_pos acc label in
            let acc = jump_label acc label f jump_kind in
            (acc, a2c)
        | TJmpRegister (None, r1) ->
            incr_and_ret accc (Jmp r1)
        | TJmpRegister (Some f, r1) ->
            incr_and_ret accc (JmpCond (r1, f))
        | TJmpOffset (f, offset) ->
            ( if not (ProgramAddress.well_defined (current_address acc) offset.v)
              then
                let txt =
                  Format.asprintf
                    "The address to which the program jumps, calculated from \
                     the  offset '%a' of the current address, is not well \
                     defined. In fact, this address is outside the permitted \
                     range (from %a to %a). The resulting address is \
                     implementation defined, please fix this."
                    Offset.pp offset.v ProgramAddress.pp ProgramAddress.first
                    ProgramAddress.pp ProgramAddress.last
                in
                warning txt offset.pos ) ;
            let acc, target_addr = jump_offset acc offset.v f offset.pos in
            (acc, ProgramAddress.Map.add target_addr offset.pos a2c)
        | TJmpAddress (f, target_addr) ->
            let offset =
              ProgramAddress.offset_from_to (current_address acc) target_addr.v
            in
            let acc, _ = jump_offset acc offset f target_addr.pos in
            (acc, ProgramAddress.Map.add target_addr.v target_addr.pos a2c)
        | TCallAddr reg ->
            let acc = setup_stack acc 1 in
            let acc = add_instr acc (Jmp reg) in
            (acc, a2c)
        | TCallLabel label ->
            (* The call is performed two stages:
               - First, we setup the stack (load the return address in the stack, ...)
               - Second, we jump to the label of the function
               We need to estimate the number of operation needed for the last step. *)
            let nb_op, jump_kind = label_jump_kind labels_pos acc label in
            let acc = setup_stack acc nb_op in
            let acc = jump_label acc label None jump_kind in
            (acc, a2c) )
      ((begin_addr, Monoid.empty), ProgramAddress.Map.empty)
      sec.body
  in
  (next_addr, instrs, addr2check)

let position_instrs estim_labels tprog =
  let a2c, pprog_label_position, pprog_next_address, pprog_instrs =
    Monoid.fold_left
      (fun (a2c, label_map, curr_addr, p_instrs) sec ->
        let next_addr, instrs, addr2check =
          position_section (estim_labels, label_map) curr_addr sec
        in
        let a2c =
          ProgramAddress.Map.union (fun _ p _ -> Some p) a2c addr2check
        in
        let label_map = ProgramLabel.Map.add sec.label curr_addr label_map in
        let p_instrs = Monoid.(p_instrs @@ instrs) in
        (a2c, label_map, next_addr, p_instrs) )
      ( ProgramAddress.Map.empty
      , ProgramLabel.Map.empty
      , ProgramAddress.first
      , Monoid.empty )
      tprog
  in
  ProgramAddress.Map.iter
    (fun addr pos ->
      if pprog_next_address < addr && addr < ProgramAddress.last then
        let txt =
          Format.asprintf
            "The address we jump at, %a, is not in the program. The program \
             instructions ends at %a."
            ProgramAddress.pp addr ProgramAddress.pp pprog_next_address
        in
        warning txt pos )
    a2c ;
  {pprog_instrs; pprog_label_position; pprog_next_address}
