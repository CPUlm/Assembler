open Integers
open Labels
open TAst
open ErrorUtils

let section_too_large sec start_addr estim_size =
  let txt =
    Format.asprintf
      "The '%s' section defined at %a is too long to be addressed with 32 \
       bits. In fact, it could have started at address %a, which is not \
       compatible with its size, which is estimated at %d instructions."
      (ProgramLabel.name sec.label)
      pp_pos sec.pos ProgramAddress.pp start_addr estim_size
  in
  error txt sec.pos

let max_section_end_addr lbl_map start_addr sec =
  let atomic_op (max_size, max_addr) =
    match ProgramAddress.next max_addr with
    | Some i ->
        (max_size + 1, i)
    | None ->
        section_too_large sec max_addr max_size
  in
  let load_address acc addr_op =
    (* Only depend on the address. Because its known, min and max do not
       diverges *)
    match addr_op with
    | None ->
        (* Unknown address, so at most 2 loads needed *)
        atomic_op (atomic_op acc)
    | Some addr ->
        if ProgramAddress.fit_in_uint16 addr 0 then
          (* Only one load ! The address is in [0; 2^16 - 1] *)
          atomic_op acc
        else
          (* Two loads :/ The address is in [2^16; 2^31 - 1] *)
          atomic_op (atomic_op acc)
  in
  let jump_offset acc ofs =
    if Offset.fit_in_int24 ofs then
      (* We can use the jmpi instruction directly :) *)
      atomic_op acc
    else
      (* We can't use jmpi :/ So we will load the absolute address in rpriv
         and we jump. IN THE WORST CASE POSSIBLE the load take 2 atomic
         operations *)
      let acc = atomic_op (atomic_op acc) in
      (* And after, we do a jump *)
      atomic_op acc
  in
  let jump_address acc addr =
    (* IN THE WORST CASE POSSIBLE, a jump to an address is made by:
       - Load the address in rpriv *)
    let acc = load_address acc addr in
    (* Jumping to rpriv *)
    atomic_op acc
  in
  let _, end_addr =
    Monoid.fold_left
      (fun ((_, curr_addr) as acc) (i : tinstr) ->
        match get_instr i with
        (* Logical Operations *)
        | TAnd _ ->
            atomic_op acc
        | TOr _ ->
            atomic_op acc
        | TNor _ ->
            atomic_op acc
        | TXor _ ->
            atomic_op acc
        (* Arithmetic operation *)
        | TAdd _ ->
            atomic_op acc
        | TSub _ ->
            atomic_op acc
        | TMul _ ->
            atomic_op acc
        | TDiv _ ->
            atomic_op acc
        (* Shifts operations *)
        | TShiftLeftLogical _ ->
            atomic_op acc
        | TShiftRightArith _ ->
            atomic_op acc
        | TShiftRightLogical _ ->
            atomic_op acc
        (* Memory operations *)
        | TLoad _ ->
            atomic_op acc
        | TLoadImmediateAdd _ ->
            atomic_op acc
        | TLoadProgLabelAdd (_, lbl, _) ->
            load_address acc (ProgramLabel.Map.find_opt lbl lbl_map)
        | TStore _ ->
            atomic_op acc
        (* Flow instructions *)
        | TJmpLabel (_, lbl) ->
            jump_address acc (ProgramLabel.Map.find_opt lbl lbl_map)
        | TJmpRegister _ ->
            atomic_op acc
        | TJmpOffset (_, offset) ->
            jump_offset acc offset.v
        | TJmpAddress (_, prog_addr) ->
            jump_address acc (Some prog_addr.v)
        (* Function Call *)
        | TCallAddr _ ->
            (* We will load the current address in rpriv *)
            let acc = load_address acc (Some curr_addr) in
            (* Push rpriv to the stack : 2 atomic operations
               - store sp rpriv
               - add sp sp r1 *)
            let acc = atomic_op (atomic_op acc) in
            (* And now, we can jump to the given register ! *)
            atomic_op acc
        | TCallLabel lbl ->
            (* We will load the current address in rpriv *)
            let acc = load_address acc (Some curr_addr) in
            (* Push rpriv to the stack : 2 atomic operations
               - store sp rpriv
               - add sp sp r1 *)
            let acc = atomic_op (atomic_op acc) in
            (* And now, we jump to the address associate to [lbl] *)
            jump_address acc (ProgramLabel.Map.find_opt lbl lbl_map) )
      (0, start_addr) sec.body
  in
  end_addr

let estimate_labels tprog =
  let label_estimation, _ =
    Monoid.fold_left
      (fun (map, curr_pos) sec ->
        let map = ProgramLabel.Map.add sec.label curr_pos map in
        let end_addr = max_section_end_addr map curr_pos sec in
        (map, end_addr) )
      (ProgramLabel.Map.empty, ProgramAddress.first)
      tprog
  in
  label_estimation
