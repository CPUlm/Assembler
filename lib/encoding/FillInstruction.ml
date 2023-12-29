open Isa
open Integers
open Labels
open PAst

let next addr =
  (* We can force because we have a majoration *)
  Option.get (ProgramAddress.next addr)

let rec pad beg_addr inst res_space =
  let len = List.length inst in
  if len > res_space then failwith "Not enough space reserved !"
  else if len = res_space then
    let next_addr, pinst_l =
      List.fold_left_map
        (fun cur_addr i -> (next cur_addr, (cur_addr, i)))
        beg_addr inst
    in
    (next_addr, Monoid.of_list pinst_l)
  else
    let cur_addr, inst = pad beg_addr inst (res_space - 1) in
    let next_addr = next cur_addr in
    (next_addr, Monoid.(inst @@ Monoid.of_elm (cur_addr, And (R0, R0, R0))))

let pad beg_addr inst res_space = snd (pad beg_addr inst res_space)

let fill_instruction pprog =
  let fprog_instrs =
    Monoid.map
      (fun (addr, instr) ->
        match instr with
        | Either.Left i ->
            Monoid.of_elm (addr, i)
        | Either.Right finst -> (
          match finst.op with
          | FuturePLabelLoad (r1, lbl, r2) -> (
              let target_addr =
                ProgramLabel.Map.find lbl pprog.pprog_label_position
              in
              match ProgramAddress.to_uint16 target_addr with
              | Single i ->
                  pad addr
                    [LoadImmediateAdd (r1, r2, i, LowHalf)]
                    finst.res_space
              | Multiple i ->
                  pad addr
                    [ LoadImmediateAdd (r1, r2, i.low, LowHalf)
                    ; LoadImmediateAdd (r1, r1, i.high, HighHalf) ]
                    finst.res_space )
          | FutureJumpOffset lbl -> (
              let target_addr =
                ProgramLabel.Map.find lbl pprog.pprog_label_position
              in
              let ofs = ProgramAddress.offset_from_to addr target_addr in
              match Offset.to_int24 ofs with
              | Some ofs ->
                  pad addr [JmpImmediate ofs] finst.res_space
              | None ->
                  failwith "Expected an Int24-compatible offset." )
          | FutureJumpOffsetCond (flag, lbl) -> (
              let target_addr =
                ProgramLabel.Map.find lbl pprog.pprog_label_position
              in
              let ofs = ProgramAddress.offset_from_to addr target_addr in
              match Offset.to_int24 ofs with
              | Some ofs ->
                  pad addr [JmpImmediateCond (ofs, flag)] finst.res_space
              | None ->
                  failwith "Expected an Int24-compatible offset." ) ) )
      pprog.pprog_instrs
  in
  { fprog_instrs
  ; fprog_label_position= pprog.pprog_label_position
  ; fprog_next_address= pprog.pprog_next_address }
