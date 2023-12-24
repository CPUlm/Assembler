open PositionUtils
open Labels
open ErrorUtils

let split_by_label create_id l =
  match l with
  | [] -> (Monoid.empty, SMap.empty)
  | (None, i) :: _ ->
      let strip_pos =
        let pos = i.pos in
        { pos with end_line = pos.beg_line; beg_col = 1; end_col = 1 }
      in
      error "Missing name of the first section." strip_pos
  | (Some _, i) :: _ ->
      let close_section final_list cur_label current_sec label_map last_pos =
        if Monoid.is_empty current_sec then (final_list, label_map)
        else
          let sec_label = Option.get cur_label in
          let id = create_id sec_label in
          let section_pos = merge_pos sec_label.pos last_pos in
          let label_map = SMap.add sec_label.v id label_map in
          Monoid.
            ( final_list @@ Monoid.of_elm (mk_pos section_pos (id, current_sec)),
              label_map )
      in
      let final_list, cur_label, current_sec, label_map, last_pos =
        List.fold_left
          (fun (final_list, cur_label, current_sec, label_map, last_pos)
               (label, instr) ->
            match label with
            | None ->
                ( final_list,
                  cur_label,
                  Monoid.(current_sec @@ of_elm instr),
                  label_map,
                  instr.pos )
            | Some label ->
                let final_list, label_map =
                  close_section final_list cur_label current_sec label_map
                    last_pos
                in
                if SMap.mem label.v label_map then
                  let txt =
                    Format.sprintf "The label '%s' has already been declared."
                      label.v
                  in
                  error txt label.pos
                else
                  ( final_list,
                    Some label,
                    Monoid.of_elm instr,
                    label_map,
                    instr.pos ))
          (Monoid.empty, None, Monoid.empty, SMap.empty, i.pos)
          l
      in
      let final_list, label_set =
        close_section final_list cur_label current_sec label_map last_pos
      in
      (final_list, label_set)
