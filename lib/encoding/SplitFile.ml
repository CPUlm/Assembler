open PositionUtils
open ErrorUtils

let split_by_label fns l =
  let empty, create, add, mem = fns in
  match l with
  | [] -> (Monoid.empty, empty)
  | (None, i) :: _ ->
      let strip_pos =
        let pos = i.pos in
        { pos with end_line = pos.beg_line; beg_col = 1; end_col = 1 }
      in
      error "Missing name of the first section." strip_pos
  | (Some _, i) :: _ ->
      let close_section final_list cur_label current_sec label_set last_pos =
        if Monoid.is_empty current_sec then (final_list, label_set)
        else
          let sec_label = Option.get cur_label in
          let id = create sec_label in
          let section_pos = merge_pos sec_label.pos last_pos in
          Monoid.
            ( final_list @@ Monoid.of_elm (mk_pos section_pos (id, current_sec)),
              add sec_label id label_set )
      in
      let final_list, cur_label, current_sec, label_set, last_pos =
        List.fold_left
          (fun (final_list, cur_label, current_sec, label_set, last_pos)
               (label, instr) ->
            match label with
            | None ->
                ( final_list,
                  cur_label,
                  Monoid.(current_sec @@ of_elm instr),
                  label_set,
                  instr.pos )
            | Some label ->
                let final_list, label_set =
                  close_section final_list cur_label current_sec label_set
                    last_pos
                in
                if mem label.v label_set then
                  let txt =
                    Format.sprintf "The label '%s' has already been declared."
                      label.v
                  in
                  error txt label.pos
                else
                  ( final_list,
                    Some label,
                    Monoid.of_elm instr,
                    label_set,
                    instr.pos ))
          (Monoid.empty, None, Monoid.empty, empty, i.pos)
          l
      in
      let final_list, label_set =
        close_section final_list cur_label current_sec label_set last_pos
      in
      (final_list, label_set)