open Ast
open ErrorUtils

let two_32 = Int32.(shift_left one 32)
let two_31 = Int32.(shift_left one 31)
let neg_two_31 = Int32.(sub zero two_31)
let _signed i = neg_two_31 <= i && i < two_31
let _unsigned i = 0l <= i && i < two_32

let check_signed_immediate imm =
  if _signed imm.v then imm.v
  else
    let txt =
      Format.sprintf
        "The value '%ld' cannot be represented as a 16-bit signed integer."
        imm.v
    in
    type_error txt (Some imm.pos)

let check_unsigned_immediate imm =
  if _unsigned imm.v then imm.v
  else
    let txt =
      Format.sprintf
        "The value '%ld' cannot be represented as a 16-bit unsigned integer."
        imm.v
    in
    type_error txt (Some imm.pos)

let check_immediate imm =
  if _unsigned imm.v then check_unsigned_immediate imm
  else if _signed imm.v then check_signed_immediate imm
  else
    let txt =
      Format.sprintf "The value '%ld' cannot be represented on 16 bit." imm.v
    in
    type_error txt (Some imm.pos)

let split_by_label fns l =
  let empty, create, add, mem = fns in
  let close_section final_list cur_label current_list label_set =
    if current_list <> [] then
      match cur_label with
      | None -> type_error "Non-empty section without name." None
      | Some s ->
          let id = create s in
          (final_list @ [ (id, List.rev current_list) ], add s id label_set)
    else (final_list, label_set)
  in
  let final_list, cur_label, current_list, label_set =
    List.fold_left
      (fun (final_list, cur_label, current_list, label_set) (label, instr) ->
        match label with
        | None ->
            if cur_label = None then
              type_error "Missing name of the first section." None
            else (final_list, cur_label, instr :: current_list, label_set)
        | Some label ->
            let final_list, label_set =
              close_section final_list cur_label current_list label_set
            in
            if mem label.v label_set then
              let txt =
                Format.sprintf "The label '%s' has already been declared."
                  label.v
              in
              type_error txt (Some label.pos)
            else (final_list, Some label.v, [ instr ], label_set))
      ([], None, [], empty) l
  in
  close_section final_list cur_label current_list label_set
