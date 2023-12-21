open PositionUtils
open Integers
open ErrorUtils

module type Address = sig
  type t

  val zero : t
  val add_section : t -> string * int * position -> t
  val next : t -> string * int * position -> 'a pos -> t
  val with_offset : t -> Offset.t -> t
  val to_uint16 : t -> UInt16.res

  module Set : Set.S with type elt = t

  type set = Set.t

  module Map : Map.S with type key = t

  type 'a map = 'a Map.t
end

module Address32Bit : Address = struct
  type t = int

  let zero = 0
  let last = 0xffffffff
  let is_valid i = 0 <= i && i <= last

  let add_section t sec =
    let sec_name, sec_size, sec_pos = sec in
    let new_addr = t + sec_size in
    if is_valid new_addr then new_addr
    else
      let txt =
        Format.sprintf
          "The section '%s' of size %d, is too long to be in a 32bit address \
           space. It should end at the address %x, but this does not fit in a \
           32bit integer."
          sec_name sec_size new_addr
      in
      error txt sec_pos

  let next t sec _ =
    let new_addr = t + 1 in
    if is_valid new_addr then new_addr
    else
      let sec_name, sec_beg, sec_pos = sec in
      let txt =
        Format.sprintf
          "The section '%s' begining at address %x is too long to be in a \
           32bit address space."
          sec_name sec_beg
      in
      error txt sec_pos

  let with_offset t (offset, pos) =
    let new_addr = t + offset in
    if is_valid new_addr then new_addr
    else
      let txt =
        Format.sprintf "The address '%x' is not a 32bit integer." new_addr
      in
      error txt pos

  let to_uint16 i = UInt16.of_int32 (Int32.of_int i)

  module Set = Set.Make (Int)

  type set = Set.t

  module Map = Map.Make (Int)

  type 'a map = 'a Map.t
end

let split_by_label fns l =
  let empty, create, add, mem = fns in
  let close_section final_list cur_label current_list label_set =
    if current_list <> [] then
      match cur_label with
      | None -> file_error "Non-empty section without name."
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
              file_error "Missing name of the first section."
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
              error txt label.pos
            else (final_list, Some label, [ instr ], label_set))
      ([], None, [], empty) l
  in
  close_section final_list cur_label current_list label_set
