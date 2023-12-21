open Ast
open ErrorUtils

module Int16 : sig
  type t
  type res = Single of t | Multiple of { low : t; high : t }

  val zero : t
  val of_int32 : int32 -> res
end = struct
  type t = int32
  type res = Single of t | Multiple of { low : t; high : t }

  let two_16 = Int32.(shift_left one 16)
  let is_16bit imm = 0l <= imm && imm < two_16
  let zero = 0l

  let of_int32 i =
    if is_16bit i then Single i
    else
      let low, high =
        (Int32.(logand i 0xffffl), Int32.(shift_right_logical i 16))
      in
      Multiple { low; high }
end

module type Address = sig
  type t

  val zero : t
  val of_imm : immediate -> t
  val add_section : t -> string * int * position -> t
  val next : t -> string * int * position -> 'a pos -> t
  val with_offset : t -> int * position -> t
  val fit_16bit : t -> int * position -> bool
  val to_int16 : t -> Int16.res

  module Set : Set.S with type elt = t

  type set = Set.t

  module Map : Map.S with type key = t

  type 'a map = 'a Map.t
end

let _ =
  if Sys.int_size < 33 then
    failwith "The size of the integers available is not large enough."

let is_signed, is_unsigned =
  let two_32 = 1 lsl 32 in
  let two_31 = 1 lsl 31 in
  let neg_two_31 = -two_31 in
  ((fun i -> neg_two_31 <= i && i < two_31), fun i -> 0 <= i && i < two_32)

let is_32bit i = is_unsigned i || is_signed i

let check_immediate imm =
  if is_32bit imm.v then Int32.of_int imm.v
  else
    let txt =
      Format.sprintf "The value '%d' cannot be represented on 32 bit." imm.v
    in
    error txt imm.pos

let check_offset imm =
  if is_32bit imm.v then imm.v (* TODO, change here *)
  else
    let txt =
      Format.sprintf
        "The value '%d' does not represent a valid 32-bit program address \
         offset."
        imm.v
    in
    error txt imm.pos

let immediate_to_int16 imm = Int16.of_int32 (check_immediate imm)

module Address32Bit : Address = struct
  type t = int

  let zero = 0

  let of_imm imm =
    if is_32bit imm.v then imm.v
    else
      let txt =
        Format.sprintf
          "The value '%d' does not represent a valid 32-bit address." imm.v
      in
      error txt imm.pos

  let add_section t sec =
    let sec_name, sec_size, sec_pos = sec in
    let new_addr = t + sec_size in
    if is_32bit new_addr then new_addr
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
    if is_32bit new_addr then new_addr
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
    if is_32bit new_addr then new_addr
    else
      let txt =
        Format.sprintf "The address '%x' is not a 32bit integer." new_addr
      in
      error txt pos

  let fit_16bit t ofs =
    let i = with_offset t ofs in
    match Int16.of_int32 (Int32.of_int i) with
    | Single _ -> true
    | Multiple _ -> false

  let to_int16 t = Int16.of_int32 (Int32.of_int t)

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
