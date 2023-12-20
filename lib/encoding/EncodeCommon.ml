open Ast
open ErrorUtils

let _ =
  if Sys.int_size < 33 then
    failwith "The size of the integers available is not large enough."

let is_signed, is_unsigned =
  let two_32 = 1 lsl 32 in
  let two_31 = 1 lsl 31 in
  let neg_two_31 = -two_31 in
  ((fun i -> neg_two_31 <= i && i < two_31), fun i -> 0 <= i && i < two_32)

let check_signed_immediate imm =
  if is_signed imm.v then Int32.of_int imm.v
  else
    let txt =
      Format.sprintf
        "The value '%d' cannot be represented as a 16-bit signed integer." imm.v
    in
    type_error txt imm.pos

let check_unsigned_immediate imm =
  if is_unsigned imm.v then Int32.of_int imm.v
  else
    let txt =
      Format.sprintf
        "The value '%d' cannot be represented as a 16-bit unsigned integer."
        imm.v
    in
    type_error txt imm.pos

let check_immediate imm =
  if is_unsigned imm.v || is_signed imm.v then Int32.of_int imm.v
  else
    let txt =
      Format.sprintf "The value '%d' cannot be represented on 16 bit." imm.v
    in
    type_error txt imm.pos

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
  type res = { high : Int16.t; low : Int16.t }

  val zero : t
  val of_imm : immediate -> t
  val add : t -> int -> t
  val to_int16 : t -> res
end

(* let check_offset imm =
     if is_signed imm.v then imm.v
     else
       let txt =
         Format.sprintf
           "The value '%d' does not represent a valid 32-bit program address \
            offset."
           imm.v
       in
       type_error txt (Some imm.pos) *)

module Address32Bit : Address = struct
  type t = int
  type res = { high : Int16.t; low : Int16.t }

  let of_int addr pos =
    if is_unsigned addr then addr
    else
      let txt =
        Format.sprintf
          "The value '%d' does not represent a valid 32-bit program address."
          addr
      in
      type_error txt pos

  let zero = 0
  let of_imm imm = of_int imm.v imm.pos

  let add t i =
    (* of_int (t + i) None *)
    ignore (t, i);
    assert false

  let to_int16 t =
    match Int16.of_int32 (Int32.of_int t) with
    | Single i -> { high = Int16.zero; low = i }
    | Multiple i -> { high = i.high; low = i.low }
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
              type_error txt label.pos
            else (final_list, Some label, [ instr ], label_set))
      ([], None, [], empty) l
  in
  close_section final_list cur_label current_list label_set
