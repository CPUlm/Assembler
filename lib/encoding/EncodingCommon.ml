open Integers
open Ast
open Isa

(** Encode a character *)
let encode_ascii (pos, v) chr =
  let ascii_size = 8 in
  let c = Char.code chr in
  (pos + ascii_size, Word.add_at v (Int32.of_int c) pos)

(** Encode a color *)
let encode_color (pos, v) col =
  let color_size = 5 in
  let c =
    match col with
    | Black ->
        0
    | Red ->
        1
    | Green ->
        2
    | Yellow ->
        3
    | Blue ->
        4
    | Magenta ->
        5
    | Cyan ->
        6
    | White ->
        7
    | BrightBlack ->
        8
    | BrightRed ->
        9
    | BrightGreen ->
        10
    | BrightYellow ->
        11
    | BrightBlue ->
        12
    | BrightMagenta ->
        13
    | BrightCyan ->
        14
    | BrightWhite ->
        15
    | Default ->
        16
  in
  (pos + color_size, Word.add_at v (Int32.of_int c) pos)

(** Encode a style set *)
let encode_style (pos, v) style =
  let style_size = 8 in
  let c =
    StyleSet.fold
      (fun style_piece acc ->
        match style_piece with
        | Bold ->
            Int32.(logor acc (shift_left one 0))
        | Faint ->
            Int32.(logor acc (shift_left one 1))
        | Italic ->
            Int32.(logor acc (shift_left one 2))
        | Underline ->
            Int32.(logor acc (shift_left one 3))
        | Blinking ->
            Int32.(logor acc (shift_left one 4))
        | Hide ->
            Int32.(logor acc (shift_left one 5))
        | Crossed ->
            Int32.(logor acc (shift_left one 6))
        | Overline ->
            Int32.(logor acc (shift_left one 7))
        | Default ->
            0l )
      style 0l
  in
  (pos + style_size, Word.add_at v c pos)

let encode_opcode (pos, v) inst =
  let opcode_size = 4 in
  let c =
    match inst with
    | And _ | Or _ | Nor _ | Xor _ | Add _ | Sub _ | Mul _ | Div _ ->
        0
    | ShiftLeftLogical _ ->
        1
    | ShiftRightArith _ ->
        2
    | ShiftRightLogical _ ->
        3
    | Load _ ->
        4
    | LoadImmediateAdd _ ->
        5
    | Store _ ->
        6
    | Jmp _ ->
        7
    | JmpCond _ ->
        8
    | JmpImmediate _ ->
        9
    | JmpImmediateCond _ ->
        10
  in
  (pos + opcode_size, Word.add_at v (Int32.of_int c) pos)

let encode_reg (pos, v) reg =
  let reg_size = 5 in
  let c =
    match reg with
    | R0 ->
        0
    | R1 ->
        1
    | R2 ->
        2
    | R3 ->
        3
    | R4 ->
        4
    | R5 ->
        5
    | R6 ->
        6
    | R7 ->
        7
    | R8 ->
        8
    | R9 ->
        9
    | R10 ->
        10
    | R11 ->
        11
    | R12 ->
        12
    | R13 ->
        13
    | R14 ->
        14
    | R15 ->
        15
    | R16 ->
        16
    | R17 ->
        17
    | R18 ->
        18
    | R19 ->
        19
    | R20 ->
        20
    | R21 ->
        21
    | R22 ->
        22
    | R23 ->
        23
    | R24 ->
        24
    | R25 ->
        25
    | R26 ->
        26
    | R27 ->
        27
    | ROut ->
        28
    | SP ->
        29
    | FP ->
        30
    | PrivateReg ->
        31
  in
  (pos + reg_size, Word.add_at v (Int32.of_int c) pos)

let encode_alucode (pos, v) inst =
  let alu_size = 5 in
  let c =
    match inst with
    | And _ ->
        0
    | Or _ ->
        1
    | Nor _ ->
        2
    | Xor _ ->
        3
    | Add _ ->
        4
    | Sub _ ->
        5
    | Mul _ ->
        6
    | Div _ ->
        7
    | _ ->
        failwith "Instruction not in the ALU !"
  in
  (pos + alu_size, Word.add_at v (Int32.of_int c) pos)

let encode_imm (pos, v) imm16 =
  let imm_size = 16 in
  let c = UInt16.encode imm16 in
  (pos + imm_size, Word.add_at v c pos)

let encode_loadmode (pos, v) mode =
  let loadmode_size = 1 in
  let c = match mode with LowHalf -> Int32.one | HighHalf -> Int32.zero in
  (pos + loadmode_size, Word.add_at v c pos)

let encode_flag (pos, v) flag =
  let flag_size = 4 in
  let c =
    match flag with
    | Zero ->
        Int32.of_int 1
    | Negative ->
        Int32.of_int 2
    | UnsignedUnderflowFlag ->
        Int32.of_int 4
    | SignedOverflowFlag ->
        Int32.of_int 8
  in
  (pos + flag_size, Word.add_at v c pos)

let encode_offset (pos, v) ofs24 =
  let offset_size = 24 in
  let c = Int24.encode ofs24 in
  (pos + offset_size, Word.add_at v c pos)
