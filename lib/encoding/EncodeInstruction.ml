open Isa
open PAst
open Integers
open EncodedFile

let encode_opcode (pos, v) inst =
  let opcode_size = 4 in
  let c =
    match inst with
    | And _ | Or _ | Nor _ | Xor _ | Add _ | Sub _ | Mul _ | Div _ -> 0
    | ShiftLeftLogical _ -> 1
    | ShiftRightArith _ -> 2
    | ShiftRightLogical _ -> 3
    | Load _ -> 4
    | LoadImmediateAdd _ -> 5
    | Store _ -> 6
    | Jmp _ -> 7
    | JmpCond _ -> 8
    | JmpImmediate _ -> 9
    | JmpImmediateCond _ -> 10
  in
  (pos + opcode_size, Word.add_at v (Int32.of_int c) pos)

let encode_reg (pos, v) reg =
  let reg_size = 5 in
  let c =
    match reg with
    | R0 -> 0
    | R1 -> 1
    | R2 -> 2
    | R3 -> 3
    | R4 -> 4
    | R5 -> 5
    | R6 -> 6
    | R7 -> 7
    | R8 -> 8
    | R9 -> 9
    | R10 -> 10
    | R11 -> 11
    | R12 -> 12
    | R13 -> 13
    | R14 -> 14
    | R15 -> 15
    | R16 -> 16
    | R17 -> 17
    | R18 -> 18
    | R19 -> 19
    | R20 -> 20
    | R21 -> 21
    | R22 -> 22
    | R23 -> 23
    | R24 -> 24
    | R25 -> 25
    | R26 -> 26
    | R27 -> 27
    | ROut -> 28
    | SP -> 29
    | FP -> 30
    | PrivateReg -> 31
  in
  (pos + reg_size, Word.add_at v (Int32.of_int c) pos)

let encode_alucode (pos, v) inst =
  let alu_size = 5 in
  let c =
    match inst with
    | And _ -> 0
    | Or _ -> 1
    | Nor _ -> 2
    | Xor _ -> 3
    | Add _ -> 4
    | Sub _ -> 5
    | Mul _ -> 6
    | Div _ -> 7
    | _ -> failwith "Instruction not in the ALU !"
  in
  (pos + alu_size, Word.add_at v (Int32.of_int c) pos)

let encode_imm (pos, v) imm16 =
  let imm_size = 16 in
  let c = UInt16.encode imm16 in
  (pos + imm_size, Word.add_at v c pos)

let encode_loadmode (pos, v) mode =
  let loadmode_size = 1 in
  let c = match mode with LowHalf -> Int32.zero | HighHalf -> Int32.one in
  (pos + loadmode_size, Word.add_at v c pos)

let encode_flag (pos, v) flag =
  let flag_size = 4 in
  let c =
    match flag with
    | Zero -> Int32.of_int 1
    | Negative -> Int32.of_int 2
    | UnsignedUnderflowFlag -> Int32.of_int 4
    | SignedOverflowFlag -> Int32.of_int 8
  in
  (pos + flag_size, Word.add_at v c pos)

let encode_offset (pos, v) ofs24 =
  let offset_size = 24 in
  let c = Int24.encode ofs24 in
  (pos + offset_size, Word.add_at v c pos)

let get_code = snd

let encode_inst inst =
  let acc = (0, Word.zero) in
  match inst with
  | And (rd, rs1, rs2)
  | Or (rd, rs1, rs2)
  | Nor (rd, rs1, rs2)
  | Xor (rd, rs1, rs2)
  | Add (rd, rs1, rs2)
  | Sub (rd, rs1, rs2)
  | Mul (rd, rs1, rs2)
  | Div (rd, rs1, rs2) ->
      (* We encode the op-code *)
      let acc = encode_opcode acc inst in
      (* Then rd *)
      let acc = encode_reg acc rd in
      (* Then rs1 *)
      let acc = encode_reg acc rs1 in
      (* Then rs2 *)
      let acc = encode_reg acc rs2 in
      (* Then the alu-code *)
      let acc = encode_alucode acc inst in
      get_code acc
  | ShiftLeftLogical (rd, rs1, rs2)
  | ShiftRightArith (rd, rs1, rs2)
  | ShiftRightLogical (rd, rs1, rs2) ->
      (* We encode the op-code *)
      let acc = encode_opcode acc inst in
      (* Then rd *)
      let acc = encode_reg acc rd in
      (* Then rs1 *)
      let acc = encode_reg acc rs1 in
      (* Then rs2 *)
      let acc = encode_reg acc rs2 in
      get_code acc
  | Load (rd, rs) | Store (rd, rs) ->
      (* We encode the op-code *)
      let acc = encode_opcode acc inst in
      (* Then rd *)
      let acc = encode_reg acc rd in
      (* Then rs *)
      let acc = encode_reg acc rs in
      get_code acc
  | LoadImmediateAdd (rd, rs, imm, mode) ->
      (* We encode the op-code *)
      let acc = encode_opcode acc inst in
      (* Then rd *)
      let acc = encode_reg acc rd in
      (* Then rs *)
      let acc = encode_reg acc rs in
      (* Then the immediate *)
      let acc = encode_imm acc imm in
      (* Then the load mode *)
      let acc = encode_loadmode acc mode in
      get_code acc
  | Jmp rs ->
      (* We encode the op-code *)
      let acc = encode_opcode acc inst in
      (* Then rs *)
      let acc = encode_reg acc rs in
      get_code acc
  | JmpCond (rs, f) ->
      (* We encode the op-code *)
      let acc = encode_opcode acc inst in
      (* Then rs *)
      let acc = encode_reg acc rs in
      (* The the flags *)
      let acc = encode_flag acc f in
      get_code acc
  | JmpImmediate ofs ->
      (* We encode the op-code *)
      let acc = encode_opcode acc inst in
      (* Then the int24 offset *)
      let acc = encode_offset acc ofs in
      get_code acc
  | JmpImmediateCond (ofs, f) ->
      (* We encode the op-code *)
      let acc = encode_opcode acc inst in
      (* Then the int24 offset *)
      let acc = encode_offset acc ofs in
      (* The the flags *)
      let acc = encode_flag acc f in
      get_code acc

let encode_prog fprog =
  let instr_bytes =
    Monoid.map
      (fun (_, inst) -> Monoid.of_elm (encode_inst inst))
      fprog.fprog_instrs
  in
  {
    instr_bytes;
    instr_label_mapping = fprog.fprog_label_mapping;
    instr_label_position = fprog.fprog_label_position;
  }
