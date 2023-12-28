open Isa
open PAst
open Integers
open EncodedFile
open EncodingCommon

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
  { instr_bytes
  ; instr_label_mapping= fprog.fprog_label_mapping
  ; instr_label_position= fprog.fprog_label_position
  ; instr_next_address= fprog.fprog_next_address }
