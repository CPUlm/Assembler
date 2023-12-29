open Format
open Isa
open Integers
open Labels
open PAst
open TAst
open PositionUtils
open EncodedFile

let pp_reg ppf = function
  | R0 ->
      pp_print_string ppf "r0"
  | R1 ->
      pp_print_string ppf "r1"
  | R2 ->
      pp_print_string ppf "r2"
  | R3 ->
      pp_print_string ppf "r3"
  | R4 ->
      pp_print_string ppf "r4"
  | R5 ->
      pp_print_string ppf "r5"
  | R6 ->
      pp_print_string ppf "r6"
  | R7 ->
      pp_print_string ppf "r7"
  | R8 ->
      pp_print_string ppf "r8"
  | R9 ->
      pp_print_string ppf "r9"
  | R10 ->
      pp_print_string ppf "r10"
  | R11 ->
      pp_print_string ppf "r11"
  | R12 ->
      pp_print_string ppf "r12"
  | R13 ->
      pp_print_string ppf "r13"
  | R14 ->
      pp_print_string ppf "r14"
  | R15 ->
      pp_print_string ppf "r15"
  | R16 ->
      pp_print_string ppf "r16"
  | R17 ->
      pp_print_string ppf "r17"
  | R18 ->
      pp_print_string ppf "r18"
  | R19 ->
      pp_print_string ppf "r19"
  | R20 ->
      pp_print_string ppf "r20"
  | R21 ->
      pp_print_string ppf "r21"
  | R22 ->
      pp_print_string ppf "r22"
  | R23 ->
      pp_print_string ppf "r23"
  | R24 ->
      pp_print_string ppf "r24"
  | R25 ->
      pp_print_string ppf "r25"
  | R26 ->
      pp_print_string ppf "r26"
  | R27 ->
      pp_print_string ppf "r27"
  | ROut ->
      pp_print_string ppf "rout"
  | SP ->
      pp_print_string ppf "sp"
  | FP ->
      pp_print_string ppf "fp"
  | PrivateReg ->
      pp_print_string ppf "rpriv"

let pp_flag ppf = function
  | Zero ->
      pp_print_string ppf "Z"
  | Negative ->
      pp_print_string ppf "N"
  | UnsignedUnderflowFlag ->
      pp_print_string ppf "C"
  | SignedOverflowFlag ->
      pp_print_string ppf "V"

let pp_isa ppf = function
  | And (rd, rs1, rs2) ->
      fprintf ppf "AND %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Or (rd, rs1, rs2) ->
      fprintf ppf "OR %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Nor (rd, rs1, rs2) ->
      fprintf ppf "NOR %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Xor (rd, rs1, rs2) ->
      fprintf ppf "XOR %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Add (rd, rs1, rs2) ->
      fprintf ppf "ADD %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Sub (rd, rs1, rs2) ->
      fprintf ppf "SUB %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Mul (rd, rs1, rs2) ->
      fprintf ppf "MUL %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Div (rd, rs1, rs2) ->
      fprintf ppf "DIV %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | ShiftLeftLogical (rd, rs1, rs2) ->
      fprintf ppf "LSL %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | ShiftRightArith (rd, rs1, rs2) ->
      fprintf ppf "ASR %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | ShiftRightLogical (rd, rs1, rs2) ->
      fprintf ppf "LSR %a %a %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Load (rd, rs) ->
      fprintf ppf "LOAD %a %a" pp_reg rd pp_reg rs
  | LoadImmediateAdd (rd, rs, imm, LowHalf) ->
      fprintf ppf "LOADI %a %a %a 1" pp_reg rd UInt16.pp imm pp_reg rs
  | LoadImmediateAdd (rd, rs, imm, HighHalf) ->
      fprintf ppf "LOADI %a %a %a 0" pp_reg rd UInt16.pp imm pp_reg rs
  | Store (rd, rs) ->
      fprintf ppf "STORE %a %a" pp_reg rd pp_reg rs
  | Jmp rd ->
      fprintf ppf "JMP %a" pp_reg rd
  | JmpCond (rd, f) ->
      fprintf ppf "JMPC %a %a" pp_reg rd pp_flag f
  | JmpImmediate imm24 ->
      fprintf ppf "JMPI %a" Int24.pp imm24
  | JmpImmediateCond (imm24, f) ->
      fprintf ppf "JMPIC %a %a" Int24.pp imm24 pp_flag f

let pp_future ppf = function
  | FuturePLabelLoad (rd, lbl, rs) ->
      fprintf ppf "Future (%a <- %s + %a)" pp_reg rd (ProgramLabel.name lbl)
        pp_reg rs
  | FutureJumpOffset lbl ->
      fprintf ppf "Future (Jump %s)" (ProgramLabel.name lbl)
  | FutureJumpOffsetCond (f, lbl) ->
      fprintf ppf "Future (Jump %s if %a)" (ProgramLabel.name lbl) pp_flag f

let pp_label_head ppf rev_map addr pad =
  match Hashtbl.find_opt rev_map addr with
  | Some lbl ->
      let str = ProgramLabel.name lbl in
      if str = "" then () else fprintf ppf "@.%s%s:@." pad str
  | None ->
      ()

let print_fprog ppf fprog =
  let rev_map = Hashtbl.create 17 in
  let _ =
    ProgramLabel.Map.iter
      (fun label addr -> Hashtbl.add rev_map addr label)
      fprog.fprog_label_position
  in
  let label_pad = String.make 11 ' ' in
  let instr_pad = String.make 5 ' ' in
  Monoid.iter
    (fun (addr, isa) ->
      pp_label_head ppf rev_map addr label_pad ;
      fprintf ppf "%a%s%a@." ProgramAddress.pp addr instr_pad pp_isa isa )
    fprog.fprog_instrs

let rec repeat f acc n =
  if n = 0 then acc
  else
    let acc = f acc in
    repeat f acc (n - 1)

let print_pprog ppf pprog =
  let rev_map = Hashtbl.create 17 in
  let _ =
    ProgramLabel.Map.iter
      (fun label addr -> Hashtbl.add rev_map addr label)
      pprog.pprog_label_position
  in
  let label_pad = String.make 11 ' ' in
  let instr_pad = String.make 5 ' ' in
  Monoid.iter
    (fun (addr, instr) ->
      pp_label_head ppf rev_map addr label_pad ;
      match instr with
      | Either.Left isa ->
          fprintf ppf "%a%s%a@." ProgramAddress.pp addr instr_pad pp_isa isa
      | Either.Right futur ->
          fprintf ppf "%a%s%a@." ProgramAddress.pp addr instr_pad pp_future
            futur.op ;
          let _ =
            repeat
              (fun last_addr ->
                let next_addr = Option.get (ProgramAddress.next last_addr) in
                fprintf ppf "%a%s  [Reserved]@." ProgramAddress.pp next_addr
                  instr_pad ;
                next_addr )
              addr (futur.res_space - 1)
          in
          () )
    pprog.pprog_instrs

let print_encoded ppf prog_file fprog =
  let rev_map = Hashtbl.create 17 in
  ProgramLabel.Map.iter
    (fun label addr -> Hashtbl.add rev_map addr label)
    fprog.fprog_label_position ;
  let word_list = Monoid.to_list prog_file.instr_bytes in
  let isa_list = Monoid.to_list fprog.fprog_instrs in
  let label_pad = String.make 44 ' ' in
  let instr_pad = String.make 5 ' ' in
  List.iter2
    (fun word (addr, isa) ->
      pp_label_head ppf rev_map addr label_pad ;
      fprintf ppf "%a %a%s%a@." ProgramAddress.pp addr Word.pp word instr_pad
        pp_isa isa )
    word_list isa_list

let pp_tast ppf = function
  | TAnd (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a and %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TOr (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a or %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TNor (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a nor %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TXor (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a xor %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TAdd (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a + %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TSub (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a - %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TMul (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a * %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TDiv (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a / %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TShiftLeftLogical (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a lsl %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TShiftRightArith (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a asr %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TShiftRightLogical (rd, rs1, rs2) ->
      fprintf ppf "%a <- %a lsr %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | TLoad (rd, rs) ->
      fprintf ppf "%a <- MEM[%a]" pp_reg rd pp_reg rs
  | TLoadImmediateAdd (rd, imm16, LowHalf, rs) ->
      fprintf ppf "%a <- %a + %a" pp_reg rd UInt16.pp imm16 pp_reg rs
  | TLoadImmediateAdd (rd, imm16, HighHalf, rs) ->
      fprintf ppf "%a <- %a0000 + %a" pp_reg rd UInt16.pp imm16 pp_reg rs
  | TLoadProgLabelAdd (rd, lbl, None) ->
      fprintf ppf "%a <- $%s" pp_reg rd (ProgramLabel.name lbl)
  | TLoadProgLabelAdd (rd, lbl, Some rs) ->
      fprintf ppf "%a <- $%s + %a" pp_reg rd (ProgramLabel.name lbl) pp_reg rs
  | TStore (rd, rs) ->
      fprintf ppf "MEM[%a] <- %a" pp_reg rd pp_reg rs
  | TJmpLabel (None, lbl) ->
      fprintf ppf "Jump to %s" (ProgramLabel.name lbl)
  | TJmpLabel (Some f, lbl) ->
      fprintf ppf "Jump to %s if %a" (ProgramLabel.name lbl) pp_flag f
  | TJmpRegister (None, rd) ->
      fprintf ppf "Jump to %a" pp_reg rd
  | TJmpRegister (Some f, rd) ->
      fprintf ppf "Jump to %a if %a" pp_reg rd pp_flag f
  | TJmpOffset (None, ofs) ->
      fprintf ppf "Jump %a instructions" Offset.pp ofs.v
  | TJmpOffset (Some f, ofs) ->
      fprintf ppf "Jump %a instructions if %a" Offset.pp ofs.v pp_flag f
  | TJmpAddress (None, addr) ->
      fprintf ppf "Jump to %a" ProgramAddress.pp addr.v
  | TJmpAddress (Some f, addr) ->
      fprintf ppf "Jump to %a if %a" ProgramAddress.pp addr.v pp_flag f
  | TCallAddr rd ->
      fprintf ppf "Call %a" pp_reg rd
  | TCallLabel lbl ->
      fprintf ppf "Call $%s" (ProgramLabel.name lbl)

let print_tprog ppf tprog =
  let instr_pad = String.make 4 ' ' in
  Monoid.iter
    (fun sec ->
      let sec_name = ProgramLabel.name sec.label in
      if sec_name <> "" then fprintf ppf "@.%s:@." sec_name ;
      Monoid.iter (fun i -> fprintf ppf "%s%a@." instr_pad pp_tast i.v) sec.body
      )
    tprog

let colum_sep = String.make 5 ' '

let print_label_estimation ppf e =
  let label_title = "Program Labels" in
  let estim_title = "Estimated Address" in
  let estim_padding_left =
    let space =
      assert (String.length estim_title > 10) ;
      String.length estim_title - 10
    in
    String.make (space / 2) ' '
  in
  let max_label_length =
    ProgramLabel.Map.fold
      (fun lbl _ acc ->
        let name = ProgramLabel.name lbl in
        if String.length name > acc then String.length name else acc )
      e
      (String.length label_title)
  in
  let _ =
    let title_padding =
      String.make (max_label_length - String.length label_title) ' '
    in
    fprintf ppf "%s%s %s %s@." label_title title_padding colum_sep estim_title
  in
  ProgramLabel.Map.iter
    (fun lbl addr ->
      let name = ProgramLabel.name lbl in
      if name = "" then ()
      else
        let label_padding =
          String.make (max_label_length - String.length name) ' '
        in
        fprintf ppf "%s%s %s %s%a@." name label_padding colum_sep
          estim_padding_left ProgramAddress.pp addr )
    e

let print_label_final ppf pprog e =
  let label_title = "Program Labels" in
  let final_title = "Final Address" in
  let final_padding_left, final_padding_right =
    let space =
      assert (String.length final_title > 10) ;
      String.length final_title - 10
    in
    (String.make (space / 2) ' ', String.make (space - (space / 2)) ' ')
  in
  let estim_title = "Estimated Address" in
  let estim_padding_left =
    let space =
      assert (String.length estim_title > 10) ;
      String.length estim_title - 10
    in
    String.make (space / 2) ' '
  in
  let max_label_length =
    ProgramLabel.Map.fold
      (fun lbl _ acc ->
        let name = ProgramLabel.name lbl in
        if String.length name > acc then String.length name else acc )
      e
      (String.length label_title)
  in
  let _ =
    let title_padding =
      String.make (max_label_length - String.length label_title) ' '
    in
    fprintf ppf "%s%s %s %s %s %s@." label_title title_padding colum_sep
      final_title colum_sep estim_title
  in
  ProgramLabel.Map.iter
    (fun lbl addr ->
      let name = ProgramLabel.name lbl in
      if name = "" then ()
      else
        let label_padding =
          String.make (max_label_length - String.length name) ' '
        in
        let estim_addr = ProgramLabel.Map.find lbl e in
        fprintf ppf "%s%s %s %s%a%s %s %s%a@." name label_padding colum_sep
          final_padding_left ProgramAddress.pp addr final_padding_right
          colum_sep estim_padding_left ProgramAddress.pp estim_addr )
    pprog.pprog_label_position

let print_memory_map ppf data_file =
  let label_title = "Data Labels" in
  let address_title = "Memory Address" in
  let addr_padding_left, addr_padding_right =
    let space =
      assert (String.length address_title > 10) ;
      String.length address_title - 10
    in
    (String.make (space / 2) ' ', String.make (space - (space / 2)) ' ')
  in
  let size_title = "Size" in
  let max_label_length =
    DataLabel.Map.fold
      (fun lbl _ acc ->
        let name = DataLabel.name lbl in
        if String.length name > acc then String.length name else acc )
      data_file.data_label_info
      (String.length label_title)
  in
  let max_section_size =
    DataLabel.Map.fold
      (fun _ info acc ->
        let size_string_length = String.length (string_of_int info.size) in
        if size_string_length > acc then size_string_length else acc )
      data_file.data_label_info (String.length size_title)
  in
  let _ =
    let title_padding =
      String.make (max_label_length - String.length label_title) ' '
    in
    let size_padding =
      String.make ((max_section_size - String.length size_title) / 2) ' '
    in
    fprintf ppf "%s%s %s %s %s %s%s@." label_title title_padding colum_sep
      address_title colum_sep size_padding size_title
  in
  DataLabel.Map.iter
    (fun lbl info ->
      let name = DataLabel.name lbl in
      if name = "" then ()
      else
        let label_padding =
          String.make (max_label_length - String.length name) ' '
        in
        let size_str = string_of_int info.size in
        let size_padding =
          String.make ((max_section_size - String.length size_str) / 2) ' '
        in
        fprintf ppf "%s%s %s %s%a%s %s %s%s@." name label_padding colum_sep
          addr_padding_left MemoryAddress.pp info.address addr_padding_right
          colum_sep size_padding size_str )
    data_file.data_label_info
