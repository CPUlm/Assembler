open Integers
open Labels
open ErrorUtils

type data_file =
  { data_bytes: Word.t Monoid.t
  ; data_label_mapping: DataLabel.t SMap.t
  ; data_label_position: MemoryAddress.t DataLabel.map }

type instr_file =
  { instr_bytes: Word.t Monoid.t
  ; instr_label_mapping: ProgramLabel.t SMap.t
  ; instr_label_position: ProgramAddress.t ProgramLabel.map }

let write_data_file filename dfile =
  if Sys.file_exists filename then
    let txt =
      Format.sprintf "The data segment output file '%s' already exists."
        filename
    in
    file_error txt
  else
    let f = open_out filename in
    Monoid.iter (fun w -> output_bytes f (Word.to_bytes w)) dfile.data_bytes ;
    close_out f

let write_program_file filename pfile =
  if Sys.file_exists filename then
    let txt =
      Format.sprintf "The program segment output file '%s' already exists."
        filename
    in
    file_error txt
  else
    let f = open_out filename in
    Monoid.iter (fun w -> output_bytes f (Word.to_bytes w)) pfile.instr_bytes ;
    close_out f
