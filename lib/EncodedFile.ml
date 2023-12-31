open Integers
open Labels
open ErrorUtils

type data_label_info = {address: MemoryAddress.t; size: int}

type data_file =
  { data_bytes: Word.t Monoid.t
  ; data_label_mapping: DataLabel.t SMap.t
  ; data_label_info: data_label_info DataLabel.map
  ; data_next_address: MemoryAddress.t }

type instr_file =
  { instr_bytes: Word.t Monoid.t
  ; instr_label_position: ProgramAddress.t ProgramLabel.map
  ; instr_next_address: ProgramAddress.t }

let write_data_file filename dfile =
  ( if Sys.file_exists filename then
      let txt =
        Format.sprintf
          "The data segment output file '%s' already exists. It will be \
           overwritten"
          filename
      in
      file_warning txt ) ;
  let f = open_out filename in
  try
    Monoid.iter (fun w -> output_bytes f (Word.to_bytes w)) dfile.data_bytes ;
    close_out f
  with e -> close_out f ; raise e

let write_program_file filename pfile =
  ( if Sys.file_exists filename then
      let txt =
        Format.sprintf
          "The program segment output file '%s' already exists. It will be \
           overwritten"
          filename
      in
      file_warning txt ) ;
  let f = open_out filename in
  try
    Monoid.iter (fun w -> output_bytes f (Word.to_bytes w)) pfile.instr_bytes ;
    close_out f
  with e -> close_out f ; raise e
