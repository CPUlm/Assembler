open LibUlmAssembly

type action =
  | ParseOnly
  | PrintMemMap
  | PrintTProg
  | PrintLblEstimation
  | PrintPProg
  | PrintLblPos
  | PrintFProg
  | PrintEncoded
  | Assemble

let action = ref Assemble

let set_action act () = action := act

let usage = "usage: asm [options] file.ulm"

let spec =
  Arg.align
    [ (* Disable '-help' because its ugly without the -- *)
      ( "-help"
      , Arg.Unit (fun () -> raise (Arg.Bad "unknown option '-help'"))
      , "" )
    ; ( "--fatal-warnings"
      , Arg.Set ErrorUtils.fatal_warnings
      , "  Treats warnings as errors." )
    ; ( "--parse-only"
      , Arg.Unit (set_action ParseOnly)
      , "  Stop after parsing and exits." )
    ; ( "--print-mem-map"
      , Arg.Unit (set_action PrintMemMap)
      , "  Print the mapping of the memory on the standard output." )
    ; ( "--print-tprog"
      , Arg.Unit (set_action PrintTProg)
      , "  Print the program on the standard output right after processing \
         pseudo-instruction." )
    ; ( "--print-label-estim"
      , Arg.Unit (set_action PrintLblEstimation)
      , "  Print the estimation of the position of each label on the standard \
         output." )
    ; ( "--print-label-pos"
      , Arg.Unit (set_action PrintLblPos)
      , "  Print the final of the position of each label on the standard \
         output." )
    ; ( "--print-pprog"
      , Arg.Unit (set_action PrintPProg)
      , "  Print the program on the standard output right after being laid out."
      )
    ; ( "--print-fprog"
      , Arg.Unit (set_action PrintFProg)
      , " Print the program on the standard output right before being encoded."
      )
    ; ( "--print-encoded"
      , Arg.Unit (set_action PrintEncoded)
      , " Print the program on the standard output after being encoded." ) ]

let filename =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".ulm") then
      raise (Arg.Bad "no .ulm extension") ;
    file := Some s
  in
  Arg.parse spec set_file usage ;
  match !file with Some f -> f | None -> Arg.usage spec usage ; exit 1

let data_filename = Filename.chop_extension filename ^ ".do" (* Data output *)

let program_filename =
  Filename.chop_extension filename ^ ".po" (* Program output *)

let channel = open_in filename

let () = Stdlib.at_exit (fun () -> close_in channel)

let lexbuf =
  let lexbuf = Lexing.from_channel channel in
  Lexing.set_filename lexbuf filename ;
  lexbuf

let main () =
  let file = Parser.file PostLexer.next_token lexbuf in
  if !action = ParseOnly then () ;
  let data_file = EncodeData.encode_data file in
  if !action = PrintMemMap then (
    PrettyPrinter.print_memory_map Format.std_formatter data_file ;
    exit 1 ) ;
  let data_file, tprog = ProcessInstruction.pre_encode_instr data_file file in
  if !action = PrintTProg then (
    PrettyPrinter.print_tprog Format.std_formatter tprog ;
    exit 0 ) ;
  let label_estim = EstimateLabel.estimate_labels tprog in
  if !action = PrintLblEstimation then (
    PrettyPrinter.print_label_estimation Format.std_formatter label_estim ;
    exit 0 ) ;
  let pprog = PositionInstruction.position_instrs label_estim tprog in
  if !action = PrintPProg then (
    PrettyPrinter.print_pprog Format.std_formatter pprog ;
    exit 0 ) ;
  if !action = PrintLblPos then (
    PrettyPrinter.print_label_final Format.std_formatter pprog label_estim ;
    exit 0 ) ;
  let fprog = FillInstruction.fill_instruction pprog in
  if !action = PrintFProg then (
    PrettyPrinter.print_fprog Format.std_formatter fprog ;
    exit 0 ) ;
  let program_file = EncodeInstruction.encode_prog fprog in
  if !action = PrintEncoded then (
    PrettyPrinter.print_encoded Format.std_formatter program_file fprog ;
    exit 0 ) ;
  EncodedFile.write_data_file data_filename data_file ;
  EncodedFile.write_program_file program_filename program_file

let () =
  try main () with
  | Lexer.Lexing_error msg ->
      let s = Lexing.lexeme_start_p lexbuf in
      let e = Lexing.lexeme_end_p lexbuf in
      let p = PositionUtils.lexloc_to_pos (s, e) in
      ErrorUtils.error msg p
  | Parser.Error ->
      let s = Lexing.lexeme_start_p lexbuf in
      let e = Lexing.lexeme_end_p lexbuf in
      let p = PositionUtils.lexloc_to_pos (s, e) in
      ErrorUtils.error "Syntax error." p
  | x ->
      let s = Lexing.lexeme_start_p lexbuf in
      let e = Lexing.lexeme_end_p lexbuf in
      let p = PositionUtils.lexloc_to_pos (s, e) in
      let txt = Format.sprintf "Unknown error:@.%s" (Printexc.to_string x) in
      ErrorUtils.error txt p
