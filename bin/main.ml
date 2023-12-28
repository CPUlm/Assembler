open LibUlmAssembly

let usage = "usage: asm [options] file.ulm"

let parse_only = ref false

let spec =
  Arg.align
    [ (* Disable '-help' because its ugly without the -- *)
      ( "-help"
      , Arg.Unit (fun () -> raise (Arg.Bad "unknown option '-help'"))
      , "" )
    ; ("--parse-only", Arg.Set parse_only, "  Stop after parsing")
    ; ( "--fatal-warnings"
      , Arg.Set ErrorUtils.fatal_warnings
      , "  Treats warnings as errors" ) ]

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

let lexbuf =
  let lexbuf = Lexing.from_channel channel in
  Lexing.set_filename lexbuf filename ;
  lexbuf

let () =
  try
    let file = Parser.file PostLexer.next_token lexbuf in
    let data_file = EncodeData.encode_data file in
    let data_file, pre_encode_instr =
      ProcessInstruction.pre_encode_instr data_file file
    in
    let label_estim = EstimateLabel.estimate_labels pre_encode_instr in
    let pos_instr =
      PositionInstruction.position_instrs label_estim pre_encode_instr
    in
    let fill_instr = FillInstruction.fill_instruction pos_instr in
    let program_file = EncodeInstruction.encode_prog fill_instr in
    EncodedFile.write_data_file data_filename data_file ;
    EncodedFile.write_program_file program_filename program_file ;
    close_in channel
  with
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
  | _ ->
      let s = Lexing.lexeme_start_p lexbuf in
      let e = Lexing.lexeme_end_p lexbuf in
      let p = PositionUtils.lexloc_to_pos (s, e) in
      ErrorUtils.error "Unknown error." p
