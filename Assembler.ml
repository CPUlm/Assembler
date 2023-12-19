let usage = "usage: asm [options] file.ulm"
let parse_only = ref false

let spec =
  [
    ("--parse-only", Arg.Set parse_only, "  stop after parsing");
  ]

let filename =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".ulm") then
      raise (Arg.Bad "no .ulm extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with
  | Some f -> f
  | None ->
      Arg.usage spec usage;
      exit 1

let channel = open_in filename

let lexbuf =
  let lexbuf = Lexing.from_channel channel in
  Lexing.set_filename lexbuf filename;
  lexbuf

let () =
  try
    let ast = Parser.file Lexer.gen_tokens lexbuf in
    ignore ast;
    let a =
      Ast.{ v = JmpOffset { v = 0l; pos = assert false }; pos = assert false }
    in
    let checked_ast = CheckAst.process_pseudo (assert false) a in
    ignore checked_ast
  with
  | Lexer.Lexing_error msg ->
    let s = Lexing.lexeme_start_p lexbuf in
    let e = Lexing.lexeme_end_p lexbuf in
    let p = PositionUtils.lexloc_to_pos (s, e) in
    ErrorUtils.type_error msg p
  | Parser.Error ->
    let s = Lexing.lexeme_start_p lexbuf in
    let e = Lexing.lexeme_end_p lexbuf in
    let p = PositionUtils.lexloc_to_pos (s, e) in
    ErrorUtils.type_error "Syntax error." p
