open Ast

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

let print_error p msg =
  if p.beg_line = p.end_line then
    Format.eprintf "\x1b[1mFile \"%s\", line %d, characters %d-%d:\x1b[0m\n" p.file p.beg_line p.beg_col p.end_col
  else
    Format.eprintf "\x1b[1mFile \"%s\", lines %d-%d, characters %d-%d:\x1b[0m\n" p.file p.beg_line p.end_line p.beg_col p.end_col;
  Format.eprintf "\n\x1b[1;31mError\x1b[0m: %s@." msg;
  exit 1

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
    print_error p msg
  | Parser.Error ->
    let s = Lexing.lexeme_start_p lexbuf in
    let e = Lexing.lexeme_end_p lexbuf in
    let p = PositionUtils.lexloc_to_pos (s, e) in
    print_error p "syntax error"
