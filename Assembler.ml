print_string "Hello World!"

let filename = "filename"
let channel = open_in filename

let lexbuf =
  let lexbuf = Lexing.from_channel channel in
  Lexing.set_filename lexbuf filename;
  lexbuf

let ast = Parser.file Lexer.gen_tokens lexbuf

let _ =
  ignore ast;
  let a =
    Ast.{ v = JmpOffset { v = 0l; pos = assert false }; pos = assert false }
  in
  let b = CheckAst.process_pseudo (assert false) a in
  ignore b
