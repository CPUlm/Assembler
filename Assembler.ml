print_string "Hello World!"

let _ =
  let a = Ast.JmpOffset 0 in
  let b = CheckAst.process_pseudo a in
  ignore b
