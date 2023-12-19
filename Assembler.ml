print_string "Hello World!"

let _ =
  let a =
    Ast.{ v = JmpOffset { v = 0; pos = assert false }; pos = assert false }
  in
  let b = CheckAst.process_pseudo (assert false) a in
  ignore b
