open LibUlmAssembly
open LibUlmAssembly.Parser

let dump_token token = match token with
  | EOF -> Printf.printf "EOF\n"
  | STR i -> Printf.printf "STR %s\n" i
  | LBL i -> Printf.printf "LBL %s\n" i
  | IDENT i -> Printf.printf "IDENT %s\n" i
  | IMM i -> Printf.printf "IMM %d\n" i
  | COLON -> Printf.printf "COLON\n"
  | OFFS i -> Printf.printf "OFFS %d\n" i
  | ADD -> Printf.printf "ADD\n"
  | SUB -> Printf.printf "SUB\n"
  | MUL -> Printf.printf "MUL\n"
  | DIV -> Printf.printf "DIV\n"
  | AND -> Printf.printf "AND\n"
  | NOR -> Printf.printf "NOR\n"
  | XOR -> Printf.printf "XOR\n"
  | OR -> Printf.printf "OR\n"
  | LSL -> Printf.printf "LSL\n"
  | ASR -> Printf.printf "ASR\n"
  | LSR -> Printf.printf "LSR\n"
  | NOT -> Printf.printf "NOT\n"
  | NOP -> Printf.printf "NOP\n"
  | CALL -> Printf.printf "CALL\n"
  | RET -> Printf.printf "RET\n"
  | TEST -> Printf.printf "TEST\n"
  | NEG -> Printf.printf "NEG\n"
  | LOAD -> Printf.printf "LOAD\n"
  | STORE -> Printf.printf "STORE\n"
  | LOADI -> Printf.printf "LOADI\n"
  | MOV -> Printf.printf "MOV\n"
  | JMP -> Printf.printf "JMP\n"
  | PUSH -> Printf.printf "PUSH\n"
  | POP -> Printf.printf "POP\n"
  | HALT -> Printf.printf "HALT\n"
  | INC -> Printf.printf "INC\n"
  | DEC -> Printf.printf "DEC\n"
  | JMPC _ -> Printf.printf "JMPC\n"
  | DATA -> Printf.printf "DATA\n"
  | TEXT -> Printf.printf "TEXT\n"
  | STRING -> Printf.printf "STRING\n"
  | ZSTRING -> Printf.printf "ZSTRING\n"
  | UINT -> Printf.printf "UINT\n"
  | INT -> Printf.printf "INT\n"
  | INCLUDE -> Printf.printf "INCLUDE\n"
  | MACRO -> Printf.printf "MACRO\n"
  | ENDMACRO -> Printf.printf "ENDMACRO\n"
  | R i -> Printf.printf "R %d\n" i
  | ROUT -> Printf.printf "ROUT\n"
  | SP -> Printf.printf "SP\n"
  | FP -> Printf.printf "FP\n"
  | RPRIV -> Printf.printf "RPRIV\n"
  | PLUS -> Printf.printf "PLUS\n"
  | LPAR -> Printf.printf "LPAR\n"
  | RPAR -> Printf.printf "RPAR\n"
  | COMMA -> Printf.printf "COMMA\n"
  | TXTCOL -> Printf.printf "TXTCOL\n"
  | BCKCOL -> Printf.printf "BCKCOL\n"
  | BOLD -> Printf.printf "BOLD\n"
  | FAINT -> Printf.printf "FAINT\n"
  | ITALIC -> Printf.printf "ITALIC\n"
  | UNDERLINE -> Printf.printf "UNDERLINE\n"
  | BLINKING -> Printf.printf "BLINKING\n"
  | HIDE -> Printf.printf "HIDE\n"
  | CROSSED -> Printf.printf "CROSSED\n"
  | OVERLINE -> Printf.printf "OVERLINE\n"
  | DEFAULT -> Printf.printf "DEFAULT\n"

let dump_tokens lb =
  let token = ref (PostLexer.next_token lb) in
  while !token <> EOF do
    dump_token !token;
    token := PostLexer.next_token lb;
  done;
  dump_token !token
