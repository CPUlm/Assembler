{
open Parser
}

let digit = ['0'-'9']
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let other = lower | upper | digit | '\''
let lindent = lower other*
let uindent = upper (other | '.')*
let integer = '0' | ['1'-'9'] digit*

rule gen_tokens = parse
  | '\n'            { Lexing.new_line lexbuf; gen_tokens lexbuf }
  | ' ' | '\t'      { gen_tokens lexbuf }
  | eof             { EOF }

{ }