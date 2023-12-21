{
open Parser

exception Lexing_error of string

let string_buffer = Buffer.create 128

let resolve_instruction =
  let instructions = Hashtbl.create 37 in
  List.iter (fun (s, l) -> Hashtbl.add instructions s l)
        (
        [
          ("add",ADD);
          ("sub",SUB);
          ("mul",MUL);
          ("div",DIV);
          ("and",AND);
          ("nor",NOR);
          ("xor",XOR);
          ("or",OR);
          ("lsl",LSL);
          ("asr",ASR);
          ("lsr",LSR);
          ("load",LOAD);
          ("store",STORE);
          ("loadi",LOADI);
          ("jmp",JMP);
          ("jmp.z",JMPC Ast.Zero);
          ("jmp.n",JMPC Ast.Negative);
          ("jmp.c",JMPC Ast.UnsignedUnderflowFlag);
          ("jmp.v",JMPC Ast.SignedOverflowFlag);
          ("nop",NOP);
          ("neg",NEG);
          ("not",NOT);
          ("call",CALL);
          ("ret",RET);
          ("push",PUSH);
          ("pop",POP);
          ("rout",ROUT);
          ("sp",SP);
          ("fp",FP);
          ("rpriv",RPRIV);
          ("test",TEST);
          ("halt",HALT);
          ("inc",INC);
          ("dec",DEC);
          ("mov",MOV);
        ]
        );
  fun s ->
    try
      (* We canonicalize identifiers to lowercase because instructions names
          are case insensitive (that is ADD and add are the same). *)
      Hashtbl.find instructions (String.lowercase_ascii s)
    with Not_found -> IDENT s

let resolve_directive =
  let directives = Hashtbl.create 6 in
  List.iter (fun (s, l) -> Hashtbl.add directives s l)
        (
        [
          (".text", TEXT);
          (".data", DATA);
          (".string", STRING);
          (".zstring", ZSTRING);
          (".uint", UINT);
          (".int", INT);
          (".include",INCLUDE)
        ]
        );
  fun s -> try Hashtbl.find directives s with Not_found -> raise (Lexing_error ("Unknown directive " ^ s))

let resolve_text_inst =
  let text_instructions = Hashtbl.create 11 in
  List.iter (fun (s, l) -> Hashtbl.add text_instructions s l)
        (
        [
          ("#textcolor",TXTCOL);
          ("#backcolor",BCKCOL);
          ("#bold",BOLD);
          ("#faint",FAINT);
          ("#italic",ITALIC);
          ("#underline",UNDERLINE);
          ("#blinking",BLINKING);
          ("#hide",HIDE);
          ("#crossed",CROSSED);
          ("#overlined",OVERLINE);
          ("#default",DEFAULT);
        ]
        );
  fun s -> try Hashtbl.find text_instructions s with Not_found -> raise (Lexing_error ("Unknown text instruction " ^ s))
}

let eol = '\n' | '\r' '\n' | '\r'
let digit = ['0'-'9']
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let directive = '.' lower+
let dec_integer = '0' | ['1'-'9'] digit*
let bin_integer = "0b" ('0' | '1')+
let hex_integer = "0x" (['0'-'9' 'a'-'f' 'A'-'F' ])+
let integer = dec_integer | bin_integer | hex_integer
let register = ('R' | 'r') dec_integer
let identifier = (upper | lower) (lower | upper | digit | '.')*
let offset = ('+' | '-') dec_integer
let label = "$" identifier
let text_inst = '#' identifier
let space = ' '+

rule next_token = parse
  | eol { Lexing.new_line lexbuf; next_token lexbuf }
  | ' ' | '\t' { next_token lexbuf }
  | eof { EOF }
  | ';' { line_comment lexbuf }
  | ',' { COMMA }
  | '+' { PLUS }
  | ':' { COLON }
  | '(' { LPAR }
  | ')' { RPAR}
  | '"' { string_lex lexbuf }
  | integer as i
    { IMM (int_of_string i) }

  | directive as d
    { resolve_directive d }

  | label as l
    {
      let name = String.sub l 1 (String.length l - 1) in
      LBL name
    }

  | offset as o
    { OFFS (int_of_string o) }

  | register as r
    {
      let rnum = String.sub r 1 (String.length r - 1) in
      R(int_of_string rnum)
    }

  | identifier as i
    { resolve_instruction i }

  | text_inst as i
    { resolve_text_inst i }

  | ['\x20'-'\x7E'] as c
    {
      let msg = Format.sprintf "Illegal character '%c'in code." c in
      raise (Lexing_error msg)
    }

  | _ as c
    {
      let msg = Format.sprintf "Illegal character '\\x%x'in code." (Char.code c) in
      raise (Lexing_error msg)
    }

and line_comment = parse
  | eol
    { Lexing.new_line lexbuf; next_token lexbuf }

  | eof
    { EOF }

  | _
    { line_comment lexbuf }

and string_lex = parse
    | '"'
      {
        let s = Buffer.contents string_buffer in
        Buffer.reset string_buffer;
        STR s
      }

    | "\\\""
      { Buffer.add_char string_buffer '"'; string_lex lexbuf }

    | "\\\\"
      { Buffer.add_char string_buffer '\\'; string_lex lexbuf }

    | "\\0"
      { Buffer.add_char string_buffer '\x00'; string_lex lexbuf }

    | "\\" _ as e
      { raise (Lexing_error ("Invalid escape sequence '" ^ e ^ "' in string.")) }

    | "\n" | eof
      { raise (Lexing_error "Unterminated string.") }

    | [^'\x20'-'\x7E'] as c
      {
        let msg = Format.sprintf "Non printable character '\\x%x' in string." (Char.code c) in
        raise (Lexing_error msg)
      }

    | _ as c
      { Buffer.add_char string_buffer c; string_lex lexbuf }

{ 

}