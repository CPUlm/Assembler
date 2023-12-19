{
open Parser

exception Lexing_error of string
let kw_tok  = [
  ("ADD",ADD);("SUB",SUB);("MUL",MUL);("DIV",DIV);
  ("AND",AND);("NOR",NOR);("XOR",XOR);("OR",OR);
  ("LSL",LSL);("ASR",ASR);("LSR",LSR);
  ("LOAD",LOAD);("STORE",STORE);("LOADI",LOADI);("LOADIA",LOADIA);
  ("JMP",JMP);("JMPC",JMPC);("JMPI",JMPI);("JMPIC",JMPIC);
  ("NOP",NOP); ("NEG",NEG); ("NOT",NOT);
  ("CALL",CALL);("RET",RET);
  ("PUSH",PUSH);("POP",POP);
  ("$",DOLLAR);(":",CLN);
  ("ROUT",Rout);("SP",SP);("FP",FP);
  ("test",TEST); ("halt",HALT);
  ("Z",FLG_Z);("N",FLG_N);("C",FLG_C);("Z",FLG_Z);
  (".ascii",ASCII);(".string",STRING);(".uint",UINT);(".int",INT);

]
let string_buffer = Buffer.create 16
let str_to_tok = Hashtbl.create 100

let () = List.iter (fun (x,y) -> Hashtbl.add str_to_tok x y) kw_tok
let tok_to_str = Hashtbl.create 100
let () = List.iter (fun (x,y) -> Hashtbl.add tok_to_str y x) kw_tok

}

let digit = ['0'-'9']
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let other = lower | upper | digit | '\''
let upperword = upper+
let directive = '.' lower+
let integer = '0' | ['1'-'9'] digit*
let register = 'R' integer | 'r' integer
let label = (upper | lower) (lower | upper | digit)*
let offset = '+' integer | '-' integer

rule gen_tokens = parse
  | '\n'            { Lexing.new_line lexbuf; END_INST}
  | ' ' | '\t'      { gen_tokens lexbuf }
  | eof             { EOF }
  | ';'             {line_comment lexbuf}
  | integer as i    {IMM((Int32.of_string i))}
  | "0b" integer as i {IMM (Int32.of_string i)} (* TODO *)
  | "0x" integer as i {IMM (Int32.of_string i)} (* TODO*)
  | '"'             {string_lex lexbuf}
  | register as r {
    (* let rnum = String.sub r 1 (String.length r - 1) in *) (* Unused var *)
    R(int_of_string r)
  }
  | ".include" {
    failwith "TODO : include"
  }
  | "loadi.h" {LOADIH}
  | directive as d {
    match Hashtbl.find_opt str_to_tok d with
    | None -> raise (Lexing_error ("Unknown instruction " ^ d))
    | Some x -> x
  }
  | '$' {DOLLAR}
  | ':' {CLN}
  | offset as o {OFFS (Int32.of_string o)}
  | upperword as u {
    match Hashtbl.find_opt str_to_tok u with
    | None -> LBL u
    | Some x -> x
  }
  | label as l {
    LBL l
  }

and line_comment = parse
  | '\n'            {gen_tokens lexbuf}
  | eof             {EOF}
  | _               {gen_tokens lexbuf}

and string_lex = parse
(* Est-ce qu'on gère la fin en \ ? Si oui j'ai le code, juste à l'ajouter. *)
    | '"' {
        let s = Buffer.contents string_buffer in
        Buffer.reset string_buffer;
        STR s
    }
    | "\\\"" {Buffer.add_char string_buffer '"'; string_lex lexbuf }
    (* No in the spec, we can't display \n or \t on out screen :/ *)
    (* | "\\n"  {Buffer.add_char string_buffer '\n'; string_lex lexbuf } *)
    (* | "\\t"  {Buffer.add_char string_buffer '\t'; string_lex lexbuf } *)
    (* | "\\a"   {print_string "pas sûr pour \a"; Buffer.add_char string_buffer '\a'} *)
    | "\n"   {raise (Lexing_error "String non terminée avant de changer de ligne")}
    | _ as c {Buffer.add_char string_buffer c; string_lex lexbuf}
    | eof       {raise (Lexing_error "String non terminée")}

{ }