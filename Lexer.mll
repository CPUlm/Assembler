{
open Parser

let kw_tok  = [
  ("ADD",ADD);("SUB",SUB);("MUL",MUL);("DIV",DIV);
  ("AND",AND);("NOR",NOR);("XOR",XOR);("OR",OR);
  ("LSL",LSL);("ASR",ASR);("LSR";LSR);
  ("LOAD",LOAD);("STORE",STORE);("LOADI",LOADI);("LOADIA",LOADIA);
  ("JMP",JMP);("JMPC",JMPC);("JMPI",JMPI);("JMPIC";JMPIC);
  ("NOP",NOP);
  ("CALL",CALL);("RET",RET);
  ("PUSH",PUSH);("POP",POP);
  ("$",DOLLAR);(":",CLN);
  ("R",R);("ROUT",Rout);("SP",SP);("FP",FP)

]
let string_buffer = Buffer.create 16
let str_to_tok = Hashtbl.create 100

let () = List.iter (fun (x,y) -> Hashtbl.add str_to_tok x y) kw_tok
let tok_to_str = Hashtbl.create 100
let () = List.iter (fun (x,y) -> Hashtbl.add token_to_identtbl y x) kw_tok

}

let digit = ['0'-'9']
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let other = lower | upper | digit | '\''
let upperword = upper+
let dotdecl = '.' lower+
let integer = '0' | ['1'-'9'] digit*
let registre=  'R' integer
let label = (upper | lower ) (lower | upper | digit)*



rule gen_tokens = parse
  | '\n'            { Lexing.new_line lexbuf; END_INST }
  | ' ' | '\t'      { gen_tokens lexbuf }
  | eof             { EOF }
  | ';'             {line_comment lexbuf}
  | integer as i    {INT((int_of_string i))}
  | "0b" integer as i {INT (int_of_string i)} (* TODO *)
  | "0x" integer as i {INT (int_of_string i)} (* TODO*)
  | '"'             {string_lex lexbuf}
  | registre as r { 
    let rnum = String.sub r 1 (String.length rsub - 1) in
    R(int_of_string r)
  }
  | ".include" {
    raise (Lexing_error "TODO : include")
  }
  | dotdecl as d {
    match Hashtbl.find_opt str_to_tok d with
    | None -> raise (Lexing_error ("Unknown instruction " ^ d))
    | Some x -> x
  }
  | '$' {DOLLAR}
  | ':' {CLN}
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
    | "\\n"  {Buffer.add_char string_buffer '\n'; string_lex lexbuf }
    | "\\t"  {Buffer.add_char string_buffer '\t'; string_lex lexbuf }
    | "\\a"   {print_string "pas sûr pour \a"; Buffer.add_char string_buffer '\a'}
    | "\n"   {raise (Lexing_error "String non terminée avant de changer de ligne")}
    | _ as c {Buffer.add_char string_buffer c; string_lex lexbuf}
    | eof       {raise (Lexing_error "String non terminée")}

{ }