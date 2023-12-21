%{
  open Ast
  open ParsingUtils
%}

%token EOF
%token <string> STR
%token <string> LBL
%token <string> IDENT
%token <int> IMM
%token COLON
%token <int> OFFS

(* Instructions *)
%token ADD SUB MUL DIV AND NOR XOR OR LSL ASR LSR NOT NOP CALL RET TEST NEG
%token LOAD STORE LOADI MOV JMP PUSH POP HALT INC DEC
%token <Ast.flag> JMPC

(* Directives *)
%token DATA TEXT STRING ZSTRING INT

(* Registers *)
%token <int> R
%token ROUT SP FP RPRIV

(* String instructions *)
%token PLUS
%token LPAR RPAR COMMA TXTCOL BCKCOL BOLD FAINT ITALIC
%token UNDERLINE BLINKING HIDE CROSSED OVERLINE DEFAULT
%left PLUS

%start<Ast.file> file
%%

%inline reg:
  | ROUT  { ROut }
  | SP    { SP }
  | FP    { FP }
  | RPRIV { PrivateReg }
  | r=R   { int_to_reg $loc r }

%inline imm: i=IMM   { mk_imm $loc i }
%inline lbl: l=LBL   { mk_pos $loc l }
%inline offs: o=OFFS { mk_offset $loc o }

inst_without_label:
  | AND r1=reg r2=reg r3=reg   { mk_pos $loc (And (r1, r2, r3)) }
  | OR  r1=reg r2=reg r3=reg   { mk_pos $loc (Or (r1,  r2, r3)) }
  | NOR r1=reg r2=reg r3=reg   { mk_pos $loc (Or (r1,  r2, r3)) }
  | XOR r1=reg r2=reg r3=reg   { mk_pos $loc (Xor (r1, r2, r3)) }
  | NOT r1=reg r2=reg          { mk_pos $loc (Not (r1, r2)) }
  | ADD r1=reg r2=reg r3=reg   { mk_pos $loc (Add (r1, r2, r3)) }
  | SUB r1=reg r2=reg r3=reg   { mk_pos $loc (Sub (r1, r2, r3)) }
  | MUL r1=reg r2=reg r3=reg   { mk_pos $loc (Mul (r1, r2, r3)) }
  | DIV r1=reg r2=reg r3=reg   { mk_pos $loc (Div (r1, r2, r3)) }
  | NEG r1=reg r2=reg          { mk_pos $loc (Neg (r1, r2)) }
  | INC r1=reg r2=reg          { mk_pos $loc (Incr (r1, r2)) }
  | DEC r1=reg r2=reg          { mk_pos $loc (Decr (r1, r2)) }
  | NOP                        { mk_pos $loc (Nop) }
  | LSL r1=reg r2=reg r3=reg   { mk_pos $loc (ShiftLeftLogical (r1, r2, r3)) }
  | ASR r1=reg r2=reg r3=reg   { mk_pos $loc (ShiftRightArith (r1, r2, r3)) }
  | LSR r1=reg r2=reg r3=reg   { mk_pos $loc (ShiftRightLogical (r1, r2, r3)) }
  | PUSH r=reg                 { mk_pos $loc (Push r) }
  | POP r=reg                  { mk_pos $loc (Pop r) }
  | LOAD r1=reg r2=reg         { mk_pos $loc (Load (r1, r2)) }
  | LOADI r1=reg i=imm         { mk_pos $loc (LoadImmediate (r1, i)) }
  | LOADI r1=reg l=lbl         { mk_pos $loc (LoadImmediateLabel (r1, l)) }
  | LOADI r1=reg i=imm r2=reg  { mk_pos $loc (LoadImmediateAdd (r1, i, r2)) }
  | LOADI r1=reg l=lbl r2=reg  { mk_pos $loc (LoadImmediateAddLabel (r1, l, r2)) }
  | STORE r1=reg r2=reg        { mk_pos $loc (Store (r1, r2)) }
  | MOV r1=reg r2=reg          { mk_pos $loc (Mov (r1, r2)) }
  | TEST r=reg                 { mk_pos $loc (Test r) }
  | JMP i=imm                  { mk_pos $loc (JmpImmediate i) }
  | JMP o=offs                 { mk_pos $loc (JmpOffset o) }
  | JMP l=lbl                  { mk_pos $loc (JmpLabel l) }
  | JMP r=reg                  { mk_pos $loc (JmpAddr r) }
  | f=JMPC i=imm               { mk_pos $loc (JmpImmediateCond (f, i)) }
  | f=JMPC o=offs              { mk_pos $loc (JmpOffsetCond (f, o)) }
  | f=JMPC l=lbl               { mk_pos $loc (JmpLabelCond (f, l)) }
  | f=JMPC r=reg               { mk_pos $loc (JmpAddrCond (f, r)) }
  | HALT                       { mk_pos $loc Halt }
  | CALL r=reg                 { mk_pos $loc (CallAddr r) }
  | CALL l=lbl                 { mk_pos $loc (CallLabel l) }
  | RET                        { mk_pos $loc Ret }

inst:
  | l=IDENT COLON i = inst_without_label { (Some (mk_pos $loc(l) l), i) }
  | i = inst_without_label { (None, i) }

color:
  | i=IDENT | i=STR { str_to_col $loc i }

styled_string:
  | s=STR { (Text s) }
  | t1=styled_string PLUS t2=styled_string {
      (Concat (t1,t2))
  }
  | TXTCOL LPAR c=color COMMA t=styled_string RPAR {
      (TextColor (c, t))
  }
  | BCKCOL LPAR c=color COMMA t=styled_string RPAR {
      (BackColor (c, t))
  }
  | BOLD LPAR t=styled_string RPAR {
      (Style (Bold,t))
  }
  | FAINT LPAR t=styled_string RPAR {
      (Style (Faint,t))
  }
  | BLINKING LPAR t=styled_string RPAR {
      (Style (Blinking,t))
  }
  | ITALIC LPAR t=styled_string RPAR {
      (Style (Italic,t))
  }
  | UNDERLINE LPAR t=styled_string RPAR {
      (Style (Underline,t))
  }
  | HIDE LPAR t=styled_string RPAR {
      (Style (Hide,t))
  }
  | CROSSED LPAR t=styled_string RPAR {
      (Style (Crossed,t))
  }
  | OVERLINE LPAR t=styled_string RPAR {
      (Style (Overline,t))
  }
  | DEFAULT LPAR t=styled_string RPAR {
      (Style (Default,t))
  }

data_without_label:
  | STRING s=styled_string  { mk_pos $loc (Str s) }
  | ZSTRING s=styled_string { mk_pos $loc (Str (Concat (s, Text "\000"))) }
  | INT i=imm               { mk_pos $loc (Int i) }

data:
  | l=IDENT COLON d = data_without_label { (Some (mk_pos $loc(l) l), d) }
  | d=data_without_label { (None, d) }

text_section:
  | i=inst s=text_section
    { (Either.Left i) :: s }
  |
    { [] }
  | DATA d=data_section
    { d }

data_section:
  | i=data s=data_section
    { (Either.Right i) :: s }
  |
    { [] }
  | TEXT d=text_section
    { d }

sections:
  | TEXT s=text_section
    { s }
  | DATA s=data_section
    { s }

file:
  | s=sections EOF {
      let text, data = List.partition_map (fun i -> i) s in
      { text; data }
  }