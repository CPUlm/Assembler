%{
    open Ast
    open Funcs

%}

%token ADD SUB MUL DIV
%token AND NOR XOR OR
%token LSL ASR LSR
%token LOAD STORE LOADI MOV LOADIH
%token JMP JMPC JMPI JMPIC
%token NOP
%token CALL RET
%token PUSH POP
%token DATA TEXT
%token STRING UINT INT
%token END_INST
%token <string>STR
%token <string>LBL
%token <int>IMM
%token DOLLAR CLN (* : *) DOT
%token EOF
%token   <int>R
%token Rout SP  FP
%token FLG_Z FLG_N FLG_C FLG_V
%token TEST
%token <int>OFFS
%token PLUS MOINS 
%token HALT
%token ASCII STRING UINT INT
%start<Ast.file> file
%%

flag:
    | FLG_C {UnsignedUnderflowFlag}
    | FLG_Z {Zero}
    | FLG_V {SignedOverflowFlag}
    | FLG_N {Negative}

inst_sans_label:
    | AND i1=R i2=R i3=R {Some {v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | OR  i1=R i2=R i3=R {Some {v=Or(int_to_reg i1,  int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | NOR i1=R i2=R i3=R {Some {v=Or(int_to_reg i1,  int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | XOR i1=R i2=R i3=R {Some {v=Xor(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | NOT i1=R i2=R      {Some {v=Not(int_to_reg i1, int_to_reg i2)               ;pos=(lexloc_to_pos $pos)}}
    | ADD i1=R i2=R i3=R {Some {v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | SUB i1=R i2=R i3=R {Some {v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | MUL i1=R i2=R i3=R {Some {v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | DIV i1=R i2=R i3=R {Some {v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | NEG i1=R i2=R      {Some {v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | NOP                {Some {v=Nop;pos=(lexloc_to_pos $pos)}}
    | LSL i1=R i2=R i3=R {Some {v=ShiftLeftLogical(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | ASR i1=R i2=R i3=R {Some {v=ShiftRightArith(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | LSR i1=R i2=R i3=R {Some {v=ShiftRightLogical(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | PUSH i1=R          {Some {v=Push(int_to_reg i1);pos=(lexloc_to_pos $pos)}}
    | POP i1=R           {Some {v=Pop(int_to_reg i1);pos=(lexloc_to_pos $pos)}}
    | LOAD i1=R i2=R     {Some {v=Load(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $pos)}}

    | LOADI i1=R i=IMM   {Some {v=LoadImmediate(int_to_reg i1,i,false);pos = (lexloc_to_pos $pos)}}
    | LOADI i1=R l=LBL   {Some {v=LoadImmediateLabel(int_to_reg i1,label_to_pos $pos l,false); pos = (lexloc_to_pos $pos)}}
    | LOADI i1=R i=INT i2=R {Some {v=LoadImmediateAdd(int_to_reg i1,i,false,int_to_reg i2); pos = (lexloc_to_pos $pos)}}
    | LOADI i1=R l=LBL i2=R {Some {v=LoadImmediateAddLabel(int_to_reg i1,l,false,int_to_reg i2); pos = (lexloc_to_pos $pos)}}

    | LOADIH i1=R i=IMM   {Some {v=LoadImmediate(int_to_reg i1,i,true);pos = (lexloc_to_pos $pos)}}
    | LOADIH i1=R l=LBL   {Some {v=LoadImmediateLabel(int_to_reg i1,label_to_pos $pos l,true); pos = (lexloc_to_pos $pos)}}
    | LOADIH i1=R i=INT i2=R {Some {v=LoadImmediateAdd(int_to_reg i1,i,true,int_to_reg i2); pos = (lexloc_to_pos $pos)}}
    | LOADIH i1=R l=LBL i2=R {Some {v=LoadImmediateAddLabel(int_to_reg i1,label_to_pos $pos l,true,int_to_reg i2); pos = (lexloc_to_pos $pos)}}

    | STORE i1=R i2=R    {Some {v=Store(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $pos)}}
    | MOV i1=R i2=R      {Some {v=Mov(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $pos)}}
    | TEST i1=R          {Some {v=Test(int_to_reg i1);pos=(lexloc_to_pos $pos)}}
    | JMP i=IMM          {Some {v=JmpImmediate(i);pos=(lexloc_to_pos $pos)}}
    | JMP o=OFFS         {Some {v=JmpOffset (i);pos=(lexloc_to_pos $pos)}}
    | JMP l=LBL          {Some {v=JmpLabel (label_to_pos $pos l); pos=(lexloc_to_pos $pos)}}
    | JMP i=R          {Some {v=JmpAddr (int_to_reg i); pos=(lexloc_to_pos $pos)}}
    | JMP DOT f=flag  i=IMM {Some {v=JmpImmediateCond (f,i);pos=(lexloc_to_pos $pos)}}
    | JMP DOT f=flag  o=OFFS {Some {v=JmpOffsetCond (f,o);pos=(lexloc_to_pos $pos)}}
    | JMP DOT f=flag  l=LBL {Some {v=JmpLabelCond (f,label_to_pos $pos l);pos=(lexloc_to_pos $pos)}}
    | JMP DOT f=flag  i=IMM {Some {v=JmpImmediateCond (f,i);pos=(lexloc_to_pos $pos)}}
    | JMP DOT f=flag  i=R   {Some {v=JmpAddrCond (f,int_to_reg i); pos=(lexloc_to_pos $pos)}}
    | HALT                  {Some {v=Halt; pos = (lexloc_to_pos $pos)}}
    | CALL i=R              {Some {v=(CallAddr (int_to_reg i));pos = (lexloc_to_pos $pos)}}
    | CALL l=LBL            {Some {v=(CallLabel (label_to_pos $pos l));pos = (lexloc_to_pos $pos)}}
    | RET                   {Some {v=Ret; pos= (lexloc_to_pos $pos)}}
    | TEXT                  {None}
    

    



inst:
    | l=LBL CLN i = inst_sans_label {(Some l,i)}
    | i =  inst_sans_label {(None,i)}

data_sans_label:
    | ASCII s=STR {{v=(Text s);pos=(lexloc_to_pos $pos)}}}
    | STRING s=STR {{v=(Text s);pos=(lexloc_to_pos $pos)}}
    | UINT u=INT {{v=(UInt u);pos=(lexloc_to_pos $pos)}}
    | INT u=INT {{v=(Int u);pos=(lexloc_to_pos $pos)}}

data:
    | l=LBL CLN d = data_sans_label {(Some l, d)}
    | d=data_sans_label {(None,d)}

un_cycle:
| TEXT l1=separated_list(END_INST,inst) DATA l2=separated_list(END_INST,data) {l1,l2}
file:
    | l=un_cycle* {
        format_file l
    }