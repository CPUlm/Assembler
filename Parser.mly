%{
    open Ast
    open Funcs
    open PositionUtils
%}

%token ADD SUB MUL DIV
%token AND NOR XOR OR
%token LSL ASR LSR
%token NOT
%token LOAD STORE LOADI MOV LOADIH LOADIA
%token JMP JMPC JMPI JMPIC
%token NOP
%token CALL RET
%token PUSH POP
%token DATA TEXT
%token END_INST
%token <string> STR
%token <string> LBL
%token <int32> IMM
%token DOLLAR CLN (* : *) DOT
%token EOF
%token <int> R
%token Rout SP FP
%token FLG_Z FLG_N FLG_C FLG_V
%token TEST
%token <int32> OFFS
%token PLUS MOINS NEG
%token HALT
%token ASCII STRING UINT INT

%start<Ast.file> file
%%

flag:
    | FLG_C {UnsignedUnderflowFlag}
    | FLG_Z {Zero}
    | FLG_V {SignedOverflowFlag}
    | FLG_N {Negative}

inst_without_label:
    | AND i1=R i2=R i3=R {Some {v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | OR  i1=R i2=R i3=R {Some {v=Or(int_to_reg i1,  int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | NOR i1=R i2=R i3=R {Some {v=Or(int_to_reg i1,  int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | XOR i1=R i2=R i3=R {Some {v=Xor(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | NOT i1=R i2=R      {Some {v=Not(int_to_reg i1, int_to_reg i2)               ;pos=(lexloc_to_pos $loc)}}
    | ADD i1=R i2=R i3=R {Some {v=Add(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | SUB i1=R i2=R i3=R {Some {v=Sub(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | MUL i1=R i2=R i3=R {Some {v=Mul(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | DIV i1=R i2=R i3=R {Some {v=Div(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | NEG i1=R i2=R      {Some {v=Neg(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $loc)}}
    | NOP                {Some {v=Nop;pos=(lexloc_to_pos $loc)}}
    | LSL i1=R i2=R i3=R {Some {v=ShiftLeftLogical(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | ASR i1=R i2=R i3=R {Some {v=ShiftRightArith(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | LSR i1=R i2=R i3=R {Some {v=ShiftRightLogical(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $loc)}}
    | PUSH i1=R          {Some {v=Push(int_to_reg i1);pos=(lexloc_to_pos $loc)}}
    | POP i1=R           {Some {v=Pop(int_to_reg i1);pos=(lexloc_to_pos $loc)}}
    | LOAD i1=R i2=R     {Some {v=Load(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $loc)}}

    | LOADI i1=R i=IMM   {Some {v=LoadImmediate(int_to_reg i1,(int_to_pos $loc i),false);pos = (lexloc_to_pos $loc)}}
    | LOADI i1=R l=LBL   {Some {v=LoadImmediateLabel(int_to_reg i1,label_to_pos $loc l,false); pos = (lexloc_to_pos $loc)}}
    | LOADI i1=R i=IMM i2=R {Some {v=LoadImmediateAdd(int_to_reg i1,(int_to_pos $loc i),false,int_to_reg i2); pos = (lexloc_to_pos $loc)}}
    | LOADI i1=R l=LBL i2=R {Some {v=LoadImmediateAddLabel(int_to_reg i1,(label_to_pos $loc l),false,int_to_reg i2); pos = (lexloc_to_pos $loc)}}

    | LOADIH i1=R i=IMM   {Some {v=LoadImmediate(int_to_reg i1,(int_to_pos $loc i),true);pos = (lexloc_to_pos $loc)}}
    | LOADIH i1=R l=LBL   {Some {v=LoadImmediateLabel(int_to_reg i1,label_to_pos $loc l,true); pos = (lexloc_to_pos $loc)}}
    | LOADIH i1=R i=IMM i2=R {Some {v=LoadImmediateAdd(int_to_reg i1,(int_to_pos $loc i),true,int_to_reg i2); pos = (lexloc_to_pos $loc)}}
    | LOADIH i1=R l=LBL i2=R {Some {v=LoadImmediateAddLabel(int_to_reg i1,label_to_pos $loc l,true,int_to_reg i2); pos = (lexloc_to_pos $loc)}}

    | STORE i1=R i2=R    {Some {v=Store(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $loc)}}
    | MOV i1=R i2=R      {Some {v=Mov(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $loc)}}
    | TEST i1=R          {Some {v=Test(int_to_reg i1);pos=(lexloc_to_pos $loc)}}
    | JMP i=IMM          {Some {v=JmpImmediate(int_to_pos $loc i);pos=(lexloc_to_pos $loc)}}
    | JMP o=OFFS         {Some {v=JmpOffset (int_to_pos $loc o);pos=(lexloc_to_pos $loc)}}
    | JMP l=LBL          {Some {v=JmpLabel (label_to_pos $loc l); pos=(lexloc_to_pos $loc)}}
    | JMP i=R          {Some {v=JmpAddr (int_to_reg i); pos=(lexloc_to_pos $loc)}}
    | JMP DOT f=flag  i=IMM {Some {v=JmpImmediateCond (f,(int_to_pos $loc i));pos=(lexloc_to_pos $loc)}}
    | JMP DOT f=flag  o=OFFS {Some {v=JmpOffsetCond (f,(int_to_pos $loc o));pos=(lexloc_to_pos $loc)}}
    | JMP DOT f=flag  l=LBL {Some {v=JmpLabelCond (f,label_to_pos $loc l);pos=(lexloc_to_pos $loc)}}
    | JMP DOT f=flag  i=IMM {Some {v=JmpImmediateCond (f,(int_to_pos $loc i));pos=(lexloc_to_pos $loc)}}
    | JMP DOT f=flag  i=R   {Some {v=JmpAddrCond (f,int_to_reg i); pos=(lexloc_to_pos $loc)}}
    | HALT                  {Some {v=Halt; pos = (lexloc_to_pos $loc)}}
    | CALL i=R              {Some {v=(CallAddr (int_to_reg i));pos = (lexloc_to_pos $loc)}}
    | CALL l=LBL            {Some {v=(CallLabel (label_to_pos $loc l));pos = (lexloc_to_pos $loc)}}
    | RET                   {Some {v=Ret; pos=(lexloc_to_pos $loc)}}
    | TEXT                  {None}

inst:
    | l=LBL CLN i = inst_without_label {(Some (label_to_pos $loc l),i)}
    | i =  inst_without_label {(None,i)}

data_without_label:
    | ASCII s=STR {{v=(Ascii ({v=Text s; pos=(lexloc_to_pos $loc)}));pos=(lexloc_to_pos $loc)}}
    | STRING s=STR {{v=(Str ({v=Text s; pos=(lexloc_to_pos $loc)}));pos=(lexloc_to_pos $loc)}}
    | INT u=IMM | UINT u=IMM {{v=(Int (int_to_pos $loc u));pos=(lexloc_to_pos $loc)}}

data:
    | l=LBL CLN d = data_without_label {(Some (label_to_pos $loc l), d)}
    | d=data_without_label {(None,d)}

un_cycle:
    | TEXT l1=separated_list(END_INST,inst) DATA l2=separated_list(END_INST,data) {l1,l2}

file:
    | l=un_cycle* {
        format_file l
    }