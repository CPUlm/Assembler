%{
    open Ast
    open Funcs
    open PositionUtils

    let mk_inst loc inst =
        Some { v = inst; pos = lexloc_to_pos loc }
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
    | AND i1=R i2=R i3=R { mk_inst $loc (And(int_to_reg i1, int_to_reg i2, int_to_reg i3)) }
    | OR  i1=R i2=R i3=R { mk_inst $loc (Or(int_to_reg i1,  int_to_reg i2, int_to_reg i3)) }
    | NOR i1=R i2=R i3=R { mk_inst $loc (Or(int_to_reg i1,  int_to_reg i2, int_to_reg i3)) }
    | XOR i1=R i2=R i3=R { mk_inst $loc (Xor(int_to_reg i1, int_to_reg i2, int_to_reg i3)) }
    | NOT i1=R i2=R      { mk_inst $loc (Not(int_to_reg i1, int_to_reg i2)               ) }
    | ADD i1=R i2=R i3=R { mk_inst $loc (Add(int_to_reg i1, int_to_reg i2, int_to_reg i3)) }
    | SUB i1=R i2=R i3=R { mk_inst $loc (Sub(int_to_reg i1, int_to_reg i2, int_to_reg i3)) }
    | MUL i1=R i2=R i3=R { mk_inst $loc (Mul(int_to_reg i1, int_to_reg i2, int_to_reg i3)) }
    | DIV i1=R i2=R i3=R { mk_inst $loc (Div(int_to_reg i1, int_to_reg i2, int_to_reg i3)) }
    | NEG i1=R i2=R      { mk_inst $loc (Neg(int_to_reg i1, int_to_reg i2)) }
    | NOP                { mk_inst $loc (Nop) }
    | LSL i1=R i2=R i3=R { mk_inst $loc (ShiftLeftLogical(int_to_reg i1, int_to_reg i2, int_to_reg i3)) }
    | ASR i1=R i2=R i3=R { mk_inst $loc (ShiftRightArith(int_to_reg i1, int_to_reg i2, int_to_reg i3)) }
    | LSR i1=R i2=R i3=R { mk_inst $loc (ShiftRightLogical(int_to_reg i1, int_to_reg i2, int_to_reg i3)) }
    | PUSH i1=R          { mk_inst $loc (Push(int_to_reg i1)) }
    | POP i1=R           { mk_inst $loc (Pop(int_to_reg i1)) }
    | LOAD i1=R i2=R     { mk_inst $loc (Load(int_to_reg i1, int_to_reg i2)) }

    | LOADI i1=R i=IMM   { mk_inst $loc (LoadImmediate(int_to_reg i1,(int_to_pos $loc i),false)) }
    | LOADI i1=R l=LBL   { mk_inst $loc (LoadImmediateLabel(int_to_reg i1,label_to_pos $loc l,false)) }
    | LOADI i1=R i=IMM i2=R { mk_inst $loc (LoadImmediateAdd(int_to_reg i1,(int_to_pos $loc i),false,int_to_reg i2)) }
    | LOADI i1=R l=LBL i2=R { mk_inst $loc (LoadImmediateAddLabel(int_to_reg i1,(label_to_pos $loc l),false,int_to_reg i2)) }

    | LOADIH i1=R i=IMM   { mk_inst $loc (LoadImmediate(int_to_reg i1,(int_to_pos $loc i),true)) }
    | LOADIH i1=R l=LBL   { mk_inst $loc (LoadImmediateLabel(int_to_reg i1,label_to_pos $loc l,true)) }
    | LOADIH i1=R i=IMM i2=R { mk_inst $loc (LoadImmediateAdd(int_to_reg i1,(int_to_pos $loc i),true,int_to_reg i2)) }
    | LOADIH i1=R l=LBL i2=R { mk_inst $loc (LoadImmediateAddLabel(int_to_reg i1,label_to_pos $loc l,true,int_to_reg i2)) }

    | STORE i1=R i2=R    { mk_inst $loc (Store(int_to_reg i1, int_to_reg i2)) }
    | MOV i1=R i2=R      { mk_inst $loc (Mov(int_to_reg i1, int_to_reg i2)) }
    | TEST i1=R          { mk_inst $loc (Test(int_to_reg i1)) }
    | JMP i=IMM          { mk_inst $loc (JmpImmediate(int_to_pos $loc i)) }
    | JMP o=OFFS         { mk_inst $loc (JmpOffset (int_to_pos $loc o)) }
    | JMP l=LBL          { mk_inst $loc (JmpLabel (label_to_pos $loc l)) }
    | JMP i=R          { mk_inst $loc (JmpAddr (int_to_reg i)) }
    | JMP DOT f=flag  i=IMM { mk_inst $loc (JmpImmediateCond (f,(int_to_pos $loc i))) }
    | JMP DOT f=flag  o=OFFS { mk_inst $loc (JmpOffsetCond (f,(int_to_pos $loc o))) }
    | JMP DOT f=flag  l=LBL { mk_inst $loc (JmpLabelCond (f,label_to_pos $loc l)) }
    | JMP DOT f=flag  i=IMM { mk_inst $loc (JmpImmediateCond (f,(int_to_pos $loc i))) }
    | JMP DOT f=flag  i=R   { mk_inst $loc (JmpAddrCond (f,int_to_reg i)) }
    | HALT                  { mk_inst $loc Halt }
    | CALL i=R              { mk_inst $loc (CallAddr (int_to_reg i)) }
    | CALL l=LBL            { mk_inst $loc (CallLabel (label_to_pos $loc l)) }
    | RET                   { mk_inst $loc Ret }
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