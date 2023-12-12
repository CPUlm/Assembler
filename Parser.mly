%{
    open Ast
    open Funcs

%}

%token ADD SUB MUL DIV
%token AND NOR XOR OR
%token LSL ASR LSR
%token LOAD STORE LOADI LOADIA (* LoadImmediate Add *) MOV
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
%token DOLLAR CLN (* : *)
%token EOF
%token   <int>R
%token Rout SP  FP


%start<Ast.file> file
%%


inst_sans_label:
    | AND i1=R i2=R i3=R {{v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | OR  i1=R i2=R i3=R {{v=Or(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | NOR i1=R i2=R i3=R {{v=Or(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | XOR i1=R i2=R i3=R {{v=Xor(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | NOT i1=R i2=R      {{v=Not(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $pos)}}
    | ADD i1=R i2=R i3=R {{v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | SUB i1=R i2=R i3=R {{v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | MUL i1=R i2=R i3=R {{v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | DIV i1=R i2=R i3=R {{v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | NEG i1=R i2=R      {{v=And(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | NOP                {Nop}
    | LSL i1=R i2=R i3=R {{v=ShiftLeftLogical(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | ASR i1=R i2=R i3=R {{v=ShiftRightArith(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | LSR i1=R i2=R i3=R {{v=ShiftRightLogical(int_to_reg i1, int_to_reg i2, int_to_reg i3);pos=(lexloc_to_pos $pos)}}
    | PUSH i1=R          {{v=Push(int_to_reg i1);pos=(lexloc_to_pos $pos)}}
    | POP i1=R           {{v=Pop(int_to_reg i1);pos=(lexloc_to_pos $pos)}}
    | LOAD i1=R i2=R     {{v=Load(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $pos)}}
    | STORE i1=R i2=R    {{v=Store(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $pos)}}
    | MOV i1=R i2=R      {{v=Mov(int_to_reg i1, int_to_reg i2);pos=(lexloc_to_pos $pos)}}
    

    



inst:
    | l=LBL CLN i = inst_sans_label {(Some l,i)}
    | i =  inst_sans_label {(None,i)}

file:
    | il = separated_list(END_INST,inst) {il}