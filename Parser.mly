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

%token   <int>R
%token Rout SP  FP


%start<Ast.file> file
%%


inst_sans_label:
    | AND i=R {Nop}

inst:
    | l=LBL CLN i = inst_sans_label {(Some l,i)}
    | i =  inst_sans_label {(None,i)}

file:
    | il = separated_list(END_INST,inst) {il}