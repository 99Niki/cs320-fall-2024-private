%{
open Utils
%}

%token LET
%token <string>var
%token EQUALS
%token IN
%token EOF

%start <Utils.prog> prog

%%

prog:
  | e=expr; EOF { e }


expr:
  | LET; x=var; EQUALS; e1=expr; IN; e2=expr {let(x,e1,e2)}
  | e = expr1{e}


expr1 :
  | e1 = expr1