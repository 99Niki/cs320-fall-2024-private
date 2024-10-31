%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token IF
%token THEN
%token ELSE
%token LET 
%token EQ
%token IN
%token FUN
%token ARROW
%token EOF
%token LBRACE
%token RBRACE
%token UNIT
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token LT
%token LTE
%token GT
%token GTE
%token NEQ
%token AND
%token OR

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | e=expr; EOF {e}

expr:
  | IF; e1=expr; THEN; e2=expr; ELSE; e3=expr {If(e1,e2,e3)}
  | LET; x=VAR; EQ; e1=expr; IN; e2=expr {Let(x,e1,e2)}
  | FUN; x= VAR; ARROW; e1=expr {Fun(x,e1)}
  | e1=expr2  {e1}

expr2:
  | e1=expr2; op=bop; e2=expr2 {Bop(op,e1,e2)}
  | e1=expr3; LBRACE; e2=expr3; RBRACE {App(e1,e2)}
  | e1=expr3; es=expr3* 
    {
      let rec helper acc lst =
        match lst with
        | [] -> acc
        | h::t -> helper (App(acc, h)) t
      in
      helper e1 es
    }


expr3:
  | UNIT {Unit}
  | TRUE {True}
  | FALSE {False}
  | n = NUM {Num n}
  | x = VAR {Var x}
  | LPAREN; e1=expr; RPAREN {e1}

%inline bop:
  | ADD {Add}
  | SUB {Sub}
  | MUL {Mul}
  | DIV {Div}
  | MOD {Mod}
  | LT  {Lt}
  | LTE {Lte}
  | GT  {Gt}
  | GTE {Gte}
  | EQ  {Eq}
  | NEQ {Neq}
  | AND {And}
  | OR  {Or}

