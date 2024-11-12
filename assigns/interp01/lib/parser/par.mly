%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token EOF
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token LET "let"
%token EQ "="
%token IN "in"
%token FUN "fun"
%token ARROW "->"
%token LBRACE "("
%token RBRACE ")"
%token UNIT "()"
%token TRUE "true"
%token FALSE "false"
%token LPAREN "{"
%token RPAREN "}"
%token ADD "add"
%token SUB "-"
%token MUL "*"
%token DIV "/"
%token MOD "mod"
%token LT "<"
%token LTE "<="
%token GT ">"
%token GTE ">="
%token NEQ "<>"
%token AND "and"
%token OR "or"

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD
(*function application ->left*)

%start <Utils.prog> prog

%%

prog:
  | lets = toplet* EOF {lets}

toplet:
  | "let" x = VAR "=" e = expr { (x, e) }

expr:
  | "if" e1=expr "then" e2=expr "else" e3=expr {If(e1,e2,e3)}
  | "let" x = VAR "=" e1 = expr "in" e2 = expr {Let(x,e1,e2)}
  | "fun" x = VAR "->" e = expr {Fun(x,e)}
  | e1 = expr {e1}


expr2:
  | e1=expr2; op=bop; e2=expr2 {Bop(op,e1,e2)}
  | e1=expr3 "{" e2=expr3 "}" {App(e1,e2)}

expr3:
  | "()" {Unit}
  | "true" {True}
  | "false" {False}
  | n = NUM {Num n}
  | x = VAR {Var x}
  | "(" e1=expr ")"{e1}

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

