%{
  open Utils
  let rec mk_app e = function
  | [] -> e
  | x :: es -> mk_app (SApp (e, x)) es
%}

%token EOF
%token <int> NUM
%token <string> VAR

%token LET "let"
%token COLON ":"
%token EQ "="
%token REC "rec"
%token LPAREN "("
%token RPAREN ")"
%token ARROW "->"
%token IN "in"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token FUN "fun"
%token ASSERT "assert"
%token UNIT "()"
%token TRUE "true"
%token FALSE "false"

%token LT "<"
%token LTE "<="
%token GT ">"
%token GTE ">="
%token NEQ "<>"
%token AND "&&"
%token OR "||"
%token ADD "+"
%token SUB "-"
%token MUL "*"
%token DIV "/"
%token MOD "mod"

%token INTTY "int"
%token BOOLTY "bool"
%token UNITTY "unit"

%right ARROW
%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | lets = toplet* EOF {lets}

toplet:
  |"let" v = VAR  alst = arg* ":" t=ty "=" e = expr
    {{is_rec=false; name =v; args =alst; ty=t; value =e}}
  | "let" "rec" v=VAR a = arg alst=arg* ":" t=ty "="e = expr
    {{is_rec=false; name =v; args =a::alst; ty=t; value =e}}

arg:
  | "(" v=VAR t=ty ")" {(v,t)}

ty:
  | "int" {IntTy}
  | "bool" {BoolTy}
  | "unit" {UnitTy}
  | t1=ty "->" t2=ty {FunTy(t1,t2)}
  | "(" t=ty ")" {t}

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }

expr:
  | "let" v=VAR alst=arg* ":" t = ty "=" e1=expr "in" e2= expr 
    {SLet{is_rec=false; name=v; args=alst; ty=t; value=e1; body=e2}}
  |  "let" "rec" v=VAR a=arg alst=arg* ":" t = ty "=" e1=expr "in" e2= expr
    {SLet{is_rec=true; name=v; args=a::alst; ty=t; value=e1; body=e2}}
  | "if" e1=expr "then" e2=expr "else" e3=expr {SIf(e1,e2,e3)}
  | "fun" a=arg alst=arg* "->" e=expr {SFun{arg=a; args=alst; body=e}}
  | e=expr2 {e}

expr2:
  | e1=expr2 b=bop e2=expr2 {SBop(b,e1,e2)}
  | "assert" e=expr3 {SAssert(e)}
  | e1=expr3 es=expr3* { mk_app e1 es}

expr3:
  | "()" {SUnit}
  | "true" {STrue}
  | "false" {SFalse}
  | n=NUM {SNum n}
  | v=VAR {SVar v}
  | "(" e=expr ")" {e}
