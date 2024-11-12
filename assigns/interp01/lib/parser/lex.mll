{
open Par
exception SyntaxError of string
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | "if"       {IF}
  | "then"     {THEN}
  | "else"     {ELSE}
  | "let"      {LET}
  | "="        {EQ}
  | "in"       {IN}
  | "fun"      {FUN}
  | "->"       {ARROW}
  | "("        {LPAREN}
  | ")"        {RPAREN}
  | "+"        {ADD}
  | "-"        {SUB}
  | "*"        {MUL}
  | "/"        {DIV}
  | "mod"      {MOD}
  | "<"        {LT}
  | "<="       {LTE}
  | ">"        {GT}
  | ">="       {GTE}
  | "<>"       {NEQ}
  | "&&"       {AND}
  | "||"       {OR}
  | "()"       {UNIT}
  | "{"        {LBRACE}
  | "}"        {RBRACE}
  | "true"     {TRUE}
  | "false"    {FALSE}
  | num        { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var        { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
