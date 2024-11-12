{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | whitespace    { read lexbuf }
  | "if"         { IF }
  | "then"       { THEN }
  | "else"       { ELSE }
  | "let"        { LET }
  | "rec"        { REC }
  | "="          { EQ }
  | "in"         { IN }
  | "fun"        { FUN }
  | "->"         { ARROW }
  | "("          { LPAREN }
  | ")"          { RPAREN }
  | "()"         { UNIT }
  | "true"       { TRUE }
  | "false"      { FALSE }
  | "+"          { ADD }
  | "-"          { SUB }
  | "*"          { MUL }
  | "/"          { DIV }
  | "mod"        { MOD }
  | "<"          { LT }
  | "<="         { LTE }
  | ">"          { GT }
  | ">="         { GTE }
  | "<>"         { NEQ }
  | "&&"         { AND }
  | "||"         { OR }
  | num as n     { NUM (int_of_string n) }
  | var as x     { VAR x }
  | eof          { EOF }
  | _            { raise (Failure "Unexpected character") }
