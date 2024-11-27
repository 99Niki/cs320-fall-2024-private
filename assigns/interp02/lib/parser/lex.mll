{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | "()" { UNIT }
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "mod" { MOD }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "=" { EQ }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "if" { IF }
  | ":" {COLON}
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | "fun" { FUN }
  | "->" { ARROW }
  | "int" {INTTY}
  | "bool" {BOOLTY}
  | "unit" {UNITTY}
  | "assert" {ASSERT}
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }