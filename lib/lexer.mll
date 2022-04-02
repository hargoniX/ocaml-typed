{
open Parser
open Lexing

let choke _lexbuf s =
  invalid_arg s

exception Error of string
}

let digit = ['0'-'9']
let int = digit+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule lex =
  parse
  | white { lex lexbuf }
  | newline { new_line lexbuf; lex lexbuf }
  | int { INT (int_of_string (lexeme lexbuf)) }
  | "let" { LET }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "=" { EQ }
  | "int" { INTEGER }
  | "in" { IN }
  | "\\" { LAMBDA }
  | "=>" { ARROW }
  | ":" { COL }
  | "->" { FUNCTION }
  | id { ID (lexeme lexbuf) }
  | eof { EOF }
  | _ { choke lexbuf "Invalid syntax" }