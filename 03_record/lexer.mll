{
open Parser
open Lexing

exception SyntaxError of string
}

let digit = ['0'-'9']
let int = digit | ['1'-'9'] digit*

let white = [' ''\t']+

let var = ['A'-'Z'] ['a'-'z''A'-'Z''0'-'9']* | ['+' '-' '*']
let label = ['a'-'z'] ['a'-'z''A'-'Z''0'-'9']*

rule read = parse
  | ',' { COMMA }
  | ':' { COLON }
  | "skip" { SKIP }
  | '=' { EQ }
  | "local" { LOCAL }
  | "in" { IN }
  | "end" { END }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | "proc" { PROC }
  | '$' { DOLLAR }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | label { LABEL (Lexing.lexeme lexbuf) }
  | white { read lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
