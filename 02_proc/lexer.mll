{
open Parser
open Lexing

exception SyntaxError of string
}

let digit = ['0'-'9']
let int = digit | ['1'-'9'] digit*

let white = [' ''\t']+

let id = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9']* | ['+' '-' '*']

rule read = parse
  | ',' { COMMA }
  | "skip" { SKIP }
  | '=' { EQ }
  | "local" { LOCAL }
  | "in" { IN }
  | "end" { END }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | white { read lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
