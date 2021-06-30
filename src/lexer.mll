{
open Lexing
open Parser
    
let on_newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    {
      pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

let parse_options = function
  | None -> []
  | Some o -> String.sub o 1 (String.length o-2) |> String.split_on_char ','
}

let space = ' ' | '\t' | '\r'
let newline = '\n'

rule token = parse
  | "\\deftwocell" { GEN }
  | "\\twocellopt" { OPT }
  | (['0'-'9']+ as n)":\\twocell"("["[^']']*"]" as o)? { CELL (int_of_string n, parse_options o) }
  | "=" { EQ }
  | ":" { COLON }
  | "," { COMMA }
  | "->" { TO }
  | "→" { TO }
  | "%"[^'\n']* { token lexbuf }
  | '{' { LACC }
  | '}' { RACC }
  | '(' { LPAR }
  | ')' { RPAR }
  | '[' { LBRA }
  | ']' { RBRA }
  | '*' { COMP }
  | "label" { LABEL }
  | "space"(['0'-'9''.']+ as n) { SPACE (float_of_string n) }
  | ('"'[^'"']*'"' as str) { STRING str }
  | ('-'?['0'-'9']+ as n) { INT (int_of_string n) }
  | ('-'?['0'-'9']*"."['0'-'9']+ as s) { STRING s } (* Floats are handled as strings for now *)
  | (['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''.''-''+''\'']* as str) { STRING str }
  | space+ { token lexbuf }
  | newline { on_newline lexbuf; token lexbuf }
  | eof { EOF }
