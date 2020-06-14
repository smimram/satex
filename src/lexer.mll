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
}

let space = ' ' | '\t' | '\r'
let newline = '\n'

rule token = parse
  | "\\deftwocell" { GEN }
  | (['0'-'9']+ as n)":\\twocell" { CELL (int_of_string n) }
  | "=" { EQ }
  | ":" { COLON }
  | "," { COMMA }
  | "->" { TO }
  | "%"[^'\n']* { token lexbuf }
  | '{' { LACC }
  | '}' { RACC }
  | '(' { LPAR }
  | ')' { RPAR }
  | '[' { LBRA }
  | ']' { RBRA }
  | '*' { COMP }
  | "label" { LABEL }
  | (['a'-'z''A'-'Z''-''+']+ as str) { STRING str }
  | ('"'[^'"']*'"' as str) { STRING str }
  | (['0'-'9']+ as n) { INT (int_of_string n) }
  | space+ { token lexbuf }
  | newline { on_newline lexbuf; token lexbuf }
  | eof { EOF }
