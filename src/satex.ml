let usage = "strit [options] file"

let parse f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  let decls =
    try
      Parser.decls Lexer.token lexbuf
    with
    | Parsing.Parse_error ->
      let pos = Lexing.lexeme_end_p lexbuf in
      let err =
        Printf.sprintf
          "Parse error at word \"%s\", line %d, character %d."
          (Lexing.lexeme lexbuf)
          pos.Lexing.pos_lnum
          (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
      in
      failwith err
    | Lang.Typing e ->
      let pos = Lexing.lexeme_end_p lexbuf in
      let err =
        Printf.sprintf
          "Typing error at word \"%s\", line %d, character %d: %s."
          (Lexing.lexeme lexbuf)
          pos.Lexing.pos_lnum
          (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
          e
      in
      failwith err
  in
  close_in ic;
  decls

let () =
  let fname = ref "" in
  Arg.parse (Arg.align []) (fun s -> fname := s) usage;
  let decls = parse !fname in
  ()