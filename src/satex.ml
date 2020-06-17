let usage = "strit [options] file"

let parse f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  let line n =
    let ic = open_in f in
    for i = 0 to n - 2 do ignore (input_line ic) done;
    let ans =
      try input_line ic
      with _ -> "???"
    in
    close_in ic;
    ans
  in
  let decls =
    try
      Parser.decls Lexer.token lexbuf
    with
    | Parsing.Parse_error ->
      let pos = Lexing.lexeme_end_p lexbuf in
      let err =
        Printf.sprintf
          "Parse error at word \"%s\", line %d, character %d.\n%s"
          (Lexing.lexeme lexbuf)
          pos.Lexing.pos_lnum
          (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
          (line pos.Lexing.pos_lnum)
      in
      failwith err
    | Lang.Typing e ->
      let pos = Lexing.lexeme_end_p lexbuf in
      let err =
        Printf.sprintf
          "Typing error at word \"%s\", line %d, character %d: %s.\n%s"
          (Lexing.lexeme lexbuf)
          pos.Lexing.pos_lnum
          (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
          e
          (line pos.Lexing.pos_lnum)
      in
      failwith err
  in
  close_in ic;
  decls

let () =
  try
    let fname = ref "" in
    Arg.parse (Arg.align []) (fun s -> fname := s) usage;
    if !fname = "" then Common.error "Please provide a .satex file name as input.";
    let satix_fname = Filename.chop_extension !fname ^ ".satix" in
    let decls = parse !fname in
    Lang.draw satix_fname decls
  with
  | Failure e ->
    Printf.eprintf "%s\n%!" e;
    exit 1
