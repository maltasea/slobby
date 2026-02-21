let parse (source : string) : Ast.program =
  (* Ensure source ends with newline for parser *)
  let source = if String.length source > 0 && source.[String.length source - 1] <> '\n'
    then source ^ "\n" else source in
  let lexbuf = Lexing.from_string source in
  Lexer.prev_was_expr_end := false;
  try
    Parser.program Lexer.token lexbuf
  with
  | Parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    Printf.eprintf "Parse error at line %d, column %d\n"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
    exit 1
  | Lexer.Lexer_error msg ->
    let pos = Lexing.lexeme_start_p lexbuf in
    Printf.eprintf "Lexer error at line %d: %s\n" pos.pos_lnum msg;
    exit 1
