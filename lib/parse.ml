let parse input =
  let lexbuf = Lexing.from_string input in
  Parser.program Lexer.read lexbuf
