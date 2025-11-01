let parse input = Lexing.from_string input |> Parser.file Lexer.read
