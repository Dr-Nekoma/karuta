let parse input = Lexing.from_string input |> Parser.program Lexer.read
