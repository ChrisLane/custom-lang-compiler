let _ = 
  read_line()
  |> Lexing.from_string
  |> Parser.program Lexer.read
  |> ignore;
  print_newline()
