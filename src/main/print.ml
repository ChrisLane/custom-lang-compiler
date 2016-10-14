open Lexer
open Printf

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->  print_endline msg; exit (-1)
  | Parser.Error    ->  print_endline ("Found: \'" ^ Lexing.lexeme lexbuf ^ "\' but expected something else.\n"); exit (-1)

let parse_file filename = open_in filename
  |> Lexing.from_channel
  |> parse_with_error
  |> List.map Ast.function_string
  |> String.concat " "
  |> print_string
  |> print_newline

