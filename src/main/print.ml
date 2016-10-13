open Lexer
open Lexing

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->  print_endline msg; exit (-1)
  | Parser.Error    ->  
    print_endline ("Found: \'" ^ Lexing.lexeme lexbuf ^ "\' but expected something else."); exit (-1)


let help () = 
  print_endline ("Please use: './main <file>\n")

let parse_file filename = 
  open_in filename
  |> Lexing.from_channel
  |> parse_with_error
  |> List.map Ast.function_string
  |> String.concat " "
  |> print_string
  |> print_newline

let _ =
  if Array.length (Sys.argv) != 2
  then (help (); exit 1)
  else parse_file (Sys.argv.(1));;
