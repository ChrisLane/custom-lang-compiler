open Core.Std
open Lexer
open Lexing

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->  print_endline msg; exit (-1)
  | Parser.Error    ->  print_endline (Lexing.lexeme lexbuf); exit (-1)


let help () = 
  print_endline ("Please use: './main <file>\n")

let parse_file filename = 
  In_channel.create filename
  |> Lexing.from_channel
  |> parse_with_error


let _ =
  if Array.length (Sys.argv) != 2
  then (help (); exit 1)
  else parse_file (Sys.argv.(1));;
