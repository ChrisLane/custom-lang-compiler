open Lexer
open Lexing
exception Error

(* Error printing *)
let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->  let _ = Printf.eprintf "%s%!" msg in raise Error
  | Parser.Error    ->  
    let pos = Lexing.lexeme_start_p lexbuf in
    let col = pos.pos_cnum - pos.pos_bol in
    let _ = Printf.eprintf "Syntax error at line %d and column %d: %s.\n%!"
        pos.pos_lnum col (Lexing.lexeme lexbuf) in
    raise Error;;
