{
open Parser
exception SyntaxError of string
}

let int = ['0'-'9'] ['0'-'9']*
let name = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | white   { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }

  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIVIDE }

  | "<="    { LEQ }
  | ">="    { GEQ }
  | '='     { EQUAL }
  | "!="    { NOTEQ }
  | "&&"    { AND }
  | "||"    { OR }
  | '!'     { NOT }

  | '('     { LPAREN }
  | ')'     { RPAREN }
  | ';'     { SEMICOLON }
  | '{'     { LBRACE }
  | '}'     { RBRACE }

  | "while"         { WHILE }
  | "if"            { IF }
  | "printint"      { PRINTINT }
  | "let"           { LET }
  | "new"           { NEW }
  | "return"        { RETURN }

  | name    { NAME (Lexing.lexeme lexbuf) }
  | eof     { EOF }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
