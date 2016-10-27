{
open Parser
exception SyntaxError of string
}

(* Regex *)
let int = ['0'-'9'] ['0'-'9']*
let name = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let main = "main"
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = "//"[^'\r' '\n']* newline (* // Line comments in this form *)

(* Match to tokens *)
rule read = parse
  | white           { read lexbuf } (* Skip any whitespace *)
  | comment         { read lexbuf } (* Skip any comments *)
  | newline         { Lexing.new_line lexbuf; read lexbuf } (* Skip any new lines*)

  | int             { INT (int_of_string (Lexing.lexeme lexbuf)) }

  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIVIDE }

  | "<="            { LEQ }
  | ">="            { GEQ }
  | "=="            { EQUALTO }
  | "!="            { NOTEQTO }
  | "&&"            { AND }
  | "||"            { OR }
  | '!'             { NOT }
  | '$'             { DEREF }

  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ';'             { SEMICOLON }
  | '{'             { LBRACE }
  | '}'             { RBRACE }

  | "var"           { TYPE }

  | ","             { PARAMSEP }

  | "while"         { WHILE }
  | "if"            { IF }
  | "else"          { ELSE }
  | '='             { ASG }
  | "readint()"     { READINT }
  | "print"         { PRINT }
  | "let"           { LET }
  | "return"        { RETURN }

  | main            { MAIN (Lexing.lexeme lexbuf) }
  | name            { NAME (Lexing.lexeme lexbuf) }
  | eof             { EOF }
  | _               { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) } (* Failed to match. Raise an error.*)
