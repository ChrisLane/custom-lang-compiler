{
open Parser
exception SyntaxError of string
}

(* Regex *)
let int = ['0'-'9'] ['0'-'9']*
let name = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
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

  | "int"           { TYPE }

  | ","             { PARAMSEP }

  | "while"         { WHILE }
  | "if"            { IF }
  | "else"          { ELSE }
  | '='             { ASG }
  | "readint()"     { READINT }
  | "printint"      { PRINTINT }
  | "let"           { LET }
  | "return"        { RETURN }

  | name            { NAME (Lexing.lexeme lexbuf) }
  | eof             { EOF }
  | _               { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) } (* Failed to match. Raise an error.*)
