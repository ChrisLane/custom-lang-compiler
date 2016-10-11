%{ open Ast %}

%token  <int>       INT
%token  <string>    NAME

%token              PLUS MINUS TIMES DIVIDE
%token              LEQ GEQ EQUAL NOTEQ
%token              AND OR NOT

%token              WHILE IF DO ELSE

%token              PRINTINT

%token              LET IN NEW

%token              RETURN

%token              LPAREN RPAREN SEMICOLON LBRACE RBRACE

%token              EOF

%right              NOT
%left               PLUS MINUS
%left               TIMES DIVIDE
%left               LEQ GEQ
%right              EQUAL NOTEQ
%left               AND
%left               OR

%start  <Ast.program>   program
%%

program:
  | f = fundef*; EOF    { f };;

fundef:
  | s = NAME; a = args; b = body    { Function (s, a, b) };;

args:
  | LPAREN; ss = NAME*; RPAREN   { ss };;

body:
  | LBRACE; es = exp*; RBRACE { Ast.make_seq es };;

params:
  | LPAREN; es = exp+; RPAREN    { Ast.make_seq es };;

basicexp:
  | i = INT                     { Const i }
  | s = NAME                    { Identifier s }

exp:
  | es  = basicexp+                 { List.fold_left (fun x y -> Application (x, y)) (List.hd es) (List.tl es) }
  | e   = exp; PLUS;      f = exp   { Operator (Plus, e, f) }
  | e   = exp; MINUS;     f = exp   { Operator (Minus, e, f) }
  | e   = exp; TIMES;     f = exp   { Operator (Times, e, f) }
  | e   = exp; DIVIDE;    f = exp   { Operator (Divide, e, f) }
  | e   = exp; LEQ;       f = exp   { Operator (Leq, e, f) }
  | e   = exp; GEQ;       f = exp   { Operator (Geq, e, f) }
  | e   = exp; EQUAL;     f = exp   { Operator (Equal, e, f) }
  | e   = exp; NOTEQ;     f = exp   { Operator (Noteq, e, f) }
  | e   = exp; AND;       f = exp   { Operator (And, e, f) }
  | e   = exp; OR;        f = exp   { Operator (Or, e, f) }
  | e   = exp; NOT;       f = exp   { Operator (Not, e, f) }

  | e = exp;    SEMICOLON   f = exp                                 { Seq (e, f) }
  | WHILE;      e = params; DO;     f = body                        { While (e, f) }
  | IF;         e = params; DO;     f = body;   ELSE;   g = body    { If (e, f, g) }
  | PRINTINT;   e = exp                                             { Printint e }
  | LET;        s = NAME;   EQUAL;  e = exp;    IN;     f = exp     { Let (s, e, f) }
  | NEW;        s = NAME;   EQUAL;  e = exp;    IN;     f = exp     { New (s, e, f) }
  | RETURN;     x = basicexp;                                       { Deref x }
