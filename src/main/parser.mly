%{ open Ast %}

%token  <int>       INT
%token  <string>    NAME

%token              PLUS MINUS TIMES DIVIDE
%token              LEQ GEQ EQUALTO NOTEQTO
%token              AND OR NOT

%token              WHILE IF ASG DO ELSE

%token              PRINTINT

%token              LET IN

%token              RETURN

%token              LPAREN RPAREN SEMICOLON LBRACE RBRACE

%token              TYPE

%token              PARAMSEP

%token              EOF

%right              NOT
%left               PLUS MINUS
%left               TIMES DIVIDE
%left               LEQ GEQ
%right              EQUALTO NOTEQTO
%left               AND
%left               OR

%start  <Ast.program>   program
%%

program:
  | f = fundef*; EOF    { f };;

fundef:
  | n = NAME; p = funparams; b = funbody    { Function (n, p, b) };;

funparams:
  | LPAREN; RPAREN { [] }
  | LPAREN; p = separated_list (PARAMSEP, NAME); RPAREN { p };;

funbody:
  | LBRACE; e = body; RBRACE { e };;

body:
  | e = action                          { e }
  | e = other                           { e }
  | e = action; SEMICOLON   f = body    { Seq (e, f) }

action:
  | TYPE;   i = NAME;   ASG;    c = const;  SEMICOLON;  e = body    { New (i, c, e) } (* NEW *)
  | LET;    s = NAME;   ASG;    e = exp;    IN;         f = body    { Let (s, e, f) }
  | WHILE;  e = params; DO;     f = body                            { While (e, f) }
  | IF;     e = params; DO;     f = body;   ELSE;       g = body    { If (e, f, g) }

params:
  | LPAREN; es = exp+; RPAREN    { Ast.make_seq es };;

const:
  | i = INT     { Const i };;

identifier:
  | s = NAME    { Identifier s };;

exp:
  | e   = const                         { e }
  | e   = identifier                    { e }
  | e   = exp; PLUS;        f = exp     { Operator (Plus, e, f) }
  | e   = exp; MINUS;       f = exp     { Operator (Minus, e, f) }
  | e   = exp; TIMES;       f = exp     { Operator (Times, e, f) }
  | e   = exp; DIVIDE;      f = exp     { Operator (Divide, e, f) }
  | e   = exp; LEQ;         f = exp     { Operator (Leq, e, f) }
  | e   = exp; GEQ;         f = exp     { Operator (Geq, e, f) }
  | e   = exp; EQUALTO;     f = exp     { Operator (Equal, e, f) }
  | e   = exp; NOTEQTO;     f = exp     { Operator (Noteq, e, f) }
  | e   = exp; AND;         f = exp     { Operator (And, e, f) }
  | e   = exp; OR;          f = exp     { Operator (Or, e, f) }
  | e   = exp; NOT;         f = exp     { Operator (Not, e, f) };;
  | e = other   { e }

other:
  | e = exp;        ASG;        f = exp                                 { Asg (e, f) }
  | PRINTINT;       e = identifier                                      { Printint e }
  | RETURN;         x = identifier                                      { Deref x };;
  | e = other;        SEMICOLON;  f = other                             { Seq (e, f) }
