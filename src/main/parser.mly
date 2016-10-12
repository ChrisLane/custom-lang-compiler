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
  | LPAREN; p = separated_list (PARAMSEP, NAME); RPAREN { p };;

funbody:
  | LBRACE; RBRACE              { Empty }
  | LBRACE; e = body; RBRACE    { e };;

body:
  | e = action                              { e }
  | e = action;  SEMICOLON;   f = body      { Seq (e, f) };;

action:
  | TYPE;       i = NAME;       ASG;    c = const;  SEMICOLON;  e = body    { New (i, c, e) } (* NEW *)
  | LET;        s = NAME;       ASG;    e = exp;    IN;         f = body    { Let (s, e, f) }
  | WHILE;      e = params;     DO;     f = body                            { While (e, f) }
  | IF;         e = params;     DO;     f = body;   ELSE;       g = body    { If (e, f, g) }
  | RETURN;     x = identifier                                              { Deref x }
  | PRINTINT;   e = identifier                                              { Printint e }
  | e = exp;    ASG;            f = exp                                     { Asg (e, f) };;

params:
  | LPAREN; es = exp+; RPAREN    { Ast.make_seq es };;

const:
  | i = INT     { Const i };;

identifier:
  | s = NAME    { Identifier s };;

%inline operator:
  | PLUS    { Plus }
  | MINUS   { Minus }
  | TIMES   { Times }
  | DIVIDE  { Divide }
  | LEQ     { Leq }
  | GEQ     { Geq }
  | EQUALTO { Equal }
  | NOTEQTO { Noteq }
  | AND     { And }
  | OR      { Or }
  | NOT     { Not }

exp:
  | e = const                           { e }
  | e = identifier                      { e }
  | e = exp; o = operator;  f = exp     { Operator (o, e, f) };;
