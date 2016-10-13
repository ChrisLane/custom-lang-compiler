%{ open Ast %}

%token  <int>       INT
%token  <string>    NAME

%token              PLUS MINUS TIMES DIVIDE
%token              LEQ GEQ EQUALTO NOTEQTO
%token              AND OR NOT

%token              TYPE LET IN WHILE IF ASG DO ELSE PRINTINT RETURN

%token              LPAREN RPAREN SEMICOLON LBRACE RBRACE PARAMSEP

%token              EOF

%left               OR
%left               AND
%right              EQUALTO NOTEQTO
%left               LEQ GEQ
%left               PLUS MINUS
%left               TIMES DIVIDE
%right              NOT

%start  <Ast.program>   program
%%

program:
  | f = fundef*; EOF    { f };;

fundef:
  | n = NAME; p = funparams; b = bracedbody    { Function (n, p, b) };;

funparams:
  | LPAREN; p = separated_list (PARAMSEP, NAME); RPAREN     { p };;

bracedbody:
  | LBRACE; RBRACE              { Empty }
  | LBRACE; e = body; RBRACE    { e };;

body:
  | e = action                              { e }
  | e = action;  SEMICOLON;   f = body      { Seq (e, f) };;

action:
  | TYPE;       i = NAME;       ASG;    c = const;      SEMICOLON;  e = body        { New (i, c, e) }
  | LET;        s = NAME;       ASG;    e = exp;        IN;         f = body        { Let (s, e, f) }
  | WHILE;      e = params;     DO;     f = bracedbody                              { While (e, f) }
  | IF;         e = params;     DO;     f = bracedbody; ELSE;       g = bracedbody  { If (e, f, g) }
  | RETURN;     x = identifier                                                      { Deref x }
  | PRINTINT;   e = identifier                                                      { Printint e }
  | e = exp;    ASG;            f = exp                                             { Asg (e, f) };;

params:
  | LPAREN; e = exp; RPAREN    { e };;

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
