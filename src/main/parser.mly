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

expaction:
  | e = exp     { e }
  | a = action  { a }

action:
  | TYPE;       s = NAME;       ASG;    e = expaction;          SEMICOLON;      f = body    { New (s, e, f) }
  | LET;        s = NAME;       ASG;    e = expaction;          IN;             f = body    { Let (s, e, f) }
  | IF;         e = params;     DO;     f = bracedbody; ELSE;   g = bracedbody              { If (e, f, g) }
  | WHILE;      e = params;     DO;     f = bracedbody                                      { While (e, f) }
  | RETURN;     e = expaction                                                               { Deref e }
  | PRINTINT;   e = expaction                                                               { Printint e }
  | e = exp;    ASG             f = expaction                                               { Asg (e, f) }
  | e = exp;    p = params                                                                  { Application (e, p) }

params:
  | LPAREN; e = expaction; RPAREN    { e };;

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
  | e = INT                             { Const e }
  | e = NAME                            { Identifier e }
  | e = exp; o = operator;  f = exp     { Operator (o, e, f) };;
  | LPAREN; e = expaction; RPAREN       {e}
