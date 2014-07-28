%{
  open Expr
  exception Parse_error of string
  let p exp = (!Expr.line, exp)
%}

%token <int> INT
%token <string> ID
%token COMMA SEMI CONS
%token LET IN AND
%token MATCH WITH OR UNDER
%token FUN REC ARROW
%token IF ELSE THEN
%token CAR CDR ATOM DEBUG
%token MINUS PLUS TIMES DIV
%token AND2 OR2 NOT
%token GT GTE EQ LT LTE NEQ
%token LLIST RLIST
%token LPAR RPAR
%token EOF
%token UMINUS

%nonassoc LET
%nonassoc REC
%right AND
%right COMMA
%right CONS
%nonassoc MATCH WITH
%nonassoc FUN ARROW
%right OR
%nonassoc IF THEN ELSE
%left AND2 OR2
%nonassoc NOT
%nonassoc EQ NEQ GT GTE LT LTE
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%left APP CAR CDR ATOM DEBUG
%nonassoc ID INT RPAR LPAR

%start expr
%type <(Expr.pos * Expr.pos Expr.expr)> expr

%%

match_pattern:
  | OR pattern ARROW expr OR match_pattern { ($2, $4)::$6 }
  | pattern ARROW expr OR match_pattern    { ($1, $3)::$5 }
  | pattern ARROW expr { [($1, $3)] }
;

plist:
  | pattern SEMI plist     { p (PCons($1, $3)) }
  | pattern                { p (PCons($1, p PUnit)) }
;

ptuple:
  | pattern COMMA ptuple     { p (PCons($1, $3)) }
  | pattern COMMA pattern    { p (PCons($1, $3)) }
;

pattern:
  | INT                    { p (PConst $1) }
  | LLIST RLIST            { p (PUnit) }
  | LLIST plist RLIST      { $2 }
  | ID                     { p (PVar $1) }
  | UNDER                  { p (PAny) }
  | pattern CONS pattern   { p (PCons ($1, $3)) }
  | LPAR ptuple RPAR       { $2 }
  | LPAR pattern RPAR      { $2 }
;

expr:
  | u_expr                             { $1 }
  | LET let_ands IN expr %prec LET     { p (ELetIn ($2, $4)) }
  | LET REC let_ands IN expr %prec REC { p (ERecIn ($3, $5)) }
  | expr CONS expr                { p (ECons ($1, $3)) }
  | MATCH expr WITH match_pattern { p (EMatch ($2, $4)) }
  | FUN ids ARROW expr            { p (EFun ($2, $4)) }
  | IF expr THEN expr ELSE expr  { p (EIf ($2, $4, $6)) }
  | expr AND2 expr          { p (EIf ($1, $3, p (EConst 0))) }
  | expr OR2 expr           { p (EIf ($1, p (EConst 1), $3)) }
  | NOT expr                { p (EIf ($2, p (EConst 0), p (EConst 1))) }
  | expr PLUS expr          { p (EAdd ($1, $3)) }
  | expr MINUS expr         { p (ESub ($1, $3)) }
  | expr TIMES expr         { p (EMul ($1, $3)) }
  | expr DIV expr           { p (EDiv ($1, $3)) }
  | expr EQ expr            { p (EEq ($1, $3)) }
  | expr GT expr            { p (EGt ($1, $3)) }
  | expr GTE expr           { p (EGte ($1, $3)) }
  | expr NEQ expr           { p (EIf (p (EEq ($1, $3)), p (EConst 0), p (EConst 1))) }
  | expr LT expr            { p (EGt ($3, $1)) }
  | expr LTE expr           { p (EGte ($3, $1)) }
  | CAR expr                { p (ECar $2) }
  | CDR expr                { p (ECdr $2) }
  | DEBUG expr              { p (EDebug $2) }
  | ATOM expr               { p (EAtom $2) }
  | u_expr u_exprs          { p (EApp ($1, $2)) }
  | MINUS expr %prec UMINUS { p (ESub (p (EConst 0), $2)) }
  | error                   { raise (Parse_error (string_of_pos !line)) }
;

u_expr:
  | INT                     { p (EConst $1) }
  | LPAR expr RPAR          { $2 }
  | ID                      { p (EVar $1) }
  | LLIST RLIST             { p (EConst 0) }
  | LLIST list RLIST        { $2 }
  | LPAR tuple RPAR         { $2 }
;

let_ands:
  | ID let_arg_body AND let_ands  { ($1, $2)::$4 }
  | ID let_arg_body               { [($1, $2)] }
;

let_arg_body:
  | EQ expr        { $2 }
  | ids EQ expr    { p(EFun ($1, $3)) }
;

ids:
  | ID ids { $1::$2 }
  | ID     { [$1] }
;

u_exprs:
  | u_expr u_exprs { $1::$2 }
  | u_expr         { [$1] }
;

tuple:
  | expr COMMA tuple  { p (ECons ($1, $3)) }
  | expr COMMA expr   { p (ECons ($1, $3)) }
;

list:
  | expr SEMI list    { p (ECons($1, $3)) }
  | expr              { p (ECons($1, p (EConst 0))) }
;

%%
