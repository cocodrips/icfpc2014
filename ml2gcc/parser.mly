%{
  open Expr
%}

%token <int> INT
%token <string> ID
%token COMMA CONS
%token LET IN AND
%token MATCH WITH OR UNDER
%token FUN REC ARROW
%token IF ELSE THEN
%token CAR CDR
%token MINUS PLUS TIMES DIV
%token AND2 OR2 NOT
%token GT GTE EQ
%token LLIST RLIST
%token LPAR RPAR
%token EOC EOF
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
%nonassoc EQ LT
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%left APP
%nonassoc ID INT RPAR LPAR

%start expr
%type <Expr.expr> expr

%%
let_funs:
  | ID let_fun AND let_funs  { ($1, $2)::$4 }
  | ID let_fun               { [($1, $2)] }
;

let_fun:
  | ids EQ expr    { EFun ($1, $3) }
;

match_pattern:
  | pattern ARROW expr OR match_pattern { ($1, $3)::$5 }
  | pattern ARROW expr { [($1, $3)] }
;

plist:
  | pattern COMMA plist    { PCons($1, $3) }
  | pattern                { PCons($1, PConst 0) }
;

pattern:
  | INT                    { PConst $1 }
  | LLIST RLIST            { PConst 0 }
  | LLIST plist RLIST      { $2 }
  | ID                     { PVar $1 }
  | UNDER                  { PAny }
  | pattern CONS pattern   { PCons ($1, $3) }
  | LPAR pattern RPAR      { $2 }
;

expr:
  | u_expr                  { $1 }
  | LET let_funs IN expr %prec LET     { ELetIn ($2, $4) }
  | LET REC let_funs IN expr %prec REC { ERecIn ($3, $5) }

  | expr CONS expr          { ECons ($1, $3) }
  | MATCH expr WITH match_pattern { EMatch ($2, $4) }
  | FUN ids ARROW expr       { EFun ($2, $4) }

  | IF expr THEN expr ELSE expr  { EIf ($2, $4, $6) }
  | expr AND2 expr          { EIf ($1, $3, EConst 0) }
  | expr OR2 expr           { EIf ($1, EConst 1, $3) }
  | NOT expr                { EIf ($2, EConst 0, EConst 1) }
  | CAR expr                { ECar $2 }
  | CDR expr                { ECdr $2 }

  | expr PLUS expr          { EAdd ($1, $3) }
  | expr MINUS expr         { ESub ($1, $3) }
  | expr TIMES expr         { EMul ($1, $3) }
  | expr DIV expr           { EDiv ($1, $3) }
  | expr EQ expr            { EEq ($1, $3) }
  | expr GT expr            { EGt ($1, $3) }
  | expr GTE expr           { EGte ($1, $3) }

  | u_expr u_exprs          { EApp ($1, $2) }
  | MINUS expr %prec UMINUS { ESub ((EConst 0), $2) }
;

ids:
  | ID ids { $1::$2 }
  | ID     { [$1] }

u_exprs:
  | u_expr u_exprs { $1::$2 }
  | u_expr         { [$1] }

list:
  | expr COMMA list       { ECons($1, $3) }
  | expr                  { ECons($1, EConst 0)  }
;

u_expr:
  | INT                     { EConst $1 }
  | LPAR expr RPAR          { $2 }
  | ID                      { EVar $1 }
  | LLIST RLIST             { EConst 0 }
  | LLIST list RLIST        { $2 }
;

%%
