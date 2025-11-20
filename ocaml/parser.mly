%{ open Ast %}

%token <string> IDENT
%token <float> FLOAT
%token TRUE FALSE
%token FUN REC LET IN IF THEN ELSE
%token MATCH WITH INL INR FST SND
%token UNIFORM GAUSS
%token ARROW DOT EQ BAR COMMA
%token LPAREN RPAREN LT GT
%token PLUS TIMES MINUS
%token DARROW
%token EOF

%start <Ast.expr> main
%%
main:
  | expr EOF                           { $1 }

expr:
  | IF expr THEN expr ELSE expr        { If ($2, $4, $6) }
  | LET IDENT EQ expr IN expr          { Let ($2, $4, $6) }
  | MATCH expr WITH INL IDENT ARROW expr BAR INR IDENT ARROW expr
                                        { Case ($2, ($5, $7), ($10, $12)) }
  | fun_expr                           { $1 }

fun_expr:
  | FUN IDENT DARROW expr              { Lam ($2, $4) }
  | REC IDENT IDENT DARROW expr        { Rec ($2, $3, $5) }
  | cmp                                { $1 }

cmp:
  | cmp LT add                         { Lt ($1, $3) }
  | add                                { $1 }

add:
  | add PLUS mul                       { Add ($1, $3) }
  | mul                                { $1 }

mul:
  | mul TIMES unary                    { Mul ($1, $3) }
  | unary                              { $1 }

unary:
  | MINUS unary                        { Neg $2 }
  | app                                { $1 }

app:
  | app atom                           { App ($1, $2) }
  | atom                               { $1 }

atom:
  | IDENT                              { Var $1 }
  | FLOAT                              { Const $1 }
  | TRUE                               { Bool true }
  | FALSE                              { Bool false }
  | LPAREN RPAREN                      { Unit }
  | LPAREN expr RPAREN                 { $2 }
  | LT expr COMMA expr GT              { Pair ($2, $4) }
  | FST atom                           { Fst $2 }
  | SND atom                           { Snd $2 }
  | INL atom                           { Inl $2 }
  | INR atom                           { Inr $2 }
  | UNIFORM LPAREN expr COMMA expr RPAREN { Uniform ($3, $5) }
  | GAUSS LPAREN expr COMMA expr RPAREN    { Gauss ($3, $5) }
