%{ open Ast %}

%token <string> IDENT
%token <float> FLOAT
%token TRUE FALSE
%token FUN REC LET IN IF THEN ELSE
%token MATCH WITH INL INR FST SND
%token UNIFORM GAUSS EXPONENTIAL GAMMA BETA FLIP BERNOULLI POISSON DISCRETE
%token DOT EQ BAR COMMA CONS
%token LPAREN RPAREN LBRACK RBRACK LT LEQ GT
%token PLUS TIMES MINUS DIVIDE
%token OBSERVE
%token DARROW
%token EOF

%start <Ast.expr> main
%%
main:
  | expr EOF                           { $1 }

expr:
  | IF expr THEN expr ELSE expr        { If ($2, $4, $6) }
  | LET IDENT EQ expr IN expr          { Let ($2, $4, $6) }
  | MATCH expr WITH INL IDENT DARROW expr BAR INR IDENT DARROW expr
                                        { Case ($2, ($5, $7), ($10, $12)) }
  | MATCH expr WITH LBRACK RBRACK DARROW expr BAR IDENT CONS IDENT DARROW expr
                                        { MatchList ($2, $7, ($9, $11, $13)) }
  | fun_expr                           { $1 }

fun_expr:
  | FUN IDENT DARROW expr              { Lam ($2, $4) }
  | REC IDENT IDENT DARROW expr        { Rec ($2, $3, $5) }
  | cmp                                { $1 }

cmp:
  | cmp LT cons                        { Lt ($1, $3) }
  | cmp LEQ cons                       { Leq ($1, $3) }
  | cons                               { $1 }

cons:
  | add CONS cons                      { Cons ($1, $3) }
  | add                                { $1 }

add:
  | add PLUS mul                       { Add ($1, $3) }
  | add MINUS mul                      { Sub ($1, $3) }
  | mul                                { $1 }

mul:
  | mul TIMES unary                    { Mul ($1, $3) }
  | mul DIVIDE unary                   { Div ($1, $3) }
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
  | LBRACK RBRACK                      { Nil }
  | LPAREN RPAREN                      { Unit }
  | LPAREN expr RPAREN                 { $2 }
  | LPAREN expr COMMA expr RPAREN      { Pair ($2, $4) }
  | FST atom                           { Fst $2 }
  | SND atom                           { Snd $2 }
  | INL atom                           { Inl $2 }
  | INR atom                           { Inr $2 }
  | OBSERVE LPAREN expr RPAREN            { Observe ($3) }
  | UNIFORM LPAREN expr COMMA expr RPAREN { Uniform ($3, $5) }
  | GAUSS LPAREN expr COMMA expr RPAREN    { Gauss ($3, $5) }
  | EXPONENTIAL LPAREN expr RPAREN         { Exponential ($3) }
  | GAMMA LPAREN expr COMMA expr RPAREN    { Gamma ($3, $5) }
  | BETA  LPAREN expr COMMA expr RPAREN    { Beta ($3, $5) }
  | FLIP LPAREN expr RPAREN                { Flip ($3) }
  | BERNOULLI LPAREN expr RPAREN           { Bernoulli ($3) }
  | POISSON LPAREN expr RPAREN             { Poisson ($3) }
  | DISCRETE LPAREN probs = separated_nonempty_list(COMMA, FLOAT) RPAREN
    {
      let cases =
        List.mapi (fun i p -> (p, Const (float_of_int i))) probs
      in
      Discrete cases
    }
