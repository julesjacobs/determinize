{
open Parser

exception Lex_error of string
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | "(*"                        { comment lexbuf; token lexbuf }
  | "=>"                        { DARROW }
  | "\\" | "lambda" | "fun"     { FUN }
  | "rec"                       { REC }
  | "let"                       { LET }
  | "in"                        { IN }
  | "if"                        { IF }
  | "then"                      { THEN }
  | "else"                      { ELSE }
  | "match"                     { MATCH }
  | "with"                      { WITH }
  | "inl"                       { INL }
  | "inr"                       { INR }
  | "fst"                       { FST }
  | "snd"                       { SND }
  | "uniform"                   { UNIFORM }
  | "gauss"                     { GAUSS }
  | "exponential"               { EXPONENTIAL }
  | "gamma"                     { GAMMA }
  | "beta"                      { BETA }
  | "flip"                      { FLIP }
  | "discrete"                  { DISCRETE }
  | "true"                      { TRUE }
  | "false"                     { FALSE }
  | "("                         { LPAREN }
  | ")"                         { RPAREN }
  | "["                         { LBRACK }
  | "]"                         { RBRACK }
  | "<"                         { LT }
  | ">"                         { GT }
  | "::"                        { CONS }
  | ","                         { COMMA }
  | "|"                         { BAR }
  | "="                         { EQ }
  | "."                         { DOT }
  | "+"                         { PLUS }
  | "*"                         { TIMES }
  | "-"                         { MINUS }
  | "/"                         { DIVIDE }
  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E']['+' '-']? ['0'-'9']+)? as f { FLOAT (float_of_string f) }
  | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as id { IDENT id }
  | eof                        { EOF }
  | _ as c                     { raise (Lex_error (Printf.sprintf "unexpected character: %c" c)) }

and comment = parse
  | "*)" { () }
  | eof { raise (Lex_error "unterminated comment") }
  | _ { comment lexbuf }
