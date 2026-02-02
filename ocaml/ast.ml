type ident = string

type expr =
  | Var of ident
  | Lam of ident * expr
  | Rec of ident * ident * expr
  | App of expr * expr
  | Unit
  | Nil
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Inl of expr
  | Inr of expr
  | Case of expr * (ident * expr) * (ident * expr)
  | Cons of expr * expr
  | MatchList of expr * expr * (ident * ident * expr)
  | Bool of bool
  | If of expr * expr * expr
  | Let of ident * expr * expr
  | Const of float
  | Neg of expr
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Lt of expr * expr
  | Leq of expr * expr
  | Uniform of expr * expr
  | Gauss of expr * expr
  | Exponential of expr
  | Gamma of expr * expr
  | Beta of expr * expr
  | Flip of expr
  | Bernoulli of expr
  | Poisson of expr
  | Discrete of (float * expr) list
  | Observe of expr 
