type ident = string

type expr =
  | Var of ident
  | Lam of ident * expr
  | Rec of ident * ident * expr
  | App of expr * expr
  | Unit
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Inl of expr
  | Inr of expr
  | Case of expr * (ident * expr) * (ident * expr)
  | Bool of bool
  | If of expr * expr * expr
  | Let of ident * expr * expr
  | Const of float
  | Neg of expr
  | Add of expr * expr
  | Mul of expr * expr
  | Lt of expr * expr
  | Uniform of expr * expr
  | Gauss of expr * expr
