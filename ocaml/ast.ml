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

(* ---------- Utilities: sets of identifiers ---------- *)
module StringSet = Set.Make (String)

let rec free_vars (e : expr) : StringSet.t =
  match e with
  | Var x -> StringSet.singleton x
  | Lam (x, body) -> StringSet.remove x (free_vars body)
  | Rec (f, x, body) -> free_vars body |> StringSet.remove f |> StringSet.remove x
  | App (e1, e2)
  | Pair (e1, e2)
  | Cons (e1, e2)
  | Add (e1, e2)
  | Mul (e1, e2)
  | Sub (e1, e2)
  | Div (e1, e2)
  | Lt (e1, e2)
  | Leq (e1, e2)
  | Uniform (e1, e2)
  | Gauss (e1, e2)
  | Gamma (e1, e2)
  | Beta (e1, e2) -> StringSet.union (free_vars e1) (free_vars e2)
  | Fst e
  | Snd e
  | Inl e
  | Inr e
  | Neg e
  | Exponential e
  | Flip e
  | Bernoulli e
  | Poisson e
  | Observe e -> free_vars e
  | If (c, t, f) ->
      StringSet.union (free_vars c) (StringSet.union (free_vars t) (free_vars f))
  | Let (x, e1, e2) ->
      StringSet.union (free_vars e1) (StringSet.remove x (free_vars e2))
  | Case (e, (x, e1), (y, e2)) ->
      StringSet.union (free_vars e)
        (StringSet.union
           (StringSet.remove x (free_vars e1))
           (StringSet.remove y (free_vars e2)))
  | MatchList (e, nil_br, (x, xs, cons_br)) ->
      StringSet.union (free_vars e)
        (StringSet.union (free_vars nil_br)
           (free_vars cons_br |> StringSet.remove x |> StringSet.remove xs))
  | Discrete cases ->
      List.fold_left
        (fun acc (_p, ei) -> StringSet.union acc (free_vars ei))
        StringSet.empty cases
  | Unit | Nil | Bool _ | Const _ -> StringSet.empty

(* A name based on [base] that is not in [avoid]. *)
let fresh_name (avoid : StringSet.t) (base : string) : string =
  if not (StringSet.mem base avoid) then base
  else
    let rec go i =
      let cand = base ^ "'" ^ string_of_int i in
      if StringSet.mem cand avoid then go (i + 1) else cand
    in
    go 0
