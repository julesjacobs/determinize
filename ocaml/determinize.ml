open Types
open Ast

(* Determinize a typed expression: strip types and replace expectation-mode 
   samples with their expected values if the type says Float[E]. *)

let rec of_texpr (t : typed_expr) : Ast.expr =
  match t.expr, t.typ with
  | EVar x, _ -> Var x
  | ELam (x, body), TArrow (_, _) -> Lam (x, of_texpr body)
  | ERec (f, x, body), TArrow (_, _) -> Rec (f, x, of_texpr body)
  | EApp (e1, e2), _ -> App (of_texpr e1, of_texpr e2)
  | EUnit, _ -> Unit
  | EPair (a, b), _ -> Pair (of_texpr a, of_texpr b)
  | EFst e, _ -> Fst (of_texpr e)
  | ESnd e, _ -> Snd (of_texpr e)
  | EInl e, _ -> Inl (of_texpr e)
  | EInr e, _ -> Inr (of_texpr e)
  | EMatch (e, (x,e1), (y,e2)), _ ->
      Case (of_texpr e, (x, of_texpr e1), (y, of_texpr e2))
  | EBool b, _ -> Bool b
  | EIf (c,t,f), _ -> If (of_texpr c, of_texpr t, of_texpr f)
  | ELet (x, e1, e2), _ -> Let (x, of_texpr e1, of_texpr e2)
  | EConst f, _ -> Const f
  | ENeg e, _ -> Neg (of_texpr e)
  | EAdd (a,b), _ -> Add (of_texpr a, of_texpr b)
  | EMul (a,b), _ -> Mul (of_texpr a, of_texpr b)
  | ELt (a,b), _ -> Lt (of_texpr a, of_texpr b)
  | EUniform (a,b), TFloat mvar ->
      (match mvar.mode with
       | Some E -> Add (of_texpr a, of_texpr b) |> fun sum -> Mul (sum, Const 0.5)
       | _ -> Uniform (of_texpr a, of_texpr b))
  | EGauss (a,b), TFloat mvar ->
      (match mvar.mode with
       | Some E -> of_texpr a
       | _ -> Gauss (of_texpr a, of_texpr b))
  | _, _ -> failwith " ill-typed texpr"

let rec pp_mode fmt = function
  | G -> Format.fprintf fmt "G"
  | E -> Format.fprintf fmt "E"

let rec pp_typ fmt = function
  | TUnit -> Format.fprintf fmt "unit"
  | TBool -> Format.fprintf fmt "bool"
  | TNat -> Format.fprintf fmt "nat"
  | TFloat m ->
      (match m.mode with
       | Some m' -> Format.fprintf fmt "float[%a]" pp_mode m'
       | None -> Format.fprintf fmt "float[?m%d]" m.mode_id)
  | TPair (a,b) -> Format.fprintf fmt "(%a * %a)" pp_typ a pp_typ b
  | TSum (a,b) -> Format.fprintf fmt "(%a + %a)" pp_typ a pp_typ b
  | TArrow (a,b) -> Format.fprintf fmt "(%a -> %a)" pp_typ a pp_typ b
  | TMeta m ->
      (match m.ty_value with
       | Some t -> pp_typ fmt t
       | None -> Format.fprintf fmt "?t%d" m.ty_id)

let rec pp_texpr fmt (t : typed_expr) =
  let rec go fmt texpr =
    match texpr with
    | EVar x -> Format.fprintf fmt "%s" x
    | ELam (x, body) -> Format.fprintf fmt "(fun %s => %a)" x go body.expr
    | ERec (f, x, body) -> Format.fprintf fmt "(rec %s %s => %a)" f x go body.expr
    | EApp (a,b) -> Format.fprintf fmt "(%a %a)" go a.expr go b.expr
    | EUnit -> Format.fprintf fmt "()"
    | EPair (a,b) -> Format.fprintf fmt "<%a, %a>" go a.expr go b.expr
    | EFst e -> Format.fprintf fmt "fst %a" go e.expr
    | ESnd e -> Format.fprintf fmt "snd %a" go e.expr
    | EInl e -> Format.fprintf fmt "inl %a" go e.expr
    | EInr e -> Format.fprintf fmt "inr %a" go e.expr
    | EMatch (e,(x,a),(y,b)) ->
        Format.fprintf fmt "match %a with inl %s -> %a | inr %s -> %a" go e.expr x go a.expr y go b.expr
    | EBool b -> Format.fprintf fmt "%B" b
    | EIf (c,t,f) -> Format.fprintf fmt "if %a then %a else %a" go c.expr go t.expr go f.expr
    | ELet (x,a,b) -> Format.fprintf fmt "let %s = %a in %a" x go a.expr go b.expr
    | EConst f -> Format.fprintf fmt "%g" f
    | ENeg e -> Format.fprintf fmt "-%a" go e.expr
    | EAdd (a,b) -> Format.fprintf fmt "(%a + %a)" go a.expr go b.expr
    | EMul (a,b) -> Format.fprintf fmt "(%a * %a)" go a.expr go b.expr
    | ELt (a,b) -> Format.fprintf fmt "(%a < %a)" go a.expr go b.expr
    | EUniform (a,b) -> Format.fprintf fmt "uniform(%a, %a)" go a.expr go b.expr
    | EGauss (a,b) -> Format.fprintf fmt "gauss(%a, %a)" go a.expr go b.expr
  in
  Format.fprintf fmt "(%a : %a)" go t.expr pp_typ t.typ
