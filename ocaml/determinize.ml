open Types
open Ast

(* Determinize a typed expression: strip types and replace expectation-mode 
   samples with their expected values if the type says Float[E]. *)

(* Set any unresolved mode variables to expectation mode for deterministic handling. *)
let rec default_modes_typ t =
  match zonk t with
  | TFloat m ->
      if m.mode = None then set_mode m E
  | TPair (a, b)
  | TSum (a, b) ->
      default_modes_typ a;
      default_modes_typ b
  | TArrow (a, b) ->
      default_modes_typ a;
      default_modes_typ b
  | TMeta m ->
      (match m.ty_value with
       | Some t' -> default_modes_typ t'
       | None -> ())
  | _ -> ()

let default_modes (t : typed_expr) =
  let rec go te =
    default_modes_typ te.typ;
    match te.expr with
    | EVar _ | EUnit | EBool _ | EConst _ -> ()
    | ELam (_, body) -> go body
    | ERec (_, _, body) -> go body
    | EApp (a, b) -> go a; go b
    | EPair (a, b) -> go a; go b
    | EFst e -> go e
    | ESnd e -> go e
    | EInl e -> go e
    | EInr e -> go e
    | EMatch (e, (_, a), (_, b)) -> go e; go a; go b
    | EIf (c, t1, t2) -> go c; go t1; go t2
    | ELet (_, a, b) -> go a; go b
    | ENeg e -> go e
    | EAdd (a, b) -> go a; go b
    | EMul (a, b) -> go a; go b
    | ELt (a, b) -> go a; go b
    | EUniform (a, b) -> go a; go b
    | EGauss (a, b) -> go a; go b
  in
  go t

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

let pp_mode fmt = function
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

let pp_texpr fmt (t : typed_expr) =
  let rec go fmt (t : typed_expr) =
    match t.expr with
    | EVar x -> Format.fprintf fmt "(%s : %a)" x pp_typ t.typ
    | ELam (x, body) -> Format.fprintf fmt "(fun %s => %a : %a)" x go body pp_typ t.typ
    | ERec (f, x, body) -> Format.fprintf fmt "(rec %s %s => %a : %a)" f x go body pp_typ t.typ
    | EApp (a,b) -> Format.fprintf fmt "(%a %a : %a)" go a go b pp_typ t.typ
    | EUnit -> Format.fprintf fmt "(() : %a)" pp_typ t.typ
    | EPair (a,b) -> Format.fprintf fmt "(<%a, %a> : %a)" go a go b pp_typ t.typ
    | EFst e -> Format.fprintf fmt "(fst %a : %a)" go e pp_typ t.typ
    | ESnd e -> Format.fprintf fmt "(snd %a : %a)" go e pp_typ t.typ
    | EInl e -> Format.fprintf fmt "(inl %a : %a)" go e pp_typ t.typ
    | EInr e -> Format.fprintf fmt "(inr %a : %a)" go e pp_typ t.typ
    | EMatch (e,(x,a),(y,b)) ->
        Format.fprintf fmt "(match %a with inl %s -> %a | inr %s -> %a : %a)"
          go e x go a y go b pp_typ t.typ
    | EBool b -> Format.fprintf fmt "(%B : %a)" b pp_typ t.typ
    | EIf (c,tbr,fbr) -> Format.fprintf fmt "(if %a then %a else %a : %a)" go c go tbr go fbr pp_typ t.typ
    | ELet (x,a,b) -> Format.fprintf fmt "(let %s = %a in %a : %a)" x go a go b pp_typ t.typ
    | EConst f -> Format.fprintf fmt "(%g : %a)" f pp_typ t.typ
    | ENeg e -> Format.fprintf fmt "(-%a : %a)" go e pp_typ t.typ
    | EAdd (a,b) -> Format.fprintf fmt "(%a + %a : %a)" go a go b pp_typ t.typ
    | EMul (a,b) -> Format.fprintf fmt "(%a * %a : %a)" go a go b pp_typ t.typ
    | ELt (a,b) -> Format.fprintf fmt "(%a < %a : %a)" go a go b pp_typ t.typ
    | EUniform (a,b) -> Format.fprintf fmt "(uniform(%a, %a) : %a)" go a go b pp_typ t.typ
    | EGauss (a,b) -> Format.fprintf fmt "(gauss(%a, %a) : %a)" go a go b pp_typ t.typ
  in
  go fmt t
