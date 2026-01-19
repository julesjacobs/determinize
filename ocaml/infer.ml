open Ast
open Types

type env = (string * typ) list

let lookup x env =
  try List.assoc x env with Not_found -> failwith ("unbound variable " ^ x)

let fresh_float () = TFloat (fresh_mode_meta ())

let ensure_float expected =
  match zonk expected with
  | TFloat m -> TFloat m
  | TMeta m ->
      let mvar = fresh_mode_meta () in
      set_type m (TFloat mvar); TFloat mvar
  | _ -> failwith "expected float type"

let rec infer (env : env) (exp : Ast.expr) (expected : typ) : typed_expr =
  match exp with
  | Var x ->
      let ty_var = lookup x env in
      assert_subtype ty_var expected;
      { expr = EVar x; typ = expected }
  | Lam (x, body) ->
      let dom = TMeta (fresh_meta ()) in
      let cod = TMeta (fresh_meta ()) in
      let body_t = infer ((x, dom) :: env) body cod in
      let lam_ty = TArrow (dom, cod) in
      assert_subtype lam_ty expected;
      { expr = ELam (x, body_t); typ = lam_ty }
  | Rec (f, x, body) ->
      let dom = TMeta (fresh_meta ()) in
      let cod = TMeta (fresh_meta ()) in
      let fn_ty = TArrow (dom, cod) in
      let env' = (f, fn_ty) :: (x, dom) :: env in
      let body_t = infer env' body cod in
      assert_subtype body_t.typ cod;
      assert_subtype fn_ty expected;
      { expr = ERec (f, x, body_t); typ = fn_ty }
  | App (e1, e2) ->
      let arg_ty = TMeta (fresh_meta ()) in
      let res_ty = TMeta (fresh_meta ()) in
      let fn_ty = TArrow (arg_ty, res_ty) in
      let e1_t = infer env e1 fn_ty in
      let e2_t = infer env e2 arg_ty in
      assert_subtype res_ty expected;
      { expr = EApp (e1_t, e2_t); typ = res_ty }
  | Unit ->
      assert_subtype TUnit expected;
      { expr = EUnit; typ = TUnit }
  | Nil ->
      let elem = TMeta (fresh_meta ()) in
      let list_ty = TList elem in
      assert_subtype list_ty expected;
      { expr = ENil; typ = list_ty }
  | Cons (hd, tl) ->
      let elem = TMeta (fresh_meta ()) in
      let list_ty = TList elem in
      let hd_t = infer env hd elem in
      let tl_t = infer env tl list_ty in
      assert_subtype list_ty expected;
      { expr = ECons (hd_t, tl_t); typ = list_ty }
  | Pair (e1, e2) ->
      let t1 = TMeta (fresh_meta ()) in
      let t2 = TMeta (fresh_meta ()) in
      let e1_t = infer env e1 t1 in
      let e2_t = infer env e2 t2 in
      let pair_ty = TPair (e1_t.typ, e2_t.typ) in
      assert_subtype pair_ty expected;
      { expr = EPair (e1_t, e2_t); typ = pair_ty }
  | Fst e ->
      let a_ty = TMeta (fresh_meta ()) in
      let b_ty = TMeta (fresh_meta ()) in
      let pair_ty = TPair (a_ty, b_ty) in
      let e_t = infer env e pair_ty in
      assert_subtype a_ty expected;
      { expr = EFst e_t; typ = a_ty }
  | Snd e ->
      let a_ty = TMeta (fresh_meta ()) in
      let b_ty = TMeta (fresh_meta ()) in
      let pair_ty = TPair (a_ty, b_ty) in
      let e_t = infer env e pair_ty in
      assert_subtype b_ty expected;
      { expr = ESnd e_t; typ = b_ty }
  | Inl e ->
      let left_ty = TMeta (fresh_meta ()) in
      let right_ty = TMeta (fresh_meta ()) in
      let e_t = infer env e left_ty in
      let sum_ty = TSum (e_t.typ, right_ty) in
      assert_subtype sum_ty expected;
      { expr = EInl e_t; typ = sum_ty }
  | Inr e ->
      let left_ty = TMeta (fresh_meta ()) in
      let right_ty = TMeta (fresh_meta ()) in
      let e_t = infer env e right_ty in
      let sum_ty = TSum (left_ty, e_t.typ) in
      assert_subtype sum_ty expected;
      { expr = EInr e_t; typ = sum_ty }
  | Case (e, (x, e1), (y, e2)) ->
      let l_ty = TMeta (fresh_meta ()) in
      let r_ty = TMeta (fresh_meta ()) in
      let scrut_ty = TSum (l_ty, r_ty) in
      let scrut = infer env e scrut_ty in
      let e1_t = infer ((x, l_ty) :: env) e1 expected in
      let e2_t = infer ((y, r_ty) :: env) e2 expected in
      { expr = EMatch (scrut, (x, e1_t), (y, e2_t)); typ = expected }
  | MatchList (e, nil_br, (x, xs, cons_br)) ->
      let elem_ty = TMeta (fresh_meta ()) in
      let list_ty = TList elem_ty in
      let scrut = infer env e list_ty in
      let nil_t = infer env nil_br expected in
      let cons_env = (x, elem_ty) :: (xs, list_ty) :: env in
      let cons_t = infer cons_env cons_br expected in
      { expr = EMatchList (scrut, nil_t, (x, xs, cons_t)); typ = expected }
  | Bool b ->
      assert_subtype TBool expected;
      { expr = EBool b; typ = TBool }
  | If (c, t, f) ->
      let c_t = infer env c TBool in
      let t_t = infer env t expected in
      let f_t = infer env f expected in
      { expr = EIf (c_t, t_t, f_t); typ = expected }
  | Let (x, e1, e2) ->
      let ty1 = TMeta (fresh_meta ()) in
      let e1_t = infer env e1 ty1 in
      let e2_t = infer ((x, e1_t.typ) :: env) e2 expected in
      { expr = ELet (x, e1_t, e2_t); typ = e2_t.typ }
  | Const f ->
      let ty = ensure_float expected in
      { expr = EConst f; typ = ty }
  | Neg e ->
      let ty = ensure_float expected in
      let e_t = infer env e ty in
      { expr = ENeg e_t; typ = ty }
  | Add (a, b) ->
      let ty = ensure_float expected in
      let a_t = infer env a ty in
      let b_t = infer env b ty in
      { expr = EAdd (a_t, b_t); typ = ty }
  | Sub (a, b) ->
      let ty = ensure_float expected in
      let a_t = infer env a ty in
      let b_t = infer env b ty in
      { expr = ESub (a_t, b_t); typ = ty }
  | Mul (a, b) ->
        let is_scaling = match (a,b) with (Const _, _) | (_, Const _) -> true | _ -> false in
        let a_ty, b_ty = 
            if is_scaling 
            then 
                expected, expected
            else
                let g_mode = fresh_mode_meta () in
                set_mode g_mode G;
                (TFloat g_mode, TFloat g_mode) in
          let res_ty = ensure_float expected in
          let a_t = infer env a a_ty in
          let b_t = infer env b b_ty in
          { expr = EMul (a_t, b_t); typ = res_ty }
    | Div (a, b) ->
        let is_scaling = match (a,b) with (_, Const _) -> true | _ -> false in
        let a_ty, b_ty =
            if is_scaling then expected, expected
            else
            let g_mode = fresh_mode_meta () in
            set_mode g_mode G;
            (TFloat g_mode, TFloat g_mode)
        in
        let res_ty = ensure_float expected in
        let a_t = infer env a a_ty in
        let b_t = infer env b b_ty in
        { expr = EDiv (a_t, b_t); typ = res_ty }
  | Lt (a, b) ->
      let float_g = TFloat (fresh_mode_meta ()) in
      set_mode (match float_g with TFloat m -> m | _ -> assert false) G;
      let a_t = infer env a float_g in
      let b_t = infer env b float_g in
      assert_subtype TBool expected;
      { expr = ELt (a_t, b_t); typ = TBool }
  | Uniform (a, b) ->
      let ty = ensure_float expected in
      let a_t = infer env a ty in
      let b_t = infer env b ty in
      { expr = EUniform (a_t, b_t); typ = ty }
  | Gauss (a, b) ->
      let mean_ty = ensure_float expected in
      let var_mode = fresh_mode_meta () in
      set_mode var_mode G;
      let var_ty = TFloat var_mode in
      let a_t = infer env a mean_ty in
      let b_t = infer env b var_ty in
      { expr = EGauss (a_t, b_t); typ = mean_ty }
  | Exponential e ->
        let ty = ensure_float expected in
        let rate_mode = fresh_mode_meta () in
        set_mode rate_mode G;
        let rate_ty = TFloat rate_mode in
        let e_t = infer env e rate_ty in
        { expr = EExponential e_t; typ = ty }
  | Gamma (a, b) ->
        let a_ty = ensure_float expected in 
        let b_mode = fresh_mode_meta () in 
        set_mode b_mode G;
        let b_ty = TFloat b_mode in
        let a_t = infer env a a_ty in
        let b_t = infer env b b_ty in
        { expr = EGamma (a_t, b_t); typ = a_ty }
  | Beta (a, b) ->
        let ty = ensure_float expected in
        let param_mode = fresh_mode_meta () in
        set_mode param_mode G;
        let param_ty = TFloat param_mode in
        let a_t = infer env a param_ty in
        let b_t = infer env b param_ty in
        { expr = EBeta (a_t, b_t); typ = ty }
  | Flip p ->
      let p_ty = fresh_float () in
      let p_t = infer env p p_ty in
      assert_subtype TBool expected;
      { expr = EFlip p_t; typ = TBool }
  | Discrete choices ->
      let ty = ensure_float expected in 
      let typed_choices =
          List.map
            (fun (p, ei) ->
                if p < 0.0 || p > 1.0 then failwith "Discrete: probability not in [0,1]";
                let ei_t = infer env ei ty in
                (p, ei_t))
            choices
      in
      { expr = EDiscrete typed_choices; typ = ty }
      