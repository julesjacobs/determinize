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

module Doc = Pretty

let doc_mode = function
  | G -> Doc.text "G"
  | E -> Doc.text "E"

let doc_mode_meta m =
  match m.mode with
  | Some m' -> doc_mode m'
  | None -> Doc.text (Format.sprintf "?m%d" m.mode_id)

let rec doc_typ ?(prec = 0) t =
  let open Doc in
  let wrap level d = if prec > level then parens d else d in
  match zonk t with
  | TUnit -> text "unit"
  | TBool -> text "bool"
  | TNat -> text "nat"
  | TFloat m -> text "float[" ^^ doc_mode_meta m ^^ text "]"
  | TPair (a, b) ->
      let d = group (hsep [ doc_typ ~prec:2 a; text "*"; doc_typ ~prec:2 b ]) in
      wrap 2 d
  | TSum (a, b) ->
      let d = group (hsep [ doc_typ ~prec:2 a; text "+"; doc_typ ~prec:2 b ]) in
      wrap 2 d
  | TArrow (a, b) ->
      let d =
        group (hsep [ doc_typ ~prec:1 a; text "->"; nest 2 (softline ^^ doc_typ b) ])
      in
      wrap 1 d
  | TMeta m ->
      (match m.ty_value with
       | Some t' -> doc_typ ~prec t' | None -> text (Format.sprintf "?t%d" m.ty_id))

let pp_typ fmt t = Doc.render fmt (doc_typ t)

let doc_typed_expr (t : typed_expr) =
  let open Doc in
  let rec app_chain e acc =
    match e.expr with
    | EApp (f, arg) -> app_chain f (arg :: acc)
    | _ -> (e, acc)
  in
  let rec render ctx_prec t =
    let prec, doc_body =
      match t.expr with
      | EVar x -> (5, text x)
      | ELam (x, body) ->
          (1,
           group
             (vsep
                [ hsep [ text "fun"; text x; text "=>" ]
                ; nest 2 (render 0 body)
                ]))
      | ERec (f, x, body) ->
          (1,
           group
             (vsep
                [ hsep [ text "rec"; text f; text x; text "=>" ]
                ; nest 2 (render 0 body)
                ]))
      | ELet (x, a, b) ->
          (0,
           group
             (vsep
                [ hsep [ text "let"; text x; text "=" ]
                ; nest 2 (render 0 a)
                ; text "in"
                ; nest 2 (render 0 b)
                ]))
      | EIf (c, tbr, fbr) ->
          (0,
           group
             (vsep
                [
                  hsep [ text "if"; render 0 c ];
                  hsep [ text "then" ];
                  nest 2 (render 0 tbr);
                  hsep [ text "else" ];
                  nest 2 (render 0 fbr);
                ]))
      | EMatch (e, (x, a), (y, b)) ->
          let branch_docs =
            [
              nest 2 (hsep [ text "|"; text "inl"; text x; text "=>"; render 0 a ]);
              nest 2 (hsep [ text "|"; text "inr"; text y; text "=>"; render 0 b ]);
            ]
          in
          (0, vsep (hsep [ text "match"; render 0 e; text "with" ] :: branch_docs))
      | EApp _ ->
          let head, args_rev = app_chain t [] in
          let docs = render 3 head :: List.rev_map (render 4) args_rev in
          (3, group (sep docs))
      | EPair (a, b) ->
          (4,
           group (enclose_separated "<" ">" (text "," ^^ softline) [ render 0 a; render 0 b ]))
      | EUnit -> (5, text "()")
      | EFst e -> (4, group (hsep [ text "fst"; render 0 e ]))
      | ESnd e -> (4, group (hsep [ text "snd"; render 0 e ]))
      | EInl e -> (4, group (hsep [ text "inl"; render 0 e ]))
      | EInr e -> (4, group (hsep [ text "inr"; render 0 e ]))
      | EConst f -> (5, text (Format.asprintf "%g" f))
      | EBool b -> (5, text (string_of_bool b))
      | ENeg e -> (4, group (hsep [ text "-"; render 4 e ]))
      | EMul (a, b) ->
          (2, group (nest 2 (sep [ render 2 a; text "*"; render 3 b ])))
      | EAdd (a, b) ->
          (1, group (nest 2 (sep [ render 1 a; text "+"; render 2 b ])))
      | ELt (a, b) ->
          (1, group (nest 2 (sep [ render 1 a; text "<"; render 2 b ])))
      | EUniform (a, b) ->
          (4,
           group
             (text "uniform"
              ^^ enclose_separated "(" ")" (text "," ^^ softline) [ render 0 a; render 0 b ]))
      | EGauss (a, b) ->
          (4,
           group
             (text "gauss"
              ^^ enclose_separated "(" ")" (text "," ^^ softline) [ render 0 a; render 0 b ]))
    in
    let doc_with_type = group (parens (nest 2 (sep [ doc_body; text ":"; doc_typ t.typ ]))) in
    paren_if (prec < ctx_prec) doc_with_type
  and paren_if p d = if p then parens d else d in
  render 0 t

let pp_texpr fmt t = Doc.render fmt (doc_typed_expr t)
