module A = Ast
module P = Parser
module L = Lexer
module I = Interp
module Infer = Infer
module Types = Types
module Det = Determinize
module Doc = Pretty

let parse_file path =
  let ic = open_in path in
  let lb = Lexing.from_channel ic in
  try
    let ast = P.main L.token lb in
    close_in ic;
    ast
  with e ->
    close_in_noerr ic;
    raise e

let rec doc_expr ?(ctx_prec = 0) expr =
  let open Doc in
  let paren_if needed d = if needed then parens d else d in
  let rec app_chain e acc =
    match e with
    | A.App (f, arg) -> app_chain f (arg :: acc)
    | head -> (head, acc)
  in
  let prec, body =
    match expr with
    | A.Var x -> (5, text x)
    | A.Unit -> (5, text "()")
    | A.Bool b -> (5, text (string_of_bool b))
    | A.Const f -> (5, text (Format.asprintf "%g" f))
    | A.Lam (x, e) ->
        (1,
         group
           (vsep
              [ hsep [ text "fun"; text x; text "=>" ]
              ; nest 2 (doc_expr e)
              ]))
    | A.Rec (f, x, e) ->
        (1,
         group
           (vsep
              [ hsep [ text "rec"; text f; text x; text "=>" ]
              ; nest 2 (doc_expr e)
              ]))
    | A.Let (x, e1, e2) ->
        (0,
         group
           (vsep
              [
                hsep [ text "let"; text x; text "=" ];
                nest 2 (doc_expr ~ctx_prec:0 e1);
                text "in";
                nest 2 (doc_expr ~ctx_prec:0 e2);
              ]))
    | A.If (c, tbr, fbr) ->
        (0,
         group
           (vsep
              [
                hsep [ text "if"; doc_expr c ];
                hsep [ text "then" ];
                nest 2 (doc_expr ~ctx_prec:0 tbr);
                hsep [ text "else" ];
                nest 2 (doc_expr ~ctx_prec:0 fbr);
              ]))
    | A.MatchList (e, nil_br, (x, xs, cons_br)) ->
        let branch_docs =
          [
            nest 2 (hsep [ text "|"; text "[]"; text "=>"; doc_expr ~ctx_prec:0 nil_br ]);
            nest 2
              (hsep [ text "|"; text x; text "::"; text xs; text "=>"
                    ; doc_expr ~ctx_prec:0 cons_br ]);
          ]
        in
        (0, vsep (hsep [ text "match"; doc_expr e; text "with" ] :: branch_docs))
    | A.Case (e, (x, e1), (y, e2)) ->
        let branch_docs =
          [
            nest 2 (hsep [ text "|"; text "inl"; text x; text "=>"; doc_expr ~ctx_prec:0 e1 ]);
            nest 2 (hsep [ text "|"; text "inr"; text y; text "=>"; doc_expr ~ctx_prec:0 e2 ]);
          ]
        in
        (0, vsep (hsep [ text "match"; doc_expr e; text "with" ] :: branch_docs))
    | A.App _ ->
        let head, args_rev = app_chain expr [] in
        let docs = doc_expr ~ctx_prec:3 head :: List.rev_map (doc_expr ~ctx_prec:4) args_rev in
        (3, group (sep docs))
    | A.Pair (e1, e2) ->
        (4,
         group
           (enclose_separated "<" ">" (text "," ^^ softline)
              [ doc_expr ~ctx_prec:0 e1; doc_expr ~ctx_prec:0 e2 ]))
    | A.Nil -> (5, text "[]")
    | A.Cons (hd, tl) ->
        (0,
         group
           (nest 2
              (sep [ doc_expr ~ctx_prec:1 hd; text "::"; doc_expr ~ctx_prec:0 tl ])))
    | A.Fst e -> (4, group (hsep [ text "fst"; doc_expr ~ctx_prec:0 e ]))
    | A.Snd e -> (4, group (hsep [ text "snd"; doc_expr ~ctx_prec:0 e ]))
    | A.Inl e -> (4, group (hsep [ text "inl"; doc_expr ~ctx_prec:0 e ]))
    | A.Inr e -> (4, group (hsep [ text "inr"; doc_expr ~ctx_prec:0 e ]))
    | A.Neg e -> (4, group (hsep [ text "-"; doc_expr ~ctx_prec:4 e ]))
    | A.Mul (e1, e2) ->
        (2,
         group (nest 2 (sep [ doc_expr ~ctx_prec:2 e1; text "*"; doc_expr ~ctx_prec:3 e2 ])))
    | A.Add (e1, e2) ->
        (1, group (nest 2 (sep [ doc_expr ~ctx_prec:1 e1; text "+"; doc_expr ~ctx_prec:2 e2 ])))
    | A.Lt (e1, e2) ->
        (1, group (nest 2 (sep [ doc_expr ~ctx_prec:1 e1; text "<"; doc_expr ~ctx_prec:2 e2 ])))
    | A.Uniform (e1, e2) ->
        (4,
         group
           (text "uniform"
            ^^ enclose_separated "(" ")" (text "," ^^ softline) [ doc_expr e1; doc_expr e2 ]))
    | A.Gauss (e1, e2) ->
        (4,
         group
           (text "gauss"
            ^^ enclose_separated "(" ")" (text "," ^^ softline) [ doc_expr e1; doc_expr e2 ]))
  in
  paren_if (prec < ctx_prec) body

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "Usage: determinize <file.det>";
    exit 1);
  let path = Sys.argv.(1) in
  let ast = parse_file path in
  (* Type/direct elaboration expecting a float result. *)
  let expected = Types.TFloat (Types.fresh_mode_meta ()) in
  let elaborated = Infer.infer [] ast expected in
  let raw_elab_doc = Det.doc_typed_expr elaborated in
  (* Default modes before determinization/output. *)
  Det.default_modes elaborated;
  let defaulted_elab_doc = Det.doc_typed_expr elaborated in
  let det_ast = Det.of_texpr elaborated in
  let out_path = path ^ ".dout" in
  let oc = open_out out_path in
  let fmt = Format.formatter_of_out_channel oc in
  let trials = 100 in
  let mean_over expr kind =
    let acc = ref 0.0 in
    for _ = 1 to trials do
      match I.eval (module I.StdRng) [] expr with
      | I.VFloat f -> acc := !acc +. f
      | _ -> failwith ("expected top-level float result for " ^ kind)
    done;
    !acc /. float_of_int trials
  in
  (* Use deterministic seeds for reproducible sampling in both runs. *)
  Random.init 0;
  let stochastic_mean = mean_over ast "stochastic mean computation" in
  Random.init 0;
  let determinized_mean = mean_over det_ast "determinized mean computation" in
  let print_doc out doc = Doc.render ~width:80 out doc in
  let report out =
    Format.fprintf out "== Elaboration ==@.@[<v 0>raw:@ %a@,defaulted:@ %a@]@.@."
      print_doc raw_elab_doc print_doc defaulted_elab_doc;
    Format.fprintf out "== Determinized ==@.%a@.@." print_doc (doc_expr det_ast);
    Format.fprintf out "== Evaluation (%d trials) ==@.@[<v 0>program mean: %g@,\
                                       determinized mean: %g@]@."
      trials stochastic_mean determinized_mean
  in
  report fmt;
  close_out oc;
  report Format.std_formatter
