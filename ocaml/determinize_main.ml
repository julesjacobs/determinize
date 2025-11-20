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

let rec doc_expr ?(paren = false) expr =
  let open Doc in
  let wrap d = if paren then parens d else d in
  let infix op a b =
    wrap (group (nest 2 (join softline [ doc_expr ~paren:true a; text op; doc_expr ~paren:true b ])))
  in
  let rec app_chain e acc =
    match e with
    | A.App (f, arg) -> app_chain f (arg :: acc)
    | head -> (head, acc)
  in
  match expr with
  | A.Var x -> text x
  | A.Unit -> text "()"
  | A.Bool b -> text (string_of_bool b)
  | A.Const f -> text (Format.asprintf "%g" f)
  | A.Lam (x, e) ->
      wrap
        (group
           (vsep
              [ hsep [ text "fun"; text x; text "=>" ]
              ; nest 2 (doc_expr e)
              ]))
  | A.Rec (f, x, e) ->
      wrap
        (group
           (vsep
              [ hsep [ text "rec"; text f; text x; text "=>" ]
              ; nest 2 (doc_expr e)
              ]))
  | A.App _ ->
      let head, args_rev = app_chain expr [] in
      let docs = doc_expr ~paren:true head :: List.rev_map (doc_expr ~paren:true) args_rev in
      wrap (group (join softline docs))
  | A.Pair (e1, e2) ->
      wrap (enclose_separated "<" ">" (text "," ^^ softline) [ doc_expr e1; doc_expr e2 ])
  | A.Fst e -> group (hsep [ text "fst"; doc_expr e ])
  | A.Snd e -> group (hsep [ text "snd"; doc_expr e ])
  | A.Inl e -> group (hsep [ text "inl"; doc_expr e ])
  | A.Inr e -> group (hsep [ text "inr"; doc_expr e ])
  | A.Case (e, (x, e1), (y, e2)) ->
      let branches =
        vsep
          [
            hsep [ text "| inl"; text x; text "=>"; nest 2 (softline ^^ doc_expr e1) ];
            hsep [ text "| inr"; text y; text "=>"; nest 2 (softline ^^ doc_expr e2) ];
          ]
      in
      wrap (group (vsep [ hsep [ text "match"; doc_expr e; text "with" ]; nest 2 branches ]))
  | A.If (e1, e2, e3) ->
      wrap
        (group
           (vsep
              [
                hsep [ text "if"; doc_expr e1 ];
                hsep [ text "then" ];
                nest 2 (doc_expr e2);
                hsep [ text "else" ];
                nest 2 (doc_expr e3);
              ]))
  | A.Let (x, e1, e2) ->
      wrap
        (group
           (vsep
              [
                hsep [ text "let"; text x; text "=" ];
                nest 2 (doc_expr e1);
                text "in";
                nest 2 (doc_expr e2);
              ]))
  | A.Neg e -> wrap (group (hsep [ text "-"; doc_expr e ]))
  | A.Add (e1, e2) -> infix "+" e1 e2
  | A.Mul (e1, e2) -> infix "*" e1 e2
  | A.Lt (e1, e2) -> infix "<" e1 e2
  | A.Uniform (e1, e2) ->
      wrap
        (group
           (text "uniform" ^^ enclose_separated "(" ")" (text "," ^^ softline) [ doc_expr e1; doc_expr e2 ]))
  | A.Gauss (e1, e2) ->
      wrap
        (group
           (text "gauss" ^^ enclose_separated "(" ")" (text "," ^^ softline) [ doc_expr e1; doc_expr e2 ]))

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
