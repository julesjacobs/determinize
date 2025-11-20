module A = Determinize.Ast
module P = Determinize.Parser
module L = Determinize.Lexer
module I = Determinize.Interp

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

let rec pp fmt = function
  | A.Var x -> Format.fprintf fmt "%s" x
  | A.Lam (x, e) -> Format.fprintf fmt "fun %s => %a" x pp e
  | A.Rec (f, x, e) -> Format.fprintf fmt "rec %s %s => %a" f x pp e
  | A.App (e1, e2) -> Format.fprintf fmt "(%a %a)" pp e1 pp e2
  | A.Unit -> Format.fprintf fmt "()"
  | A.Pair (e1, e2) -> Format.fprintf fmt "<%a, %a>" pp e1 pp e2
  | A.Fst e -> Format.fprintf fmt "fst %a" pp e
  | A.Snd e -> Format.fprintf fmt "snd %a" pp e
  | A.Inl e -> Format.fprintf fmt "inl %a" pp e
  | A.Inr e -> Format.fprintf fmt "inr %a" pp e
  | A.Case (e, (x, e1), (y, e2)) ->
      Format.fprintf fmt "match %a with inl %s -> %a | inr %s -> %a" pp e x pp e1 y pp e2
  | A.Bool b -> Format.fprintf fmt "%B" b
  | A.If (e1, e2, e3) -> Format.fprintf fmt "if %a then %a else %a" pp e1 pp e2 pp e3
  | A.Let (x, e1, e2) -> Format.fprintf fmt "let %s = %a in %a" x pp e1 pp e2
  | A.Const f -> Format.fprintf fmt "%g" f
  | A.Neg e -> Format.fprintf fmt "-%a" pp e
  | A.Add (e1, e2) -> Format.fprintf fmt "(%a + %a)" pp e1 pp e2
  | A.Mul (e1, e2) -> Format.fprintf fmt "(%a * %a)" pp e1 pp e2
  | A.Lt (e1, e2) -> Format.fprintf fmt "(%a < %a)" pp e1 pp e2
  | A.Uniform (e1, e2) -> Format.fprintf fmt "uniform(%a, %a)" pp e1 pp e2
  | A.Gauss (e1, e2) -> Format.fprintf fmt "gauss(%a, %a)" pp e1 pp e2

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "Usage: determinize <file.det>";
    exit 1);
  let path = Sys.argv.(1) in
  let ast = parse_file path in
  let out_path = path ^ ".out" in
  let oc = open_out out_path in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@." pp ast;
  Random.self_init ();
  let trials = 100 in
  let acc = ref 0.0 in
  for _ = 1 to trials do
    match I.eval (module I.StdRng) [] ast with
    | I.VFloat f -> acc := !acc +. f
    | _ -> failwith "expected top-level float result for mean computation"
  done;
  let mean = !acc /. float_of_int trials in
  Format.fprintf fmt "mean: %g@." mean;
  close_out oc;
  Format.printf "%a@.mean: %g@." pp ast mean
