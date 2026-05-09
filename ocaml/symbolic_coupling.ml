(* Standalone prototype for the symbolic coupling idea.

   This file intentionally does not depend on the rest of the implementation.
   Compile and run it directly with:

     ocamlc -o /tmp/symbolic_coupling symbolic_coupling.ml
     /tmp/symbolic_coupling
     /tmp/symbolic_coupling --fuel 12 examples.det

   The prototype models a small expression language with float modes, lets,
   conditionals, arithmetic, flips, and uniform samples.  It compares:

   - ordinary small-step semantics;
   - symbolic small-step semantics, where expectation-mode samples are recorded
     in a symbolic environment instead of sampled immediately;
   - determinized small-step semantics.

   The symbolic state [<sigma || e>] has two interpretations:

   - actual: sample every symbolic binding in sigma, then return e;
   - expected: replace every symbolic binding by its expectation, then return e.

   The examples at the bottom check, up to the printed symbolic distribution
   representation, that:

   - original semantics = actual interpretation of symbolic semantics;
   - determinized semantics = expected interpretation of symbolic semantics.
*)

type mode = E | G

type expr =
  | Var of string
  | Float of float
  | Bool of bool
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Add of expr * expr
  | Div of expr * expr
  | Lt of expr * expr
  | Uniform of mode * expr * expr
  | Flip
  | Nil
  | Cons of expr * expr

type random =
  | RUniform of expr * expr

type sample = {
  name : string;
  random : random;
}

type 'a measure =
  | Return of 'a
  | Sample of sample * 'a measure
  | Choice of (float * 'a measure) list

type sym_state = {
  sigma : sample list;
  residual : expr;
}

type context = {
  mutable next_sample : int;
}

let empty_context () = { next_sample = 0 }

let fresh_sample ctx =
  let name = "u" ^ string_of_int ctx.next_sample in
  ctx.next_sample <- ctx.next_sample + 1;
  name

let mode_to_string = function
  | E -> "E"
  | G -> "G"

let rec expr_to_string = function
  | Var x -> x
  | Float f ->
      if Float.is_integer f then string_of_int (int_of_float f)
      else Printf.sprintf "%.6g" f
  | Bool true -> "true"
  | Bool false -> "false"
  | Let (x, e1, e2) ->
      "let " ^ x ^ " = " ^ expr_to_string e1 ^ " in " ^ expr_to_string e2
  | If (c, t, f) ->
      "if " ^ expr_to_string c ^ " then " ^ expr_to_string t
      ^ " else " ^ expr_to_string f
  | Add (a, b) ->
      "(" ^ expr_to_string a ^ " + " ^ expr_to_string b ^ ")"
  | Div (a, b) ->
      "(" ^ expr_to_string a ^ " / " ^ expr_to_string b ^ ")"
  | Lt (a, b) ->
      "(" ^ expr_to_string a ^ " < " ^ expr_to_string b ^ ")"
  | Uniform (m, a, b) ->
      "uniform[" ^ mode_to_string m ^ "](" ^ expr_to_string a ^ ", "
      ^ expr_to_string b ^ ")"
  | Flip -> "flip()"
  | Nil -> "[]"
  | Cons (h, t) ->
      "(" ^ expr_to_string h ^ " :: " ^ expr_to_string t ^ ")"

type token =
  | TLet
  | TIn
  | TIf
  | TThen
  | TElse
  | TTrue
  | TFalse
  | TIdent of string
  | TFloat of float
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TComma
  | TEqual
  | TPlus
  | TSlash
  | TLt
  | TCons
  | TEOF

exception Parse_error of string

let parse_error msg = raise (Parse_error msg)

let token_to_string = function
  | TLet -> "let"
  | TIn -> "in"
  | TIf -> "if"
  | TThen -> "then"
  | TElse -> "else"
  | TTrue -> "true"
  | TFalse -> "false"
  | TIdent x -> "identifier " ^ x
  | TFloat f -> "number " ^ string_of_float f
  | TLParen -> "("
  | TRParen -> ")"
  | TLBracket -> "["
  | TRBracket -> "]"
  | TComma -> ","
  | TEqual -> "="
  | TPlus -> "+"
  | TSlash -> "/"
  | TLt -> "<"
  | TCons -> "::"
  | TEOF -> "end of file"

let is_digit c = c >= '0' && c <= '9'

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident_char c = is_ident_start c || is_digit c || c = '\''

let keyword_or_ident = function
  | "let" -> TLet
  | "in" -> TIn
  | "if" -> TIf
  | "then" -> TThen
  | "else" -> TElse
  | "true" -> TTrue
  | "false" -> TFalse
  | x -> TIdent x

let tokenize source =
  let len = String.length source in
  let peek i = if i < len then Some source.[i] else None in
  let rec skip_line i =
    match peek i with
    | None | Some '\n' -> i
    | Some _ -> skip_line (i + 1)
  in
  let scan_number i =
    let j = ref i in
    if !j < len && source.[!j] = '-' then incr j;
    while !j < len && is_digit source.[!j] do
      incr j
    done;
    if !j < len && source.[!j] = '.' then (
      incr j;
      while !j < len && is_digit source.[!j] do
        incr j
      done);
    let raw = String.sub source i (!j - i) in
    try (TFloat (float_of_string raw), !j)
    with Failure _ -> parse_error ("invalid number " ^ raw)
  in
  let scan_ident i =
    let j = ref (i + 1) in
    while !j < len && is_ident_char source.[!j] do
      incr j
    done;
    let raw = String.sub source i (!j - i) in
    (keyword_or_ident raw, !j)
  in
  let starts_number i =
    match peek i with
    | Some c when is_digit c -> true
    | Some '.' | Some '-' -> (
        match peek (i + 1) with
        | Some c -> is_digit c
        | None -> false)
    | _ -> false
  in
  let rec go i acc =
    if i >= len then List.rev (TEOF :: acc)
    else
      match source.[i] with
      | ' ' | '\n' | '\r' | '\t' -> go (i + 1) acc
      | '#' -> go (skip_line (i + 1)) acc
      | '/' when i + 1 < len && source.[i + 1] = '/' ->
          go (skip_line (i + 2)) acc
      | '(' -> go (i + 1) (TLParen :: acc)
      | ')' -> go (i + 1) (TRParen :: acc)
      | '[' -> go (i + 1) (TLBracket :: acc)
      | ']' -> go (i + 1) (TRBracket :: acc)
      | ',' -> go (i + 1) (TComma :: acc)
      | '=' -> go (i + 1) (TEqual :: acc)
      | '+' -> go (i + 1) (TPlus :: acc)
      | '/' -> go (i + 1) (TSlash :: acc)
      | '<' -> go (i + 1) (TLt :: acc)
      | ':' when i + 1 < len && source.[i + 1] = ':' ->
          go (i + 2) (TCons :: acc)
      | c when starts_number i ->
          let tok, next = scan_number i in
          go next (tok :: acc)
      | c when is_ident_start c ->
          let tok, next = scan_ident i in
          go next (tok :: acc)
      | c ->
          parse_error
            (Printf.sprintf "unexpected character %C while tokenizing" c)
  in
  go 0 []

type parser = {
  tokens : token array;
  mutable position : int;
}

let parser_of_tokens tokens = { tokens = Array.of_list tokens; position = 0 }

let peek_token parser =
  if parser.position < Array.length parser.tokens then
    parser.tokens.(parser.position)
  else TEOF

let take_token parser =
  let token = peek_token parser in
  parser.position <- parser.position + 1;
  token

let fixed_token_equal lhs rhs =
  match (lhs, rhs) with
  | TLet, TLet
  | TIn, TIn
  | TIf, TIf
  | TThen, TThen
  | TElse, TElse
  | TTrue, TTrue
  | TFalse, TFalse
  | TLParen, TLParen
  | TRParen, TRParen
  | TLBracket, TLBracket
  | TRBracket, TRBracket
  | TComma, TComma
  | TEqual, TEqual
  | TPlus, TPlus
  | TSlash, TSlash
  | TLt, TLt
  | TCons, TCons
  | TEOF, TEOF ->
      true
  | _ -> false

let expect parser wanted =
  let found = take_token parser in
  if not (fixed_token_equal found wanted) then
    parse_error
      ("expected " ^ token_to_string wanted ^ ", found "
      ^ token_to_string found)

let accept parser wanted =
  if fixed_token_equal (peek_token parser) wanted then (
    ignore (take_token parser);
    true)
  else false

let parse_identifier parser =
  match take_token parser with
  | TIdent x -> x
  | found -> parse_error ("expected identifier, found " ^ token_to_string found)

let parse_mode parser =
  expect parser TLBracket;
  let mode =
    match take_token parser with
    | TIdent "E" -> E
    | TIdent "G" -> G
    | found ->
        parse_error
          ("expected mode E or G, found " ^ token_to_string found)
  in
  expect parser TRBracket;
  mode

let rec parse_expression parser = parse_let_or_if parser

and parse_let_or_if parser =
  match peek_token parser with
  | TLet ->
      ignore (take_token parser);
      let x = parse_identifier parser in
      expect parser TEqual;
      let bound = parse_expression parser in
      expect parser TIn;
      let body = parse_expression parser in
      Let (x, bound, body)
  | TIf ->
      ignore (take_token parser);
      let cond = parse_expression parser in
      expect parser TThen;
      let yes_branch = parse_expression parser in
      expect parser TElse;
      let no_branch = parse_expression parser in
      If (cond, yes_branch, no_branch)
  | _ -> parse_cons parser

and parse_cons parser =
  let head = parse_compare parser in
  if accept parser TCons then Cons (head, parse_cons parser) else head

and parse_compare parser =
  let lhs = parse_add parser in
  if accept parser TLt then Lt (lhs, parse_add parser) else lhs

and parse_add parser =
  let rec loop acc =
    if accept parser TPlus then loop (Add (acc, parse_div parser)) else acc
  in
  loop (parse_div parser)

and parse_div parser =
  let rec loop acc =
    if accept parser TSlash then loop (Div (acc, parse_atom parser)) else acc
  in
  loop (parse_atom parser)

and parse_call_arguments parser =
  expect parser TLParen;
  let first = parse_expression parser in
  expect parser TComma;
  let second = parse_expression parser in
  expect parser TRParen;
  (first, second)

and parse_atom parser =
  match take_token parser with
  | TFloat f -> Float f
  | TTrue -> Bool true
  | TFalse -> Bool false
  | TIdent "flip" ->
      if accept parser TLParen then expect parser TRParen;
      Flip
  | TIdent "uniform" ->
      let mode =
        match peek_token parser with
        | TLBracket -> parse_mode parser
        | _ -> E
      in
      let a, b = parse_call_arguments parser in
      Uniform (mode, a, b)
  | TIdent "uniformE" ->
      let a, b = parse_call_arguments parser in
      Uniform (E, a, b)
  | TIdent "uniformG" ->
      let a, b = parse_call_arguments parser in
      Uniform (G, a, b)
  | TIdent "nil" -> Nil
  | TIdent x -> Var x
  | TLBracket ->
      expect parser TRBracket;
      Nil
  | TLParen ->
      let expr = parse_expression parser in
      expect parser TRParen;
      expr
  | found ->
      parse_error ("expected expression, found " ^ token_to_string found)

let parse_source source =
  let parser = parser_of_tokens (tokenize source) in
  let expr = parse_expression parser in
  expect parser TEOF;
  expr

let random_to_string = function
  | RUniform (a, b) ->
      "uniform(" ^ expr_to_string a ^ ", " ^ expr_to_string b ^ ")"

let rec measure_to_string value_to_string = function
  | Return v -> "return " ^ value_to_string v
  | Sample (s, rest) ->
      s.name ^ " ~ " ^ random_to_string s.random ^ "; "
      ^ measure_to_string value_to_string rest
  | Choice branches ->
      let render_branch (p, m) =
        Printf.sprintf "%.6g" p ^ " => " ^ measure_to_string value_to_string m
      in
      "choice {" ^ String.concat " | " (List.map render_branch branches) ^ "}"

let sym_state_to_string st =
  let sigma =
    match st.sigma with
    | [] -> "empty"
    | samples ->
        samples
        |> List.map (fun s -> s.name ^ " ~ " ^ random_to_string s.random)
        |> String.concat ", "
  in
  "<" ^ sigma ^ " || " ^ expr_to_string st.residual ^ ">"

let rec measure_bind m f =
  match m with
  | Return x -> f x
  | Sample (s, rest) -> Sample (s, measure_bind rest f)
  | Choice branches ->
      Choice (List.map (fun (p, branch) -> (p, measure_bind branch f)) branches)

let measure_map f m = measure_bind m (fun x -> Return (f x))

let rec is_float_term = function
  | Float _ -> true
  | Var _ -> true
  | Add (a, b) -> is_float_term a && is_float_term b
  | Div (a, b) -> is_float_term a && is_float_term b
  | _ -> false

let rec is_value = function
  | Float _ | Bool _ | Var _ | Nil -> true
  | Add (a, b) | Div (a, b) -> is_float_term a && is_float_term b
  | Cons (h, t) -> is_value h && is_value t
  | _ -> false

let rec subst x replacement expr =
  let go = subst x replacement in
  match expr with
  | Var y when String.equal x y -> replacement
  | Var _ | Float _ | Bool _ | Flip | Nil -> expr
  | Let (y, e1, e2) ->
      Let (y, go e1, if String.equal x y then e2 else go e2)
  | If (c, t, f) -> If (go c, go t, go f)
  | Add (a, b) -> Add (go a, go b)
  | Div (a, b) -> Div (go a, go b)
  | Lt (a, b) -> Lt (go a, go b)
  | Uniform (m, a, b) -> Uniform (m, go a, go b)
  | Cons (h, t) -> Cons (go h, go t)

let subst_many env expr =
  List.fold_left (fun acc (x, replacement) -> subst x replacement acc) expr env

let subst_random env = function
  | RUniform (a, b) -> RUniform (subst_many env a, subst_many env b)

let rec simplify expr =
  let s = simplify in
  match expr with
  | Add (a, b) -> (
      match (s a, s b) with
      | Float x, Float y -> Float (x +. y)
      | Float 0.0, e | e, Float 0.0 -> e
      | a', b' -> Add (a', b'))
  | Div (a, b) -> (
      match (s a, s b) with
      | Float x, Float y -> Float (x /. y)
      | a', Float 1.0 -> a'
      | a', b' -> Div (a', b'))
  | Lt (a, b) -> (
      match (s a, s b) with
      | Float x, Float y -> Bool (x < y)
      | a', b' -> Lt (a', b'))
  | Let (x, e1, e2) -> Let (x, s e1, s e2)
  | If (c, t, f) -> (
      match s c with
      | Bool true -> s t
      | Bool false -> s f
      | c' -> If (c', s t, s f))
  | Uniform (m, a, b) -> Uniform (m, s a, s b)
  | Cons (h, t) -> Cons (s h, s t)
  | Var _ | Float _ | Bool _ | Flip | Nil -> expr

let mean_of_random = function
  | RUniform (a, b) -> simplify (Div (Add (a, b), Float 2.0))

let rec determinize expr =
  let d = determinize in
  match expr with
  | Var _ | Float _ | Bool _ | Flip | Nil -> expr
  | Let (x, e1, e2) -> Let (x, d e1, d e2)
  | If (c, t, f) -> If (d c, d t, d f)
  | Add (a, b) -> simplify (Add (d a, d b))
  | Div (a, b) -> simplify (Div (d a, d b))
  | Lt (a, b) -> simplify (Lt (d a, d b))
  | Uniform (E, a, b) ->
      mean_of_random (RUniform (d a, d b))
  | Uniform (G, a, b) -> Uniform (G, d a, d b)
  | Cons (h, t) -> Cons (d h, d t)

let sample_uniform ctx a b =
  let name = fresh_sample ctx in
  let sample = { name; random = RUniform (a, b) } in
  Sample (sample, Return (Var name))

let rec step_expr ctx expr =
  match expr with
  | e when is_value e -> Return e
  | Let (x, e1, e2) when is_value e1 ->
      Return (subst x e1 e2)
  | Let (x, e1, e2) ->
      measure_map (fun e1' -> Let (x, e1', e2)) (step_expr ctx e1)
  | If (Bool true, t, _) -> Return t
  | If (Bool false, _, f) -> Return f
  | If (c, t, f) ->
      measure_map (fun c' -> If (c', t, f)) (step_expr ctx c)
  | Add (a, b) when not (is_value a) ->
      measure_map (fun a' -> Add (a', b)) (step_expr ctx a)
  | Add (a, b) when not (is_value b) ->
      measure_map (fun b' -> Add (a, b')) (step_expr ctx b)
  | Add _ -> Return (simplify expr)
  | Div (a, b) when not (is_value a) ->
      measure_map (fun a' -> Div (a', b)) (step_expr ctx a)
  | Div (a, b) when not (is_value b) ->
      measure_map (fun b' -> Div (a, b')) (step_expr ctx b)
  | Div _ -> Return (simplify expr)
  | Lt (a, b) when not (is_value a) ->
      measure_map (fun a' -> Lt (a', b)) (step_expr ctx a)
  | Lt (a, b) when not (is_value b) ->
      measure_map (fun b' -> Lt (a, b')) (step_expr ctx b)
  | Lt _ -> Return (simplify expr)
  | Uniform (m, a, b) when not (is_value a) ->
      measure_map (fun a' -> Uniform (m, a', b)) (step_expr ctx a)
  | Uniform (m, a, b) when not (is_value b) ->
      measure_map (fun b' -> Uniform (m, a, b')) (step_expr ctx b)
  | Uniform (_, a, b) ->
      sample_uniform ctx a b
  | Flip ->
      Choice [ (0.5, Return (Bool true)); (0.5, Return (Bool false)) ]
  | Cons (h, t) when not (is_value h) ->
      measure_map (fun h' -> Cons (h', t)) (step_expr ctx h)
  | Cons (h, t) when not (is_value t) ->
      measure_map (fun t' -> Cons (h, t')) (step_expr ctx t)
  | Cons _ | Var _ | Float _ | Bool _ | Nil ->
      Return expr

let rec step_sym_expr ctx expr =
  match expr with
  | e when is_value e -> Return ([], e)
  | Let (x, e1, e2) when is_value e1 ->
      Return ([], subst x e1 e2)
  | Let (x, e1, e2) ->
      measure_map
        (fun (new_samples, e1') -> (new_samples, Let (x, e1', e2)))
        (step_sym_expr ctx e1)
  | If (Bool true, t, _) -> Return ([], t)
  | If (Bool false, _, f) -> Return ([], f)
  | If (c, t, f) ->
      measure_map
        (fun (new_samples, c') -> (new_samples, If (c', t, f)))
        (step_sym_expr ctx c)
  | Add (a, b) when not (is_value a) ->
      measure_map
        (fun (new_samples, a') -> (new_samples, Add (a', b)))
        (step_sym_expr ctx a)
  | Add (a, b) when not (is_value b) ->
      measure_map
        (fun (new_samples, b') -> (new_samples, Add (a, b')))
        (step_sym_expr ctx b)
  | Add _ -> Return ([], simplify expr)
  | Div (a, b) when not (is_value a) ->
      measure_map
        (fun (new_samples, a') -> (new_samples, Div (a', b)))
        (step_sym_expr ctx a)
  | Div (a, b) when not (is_value b) ->
      measure_map
        (fun (new_samples, b') -> (new_samples, Div (a, b')))
        (step_sym_expr ctx b)
  | Div _ -> Return ([], simplify expr)
  | Lt (a, b) when not (is_value a) ->
      measure_map
        (fun (new_samples, a') -> (new_samples, Lt (a', b)))
        (step_sym_expr ctx a)
  | Lt (a, b) when not (is_value b) ->
      measure_map
        (fun (new_samples, b') -> (new_samples, Lt (a, b')))
        (step_sym_expr ctx b)
  | Lt _ -> Return ([], simplify expr)
  | Uniform (m, a, b) when not (is_value a) ->
      measure_map
        (fun (new_samples, a') -> (new_samples, Uniform (m, a', b)))
        (step_sym_expr ctx a)
  | Uniform (m, a, b) when not (is_value b) ->
      measure_map
        (fun (new_samples, b') -> (new_samples, Uniform (m, a, b')))
        (step_sym_expr ctx b)
  | Uniform (E, a, b) ->
      let name = fresh_sample ctx in
      let sample = { name; random = RUniform (a, b) } in
      Return ([ sample ], Var name)
  | Uniform (G, a, b) ->
      measure_map (fun e -> ([], e)) (sample_uniform ctx a b)
  | Flip ->
      Choice
        [ (0.5, Return ([], Bool true)); (0.5, Return ([], Bool false)) ]
  | Cons (h, t) when not (is_value h) ->
      measure_map
        (fun (new_samples, h') -> (new_samples, Cons (h', t)))
        (step_sym_expr ctx h)
  | Cons (h, t) when not (is_value t) ->
      measure_map
        (fun (new_samples, t') -> (new_samples, Cons (h, t')))
        (step_sym_expr ctx t)
  | Cons _ | Var _ | Float _ | Bool _ | Nil ->
      Return ([], expr)

let step_sym_state ctx st =
  measure_map
    (fun (new_samples, residual) ->
      { sigma = st.sigma @ new_samples; residual })
    (step_sym_expr ctx st.residual)

let rec nstep step ctx n start =
  if n <= 0 then Return start
  else
    let previous = nstep step ctx (n - 1) start in
    measure_bind previous (step ctx)

let nstep_expr ctx n expr = nstep step_expr ctx n expr

let nstep_sym ctx n state = nstep step_sym_state ctx n state

let interpret_actual_state st =
  List.fold_right
    (fun sample acc -> Sample (sample, acc))
    st.sigma
    (Return st.residual)

let interpret_expected_state st =
  let env =
    List.fold_left
      (fun env sample ->
        let random = subst_random env sample.random in
        let mean = mean_of_random random in
        (sample.name, mean) :: env)
      [] st.sigma
  in
  Return (simplify (subst_many env st.residual))

let simplify_measure m = measure_map simplify m

let compare_measures lhs rhs =
  String.equal (measure_to_string expr_to_string lhs)
    (measure_to_string expr_to_string rhs)

let symbolic_actual_view symbolic =
  simplify_measure (measure_bind symbolic interpret_actual_state)

let symbolic_expected_view symbolic =
  simplify_measure (measure_bind symbolic interpret_expected_state)

let print_symbolic_trace fuel expr =
  let ctx = empty_context () in
  let rec loop step symbolic =
    Printf.printf "\n-- symbolic step %d --\n" step;
    Printf.printf "symbolic states: %s\n"
      (measure_to_string sym_state_to_string symbolic);
    Printf.printf "actual view:     %s\n"
      (measure_to_string expr_to_string (symbolic_actual_view symbolic));
    Printf.printf "expected view:   %s\n"
      (measure_to_string expr_to_string (symbolic_expected_view symbolic));
    if step < fuel then
      loop (step + 1) (measure_bind symbolic (step_sym_state ctx))
  in
  loop 0 (Return { sigma = []; residual = expr })

let run_case trace name fuel expr =
  let original =
    let ctx = empty_context () in
    simplify_measure (nstep_expr ctx fuel expr)
  in
  let symbolic =
    let ctx = empty_context () in
    nstep_sym ctx fuel { sigma = []; residual = expr }
  in
  let symbolic_actual = symbolic_actual_view symbolic in
  let symbolic_expected = symbolic_expected_view symbolic in
  let determinized =
    let ctx = empty_context () in
    simplify_measure (nstep_expr ctx fuel (determinize expr))
  in
  let actual_ok = compare_measures original symbolic_actual in
  let expected_ok = compare_measures determinized symbolic_expected in
  Printf.printf "\n=== %s ===\n" name;
  Printf.printf "source:       %s\n" (expr_to_string expr);
  Printf.printf "determinized: %s\n" (expr_to_string (determinize expr));
  if trace then print_symbolic_trace fuel expr;
  Printf.printf "actual coupling:   %s\n" (if actual_ok then "OK" else "FAIL");
  Printf.printf "expected coupling: %s\n" (if expected_ok then "OK" else "FAIL");
  if not actual_ok then (
    Printf.printf "original:        %s\n"
      (measure_to_string expr_to_string original);
    Printf.printf "symbolic actual: %s\n"
      (measure_to_string expr_to_string symbolic_actual));
  if not expected_ok then (
    Printf.printf "determinized:      %s\n"
      (measure_to_string expr_to_string determinized);
    Printf.printf "symbolic expected: %s\n"
      (measure_to_string expr_to_string symbolic_expected));
  actual_ok && expected_ok

let read_file path =
  let channel = open_in path in
  try
    let contents = really_input_string channel (in_channel_length channel) in
    close_in channel;
    contents
  with exn ->
    close_in_noerr channel;
    raise exn

let parse_file path =
  try parse_source (read_file path)
  with Parse_error msg -> parse_error (path ^ ": " ^ msg)

let has_prefix prefix text =
  let prefix_len = String.length prefix in
  String.length text >= prefix_len
  && String.sub text 0 prefix_len = prefix

let parse_fuel text =
  try
    let fuel = int_of_string text in
    if fuel < 0 then failwith "negative";
    fuel
  with Failure _ -> parse_error ("invalid fuel value " ^ text)

let usage () =
  Printf.printf
    "Usage: symbolic_coupling [--fuel N] [--trace] [file ...]\n\n\
     With no files, runs the built-in examples.\n\
     --trace prints each symbolic small step and its actual/expected views.\n\
     File syntax includes: let x = e in e, if e then e else e,\n\
     uniform[E](a,b), uniform[G](a,b), flip(), [], h :: t,\n\
     +, /, <, parentheses, # comments, and // comments.\n\
     uniform(a,b) defaults to uniform[E](a,b).\n"

let parse_cli argv =
  let fuel = ref 12 in
  let trace = ref false in
  let files = ref [] in
  let i = ref 1 in
  while !i < Array.length argv do
    let arg = argv.(!i) in
    if String.equal arg "--fuel" then (
      if !i + 1 >= Array.length argv then parse_error "--fuel needs a value";
      fuel := parse_fuel argv.(!i + 1);
      i := !i + 2)
    else if has_prefix "--fuel=" arg then (
      fuel := parse_fuel (String.sub arg 7 (String.length arg - 7));
      incr i)
    else if String.equal arg "--trace" then (
      trace := true;
      incr i)
    else if String.equal arg "--help" || String.equal arg "-h" then (
      usage ();
      exit 0)
    else (
      files := arg :: !files;
      incr i)
  done;
  (!fuel, !trace, List.rev !files)

let f x = Float x

let uniform_e a b = Uniform (E, a, b)

let uniform_g a b = Uniform (G, a, b)

let example_dependent_uniform =
  Let
    ( "x",
      uniform_e (f 0.0) (f 1.0),
      Let
        ( "y",
          uniform_e (Var "x") (f 2.0),
          Add (Var "x", Var "y") ) )

let example_flip_kept =
  Let
    ( "b",
      Flip,
      If
        ( Var "b",
          uniform_e (f 0.0) (f 1.0),
          uniform_e (f 2.0) (f 4.0) ) )

let example_mixed_modes =
  Let
    ( "g",
      uniform_g (f 0.0) (f 1.0),
      Let
        ( "x",
          uniform_e (Var "g") (f 2.0),
          Add (Var "g", Var "x") ) )

let builtin_cases fuel =
    [
      ("dependent expectation samples", fuel, example_dependent_uniform);
      ("flip retained, branch samples determinized", fuel, example_flip_kept);
      ( "general sample retained, dependent expectation sample",
        fuel,
        example_mixed_modes );
    ]

let file_cases fuel paths =
  List.map (fun path -> (path, fuel, parse_file path)) paths

let () =
  let fuel, trace, files =
    try parse_cli Sys.argv
    with Parse_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 2
  in
  let cases =
    try
      match files with
      | [] -> builtin_cases fuel
      | paths -> file_cases fuel paths
    with
    | Sys_error msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 2
    | Parse_error msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 2
  in
  let results =
    List.map (fun (name, fuel, expr) -> run_case trace name fuel expr) cases
  in
  if List.for_all Fun.id results then (
    Printf.printf "\nAll symbolic coupling checks passed.\n";
    exit 0)
  else (
    Printf.printf "\nSome symbolic coupling checks failed.\n";
    exit 1)
