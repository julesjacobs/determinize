(* Standalone prototype for the symbolic coupling idea.

   This file intentionally does not depend on the rest of the implementation.
   Compile and run it directly with:

     ocamlc -o /tmp/symbolic_coupling symbolic_coupling.ml
     /tmp/symbolic_coupling
     /tmp/symbolic_coupling --fuel 12 source.det determinized.det

   The prototype models the parser.mly expression language, plus explicit
   [E]/[G] sample-mode annotations for this coupling experiment.  It compares:

   - ordinary small-step semantics;
   - symbolic small-step semantics, where expectation-mode samples are recorded
     in a symbolic environment instead of sampled immediately;
   - small-step semantics for a provided determinized program.

   The symbolic state [<sigma || e>] has two interpretations:

   - actual: sample every symbolic binding in sigma, then return e;
   - expected: replace every symbolic binding by its expectation, then return e.

   The examples at the bottom check, up to the printed symbolic distribution
   representation, that:

   - original semantics = actual interpretation of symbolic semantics;
   - provided determinized semantics = expected interpretation of symbolic
     semantics.

   To run:
     $ /private/tmp/symbolic_coupling --fuel 5 --trace source.det determinized.det
*)

type mode = E | G

type expr =
  | Var of string
  | Lam of string * expr
  | Rec of string * string * expr
  | App of expr * expr
  | Unit
  | Float of float
  | Bool of bool
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Inl of expr
  | Inr of expr
  | Case of expr * (string * expr) * (string * expr)
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Neg of expr
  | Lt of expr * expr
  | Leq of expr * expr
  | Uniform of mode * expr * expr
  | Gauss of mode * expr * expr
  | Exponential of mode * expr
  | Gamma of mode * expr * expr
  | Beta of mode * expr * expr
  | Flip of expr
  | Bernoulli of mode * expr
  | Poisson of mode * expr
  | Discrete of mode * (float * expr) list
  | Observe of expr
  | Nil
  | Cons of expr * expr
  | MatchList of expr * expr * (string * string * expr)

type random =
  | RUniform of expr * expr
  | RGauss of expr * expr
  | RExponential of expr
  | RGamma of expr * expr
  | RBeta of expr * expr
  | RBernoulli of expr
  | RPoisson of expr
  | RDiscrete of (float * expr) list

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

let mode_suffix m = "[" ^ mode_to_string m ^ "]"

let rec expr_to_string = function
  | Var x -> x
  | Lam (x, body) -> "fun " ^ x ^ " => " ^ expr_to_string body
  | Rec (f, x, body) -> "rec " ^ f ^ " " ^ x ^ " => " ^ expr_to_string body
  | App (fn, arg) ->
      "(" ^ expr_to_string fn ^ " " ^ expr_to_string arg ^ ")"
  | Unit -> "()"
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
  | Pair (a, b) ->
      "(" ^ expr_to_string a ^ ", " ^ expr_to_string b ^ ")"
  | Fst e -> "fst " ^ expr_to_string e
  | Snd e -> "snd " ^ expr_to_string e
  | Inl e -> "inl " ^ expr_to_string e
  | Inr e -> "inr " ^ expr_to_string e
  | Case (scrut, (x, left), (y, right)) ->
      "match " ^ expr_to_string scrut ^ " with inl " ^ x ^ " => "
      ^ expr_to_string left ^ " | inr " ^ y ^ " => "
      ^ expr_to_string right
  | Add (a, b) ->
      "(" ^ expr_to_string a ^ " + " ^ expr_to_string b ^ ")"
  | Mul (a, b) ->
      "(" ^ expr_to_string a ^ " * " ^ expr_to_string b ^ ")"
  | Sub (a, b) ->
      "(" ^ expr_to_string a ^ " - " ^ expr_to_string b ^ ")"
  | Div (a, b) ->
      "(" ^ expr_to_string a ^ " / " ^ expr_to_string b ^ ")"
  | Neg e -> "(-" ^ expr_to_string e ^ ")"
  | Lt (a, b) ->
      "(" ^ expr_to_string a ^ " < " ^ expr_to_string b ^ ")"
  | Leq (a, b) ->
      "(" ^ expr_to_string a ^ " <= " ^ expr_to_string b ^ ")"
  | Uniform (m, a, b) ->
      "uniform" ^ mode_suffix m ^ "(" ^ expr_to_string a ^ ", "
      ^ expr_to_string b ^ ")"
  | Gauss (m, a, b) ->
      "gauss" ^ mode_suffix m ^ "(" ^ expr_to_string a ^ ", "
      ^ expr_to_string b ^ ")"
  | Exponential (m, e) ->
      "exponential" ^ mode_suffix m ^ "(" ^ expr_to_string e ^ ")"
  | Gamma (m, a, b) ->
      "gamma" ^ mode_suffix m ^ "(" ^ expr_to_string a ^ ", "
      ^ expr_to_string b ^ ")"
  | Beta (m, a, b) ->
      "beta" ^ mode_suffix m ^ "(" ^ expr_to_string a ^ ", "
      ^ expr_to_string b ^ ")"
  | Flip p -> "flip(" ^ expr_to_string p ^ ")"
  | Bernoulli (m, p) ->
      "bernoulli" ^ mode_suffix m ^ "(" ^ expr_to_string p ^ ")"
  | Poisson (m, p) ->
      "poisson" ^ mode_suffix m ^ "(" ^ expr_to_string p ^ ")"
  | Discrete (m, cases) ->
      let probs =
        cases
        |> List.map (fun (p, _) -> Printf.sprintf "%.6g" p)
        |> String.concat ", "
      in
      "discrete" ^ mode_suffix m ^ "(" ^ probs ^ ")"
  | Observe c -> "observe(" ^ expr_to_string c ^ ")"
  | Nil -> "[]"
  | Cons (h, t) ->
      "(" ^ expr_to_string h ^ " :: " ^ expr_to_string t ^ ")"
  | MatchList (scrut, nil_branch, (x, xs, cons_branch)) ->
      "match " ^ expr_to_string scrut ^ " with [] => "
      ^ expr_to_string nil_branch ^ " | " ^ x ^ " :: " ^ xs ^ " => "
      ^ expr_to_string cons_branch

type token =
  | TLet
  | TIn
  | TIf
  | TThen
  | TElse
  | TFun
  | TRec
  | TMatch
  | TWith
  | TInl
  | TInr
  | TFst
  | TSnd
  | TUniform
  | TGauss
  | TExponential
  | TGamma
  | TBeta
  | TFlip
  | TBernoulli
  | TPoisson
  | TDiscrete
  | TObserve
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
  | TBar
  | TDArrow
  | TPlus
  | TTimes
  | TMinus
  | TSlash
  | TLt
  | TLeq
  | TGt
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
  | TFun -> "fun"
  | TRec -> "rec"
  | TMatch -> "match"
  | TWith -> "with"
  | TInl -> "inl"
  | TInr -> "inr"
  | TFst -> "fst"
  | TSnd -> "snd"
  | TUniform -> "uniform"
  | TGauss -> "gauss"
  | TExponential -> "exponential"
  | TGamma -> "gamma"
  | TBeta -> "beta"
  | TFlip -> "flip"
  | TBernoulli -> "bernoulli"
  | TPoisson -> "poisson"
  | TDiscrete -> "discrete"
  | TObserve -> "observe"
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
  | TBar -> "|"
  | TDArrow -> "=>"
  | TPlus -> "+"
  | TTimes -> "*"
  | TMinus -> "-"
  | TSlash -> "/"
  | TLt -> "<"
  | TLeq -> "<="
  | TGt -> ">"
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
  | "fun" -> TFun
  | "rec" -> TRec
  | "match" -> TMatch
  | "with" -> TWith
  | "inl" -> TInl
  | "inr" -> TInr
  | "fst" -> TFst
  | "snd" -> TSnd
  | "uniform" -> TUniform
  | "gauss" -> TGauss
  | "gaussian" -> TGauss
  | "exponential" -> TExponential
  | "gamma" -> TGamma
  | "beta" -> TBeta
  | "flip" -> TFlip
  | "bernoulli" -> TBernoulli
  | "poisson" -> TPoisson
  | "discrete" -> TDiscrete
  | "observe" -> TObserve
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
    | Some '.' -> (
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
      | '=' when i + 1 < len && source.[i + 1] = '>' ->
          go (i + 2) (TDArrow :: acc)
      | '=' -> go (i + 1) (TEqual :: acc)
      | '|' -> go (i + 1) (TBar :: acc)
      | '+' -> go (i + 1) (TPlus :: acc)
      | '*' -> go (i + 1) (TTimes :: acc)
      | '-' -> go (i + 1) (TMinus :: acc)
      | '/' -> go (i + 1) (TSlash :: acc)
      | '<' when i + 1 < len && source.[i + 1] = '=' ->
          go (i + 2) (TLeq :: acc)
      | '<' -> go (i + 1) (TLt :: acc)
      | '>' -> go (i + 1) (TGt :: acc)
      | ':' when i + 1 < len && source.[i + 1] = ':' ->
          go (i + 2) (TCons :: acc)
      | _ when starts_number i ->
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
  | TFun, TFun
  | TRec, TRec
  | TMatch, TMatch
  | TWith, TWith
  | TInl, TInl
  | TInr, TInr
  | TFst, TFst
  | TSnd, TSnd
  | TUniform, TUniform
  | TGauss, TGauss
  | TExponential, TExponential
  | TGamma, TGamma
  | TBeta, TBeta
  | TFlip, TFlip
  | TBernoulli, TBernoulli
  | TPoisson, TPoisson
  | TDiscrete, TDiscrete
  | TObserve, TObserve
  | TTrue, TTrue
  | TFalse, TFalse
  | TLParen, TLParen
  | TRParen, TRParen
  | TLBracket, TLBracket
  | TRBracket, TRBracket
  | TComma, TComma
  | TEqual, TEqual
  | TBar, TBar
  | TDArrow, TDArrow
  | TPlus, TPlus
  | TTimes, TTimes
  | TMinus, TMinus
  | TSlash, TSlash
  | TLt, TLt
  | TLeq, TLeq
  | TGt, TGt
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

let rec parse_expression parser = parse_control parser

and parse_control parser =
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
  | TMatch ->
      ignore (take_token parser);
      let scrut = parse_expression parser in
      expect parser TWith;
      parse_match_branches parser scrut
  | _ -> parse_fun parser

and parse_match_branches parser scrut =
  match peek_token parser with
  | TInl ->
      ignore (take_token parser);
      let x = parse_identifier parser in
      expect parser TDArrow;
      let left = parse_expression parser in
      expect parser TBar;
      expect parser TInr;
      let y = parse_identifier parser in
      expect parser TDArrow;
      let right = parse_expression parser in
      Case (scrut, (x, left), (y, right))
  | TLBracket ->
      ignore (take_token parser);
      expect parser TRBracket;
      expect parser TDArrow;
      let nil_branch = parse_expression parser in
      expect parser TBar;
      let x = parse_identifier parser in
      expect parser TCons;
      let xs = parse_identifier parser in
      expect parser TDArrow;
      let cons_branch = parse_expression parser in
      MatchList (scrut, nil_branch, (x, xs, cons_branch))
  | found ->
      parse_error
        ("expected inl branch or [] branch, found " ^ token_to_string found)

and parse_fun parser =
  match peek_token parser with
  | TFun ->
      ignore (take_token parser);
      let x = parse_identifier parser in
      expect parser TDArrow;
      Lam (x, parse_expression parser)
  | TRec ->
      ignore (take_token parser);
      let f = parse_identifier parser in
      let x = parse_identifier parser in
      expect parser TDArrow;
      Rec (f, x, parse_expression parser)
  | _ -> parse_compare parser

and parse_compare parser =
  let rec loop acc =
    match peek_token parser with
    | TLt ->
        ignore (take_token parser);
        loop (Lt (acc, parse_cons parser))
    | TLeq ->
        ignore (take_token parser);
        loop (Leq (acc, parse_cons parser))
    | TGt ->
        ignore (take_token parser);
        loop (Lt (parse_cons parser, acc))
    | _ -> acc
  in
  loop (parse_cons parser)

and parse_cons parser =
  let head = parse_add parser in
  if accept parser TCons then Cons (head, parse_cons parser) else head

and parse_add parser =
  let rec loop acc =
    match peek_token parser with
    | TPlus ->
        ignore (take_token parser);
        loop (Add (acc, parse_mul parser))
    | TMinus ->
        ignore (take_token parser);
        loop (Sub (acc, parse_mul parser))
    | _ -> acc
  in
  loop (parse_mul parser)

and parse_mul parser =
  let rec loop acc =
    match peek_token parser with
    | TTimes ->
        ignore (take_token parser);
        loop (Mul (acc, parse_unary parser))
    | TSlash ->
        ignore (take_token parser);
        loop (Div (acc, parse_unary parser))
    | _ -> acc
  in
  loop (parse_unary parser)

and parse_unary parser =
  match peek_token parser with
  | TMinus ->
      ignore (take_token parser);
      Neg (parse_unary parser)
  | _ -> parse_app parser

and starts_atom = function
  | TIdent _ | TFloat _ | TTrue | TFalse | TLParen | TLBracket | TFst | TSnd
  | TInl | TInr | TUniform | TGauss | TExponential | TGamma | TBeta | TFlip
  | TBernoulli | TPoisson | TDiscrete | TObserve ->
      true
  | _ -> false

and parse_app parser =
  let rec loop acc =
    if starts_atom (peek_token parser) then loop (App (acc, parse_atom parser))
    else acc
  in
  loop (parse_atom parser)

and parse_optional_mode parser =
  match peek_token parser with
  | TLBracket -> parse_mode parser
  | _ -> E

and parse_two_arguments parser =
  expect parser TLParen;
  let first = parse_expression parser in
  expect parser TComma;
  let second = parse_expression parser in
  expect parser TRParen;
  (first, second)

and parse_one_argument parser =
  expect parser TLParen;
  let arg = parse_expression parser in
  expect parser TRParen;
  arg

and parse_flip_argument parser =
  expect parser TLParen;
  if accept parser TRParen then Float 0.5
  else
    let arg = parse_expression parser in
    expect parser TRParen;
    arg

and parse_discrete_cases parser =
  let rec loop index acc =
    match take_token parser with
    | TFloat p ->
        let acc = (p, Float (float_of_int index)) :: acc in
        if accept parser TComma then loop (index + 1) acc else List.rev acc
    | found ->
        parse_error
          ("expected probability in discrete(...), found "
          ^ token_to_string found)
  in
  expect parser TLParen;
  let cases =
    match peek_token parser with
    | TRParen -> parse_error "discrete(...) needs at least one probability"
    | _ -> loop 0 []
  in
  expect parser TRParen;
  cases

and parse_atom parser =
  match take_token parser with
  | TFloat f -> Float f
  | TTrue -> Bool true
  | TFalse -> Bool false
  | TFun ->
      parser.position <- parser.position - 1;
      parse_fun parser
  | TFst -> Fst (parse_atom parser)
  | TSnd -> Snd (parse_atom parser)
  | TInl -> Inl (parse_atom parser)
  | TInr -> Inr (parse_atom parser)
  | TUniform ->
      let mode = parse_optional_mode parser in
      let a, b = parse_two_arguments parser in
      Uniform (mode, a, b)
  | TIdent "uniformE" ->
      let a, b = parse_two_arguments parser in
      Uniform (E, a, b)
  | TIdent "uniformG" ->
      let a, b = parse_two_arguments parser in
      Uniform (G, a, b)
  | TGauss ->
      let mode = parse_optional_mode parser in
      let a, b = parse_two_arguments parser in
      Gauss (mode, a, b)
  | TExponential ->
      let mode = parse_optional_mode parser in
      Exponential (mode, parse_one_argument parser)
  | TGamma ->
      let mode = parse_optional_mode parser in
      let a, b = parse_two_arguments parser in
      Gamma (mode, a, b)
  | TBeta ->
      let mode = parse_optional_mode parser in
      let a, b = parse_two_arguments parser in
      Beta (mode, a, b)
  | TFlip -> Flip (parse_flip_argument parser)
  | TBernoulli ->
      let mode = parse_optional_mode parser in
      Bernoulli (mode, parse_one_argument parser)
  | TPoisson ->
      let mode = parse_optional_mode parser in
      Poisson (mode, parse_one_argument parser)
  | TDiscrete ->
      let mode = parse_optional_mode parser in
      Discrete (mode, parse_discrete_cases parser)
  | TObserve -> Observe (parse_one_argument parser)
  | TIdent "nil" -> Nil
  | TIdent x -> Var x
  | TLBracket ->
      expect parser TRBracket;
      Nil
  | TLParen ->
      if accept parser TRParen then Unit
      else
        let expr = parse_expression parser in
        if accept parser TComma then (
          let second = parse_expression parser in
          expect parser TRParen;
          Pair (expr, second))
        else (
          expect parser TRParen;
          expr)
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
  | RGauss (a, b) ->
      "gauss(" ^ expr_to_string a ^ ", " ^ expr_to_string b ^ ")"
  | RExponential e ->
      "exponential(" ^ expr_to_string e ^ ")"
  | RGamma (a, b) ->
      "gamma(" ^ expr_to_string a ^ ", " ^ expr_to_string b ^ ")"
  | RBeta (a, b) ->
      "beta(" ^ expr_to_string a ^ ", " ^ expr_to_string b ^ ")"
  | RBernoulli p ->
      "bernoulli(" ^ expr_to_string p ^ ")"
  | RPoisson p ->
      "poisson(" ^ expr_to_string p ^ ")"
  | RDiscrete cases ->
      let probs =
        cases
        |> List.map (fun (p, _) -> Printf.sprintf "%.6g" p)
        |> String.concat ", "
      in
      "discrete(" ^ probs ^ ")"

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
  | Neg e -> is_float_term e
  | Add (a, b) | Mul (a, b) | Sub (a, b) | Div (a, b) ->
      is_float_term a && is_float_term b
  | _ -> false

let rec is_value = function
  | Float _ | Bool _ | Var _ | Unit | Lam _ | Rec _ | Nil -> true
  | Neg e -> is_float_term e
  | Add (a, b) | Mul (a, b) | Sub (a, b) | Div (a, b) ->
      is_float_term a && is_float_term b
  | Pair (a, b) -> is_value a && is_value b
  | Inl e | Inr e -> is_value e
  | Cons (h, t) -> is_value h && is_value t
  | _ -> false

let rec subst x replacement expr =
  let go = subst x replacement in
  match expr with
  | Var y when String.equal x y -> replacement
  | Var _ | Float _ | Bool _ | Unit | Nil -> expr
  | Lam (y, body) ->
      Lam (y, if String.equal x y then body else go body)
  | Rec (f, y, body) ->
      Rec (f, y, if String.equal x f || String.equal x y then body else go body)
  | App (fn, arg) -> App (go fn, go arg)
  | Let (y, e1, e2) ->
      Let (y, go e1, if String.equal x y then e2 else go e2)
  | If (c, t, f) -> If (go c, go t, go f)
  | Pair (a, b) -> Pair (go a, go b)
  | Fst e -> Fst (go e)
  | Snd e -> Snd (go e)
  | Inl e -> Inl (go e)
  | Inr e -> Inr (go e)
  | Case (scrut, (y, left), (z, right)) ->
      Case
        ( go scrut,
          (y, if String.equal x y then left else go left),
          (z, if String.equal x z then right else go right) )
  | MatchList (scrut, nil_branch, (h, tl, cons_branch)) ->
      MatchList
        ( go scrut,
          go nil_branch,
          ( h,
            tl,
            if String.equal x h || String.equal x tl then cons_branch
            else go cons_branch ) )
  | Neg e -> Neg (go e)
  | Add (a, b) -> Add (go a, go b)
  | Mul (a, b) -> Mul (go a, go b)
  | Sub (a, b) -> Sub (go a, go b)
  | Div (a, b) -> Div (go a, go b)
  | Lt (a, b) -> Lt (go a, go b)
  | Leq (a, b) -> Leq (go a, go b)
  | Uniform (m, a, b) -> Uniform (m, go a, go b)
  | Gauss (m, a, b) -> Gauss (m, go a, go b)
  | Exponential (m, e) -> Exponential (m, go e)
  | Gamma (m, a, b) -> Gamma (m, go a, go b)
  | Beta (m, a, b) -> Beta (m, go a, go b)
  | Flip p -> Flip (go p)
  | Bernoulli (m, p) -> Bernoulli (m, go p)
  | Poisson (m, p) -> Poisson (m, go p)
  | Discrete (m, cases) -> Discrete (m, List.map (fun (p, e) -> (p, go e)) cases)
  | Observe c -> Observe (go c)
  | Cons (h, t) -> Cons (go h, go t)

let subst_many env expr =
  List.fold_left (fun acc (x, replacement) -> subst x replacement acc) expr env

let subst_random env = function
  | RUniform (a, b) -> RUniform (subst_many env a, subst_many env b)
  | RGauss (a, b) -> RGauss (subst_many env a, subst_many env b)
  | RExponential e -> RExponential (subst_many env e)
  | RGamma (a, b) -> RGamma (subst_many env a, subst_many env b)
  | RBeta (a, b) -> RBeta (subst_many env a, subst_many env b)
  | RBernoulli p -> RBernoulli (subst_many env p)
  | RPoisson p -> RPoisson (subst_many env p)
  | RDiscrete cases ->
      RDiscrete (List.map (fun (p, e) -> (p, subst_many env e)) cases)

let rec simplify expr =
  let s = simplify in
  match expr with
  | Lam (x, body) -> Lam (x, s body)
  | Rec (f, x, body) -> Rec (f, x, s body)
  | App (fn, arg) -> App (s fn, s arg)
  | Pair (a, b) -> Pair (s a, s b)
  | Fst e -> (
      match s e with
      | Pair (a, _) when is_value a -> a
      | e' -> Fst e')
  | Snd e -> (
      match s e with
      | Pair (_, b) when is_value b -> b
      | e' -> Snd e')
  | Inl e -> Inl (s e)
  | Inr e -> Inr (s e)
  | Case (scrut, (x, left), (y, right)) -> (
      match s scrut with
      | Inl v when is_value v -> s (subst x v left)
      | Inr v when is_value v -> s (subst y v right)
      | scrut' -> Case (scrut', (x, s left), (y, s right)))
  | MatchList (scrut, nil_branch, (h, tl, cons_branch)) -> (
      match s scrut with
      | Nil -> s nil_branch
      | Cons (head, tail) when is_value head && is_value tail ->
          s (subst tl tail (subst h head cons_branch))
      | scrut' -> MatchList (scrut', s nil_branch, (h, tl, s cons_branch)))
  | Neg e -> (
      match s e with
      | Float x -> Float (-. x)
      | e' -> Neg e')
  | Add (a, b) -> (
      match (s a, s b) with
      | Float x, Float y -> Float (x +. y)
      | Float 0.0, e | e, Float 0.0 -> e
      | a', b' -> Add (a', b'))
  | Mul (a, b) -> (
      match (s a, s b) with
      | Float x, Float y -> Float (x *. y)
      | Float 0.0, _ | _, Float 0.0 -> Float 0.0
      | Float 1.0, e | e, Float 1.0 -> e
      | a', b' -> Mul (a', b'))
  | Sub (a, b) -> (
      match (s a, s b) with
      | Float x, Float y -> Float (x -. y)
      | e, Float 0.0 -> e
      | a', b' -> Sub (a', b'))
  | Div (a, b) -> (
      match (s a, s b) with
      | Float x, Float y -> Float (x /. y)
      | a', Float 1.0 -> a'
      | a', b' -> Div (a', b'))
  | Lt (a, b) -> (
      match (s a, s b) with
      | Float x, Float y -> Bool (x < y)
      | a', b' -> Lt (a', b'))
  | Leq (a, b) -> (
      match (s a, s b) with
      | Float x, Float y -> Bool (x <= y)
      | a', b' -> Leq (a', b'))
  | Let (x, e1, e2) -> Let (x, s e1, s e2)
  | If (c, t, f) -> (
      match s c with
      | Bool true -> s t
      | Bool false -> s f
      | c' -> If (c', s t, s f))
  | Uniform (m, a, b) -> Uniform (m, s a, s b)
  | Gauss (m, a, b) -> Gauss (m, s a, s b)
  | Exponential (m, e) -> Exponential (m, s e)
  | Gamma (m, a, b) -> Gamma (m, s a, s b)
  | Beta (m, a, b) -> Beta (m, s a, s b)
  | Flip p -> Flip (s p)
  | Bernoulli (m, p) -> Bernoulli (m, s p)
  | Poisson (m, p) -> Poisson (m, s p)
  | Discrete (m, cases) -> Discrete (m, List.map (fun (p, e) -> (p, s e)) cases)
  | Observe c -> Observe (s c)
  | Cons (h, t) -> Cons (s h, s t)
  | Var _ | Float _ | Bool _ | Unit | Nil -> expr

let weighted_sum terms =
  let rec go = function
    | [] -> Float 0.0
    | [ (p, e) ] -> Mul (Float p, e)
    | (p, e) :: rest -> Add (Mul (Float p, e), go rest)
  in
  simplify (go terms)

let mean_of_random = function
  | RUniform (a, b) -> simplify (Div (Add (a, b), Float 2.0))
  | RGauss (mean, _) -> simplify mean
  | RExponential rate -> simplify (Div (Float 1.0, rate))
  | RGamma (shape, rate) -> simplify (Div (shape, rate))
  | RBeta (a, b) -> simplify (Div (a, Add (a, b)))
  | RBernoulli p -> simplify p
  | RPoisson p -> simplify p
  | RDiscrete cases -> weighted_sum cases

let sample_random ctx random =
  let name = fresh_sample ctx in
  let sample = { name; random } in
  Sample (sample, Return (Var name))

let sample_uniform ctx a b = sample_random ctx (RUniform (a, b))

let record_symbolic_sample ctx random =
  let name = fresh_sample ctx in
  let sample = { name; random } in
  Return ([ sample ], Var name)

let sample_runtime_random ctx random =
  measure_map (fun e -> ([], e)) (sample_random ctx random)

let rec step_expr ctx expr =
  let step_unary make arg =
    measure_map (fun arg' -> make arg') (step_expr ctx arg)
  in
  let step_binary make a b =
    if not (is_value a) then
      measure_map (fun a' -> make a' b) (step_expr ctx a)
    else if not (is_value b) then
      measure_map (fun b' -> make a b') (step_expr ctx b)
    else Return (simplify (make a b))
  in
  match expr with
  | e when is_value e -> Return e
  | Let (x, e1, e2) when is_value e1 ->
      Return (simplify (subst x e1 e2))
  | Let (x, e1, e2) ->
      measure_map (fun e1' -> Let (x, e1', e2)) (step_expr ctx e1)
  | If (Bool true, t, _) -> Return t
  | If (Bool false, _, f) -> Return f
  | If (c, t, f) when not (is_value c) ->
      measure_map (fun c' -> If (c', t, f)) (step_expr ctx c)
  | App (fn, arg) when not (is_value fn) ->
      measure_map (fun fn' -> App (fn', arg)) (step_expr ctx fn)
  | App (fn, arg) when not (is_value arg) ->
      measure_map (fun arg' -> App (fn, arg')) (step_expr ctx arg)
  | App (Lam (x, body), arg) ->
      Return (simplify (subst x arg body))
  | App (Rec (f, x, body) as self, arg) ->
      Return (simplify (subst x arg (subst f self body)))
  | Pair (a, b) -> step_binary (fun a b -> Pair (a, b)) a b
  | Fst e when not (is_value e) -> step_unary (fun e -> Fst e) e
  | Fst (Pair (a, _)) -> Return a
  | Snd e when not (is_value e) -> step_unary (fun e -> Snd e) e
  | Snd (Pair (_, b)) -> Return b
  | Inl e when not (is_value e) -> step_unary (fun e -> Inl e) e
  | Inr e when not (is_value e) -> step_unary (fun e -> Inr e) e
  | Case (scrut, (x, left), (y, right)) when not (is_value scrut) ->
      measure_map
        (fun scrut' -> Case (scrut', (x, left), (y, right)))
        (step_expr ctx scrut)
  | Case (Inl v, (x, left), _) -> Return (simplify (subst x v left))
  | Case (Inr v, _, (y, right)) -> Return (simplify (subst y v right))
  | MatchList (scrut, nil_branch, (h, tl, cons_branch))
    when not (is_value scrut) ->
      measure_map
        (fun scrut' -> MatchList (scrut', nil_branch, (h, tl, cons_branch)))
        (step_expr ctx scrut)
  | MatchList (Nil, nil_branch, _) -> Return nil_branch
  | MatchList (Cons (head, tail), _, (h, tl, cons_branch)) ->
      Return (simplify (subst tl tail (subst h head cons_branch)))
  | Neg e when not (is_value e) -> step_unary (fun e -> Neg e) e
  | Neg _ -> Return (simplify expr)
  | Add (a, b) -> step_binary (fun a b -> Add (a, b)) a b
  | Mul (a, b) -> step_binary (fun a b -> Mul (a, b)) a b
  | Sub (a, b) -> step_binary (fun a b -> Sub (a, b)) a b
  | Div (a, b) -> step_binary (fun a b -> Div (a, b)) a b
  | Lt (a, b) -> step_binary (fun a b -> Lt (a, b)) a b
  | Leq (a, b) -> step_binary (fun a b -> Leq (a, b)) a b
  | Uniform (m, a, b) when not (is_value a) ->
      measure_map (fun a' -> Uniform (m, a', b)) (step_expr ctx a)
  | Uniform (m, a, b) when not (is_value b) ->
      measure_map (fun b' -> Uniform (m, a, b')) (step_expr ctx b)
  | Uniform (_, a, b) -> sample_random ctx (RUniform (a, b))
  | Gauss (m, a, b) when not (is_value a) ->
      measure_map (fun a' -> Gauss (m, a', b)) (step_expr ctx a)
  | Gauss (m, a, b) when not (is_value b) ->
      measure_map (fun b' -> Gauss (m, a, b')) (step_expr ctx b)
  | Gauss (_, a, b) -> sample_random ctx (RGauss (a, b))
  | Exponential (m, e) when not (is_value e) ->
      measure_map (fun e' -> Exponential (m, e')) (step_expr ctx e)
  | Exponential (_, e) -> sample_random ctx (RExponential e)
  | Gamma (m, a, b) when not (is_value a) ->
      measure_map (fun a' -> Gamma (m, a', b)) (step_expr ctx a)
  | Gamma (m, a, b) when not (is_value b) ->
      measure_map (fun b' -> Gamma (m, a, b')) (step_expr ctx b)
  | Gamma (_, a, b) -> sample_random ctx (RGamma (a, b))
  | Beta (m, a, b) when not (is_value a) ->
      measure_map (fun a' -> Beta (m, a', b)) (step_expr ctx a)
  | Beta (m, a, b) when not (is_value b) ->
      measure_map (fun b' -> Beta (m, a, b')) (step_expr ctx b)
  | Beta (_, a, b) -> sample_random ctx (RBeta (a, b))
  | Flip p when not (is_value p) ->
      measure_map (fun p' -> Flip p') (step_expr ctx p)
  | Flip (Float p) ->
      Choice [ (p, Return (Bool true)); (1.0 -. p, Return (Bool false)) ]
  | Bernoulli (m, p) when not (is_value p) ->
      measure_map (fun p' -> Bernoulli (m, p')) (step_expr ctx p)
  | Bernoulli (_, p) -> sample_random ctx (RBernoulli p)
  | Poisson (m, p) when not (is_value p) ->
      measure_map (fun p' -> Poisson (m, p')) (step_expr ctx p)
  | Poisson (_, p) -> sample_random ctx (RPoisson p)
  | Discrete (_, cases) -> sample_random ctx (RDiscrete cases)
  | Observe c when not (is_value c) ->
      measure_map (fun c' -> Observe c') (step_expr ctx c)
  | Observe (Bool true) -> Return Unit
  | Observe (Bool false) -> Choice []
  | Cons (h, t) when not (is_value h) ->
      measure_map (fun h' -> Cons (h', t)) (step_expr ctx h)
  | Cons (h, t) when not (is_value t) ->
      measure_map (fun t' -> Cons (h, t')) (step_expr ctx t)
  | _ -> Return expr

let rec step_sym_expr ctx expr =
  let step_unary make arg =
    measure_map
      (fun (new_samples, arg') -> (new_samples, make arg'))
      (step_sym_expr ctx arg)
  in
  let step_binary make a b =
    if not (is_value a) then
      measure_map
        (fun (new_samples, a') -> (new_samples, make a' b))
        (step_sym_expr ctx a)
    else if not (is_value b) then
      measure_map
        (fun (new_samples, b') -> (new_samples, make a b'))
        (step_sym_expr ctx b)
    else Return ([], simplify (make a b))
  in
  match expr with
  | e when is_value e -> Return ([], e)
  | Let (x, e1, e2) when is_value e1 ->
      Return ([], simplify (subst x e1 e2))
  | Let (x, e1, e2) ->
      measure_map
        (fun (new_samples, e1') -> (new_samples, Let (x, e1', e2)))
        (step_sym_expr ctx e1)
  | If (Bool true, t, _) -> Return ([], t)
  | If (Bool false, _, f) -> Return ([], f)
  | If (c, t, f) when not (is_value c) ->
      measure_map
        (fun (new_samples, c') -> (new_samples, If (c', t, f)))
        (step_sym_expr ctx c)
  | App (fn, arg) when not (is_value fn) ->
      measure_map
        (fun (new_samples, fn') -> (new_samples, App (fn', arg)))
        (step_sym_expr ctx fn)
  | App (fn, arg) when not (is_value arg) ->
      measure_map
        (fun (new_samples, arg') -> (new_samples, App (fn, arg')))
        (step_sym_expr ctx arg)
  | App (Lam (x, body), arg) ->
      Return ([], simplify (subst x arg body))
  | App (Rec (f, x, body) as self, arg) ->
      Return ([], simplify (subst x arg (subst f self body)))
  | Pair (a, b) -> step_binary (fun a b -> Pair (a, b)) a b
  | Fst e when not (is_value e) -> step_unary (fun e -> Fst e) e
  | Fst (Pair (a, _)) -> Return ([], a)
  | Snd e when not (is_value e) -> step_unary (fun e -> Snd e) e
  | Snd (Pair (_, b)) -> Return ([], b)
  | Inl e when not (is_value e) -> step_unary (fun e -> Inl e) e
  | Inr e when not (is_value e) -> step_unary (fun e -> Inr e) e
  | Case (scrut, (x, left), (y, right)) when not (is_value scrut) ->
      measure_map
        (fun (new_samples, scrut') ->
          (new_samples, Case (scrut', (x, left), (y, right))))
        (step_sym_expr ctx scrut)
  | Case (Inl v, (x, left), _) -> Return ([], simplify (subst x v left))
  | Case (Inr v, _, (y, right)) -> Return ([], simplify (subst y v right))
  | MatchList (scrut, nil_branch, (h, tl, cons_branch))
    when not (is_value scrut) ->
      measure_map
        (fun (new_samples, scrut') ->
          (new_samples, MatchList (scrut', nil_branch, (h, tl, cons_branch))))
        (step_sym_expr ctx scrut)
  | MatchList (Nil, nil_branch, _) -> Return ([], nil_branch)
  | MatchList (Cons (head, tail), _, (h, tl, cons_branch)) ->
      Return ([], simplify (subst tl tail (subst h head cons_branch)))
  | Neg e when not (is_value e) -> step_unary (fun e -> Neg e) e
  | Neg _ -> Return ([], simplify expr)
  | Add (a, b) -> step_binary (fun a b -> Add (a, b)) a b
  | Mul (a, b) -> step_binary (fun a b -> Mul (a, b)) a b
  | Sub (a, b) -> step_binary (fun a b -> Sub (a, b)) a b
  | Div (a, b) -> step_binary (fun a b -> Div (a, b)) a b
  | Lt (a, b) -> step_binary (fun a b -> Lt (a, b)) a b
  | Leq (a, b) -> step_binary (fun a b -> Leq (a, b)) a b
  | Uniform (m, a, b) when not (is_value a) ->
      measure_map
        (fun (new_samples, a') -> (new_samples, Uniform (m, a', b)))
        (step_sym_expr ctx a)
  | Uniform (m, a, b) when not (is_value b) ->
      measure_map
        (fun (new_samples, b') -> (new_samples, Uniform (m, a, b')))
        (step_sym_expr ctx b)
  | Uniform (E, a, b) -> record_symbolic_sample ctx (RUniform (a, b))
  | Uniform (G, a, b) -> sample_runtime_random ctx (RUniform (a, b))
  | Gauss (m, a, b) when not (is_value a) ->
      measure_map
        (fun (new_samples, a') -> (new_samples, Gauss (m, a', b)))
        (step_sym_expr ctx a)
  | Gauss (m, a, b) when not (is_value b) ->
      measure_map
        (fun (new_samples, b') -> (new_samples, Gauss (m, a, b')))
        (step_sym_expr ctx b)
  | Gauss (E, a, b) -> record_symbolic_sample ctx (RGauss (a, b))
  | Gauss (G, a, b) -> sample_runtime_random ctx (RGauss (a, b))
  | Exponential (m, e) when not (is_value e) ->
      measure_map
        (fun (new_samples, e') -> (new_samples, Exponential (m, e')))
        (step_sym_expr ctx e)
  | Exponential (E, e) -> record_symbolic_sample ctx (RExponential e)
  | Exponential (G, e) -> sample_runtime_random ctx (RExponential e)
  | Gamma (m, a, b) when not (is_value a) ->
      measure_map
        (fun (new_samples, a') -> (new_samples, Gamma (m, a', b)))
        (step_sym_expr ctx a)
  | Gamma (m, a, b) when not (is_value b) ->
      measure_map
        (fun (new_samples, b') -> (new_samples, Gamma (m, a, b')))
        (step_sym_expr ctx b)
  | Gamma (E, a, b) -> record_symbolic_sample ctx (RGamma (a, b))
  | Gamma (G, a, b) -> sample_runtime_random ctx (RGamma (a, b))
  | Beta (m, a, b) when not (is_value a) ->
      measure_map
        (fun (new_samples, a') -> (new_samples, Beta (m, a', b)))
        (step_sym_expr ctx a)
  | Beta (m, a, b) when not (is_value b) ->
      measure_map
        (fun (new_samples, b') -> (new_samples, Beta (m, a, b')))
        (step_sym_expr ctx b)
  | Beta (E, a, b) -> record_symbolic_sample ctx (RBeta (a, b))
  | Beta (G, a, b) -> sample_runtime_random ctx (RBeta (a, b))
  | Flip p when not (is_value p) ->
      measure_map
        (fun (new_samples, p') -> (new_samples, Flip p'))
        (step_sym_expr ctx p)
  | Flip (Float p) ->
      Choice
        [ (p, Return ([], Bool true)); (1.0 -. p, Return ([], Bool false)) ]
  | Bernoulli (m, p) when not (is_value p) ->
      measure_map
        (fun (new_samples, p') -> (new_samples, Bernoulli (m, p')))
        (step_sym_expr ctx p)
  | Bernoulli (E, p) -> record_symbolic_sample ctx (RBernoulli p)
  | Bernoulli (G, p) -> sample_runtime_random ctx (RBernoulli p)
  | Poisson (m, p) when not (is_value p) ->
      measure_map
        (fun (new_samples, p') -> (new_samples, Poisson (m, p')))
        (step_sym_expr ctx p)
  | Poisson (E, p) -> record_symbolic_sample ctx (RPoisson p)
  | Poisson (G, p) -> sample_runtime_random ctx (RPoisson p)
  | Discrete (E, cases) -> record_symbolic_sample ctx (RDiscrete cases)
  | Discrete (G, cases) -> sample_runtime_random ctx (RDiscrete cases)
  | Observe c when not (is_value c) ->
      measure_map
        (fun (new_samples, c') -> (new_samples, Observe c'))
        (step_sym_expr ctx c)
  | Observe (Bool true) -> Return ([], Unit)
  | Observe (Bool false) -> Choice []
  | Cons (h, t) when not (is_value h) ->
      measure_map
        (fun (new_samples, h') -> (new_samples, Cons (h', t)))
        (step_sym_expr ctx h)
  | Cons (h, t) when not (is_value t) ->
      measure_map
        (fun (new_samples, t') -> (new_samples, Cons (h, t')))
        (step_sym_expr ctx t)
  | _ -> Return ([], expr)

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

let print_symbolic_trace fuel source determinized_expr =
  let ctx = empty_context () in
  let rec loop step symbolic =
    let original =
      let ctx = empty_context () in
      simplify_measure (nstep_expr ctx step source)
    in
    let determinized =
      let ctx = empty_context () in
      simplify_measure (nstep_expr ctx step determinized_expr)
    in
    let symbolic_actual = symbolic_actual_view symbolic in
    let symbolic_expected = symbolic_expected_view symbolic in
    let actual_ok = compare_measures original symbolic_actual in
    let expected_ok = compare_measures determinized symbolic_expected in
    Printf.printf "\n-- symbolic step %d --\n" step;
    Printf.printf "symbolic states: %s\n"
    (measure_to_string sym_state_to_string symbolic);
    Printf.printf "original step:   %s\n"
      (measure_to_string expr_to_string original);
    Printf.printf "actual view:     %s\n"
      (measure_to_string expr_to_string symbolic_actual);
    Printf.printf "determinized step: %s\n"
      (measure_to_string expr_to_string determinized);
    Printf.printf "expected view:   %s\n"
      (measure_to_string expr_to_string symbolic_expected);
    Printf.printf "actual coupling:   %s\n" (if actual_ok then "OK" else "FAIL");
    Printf.printf "expected coupling: %s\n" (if expected_ok then "OK" else "FAIL");
    if step < fuel then
      loop (step + 1) (measure_bind symbolic (step_sym_state ctx))
  in
  loop 0 (Return { sigma = []; residual = source })

let run_case trace name fuel source determinized_expr =
  let original =
    let ctx = empty_context () in
    simplify_measure (nstep_expr ctx fuel source)
  in
  let symbolic =
    let ctx = empty_context () in
    nstep_sym ctx fuel { sigma = []; residual = source }
  in
  let symbolic_actual = symbolic_actual_view symbolic in
  let symbolic_expected = symbolic_expected_view symbolic in
  let determinized =
    let ctx = empty_context () in
    simplify_measure (nstep_expr ctx fuel determinized_expr)
  in
  let actual_ok = compare_measures original symbolic_actual in
  let expected_ok = compare_measures determinized symbolic_expected in
  Printf.printf "\n=== %s ===\n" name;
  Printf.printf "source:       %s\n" (expr_to_string source);
  Printf.printf "determinized: %s\n" (expr_to_string determinized_expr);
  if trace then print_symbolic_trace fuel source determinized_expr;
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
    "Usage: symbolic_coupling [--fuel N] [--trace] [source.det determinized.det] ...\n\n\
     With no files, runs the built-in examples.\n\
     With files, arguments are read in source/determinized pairs.\n\
     --trace prints each symbolic small step and its actual/expected views.\n\
     File syntax includes parser.mly constructs: fun, rec, application,\n\
     let, if, pairs, fst/snd, inl/inr case matches, list matches,\n\
     arithmetic, comparisons, observe, and the supported distributions.\n\
     Distribution calls may use [E]/[G], e.g. uniform[E](a,b) or gauss[G](m,v).\n\
     Unannotated distribution calls default to [E].\n"

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
      Flip (f 0.5),
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

let det_dependent_uniform =
  Let
    ( "x",
      f 0.5,
      Let
        ( "y",
          Div (Add (Var "x", f 2.0), f 2.0),
          Add (Var "x", Var "y") ) )

let det_flip_kept =
  Let
    ( "b",
      Flip (f 0.5),
      If (Var "b", f 0.5, f 3.0) )

let det_mixed_modes =
  Let
    ( "g",
      uniform_g (f 0.0) (f 1.0),
      Let
        ( "x",
          Div (Add (Var "g", f 2.0), f 2.0),
          Add (Var "g", Var "x") ) )

let builtin_cases fuel =
    [
      ( "dependent expectation samples",
        fuel,
        example_dependent_uniform,
        det_dependent_uniform );
      ( "flip retained, branch samples determinized",
        fuel,
        example_flip_kept,
        det_flip_kept );
      ( "general sample retained, dependent expectation sample",
        fuel,
        example_mixed_modes,
        det_mixed_modes );
    ]

let file_cases fuel paths =
  let rec pair acc = function
    | [] -> List.rev acc
    | source_path :: determinized_path :: rest ->
        let name = source_path ^ " / " ^ determinized_path in
        let source = parse_file source_path in
        let determinized = parse_file determinized_path in
        pair ((name, fuel, source, determinized) :: acc) rest
    | [ _ ] ->
        parse_error
          "file arguments must come in source/determinized pairs"
  in
  pair [] paths

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
    List.map
      (fun (name, fuel, source, determinized) ->
        run_case trace name fuel source determinized)
      cases
  in
  if List.for_all Fun.id results then (
    Printf.printf "\nAll symbolic coupling checks passed.\n";
    exit 0)
  else (
    Printf.printf "\nSome symbolic coupling checks failed.\n";
    exit 1)
