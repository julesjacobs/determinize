open Ast

module type RNG = sig
  val uniform : float -> float -> float
  val gaussian : float -> float -> float
  val flip : float -> bool
  val discrete : float list -> int
end

module StdRng : RNG = struct
  let uniform a b =
    let lo = min a b in
    let hi = max a b in
    lo +. Random.float (hi -. lo)

  let gaussian mean var =
    let u1 = Random.float 1.0 in
    let u2 = Random.float 1.0 in
    let z0 = sqrt (-2. *. log u1) *. cos (2. *. Float.pi *. u2) in
    let sigma = sqrt var in
    mean +. sigma *. z0

  let flip p =
    if p < 0.0 || p > 1.0 then failwith "flip: p not in [0,1]";
    Random.float 1.0 < p

  let discrete ps =
    match ps with
    | [] -> failwith "discrete: empty probability list"
    | _ ->
        let total = List.fold_left ( +. ) 0.0 ps in
        if total <= 0.0 then failwith "discrete: total probability <= 0";
        (* check this? *)
        let r = Random.float total in
        let rec pick i acc = function
          | [] -> max 0 (i - 1)  (* numeric drift fallback *)
          | p :: tl ->
              let acc' = acc +. p in
              if r <= acc' then i else pick (i + 1) acc' tl
        in
        pick 0 0.0 ps
end

type value =
  | VUnit
  | VBool of bool
  | VFloat of float
  | VPair of value * value
  | VInl of value
  | VInr of value
  | VNil
  | VCons of value * value
  | VClosure of ident * expr * env
  | VRecClosure of ident * ident * expr * env

and env = (string * value) list

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound " ^ x)
  | (y,v)::t -> if x = y then v else lookup x t

let float_of_value = function
  | VFloat f -> f
  | _ -> failwith "expected float"

let bool_of_value = function
  | VBool b -> b
  | _ -> failwith "expected bool"

let rec eval (module R : RNG) env e =
  match e with
  | Var x -> lookup x env
  | Lam (x, body) -> VClosure (x, body, env)
  | Rec (f, x, body) -> VRecClosure (f, x, body, env)
  | App (e1, e2) ->
      let v1 = eval (module R) env e1 in
      let v2 = eval (module R) env e2 in
      (match v1 with
       | VClosure (x, body, clo_env) -> eval (module R) ((x,v2)::clo_env) body
       | VRecClosure (f, x, body, clo_env) ->
           let rec_clo = VRecClosure (f, x, body, clo_env) in
           eval (module R) ((x,v2)::(f,rec_clo)::clo_env) body
       | _ -> failwith "application to non-function")
  | Unit -> VUnit
  | Nil -> VNil
  | Pair (e1, e2) -> VPair (eval (module R) env e1, eval (module R) env e2)
  | Fst e -> (match eval (module R) env e with VPair (a, _) -> a | _ -> failwith "fst")
  | Snd e -> (match eval (module R) env e with VPair (_, b) -> b | _ -> failwith "snd")
  | Inl e -> VInl (eval (module R) env e)
  | Inr e -> VInr (eval (module R) env e)
  | Case (e, (x, e1), (y, e2)) ->
      (match eval (module R) env e with
       | VInl v -> eval (module R) ((x,v)::env) e1
       | VInr v -> eval (module R) ((y,v)::env) e2
       | _ -> failwith "match on non-sum")
  | Cons (hd, tl) ->
      let v_hd = eval (module R) env hd in
      let v_tl = eval (module R) env tl in
      VCons (v_hd, v_tl)
  | MatchList (e, nil_br, (x, xs, cons_br)) ->
      (match eval (module R) env e with
       | VNil -> eval (module R) env nil_br
       | VCons (hd, tl) -> eval (module R) ((x, hd)::(xs, tl)::env) cons_br
       | _ -> failwith "match on non-list")
  | Bool b -> VBool b
  | If (c, t, f) ->
      (match eval (module R) env c with
       | VBool true -> eval (module R) env t
       | VBool false -> eval (module R) env f
       | _ -> failwith "if condition")
  | Let (x, e1, e2) ->
      let v1 = eval (module R) env e1 in
      eval (module R) ((x,v1)::env) e2
  | Const f -> VFloat f
  | Neg e -> VFloat (-. float_of_value (eval (module R) env e))
  | Add (e1, e2) -> VFloat (float_of_value (eval (module R) env e1) +. float_of_value (eval (module R) env e2))
  | Mul (e1, e2) -> VFloat (float_of_value (eval (module R) env e1) *. float_of_value (eval (module R) env e2))
  | Lt (e1, e2) -> VBool (float_of_value (eval (module R) env e1) < float_of_value (eval (module R) env e2))
  | Uniform (e1, e2) -> VFloat (R.uniform (float_of_value (eval (module R) env e1)) (float_of_value (eval (module R) env e2)))
  | Gauss (e1, e2) -> VFloat (R.gaussian (float_of_value (eval (module R) env e1)) (float_of_value (eval (module R) env e2)))
  | Flip e1 ->
    let p = float_of_value (eval (module R) env e1) in
    VBool (R.flip p)
  | Discrete cases ->
    let ps = List.map fst cases in
    let i = R.discrete ps in
    let (_, chosen_e) = List.nth cases i in
    eval (module R) env chosen_e
