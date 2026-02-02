open Ast

exception ObserveFailure (* New custom exception for observe failures *)

module type RNG = sig
  val uniform : float -> float -> float
  val gaussian : float -> float -> float
  val exponential : float -> float
  val gamma : float -> float -> float
  val beta : float -> float -> float
  val flip : float -> bool
  val bernoulli : float -> float (* can also be float -> int *)
  val poisson : float -> float
  val discrete : float list -> int
end

module StdRng : RNG = struct
  (* Helper functions *)
  let rand_u01 () =
    let min_u = 1e-12 in
    let u = Random.float 1.0 in
    if u = 0.0 then min_u else u

  let rand_std_normal () =
    let u1 = rand_u01 () in
    let u2 = Random.float 1.0 in
    sqrt (-2. *. log u1) *. cos (2. *. Float.pi *. u2)
  
  (* Random samplers *)
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

  let bernoulli p =
    if flip p then 1.0 else 0.0

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
  
    let exponential lambda =
      if lambda <= 0.0 then failwith "exponential: rate must be > 0";
      let u = rand_u01 () in
      -. (log u) /. lambda

    let rec gamma alpha beta =
      if alpha <= 0.0 then failwith "gamma: alpha (shape) must be > 0";
      if beta  <= 0.0 then failwith "gamma: beta (rate) must be > 0";
      let scale = 1.0 /. beta in
      if alpha < 1.0 then
        let u = rand_u01 () in
        gamma (alpha +. 1.0) beta *. (u ** (1.0 /. alpha))
      else
        let d = alpha -. (1.0 /. 3.0) in
        let c = 1.0 /. sqrt (9.0 *. d) in
        let rec loop () =
          let x = rand_std_normal () in
          let v = 1.0 +. c *. x in
          if v <= 0.0 then loop ()
          else
            let v3 = v *. v *. v in
            let u = rand_u01 () in
            if u < 1.0 -. 0.0331 *. (x *. x) *. (x *. x) then
              scale *. d *. v3
            else if log u < 0.5 *. x *. x +. d *. (1.0 -. v3 +. log v3) then
              scale *. d *. v3
            else
              loop ()
        in
        loop ()

    let beta a b =
      if a <= 0.0 || b <= 0.0 then failwith "beta: a and b must be > 0";
      let x = gamma a 1.0 in   (* rate = 1 *)
      let y = gamma b 1.0 in   (* rate = 1 *)
      x /. (x +. y)

    let poisson lambda =
      if lambda < 0.0 then failwith "poisson: lambda must be >= 0";
      if lambda = 0.0 then 0.0
      else
        (* Knuth's algorithm *)
        let l = exp (-. lambda) in
        let rec loop k p =
          if p <= l then float_of_int (k - 1)
          else
            let u = rand_u01 () in
            loop (k + 1) (p *. u)
        in
        loop 0 1.0
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
  | Sub (e1, e2) -> VFloat (float_of_value (eval (module R) env e1) -. float_of_value (eval (module R) env e2))
  | Div (e1, e2) -> VFloat (float_of_value (eval (module R) env e1) /. float_of_value (eval (module R) env e2))
  | Lt (e1, e2) -> VBool (float_of_value (eval (module R) env e1) < float_of_value (eval (module R) env e2))
  | Leq (e1, e2) -> VBool (float_of_value (eval (module R) env e1) <= float_of_value (eval (module R) env e2))
  | Uniform (e1, e2) -> VFloat (R.uniform (float_of_value (eval (module R) env e1)) (float_of_value (eval (module R) env e2)))
  | Gauss (e1, e2) -> VFloat (R.gaussian (float_of_value (eval (module R) env e1)) (float_of_value (eval (module R) env e2)))
  | Exponential e1 -> VFloat (R.exponential (float_of_value (eval (module R) env e1)))
  | Gamma (e1, e2) -> VFloat (R.gamma (float_of_value (eval (module R) env e1)) (float_of_value (eval (module R) env e2)))
  | Beta (e1, e2) -> VFloat (R.beta (float_of_value (eval (module R) env e1)) (float_of_value (eval (module R) env e2)))
  | Flip e1 ->
    let p = float_of_value (eval (module R) env e1) in
    VBool (R.flip p)
  | Bernoulli e1 -> VFloat (R.bernoulli (float_of_value (eval (module R) env e1)))
  | Poisson e1 -> VFloat (R.poisson (float_of_value (eval (module R) env e1)))
  | Discrete cases ->
    let ps = List.map fst cases in
    let i = R.discrete ps in
    let (_, chosen_e) = List.nth cases i in
    eval (module R) env chosen_e
  | Observe c ->
    (match eval (module R) env c with
      | VBool true -> VUnit
      | VBool false -> raise ObserveFailure
      | _ -> failwith "observe: expected bool")
