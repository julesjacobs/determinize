module A = Ast

type transition = { 
  src : int; 
  dst : int; 
  prob : float 
}

type mc = {
  num_states : int;
  sink : int;
  transitions : transition list;
  expr_of : (int, A.expr) Hashtbl.t;  (* map: state id -> expression *)
}

(* ---------- Utilities: sets of identifiers ---------- *)

module StringSet = Set.Make (String)

let rec free_vars (e : A.expr) : StringSet.t =
  match e with
  | A.Var x -> StringSet.singleton x
  | A.Lam (x, body) -> StringSet.remove x (free_vars body)
  | A.Rec (f, x, body) -> free_vars body |> StringSet.remove f |> StringSet.remove x
  | A.App (e1, e2)
  | A.Pair (e1, e2)
  | A.Cons (e1, e2)
  | A.Add (e1, e2)
  | A.Mul (e1, e2)
  | A.Lt (e1, e2) -> StringSet.union (free_vars e1) (free_vars e2)
  | A.Fst e
  | A.Snd e
  | A.Inl e
  | A.Inr e
  | A.Neg e
  | A.Flip e -> free_vars e
  | A.If (c, t, f) ->
      StringSet.union (free_vars c) (StringSet.union (free_vars t) (free_vars f))
  | A.Let (x, e1, e2) ->
      StringSet.union (free_vars e1) (StringSet.remove x (free_vars e2))
  | A.Case (e, (x, e1), (y, e2)) ->
      StringSet.union (free_vars e)
        (StringSet.union
           (StringSet.remove x (free_vars e1))
           (StringSet.remove y (free_vars e2)))
  | A.MatchList (e, nil_br, (x, xs, cons_br)) ->
      StringSet.union (free_vars e)
        (StringSet.union (free_vars nil_br)
           (free_vars cons_br |> StringSet.remove x |> StringSet.remove xs))
  | A.Discrete cases ->
      List.fold_left
        (fun acc (_p, ei) -> StringSet.union acc (free_vars ei))
        StringSet.empty cases
  | A.Unit | A.Nil | A.Bool _ | A.Const _ -> StringSet.empty
  | A.Uniform _ | A.Gauss _ ->
      (* should not appear after determinization for MC generation *)
      StringSet.empty

let fresh_name (avoid : StringSet.t) (base : string) : string =
  if not (StringSet.mem base avoid) then base
  else
    let rec go i =
      let cand = base ^ "'" ^ string_of_int i in
      if StringSet.mem cand avoid then go (i + 1) else cand
    in
    go 0

(* Capture-avoiding renaming: replace bound var [x] with [x'] in [body]. *)
let rec rename (x : string) (x' : string) (e : A.expr) : A.expr =
  match e with
  | A.Var y -> if y = x then A.Var x' else e
  | A.Lam (y, body) ->
      if y = x then A.Lam (x', rename x x' body) else A.Lam (y, rename x x' body)
  | A.Rec (f, y, body) ->
      let f1 = if f = x then x' else f in
      let y1 = if y = x then x' else y in
      A.Rec (f1, y1, rename x x' body)
  | A.App (a, b) -> A.App (rename x x' a, rename x x' b)
  | A.Unit | A.Nil | A.Bool _ | A.Const _ -> e
  | A.Pair (a, b) -> A.Pair (rename x x' a, rename x x' b)
  | A.Fst a -> A.Fst (rename x x' a)
  | A.Snd a -> A.Snd (rename x x' a)
  | A.Inl a -> A.Inl (rename x x' a)
  | A.Inr a -> A.Inr (rename x x' a)
  | A.Case (scrut, (xl, el), (xr, er)) ->
      let scrut' = rename x x' scrut in
      let xl', el' =
        if xl = x then (x', rename x x' el) else (xl, rename x x' el)
      in
      let xr', er' =
        if xr = x then (x', rename x x' er) else (xr, rename x x' er)
      in
      A.Case (scrut', (xl', el'), (xr', er'))
  | A.Cons (hd, tl) -> A.Cons (rename x x' hd, rename x x' tl)
  | A.MatchList (scrut, nil_br, (xh, xt, cons_br)) ->
      let scrut' = rename x x' scrut in
      let nil_br' = rename x x' nil_br in
      let xh' = if xh = x then x' else xh in
      let xt' = if xt = x then x' else xt in
      let cons_br' = rename x x' cons_br in
      A.MatchList (scrut', nil_br', (xh', xt', cons_br'))
  | A.If (c, t, f) -> A.If (rename x x' c, rename x x' t, rename x x' f)
  | A.Let (y, e1, e2) ->
      let e1' = rename x x' e1 in
      if y = x then A.Let (x', e1', rename x x' e2)
      else A.Let (y, e1', rename x x' e2)
  | A.Neg a -> A.Neg (rename x x' a)
  | A.Add (a, b) -> A.Add (rename x x' a, rename x x' b)
  | A.Mul (a, b) -> A.Mul (rename x x' a, rename x x' b)
  | A.Lt (a, b) -> A.Lt (rename x x' a, rename x x' b)
  | A.Flip a -> A.Flip (rename x x' a)
  | A.Discrete cases ->
      A.Discrete (List.map (fun (p, ei) -> (p, rename x x' ei)) cases)
  | A.Uniform _ | A.Gauss _ ->
      failwith "to_mc: continuous distributions not supported (Uniform/Gauss)"

(* Capture-avoiding substitution [e[v/x]] *)
let rec subst (x : string) (v : A.expr) (e : A.expr) : A.expr =
  match e with
  | A.Var y -> if y = x then v else e
  | A.Unit | A.Nil | A.Bool _ | A.Const _ -> e
  | A.Lam (y, body) ->
      if y = x then e
      else if StringSet.mem y (free_vars v) then (
        let avoid = StringSet.union (free_vars body) (free_vars v) |> StringSet.add x in
        let y' = fresh_name avoid y in
        let body' = rename y y' body in
        A.Lam (y', subst x v body'))
      else A.Lam (y, subst x v body)
  | A.Rec (f, y, body) ->
      if f = x || y = x then e
      else
        let fv_v = free_vars v in
        let body =
          if StringSet.mem f fv_v then (
            let avoid = StringSet.union (free_vars body) fv_v |> StringSet.add x in
            let f' = fresh_name avoid f in
            rename f f' body)
          else body
        in
        let body =
          if StringSet.mem y fv_v then (
            let avoid = StringSet.union (free_vars body) fv_v |> StringSet.add x in
            let y' = fresh_name avoid y in
            rename y y' body)
          else body
        in
        A.Rec (f, y, subst x v body)
  | A.App (a, b) -> A.App (subst x v a, subst x v b)
  | A.Pair (a, b) -> A.Pair (subst x v a, subst x v b)
  | A.Fst a -> A.Fst (subst x v a)
  | A.Snd a -> A.Snd (subst x v a)
  | A.Inl a -> A.Inl (subst x v a)
  | A.Inr a -> A.Inr (subst x v a)
  | A.Case (scrut, (xl, el), (xr, er)) ->
      let scrut' = subst x v scrut in
      let el' =
        if xl = x then el
        else if StringSet.mem xl (free_vars v) then (
          let avoid = StringSet.union (free_vars el) (free_vars v) |> StringSet.add x in
          let xl' = fresh_name avoid xl in
          subst x v (rename xl xl' el))
        else subst x v el
      in
      let er' =
        if xr = x then er
        else if StringSet.mem xr (free_vars v) then (
          let avoid = StringSet.union (free_vars er) (free_vars v) |> StringSet.add x in
          let xr' = fresh_name avoid xr in
          subst x v (rename xr xr' er))
        else subst x v er
      in
      A.Case (scrut', (xl, el'), (xr, er'))
  | A.Cons (hd, tl) -> A.Cons (subst x v hd, subst x v tl)
  | A.MatchList (scrut, nil_br, (xh, xt, cons_br)) ->
      let scrut' = subst x v scrut in
      let nil_br' = subst x v nil_br in
      let cons_br' =
        if xh = x || xt = x then cons_br
        else
          let fv_v = free_vars v in
          let cons_br =
            if StringSet.mem xh fv_v then (
              let avoid = StringSet.union (free_vars cons_br) fv_v |> StringSet.add x in
              let xh' = fresh_name avoid xh in
              rename xh xh' cons_br)
            else cons_br
          in
          let cons_br =
            if StringSet.mem xt fv_v then (
              let avoid = StringSet.union (free_vars cons_br) fv_v |> StringSet.add x in
              let xt' = fresh_name avoid xt in
              rename xt xt' cons_br)
            else cons_br
          in
          subst x v cons_br
      in
      A.MatchList (scrut', nil_br', (xh, xt, cons_br'))
  | A.If (c, t, f) -> A.If (subst x v c, subst x v t, subst x v f)
  | A.Let (y, e1, e2) ->
      let e1' = subst x v e1 in
      if y = x then A.Let (y, e1', e2)
      else if StringSet.mem y (free_vars v) then (
        let avoid = StringSet.union (free_vars e2) (free_vars v) |> StringSet.add x in
        let y' = fresh_name avoid y in
        let e2' = rename y y' e2 in
        A.Let (y', e1', subst x v e2'))
      else A.Let (y, e1', subst x v e2)
  | A.Neg a -> A.Neg (subst x v a)
  | A.Add (a, b) -> A.Add (subst x v a, subst x v b)
  | A.Mul (a, b) -> A.Mul (subst x v a, subst x v b)
  | A.Lt (a, b) -> A.Lt (subst x v a, subst x v b)
  | A.Flip a -> A.Flip (subst x v a)
  | A.Discrete cases -> A.Discrete (List.map (fun (p, ei) -> (p, subst x v ei)) cases)
  | A.Uniform _ | A.Gauss _ ->
      failwith "to_mc: continuous distributions not supported (Uniform/Gauss)"

(* ---------- Values ---------- *)

let rec is_value = function
  | A.Unit | A.Bool _ | A.Const _ | A.Lam _ | A.Rec _ | A.Nil -> true
  | A.Pair (a, b) -> is_value a && is_value b
  | A.Inl a | A.Inr a -> is_value a
  | A.Cons (a, b) -> is_value a && is_value b
  | _ -> false

(* ---------- Eager arithmetic simplification ---------- *)

let float_of_const = function
  | A.Const f -> f
  | _ -> failwith "expected Const"

let bool_of_bool = function
  | A.Bool b -> b
  | _ -> failwith "expected Bool"

let rec simplify (e : A.expr) : A.expr =
  let simp = simplify in
  match e with
  | A.Neg a ->
      let a' = simp a in
      (match a' with
       | A.Const f -> A.Const (-. f)
       | _ -> A.Neg a')
  | A.Add (a, b) ->
      let a' = simp a in
      let b' = simp b in
      (match (a', b') with
       | A.Const x, A.Const y -> A.Const (x +. y)
       | _ -> A.Add (a', b'))
  | A.Mul (a, b) ->
      let a' = simp a in
      let b' = simp b in
      (match (a', b') with
       | A.Const x, A.Const y -> A.Const (x *. y)
       | _ -> A.Mul (a', b'))
  | A.Lt (a, b) ->
      let a' = simp a in
      let b' = simp b in
      (match (a', b') with
       | A.Const x, A.Const y -> A.Bool (x < y)
       | _ -> A.Lt (a', b'))
  | A.Fst a ->
      let a' = simp a in
      (match a' with
       | A.Pair (v1, v2) when is_value v1 && is_value v2 -> v1
       | _ -> A.Fst a')
  | A.Snd a ->
      let a' = simp a in
      (match a' with
       | A.Pair (v1, v2) when is_value v1 && is_value v2 -> v2
       | _ -> A.Snd a')
  | A.Pair (a, b) -> A.Pair (simp a, simp b)
  | A.Cons (a, b) -> A.Cons (simp a, simp b)
  | A.Inl a -> A.Inl (simp a)
  | A.Inr a -> A.Inr (simp a)
  | A.Case (scrut, (x, l), (y, r)) -> A.Case (simp scrut, (x, simp l), (y, simp r))
  | A.MatchList (scrut, nil_br, (x, xs, cons_br)) ->
      A.MatchList (simp scrut, simp nil_br, (x, xs, simp cons_br))
  | A.If (c, t, f) -> A.If (simp c, simp t, simp f)
  | A.Let (x, e1, e2) -> A.Let (x, simp e1, simp e2)
  | A.App (e1, e2) -> A.App (simp e1, simp e2)
  | A.Flip a -> A.Flip (simp a)
  | A.Discrete cases -> A.Discrete (List.map (fun (p, ei) -> (p, simp ei)) cases)
  | A.Var _ | A.Unit | A.Nil | A.Bool _ | A.Const _ | A.Lam _ | A.Rec _ -> e
  | A.Uniform _ | A.Gauss _ ->
      failwith "to_mc: continuous distributions not supported (Uniform/Gauss)"

(* ---------- Small-step semantics: e -> { (p_i, e_i) } ---------- *)

let check_prob (p : float) : unit =
  if p < 0.0 || p > 1.0 || Float.is_nan p || Float.is_infinite p then
    failwith (Format.asprintf "invalid probability %g" p)

let check_sum_to_one (ps : float list) : unit =
  let s = List.fold_left ( +. ) 0.0 ps in
  let eps = 1e-12 in
  if Float.abs (s -. 1.0) > eps then
    failwith (Format.asprintf "probabilities must sum to 1, got %0.17g" s)

let rec step (e : A.expr) : (float * A.expr) list =
  let e = simplify e in
  if is_value e then
    []
  else
    match e with
    | A.If (c, tbr, fbr) ->
        if not (is_value c) then
          List.map (fun (p, c') -> (p, A.If (c', tbr, fbr))) (step c)
        else
          (match c with
           | A.Bool true -> [ (1.0, tbr) ]
           | A.Bool false -> [ (1.0, fbr) ]
           | _ -> failwith "if: condition is not a bool")
    | A.Let (x, e1, e2) ->
        if not (is_value e1) then
          List.map (fun (p, e1') -> (p, A.Let (x, e1', e2))) (step e1)
        else
          (* let x = v in e2 --> e2{v/x} *)
          let e' = subst x e1 e2 |> simplify in
          [ (1.0, e') ]
    | A.App (e1, e2) ->
        if not (is_value e1) then
          List.map (fun (p, e1') -> (p, A.App (e1', e2))) (step e1)
        else if not (is_value e2) then
          List.map (fun (p, e2') -> (p, A.App (e1, e2'))) (step e2)
        else
          (match e1 with
           | A.Lam (x, body) ->
               let e' = subst x e2 body |> simplify in
               [ (1.0, e') ]
           | A.Rec (f, x, body) ->
               (* (rec f x => body) v  -->  body{v/x}{(rec f x => body)/f} *)
               let self = e1 in
               let e' = subst f self body |> subst x e2 |> simplify in
               [ (1.0, e') ]
           | _ -> failwith "application to non-function")
    | A.Neg a ->
        if not (is_value a) then
          List.map (fun (p, a') -> (p, A.Neg a')) (step a)
        else
          (match a with
           | A.Const f -> [ (1.0, A.Const (-. f)) ]
           | _ -> failwith "neg: expected float")
    | A.Add (a, b) ->
        if not (is_value a) then
          List.map (fun (p, a') -> (p, A.Add (a', b))) (step a)
        else if not (is_value b) then
          List.map (fun (p, b') -> (p, A.Add (a, b'))) (step b)
        else
          (match (a, b) with
           | A.Const x, A.Const y -> [ (1.0, A.Const (x +. y)) ]
           | _ -> failwith "add: expected floats")
    | A.Mul (a, b) ->
        if not (is_value a) then
          List.map (fun (p, a') -> (p, A.Mul (a', b))) (step a)
        else if not (is_value b) then
          List.map (fun (p, b') -> (p, A.Mul (a, b'))) (step b)
        else
          (match (a, b) with
           | A.Const x, A.Const y -> [ (1.0, A.Const (x *. y)) ]
           | _ -> failwith "mul: expected floats")
    | A.Lt (a, b) ->
        if not (is_value a) then
          List.map (fun (p, a') -> (p, A.Lt (a', b))) (step a)
        else if not (is_value b) then
          List.map (fun (p, b') -> (p, A.Lt (a, b'))) (step b)
        else
          (match (a, b) with
           | A.Const x, A.Const y -> [ (1.0, A.Bool (x < y)) ]
           | _ -> failwith "lt: expected floats")
    | A.Pair (a, b) ->
        if not (is_value a) then
          List.map (fun (p, a') -> (p, A.Pair (a', b))) (step a)
        else if not (is_value b) then
          List.map (fun (p, b') -> (p, A.Pair (a, b'))) (step b)
        else
          [] (* pair of values is a value, handled above *)
    | A.Fst a ->
        if not (is_value a) then
          List.map (fun (p, a') -> (p, A.Fst a')) (step a)
        else
          (match a with
           | A.Pair (v1, v2) when is_value v1 && is_value v2 -> [ (1.0, v1) ]
           | _ -> failwith "fst: expected pair")
    | A.Snd a ->
        if not (is_value a) then
          List.map (fun (p, a') -> (p, A.Snd a')) (step a)
        else
          (match a with
           | A.Pair (v1, v2) when is_value v1 && is_value v2 -> [ (1.0, v2) ]
           | _ -> failwith "snd: expected pair")
    | A.Inl a ->
        if not (is_value a) then List.map (fun (p, a') -> (p, A.Inl a')) (step a) else []
    | A.Inr a ->
        if not (is_value a) then List.map (fun (p, a') -> (p, A.Inr a')) (step a) else []
    | A.Case (scrut, (x, el), (y, er)) ->
        if not (is_value scrut) then
          List.map (fun (p, s') -> (p, A.Case (s', (x, el), (y, er)))) (step scrut)
        else
          (match scrut with
           | A.Inl v when is_value v -> [ (1.0, subst x v el |> simplify) ]
           | A.Inr v when is_value v -> [ (1.0, subst y v er |> simplify) ]
           | _ -> failwith "case: expected inl/inr value")
    | A.Cons (hd, tl) ->
        if not (is_value hd) then
          List.map (fun (p, hd') -> (p, A.Cons (hd', tl))) (step hd)
        else if not (is_value tl) then
          List.map (fun (p, tl') -> (p, A.Cons (hd, tl'))) (step tl)
        else
          []
    | A.MatchList (scrut, nil_br, (x, xs, cons_br)) ->
        if not (is_value scrut) then
          List.map (fun (p, s') -> (p, A.MatchList (s', nil_br, (x, xs, cons_br)))) (step scrut)
        else
          (match scrut with
           | A.Nil -> [ (1.0, nil_br) ]
           | A.Cons (vhd, vtl) when is_value vhd && is_value vtl ->
               let e' = subst xs vtl (subst x vhd cons_br) |> simplify in
               [ (1.0, e') ]
           | _ -> failwith "matchlist: expected [] or v::vs")
    | A.Var _ ->
        (* With substitution-based semantics, a free variable is stuck. *)
        failwith "to_mc: stuck on Var (did you forget to substitute?)"
    | A.Flip pexpr ->
        if not (is_value pexpr) then
          List.map (fun (p, pexpr') -> (p, A.Flip pexpr')) (step pexpr)
        else
          (match pexpr with
           | A.Const p ->
               check_prob p;
               let p1 = p in
               let p2 = 1.0 -. p in
               check_prob p2;
               check_sum_to_one [ p1; p2 ];
               [ (p1, A.Bool true); (p2, A.Bool false) ]
           | _ -> failwith "flip: expected a float probability")
    | A.Discrete cases ->
        if cases = [] then failwith "discrete: empty"
        else
          let ps = List.map fst cases in
          List.iter check_prob ps;
          check_sum_to_one ps;
          List.map (fun (p, ei) -> (p, ei)) cases
    | A.Uniform _ | A.Gauss _ ->
        failwith "to_mc: continuous distributions not supported (Uniform/Gauss)"
    | A.Unit | A.Nil | A.Bool _ | A.Const _ | A.Lam _ | A.Rec _ ->
        (* should have been caught by is_value *)
        []

(* ---------- Graph construction ---------- *)

let expr_key (e : A.expr) : string =
  let h = Hashtbl.hash e in
  string_of_int h ^ ":" ^ Marshal.to_string e [ Marshal.No_sharing ]

let to_mc (e0 : A.expr) : mc =
  let e0 = simplify e0 in
  let id_of : (string, int) Hashtbl.t = Hashtbl.create 4096 in
  let expr_of : (int, A.expr) Hashtbl.t = Hashtbl.create 4096 in
  let q : int Queue.t = Queue.create () in
  let next_id = ref 0 in

  let add_state (e : A.expr) : int =
    let key = expr_key e in
    match Hashtbl.find_opt id_of key with
    | Some id -> id
    | None ->
        let id = !next_id in
        incr next_id;
        Hashtbl.add id_of key id;
        Hashtbl.add expr_of id e;
        Queue.add id q;
        id
  in
  let start_id = add_state e0 in
  if start_id <> 0 then failwith "internal error: initial state is not 0";

  let transitions_rev : transition list ref = ref [] in
  let terminal_ids : int list ref = ref [] in

  while not (Queue.is_empty q) do
    let sid = Queue.take q in
    let e = Hashtbl.find expr_of sid in
    let succs = step e in
    if succs = [] then terminal_ids := sid :: !terminal_ids
    else
      List.iter
        (fun (p, e') ->
          let e' = simplify e' in
          let tid = add_state e' in
          transitions_rev := { src = sid; dst = tid; prob = p } :: !transitions_rev)
        succs
  done;

  (* Add sink as the last state. *)
  let sink = !next_id in
  let transitions =
    let base = !transitions_rev in
    let from_terminals =
      List.map (fun sid -> { src = sid; dst = sink; prob = 1.0 }) !terminal_ids
    in
    { src = sink; dst = sink; prob = 1.0 } :: (from_terminals @ base)
  in
  (* Sort transitions for nicer output. *)
  let transitions =
    List.sort
      (fun a b ->
        match compare a.src b.src with
        | 0 -> (match compare a.dst b.dst with 0 -> compare a.prob b.prob | c -> c)
        | c -> c)
      transitions
  in
  { num_states = sink + 1; sink; transitions; expr_of }

(* Transition file (.tra) for STORM backend *)
let write_tra_file (m : mc) (path : string) : unit =
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "dtmc@.";
  List.iter
    (fun { src; dst; prob } ->
      (* Storm accepts: <src> <dst> <prob> per line. *)
      Format.fprintf fmt "%d %d %.17g@." src dst prob)
    m.transitions;
  close_out oc 

(* Labeling file (.lab) for STORM backend *)
let write_lab_file (m : mc) (path : string) : unit =
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "#DECLARATION@.";
  Format.fprintf fmt "init done@.";
  Format.fprintf fmt "#END@.";
  Format.fprintf fmt "0 init@.";
  Format.fprintf fmt "%d done@." m.sink;
  close_out oc

(* State rewards file (.state.rew) for STORM backend *)
let write_state_rew_file (m : mc) (path : string) : unit =
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  for sid = 0 to (m.num_states - 1) do
    match Hashtbl.find_opt m.expr_of sid with
    | Some (A.Const f) ->
        (* emit rewards only for states whose expression is a numeric constant *)
        Format.fprintf fmt "%d %.17g@." sid f
    | _ ->
        ()
  done;
  close_out oc
  


  
