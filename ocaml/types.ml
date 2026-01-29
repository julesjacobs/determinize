type mode = G | E

and mode_constraint = {
  lhs_mode : mode_meta;
  rhs_mode : mode_meta;
}

and mode_meta = {
  mode_id : int;
  mutable mode: mode option;
  mutable mode_constraints: mode_constraint list;
}

and ty_constraint = {
  mutable fired : bool;
  lower : meta;
  upper : meta;
}

and typ =
  | TUnit
  | TBool
  | TNat
  | TFloat of mode_meta
  | TPair of typ * typ
  | TSum  of typ * typ
  | TList of typ
  | TArrow of typ * typ
  | TMeta of meta

and meta = {
  ty_id : int;
  mutable ty_value: typ option;
  mutable constraints: ty_constraint list;
}

and texpr =
  | EVar of string
  | ELam of string * typed_expr
  | ERec of string * string * typed_expr
  | EApp of typed_expr * typed_expr
  | EUnit
  | EPair of typed_expr * typed_expr
  | EFst of typed_expr
  | ESnd of typed_expr
  | EInl of typed_expr
  | EInr of typed_expr
  | ENil
  | ECons of typed_expr * typed_expr
  | EMatch of typed_expr * (string * typed_expr) * (string * typed_expr)
  | EMatchList of typed_expr * typed_expr * (string * string * typed_expr)
  | EBool of bool
  | EIf of typed_expr * typed_expr * typed_expr
  | ELet of string * typed_expr * typed_expr
  | EConst of float
  | ENeg of typed_expr
  | EAdd of typed_expr * typed_expr
  | EMul of typed_expr * typed_expr
  | ESub of typed_expr * typed_expr
  | EDiv of typed_expr * typed_expr
  | ELt of typed_expr * typed_expr
  | ELeq of typed_expr * typed_expr
  | EUniform of typed_expr * typed_expr
  | EGauss of typed_expr * typed_expr
  | EExponential of typed_expr
  | EGamma of typed_expr * typed_expr
  | EBeta of typed_expr * typed_expr
  | EFlip of typed_expr
  | EBernoulli of typed_expr
  | EPoisson of typed_expr
  | EDiscrete of (float * typed_expr) list

and typed_expr = {
  expr : texpr;
  typ : typ;
}

let mode_counter = ref 0
let ty_counter = ref 0

let fresh_mode_meta () =
  incr mode_counter;
  { mode_id = !mode_counter; mode = None; mode_constraints = [] }

let fresh_meta () =
  incr ty_counter;
  { ty_id = !ty_counter; ty_value = None; constraints = [] }

let rec propagate_submode a b =
  match a.mode, b.mode with
  | Some E, None -> set_mode b E
  | Some G, Some E -> ()
  | Some G, None -> ()
  | Some E, Some G -> failwith "Inconsistent submode: E <= G"
  | Some E, Some E -> ()
  | None, Some G -> set_mode a G
  | None, Some E -> ()
  | None, None -> ()
  | Some G, Some G -> ()

and set_mode mvar m =
  match mvar.mode with
  | None ->
      mvar.mode <- Some m;
      List.iter (fun c -> propagate_submode c.lhs_mode c.rhs_mode) mvar.mode_constraints
  | Some m' ->
      if m <> m' then
        failwith (Format.sprintf "mode mismatch: %d has %s but tried to set %s"
                    mvar.mode_id
                    (match m' with G -> "G" | E -> "E")
                    (match m with G -> "G" | E -> "E"))

let submode a b =
  let c = { lhs_mode = a; rhs_mode = b } in
  a.mode_constraints <- c :: a.mode_constraints;
  b.mode_constraints <- c :: b.mode_constraints;
  propagate_submode a b

let rec propagate_subtype a b =
  match a.ty_value, b.ty_value with
  | Some t1, None -> set_type b t1
  | None, Some t2 -> set_type a t2
  | Some t1, Some t2 ->
      assert_subtype t1 t2
  | None, None -> ()

and fire_constraint c =
  if c.fired then () else
    (c.fired <- true;
     assert_subtype (TMeta c.lower) (TMeta c.upper))

(* and set_type metav t =
  match metav.ty_value with
  | None ->
      metav.ty_value <- Some t;
      List.iter fire_constraint metav.constraints
  | Some t' ->
      assert_subtype t t' *)
      
(* and zonk t =
  match t with
  | TMeta m ->
      (match m.ty_value with
       | None -> t
       | Some t' -> t')
  | _ -> t *)

and zonk t =
  let rec go seen t =
    match t with
    | TMeta m ->
        if List.mem m.ty_id seen then
          (* Cycle detected: leave as-is to avoid infinite loop. *)
          TMeta m
        else (
          match m.ty_value with
          | None -> TMeta m
          | Some t' ->
              let t'' = go (m.ty_id :: seen) t' in
              (* Path compression *)
              m.ty_value <- Some t'';
              t'')
    | _ -> t
  in
  go [] t

and set_type metav t =
  let t = zonk t in
  match metav.ty_value with
  | None ->
      (* Avoid setting ?t = ?t (or cycles) *)
      (match t with
        | TMeta m2 when m2.ty_id = metav.ty_id ->
            ()
        | _ ->
            metav.ty_value <- Some t;
            List.iter fire_constraint metav.constraints)
  | Some t' ->
      (* Both sides may have become more concrete; relate them *)
      assert_subtype t (zonk t')
  
and assert_subtype t1 t2 =
  match zonk t1, zonk t2 with
  | TFloat m1, TFloat m2 -> submode m1 m2
  | TPair (a1,b1), TPair (a2,b2)
  | TSum (a1,b1), TSum (a2,b2) ->
      assert_subtype a1 a2;
      assert_subtype b1 b2
  | TList a1, TList a2 ->
      assert_subtype a1 a2
  | TArrow (a1,b1), TArrow (a2,b2) ->
      assert_subtype a2 a1;
      assert_subtype b1 b2
  | TUnit, TUnit -> ()
  | TBool, TBool -> ()
  | TNat, TNat -> ()
  | TMeta m1, TMeta m2 when m1.ty_id = m2.ty_id -> ()
  | TMeta m1, t2 ->
      set_type m1 t2
  | t1, TMeta m2 ->
      set_type m2 t1
  | t1, t2 ->
      if t1 <> t2 then failwith "type mismatch"

let subtype a b =
  let c = { fired = false; lower = a; upper = b } in
  a.constraints <- c :: a.constraints;
  b.constraints <- c :: b.constraints;
  fire_constraint c
