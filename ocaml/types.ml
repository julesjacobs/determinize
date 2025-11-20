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
  | EMatch of typed_expr * (string * typed_expr) * (string * typed_expr)
  | EBool of bool
  | EIf of typed_expr * typed_expr * typed_expr
  | ELet of string * typed_expr * typed_expr
  | EConst of float
  | ENeg of typed_expr
  | EAdd of typed_expr * typed_expr
  | EMul of typed_expr * typed_expr
  | ELt of typed_expr * typed_expr
  | EUniform of typed_expr * typed_expr
  | EGauss of typed_expr * typed_expr

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
      if t1 <> t2 then
        failwith (Format.sprintf "type mismatch: meta %d and %d" a.ty_id b.ty_id)
  | None, None -> ()

and set_type metav t =
  match metav.ty_value with
  | None ->
      metav.ty_value <- Some t;
      List.iter (fun c -> propagate_subtype c.lower c.upper) metav.constraints
  | Some t' ->
      if t <> t' then
        failwith (Format.sprintf "type mismatch: meta %d has different types" metav.ty_id)

let subtype a b =
  let c = { lower = a; upper = b } in
  a.constraints <- c :: a.constraints;
  b.constraints <- c :: b.constraints;
  propagate_subtype a b
