open Ast
open Types

(* Placeholder type inference scaffolding. *)

type env = (string * typ) list

type infer_result = typed_expr

let infer (_env : env) (e : Ast.expr) : infer_result =
  match e with
  | Var _ -> failwith "TODO"
  | Lam _ -> failwith "TODO"
  | Rec _ -> failwith "TODO"
  | App _ -> failwith "TODO"
  | Unit -> failwith "TODO"
  | Pair _ -> failwith "TODO"
  | Fst _ -> failwith "TODO"
  | Snd _ -> failwith "TODO"
  | Inl _ -> failwith "TODO"
  | Inr _ -> failwith "TODO"
  | Case _ -> failwith "TODO"
  | Bool _ -> failwith "TODO"
  | If _ -> failwith "TODO"
  | Let _ -> failwith "TODO"
  | Const _ -> failwith "TODO"
  | Neg _ -> failwith "TODO"
  | Add _ -> failwith "TODO"
  | Mul _ -> failwith "TODO"
  | Lt _ -> failwith "TODO"
  | Uniform _ -> failwith "TODO"
  | Gauss _ -> failwith "TODO"
