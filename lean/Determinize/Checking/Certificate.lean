import Determinize.Spec.Syntax

namespace Determinize.Checking
open Spec.Paper

def setAffinity (action : DistributionAction) (affinity : Affinity) : DistributionAction :=
  match action with
  | .sample _ => .sample affinity
  | .mean => .mean

inductive Certificate where
  | node (ty : Ty) (children : List Certificate)
  | sub (ty : Ty) (child : Certificate)
deriving Repr, BEq

def Certificate.ty : Certificate → Ty
  | .node ty _ | .sub ty _ => ty

abbrev Core := Expr Rat

def interpret (e : Core) : Expr := e.mapLiteral (fun (q : Rat) => (q : ℝ))

end Determinize.Checking
