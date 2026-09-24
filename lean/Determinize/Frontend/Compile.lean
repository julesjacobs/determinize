import Determinize.Frontend.Parser
import Determinize.Frontend.Elaborate
import Determinize.Theorems

namespace Determinize.Frontend
open Spec.Paper

/-- A compiled program: the resolved input, the annotated program and type that `infer` returns,
and the guarantee of `Theorems.inferenceCorrectness` for them. -/
structure Program where
  input : Input
  annotated : Annotated
  ty : Ty
  aligned : input.matches annotated
  typed : Typed [] (interpret annotated) ty

/-- The annotated program as a core program, which the runtime and the finite models run. -/
def Program.source (p : Program) : Core := p.annotated.toCore

def compile (text : String) : Except String Program := do
  let input ← elaborate (← parse text)
  match inferred : infer input with
  | .ok (annotated, ty) =>
    have correct := inferred ▸ Theorems.inferenceCorrectness input
    return { input, annotated, ty, aligned := correct.1, typed := correct.2.1 }
  | .error message => throw message

end Determinize.Frontend
