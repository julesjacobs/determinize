import Determinize.Spec.Frontend

/-!
# Affinity inference

`Frontend.infer` fills the omitted sample affinities of a resolved program `input : Input`.
A *completion* of `input` is an annotated program that fills exactly those affinities and is
closed and well typed at some type. Completions of the same input differ only in the affinities
of their free sample sites; they are compared site by site in the order `G ≤ E` of
`Ty.Sub.general`. The greatest completion has the most E sites, which determinization replaces
by the most means.

`Theorems.inferenceCorrectness` states that `infer` fails only on inputs without a completion, and
otherwise returns a completion, typed at the returned type, that lies above every completion: the
greatest completion.

The statement does not depend on how `infer` works. It determines the output program uniquely,
but not the output type.
-/

namespace Determinize.Spec

open Paper

/-- `program` fills exactly the omitted affinities of `input`, and is closed and well typed.
The type is existential: a completion counts even when it is typed only at a type other than
the one `infer` returns. -/
def Completion (input : Input) (program : Annotated) : Prop :=
  input.matches program ∧ ∃ ty, Typed [] (interpret program) ty

/-- `lower` and `upper` are the same program, except that at every site the affinity of `lower`
is below that of `upper` in the order `G ≤ E`. -/
def AffinityLE : Annotated → Annotated → Prop :=
  Expr.Sitewise fun affinity affinity' => Ty.Sub (.float affinity) (.float affinity')

end Determinize.Spec
