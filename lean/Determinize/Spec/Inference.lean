import Determinize.Spec.Frontend
import Determinize.Frontend.Infer

/-!
# Affinity inference

`Frontend.infer` fills the omitted sample affinities of a resolved program `input : Input`.
A *completion* of `input` is an annotated program that fills exactly those affinities and is
closed and well typed at some type. Completions of the same input differ only in the affinities
of their free sample sites; they are compared site by site in the order `G ≤ E` of
`Ty.Sub.general`. The greatest completion has the most E sites, which determinization replaces
by the most means.

`inferCorrectThm` states that `infer` fails only on inputs without a completion, and otherwise
returns a completion, typed at the returned type, that lies above every completion: the greatest
completion.

The statement does not depend on how `infer` works. It determines the output program uniquely,
but not the output type.
-/

namespace Determinize.Spec

open Paper

/-- `program` fills exactly the omitted affinities of `input`, and is closed and well typed.
The type is existential: a completion counts even when it is typed only at a type other than
the one `infer` returns. -/
def Completion (input : Input) (program : Core) : Prop :=
  input.matches program = true ∧ ∃ ty, Typed [] (interpret program) ty

/-- `lower` and `upper` are the same program, except that at every sample site the affinity of
`lower` is below that of `upper` in the order `G ≤ E`. Mean sites never occur in a completion
and are not related. -/
def AffinityLE : Core → Core → Prop
  | .bvar index, .bvar index' => index = index'
  | .reject, .reject => True
  | .unit, .unit => True
  | .bool value, .bool value' => value = value'
  | .real value, .real value' => value = value'
  | .nil, .nil => True
  | .lam body, .lam body' =>
      AffinityLE body body'
  | .fix body, .fix body' =>
      AffinityLE body body'
  | .app fn arg, .app fn' arg' =>
      AffinityLE fn fn' ∧ AffinityLE arg arg'
  | .pair left right, .pair left' right' =>
      AffinityLE left left' ∧ AffinityLE right right'
  | .fst body, .fst body' =>
      AffinityLE body body'
  | .snd body, .snd body' =>
      AffinityLE body body'
  | .inl body, .inl body' =>
      AffinityLE body body'
  | .inr body, .inr body' =>
      AffinityLE body body'
  | .matchSum scrutinee left right, .matchSum scrutinee' left' right' =>
      AffinityLE scrutinee scrutinee' ∧ AffinityLE left left' ∧ AffinityLE right right'
  | .cons head tail, .cons head' tail' =>
      AffinityLE head head' ∧ AffinityLE tail tail'
  | .matchList scrutinee nilCase consCase, .matchList scrutinee' nilCase' consCase' =>
      AffinityLE scrutinee scrutinee' ∧ AffinityLE nilCase nilCase' ∧ AffinityLE consCase consCase'
  | .ite condition thenBranch elseBranch, .ite condition' thenBranch' elseBranch' =>
      AffinityLE condition condition' ∧ AffinityLE thenBranch thenBranch' ∧
        AffinityLE elseBranch elseBranch'
  | .letE value body, .letE value' body' =>
      AffinityLE value value' ∧ AffinityLE body body'
  | .neg body, .neg body' =>
      AffinityLE body body'
  | .add left right, .add left' right' =>
      AffinityLE left left' ∧ AffinityLE right right'
  | .mul left right, .mul left' right' =>
      AffinityLE left left' ∧ AffinityLE right right'
  | .div left right, .div left' right' =>
      AffinityLE left left' ∧ AffinityLE right right'
  | .lt left right, .lt left' right' =>
      AffinityLE left left' ∧ AffinityLE right right'
  | .uniform (.sample affinity) lower upper, .uniform (.sample affinity') lower' upper' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE lower lower' ∧ AffinityLE upper upper'
  | .gaussian (.sample affinity) mean variance, .gaussian (.sample affinity') mean' variance' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE mean mean' ∧
        AffinityLE variance variance'
  | .poisson (.sample affinity) rate, .poisson (.sample affinity') rate' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE rate rate'
  | .bernoulli (.sample affinity) probability, .bernoulli (.sample affinity') probability' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE probability probability'
  | .exponential (.sample affinity) rate, .exponential (.sample affinity') rate' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE rate rate'
  | .beta (.sample affinity) alpha betaArg, .beta (.sample affinity') alpha' betaArg' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE alpha alpha' ∧
        AffinityLE betaArg betaArg'
  | .gamma (.sample affinity) shape rate, .gamma (.sample affinity') shape' rate' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE shape shape' ∧ AffinityLE rate rate'
  | .discrete (.sample affinity) distribution, .discrete (.sample affinity') distribution' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE distribution distribution'
  | _, _ => False

/-- Inference fails only when the input has no completion. Otherwise it returns a completion,
typed at the returned type, and every completion lies below it. -/
def inferCorrectThm : Prop :=
  ∀ input : Input,
    match Frontend.infer input with
    | .error _ =>
        ¬ ∃ program : Core, Completion input program
    | .ok (program, ty) =>
        input.matches program = true ∧
        Typed [] (interpret program) ty ∧
        ∀ completion : Core,
          Completion input completion → AffinityLE completion program

end Determinize.Spec
