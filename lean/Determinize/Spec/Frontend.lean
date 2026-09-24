import Determinize.Spec.Syntax

/-!
# Programs of the front end

The front end has rational literals, and uses the constructors of `Expr` with three kinds of
sites:

* `Input`, the resolved source program: a site carries the affinity the program requests, or
  `none`, a placeholder for an affinity to infer;
* `Annotated`, what inference returns: every site samples at an affinity;
* `Core`, the paper's sites: a site samples at an affinity or computes a mean. The runtime and
  the finite models run core programs, determinized ones included.
-/

namespace Determinize.Spec.Paper

abbrev Core := Expr Rat

def interpret (e : Core) : Expr := e.map (fun (q : Rat) => (q : ℝ)) id

/-- Resolved source syntax. A site carries its requested affinity, or `none` if the affinity is
to be inferred. -/
abbrev Input := Expr Rat (Option Affinity)

/-- A program whose sites all sample at an affinity. -/
abbrev Annotated := Expr Rat Affinity

/-- An annotated program is a core program without mean sites. Lean inserts this conversion
where a core program is expected, as in `interpret program`. -/
@[coe] def Annotated.toCore (program : Annotated) : Core := program.map id .sample

instance : Coe Annotated Core := ⟨Annotated.toCore⟩

/-- `e` and `e'` are the same program, with the same constructors, literals and variable
indices, and `R` relates every site of `e` to the corresponding site of `e'`. -/
def Expr.Sitewise {Literal Site Site' : Type} (R : Site → Site' → Prop) :
    Expr Literal Site → Expr Literal Site' → Prop
  | .bvar index, .bvar index' => index = index'
  | .bool value, .bool value' => value = value'
  | .real value, .real value' => value = value'
  | .reject, .reject | .unit, .unit | .nil, .nil => True
  | .lam body, .lam body' | .fix body, .fix body' | .fst body, .fst body'
  | .snd body, .snd body' | .inl body, .inl body' | .inr body, .inr body'
  | .neg body, .neg body' =>
      Sitewise R body body'
  | .app a b, .app a' b' | .pair a b, .pair a' b' | .cons a b, .cons a' b'
  | .letE a b, .letE a' b' | .add a b, .add a' b' | .mul a b, .mul a' b'
  | .div a b, .div a' b' | .lt a b, .lt a' b' =>
      Sitewise R a a' ∧ Sitewise R b b'
  | .matchSum a b c, .matchSum a' b' c' | .matchList a b c, .matchList a' b' c'
  | .ite a b c, .ite a' b' c' =>
      Sitewise R a a' ∧ Sitewise R b b' ∧ Sitewise R c c'
  | .poisson s a, .poisson s' a' | .discrete s a, .discrete s' a'
  | .bernoulli s a, .bernoulli s' a' | .exponential s a, .exponential s' a' =>
      R s s' ∧ Sitewise R a a'
  | .uniform s a b, .uniform s' a' b' | .gaussian s a b, .gaussian s' a' b'
  | .beta s a b, .beta s' a' b' | .gamma s a b, .gamma s' a' b' =>
      R s s' ∧ Sitewise R a a' ∧ Sitewise R b b'
  | _, _ => False

/-- `program` keeps every constructor, literal, variable index and requested affinity of
`input`, and fills its placeholders. -/
def Input.matches (input : Input) (program : Annotated) : Prop :=
  input.Sitewise (fun requested affinity => requested = none ∨ requested = some affinity) program

end Determinize.Spec.Paper
