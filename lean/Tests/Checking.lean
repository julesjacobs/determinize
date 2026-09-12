import Tests.Parsing
import Determinize.Checking.FiniteDistribution
import Determinize.Proof.Checking.Elaboration

namespace Determinize.Tests
open Checking Spec.Paper

private def floatE := Ty.float .E
private def draw : Core := .uniform (.sample .E) (.real 0) (.real 1)
private def evidence : Certificate := .node floatE [.node floatE [], .node floatE []]

example : Typed [] (interpret draw) floatE :=
  Determinize.Proof.Checking.check_sound (by decide +kernel : (check [] draw floatE evidence).isSome = true)

example : interpret draw.determinize = (interpret draw).determinize :=
  Determinize.Proof.Checking.interpret_determinize draw

def checking : IO Unit := do
  let drawInput : Input := .uniform (some .E) (.real 0) (.real 1)
  let generalInput : Input := .uniform (some .G) (.real 0) (.real 1)
  assert ((certify drawInput draw evidence).isSome) "valid certificate rejected"
  assert (!(check [] (.bvar 0) floatE (.node floatE [])).isSome) "unbound variable certificate accepted"
  assert (!(check [] draw floatE (.node floatE [.node .bool [], .node floatE []])).isSome)
    "false child type accepted"
  assert (!(certify drawInput (.uniform (.sample .E) (.real 0) (.real 2)) evidence).isSome)
    "changed literal accepted"
  assert (!(certify generalInput draw evidence).isSome) "explicit sampling affinity changed"
  assert (!(certify drawInput draw.determinize evidence).isSome) "target accepted as source"
  let coin : Core := .bernoulli (.sample .E) (.real (1/4))
  let coinInput : Input := .bernoulli (some .E) (.real (1/4))
  let coinEvidence : Certificate := .node floatE [.node floatE []]
  assert ((certify coinInput coin coinEvidence).isSome) "Bernoulli certificate rejected"
  assert (!(certify coinInput (.bernoulli (.sample .E) (.real (1/2)))
    coinEvidence).isSome) "changed Bernoulli probability accepted"
  assert (!(certify coinInput coin.determinize coinEvidence).isSome)
    "Bernoulli mean accepted as a source draw"
  assert (!(check [] coin floatE (.node floatE [])).isSome) "omitted probability evidence accepted"
  let d ← IO.ofExcept (finiteDistribution [1/6,1/3,1/2])
  let changed ← IO.ofExcept (finiteDistribution [1/2,1/3,1/6])
  let categorical : Core := .discrete (.sample .E) d
  let categoricalInput : Input := .discrete (some .E) d
  let categoricalEvidence : Certificate := .node floatE []
  assert ((certify categoricalInput categorical categoricalEvidence).isSome)
    "discrete certificate rejected"
  assert (!(certify categoricalInput (.discrete (.sample .E) changed)
    categoricalEvidence).isSome) "changed discrete weights accepted"
  assert (!(certify categoricalInput categorical.determinize categoricalEvidence).isSome)
    "discrete mean accepted as a source draw"
  assert (!(certify (.discrete (some .G) d) categorical categoricalEvidence).isSome)
    "discrete sampling affinity changed"
  let nestedInput : Input := .uniform (some .E)
    (.bernoulli (some .G) (.real (1/4))) (.real 1)
  let nested : Core := .uniform (.sample .E)
    (.bernoulli (.sample .G) (.real (1/4))) (.real 1)
  let nestedEvidence : Certificate := .node floatE
    [.sub floatE (.node (.float .G) [.node (.float .G) []]), .node floatE []]
  assert ((certify nestedInput nested nestedEvidence).isSome) "nested affinities rejected"
  let swapped : Core := .uniform (.sample .G)
    (.bernoulli (.sample .E) (.real (1/4))) (.real 1)
  assert (!(nestedInput.matches swapped)) "nested affinities matched at the wrong sites"
  let unspecified : Input := .bernoulli none (.real (1/4))
  assert ((certify unspecified coin coinEvidence).isSome) "omitted affinity did not accept E"
  assert ((certify unspecified (.bernoulli (.sample .G) (.real (1/4)))
    (.node (.float .G) [.node (.float .G) []])).isSome) "omitted affinity did not accept G"
  assert (!(certify coinInput (.poisson (.sample .E) (.real (1/4))) coinEvidence).isSome)
    "different distribution matched"
  let lambdaInput : Input := .lam (.lam (.bvar 1))
  let lambdaTy := Ty.arr .unit (.arr .unit .unit)
  let lambdaEvidence : Certificate := .node lambdaTy
    [.node (.arr .unit .unit) [.node .unit []]]
  assert ((certify lambdaInput (.lam (.lam (.bvar 1))) lambdaEvidence).isSome)
    "valid binder certificate rejected"
  assert (!(certify lambdaInput (.lam (.lam (.bvar 0))) lambdaEvidence).isSome)
    "changed binder reference accepted"
  let g := Ty.float .G
  let e := Ty.float .E
  for (a,b) in [(g,e), (.prod g g,.prod e e), (.sum g g,.sum e e),
      (.list g,.list e), (.arr e g,.arr g e)] do
    assert ((checkSubtype a b).isSome) s!"valid subtype rejected: {repr a} <= {repr b}"
    assert (!(checkSubtype b a).isSome) s!"invalid reverse subtype accepted: {repr b} <= {repr a}"
  assert (!(checkSubtype (.arr g g) (.arr e e)).isSome) "covariant function argument accepted"
  assert (!(check [] draw g (.sub g evidence)).isSome) "E-to-G subsumption accepted"
  let function : Core := .fix (.bvar 0)
  let originalTy := Ty.arr e e
  let widenedTy := Ty.arr g e
  let functionEvidence : Certificate := .sub widenedTy (.node originalTy [.node e []])
  assert ((check [] function widenedTy functionEvidence).isSome) "recursive function widening rejected"
  assert ((check [] (.app function (.real 1)) e
    (.node e [functionEvidence, .node g []])).isSome) "widened recursive application rejected"
  let invalidDomainProgram ← IO.ofExcept (Frontend.compile "uniform[E](2,1)")
  assert (invalidDomainProgram.checked.ty == floatE) "typing must not claim to establish domain safety"

#print axioms Determinize.Proof.Checking.check_sound
#print axioms Determinize.Proof.Checking.certified_trace_soundness

end Determinize.Tests
