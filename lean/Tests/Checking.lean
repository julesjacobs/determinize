import Tests.Parsing
import Determinize.Checking.FiniteDistribution
import Determinize.Proof.Checking.Elaboration

namespace Determinize.Tests
open Checking Spec.Paper

private def floatE := Ty.float .E
private def draw : Core := .uniform .E .stochastic (.real 0) (.real 1)
private def evidence : Certificate := .node floatE [.node floatE [], .node floatE []]

example : Typed [] (interpret draw) floatE :=
  Determinize.Proof.Checking.check_sound (by decide +kernel : (check [] draw floatE evidence).isSome = true)

example : interpret draw.determinize = (interpret draw).determinize :=
  Determinize.Proof.Checking.interpret_determinize draw

def checking : IO Unit := do
  assert ((certify draw draw [some .E] evidence).isSome) "valid certificate rejected"
  assert (!(check [] (.bvar 0) floatE (.node floatE [])).isSome) "unbound variable certificate accepted"
  assert (!(check [] draw floatE (.node floatE [.node .bool [], .node floatE []])).isSome)
    "false child type accepted"
  assert (!(certify draw (.uniform .E .stochastic (.real 0) (.real 2)) [none] evidence).isSome)
    "changed literal accepted"
  assert (!(certify draw draw [some .G] evidence).isSome) "explicit sampling mode changed"
  assert (!(certify draw draw.determinize [none] evidence).isSome) "target accepted as source"
  let coin : Core := .bernoulli .E .stochastic (.real (1/4))
  let coinEvidence : Certificate := .node floatE [.node floatE []]
  assert ((certify coin coin [some .E] coinEvidence).isSome) "Bernoulli certificate rejected"
  assert (!(certify coin (.bernoulli .E .stochastic (.real (1/2))) [some .E]
    coinEvidence).isSome) "changed Bernoulli probability accepted"
  assert (!(certify coin coin.determinize [some .E] coinEvidence).isSome)
    "Bernoulli mean accepted as a source draw"
  assert (!(check [] coin floatE (.node floatE [])).isSome) "omitted probability evidence accepted"
  let d ← IO.ofExcept (finiteDistribution [1/6,1/3,1/2])
  let changed ← IO.ofExcept (finiteDistribution [1/2,1/3,1/6])
  let categorical : Core := .discrete .E .stochastic d
  let categoricalEvidence : Certificate := .node floatE []
  assert ((certify categorical categorical [some .E] categoricalEvidence).isSome)
    "discrete certificate rejected"
  assert (!(certify categorical (.discrete .E .stochastic changed) [some .E]
    categoricalEvidence).isSome) "changed discrete weights accepted"
  assert (!(certify categorical categorical.determinize [some .E] categoricalEvidence).isSome)
    "discrete mean accepted as a source draw"
  assert (!(certify categorical categorical [some .G] categoricalEvidence).isSome)
    "discrete sampling mode changed"
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
