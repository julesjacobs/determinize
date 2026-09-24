import Tests.Parsing

namespace Determinize.Tests
open Frontend Checking Spec.Paper

/-! `Input.matches` accepts exactly the programs that keep every constructor, payload and
requested affinity of the resolved input, and `infer` returns such a program. -/

private def inferred (input : Input) : Option (Core × Ty) := (infer input).toOption

def matching : IO Unit := do
  let floatE := Ty.float .E
  let draw : Core := .uniform (.sample .E) (.real 0) (.real 1)
  let drawInput : Input := .uniform (some .E) (.real 0) (.real 1)
  let generalInput : Input := .uniform (some .G) (.real 0) (.real 1)
  assert (inferred drawInput == some (draw, floatE)) "explicit E draw not inferred"
  assert (!drawInput.matches (.uniform (.sample .E) (.real 0) (.real 2))) "changed literal accepted"
  assert (!generalInput.matches draw) "explicit sampling affinity changed"
  assert (!drawInput.matches draw.determinize) "target accepted as source"
  let coin : Core := .bernoulli (.sample .E) (.real (1/4))
  let coinInput : Input := .bernoulli (some .E) (.real (1/4))
  assert (inferred coinInput == some (coin, floatE)) "Bernoulli draw not inferred"
  assert (!coinInput.matches (.bernoulli (.sample .E) (.real (1/2))))
    "changed Bernoulli probability accepted"
  assert (!coinInput.matches coin.determinize) "Bernoulli mean accepted as a source draw"
  assert (!coinInput.matches (.poisson (.sample .E) (.real (1/4)))) "different distribution matched"
  let d : Core := .cons (.real (1/6)) (.cons (.real (1/3)) .nil)
  let changed : Core := .cons (.real (1/2)) (.cons (.real (1/3)) .nil)
  let categorical : Core := .discrete (.sample .E) d
  let categoricalInput : Input := .discrete (some .E) (.cons (.real (1/6)) (.cons (.real (1/3)) .nil))
  assert (inferred categoricalInput == some (categorical, floatE)) "discrete draw not inferred"
  assert (!categoricalInput.matches (.discrete (.sample .E) changed))
    "changed discrete weights accepted"
  assert (!categoricalInput.matches categorical.determinize) "discrete mean accepted as a source draw"
  assert (!(Input.discrete (some .G) (.cons (.real (1/6)) (.cons (.real (1/3)) .nil))).matches categorical)
    "discrete sampling affinity changed"
  let nestedInput : Input := .uniform (some .E)
    (.bernoulli (some .G) (.real (1/4))) (.real 1)
  let nested : Core := .uniform (.sample .E)
    (.bernoulli (.sample .G) (.real (1/4))) (.real 1)
  assert (inferred nestedInput == some (nested, floatE)) "nested affinities not inferred"
  let swapped : Core := .uniform (.sample .G)
    (.bernoulli (.sample .E) (.real (1/4))) (.real 1)
  assert (!nestedInput.matches swapped) "nested affinities matched at the wrong sites"
  let unspecified : Input := .bernoulli none (.real (1/4))
  assert (unspecified.matches coin) "omitted affinity did not accept E"
  assert (unspecified.matches (.bernoulli (.sample .G) (.real (1/4)))) "omitted affinity did not accept G"
  assert (inferred unspecified == some (coin, floatE)) "omitted affinity not inferred as E"
  let lambdaInput : Input := .lam (.lam (.bvar 1))
  assert (inferred lambdaInput == some (.lam (.lam (.bvar 1)), .arr .unit (.arr .unit .unit)))
    "binders not inferred"
  assert (!lambdaInput.matches (.lam (.lam (.bvar 0)))) "changed binder reference accepted"
  let invalidDomainProgram ← IO.ofExcept (Frontend.compile "uniform[E](2,1)")
  assert (invalidDomainProgram.ty == floatE) "typing must not claim to establish domain safety"

end Determinize.Tests
