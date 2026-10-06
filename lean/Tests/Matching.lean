import Tests.Parsing

namespace Determinize.Tests
open Frontend Spec.Paper

/-! `Input.Matches` accepts exactly the programs that keep every constructor, payload and
requested affinity of the resolved input, and `infer` returns such a program. A program with a
mean site in place of a draw is not an `Annotated` program, so `Matches` cannot accept it. -/

private def inferred (input : Input) : Option (Annotated × Ty) := (infer input).toOption

def matching : IO Unit := do
  let floatE := Ty.float .E
  let draw : Annotated := .uniform .E (.real 0) (.real 1)
  let drawInput : Input := .uniform (some .E) (.real 0) (.real 1)
  let generalInput : Input := .uniform (some .G) (.real 0) (.real 1)
  check (inferred drawInput == some (draw, floatE)) "explicit E draw not inferred"
  check (!drawInput.Matches (.uniform .E (.real 0) (.real 2))) "changed literal accepted"
  check (!generalInput.Matches draw) "explicit sampling affinity changed"
  let coin : Annotated := .bernoulli .E (.real (1 / 4))
  let coinInput : Input := .bernoulli (some .E) (.real (1 / 4))
  check (inferred coinInput == some (coin, floatE)) "Bernoulli draw not inferred"
  check (!coinInput.Matches (.bernoulli .E (.real (1 / 2))))
    "changed Bernoulli probability accepted"
  check (!coinInput.Matches (.poisson .E (.real (1 / 4)))) "different distribution matched"
  let d : Annotated := .cons (.real (1 / 6)) (.cons (.real (1 / 3)) .nil)
  let changed : Annotated := .cons (.real (1 / 2)) (.cons (.real (1 / 3)) .nil)
  let categorical : Annotated := .discrete .E d
  let categoricalInput : Input :=
    .discrete (some .E) (.cons (.real (1 / 6)) (.cons (.real (1 / 3)) .nil))
  check (inferred categoricalInput == some (categorical, floatE)) "discrete draw not inferred"
  check (!categoricalInput.Matches (.discrete .E changed))
    "changed discrete weights accepted"
  check (!Input.Matches (.discrete (some .G) (.cons (.real (1 / 6)) (.cons (.real (1 / 3)) .nil)))
      categorical)
    "discrete sampling affinity changed"
  let nestedInput : Input := .uniform (some .E)
    (.bernoulli (some .G) (.real (1 / 4))) (.real 1)
  let nested : Annotated := .uniform .E
    (.bernoulli .G (.real (1 / 4))) (.real 1)
  check (inferred nestedInput == some (nested, floatE)) "nested affinities not inferred"
  let swapped : Annotated := .uniform .G
    (.bernoulli .E (.real (1 / 4))) (.real 1)
  check (!nestedInput.Matches swapped) "nested affinities matched at the wrong sites"
  let unspecified : Input := .bernoulli none (.real (1 / 4))
  check (unspecified.Matches coin) "omitted affinity did not accept E"
  check (unspecified.Matches (.bernoulli .G (.real (1 / 4)))) "omitted affinity did not accept G"
  check (inferred unspecified == some (coin, floatE)) "omitted affinity not inferred as E"
  let lambdaInput : Input := .lam (.lam (.bvar 1))
  check (inferred lambdaInput == some (.lam (.lam (.bvar 1)), .arr .unit (.arr .unit .unit)))
    "binders not inferred"
  check (!lambdaInput.Matches (.lam (.lam (.bvar 0)))) "changed binder reference accepted"
  let invalidDomainProgram ← IO.ofExcept (Frontend.compile "uniform[E](2,1)")
  check (invalidDomainProgram.ty == floatE) "typing must not claim to establish domain safety"

end Determinize.Tests
