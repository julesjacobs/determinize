import Determinize.Statement.Primitives

namespace Determinize.Statement.FiniteModel
open Paper

/-- Initial exporter's eligibility rule for an evaluated sampling call. This does
not check arity or the parameter domain. All mean formulas are rational on rational
parameters; stochastic export initially supports only Bernoulli and finite discrete. -/
def supportedDraw : Kind → Op → Bool
  | .mean, _ => true
  | .stochastic, .bernoulli | .stochastic, .discrete _ => true
  | .stochastic, _ => false

end Determinize.Statement.FiniteModel
