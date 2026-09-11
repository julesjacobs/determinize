import Determinize.Checking.Certificate

namespace Determinize.Runtime
open Spec.Paper

abbrev RandomM := StateT UInt64 (Except String)

/-- SplitMix64; numerical sampling is deliberately outside the verified semantics. -/
def uniform01 : RandomM Float := do
  let next := (← get) + 0x9e3779b97f4a7c15
  set next
  let z := (next ^^^ (next >>> 30)) * 0xbf58476d1ce4e5b9
  let z := (z ^^^ (z >>> 27)) * 0x94d049bb133111eb
  let z := z ^^^ (z >>> 31)
  return ((z >>> 12).toFloat + 0.5) / 4503599627370496.0

private def normal : RandomM Float := do
  let u ← uniform01; let v ← uniform01
  return Float.sqrt (-2.0 * Float.log u) * Float.cos (6.283185307179586 * v)

private def gammaLarge (a : Float) : RandomM Float := do
  let d := a - 1.0 / 3.0
  let c := 1.0 / Float.sqrt (9.0 * d)
  for _ in [:100000] do
    let x ← normal
    let v := 1.0 + c * x
    if v > 0 then
      let v := v * v * v
      let u ← uniform01
      if u < 1.0 - 0.0331 * x * x * x * x ||
          Float.log u < 0.5 * x * x + d * (1.0 - v + Float.log v) then
        return d * v
  throw "gamma sampler exceeded its rejection limit"

private def gammaDraw (a : Float) : RandomM Float := do
  if a < 1.0 then
    let x ← gammaLarge (a + 1.0); let u ← uniform01
    return x * Float.pow u (1.0 / a)
  else gammaLarge a

private def poissonDraw (rate : Float) : RandomM Float := do
  -- Independent Poisson pieces avoid exp(-rate) underflow.
  if rate > 1000000.0 then throw "Poisson rate exceeds numerical runtime limit (1000000)"
  let mut remaining := rate
  let mut result := 0.0
  while remaining > 0 do
    let part := min remaining 20.0
    remaining := remaining - part
    let threshold := Float.exp (-part)
    let mut product := 1.0
    let mut count := 0
    while product > threshold do
      if count ≥ 100000 then throw "Poisson sampler exceeded its iteration limit"
      count := count + 1
      product := product * (← uniform01)
    result := result + (Float.ofNat (count - 1))
  return result

private def finite (x : Float) : Bool := !x.isNaN && !x.isInf

def sample (op : Op) (action : DistributionAction) (args : List Float) : RandomM Float := do
  unless args.all finite do throw "nonfinite distribution parameter"
  let mean := action == .mean
  let result ← match op,args with
    | .uniform,[a,b] =>
      if a > b then throw "uniform requires lower ≤ upper"
      else if mean then pure (a / 2.0 + b / 2.0)
      else if a == b then pure a
      else do let u ← uniform01; pure (a * (1.0 - u) + b * u)
    | .gaussian,[a,v] =>
      if v < 0 then throw "gaussian requires variance ≥ 0"
      else if mean || v == 0 then pure a
      else do pure (a + Float.sqrt v * (← normal))
    | .discrete d,[] => do
      if mean then pure (Float.ofInt d.mean.num / Float.ofNat d.mean.den)
      else
        let u ← uniform01
        let mut cumulative := 0.0
        let mut last := 0
        let mut selected := none
        for (p, i) in d.probabilities.zipIdx do
          if p > 0 then
            last := i
            let probability := Float.ofInt p.num / Float.ofNat p.den
            unless finite probability do throw "nonfinite discrete probability"
            cumulative := cumulative + probability
            if selected.isNone && u < cumulative then selected := some i
        pure (Float.ofNat (selected.getD last))
    | .bernoulli,[a] =>
      if a < 0 || a > 1 then throw "bernoulli requires probability in [0,1]"
      else if mean then pure a
      else do pure (if (← uniform01) < a then 1 else 0)
    | .poisson,[a] =>
      if a < 0 then throw "poisson requires rate ≥ 0"
      else if mean then pure a else poissonDraw a
    | .exponential,[a] =>
      if a ≤ 0 then throw "exponential requires rate > 0"
      else if mean then pure (1.0 / a)
      else do pure (-Float.log (← uniform01) / a)
    | .gamma,[a,b] =>
      if a ≤ 0 || b ≤ 0 then throw "gamma requires positive shape and rate"
      else if mean then pure (a / b)
      else do pure ((← gammaDraw a) / b)
    | .beta,[a,b] =>
      if a ≤ 0 || b ≤ 0 then throw "beta requires positive parameters"
      else if mean then pure (a / (a + b))
      else do
        let x ← gammaDraw a; let y ← gammaDraw b
        pure (x / (x + y))
    | _,_ => throw "invalid primitive arity"
  unless finite result do throw "nonfinite numerical sampling result"
  return result

end Determinize.Runtime
