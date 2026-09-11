import Tests.Parsing
import Determinize.Runtime.Eval
import Lean.Data.Json.FromToJson

namespace Determinize.Tests
open Frontend Checking Spec.Paper Lean

structure Moments where
  mean : Float
  variance : Float
  mean_tolerance : Float
  variance_tolerance : Float
  lower : Option Float := none
  upper : Option Float := none
  integer : Bool := false
  deriving FromJson

structure Observation where
  number : Option Float := none
  value : Option String := none
  error : Option String := none
  draws : Option Nat := none
  tolerance : Float := 1e-10
  moments : Option Moments := none
  deriving FromJson

structure CorpusCase where
  file : String
  suite : String
  outcome : String
  stage : String := ""
  expected_type : Option String := none
  modes : Option (List String) := none
  source : Option Observation := none
  target : Option Observation := none
  samples : Nat := 20000
  seed : Nat := 20260911
  fuel : Nat := 100000
  deriving FromJson

private def close (actual expected tolerance : Float) (label : String) : IO Unit :=
  assert (!actual.isNaN && !actual.isInf && Float.abs (actual - expected) ≤ tolerance)
    s!"{label}: expected {expected} ± {tolerance}, got {actual}"

private def numeric (v : Runtime.Value) : IO Float := match v with
  | .number x => pure x
  | _ => throw (IO.userError s!"expected numeric result, got {v.display}")

private def observation (e : Core) (c : CorpusCase) (o : Observation) : IO Unit := do
  if o.number.isNone && o.value.isNone && o.error.isNone && o.draws.isNone then return
  let result := Runtime.run e c.seed.toUInt64 c.fuel
  if let some expected := o.error then
    match result with
    | .error message =>
        assert ((message.splitOn expected).length > 1)
          s!"expected runtime error containing '{expected}', got '{message}'"
    | .ok _ => throw (IO.userError s!"expected runtime error '{expected}', execution succeeded")
  else
    let (v, state) ← IO.ofExcept result
    if let some expected := o.number then close (← numeric v) expected o.tolerance "result"
    if let some expected := o.value then
      assert (v.display == expected) s!"expected '{expected}', got '{v.display}'"
    if let some expected := o.draws then
      assert (state.draws == expected) s!"expected {expected} draws, got {state.draws}"

private def statistical (e : Core) (c : CorpusCase) (m : Moments) : IO Unit := do
  assert (c.samples ≥ 2) "statistical tests require at least two samples"
  let mut mean := 0.0
  let mut m2 := 0.0
  for i in [:c.samples] do
    let seed := c.seed.toUInt64 + i.toUInt64 * 0x9e3779b97f4a7c15
    let (v, _) ← IO.ofExcept (Runtime.run e seed c.fuel)
    let x ← numeric v
    assert (!x.isNaN && !x.isInf) s!"nonfinite sample at index {i}"
    if let some lower := m.lower then assert (x ≥ lower) s!"sample {x} below support {lower}"
    if let some upper := m.upper then assert (x ≤ upper) s!"sample {x} above support {upper}"
    if m.integer then assert (x == Float.floor x) s!"noninteger sample {x}"
    let delta := x - mean
    mean := mean + delta / Float.ofNat (i + 1)
    m2 := m2 + delta * (x - mean)
  let variance := m2 / Float.ofNat (c.samples - 1)
  close mean m.mean m.mean_tolerance "mean"
  close variance m.variance m.variance_tolerance "variance"
  IO.println s!"    mean={mean}, variance={variance}, n={c.samples}"

private def compileStage (text : String) : Except (String × String) Program := do
  let ast ← (parse text).mapError ("parse", ·)
  let input ← (elaborate ast).mapError ("elaboration", ·)
  let (source, cert) ← (infer input).mapError ("inference", ·)
  let some checked := certify input.expression source input.modes cert
    | throw ("certificate", "inference produced an invalid certificate")
  return ⟨input, checked⟩

private def runCase (c : CorpusCase) (withStatistics : Bool) : IO Unit := do
  let text ← IO.FS.readFile c.file
  if c.outcome == "reject" then
    assert (!(compile text).isOk) "expected rejection, production compiler accepted the program"
    match compileStage text with
    | .error (stage, message) =>
        assert (stage == c.stage)
          s!"expected {c.stage} rejection, got {stage}: {message}"
    | .ok _ => throw (IO.userError "expected rejection, compilation succeeded")
  else
    let p ← IO.ofExcept (compile text)
    if let some ty := c.expected_type then
      assert (prettyType p.checked.ty == ty) s!"expected type {ty}, got {prettyType p.checked.ty}"
    if let some modes := c.modes then
      let actual := (sampleModes p.checked.source).map prettyMode
      assert (actual == modes) s!"expected modes {modes}, got {actual}"
    for (label, e, expectation) in [("source", p.checked.source, c.source),
        ("target", p.checked.source.determinize, c.target)] do
      if let some o := expectation then
        try
          observation e c o
          if withStatistics then
            if let some m := o.moments then
              IO.println s!"  {label}"
              statistical e c m
        catch ex => throw (IO.userError s!"{label}: {ex}")

def corpus (manifest : String) (withStatistics : Bool) : IO Unit := do
  let json ← IO.ofExcept (Json.parse (← IO.FS.readFile manifest))
  let cases ← IO.ofExcept (fromJson? json : Except String (Array CorpusCase))
  for c in cases do
    try
      runCase c withStatistics
      IO.println s!"PASS {c.file}"
    catch ex => throw (IO.userError s!"{c.file}: {ex}")
  IO.println s!"{cases.size} corpus cases passed."

end Determinize.Tests
