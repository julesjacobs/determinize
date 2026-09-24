import Determinize.Finite.Explore
import Determinize.Frontend.Compile
import Determinize.Frontend.Pretty
import Determinize.Runtime.Eval
import Determinize.Finite.Export
import Determinize.Finite.Reward.Explore
import Determinize.Finite.Reward.Export

open Determinize Determinize.Frontend Determinize.Checking

private structure Options where
  file : String := ""
  seed : UInt64 := 0
  samples : Nat := 0
  fuel : Nat := 100000
  checkOnly : Bool := false
  exportPrefix : Option String := none
  certifyResult : Bool := false
  additive : Bool := false
  sampleSites : Bool := false
  solveLimits : Finite.SolveLimits := {}
  subject : Spec.FiniteModel.Subject := .determinized
  limits : Finite.Limits := {}

private def usage := "Usage: determinize [--check] [--samples N] [--seed N] [--fuel N] [--export PREFIX | --result PREFIX] [--additive] [--sample-sites] [--max-result-states N] [--subject source|determinized] [--max-states N] [--max-edges N] [--max-state-bytes N] FILE.det"
private def natural (s : String) : Except String Nat :=
  match s.toNat? with
  | some n => .ok n
  | none => .error s!"expected a nonnegative integer, got '{s}'"
private def options : List String → Options → Except String Options
  | [], o => if o.file.isEmpty then .error usage else .ok o
  | "--additive" :: rest, o => options rest {o with additive := true}
  | "--sample-sites" :: rest, o => options rest {o with sampleSites := true}
  | "--result" :: outputPath :: rest, o =>
      options rest {o with exportPrefix := some outputPath, certifyResult := true}
  | "--max-result-states" :: n :: rest, o => do
      options rest {o with solveLimits.maxStates := ← natural n}
  | "--export" :: outputPath :: rest, o => options rest {o with exportPrefix := some outputPath}
  | "--subject" :: subject :: rest, o => do
      let subject ← match subject with
        | "source" => pure Spec.FiniteModel.Subject.source
        | "determinized" => pure .determinized
        | _ => throw "subject must be source or determinized"
      options rest {o with subject}
  | "--max-states" :: n :: rest, o => do
      options rest {o with limits.maxStates := ← natural n}
  | "--max-edges" :: n :: rest, o => do
      options rest {o with limits.maxEdges := ← natural n}
  | "--max-state-bytes" :: n :: rest, o => do
      options rest {o with limits.maxStateBytes := ← natural n}
  | "--check" :: rest, o => options rest {o with checkOnly := true}
  | "--samples" :: n :: rest, o => do options rest {o with samples := ← natural n}
  | "--seed" :: n :: rest, o => do
      let seed ← natural n
      if seed ≥ 2^64 then throw "seed must fit in 64 bits"
      options rest {o with seed := UInt64.ofNat seed}
  | "--fuel" :: n :: rest, o => do options rest {o with fuel := ← natural n}
  | arg :: rest, o =>
    if arg.startsWith "-" then .error s!"unknown or incomplete option '{arg}'\n{usage}"
    else if !o.file.isEmpty then .error "expected one input file"
    else options rest {o with file := arg}

private def summarize (label : String) (e : Core) (o : Options) : IO Unit := do
  let mut count := 0
  let mut mean := 0.0
  let mut m2 := 0.0
  let mut first := ""
  let mut failed := 0
  let mut rejected := 0
  let mut error := ""
  for i in [:o.samples] do
    match Runtime.runOutcome e (o.seed + UInt64.ofNat i) o.fuel with
    | .error msg => failed := failed + 1; if error.isEmpty then error := msg
    | .ok .rejected => rejected := rejected + 1
    | .ok (.returned v _) =>
      if first.isEmpty then first := v.display
      match v with
      | .number x =>
          count := count + 1
          let delta := x - mean
          mean := mean + delta / Float.ofNat count
          m2 := m2 + delta * (x - mean)
      | _ => pure ()
  IO.println s!"{label}: {o.samples - failed - rejected}/{o.samples} runs returned a value"
  if count > 0 then
    let variance := m2 / Float.ofNat count
    let meanText := if mean.isNaN || mean.isInf then
      "unavailable (floating-point overflow)" else toString mean
    let varianceText := if variance.isNaN || variance.isInf then
      "unavailable (floating-point overflow)" else toString (max 0.0 variance)
    IO.println s!"  empirical mean among returned values: {meanText}; variance: {varianceText}"
  else if !first.isEmpty then IO.println s!"  first value: {first}"
  if rejected > 0 then IO.println s!"  rejected observations: {rejected}"
  if failed > 0 then IO.println s!"  first failure: {error}"

private structure SampleSites where
  discrete : Nat := 0
  continuous : Nat := 0

private def SampleSites.add (left right : SampleSites) : SampleSites :=
  ⟨left.discrete + right.discrete, left.continuous + right.continuous⟩

private def SampleSites.sample (action : Spec.Paper.DistributionAction)
    (discrete : Bool) : SampleSites :=
  match action with
  | .mean => {}
  | .sample _ => if discrete then ⟨1, 0⟩ else ⟨0, 1⟩

private def sampleSites : Core → SampleSites
  | .bvar _ | .reject | .unit | .bool _ | .real _ | .nil => {}
  | .lam body | .fix body | .fst body | .snd body | .inl body | .inr body | .neg body =>
      sampleSites body
  | .app left right | .pair left right | .cons left right | .letE left right
  | .add left right | .mul left right | .div left right | .lt left right =>
      (sampleSites left).add (sampleSites right)
  | .matchSum first second third | .matchList first second third | .ite first second third =>
      ((sampleSites first).add (sampleSites second)).add (sampleSites third)
  | .uniform action left right | .gaussian action left right
  | .beta action left right | .gamma action left right =>
      ((SampleSites.sample action false).add (sampleSites left)).add (sampleSites right)
  | .poisson action body | .discrete action body | .bernoulli action body =>
      (SampleSites.sample action true).add (sampleSites body)
  | .exponential action body =>
      (SampleSites.sample action false).add (sampleSites body)

private def printSampleSites (label : String) (sites : SampleSites) : IO Unit :=
  IO.println s!"Sampling sites {label}: discrete={sites.discrete}, continuous={sites.continuous}"

def main (args : List String) : IO UInt32 := do
  if args == ["--help"] then IO.println usage; return 0
  try
    let o ← IO.ofExcept (options args {})
    let text ← IO.FS.readFile o.file
    let p ← IO.ofExcept (compile text)
    IO.println s!"Checked: {prettyType p.checked.ty}"
    if o.sampleSites then
      printSampleSites "before determinization" (sampleSites p.checked.source)
      printSampleSites "after determinization" (sampleSites p.checked.source.determinize)
    unless o.checkOnly do
      IO.println s!"Annotated source:\n{pretty p.checked.source}"
      IO.println s!"Determinized:\n{pretty p.checked.source.determinize}"
    if let some outputPath := o.exportPrefix then
      if o.additive then
        match Finite.Reward.explore p.checked.source o.subject o.limits with
        | .complete candidate valid =>
            if o.certifyResult then
              let answer ← Finite.Reward.writeResult outputPath p.checked.source o.subject candidate valid o.solveLimits
              IO.println s!"Expected output (kernel-checkable certificate generated) ({reprStr o.subject}): {answer}"
            else
              Finite.Reward.write outputPath p.checked.source o.subject candidate valid
            IO.println s!"Wrote additive {reprStr o.subject} model (kernel-checkable paper correspondence): {outputPath} ({candidate.states.size} states)"
        | .incomplete limit discovered expanded edges =>
            throw (IO.userError s!"Incomplete exploration ({reprStr limit}): {discovered} discovered, {expanded} expanded, {edges} edges. No export written.")
        | .failed state failure =>
            throw (IO.userError s!"Exploration failed at state {state}: {failure.message}. No export written.")
      else
        match Finite.explore p.checked.source o.subject o.limits with
        | .complete candidate valid =>
            if o.certifyResult then
              let answer ← Finite.writeResult outputPath p.checked.source o.subject candidate valid o.solveLimits
              IO.println s!"Expected terminal reward (kernel-checkable certificate generated) ({reprStr o.subject}): {answer}"
            else
              Finite.write outputPath p.checked.source o.subject candidate valid
            IO.println s!"Wrote {reprStr o.subject} model (kernel-checkable paper correspondence): {outputPath} ({candidate.states.size} states)"
        | .incomplete limit discovered expanded edges =>
            throw (IO.userError s!"Incomplete exploration ({reprStr limit}): {discovered} discovered, {expanded} expanded, {edges} edges. No export written.")
        | .failed state failure =>
            throw (IO.userError s!"Exploration failed at state {state}: {failure.message}. No export written.")
    if o.samples > 0 then
      IO.println "Numerical estimates; domain safety and integrability are not established by typing."
      summarize "Source" p.checked.source o
      summarize "Determinized" p.checked.source.determinize o
    return 0
  catch e =>
    (← IO.getStderr).putStrLn s!"{e}"
    return 1
