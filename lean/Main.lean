import Determinize.Frontend.Certificate
import Determinize.Runtime.Eval
import Determinize.Finite.Export

open Determinize Determinize.Frontend Determinize.Checking

private structure Options where
  file : String := ""
  seed : UInt64 := 0
  samples : Nat := 0
  fuel : Nat := 100000
  certificate : Option String := none
  checkOnly : Bool := false
  exportPrefix : Option String := none
  subject : Statement.FiniteModel.Subject := .determinized
  limits : Finite.Limits := {}

private def usage := "Usage: determinize [--check] [--samples N] [--seed N] [--fuel N] [--certificate FILE.lean] [--export PREFIX] [--subject source|determinized] [--max-states N] [--max-edges N] [--max-state-bytes N] FILE.det"
private def natural (s : String) : Except String Nat :=
  match s.toNat? with
  | some n => .ok n
  | none => .error s!"expected a nonnegative integer, got '{s}'"
private def options : List String → Options → Except String Options
  | [], o => if o.file.isEmpty then .error usage else .ok o
  | "--export" :: outputPath :: rest, o => options rest {o with exportPrefix := some outputPath}
  | "--subject" :: subject :: rest, o => do
      let subject ← match subject with
        | "source" => pure Statement.FiniteModel.Subject.source
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
  | "--certificate" :: file :: rest, o => options rest {o with certificate := some file}
  | arg :: rest, o =>
    if arg.startsWith "-" then .error s!"unknown or incomplete option '{arg}'\n{usage}"
    else if !o.file.isEmpty then .error "expected one input file"
    else options rest {o with file := arg}

private def summarize (label : String) (e : Core) (o : Options) : IO Unit := do
  let mut count := 0
  let mut sum := 0.0
  let mut squares := 0.0
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
      | .number x => count := count + 1; sum := sum + x; squares := squares + x*x
      | _ => pure ()
  IO.println s!"{label}: {o.samples - failed - rejected}/{o.samples} runs returned a value"
  if count > 0 then
    let mean := sum / Float.ofNat count
    let variance := max 0.0 (squares / Float.ofNat count - mean*mean)
    IO.println s!"  empirical mean among returned values: {mean}; variance: {variance}"
  else if !first.isEmpty then IO.println s!"  first value: {first}"
  if rejected > 0 then IO.println s!"  rejected observations: {rejected}"
  if failed > 0 then IO.println s!"  first failure: {error}"

def main (args : List String) : IO UInt32 := do
  if args == ["--help"] then IO.println usage; return 0
  try
    let o ← IO.ofExcept (options args {})
    let text ← IO.FS.readFile o.file
    let p ← IO.ofExcept (compile text)
    IO.println s!"Checked: {prettyType p.checked.ty}"
    unless o.checkOnly do
      IO.println s!"Annotated source:\n{pretty p.checked.source}"
      IO.println s!"Determinized:\n{pretty p.checked.source.determinize}"
    if let some path := o.certificate then
      IO.FS.writeFile path (← IO.ofExcept (certificateText text))
      IO.println s!"Wrote kernel-checkable certificate: {path}"
    if let some outputPath := o.exportPrefix then
      match Finite.explore p.checked.source o.subject o.limits with
      | .complete candidate =>
          Finite.write outputPath p.checked.source o.subject candidate
          IO.println s!"Wrote {reprStr o.subject} model (paper correspondence checked): {outputPath} ({candidate.states.size} states)"
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
