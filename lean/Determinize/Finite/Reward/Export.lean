import Determinize.Finite.Export
import Determinize.Proof.RewardModel.Soundness
import Determinize.Finite.Reward.Solve
import Determinize.Proof.RewardModel.Moments

namespace Determinize.Finite.Reward
open Spec.FiniteModel

private def rational (q : Rat) : String :=
  if q.den == 1 then toString q.num else s!"{q.num}/{q.den}"
private def leanRat (q : Rat) : String := s!"(({q.num} : Rat) / {q.den})"

private def rowText (row : Row) : String :=
  let edges := row.edges.map fun e => s!"⟨{e.target}, {leanRat e.probability}, {leanRat e.reward}⟩"
  s!"⟨{Finite.kindText row.kind}, [{String.intercalate ", " edges}]⟩"

def candidateText (candidate : Candidate) : String :=
  "import Determinize.Finite.Reward.Graph\n\n" ++
  "open Determinize.Finite Determinize.Spec.Paper Determinize.Spec.FiniteModel\n\n" ++
  "set_option maxRecDepth 100000\nset_option maxHeartbeats 0\nset_option Elab.async false\n\n" ++
  "def candidate : Reward.Candidate where\n" ++
  s!"  initial := {candidate.initial}\n" ++
  "  states := #[\n    " ++ String.intercalate ",\n    " (candidate.states.toList.map Finite.stateText) ++ "\n  ]\n" ++
  "  rows := #[\n    " ++ String.intercalate ",\n    " (candidate.rows.toList.map rowText) ++ "\n  ]\n"

def replayCertificateText (source : Checking.Core) (subject : Subject) (candidate : Candidate) : String :=
  (candidateText candidate).replace "import Determinize.Finite.Reward.Graph"
    "import Determinize.Proof.RewardModel.Soundness" ++
  s!"\ndef checkedSource : Expr Rat := {Frontend.leanExpression source}\n" ++
  s!"def checkedSubject : Subject := {reprStr subject}\n" ++
  "\nabbrev rowClaim (i : Fin candidate.states.size) := candidate.RowValid i\n" ++
  Finite.proofTable "row" "rowClaim" "candidate.states.size" "allRows" candidate.states.size ++
  "\ntheorem machineReplay : candidate.ReplayValid checkedSource checkedSubject :=\n" ++
  "  ⟨by decide +kernel, by decide +kernel, by decide +kernel,\n" ++
  "   by decide +kernel, by decide +kernel, allRows⟩\n" ++
  "\nabbrev model := candidate.toModel machineReplay\n" ++
  "\ntheorem modelMatches : model.Matches (checkedSubject.program checkedSource) :=\n" ++
  "  Determinize.Proof.RewardModel.replay_matches candidate machineReplay\n" ++
  "\n#print axioms machineReplay\n#print axioms modelMatches\n"

private def controlCandidate (candidate : Candidate) : Finite.Candidate :=
  {initial := candidate.initial, states := candidate.states, rows := candidate.rows.map fun row =>
    {kind := row.kind, edges := Id.run do
      let mut edges : Array Finite.Edge := #[]
      for e in row.edges do
        if let some j := edges.findIdx? (fun existing => existing.target == e.target) then
          edges := edges.modify j (fun existing => {existing with probability := existing.probability + e.probability})
        else edges := edges.push ⟨e.target, e.probability⟩
      return edges}}

def write (outputPath : System.FilePath) (source : Checking.Core) (subject : Subject)
    (candidate : Candidate) (_valid : candidate.ReplayValid source subject) : IO Unit := do
  let files ← IO.ofExcept (Finite.render (controlCandidate candidate))
  let mut edgeText := "additive-rewards 1\n"
  for (row, i) in candidate.rows.toList.zipIdx do
    for edge in row.edges do
      edgeText := edgeText ++ s!"{i} {edge.target} {rational edge.probability} {rational edge.reward}\n"
  if let some parent := outputPath.parent then IO.FS.createDirAll parent
  for (suffix, content) in [(".candidate.lean", candidateText candidate),
      (".replay.lean", replayCertificateText source subject candidate), (".tra", files.transitions),
      (".lab", files.labels), (".positive.state.rew", files.positiveRewards),
      (".negative.state.rew", files.negativeRewards), (".additive.edges", edgeText)] do
    IO.FS.writeFile (outputPath.toString ++ suffix) content

def resultCertificateText (source : Checking.Core) (subject : Subject) (candidate : Candidate)
    (model : Spec.RewardModel.Model) (solution : Solution model) : String :=
  let vector (name : String) (values : Fin model.size → Rat) :=
    s!"\ndef {name} : Vector Rat model.size := ⟨#[{String.intercalate ", " ((List.ofFn values).map leanRat)}], by rfl⟩\n"
  let dead := String.intercalate ", " ((List.ofFn solution.boundary.dead).map toString)
  let ranks := String.intercalate ", " ((List.ofFn solution.paths.rank).map toString)
  let next := String.intercalate ", " ((List.ofFn solution.paths.next).map fun j => s!"⟨{j.val}, by decide +kernel⟩")
  (replayCertificateText source subject candidate).replace
    "import Determinize.Proof.RewardModel.Soundness"
    "import Determinize.Proof.RewardModel.Soundness\nimport Determinize.Proof.RewardModel.Moments" ++
  vector "massValues" solution.mass ++ vector "firstValues" solution.first ++
  vector "secondValues" solution.second ++ vector "rejectionValues" solution.rejection ++
  s!"\ndef deadStates : Vector Bool model.size := ⟨#[{dead}], by rfl⟩\n" ++
  s!"def ranks : Vector Nat model.size := ⟨#[{ranks}], by rfl⟩\n" ++
  s!"def nextStates : Vector (Fin model.size) model.size := ⟨#[{next}], by rfl⟩\n" ++
  "\ndef boundary : Determinize.Proof.FiniteModel.Boundary model.control where\n" ++
  "  dead := fun i => deadStates[i]\n  rank := fun i => ranks[i]\n  closed := by decide +kernel\n" ++
  "\ndef solution : Reward.Solution model where\n" ++
  "  boundary := boundary\n  paths := ⟨fun i => ranks[i], fun i => nextStates[i]⟩\n" ++
  "  pathsValid := by decide +kernel\n" ++
  "  mass := fun i => massValues[i]\n  massValid := by decide +kernel\n" ++
  "  first := fun i => firstValues[i]\n  firstValid := by decide +kernel\n" ++
  "  second := fun i => secondValues[i]\n  secondValid := by decide +kernel\n" ++
  "  rejection := fun i => rejectionValues[i]\n  rejectionValid := by decide +kernel\n" ++
  "\nabbrev statistics := solution.statistics\n" ++
  "\ntheorem checkedResult : Determinize.Spec.RewardModel.ResultMatches model\n" ++
  "    (checkedSubject.program checkedSource) statistics :=\n" ++
  "  Determinize.Proof.RewardModel.solution_result model _ modelMatches solution\n" ++
  "\ntheorem integrability : MeasureTheory.Integrable (fun x : ℝ => x)\n" ++
  "    (bigStepMeasure (checkedSubject.program checkedSource)) ∧\n" ++
  "    MeasureTheory.Integrable (fun x : ℝ => x^2) (bigStepMeasure (checkedSubject.program checkedSource)) := by\n" ++
  "  simpa only [modelMatches.2] using Determinize.Proof.RewardModel.outputMeasure_integrable model\n" ++
  "\ntheorem outputStatistics : statistics.Matches (bigStepMeasure (checkedSubject.program checkedSource)) := by\n" ++
  "  simpa only [modelMatches.2] using Determinize.Proof.RewardModel.solution_statistics model solution\n" ++
  "\ntheorem conditionalVariance (positive : 0 < statistics.returnMass) :\n" ++
  "    ProbabilityTheory.variance id ((bigStepMeasure (checkedSubject.program checkedSource) Set.univ)⁻¹ •\n" ++
  "      bigStepMeasure (checkedSubject.program checkedSource)) =\n" ++
  "      ((statistics.secondMoment / statistics.returnMass - (statistics.firstMoment / statistics.returnMass)^2 : Rat) : ℝ) := by\n" ++
  "  simpa only [modelMatches.2] using Determinize.Proof.RewardModel.solution_conditional_variance model solution positive\n" ++
  "\ntheorem terminationProbabilities : (⟨solution.mass model.initial, solution.rejection model.initial,\n" ++
  "    1-solution.mass model.initial-solution.rejection model.initial⟩ : TerminationStatistics).Matches model.control :=\n" ++
  "  Determinize.Proof.RewardModel.solution_termination model solution\n" ++
  "\n#print axioms checkedResult\n#print axioms integrability\n#print axioms outputStatistics\n" ++
  "#print axioms conditionalVariance\n#print axioms terminationProbabilities\n"

def writeResult (outputPath : System.FilePath) (source : Checking.Core) (subject : Subject)
    (candidate : Candidate) (valid : candidate.ReplayValid source subject) (limits : SolveLimits := {}) : IO Rat := do
  let model := candidate.toModel valid
  let solution ← IO.ofExcept (solve model limits)
  let statistics := solution.statistics
  let optional := fun value : Option Rat => match value with
    | none => Lean.Json.null | some q => Lean.toJson (rational q)
  let metadata := Lean.Json.mkObj [
    ("answer", Lean.toJson (rational statistics.firstMoment)),
    ("return_mass", Lean.toJson (rational statistics.returnMass)),
    ("kernel_checked", Lean.toJson false),
    ("certificate_status", Lean.toJson "generated"),
    ("termination_statistics_scope", Lean.toJson "graph"),
    ("rejection_probability", Lean.toJson (rational (solution.rejection model.initial))),
    ("divergence_probability", Lean.toJson (rational (1-statistics.returnMass-solution.rejection model.initial))),
    ("second_moment", Lean.toJson (rational statistics.secondMoment)),
    ("conditional_mean", optional statistics.conditionalMean),
    ("conditional_variance", optional statistics.conditionalVariance),
    ("subject", Lean.toJson (if subject == .source then "source" else "determinized")),
    ("mode", Lean.toJson "additive"), ("states", Lean.toJson model.size),
    ("rank_bound", Lean.toJson ((List.ofFn solution.paths.rank).foldl max 0))]
  write outputPath source subject candidate valid
  IO.FS.writeFile (outputPath.toString ++ ".result.lean") (resultCertificateText source subject candidate model solution)
  IO.FS.writeFile (outputPath.toString ++ ".result.json") (metadata.pretty ++ "\n")
  return statistics.firstMoment

end Determinize.Finite.Reward
