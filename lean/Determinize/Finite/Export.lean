import Determinize.Finite.Statistics
import Determinize.Checking.Statistics
import Determinize.Frontend.Pretty

namespace Determinize.Finite
open Spec.Paper Spec.FiniteModel

private def rational (q : Rat) : String :=
  if q.den == 1 then toString q.num else s!"{q.num}/{q.den}"
private def leanRat (q : Rat) : String := s!"(({q.num} : Rat) / {q.den})"
private def listText {α : Type} (render : α → String) (values : List α) : String :=
  "[" ++ String.intercalate ", " (values.map render) ++ "]"
private def opText : Op → String
  | .discrete n => s!"(.discrete {n})"
  | op => reprStr op
private def siteText (site : DistributionAction × Op) : String :=
  s!"({reprStr site.1}, {opText site.2})"

private partial def valueText : Value → String
  | .unit => ".unit" | .nil => ".nil"
  | .bool b => s!"(.bool {b})"
  | .number q => s!"(.number {leanRat q})"
  | .pair a b => s!"(.pair {valueText a} {valueText b})"
  | .cons a b => s!"(.cons {valueText a} {valueText b})"
  | .inl a => s!"(.inl {valueText a})"
  | .inr a => s!"(.inr {valueText a})"
  | .closure body environment =>
      s!"(.closure {Frontend.leanExpression body} {listText valueText environment})"
  | .recursive body environment =>
      s!"(.recursive {Frontend.leanExpression body} {listText valueText environment})"

private def frameText : Frame → String
  | .unary op => s!"(.unary {reprStr op})"
  | .left op right environment =>
      s!"(.left {reprStr op} {Frontend.leanExpression right} {listText valueText environment})"
  | .right op left => s!"(.right {reprStr op} {valueText left})"
  | .choose yes no environment =>
      s!"(.choose {Frontend.leanExpression yes} {Frontend.leanExpression no} {listText valueText environment})"
  | .letBody body environment =>
      s!"(.letBody {Frontend.leanExpression body} {listText valueText environment})"
  | .matchSum left right environment =>
      s!"(.matchSum {Frontend.leanExpression left} {Frontend.leanExpression right} {listText valueText environment})"
  | .matchList nilCase consCase environment =>
      s!"(.matchList {Frontend.leanExpression nilCase} {Frontend.leanExpression consCase} {listText valueText environment})"
  | .discrete action => s!"(.discrete ({reprStr action}))"
  | .draw site pending environment arguments =>
      s!"(.draw {siteText site} {listText Frontend.leanExpression pending} {listText valueText environment} {listText leanRat arguments})"

private def stateText : State → String
  | .rejected => ".rejected"
  | .eval expression environment stack =>
      s!"(.eval {Frontend.leanExpression expression} {listText valueText environment} {listText frameText stack})"
  | .deliver value stack => s!"(.deliver {valueText value} {listText frameText stack})"
private def kindText : StateKind → String
  | .transient => ".transient" | .rejected => ".rejected"
  | .returned reward => s!"(.returned {leanRat reward})"
private def rowText (row : Row) : String :=
  let edges := row.edges.toList.map fun e => s!"⟨{e.target}, {leanRat e.probability}⟩"
  s!"⟨{kindText row.kind}, #[{String.intercalate ", " edges}]⟩"

def candidateText (candidate : Candidate) : String :=
  "import Determinize.Finite.Graph\n\n" ++
  "open Determinize.Finite Determinize.Spec.Paper Determinize.Spec.FiniteModel\n\n" ++
  "set_option maxRecDepth 100000\nset_option maxHeartbeats 0\n\n" ++
  "def candidate : Candidate where\n" ++
  s!"  initial := {candidate.initial}\n" ++
  "  states := #[\n    " ++ String.intercalate ",\n    " (candidate.states.toList.map stateText) ++ "\n  ]\n" ++
  "  rows := #[\n    " ++ String.intercalate ",\n    " (candidate.rows.toList.map rowText) ++ "\n  ]\n"

/-- Export kernel-checked replay and paper-semantics correspondence. -/
def replayCertificateText (source : Checking.Core) (subject : Subject) (candidate : Candidate) : String :=
  (candidateText candidate).replace "import Determinize.Finite.Graph"
    "import Determinize.Checking.FiniteModel" ++
  s!"\ndef checkedSource : Expr Rat := {Frontend.leanExpression source}\n" ++
  s!"def checkedSubject : Subject := {reprStr subject}\n" ++
  "\ntheorem machineReplay : candidate.ReplayValid checkedSource checkedSubject := by\n" ++
  "  decide +kernel\n\ndef model : Model := candidate.toModel machineReplay\n" ++
  "\ntheorem modelMatches : model.Matches (checkedSubject.program checkedSource) :=\n" ++
  "  Determinize.Proof.FiniteModel.replay_matches candidate machineReplay\n" ++
  "\n#print axioms machineReplay\n#print axioms modelMatches\n"


structure Files where
  candidate : String
  transitions : String
  labels : String
  positiveRewards : String
  negativeRewards : String

/-- Structural validation only; this does not check machine transition correctness. -/
def render (candidate : Candidate) : Except String Files := do
  let size := candidate.states.size
  if size == 0 || candidate.initial ≥ size || candidate.rows.size != size then
    throw "invalid candidate dimensions"
  let mut transitions := "dtmc\n"
  let mut labels := "#DECLARATION\ninit returned rejected done\n#END\n"
  let mut positiveRewards := ""
  let mut negativeRewards := ""
  for i in [:size] do
    let row := candidate.rows[i]?.getD ⟨.rejected, #[]⟩
    let mut mass := (0 : Rat)
    let mut targets : List Nat := []
    for edge in row.edges do
      if edge.target ≥ size || edge.probability ≤ 0 || targets.contains edge.target then
        throw "invalid or duplicate candidate edge"
      targets := edge.target :: targets
      mass := mass + edge.probability
    if mass != 1 then throw "candidate row is not normalized"
    let mut names := if i == candidate.initial then ["init"] else []
    match row.kind with
    | .transient =>
        for edge in row.edges do
          transitions := transitions ++ s!"{i} {edge.target} {rational edge.probability}\n"
    | terminal =>
        unless row.edges == #[⟨i,1⟩] do throw "candidate terminal state is not absorbing"
        transitions := transitions ++ s!"{i} {size} 1\n"
        match terminal with
        | .returned reward =>
            names := names ++ ["returned"]
            if reward > 0 then positiveRewards := positiveRewards ++ s!"{i} {rational reward}\n"
            if reward < 0 then negativeRewards := negativeRewards ++ s!"{i} {rational (-reward)}\n"
        | _ => names := names ++ ["rejected"]
    if !names.isEmpty then labels := labels ++ s!"{i} {String.intercalate " " names}\n"
  transitions := transitions ++ s!"{size} {size} 1\n"
  labels := labels ++ s!"{size} done\n"
  return ⟨candidateText candidate, transitions, labels, positiveRewards, negativeRewards⟩

/-- Call only with complete exploration data. No files are written if rendering fails. -/
def write (outputPath : System.FilePath) (source : Checking.Core) (subject : Subject)
    (candidate : Candidate) (_valid : candidate.ReplayValid source subject) : IO Unit := do
  let files ← IO.ofExcept (render candidate)
  if let some parent := outputPath.parent then IO.FS.createDirAll parent
  for (suffix, content) in [(".candidate.lean", files.candidate),
      (".replay.lean", replayCertificateText source subject candidate), (".tra", files.transitions),
      (".lab", files.labels), (".positive.state.rew", files.positiveRewards),
      (".negative.state.rew", files.negativeRewards)] do
    IO.FS.writeFile (outputPath.toString ++ suffix) content


/-- A standalone theorem about the selected program's mass and output moments. -/
def resultCertificateText (source : Checking.Core) (subject : Subject) (candidate : Candidate)
    (model : Model) (certificate : Proof.FiniteModel.MomentCertificate model) : String :=
  let vector := fun moment => "#[" ++ String.intercalate ", " ((List.ofFn (certificate.values moment)).map leanRat) ++ "]"
  let ranks := "#[" ++ String.intercalate ", " ((List.ofFn certificate.rank).map toString) ++ "]"
  let next := "#[" ++ String.intercalate ", " ((List.ofFn certificate.next).map fun j => s!"⟨{j.val}, by decide +kernel⟩") ++ "]"
  let dead := "#[" ++ String.intercalate ", " ((List.ofFn certificate.dead).map toString) ++ "]"
  (replayCertificateText source subject candidate).replace
    "import Determinize.Checking.FiniteModel" "import Determinize.Checking.Statistics" ++
  "\nopen Determinize.Proof.FiniteModel\n" ++
  s!"\ndef nextStates : Vector (Fin model.size) model.size := ⟨{next}, by rfl⟩\n" ++
  "\ndef result : MomentCertificate model where\n" ++
  s!"  dead := fun i => {dead}[i.val]!\n" ++
  s!"  rank := fun i => {ranks}[i.val]!\n" ++
  "  next := fun i => nextStates[i]\n" ++
  "  values := fun moment i => (match moment with\n" ++
  s!"    | .mass => {vector .mass}\n    | .first => {vector .first}\n    | .second => {vector .second})[i.val]!\n" ++
  "\ntheorem resultAccepted : Determinize.Checking.checkStatistics model result = true := by\n" ++
  "  decide +kernel\n" ++
  "\ndef statistics := result.statistics model\n" ++
  "\ntheorem outputStatistics : statistics.Matches (bigStepMeasure (checkedSubject.program checkedSource)) :=\n" ++
  "  Determinize.Checking.checked_statistics ⟨model, modelMatches⟩ result resultAccepted\n" ++
  "\ntheorem expectedReward :\n" ++
  "    MeasureTheory.Integrable id (bigStepMeasure (checkedSubject.program checkedSource)) ∧\n" ++
  "    (∫ value : ℝ, value ∂bigStepMeasure (checkedSubject.program checkedSource)) =\n" ++
  "      (statistics.firstMoment : ℝ) := by\n" ++
  "  exact ⟨modelMatches.2 ▸ Determinize.Proof.FiniteModel.outputMeasure_integrable model, outputStatistics.2.1⟩\n" ++
  "\ntheorem conditionalVariance (positive : 0 < statistics.returnMass) :\n" ++
  "    ProbabilityTheory.variance id ((bigStepMeasure (checkedSubject.program checkedSource) Set.univ)⁻¹ •\n" ++
  "      bigStepMeasure (checkedSubject.program checkedSource)) =\n" ++
  "      ((statistics.secondMoment / statistics.returnMass - (statistics.firstMoment / statistics.returnMass)^2 : Rat) : ℝ) :=\n" ++
  "  Determinize.Checking.checked_conditionalVariance ⟨model, modelMatches⟩ result resultAccepted positive\n" ++
  "\n#print axioms resultAccepted\n#print axioms expectedReward\n#print axioms outputStatistics\n#print axioms conditionalVariance\n"

def writeResult (outputPath : System.FilePath) (source : Checking.Core) (subject : Subject)
    (candidate : Candidate) (valid : candidate.ReplayValid source subject)
    (limits : SolveLimits := {}) : IO Rat := do
  let model := candidate.toModel valid
  let certified ← IO.ofExcept (solveStatistics model limits)
  let certificate := certified.val
  let statistics := certificate.statistics model
  let rankBound := (List.ofFn certificate.rank).foldl max 0
  let optional := fun value : Option Rat => match value with
    | none => Lean.Json.null | some q => Lean.toJson (rational q)
  let metadata := Lean.Json.mkObj [
    ("answer", Lean.toJson (rational statistics.firstMoment)),
    ("return_mass", Lean.toJson (rational statistics.returnMass)),
    ("second_moment", Lean.toJson (rational statistics.secondMoment)),
    ("conditional_mean", optional statistics.conditionalMean),
    ("conditional_variance", optional statistics.conditionalVariance),
    ("subject", Lean.toJson (if subject == .source then "source" else "determinized")),
    ("states", Lean.toJson model.size),
    ("rank_bound", Lean.toJson rankBound)]
  write outputPath source subject candidate valid
  IO.FS.writeFile (outputPath.toString ++ ".result.lean")
    (resultCertificateText source subject candidate model certificate)
  IO.FS.writeFile (outputPath.toString ++ ".result.json") (metadata.pretty ++ "\n")
  return statistics.firstMoment

end Determinize.Finite
