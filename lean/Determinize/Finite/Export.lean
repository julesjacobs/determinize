import Determinize.Finite.Solve
import Determinize.Frontend.Pretty

namespace Determinize.Finite
open Statement.Paper Statement.FiniteModel

private def rational (q : Rat) : String :=
  if q.den == 1 then toString q.num else s!"{q.num}/{q.den}"
private def leanRat (q : Rat) : String := s!"(({q.num} : Rat) / {q.den})"
private def listText {α : Type} (render : α → String) (values : List α) : String :=
  "[" ++ String.intercalate ", " (values.map render) ++ "]"
private def distributionText (d : FiniteDistribution) : String :=
  s!"⟨{listText leanRat d.probabilities}, by decide +kernel, by decide +kernel⟩"
private def opText : Op → String
  | .discrete d => s!"(.discrete {distributionText d})"
  | op => reprStr op
private def siteText (site : Mode × Kind × Op) : String :=
  s!"({reprStr site.1}, {reprStr site.2.1}, {opText site.2.2})"

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
  "import Determinize.Finite.Explore\n\n" ++
  "open Determinize.Finite Determinize.Statement.Paper Determinize.Statement.FiniteModel\n\n" ++
  "set_option maxRecDepth 100000\nset_option maxHeartbeats 0\n\n" ++
  "def candidate : Candidate where\n" ++
  s!"  initial := {candidate.initial}\n" ++
  "  states := #[\n    " ++ String.intercalate ",\n    " (candidate.states.toList.map stateText) ++ "\n  ]\n" ++
  "  rows := #[\n    " ++ String.intercalate ",\n    " (candidate.rows.toList.map rowText) ++ "\n  ]\n"

/-- Export kernel-checked replay and paper-semantics correspondence. -/
def replayCertificateText (source : Checking.Core) (subject : Subject) (candidate : Candidate) : String :=
  (candidateText candidate).replace "import Determinize.Finite.Explore"
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
    (candidate : Candidate) : IO Unit := do
  let some _ := Checking.checkModel source subject candidate
    | throw (IO.userError "model candidate failed validation")
  let files ← IO.ofExcept (render candidate)
  if let some parent := outputPath.parent then IO.FS.createDirAll parent
  for (suffix, content) in [(".candidate.lean", files.candidate),
      (".replay.lean", replayCertificateText source subject candidate), (".tra", files.transitions),
      (".lab", files.labels), (".positive.state.rew", files.positiveRewards),
      (".negative.state.rew", files.negativeRewards)] do
    IO.FS.writeFile (outputPath.toString ++ suffix) content


/-- A standalone theorem about the selected paper program's expected output. -/
def resultCertificateText (source : Checking.Core) (subject : Subject) (candidate : Candidate)
    (model : Model) (certificate : ResultCertificate model) : String :=
  let values := (List.ofFn certificate.values).map leanRat
  (replayCertificateText source subject candidate).replace
    "import Determinize.Checking.FiniteModel" "import Determinize.Checking.Result" ++
  "\ndef result : ResultCertificate model where\n" ++
  "  values := fun i => #[" ++ String.intercalate ", " values ++ "][i.val]!\n" ++
  s!"  horizon := {certificate.horizon}\n  escape := {leanRat certificate.escape}\n" ++
  "\ntheorem resultAccepted : Determinize.Checking.checkResult model result = true := by\n" ++
  "  decide +kernel\n" ++
  "\ntheorem expectedReward :\n" ++
  "    MeasureTheory.Integrable id (bigStepMeasure (checkedSubject.program checkedSource)) ∧\n" ++
  "    (∫ value : ℝ, value ∂bigStepMeasure (checkedSubject.program checkedSource)) =\n" ++
  "      (result.values model.initial : ℝ) := by\n" ++
  "  rw [← modelMatches.2]\n" ++
  "  exact (Determinize.Checking.checkResult_sound model result resultAccepted).2\n" ++
  "\n#print axioms resultAccepted\n#print axioms expectedReward\n"

def writeResult (outputPath : System.FilePath) (source : Checking.Core) (subject : Subject)
    (candidate : Candidate) (limits : SolveLimits := {}) : IO Rat := do
  let some checked := Checking.checkModel source subject candidate
    | throw (IO.userError "model candidate failed validation")
  let certificate ← IO.ofExcept (solve checked.model limits)
  let answer := certificate.values checked.model.initial
  let metadata := Lean.Json.mkObj [
    ("answer", Lean.toJson (rational answer)),
    ("subject", Lean.toJson (if subject == .source then "source" else "determinized")),
    ("states", Lean.toJson checked.model.size),
    ("horizon", Lean.toJson certificate.horizon),
    ("escape", Lean.toJson (rational certificate.escape))]
  write outputPath source subject candidate
  IO.FS.writeFile (outputPath.toString ++ ".result.lean")
    (resultCertificateText source subject candidate checked.model certificate)
  IO.FS.writeFile (outputPath.toString ++ ".result.json") (metadata.pretty ++ "\n")
  return answer

end Determinize.Finite
