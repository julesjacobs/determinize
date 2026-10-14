import Determinize.Finite.Shared
import Determinize.Finite.Reward.Explore
import Determinize.Finite.Reward.Export
import Determinize.Proof.FiniteModel.Evidence
import Determinize.Proof.RewardModel.Table
import Determinize.Proof.FiniteModel.Lookup
import Lean.Elab.Tactic.Decide
import Lean.Meta.Native
import Determinize.Proof.RewardModel.Native

register_option determinize.storm.native : Bool := {
  defValue := true
  descr := "check the complete Storm certificate using native_decide" }

register_option determinize.storm.maxStates : Nat := {
  defValue := 100000
  descr := "maximum number of states explored by the storm tactic" }
register_option determinize.storm.timeout : Nat := {
  defValue := 120
  descr := "Storm subprocess timeout in seconds" }
register_option determinize.storm.worker : String := {
  defValue := ""
  descr := "path to determinize's tools/storm.py (otherwise found above the current directory)" }

initialize Lean.registerTraceClass `Determinize.storm

namespace Determinize.Tactic.Storm
open Lean Meta Elab
open Lean.Elab.Tactic
open Spec.Paper (Core)
open Spec.FiniteModel (Subject)

meta section

private def define (value : Lean.Expr) (compile : Bool := false) : MetaM Lean.Expr := do
  let value ← mkAuxDefinitionFor (← mkAuxDeclName `stormData) value (compile := compile)
  setReducibleAttribute value.constName!
  return value

private def decideTerm (type : Lean.Expr) : MetaM Lean.Expr := do
  let decider ← synthInstance (mkApp (mkConst ``Decidable) type)
  let trueTerm := mkConst ``Bool.true
  let refl := mkApp2 (mkConst ``Eq.refl [1]) (mkConst ``Bool) trueTerm
  return mkApp3 (mkConst ``of_decide_eq_true) type decider refl

private partial def proofTerm (type : Lean.Expr) : MetaM Lean.Expr := do
  let type := type.headBeta
  let reduced ← whnf type
  try
    if reduced.isAppOfArity ``And 2 then
      let args := reduced.getAppArgs
      return mkApp4 (mkConst ``And.intro) args[0]! args[1]!
        (← proofTerm args[0]!) (← proofTerm args[1]!)
    if reduced.isAppOfArity ``Eq 3 then
      let args := reduced.getAppArgs
      return mkApp2 (mkConst ``Eq.refl [(← getLevel args[0]!)]) args[0]! args[2]!
    decideTerm type
  catch _ => decideTerm type

private def prove (type : Lean.Expr) : MetaM Lean.Expr := do
  return mkConst (← mkAuxLemma [] type (← proofTerm type) (cache := false))

private def declareShared (s : Finite.Shared.Builder) (compile : Bool := false) : MetaM Unit := do
  for d in s.definitions do
    discard <| mkAuxDefinition d.name d.type d.value (compile := compile)

private partial def lookupTree (type : Lean.Expr) (entries : Array Lean.Expr)
    (start stop : Nat) (offset : Nat := 0) : MetaM Lean.Expr := do
  if stop - start ≤ 1 then
    mkAppM ``Proof.FiniteModel.Lookup.value #[entries[start]!]
  else
    let middle := (start + stop) / 2
    mkAppM ``Proof.FiniteModel.Lookup.branch #[toExpr (offset + middle),
      ← lookupTree type entries start middle offset,
      ← lookupTree type entries middle stop offset]

private def lookup (type : Lean.Expr) (entries : Array Lean.Expr) : MetaM Lean.Expr := do
  unless entries.size > 0 do throwError "storm: empty certificate table"
  let tree ← define (← lookupTree type entries 0 entries.size)
  define (← mkAppM ``Proof.FiniteModel.Lookup.get #[tree])

private def finiteLookup (n : Nat) (type : Lean.Expr)
    (entries : Array Lean.Expr) : MetaM Lean.Expr := do
  let getter ← lookup type entries
  withLocalDeclD `i (mkApp (mkConst ``Fin) (toExpr n)) fun i ↦ do
    mkLambdaFVars #[i] (mkApp getter (← mkAppM ``Fin.val #[i]))

private def allStates (n : Nat) (predicate : Lean.Expr)
    (certify : Fin n → MetaM Lean.Expr) : MetaM Lean.Expr := do
  let subtype ← mkAppM ``Subtype #[predicate]
  let entries ← (Array.ofFn id : Array (Fin n)).mapM fun i ↦ do
    if n ≥ 1000 && i.val % 1000 == 0 then
      if ← isTracingEnabledFor `Determinize.storm then
        IO.eprintln s!"storm: checking state {i.val}/{n}"
    let claim := mkApp predicate (toExpr i)
    let proof := mkConst (← mkAuxLemma [] claim (← certify i) (cache := false))
    return mkApp4 (mkConst ``Subtype.mk [1])
      (mkApp (mkConst ``Fin) (toExpr n)) predicate (toExpr i) proof
  let tree ← define (← lookupTree subtype entries 0 n)
  let label ← withLocalDeclD `entry subtype fun entry ↦ do
    let index ← mkAppM ``Subtype.val #[entry]
    mkLambdaFVars #[entry] (← mkAppM ``Fin.val #[index])
  let indexed ← prove (← mkAppM ``Proof.FiniteModel.Lookup.Indexed
    #[tree, label, toExpr (0 : Nat), toExpr n])
  mkAppM ``Proof.FiniteModel.lookup_evidence_all #[predicate, tree, indexed]

private partial def findProgram? (e : Lean.Expr) : Option Lean.Expr :=
  if e.isAppOf ``Spec.Paper.bigStepMeasure || e.isAppOf ``Spec.returnedExpectation ||
      e.isAppOf ``Spec.returnedVariance then
    e.getAppArgs.back?
  else
    e.getAppArgs.findSome? findProgram?

private def sourceOf (program : Lean.Expr) : MetaM (Lean.Expr × Lean.Expr) := do
  if program.isAppOfArity ``Subject.program 2 then
    let args := program.getAppArgs
    return (args[1]!, args[0]!)
  if program.isAppOfArity ``Spec.Paper.interpret 1 then
    return (program.appArg!, toExpr Subject.source)
  if program.isAppOf ``Spec.Paper.Expr.determinize then
    let input := program.appArg!
    if input.isAppOfArity ``Spec.Paper.interpret 1 then
      return (input.appArg!, toExpr Subject.determinized)
  throwError "storm: use Subject.program or interpret to identify the closed rational program"

private def workerPath : IO System.FilePath := do
  let mut directory ← IO.currentDir
  repeat
    let path := directory / "tools" / "storm.py"
    if ← path.pathExists then return path
    let some parent := directory.parent | break
    if parent == directory then break
    directory := parent
  throw (IO.userError "storm: cannot find tools/storm.py; set determinize.storm.worker")

private def parseRat (s : String) : Except String Rat := do
  match s.splitOn "/" with
  | [num] =>
    let some n := num.toInt? | throw s!"invalid Storm rational: {s}"
    return n
  | [num, den] =>
    let some n := num.toInt? | throw s!"invalid Storm rational: {s}"
    let some d := den.toNat? | throw s!"invalid Storm rational: {s}"
    if d == 0 then throw "zero denominator in Storm rational"
    return (n : Rat) / d
  | _ => throw s!"invalid Storm rational: {s}"

private def field (result : Json) (name : String) (n : Nat) : Except String (Array Json) := do
  let values ← (← result.getObjVal? name).getArr?
  unless values.size == n do throw s!"invalid Storm vector length: {name}"
  return values

private def certificate (result : Json) (n : Nat) : MetaM Lean.Expr := do
  let get {α : Type} (value : Except String α) : MetaM α :=
    match value with | .ok x => pure x | .error e => throwError "storm: {e}"
  let dead ← get (field result "dead" n >>= fun xs ↦ xs.mapM Json.getBool?)
  let rank ← get (field result "rank" n >>= fun xs ↦ xs.mapM Json.getNat?)
  let next ← get (field result "next" n >>= fun xs ↦ xs.mapM Json.getNat?)
  let next ← next.mapM fun i ↦ do
    if h : i < n then return toExpr (⟨i, h⟩ : Fin n)
    else throwError "storm: successor index {i} is outside the model"
  let values ← get (result.getObjVal? "values")
  let rats ← #["mass", "first", "second", "rejection"].mapM fun name ↦ do
    let xs ← get (field values name n >>= fun xs ↦ xs.mapM fun x ↦ x.getStr? >>= parseRat)
    finiteLookup n (toTypeExpr Rat) (xs.map toExpr)
  mkAppM ``Proof.RewardModel.Certificate.mk #[
    ← finiteLookup n (toTypeExpr Bool) (dead.map toExpr),
    ← finiteLookup n (toTypeExpr Nat) (rank.map toExpr),
    ← finiteLookup n (mkApp (mkConst ``Fin) (toExpr n)) next,
    rats[0]!, rats[1]!, rats[2]!, rats[3]!]

private def solveNative (source subject : Lean.Expr) (candidate : Finite.Reward.Candidate)
    (result : Json) : TacticM Lean.Expr := do
  let vectors ← define (← mkAppM ``Proof.RewardModel.Vectors.decode #[toExpr result.compress]) true
  let values := result.getObjValD "values"
  let value (name : String) : TacticM Rat := do
    let parsed := do
      parseRat (← (← (← values.getObjVal? name).getArrVal? candidate.initial).getStr?)
    match parsed with
    | .ok q => pure q | .error e => throwError "storm: {e}"
  let answer ← mkAppM ``Spec.FiniteModel.OutputStatistics.mk #[
    toExpr (← value "mass"), toExpr (← value "first"), toExpr (← value "second")]
  let limits ← mkAppM ``Finite.Limits.mk #[
    toExpr (determinize.storm.maxStates.get (← getOptions)), toExpr (100000 : Nat),
    toExpr (1000000 : Nat)]
  let initial := toExpr candidate.initial
  let check ← mkAppM ``Proof.RewardModel.Vectors.verify #[vectors, source, subject, limits, initial, answer]
  trace[Determinize.storm] "Checking graph and sparse equations with native_decide"
  let .success verified ← nativeEqTrue `native_decide check |
    throwError "storm: native certificate check failed"
  mkAppM ``Proof.RewardModel.Vectors.verify_sound #[vectors, source, subject, limits, initial, answer, verified]

private unsafe def solve (source subject : Lean.Expr) :
    TacticM (Lean.Expr × Option (Rat × Rat)) := do
  if source.hasFVar || source.hasMVar then
    throwError "storm: the rational program must be closed"
  let core ← evalExpr Core (toTypeExpr Core) source
  let selected ← evalExpr Subject (toTypeExpr Subject) subject
  if ← isTracingEnabledFor `Determinize.storm then
    IO.eprintln "storm: exploring graph"
  let limits : Finite.Limits := {maxStates := determinize.storm.maxStates.get (← getOptions)}
  let candidate ← match Finite.Reward.explore core selected limits with
    | .complete c _ => pure c
    | .incomplete limit discovered expanded edges =>
      throwError "storm: exploration limit {repr limit}: {discovered} states, \
        {expanded} expanded, {edges} edges"
    | .failed i error => throwError "storm: state {i}: {error.message}"
  if ← isTracingEnabledFor `Determinize.storm then
    IO.eprintln s!"storm: solving {candidate.states.size} states"
  let result ← IO.FS.withTempDir fun directory ↦ do
    let path := directory / "model"
    Finite.Reward.writeStorm path candidate
    let workerOption := determinize.storm.worker.get (← getOptions)
    let worker ← if workerOption.isEmpty then workerPath else pure ⟨workerOption⟩
    let python := (← IO.getEnv "STORM_PYTHON").getD "python3"
    let output ← IO.Process.output {cmd := python, args := #[worker.toString,
      "--tactic-worker", path.toString, toString (determinize.storm.timeout.get (← getOptions))]}
    unless output.exitCode == 0 do
      throwError "storm: worker failed:\n{output.stderr}\n{output.stdout}"
    match Json.parse (← IO.FS.readFile (path.toString ++ ".storm-values.json")) with
    | .ok json => pure json | .error e => throwError "storm: {e}"
  trace[Determinize.storm] "Storm returned vectors for {candidate.states.size} states"
  if determinize.storm.native.get (← getOptions) then
    let proof ← solveNative source subject candidate result
    let values := result.getObjValD "values"
    let getValue (name : String) : TacticM Rat := do
      let value := do
        parseRat (← (← (← values.getObjVal? name).getArrVal? candidate.initial).getStr?)
      match value with
      | .ok q => pure q | .error e => throwError "storm: {e}"
    let mass ← getValue "mass"
    let first ← getValue "first"
    let second ← getValue "second"
    return (proof, if mass == 0 then none else some
      (first / mass, second / mass - (first / mass)^2))
  let namespacePrefix ← mkAuxDeclName `stormShared
  if ← isTracingEnabledFor `Determinize.storm then
    IO.eprintln "storm: quoting shared graph"
  let n := candidate.states.size
  let ((states, rowsData, actions), shared) := (do
    let states ← candidate.states.mapM Finite.Shared.quote
    let rows ← candidate.rows.mapM Finite.Shared.quote
    let actions ← candidate.states.mapM fun state ↦
      Finite.Shared.quote (Finite.Reward.step state)
    pure (states, rows, actions)).run {namespacePrefix}
  trace[Determinize.storm] "Declaring {shared.definitions.size} shared definitions"
  if ← isTracingEnabledFor `Determinize.storm then
    IO.eprintln s!"storm: declaring {shared.definitions.size} shared definitions"
  declareShared shared
  let state ← lookup (toTypeExpr Finite.State) states
  let row ← lookup (toTypeExpr Finite.Reward.Row) rowsData
  let initialIndex ← if h : candidate.initial < n then
    pure (⟨candidate.initial, h⟩ : Fin n)
    else throwError "storm: invalid initial state"
  let data ← define (← mkAppM ``Finite.Reward.Table.mk
    #[toExpr n, toExpr initialIndex, state, row])
  let initial ← prove (← mkEq (mkApp state (toExpr candidate.initial))
    (← mkAppM ``Finite.initialState #[source, subject]))
  let sourceScoped ← prove (← mkAppM ``Proof.FiniteModel.Binding.Scoped #[toExpr (0 : Nat), source])
  trace[Determinize.storm] "Checking graph replay"
  let fin := mkApp (mkConst ``Fin) (toExpr n)
  let rows ← withLocalDeclD `i fin fun i ↦ do
    mkLambdaFVars #[i] (← mkAppM ``Finite.Reward.Table.StateValid #[data, i])
  let replayRows ← allStates n rows fun i ↦ do
    let index := toExpr i
    let state := mkApp state (toExpr i.val)
    let action := actions[i.val]!
    let actionType := mkApp2 (mkConst ``Except [0, 0])
      (toTypeExpr Finite.Failure) (toTypeExpr Finite.Reward.Step)
    let stepProof := mkApp2 (mkConst ``Eq.refl [1]) actionType action
    let same ← mkAppM ``Prod.mk #[toExpr (0 : Rat), state]
    let normalizedProof ← mkEqRefl same
    let rowProof ← proofTerm (← mkAppM ``Finite.Reward.Table.RowClaim #[data, index, action])
    let proof := mkAppN (mkConst ``Finite.Reward.Table.stateValid_of_step)
      #[data, index, action, stepProof, normalizedProof, rowProof]
    return proof
  let model ← define (← mkAppM ``Finite.Reward.Table.model #[data, replayRows])
  let matching ← mkAppM ``Finite.Reward.Table.matches
    #[data, source, subject, initial, sourceScoped, replayRows]
  trace[Determinize.storm] "Checking sparse moment equations"
  let c ← define (← certificate result n)
  let claims ← withLocalDeclD `i fin fun i ↦ do
    mkLambdaFVars #[i] (← mkAppM ``Proof.RewardModel.Certificate.StateValid #[model, c, i])
  let solution ← define (← mkAppM ``Proof.RewardModel.Certificate.solution
    #[model, c, ← allStates n claims (fun i ↦ proofTerm (mkApp claims (toExpr i)))])
  trace[Determinize.storm] "Graph and moment equations checked"
  let proof ← mkAppM ``Proof.RewardModel.solution_outputStatistics #[model,
    ← mkAppM ``Subject.program #[subject, source], matching, solution]
  let values := result.getObjValD "values"
  let getValue (name : String) : TacticM Rat := do
    let value := do
      let field ← values.getObjVal? name
      let value ← field.getArrVal? candidate.initial
      parseRat (← value.getStr?)
    match value with
    | .ok q => pure q | .error e => throwError "storm: {e}"
  let mass ← getValue "mass"
  let first ← getValue "first"
  let second ← getValue "second"
  return (proof, if mass == 0 then none else some
    (first / mass, second / mass - (first / mass)^2))

syntax (name := storm) "storm" : tactic

@[tactic storm] private unsafe def evalStorm : Tactic := fun _ ↦
  withOptions (fun o ↦ Elab.async.set (maxRecDepth.set o 100000) false) <| withMainContext do
  let goal ← getMainGoal
  let target ← goal.getType
  let some program := findProgram? target |
    throwError "storm: expected output statistics, returned expectation, or returned variance"
  let (source, subject) ← sourceOf program
  let (result, conditional) ← solve source subject
  let output := result
  if ← isDefEq (← inferType output) target then
    goal.assign output
  else
    let statistics := (← inferType output).getAppArgs[0]!
    let some (meanValue, varianceValue) := conditional |
      throwError "storm: conditional moments are undefined when return mass is zero"
    let meanTerm ← mkAppM ``Spec.FiniteModel.OutputStatistics.conditionalMean #[statistics]
    let varianceTerm ← mkAppM ``Spec.FiniteModel.OutputStatistics.conditionalVariance #[statistics]
    let meanEq ← prove (← mkEq meanTerm (toExpr (some meanValue)))
    let varianceEq ← prove (← mkEq varianceTerm (toExpr (some varianceValue)))
    let moments ← mkAppM ``Proof.FiniteModel.statistics_returned_moments
      #[← mkAppM ``Subject.program #[subject, source], statistics, output, meanEq, varianceEq]
    let mean ← mkAppM ``And.left #[moments]
    let variance ← mkAppM ``And.right #[moments]
    if ← isDefEq (← inferType mean) target then goal.assign mean
    else if ← isDefEq (← inferType variance) target then goal.assign variance
    else if ← isDefEq (← inferType moments) target then goal.assign moments
    else throwError "storm: the certified result does not match the goal"
  replaceMainGoal []

end
end Determinize.Tactic.Storm
