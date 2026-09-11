import Determinize.Checking.Result

namespace Determinize.Finite
open Statement.FiniteModel

/-- Resource limit for dense exact Gaussian elimination. -/
structure SolveLimits where
  maxStates : Nat := 256

private def eliminate (rows : Array (Array Rat)) : Except String (Array Rat) := do
  let n := rows.size
  let mut rows := rows
  for col in [:n] do
    let mut pivot := none
    for row in [col:n] do
      if pivot.isNone && (rows[row]!)[col]! != 0 then pivot := some row
    let some pivotRow := pivot | throw "singular value equations; no absorption certificate"
    let old := rows[col]!
    rows := (rows.set! col rows[pivotRow]!).set! pivotRow old
    let divisor := (rows[col]!)[col]!
    let normalized := rows[col]!.map (· / divisor)
    rows := rows.set! col normalized
    for row in [:n] do
      if row != col then
        let factor := (rows[row]!)[col]!
        if factor != 0 then
          rows := rows.set! row (Array.zipWith (fun x y => x - factor*y) rows[row]! normalized)
  return rows.map (fun row => row[n]!)

/-- Candidate generation is unverified; acceptance always calls the proved checker. -/
def solve (model : Model) (limits : SolveLimits := {}) : Except String (ResultCertificate model) := do
  if model.size > limits.maxStates then
    throw s!"exact solver state limit exceeded ({model.size} > {limits.maxStates})"
  let states := List.ofFn (fun state : Fin model.size => state)
  let rows := states.toArray.map fun state =>
    let coefficients := states.toArray.map fun next =>
      let identity : Rat := if state = next then 1 else 0
      match model.kind state with
      | .transient => identity - model.transition state next
      | _ => identity
    coefficients.push (match model.kind state with | .returned reward => reward | _ => 0)
  let values ← eliminate rows
  let mut survival := Vector.ofFn fun state : Fin model.size =>
    if model.kind state = .transient then (1 : Rat) else 0
  for horizon in [1:model.size + 1] do
    survival := Vector.ofFn fun state => if model.kind state = .transient then
      (states.map fun next => model.transition state next * survival[next]).sum else 0
    let maximum := survival.toArray.foldl max 0
    if maximum < 1 then
      let certificate : ResultCertificate model :=
        ⟨fun state => values[state.val]!, horizon, 1 - maximum⟩
      if Checking.checkResult model certificate then return certificate
      throw "generated result certificate failed validation"
  throw "no uniform absorption bound: some state cannot reach a terminal state"

end Determinize.Finite
