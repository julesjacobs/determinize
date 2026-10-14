import Determinize.Finite.Reward.Explore
import Determinize.Proof.RewardModel.Sparse
import Determinize.Proof.RewardModel.Soundness
import Lean.Data.Json.Parser

namespace Determinize.Proof.RewardModel
open Spec.FiniteModel
open Spec.Paper (Core)

structure Vectors where
  dead : Array Bool
  rank : Array Nat
  next : Array Nat
  mass : Array Rat
  first : Array Rat
  second : Array Rat
  rejection : Array Rat
deriving Inhabited

private def rational (s : String) : Except String Rat := do
  match s.splitOn "/" with
  | [num] =>
    let some n := num.toInt? | throw "invalid numerator"
    return n
  | [num, den] =>
    let some n := num.toInt? | throw "invalid numerator"
    let some d := den.toNat? | throw "invalid denominator"
    if d == 0 then throw "zero denominator"
    return (n : Rat) / d
  | _ => throw "invalid rational"

def Vectors.decode (text : String) : Vectors :=
  let parsed : Except String Vectors := do
    let json ← Lean.Json.parse text
    let field (object : Lean.Json) (name : String) :=
      object.getObjVal? name >>= Lean.Json.getArr?
    let dead ← (← field json "dead").mapM Lean.Json.getBool?
    let rank ← (← field json "rank").mapM Lean.Json.getNat?
    let next ← (← field json "next").mapM Lean.Json.getNat?
    let values ← json.getObjVal? "values"
    let rats ← #["mass", "first", "second", "rejection"].mapM fun name ↦ do
      (← field values name).mapM fun x ↦ x.getStr? >>= rational
    return ⟨dead, rank, next, rats[0]!, rats[1]!, rats[2]!, rats[3]!⟩
  parsed.toOption.getD default

def Vectors.certificate (v : Vectors) (n : Nat) (positive : 0 < n) : Certificate n where
  dead i := v.dead[i.val]?.getD false
  rank i := v.rank[i.val]?.getD 0
  next i := ⟨v.next[i.val]?.getD 0 % n, Nat.mod_lt _ positive⟩
  mass i := v.mass[i.val]?.getD 0
  first i := v.first[i.val]?.getD 0
  second i := v.second[i.val]?.getD 0
  rejection i := v.rejection[i.val]?.getD 0

def Vectors.statistics (v : Vectors) (i : Nat) : OutputStatistics :=
  ⟨v.mass[i]?.getD 0, v.first[i]?.getD 0, v.second[i]?.getD 0⟩

def Vectors.verify (v : Vectors) (source : Core) (subject : Subject)
    (limits : Finite.Limits) (initial : Nat) (answer : OutputStatistics) : Bool :=
  match Finite.Reward.explore source subject limits with
  | .complete candidate replay =>
    let model := candidate.toModel replay
    if positive : 0 < model.size then
      let c := v.certificate model.size positive
      decide (candidate.initial = initial) &&
      decide (v.dead.size = model.size ∧ v.rank.size = model.size ∧
        v.next.size = model.size ∧ v.mass.size = model.size ∧ v.first.size = model.size ∧
        v.second.size = model.size ∧ v.rejection.size = model.size) &&
      v.next.all (fun i ↦ decide (i < model.size)) &&
      decide (∀ i, c.StateValid model i) && decide (v.statistics initial = answer)
    else false
  | _ => false

theorem Vectors.verify_sound (v : Vectors) (source : Core) (subject : Subject)
    (limits : Finite.Limits) (initial : Nat)
    (answer : OutputStatistics)
    (checked : v.verify source subject limits initial answer = true) :
    answer.Matches (Spec.Paper.bigStepMeasure (subject.program source)) := by
  unfold Vectors.verify at checked
  cases explored : Finite.Reward.explore source subject limits with
  | incomplete limit discovered expanded edges => simp [explored] at checked
  | failed state failure => simp [explored] at checked
  | complete candidate replay =>
    simp only [explored] at checked
    split at checked
    next positive =>
      simp only [Bool.and_eq_true, decide_eq_true_eq] at checked
      have valid := checked.1.2
      have first := checked.1.1.1.1
      have sound := solution_outputStatistics (candidate.toModel replay) _
        (replay_matches candidate replay) ((v.certificate _ positive).solution _ valid)
      have result : (v.statistics initial).Matches (Spec.Paper.bigStepMeasure (subject.program source)) := by
        simpa [Finite.Reward.Solution.statistics, Certificate.solution,
          Vectors.statistics, Vectors.certificate, Finite.Reward.Candidate.toModel, first] using sound
      exact checked.2 ▸ result
    next => contradiction

end Determinize.Proof.RewardModel
