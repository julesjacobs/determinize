import Determinize.Proof.FiniteModel.Execution
import Determinize.Proof.FiniteModel.Administrative

namespace Determinize.Proof.FiniteModel
open Statement.Paper Determinize.Finite Checking Binding MeasureTheory

theorem sameObservations_of_eq (before after : State) (equal : stateExpr before = stateExpr after) :
    SameObservations before after := by
  simp [SameObservations, equal]

theorem paperStep_next (before after : State) (notValue : (stateExpr before).isValue = false)
    (reduction : reduce (stateExpr before) = .next (stateExpr after)) :
    PaperStep before [(1,after)] := by
  refine ⟨?_, ?_, ?_⟩
  · cases h : stateExpr before <;> simp_all [cumulativeOutputMeasure, Expr.isValue]
  · intro fuel
    simp [cumulativeOutputMeasure, reduction, weightedOutput]
  · intro fuel
    simp [DoesNotGetStuckAt, notValue, reduction]

theorem paperStep_sample (before : State) (site : Mode × Kind × Op) (arguments : List Rat)
    (outcomes : List (Rat × Rat)) (stack : List Frame)
    (success : finiteLaw site.2.2 site.2.1 arguments = .ok outcomes)
    (notValue : (stateExpr before).isValue = false)
    (reduction : reduce (stateExpr before) =
      .sample site (outcomeMeasure outcomes) (fun x => stackExpr stack (.real x))) :
    PaperStep before (outcomes.map fun (p,x) => (p, .deliver (.number x) stack)) := by
  refine ⟨?_, ?_, ?_⟩
  · cases h : stateExpr before <;> simp_all [cumulativeOutputMeasure, Expr.isValue]
  · intro fuel
    simp only [cumulativeOutputMeasure, reduction]
    rw [outcomeMeasure_bind _ _ (Proof.Paper.measurable_sample_cumulative fuel _ _ _ _ reduction)]
    simp only [weightedOutput, List.map_map, Function.comp_def, stateExpr, valueExpr]
  · intro fuel
    have mass := (finiteLaw_probability _ _ _ _ success).measure_univ
    simp only [DoesNotGetStuckAt, notValue, Bool.false_eq_true, ↓reduceIte, reduction,
      mass, true_and, outcomeMeasure_ae, List.mem_map]
    constructor
    · intro safety entry member positive
      obtain ⟨⟨p,x⟩, member, rfl⟩ := member
      simpa only [stateExpr, valueExpr] using safety (p,x) member positive
    · intro safety entry member positive
      simpa only [stateExpr, valueExpr] using safety (entry.1, .deliver (.number entry.2) stack)
        ⟨entry, member, rfl⟩ positive

theorem reject_same (environment : List Value) (stack : List Frame) :
    SameObservations (.eval .reject environment stack) .rejected := by
  have left := stack_absorbing stack .reject rfl rfl
  have right := stack_absorbing [] .reject rfl rfl
  have equality : stateExpr (.eval .reject environment stack) = stackExpr stack .reject := by
    simp [stateExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]
  refine ⟨?_, ?_⟩
  · intro fuel
    rw [equality, absorbing_cumulative_zero _ left.1 left.2]
    exact (absorbing_cumulative_zero _ right.1 right.2 fuel).symm
  · intro fuel
    exact iff_of_true ((equality ▸ absorbing_safe _ left.1 left.2) fuel)
      (absorbing_safe _ right.1 right.2 fuel)

theorem stepMeaning_returned (reward : Rat) : StepMeaning (.deliver (.number reward) []) (.returned reward) := by
  have realState : stateExpr (.deliver (.number reward) []) = .real (reward : ℝ) := by
    simp [stateExpr, stackExpr, valueExpr]
  constructor
  · intro fuel
    rw [realState]
    induction fuel with
    | zero => rfl
    | succ fuel ih => exact ih
  · intro fuel
    rw [realState]
    cases fuel <;> trivial

theorem stepMeaning_rejected : StepMeaning .rejected .rejected := by
  exact ⟨absorbing_cumulative_zero .reject rfl rfl, absorbing_safe .reject rfl rfl⟩

end Determinize.Proof.FiniteModel
