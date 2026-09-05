import Determinize.Proof.Replay

namespace Determinize.Proof.StepTraces

open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr

noncomputable section

theorem symbolic_skeleton_isValue (expression : AffineExpr n) :
    expression.skeleton.isValue = expression.isValue := by
  rw [← expression.realize_skeleton (fun _ => 0), ← isValue_eq_skeletonIsValue,
    AffineExpr.realize_isValue]

theorem operandOp_symbolic (arguments : List (AffineExpr n)) :
    operandOp (arguments.map (fun e => (e.isValue, generationOp e.skeleton))) =
      (AffineExpr.firstNonValue arguments).map (fun found => generationOp found.2.1.skeleton) := by
  induction arguments with
  | nil => rfl
  | cons head tail ih =>
      simp only [List.map_cons, operandOp, AffineExpr.firstNonValue]
      cases headValue : head.isValue <;> simp only [Bool.false_eq_true, ↓reduceIte]
      · rfl
      · rw [ih]
        cases AffineExpr.firstNonValue tail <;> rfl

def generationDraw : SymbolicAction laws n → Bool
  | .sampleG _ _ _ => true
  | _ => false

@[simp] theorem generationDraw_wrap (action : SymbolicAction laws n)
    (context : AffineExpr n → AffineExpr n)
    (lifted : AffineExpr (n + 1) → AffineExpr (n + 1)) :
    generationDraw (action.wrap context lifted) = generationDraw action := by
  cases action <;> rfl

@[simp] theorem generationDraw_next (expression : AffineExpr n) :
    generationDraw (.next expression : SymbolicAction laws n) = false := rfl
@[simp] theorem generationDraw_stuck : generationDraw (.stuck : SymbolicAction laws n) = false := rfl
@[simp] theorem generationDraw_sampleE (op affine general continuation) :
    generationDraw (.sampleE op affine general continuation : SymbolicAction laws n) = false := rfl
@[simp] theorem generationDraw_sampleG (fiber continuation) :
    generationDraw (.sampleG site fiber continuation : SymbolicAction laws n) = true := rfl

@[simp] theorem generationOp_symbolic_value (expression : AffineExpr n)
    (value : expression.isValue = true) : generationOp expression.skeleton = none :=
  generationOp_value (by rwa [symbolic_skeleton_isValue])

set_option linter.unusedSimpArgs false in
set_option maxRecDepth 2048 in
set_option maxHeartbeats 800000 in
theorem symbolic_generationDraw (laws : PrimitiveLaws)
    (typed : WellTyped context expression ty) :
    (generationOp expression.skeleton).isSome = generationDraw (symbolicReduce laws expression) := by
  induction typed with
  | sampleE op affineLength generalLength affineTyped generalTyped iha ihg =>
      simp only [AffineExpr.skeleton, generationOp, List.map_map, Function.comp_def,
        symbolic_skeleton_isValue, operandOp_append, operandOp_symbolic]
      rw [symbolicReduce_sample_eq]
      cases found : AffineExpr.firstNonValue _ with
      | some result =>
          rcases result with ⟨front, current, suffix⟩
          simp only [Option.map_some, generationDraw_wrap]
          exact iha current (AffineExpr.firstNonValue_current_mem found)
      | none =>
          simp only [Option.map_none]
          cases foundGeneral : AffineExpr.firstNonValue _ with
          | some result =>
              rcases result with ⟨front, current, suffix⟩
              simp only [Option.map_some, generationDraw_wrap]
              exact ihg current (AffineExpr.firstNonValue_current_mem foundGeneral)
          | none =>
              have affineValues := (AffineExpr.firstNonValue_eq_none_iff _).mp found
              have generalValues := (AffineExpr.firstNonValue_eq_none_iff _).mp foundGeneral
              obtain ⟨av, ha⟩ := allAffineValues_of_wellTyped_values affineTyped affineValues
              obtain ⟨gv, hg⟩ := allConstantValues_of_wellTypedG_values generalTyped generalValues
                (fun child member => (generalTyped child member).gconstant)
              simp [ha, hg]
  | sampleG op affineLength generalLength affineTyped generalTyped iha ihg =>
      simp only [AffineExpr.skeleton, generationOp, List.map_map, Function.comp_def,
        symbolic_skeleton_isValue, operandOp_append, operandOp_symbolic]
      rw [symbolicReduce_sample_eq]
      cases found : AffineExpr.firstNonValue _ with
      | some result =>
          rcases result with ⟨front, current, suffix⟩
          simp only [Option.map_some, generationDraw_wrap]
          exact iha current (AffineExpr.firstNonValue_current_mem found)
      | none =>
          simp only [Option.map_none]
          cases foundGeneral : AffineExpr.firstNonValue _ with
          | some result =>
              rcases result with ⟨front, current, suffix⟩
              simp only [Option.map_some, generationDraw_wrap]
              exact ihg current (AffineExpr.firstNonValue_current_mem foundGeneral)
          | none =>
              have affineValues := (AffineExpr.firstNonValue_eq_none_iff _).mp found
              have generalValues := (AffineExpr.firstNonValue_eq_none_iff _).mp foundGeneral
              obtain ⟨av, ha⟩ := allConstantValues_of_wellTypedG_values affineTyped affineValues
                (fun child member => (affineTyped child member).gconstant)
              obtain ⟨gv, hg⟩ := allConstantValues_of_wellTypedG_values generalTyped generalValues
                (fun child member => (generalTyped child member).gconstant)
              simp [ha, hg]
  | _ =>
      simp only [AffineExpr.skeleton, generationOp, symbolic_skeleton_isValue]
      rw [symbolicReduce.eq_def]
      iterate 5
        all_goals try split
        all_goals try simp only [generationDraw_wrap, generationDraw_next,
          generationDraw_stuck, Option.isSome_none, ↓reduceIte] at *
      all_goals try simp_all [generationOp_symbolic_value]

theorem generationOperandMap_determinize (args : List Expr)
    (ih : ∀ e ∈ args, generationOp e.determinize.skeleton = generationOp e.skeleton) :
    args.map (fun e => (e.isValue, generationOp e.determinize.skeleton)) =
      args.map (fun e => (e.isValue, generationOp e.skeleton)) := by
  apply List.map_congr_left
  intro e mem
  rw [ih e mem]

set_option maxHeartbeats 800000 in
theorem generationOp_determinize (expression : Expr) :
    generationOp expression.determinize.skeleton = generationOp expression.skeleton := by
  fun_induction Expr.determinize expression <;>
    simp_all [Expr.skeleton, generationOp, ← isValue_eq_skeletonIsValue,
      SymbolicSoundness.TargetSafety.determinize_isValue, Function.comp_def,
      generationOperandMap_determinize]
  rename_i mode tag affine general _ iha ihg
  cases mode <;> cases tag <;>
    simp_all [generationOp, List.map_map, Function.comp_def, ← isValue_eq_skeletonIsValue,
      SymbolicSoundness.TargetSafety.determinize_isValue, generationOperandMap_determinize]

end
end Determinize.Proof.StepTraces
