import Determinize.Proof.Replay

namespace Determinize.Proof.StepTraces

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr

noncomputable section

theorem symbolic_skeleton_isValue (expression : AffineExpr n) :
    expression.skeleton.isValue = expression.isValue := by
  rw [← expression.realize_skeleton (fun _ => 0), ← isValue_eq_skeletonIsValue,
    AffineExpr.realize_isValue]

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
  | uniform lowerTyped upperTyped ihl ihr =>
      rename_i context' lower mode upper
      simp only [AffineExpr.skeleton, generationOp, symbolic_skeleton_isValue]
      rw [symbolicReduce_uniform_eq]
      by_cases lowerValue : lower.isValue = true
      · simp only [lowerValue, ↓reduceIte]
        by_cases upperValue : upper.isValue = true
        · simp only [upperValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value lowerTyped lowerValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value upperTyped upperValue
          cases mode with
          | E => simp [affineValue?, siteOp]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients lowerTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients upperTyped
              simp [constantValue?, siteOp]
        · simp only [upperValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
          exact ihr
      · simp only [lowerValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
        exact ihl
  | gaussian lowerTyped upperTyped ihl ihr =>
      rename_i context' lower mode upper
      simp only [AffineExpr.skeleton, generationOp, symbolic_skeleton_isValue]
      rw [symbolicReduce_gaussian_eq]
      by_cases lowerValue : lower.isValue = true
      · simp only [lowerValue, ↓reduceIte]
        by_cases upperValue : upper.isValue = true
        · simp only [upperValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value lowerTyped lowerValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value upperTyped upperValue
          cases mode with
          | E =>
              rcases y with ⟨y0, yc⟩
              obtain rfl : yc = 0 := wellTyped_realG_coefficients upperTyped
              simp [affineValue?, constantValue?, siteOp]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients lowerTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients upperTyped
              simp [constantValue?, siteOp]
        · simp only [upperValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
          exact ihr
      · simp only [lowerValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
        exact ihl
  | poisson rateTyped ih =>
      rename_i context' rate mode
      simp only [AffineExpr.skeleton, generationOp, symbolic_skeleton_isValue]
      rw [symbolicReduce_poisson_eq]
      by_cases rateValue : rate.isValue = true
      · simp only [rateValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value rateTyped rateValue
        cases mode with
        | E => simp [affineValue?, siteOp]
        | G =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients rateTyped
            simp [constantValue?, siteOp]
      · simp only [rateValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
        exact ih
  | discrete =>
      rename_i context' mode d
      cases mode <;> simp [AffineExpr.skeleton, generationOp, symbolicReduce, siteOp]
  | bernoulli probabilityTyped ih =>
      rename_i context' probability mode
      simp only [AffineExpr.skeleton, generationOp, symbolic_skeleton_isValue]
      rw [symbolicReduce_bernoulli_eq]
      by_cases probabilityValue : probability.isValue = true
      · simp only [probabilityValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value probabilityTyped probabilityValue
        cases mode with
        | E => simp [affineValue?, siteOp]
        | G =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients probabilityTyped
            simp [constantValue?, siteOp]
      · simp only [probabilityValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
        exact ih
  | exponential rateTyped ih =>
      rename_i context' rate mode
      simp only [AffineExpr.skeleton, generationOp, symbolic_skeleton_isValue]
      rw [symbolicReduce_exponential_eq]
      by_cases rateValue : rate.isValue = true
      · simp only [rateValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value rateTyped rateValue
        cases mode with
        | E =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients rateTyped
            simp [constantValue?, siteOp]
        | G =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients rateTyped
            simp [constantValue?, siteOp]
      · simp only [rateValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
        exact ih
  | beta lowerTyped upperTyped ihl ihr =>
      rename_i context' lower upper mode
      simp only [AffineExpr.skeleton, generationOp, symbolic_skeleton_isValue]
      rw [symbolicReduce_beta_eq]
      by_cases lowerValue : lower.isValue = true
      · simp only [lowerValue, ↓reduceIte]
        by_cases upperValue : upper.isValue = true
        · simp only [upperValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value lowerTyped lowerValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value upperTyped upperValue
          cases mode with
          | E =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients lowerTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients upperTyped
              simp [constantValue?, siteOp]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients lowerTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients upperTyped
              simp [constantValue?, siteOp]
        · simp only [upperValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
          exact ihr
      · simp only [lowerValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
        exact ihl
  | gamma lowerTyped upperTyped ihl ihr =>
      rename_i context' lower mode upper
      simp only [AffineExpr.skeleton, generationOp, symbolic_skeleton_isValue]
      rw [symbolicReduce_gamma_eq]
      by_cases lowerValue : lower.isValue = true
      · simp only [lowerValue, ↓reduceIte]
        by_cases upperValue : upper.isValue = true
        · simp only [upperValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value lowerTyped lowerValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value upperTyped upperValue
          cases mode with
          | E =>
              rcases y with ⟨y0, yc⟩
              obtain rfl : yc = 0 := wellTyped_realG_coefficients upperTyped
              simp [affineValue?, constantValue?, siteOp]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients lowerTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients upperTyped
              simp [constantValue?, siteOp]
        · simp only [upperValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
          exact ihr
      · simp only [lowerValue, Bool.false_eq_true, ↓reduceIte, generationDraw_wrap]
        exact ihl
  | sub _ _ ih => exact ih
  | _ =>
      simp only [AffineExpr.skeleton, generationOp, symbolic_skeleton_isValue]
      rw [symbolicReduce.eq_def]
      iterate 5
        all_goals try split
        all_goals try simp only [generationDraw_wrap, generationDraw_next,
          generationDraw_stuck, Option.isSome_none, ↓reduceIte] at *
      all_goals try simp_all [generationOp_symbolic_value]

set_option maxHeartbeats 800000 in
theorem generationOp_determinize (expression : Expr) :
    generationOp expression.determinize.skeleton = generationOp expression.skeleton := by
  fun_induction Expr.determinize expression <;>
    simp_all [Expr.skeleton, generationOp, ← isValue_eq_skeletonIsValue,
      SymbolicSoundness.TargetSafety.determinize_isValue]

end
end Determinize.Proof.StepTraces
