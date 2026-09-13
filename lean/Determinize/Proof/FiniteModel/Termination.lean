import Determinize.Spec.FiniteModel.Termination
import Determinize.Proof.FiniteModel.Statistics

namespace Determinize.Proof.FiniteModel
open Spec.FiniteModel MeasureTheory

theorem survival_nonnegative (model : Model) (n : Nat) (state : Fin model.size) :
    0 ≤ model.survivalWithin n state := by
  induction n generalizing state with
  | zero => simp [Model.survivalWithin]; split <;> norm_num
  | succ n ih =>
    simp only [Model.survivalWithin]
    split
    · exact Finset.sum_nonneg fun next _ => mul_nonneg (model.nonnegative state next) (ih next)
    · exact le_rfl

theorem within_mass_balance (model : Model) (n : Nat) (state : Fin model.size) :
    model.outputWithin n state Set.univ + model.rejectionModel.outputWithin n state Set.univ +
      ENNReal.ofReal (model.survivalWithin n state : ℝ) = 1 := by
  induction n generalizing state with
  | zero => cases h : model.kind state <;> simp [Model.outputWithin, Model.survivalWithin, h]
  | succ n ih =>
    cases h : model.kind state with
    | returned r => simp [Model.outputWithin, Model.survivalWithin, h]
    | rejected => simp [Model.outputWithin, Model.survivalWithin, h]
    | transient =>
      simp only [Model.outputWithin, Model.survivalWithin, h, ↓reduceIte, Rat.cast_sum,
        Rat.cast_mul, Measure.finsetSum_apply, Measure.smul_apply, smul_eq_mul]
      change (∑ next, ENNReal.ofReal (model.transition state next : ℝ) * model.outputWithin n next Set.univ) +
        (∑ next, ENNReal.ofReal (model.transition state next : ℝ) * model.rejectionModel.outputWithin n next Set.univ) +
        ENNReal.ofReal (∑ next, (model.transition state next : ℝ) * (model.survivalWithin n next : ℝ)) = 1
      rw [ENNReal.ofReal_sum_of_nonneg (fun next _ => mul_nonneg
        (by exact_mod_cast model.nonnegative state next) (by exact_mod_cast survival_nonnegative model n next))]
      have cast_mul (next : Fin model.size) (r : ℝ) :
          ENNReal.ofReal ((model.transition state next : ℝ) * r) =
            ENNReal.ofReal (model.transition state next : ℝ) * ENNReal.ofReal r :=
        ENNReal.ofReal_mul (by exact_mod_cast model.nonnegative state next)
      simp_rw [cast_mul]
      rw [← Finset.sum_add_distrib, ← Finset.sum_add_distrib]
      simp_rw [← mul_add, ih, mul_one]
      rw [← ENNReal.ofReal_sum_of_nonneg (fun next _ => by exact_mod_cast model.nonnegative state next)]
      have total : ∑ next, (model.transition state next : ℝ) = 1 := by exact_mod_cast model.normalized state
      simp [total]

theorem massBalance : Spec.FiniteModel.massBalanceThm := by
  intro model
  have complements (n : Nat) :
      model.outputWithin n model.initial Set.univ + model.rejectionModel.outputWithin n model.initial Set.univ =
        1 - ENNReal.ofReal (model.survivalWithin n model.initial : ℝ) :=
    ENNReal.eq_sub_of_add_eq (by simp) (within_mass_balance model n model.initial)
  have sumMass : model.outputMeasure Set.univ + model.rejectionProbability =
      1 - model.divergenceProbability := by
    rw [Model.outputMeasure, Model.rejectionProbability, Model.outputMeasure,
      monotone_measure_iSup_apply _ (outputWithin_mono model _) _ MeasurableSet.univ,
      monotone_measure_iSup_apply _ (outputWithin_mono model.rejectionModel _) _ MeasurableSet.univ,
      ENNReal.iSup_add_iSup_of_monotone
        (fun a b h => outputWithin_mono model model.initial h Set.univ)
        (fun a b h => outputWithin_mono model.rejectionModel model.initial h Set.univ)]
    simp only [complements, Model.divergenceProbability, ENNReal.sub_iInf]
  rw [sumMass]
  apply tsub_add_cancel_of_le
  apply (iInf_le (fun n => ENNReal.ofReal (model.survivalWithin n model.initial : ℝ)) 0).trans
  simp only [Model.survivalWithin]
  split <;> norm_num

theorem rejection_closed (model : Model) (dead : Fin model.size → Bool)
    (closed : ClosedDivergence model dead) : ClosedDivergence model.rejectionModel dead := by
  intro state h
  refine ⟨?_, (closed state h).2⟩
  simp [(closed state h).1]

abbrev rejectionQuery (model : Model) (dead : Fin model.size → Bool) :=
  rewards (cut model.rejectionModel dead) (fun _ => 1)

structure TerminationCertificate (model : Model) where
  output : MomentCertificate model
  rejection : Fin model.size → Rat

def TerminationCertificate.Valid (model : Model) (certificate : TerminationCertificate model) : Prop :=
  certificate.output.Valid model ∧
    (⟨certificate.rejection, 0⟩ : ResultCertificate (rejectionQuery model certificate.output.dead)).Equations
      (rejectionQuery model certificate.output.dead)

instance (model : Model) (certificate : TerminationCertificate model) : Decidable (certificate.Valid model) :=
  inferInstanceAs (Decidable (_ ∧ _))

def TerminationCertificate.statistics (model : Model) (certificate : TerminationCertificate model) : TerminationStatistics :=
  let p := certificate.output.values .mass model.initial
  let r := certificate.rejection model.initial
  ⟨p, r, 1-p-r⟩

theorem terminationCertificate_sound (model : Model) (certificate : TerminationCertificate model)
    (valid : certificate.Valid model) : (certificate.statistics model).Matches model := by
  have output := (momentCertificate_sound model certificate.output valid.1).1
  have rejection : model.rejectionProbability.toReal = (certificate.rejection model.initial : ℝ) := by
    have query := query_sound (cut model.rejectionModel certificate.output.dead) (fun _ => 1)
      (fun _ => 1) (fun _ => by simp) ⟨certificate.rejection, 0⟩ valid.2
      ⟨certificate.output.rank, certificate.output.next⟩ (by
        intro state transient
        apply valid.1.2.1 state
        cases h : model.kind state <;> cases d : certificate.output.dead state <;> simp_all)
    rw [cut_outputMeasure _ _ (rejection_closed model _ valid.1.1)] at query
    simpa [integral_const, Model.rejectionProbability, measureReal_def] using query
  refine ⟨output, rejection, ?_⟩
  have balance := massBalance model
  have finiteReturn : model.outputMeasure Set.univ ≠ ⊤ := by
    exact ne_top_of_le_ne_top (by simp) ((le_add_right le_rfl).trans ((le_add_right le_rfl).trans_eq balance))
  have finiteReject : model.rejectionProbability ≠ ⊤ := by
    exact ne_top_of_le_ne_top (by simp) ((le_add_left le_rfl).trans ((le_add_right le_rfl).trans_eq balance))
  have finiteDiverge : model.divergenceProbability ≠ ⊤ := by
    exact ne_top_of_le_ne_top (by simp) ((le_add_left le_rfl).trans_eq balance)
  have realBalance := congrArg ENNReal.toReal balance
  rw [ENNReal.toReal_add (ENNReal.add_ne_top.mpr ⟨finiteReturn, finiteReject⟩) finiteDiverge,
    ENNReal.toReal_add finiteReturn finiteReject, ENNReal.toReal_one] at realBalance
  change (model.outputMeasure Set.univ).toReal = _ at output
  rw [output, rejection] at realBalance
  simp only [TerminationCertificate.statistics, Rat.cast_sub, Rat.cast_one]
  simp only [MomentCertificate.statistics] at realBalance
  linarith

end Determinize.Proof.FiniteModel
