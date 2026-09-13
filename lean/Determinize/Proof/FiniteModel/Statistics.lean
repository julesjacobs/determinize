import Determinize.Proof.FiniteModel.Queries
import Determinize.Spec.FiniteModel.Statistics
import Determinize.Proof.Normalization

namespace Determinize.Proof.FiniteModel
open Spec.FiniteModel MeasureTheory ProbabilityTheory

inductive Moment where
  | mass | first | second
  deriving DecidableEq, Repr

instance : Fintype Moment := ⟨{.mass, .first, .second}, by intro m; cases m <;> simp⟩

def Moment.rational : Moment → Rat → Rat
  | .mass => fun _ => 1
  | .first => id
  | .second => fun x => x ^ 2

def Moment.real : Moment → ℝ → ℝ
  | .mass => fun _ => 1
  | .first => id
  | .second => fun x => x ^ 2

theorem Moment.agree (moment : Moment) (q : Rat) : moment.real (q : ℝ) = (moment.rational q : ℝ) := by
  cases moment <;> simp [Moment.real, Moment.rational]

theorem survival_rewards (model : Model) (f : Rat → Rat) (n : Nat) (state : Fin model.size) :
    (rewards model f).survivalWithin n state = model.survivalWithin n state := by
  induction n generalizing state with
  | zero => cases h : model.kind state <;> simp [Model.survivalWithin, h]
  | succ n ih =>
      cases h : model.kind state <;> simp [Model.survivalWithin, h, ih]

structure MomentCertificate (model : Model) where
  dead : Fin model.size → Bool
  horizon : Nat
  values : Moment → Fin model.size → Rat

abbrev MomentCertificate.model (model : Model) (certificate : MomentCertificate model) (moment : Moment) :=
  rewards (cut model certificate.dead) moment.rational

def MomentCertificate.result (model : Model) (certificate : MomentCertificate model) (moment : Moment) :
    ResultCertificate (certificate.model model moment) := ⟨certificate.values moment, certificate.horizon⟩

def MomentCertificate.Valid (model : Model) (certificate : MomentCertificate model) : Prop :=
  ClosedDivergence model certificate.dead ∧
    (∀ state, (cut model certificate.dead).survivalWithin certificate.horizon state < 1) ∧
    ∀ moment, (certificate.result model moment).Equations (certificate.model model moment)

instance (model : Model) (certificate : MomentCertificate model) : Decidable (certificate.Valid model) :=
  inferInstanceAs (Decidable (_ ∧ _ ∧ _))

def MomentCertificate.statistics (model : Model) (certificate : MomentCertificate model) : OutputStatistics :=
  ⟨certificate.values .mass model.initial, certificate.values .first model.initial,
    certificate.values .second model.initial⟩

theorem momentCertificate_integral (model : Model) (certificate : MomentCertificate model)
    (valid : certificate.Valid model) (moment : Moment) :
    (∫ x, moment.real x ∂model.outputMeasure) = (certificate.values moment model.initial : ℝ) := by
  rw [← cut_outputMeasure model certificate.dead valid.1]
  apply query_sound (cut model certificate.dead) moment.rational moment.real moment.agree
    (certificate.result model moment)
  refine ⟨valid.2.2 moment, ?_⟩
  intro state
  simpa [ResultCertificate.Absorption, MomentCertificate.result, survival_rewards] using valid.2.1 state

theorem momentCertificate_sound (model : Model) (certificate : MomentCertificate model)
    (valid : certificate.Valid model) : (certificate.statistics model).Matches model.outputMeasure := by
  refine ⟨?_, ?_, ?_⟩
  · simpa [Moment.real, MomentCertificate.statistics, integral_const] using
      momentCertificate_integral model certificate valid .mass
  · exact momentCertificate_integral model certificate valid .first
  · exact momentCertificate_integral model certificate valid .second

theorem outputMeasure_memLp (model : Model) : MemLp id 2 model.outputMeasure :=
  (memLp_two_iff_integrable_sq aestronglyMeasurable_id).mpr
    (outputAt_integrable model model.initial (fun x => x ^ 2))

theorem statistics_conditional_mean (statistics : OutputStatistics) (law : Measure ℝ)
    (correct : statistics.Matches law) :
    (∫ x : ℝ, x ∂law) / law.real Set.univ = (statistics.firstMoment / statistics.returnMass : Rat) := by
  rw [correct.1, correct.2.1, Rat.cast_div]

theorem statistics_conditional_variance (model : Model) (statistics : OutputStatistics)
    (correct : statistics.Matches model.outputMeasure) (positive : 0 < statistics.returnMass) :
    variance id ((model.outputMeasure Set.univ)⁻¹ • model.outputMeasure) =
      ((statistics.secondMoment / statistics.returnMass -
        (statistics.firstMoment / statistics.returnMass) ^ 2 : Rat) : ℝ) := by
  have finite : IsFiniteMeasure model.outputMeasure := by
    exact (integrable_const_iff_isFiniteMeasure (by norm_num : (1 : ℝ) ≠ 0)).mp
      (outputAt_integrable model model.initial (fun _ => 1))
  have nonzero : model.outputMeasure Set.univ ≠ 0 := by
    intro zero
    have h := correct.1
    simp [measureReal_def, zero] at h
    have : (0 : ℝ) < (statistics.returnMass : ℝ) := by exact_mod_cast positive
    linarith
  rw [normalized_variance _ nonzero (outputMeasure_memLp model)]
  change (∫ x : ℝ, x ^ 2 ∂model.outputMeasure) / model.outputMeasure.real Set.univ -
    ((∫ x : ℝ, x ∂model.outputMeasure) / model.outputMeasure.real Set.univ) ^ 2 = _
  rw [correct.1, correct.2.1, correct.2.2]
  push_cast
  rfl

end Determinize.Proof.FiniteModel
