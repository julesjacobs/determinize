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

structure MomentCertificate (model : Model) where
  dead : Fin model.size → Bool
  rank : Fin model.size → Nat
  next : Fin model.size → Fin model.size
  values : Moment → Fin model.size → Rat

abbrev MomentCertificate.model (model : Model) (certificate : MomentCertificate model) (moment : Moment) :=
  rewards (cut model certificate.dead) moment.rational

def MomentCertificate.result (model : Model) (certificate : MomentCertificate model) (moment : Moment) :
    ResultCertificate (certificate.model model moment) := ⟨certificate.values moment, 0⟩

def MomentCertificate.Valid (model : Model) (certificate : MomentCertificate model) : Prop :=
  ClosedDivergence model certificate.dead ∧
    (⟨certificate.rank, certificate.next⟩ : Paths (cut model certificate.dead)).Valid (cut model certificate.dead) ∧
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
    (certificate.result model moment) (valid.2.2 moment) ⟨certificate.rank, certificate.next⟩
  intro state transient
  apply valid.2.1 state
  cases h : (cut model certificate.dead).kind state <;> simp_all

theorem momentCertificate_sound (model : Model) (certificate : MomentCertificate model)
    (valid : certificate.Valid model) : (certificate.statistics model).Matches model.outputMeasure := by
  refine ⟨(integrable_const_iff_isFiniteMeasure (by norm_num : (1 : ℝ) ≠ 0)).mp
    (outputAt_integrable model model.initial (fun _ => 1)), outputAt_integrable model model.initial (fun x => x ^ 2), ?_, ?_, ?_⟩
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
  rw [correct.mass, correct.first, Rat.cast_div]

theorem statistics_first_integrable (statistics : OutputStatistics) (law : Measure ℝ)
    (correct : statistics.Matches law) : Integrable id law := by
  let := correct.finite
  exact ((memLp_two_iff_integrable_sq aestronglyMeasurable_id).mpr
    correct.squareIntegrable).integrable (by norm_num)

theorem statistics_conditional_variance (law : Measure ℝ) (statistics : OutputStatistics)
    (correct : statistics.Matches law) (positive : 0 < statistics.returnMass) :
    variance id ((law Set.univ)⁻¹ • law) =
      ((statistics.secondMoment / statistics.returnMass -
        (statistics.firstMoment / statistics.returnMass) ^ 2 : Rat) : ℝ) := by
  let := correct.finite
  have nonzero : law Set.univ ≠ 0 := by
    intro zero
    have h := correct.mass
    simp [measureReal_def, zero] at h
    have : (0 : ℝ) < (statistics.returnMass : ℝ) := by exact_mod_cast positive
    linarith
  have mem : MemLp id 2 law :=
    (memLp_two_iff_integrable_sq aestronglyMeasurable_id).mpr correct.squareIntegrable
  rw [normalized_variance _ nonzero mem]
  change (∫ x : ℝ, x ^ 2 ∂law) / law.real Set.univ -
    ((∫ x : ℝ, x ∂law) / law.real Set.univ) ^ 2 = _
  rw [correct.mass, correct.first, correct.second]
  push_cast
  rfl

end Determinize.Proof.FiniteModel
