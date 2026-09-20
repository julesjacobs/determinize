import Determinize.Proof.RewardModel.Measure

namespace Determinize.Proof.RewardModel
open MeasureTheory Spec.RewardModel
open scoped ENNReal

private theorem lintegral_iSup_measure (μ : Nat → Measure ℝ) (mono : Monotone μ)
    (f : ℝ → ℝ≥0∞) (hf : Measurable f) :
    (∫⁻ x, f x ∂(⨆ n, μ n)) = ⨆ n, ∫⁻ x, f x ∂μ n := by
  simp_rw [lintegral_eq_iSup_eapprox_lintegral hf]
  rw [iSup_comm]
  apply iSup_congr
  intro k
  simp only [SimpleFunc.lintegral]
  simp_rw [FiniteModel.monotone_measure_iSup_apply μ mono _
    ((SimpleFunc.eapprox f k).measurableSet_fiber _), ENNReal.mul_iSup]
  exact ENNReal.finsetSum_iSup_of_monotone (fun a _ _ h => mul_le_mul' le_rfl (mono h _))

theorem integrable_iSup_of_bound (μ : Nat → Measure ℝ) (mono : Monotone μ)
    (f : ℝ → ℝ) (hf : Measurable f) (bound : ℝ≥0∞) (finite : bound < ⊤)
    (bounded : ∀ n, (∫⁻ x, ENNReal.ofReal ‖f x‖ ∂μ n) ≤ bound) :
    Integrable f (⨆ n, μ n) := by
  refine ⟨hf.aestronglyMeasurable, (hasFiniteIntegral_iff_norm f).mpr ?_⟩
  rw [lintegral_iSup_measure μ mono _ (by fun_prop)]
  exact (iSup_le bounded).trans_lt finite

private def firstCost (x : ℝ) : ℝ≥0∞ := ENNReal.ofReal |x|
private def secondCost (x : ℝ) : ℝ≥0∞ := ENNReal.ofReal (x^2)

private theorem firstCost_translate (x r : ℝ) :
    firstCost (x+r) ≤ firstCost x + ENNReal.ofReal |r| := by
  unfold firstCost
  rw [← ENNReal.ofReal_add (abs_nonneg x) (abs_nonneg r)]
  exact ENNReal.ofReal_le_ofReal (abs_add_le x r)

private theorem secondCost_translate (x r : ℝ) :
    secondCost (x+r) ≤ secondCost x + ENNReal.ofReal (2*|r|) * firstCost x + ENNReal.ofReal (r^2) := by
  unfold secondCost firstCost
  rw [← ENNReal.ofReal_mul (by positivity : 0 ≤ 2*|r|),
    ← ENNReal.ofReal_add (sq_nonneg x) (by positivity),
    ← ENNReal.ofReal_add (by positivity) (sq_nonneg r)]
  apply ENNReal.ofReal_le_ofReal
  have h : r*x ≤ |r| * |x| := by simpa [abs_mul] using le_abs_self (r*x)
  nlinarith

private theorem shift_mass (μ : Measure ℝ) (r : Rat) : shift r μ Set.univ = μ Set.univ := by
  simp [shift, Measure.map_apply (show Measurable (fun x : ℝ => x+(r:ℝ)) from measurable_id.add_const _) MeasurableSet.univ]

private theorem shift_first_bound (μ : Measure ℝ) (r : Rat) (a : ℝ)
    (mass : μ Set.univ ≤ 1) (first : (∫⁻ x, firstCost x ∂μ) ≤ ENNReal.ofReal a) :
    (∫⁻ x, firstCost x ∂shift r μ) ≤ ENNReal.ofReal a + ENNReal.ofReal |(r : ℝ)| := by
  rw [shift, lintegral_map (by unfold firstCost; fun_prop : Measurable firstCost)
    (show Measurable (fun x : ℝ => x+(r:ℝ)) from measurable_id.add_const _)]
  calc
    _ ≤ ∫⁻ x, firstCost x + ENNReal.ofReal |(r:ℝ)| ∂μ := lintegral_mono (fun x => firstCost_translate x _)
    _ = (∫⁻ x, firstCost x ∂μ) + ENNReal.ofReal |(r:ℝ)| * μ Set.univ := by
      rw [lintegral_add_left (by unfold firstCost; fun_prop : Measurable firstCost)]
      simp
    _ ≤ ENNReal.ofReal a + ENNReal.ofReal |(r:ℝ)| := by
      simpa using add_le_add first (mul_le_mul' (le_refl (ENNReal.ofReal |(r:ℝ)|)) mass)

private theorem shift_second_bound (μ : Measure ℝ) (r : Rat) (a b : ℝ)
    (mass : μ Set.univ ≤ 1) (first : (∫⁻ x, firstCost x ∂μ) ≤ ENNReal.ofReal a)
    (second : (∫⁻ x, secondCost x ∂μ) ≤ ENNReal.ofReal b) :
    (∫⁻ x, secondCost x ∂shift r μ) ≤
      ENNReal.ofReal b + ENNReal.ofReal (2*|(r:ℝ)|) * ENNReal.ofReal a + ENNReal.ofReal ((r:ℝ)^2) := by
  rw [shift, lintegral_map (by unfold secondCost; fun_prop : Measurable secondCost)
    (show Measurable (fun x : ℝ => x+(r:ℝ)) from measurable_id.add_const _)]
  calc
    _ ≤ ∫⁻ x, secondCost x + ENNReal.ofReal (2*|(r:ℝ)|) * firstCost x + ENNReal.ofReal ((r:ℝ)^2) ∂μ :=
      lintegral_mono (fun x => secondCost_translate x _)
    _ = (∫⁻ x, secondCost x ∂μ) + ENNReal.ofReal (2*|(r:ℝ)|) * (∫⁻ x, firstCost x ∂μ) +
        ENNReal.ofReal ((r:ℝ)^2) * μ Set.univ := by
      rw [lintegral_add_left (by unfold secondCost firstCost; fun_prop),
        lintegral_add_left (by unfold secondCost; fun_prop),
        lintegral_const_mul _ (by unfold firstCost; fun_prop : Measurable firstCost)]
      simp
    _ ≤ _ := by
      simpa using add_le_add (add_le_add second (mul_le_mul' le_rfl first)) (mul_le_mul' le_rfl mass)

structure MomentBounds (model : Model) where
  first : Fin model.size → Rat
  second : Fin model.size → Rat
  first_nonnegative : ∀ i, 0 ≤ first i
  second_nonnegative : ∀ i, 0 ≤ second i
  first_bound : ∀ i, (match model.kind i with
    | .returned b => |b|
    | .rejected => 0
    | .transient => ((model.edges i).map fun e => e.probability * (first e.target + |e.reward|)).sum) ≤ first i
  second_bound : ∀ i, (match model.kind i with
    | .returned b => b^2
    | .rejected => 0
    | .transient => ((model.edges i).map fun e => e.probability *
        (second e.target + 2*|e.reward| * first e.target + e.reward^2)).sum) ≤ second i

private theorem enn_sum (xs : List Rat) (nonneg : ∀ x ∈ xs, 0 ≤ x) :
    (xs.map fun x : Rat => ENNReal.ofReal (x : ℝ)).sum = ENNReal.ofReal (xs.sum : ℝ) := by
  induction xs with
  | nil => simp
  | cons x xs ih =>
    rw [List.map_cons, List.sum_cons, List.sum_cons, Rat.cast_add,
      ENNReal.ofReal_add (by exact_mod_cast nonneg x (by simp))
        (by exact_mod_cast List.sum_nonneg (fun x hx => nonneg x (by simp [hx]))),
      ih (fun x hx => nonneg x (by simp [hx]))]

private theorem weighted_bound {α : Type} (xs : List α) (p b : α → Rat) (v : α → ℝ≥0∞)
    (pn : ∀ x ∈ xs, 0 ≤ p x) (bn : ∀ x ∈ xs, 0 ≤ b x)
    (bound : ∀ x ∈ xs, v x ≤ ENNReal.ofReal (b x : ℝ)) :
    (xs.map fun x => ENNReal.ofReal (p x : ℝ) * v x).sum ≤
      ENNReal.ofReal (((xs.map fun x => p x * b x).sum : Rat) : ℝ) := by
  calc
    _ ≤ (xs.map fun x => ENNReal.ofReal (p x : ℝ) * ENNReal.ofReal (b x : ℝ)).sum := by
      apply List.sum_le_sum
      intro x hx
      exact mul_le_mul' le_rfl (bound x hx)
    _ = _ := by
      have cast : ∀ x ∈ xs, ENNReal.ofReal (p x : ℝ) * ENNReal.ofReal (b x : ℝ) =
          ENNReal.ofReal ((p x * b x : Rat) : ℝ) := by
        intro x hx
        rw [Rat.cast_mul, ENNReal.ofReal_mul (by exact_mod_cast pn x hx)]
      rw [List.map_congr_left cast]
      simpa only [List.map_map, Function.comp_def] using enn_sum (xs.map fun x => p x * b x) (by
        intro y hy
        obtain ⟨x, hx, rfl⟩ := List.mem_map.mp hy
        exact mul_nonneg (pn x hx) (bn x hx))

private theorem measure_list_sum {α : Type} (xs : List α) (μ : α → Measure ℝ) :
    (xs.map μ).sum Set.univ = (xs.map fun x => μ x Set.univ).sum := by
  induction xs with
  | nil => simp
  | cons x xs ih => simp [Measure.add_apply, ih]

private theorem lintegral_list_sum {α : Type} (xs : List α) (μ : α → Measure ℝ) (f : ℝ → ℝ≥0∞) :
    (∫⁻ y, f y ∂(xs.map μ).sum) = (xs.map fun x => ∫⁻ y, f y ∂μ x).sum := by
  induction xs with
  | nil => simp
  | cons x xs ih => simp [lintegral_add_measure, ih]

theorem outputWithin_mass_le_one (model : Model) (n : Nat) (i : Fin model.size) :
    model.outputWithin n i Set.univ ≤ 1 := by
  induction n generalizing i with
  | zero => cases h : model.kind i <;> simp [Model.outputWithin, h]
  | succ n ih =>
    cases h : model.kind i with
    | returned b => simp [Model.outputWithin, h]
    | rejected => simp [Model.outputWithin, h]
    | transient =>
      simp only [Model.outputWithin, h, measure_list_sum, Measure.smul_apply, smul_eq_mul, shift_mass]
      have bound := weighted_bound (model.edges i) (fun e => e.probability) (fun _ => 1)
        (fun e => model.outputWithin n e.target Set.univ)
        (model.nonnegative i) (by simp) (by simpa using fun e (_ : e ∈ model.edges i) => ih e.target)
      simpa [model.normalized] using bound

theorem outputWithin_cost_bounds (model : Model) (bounds : MomentBounds model) (n : Nat) (i : Fin model.size) :
    (∫⁻ x, firstCost x ∂model.outputWithin n i) ≤ ENNReal.ofReal (bounds.first i : ℝ) ∧
    (∫⁻ x, secondCost x ∂model.outputWithin n i) ≤ ENNReal.ofReal (bounds.second i : ℝ) := by
  have terminal (i : Fin model.size) (b : Rat) (h : model.kind i = .returned b) :
      firstCost (b : ℝ) ≤ ENNReal.ofReal (bounds.first i : ℝ) ∧
      secondCost (b : ℝ) ≤ ENNReal.ofReal (bounds.second i : ℝ) := by
    have a := bounds.first_bound i
    have bnd := bounds.second_bound i
    simp only [h] at a bnd
    constructor <;> apply ENNReal.ofReal_le_ofReal
    · exact_mod_cast a
    · exact_mod_cast bnd
  induction n generalizing i with
  | zero =>
    cases h : model.kind i with
    | transient => simp [Model.outputWithin, h]
    | rejected => simp [Model.outputWithin, h]
    | returned b => simpa [Model.outputWithin, h] using terminal i b h
  | succ n ih =>
    cases h : model.kind i with
    | rejected => simp [Model.outputWithin, h]
    | returned b => simpa [Model.outputWithin, h] using terminal i b h
    | transient =>
      simp only [Model.outputWithin, h, lintegral_list_sum, lintegral_smul_measure]
      constructor
      · apply le_trans (weighted_bound (model.edges i) (fun e => e.probability)
          (fun e => bounds.first e.target + |e.reward|) _ (model.nonnegative i)
          (fun e _ => add_nonneg (bounds.first_nonnegative _) (abs_nonneg _)) ?_)
        · apply ENNReal.ofReal_le_ofReal
          have hb := bounds.first_bound i
          rw [h] at hb
          exact_mod_cast hb
        · intro e _
          have := shift_first_bound (model.outputWithin n e.target) e.reward (bounds.first e.target)
            (outputWithin_mass_le_one model n _) (ih e.target).1
          simpa only [Rat.cast_add, Rat.cast_abs,
            ENNReal.ofReal_add (show 0 ≤ (bounds.first e.target : ℝ) by exact_mod_cast bounds.first_nonnegative e.target) (abs_nonneg (e.reward : ℝ))] using this
      · apply le_trans (weighted_bound (model.edges i) (fun e => e.probability)
          (fun e => bounds.second e.target + 2*|e.reward| * bounds.first e.target + e.reward^2) _
          (model.nonnegative i) (fun e _ => by
            have := bounds.first_nonnegative e.target
            have := bounds.second_nonnegative e.target
            positivity) ?_)
        · apply ENNReal.ofReal_le_ofReal
          have hb := bounds.second_bound i
          rw [h] at hb
          exact_mod_cast hb
        · intro e _
          have := shift_second_bound (model.outputWithin n e.target) e.reward
            (bounds.first e.target) (bounds.second e.target)
            (outputWithin_mass_le_one model n _) (ih e.target).1 (ih e.target).2
          have a : 0 ≤ (bounds.first e.target : ℝ) := by exact_mod_cast bounds.first_nonnegative e.target
          have b : 0 ≤ (bounds.second e.target : ℝ) := by exact_mod_cast bounds.second_nonnegative e.target
          simpa only [Rat.cast_add, Rat.cast_mul, Rat.cast_ofNat, Rat.cast_abs, Rat.cast_pow,
            ENNReal.ofReal_add (by positivity : 0 ≤ (bounds.second e.target : ℝ) +
              2*|(e.reward:ℝ)| * (bounds.first e.target:ℝ)) (sq_nonneg _),
            ENNReal.ofReal_add b (show 0 ≤ 2*|(e.reward:ℝ)| * (bounds.first e.target : ℝ) by positivity), ENNReal.ofReal_mul (by positivity : 0 ≤ 2*|(e.reward:ℝ)|)] using this

theorem outputAt_integrable (model : Model) (bounds : MomentBounds model) (i : Fin model.size) :
    Integrable (fun x : ℝ => x) (model.outputAt i) ∧
    Integrable (fun x : ℝ => x^2) (model.outputAt i) := by
  constructor
  · apply integrable_iSup_of_bound _ (outputWithin_mono model i) _ measurable_id
      (ENNReal.ofReal (bounds.first i : ℝ)) ENNReal.ofReal_lt_top
    intro n
    simpa only [Real.norm_eq_abs, firstCost, id_eq] using (outputWithin_cost_bounds model bounds n i).1
  · apply integrable_iSup_of_bound _ (outputWithin_mono model i) _ (by fun_prop)
      (ENNReal.ofReal (bounds.second i : ℝ)) ENNReal.ofReal_lt_top
    intro n
    simpa only [Real.norm_eq_abs, abs_sq, secondCost] using
      (outputWithin_cost_bounds model bounds n i).2

end Determinize.Proof.RewardModel
