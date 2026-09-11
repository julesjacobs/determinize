import Determinize.Proof.FiniteModel.Contexts
import Determinize.Proof.FiniteDistributionMeasure

namespace Determinize.Proof.FiniteModel
open Spec.Paper Spec.FiniteModel Determinize.Finite MeasureTheory

noncomputable def outcomeMeasure (outcomes : List (Rat × Rat)) : Measure ℝ :=
  (outcomes.map fun (p,x) => ENNReal.ofReal (p : ℝ) • Measure.dirac (x : ℝ)).sum

theorem outcomeMeasure_mass (outcomes : List (Rat × Rat))
    (nonnegative : ∀ outcome ∈ outcomes, 0 ≤ outcome.1) :
    outcomeMeasure outcomes Set.univ = ENNReal.ofReal ((outcomes.map Prod.fst).sum : ℝ) := by
  induction outcomes with
  | nil => simp [outcomeMeasure]
  | cons entry rest ih =>
      have hp : 0 ≤ (entry.1 : ℝ) := by exact_mod_cast nonnegative entry (by simp)
      have hr : ∀ q ∈ rest, 0 ≤ q.1 := fun q hq => nonnegative q (by simp [hq])
      have hs : 0 ≤ ((rest.map Prod.fst).sum : ℝ) := by
        exact_mod_cast List.sum_nonneg (by simpa using hr)
      simp only [outcomeMeasure, List.map_cons, List.sum_cons, Measure.add_apply,
        Measure.smul_apply, Measure.dirac_apply_of_mem (Set.mem_univ _), smul_eq_mul, mul_one]
      simp only [outcomeMeasure] at ih
      rw [ih hr, Rat.cast_add, ENNReal.ofReal_add hp hs]

/-- Exact probabilities and the entire paper sampling law, not just its mean. -/
def FiniteLawMatches (op : Op) (kind : Kind) (arguments : List Rat)
    (outcomes : List (Rat × Rat)) : Prop :=
  (∀ outcome ∈ outcomes, 0 ≤ outcome.1) ∧
  (outcomes.map Prod.fst).sum = 1 ∧
  ∀ mode, reduce (primitiveExpr (mode,kind,op) (arguments.map fun (q : Rat) => .real (q : ℝ))) =
    .sample (mode,kind,op) (outcomeMeasure outcomes) .real

private theorem singleton_matches (op : Op) (kind : Kind) (arguments : List Rat) (mean : Rat)
    (law : ∀ mode, reduce (primitiveExpr (mode,kind,op) (arguments.map fun (q : Rat) => .real (q : ℝ))) =
      .sample (mode,kind,op) (Measure.dirac (mean : ℝ)) .real) :
    FiniteLawMatches op kind arguments [(1,mean)] := by
  refine ⟨by simp, by simp, ?_⟩
  simpa [outcomeMeasure] using law

theorem bernoulli_stochastic_matches (p : Rat) (nonnegative : 0 ≤ p) (bounded : p ≤ 1) :
    FiniteLawMatches .bernoulli .stochastic [p] [(1-p,0),(p,1)] := by
  have h0 : (0 : ℝ) ≤ p := by exact_mod_cast nonnegative
  have h1 : (p : ℝ) ≤ 1 := by exact_mod_cast bounded
  refine ⟨?_, ?_, ?_⟩
  · simp [nonnegative, sub_nonneg.mpr bounded]
  · simp
  · intro mode
    simp [primitiveExpr, reduce, Expr.isValue, realValue?, bernoulliFiber, h0, h1, outcomeMeasure]

theorem discrete_stochastic_matches (d : Spec.Paper.FiniteDistribution) :
    FiniteLawMatches (.discrete d) .stochastic []
      (d.probabilities.zipIdx.map fun (p,i) => (p, (i : Rat))) := by
  refine ⟨?_, ?_, ?_⟩
  · intro outcome member
    obtain ⟨entry, entryMember, rfl⟩ := List.mem_map.mp member
    exact d.nonnegative _ (List.fst_mem_of_mem_zipIdx entryMember)
  · simpa [List.map_map, Function.comp_def, List.zipIdx_map_fst] using d.total
  · intro mode
    simp [primitiveExpr, reduce, discreteFiber, Spec.Paper.FiniteDistribution.measure,
      outcomeMeasure, List.map_map, Function.comp_def]

theorem bernoulli_mean_matches (p : Rat) (nonnegative : 0 ≤ p) (bounded : p ≤ 1) :
    FiniteLawMatches .bernoulli .mean [p] [(1,p)] := by
  apply singleton_matches
  have h0 : (0 : ℝ) ≤ p := by exact_mod_cast nonnegative
  have h1 : (p : ℝ) ≤ 1 := by exact_mod_cast bounded
  intro mode
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, bernoulliFiber, h0, h1]

theorem discrete_mean_matches (d : Spec.Paper.FiniteDistribution) :
    FiniteLawMatches (.discrete d) .mean [] [(1,d.mean)] := by
  apply singleton_matches
  intro mode
  simp [primitiveExpr, reduce, discreteFiber]

theorem uniform_mean_matches (a b : Rat) (domain : a ≤ b) :
    FiniteLawMatches .uniform .mean [a,b] [(1,(a+b)/2)] := by
  apply singleton_matches
  have h : (a : ℝ) ≤ b := by exact_mod_cast domain
  intro mode
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, uniformFiber, h]

theorem gaussian_mean_matches (a v : Rat) (domain : 0 ≤ v) :
    FiniteLawMatches .gaussian .mean [a,v] [(1,a)] := by
  apply singleton_matches
  have h : (0 : ℝ) ≤ v := by exact_mod_cast domain
  intro mode
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, gaussianFiber, h]

theorem poisson_mean_matches (a : Rat) (domain : 0 ≤ a) :
    FiniteLawMatches .poisson .mean [a] [(1,a)] := by
  apply singleton_matches
  have h : (0 : ℝ) ≤ a := by exact_mod_cast domain
  intro mode
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, poissonFiber, h]

theorem exponential_mean_matches (a : Rat) (domain : 0 < a) :
    FiniteLawMatches .exponential .mean [a] [(1,1/a)] := by
  apply singleton_matches
  have h : (0 : ℝ) < a := by exact_mod_cast domain
  intro mode
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, exponentialFiber, h]

theorem beta_mean_matches (a b : Rat) (ha : 0 < a) (hb : 0 < b) :
    FiniteLawMatches .beta .mean [a,b] [(1,a/(a+b))] := by
  apply singleton_matches
  have h0 : (0 : ℝ) < a := by exact_mod_cast ha
  have h1 : (0 : ℝ) < b := by exact_mod_cast hb
  intro mode
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, betaFiber, h0, h1]

theorem gamma_mean_matches (a b : Rat) (ha : 0 < a) (hb : 0 < b) :
    FiniteLawMatches .gamma .mean [a,b] [(1,a/b)] := by
  apply singleton_matches
  have h0 : (0 : ℝ) < a := by exact_mod_cast ha
  have h1 : (0 : ℝ) < b := by exact_mod_cast hb
  intro mode
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, gammaFiber, h0, h1]

/-- Successful executable finite laws preserve exact mass and the complete real
sampling measure. Unsupported or invalid calls cannot satisfy the premise. -/
theorem finiteLaw_sound (op : Op) (kind : Kind) (arguments : List Rat)
    (outcomes : List (Rat × Rat)) (success : finiteLaw op kind arguments = .ok outcomes) :
    FiniteLawMatches op kind arguments outcomes := by
  cases op <;>
    rcases arguments with _ | ⟨a, _ | ⟨b, _ | ⟨c, rest⟩⟩⟩ <;>
    cases kind <;>
    simp only [finiteLaw, supportedDraw] at success
  all_goals split_ifs at success <;> simp_all [pure, bind, Except.bind, Except.pure, throw]
  all_goals subst outcomes
  all_goals try { simpa only [one_div] using exponential_mean_matches a (by assumption) }
  all_goals aesop (add safe apply [bernoulli_stochastic_matches, discrete_stochastic_matches,
    bernoulli_mean_matches, discrete_mean_matches, uniform_mean_matches,
    gaussian_mean_matches, poisson_mean_matches, exponential_mean_matches,
    beta_mean_matches, gamma_mean_matches])

theorem finiteLaw_probability (op : Op) (kind : Kind) (arguments : List Rat)
    (outcomes : List (Rat × Rat)) (success : finiteLaw op kind arguments = .ok outcomes) :
    IsProbabilityMeasure (outcomeMeasure outcomes) where
  measure_univ := by
    have law := finiteLaw_sound op kind arguments outcomes success
    rw [outcomeMeasure_mass outcomes law.1, law.2.1]
    simp

theorem primitiveExpr_notValue (site : Mode × Kind × Op) (arguments : List Expr) :
    (primitiveExpr site arguments).isValue = false := by
  rcases site with ⟨mode, kind, op⟩
  cases op <;>
    rcases arguments with _ | ⟨a, _ | ⟨b, _ | ⟨c, rest⟩⟩⟩ <;>
    rfl

theorem finiteLaw_stack (site : Mode × Kind × Op) (arguments : List Rat)
    (outcomes : List (Rat × Rat)) (success : finiteLaw site.2.2 site.2.1 arguments = .ok outcomes)
    (stack : List Frame) (shape : ∀ frame ∈ stack, FrameShape frame) :
    reduce (stackExpr stack (primitiveExpr site (arguments.map fun (q : Rat) => .real (q : ℝ)))) =
      .sample site (outcomeMeasure outcomes) (fun x => stackExpr stack (.real x)) := by
  rw [(stack_context stack shape _ (primitiveExpr_notValue _ _)).2,
    (finiteLaw_sound _ _ _ _ success).2.2 site.1]
  rfl

theorem draw_step (site : Mode × Kind × Op) (arguments : List Rat) (x : Rat)
    (environment : List Value) (stack : List Frame) (outcomes : List (Rat × Rat))
    (success : finiteLaw site.2.2 site.2.1 (arguments ++ [x]) = .ok outcomes) :
    step (.deliver (.number x) (.draw site [] environment arguments :: stack)) =
      .ok (.next (.sample site (arguments ++ [x]))
        (outcomes.map fun (p,y) => (p, .deliver (.number y) stack))) := by
  change (finiteLaw site.2.2 site.2.1 (arguments ++ [x]) >>= fun outcomes =>
    pure (Step.next (.sample site (arguments ++ [x]))
      (outcomes.map fun (p,y) => (p, .deliver (.number y) stack)))) = _
  rw [success]
  rfl

theorem draw_correspondence (site : Mode × Kind × Op) (arguments : List Rat) (x : Rat)
    (environment : List Value) (stack : List Frame) (outcomes : List (Rat × Rat))
    (success : finiteLaw site.2.2 site.2.1 (arguments ++ [x]) = .ok outcomes)
    (shape : ∀ frame ∈ stack, FrameShape frame) :
    reduce (stateExpr (.deliver (.number x) (.draw site [] environment arguments :: stack))) =
      .sample site (outcomeMeasure outcomes) (fun y => stackExpr stack (.real y)) := by
  simpa only [stateExpr, stackExpr, List.foldl_cons, frameExpr, valueExpr,
    List.map_append, List.map_cons, List.map_nil] using
    finiteLaw_stack site (arguments ++ [x]) outcomes success stack shape

theorem discrete_step (mode : Mode) (kind : Kind) (d : FiniteDistribution)
    (environment : List Value) (stack : List Frame) (outcomes : List (Rat × Rat))
    (success : finiteLaw (.discrete d) kind [] = .ok outcomes) :
    step (.eval (.discrete mode kind d) environment stack) =
      .ok (.next (.sample (mode,kind,.discrete d) [])
        (outcomes.map fun (p,y) => (p, .deliver (.number y) stack))) := by
  change (finiteLaw (.discrete d) kind [] >>= fun outcomes =>
    pure (Step.next (.sample (mode,kind,.discrete d) [])
      (outcomes.map fun (p,y) => (p, .deliver (.number y) stack)))) = _
  rw [success]
  rfl

theorem discrete_correspondence (mode : Mode) (kind : Kind) (d : FiniteDistribution)
    (environment : List Value) (stack : List Frame) (outcomes : List (Rat × Rat))
    (success : finiteLaw (.discrete d) kind [] = .ok outcomes)
    (shape : ∀ frame ∈ stack, FrameShape frame) :
    reduce (stateExpr (.eval (.discrete mode kind d) environment stack)) =
      .sample (mode,kind,.discrete d) (outcomeMeasure outcomes)
        (fun y => stackExpr stack (.real y)) := by
  simpa [stateExpr, Binding.close, Checking.interpret, Expr.mapLiteral, Expr.mapVars,
    primitiveExpr] using finiteLaw_stack (mode,kind,.discrete d) [] outcomes success stack shape

end Determinize.Proof.FiniteModel
