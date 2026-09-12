import Determinize.Proof.FiniteModel.Contexts
import Determinize.Proof.Primitives.DiscreteRational

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
def FiniteLawMatches (op : Op) (kind : DistributionAction) (arguments : List Rat)
    (outcomes : List (Rat × Rat)) : Prop :=
  (∀ outcome ∈ outcomes, 0 ≤ outcome.1) ∧
  (outcomes.map Prod.fst).sum = 1 ∧
  reduce (primitiveExpr (kind, op) (arguments.map fun (q : Rat) => .real (q : ℝ))) =
    .sample (kind, op) (outcomeMeasure outcomes) .real

private theorem singleton_matches (op : Op) (kind : DistributionAction) (arguments : List Rat) (mean : Rat)
    (law : reduce (primitiveExpr (kind, op) (arguments.map fun (q : Rat) => .real (q : ℝ))) =
      .sample (kind, op) (Measure.dirac (mean : ℝ)) .real) :
    FiniteLawMatches op kind arguments [(1,mean)] := by
  refine ⟨by simp, by simp, ?_⟩
  simpa [outcomeMeasure] using law

theorem bernoulli_stochastic_matches {affinity : Affinity} (p : Rat) (nonnegative : 0 ≤ p) (bounded : p ≤ 1) :
    FiniteLawMatches .bernoulli (.sample affinity) [p] [(1-p,0),(p,1)] := by
  have h0 : (0 : ℝ) ≤ p := by exact_mod_cast nonnegative
  have h1 : (p : ℝ) ≤ 1 := by exact_mod_cast bounded
  refine ⟨?_, ?_, ?_⟩
  · simp [nonnegative, sub_nonneg.mpr bounded]
  · simp
  · simp [primitiveExpr, reduce, Expr.isValue, realValue?, bernoulliFiber, h0, h1, outcomeMeasure]

theorem bernoulli_mean_matches (p : Rat) (nonnegative : 0 ≤ p) (bounded : p ≤ 1) :
    FiniteLawMatches .bernoulli .mean [p] [(1,p)] := by
  apply singleton_matches
  have h0 : (0 : ℝ) ≤ p := by exact_mod_cast nonnegative
  have h1 : (p : ℝ) ≤ 1 := by exact_mod_cast bounded
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, bernoulliFiber, h0, h1]

theorem realListValue?_fold (p : List Rat) :
    realListValue? ((p.map fun q => .real (q : ℝ)).foldr Expr.cons .nil) = some (p.map Rat.cast) := by
  induction p <;> simp_all [realListValue?, realValue?]

theorem isValue_list_fold (p : List Rat) :
    ((p.map fun q => .real (q : ℝ)).foldr Expr.cons .nil).isValue = true := by
  induction p <;> simp_all [Expr.isValue]

theorem discrete_stochastic_matches (affinity : Affinity) (p : List Rat)
    (d : Spec.Paper.FiniteDistribution) (completed : d.probabilities = p ++ [1 - p.sum]) :
    FiniteLawMatches (.discrete p.length) (.sample affinity) p
      (d.probabilities.zipIdx.map fun (p,i) => (p, (i : Rat))) := by
  refine ⟨?_, ?_, ?_⟩
  · intro outcome member
    obtain ⟨entry, entryMember, rfl⟩ := List.mem_map.mp member
    exact d.nonnegative _ (List.fst_mem_of_mem_zipIdx entryMember)
  · simpa [List.map_map, Function.comp_def, List.zipIdx_map_fst] using d.total
  · simp only [primitiveExpr, reduce, isValue_list_fold, ↓reduceIte, realListValue?_fold,
      List.length_map, DiscreteLaws.completed_sample affinity p d completed]
    simp [Spec.Paper.FiniteDistribution.measure, outcomeMeasure, List.map_map, Function.comp_def]

theorem discrete_mean_matches (p : List Rat) (d : Spec.Paper.FiniteDistribution)
    (completed : d.probabilities = p ++ [1 - p.sum]) :
    FiniteLawMatches (.discrete p.length) .mean p [(1,d.mean)] := by
  apply singleton_matches
  simp only [primitiveExpr, reduce, isValue_list_fold, ↓reduceIte, realListValue?_fold,
    List.length_map, DiscreteLaws.completed_mean p d completed]

theorem remainderDistribution_probabilities (p : List Rat) (d : Spec.Paper.FiniteDistribution)
    (checked : Checking.remainderDistribution p = .ok d) :
    d.probabilities = p ++ [1 - p.sum] := by
  unfold Checking.remainderDistribution Checking.finiteDistribution at checked
  split at checked
  · split at checked
    · cases checked; rfl
    · cases checked
  · cases checked

theorem uniform_mean_matches (a b : Rat) (domain : a ≤ b) :
    FiniteLawMatches .uniform .mean [a,b] [(1,(a+b)/2)] := by
  apply singleton_matches
  have h : (a : ℝ) ≤ b := by exact_mod_cast domain
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, uniformFiber, h]

theorem gaussian_mean_matches (a v : Rat) (domain : 0 ≤ v) :
    FiniteLawMatches .gaussian .mean [a,v] [(1,a)] := by
  apply singleton_matches
  have h : (0 : ℝ) ≤ v := by exact_mod_cast domain
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, gaussianFiber, h]

theorem poisson_mean_matches (a : Rat) (domain : 0 ≤ a) :
    FiniteLawMatches .poisson .mean [a] [(1,a)] := by
  apply singleton_matches
  have h : (0 : ℝ) ≤ a := by exact_mod_cast domain
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, poissonFiber, h]

theorem exponential_mean_matches (a : Rat) (domain : 0 < a) :
    FiniteLawMatches .exponential .mean [a] [(1,1/a)] := by
  apply singleton_matches
  have h : (0 : ℝ) < a := by exact_mod_cast domain
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, exponentialFiber, h]

theorem beta_mean_matches (a b : Rat) (ha : 0 < a) (hb : 0 < b) :
    FiniteLawMatches .beta .mean [a,b] [(1,a/(a+b))] := by
  apply singleton_matches
  have h0 : (0 : ℝ) < a := by exact_mod_cast ha
  have h1 : (0 : ℝ) < b := by exact_mod_cast hb
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, betaFiber, h0, h1]

theorem gamma_mean_matches (a b : Rat) (ha : 0 < a) (hb : 0 < b) :
    FiniteLawMatches .gamma .mean [a,b] [(1,a/b)] := by
  apply singleton_matches
  have h0 : (0 : ℝ) < a := by exact_mod_cast ha
  have h1 : (0 : ℝ) < b := by exact_mod_cast hb
  simp [primitiveExpr, reduce, Expr.isValue, realValue?, gammaFiber, h0, h1]

/-- Successful executable finite laws preserve exact mass and the complete real
sampling measure. Unsupported or invalid calls cannot satisfy the premise. -/
theorem finiteLaw_sound (op : Op) (kind : DistributionAction) (arguments : List Rat)
    (outcomes : List (Rat × Rat)) (success : finiteLaw op kind arguments = .ok outcomes) :
    FiniteLawMatches op kind arguments outcomes := by
  cases op with
  | discrete n =>
      by_cases arity : arguments.length = n
      · cases checked : Checking.remainderDistribution arguments with
        | error message => simp [finiteLaw, arity, checked, Except.mapError, bind, Except.bind] at success
        | ok d =>
            have completed := remainderDistribution_probabilities arguments d checked
            subst n
            cases kind with
            | sample affinity =>
                simp [finiteLaw, checked, Except.mapError, bind, Except.bind, pure, Except.pure] at success
                subst outcomes
                exact discrete_stochastic_matches affinity arguments d completed
            | mean =>
                simp [finiteLaw, checked, Except.mapError, bind, Except.bind, pure, Except.pure] at success
                subst outcomes
                exact discrete_mean_matches arguments d completed
      · simp [finiteLaw, arity, bind, Except.bind, throw] at success
  | _ =>
    rcases arguments with _ | ⟨a, _ | ⟨b, _ | ⟨c, rest⟩⟩⟩ <;>
      cases kind <;>
      simp only [finiteLaw, supportedDraw] at success
    all_goals split_ifs at success <;> simp_all [pure, bind, Except.bind, Except.pure, throw]
    all_goals subst outcomes
    all_goals try { simpa only [one_div] using exponential_mean_matches a (by assumption) }
    all_goals aesop (add safe apply [bernoulli_stochastic_matches,
      bernoulli_mean_matches, uniform_mean_matches,
      gaussian_mean_matches, poisson_mean_matches, exponential_mean_matches,
      beta_mean_matches, gamma_mean_matches])

theorem finiteLaw_probability (op : Op) (kind : DistributionAction) (arguments : List Rat)
    (outcomes : List (Rat × Rat)) (success : finiteLaw op kind arguments = .ok outcomes) :
    IsProbabilityMeasure (outcomeMeasure outcomes) where
  measure_univ := by
    have law := finiteLaw_sound op kind arguments outcomes success
    rw [outcomeMeasure_mass outcomes law.1, law.2.1]
    simp

theorem primitiveExpr_notValue (site : DistributionAction × Op) (arguments : List Expr) :
    (primitiveExpr site arguments).isValue = false := by
  rcases site with ⟨kind, op⟩
  cases op <;>
    rcases arguments with _ | ⟨a, _ | ⟨b, _ | ⟨c, rest⟩⟩⟩ <;>
    rfl

theorem finiteLaw_stack (site : DistributionAction × Op) (arguments : List Rat)
    (outcomes : List (Rat × Rat)) (success : finiteLaw site.2 site.1 arguments = .ok outcomes)
    (stack : List Frame) (shape : ∀ frame ∈ stack, FrameShape frame) :
    reduce (stackExpr stack (primitiveExpr site (arguments.map fun (q : Rat) => .real (q : ℝ)))) =
      .sample site (outcomeMeasure outcomes) (fun x => stackExpr stack (.real x)) := by
  rw [(stack_context stack shape _ (primitiveExpr_notValue _ _)).2,
    (finiteLaw_sound _ _ _ _ success).2.2]
  rfl

theorem draw_step (site : DistributionAction × Op) (arguments : List Rat) (x : Rat)
    (environment : List Value) (stack : List Frame) (outcomes : List (Rat × Rat))
    (success : finiteLaw site.2 site.1 (arguments ++ [x]) = .ok outcomes) :
    step (.deliver (.number x) (.draw site [] environment arguments :: stack)) =
      .ok (.next (.sample site (arguments ++ [x]))
        (outcomes.map fun (p,y) => (p, .deliver (.number y) stack))) := by
  change (finiteLaw site.2 site.1 (arguments ++ [x]) >>= fun outcomes =>
    pure (Step.next (.sample site (arguments ++ [x]))
      (outcomes.map fun (p,y) => (p, .deliver (.number y) stack)))) = _
  rw [success]
  rfl

theorem draw_correspondence (site : DistributionAction × Op) (arguments : List Rat) (x : Rat)
    (environment : List Value) (stack : List Frame) (outcomes : List (Rat × Rat))
    (success : finiteLaw site.2 site.1 (arguments ++ [x]) = .ok outcomes)
    (shape : ∀ frame ∈ stack, FrameShape frame) :
    reduce (stateExpr (.deliver (.number x) (.draw site [] environment arguments :: stack))) =
      .sample site (outcomeMeasure outcomes) (fun y => stackExpr stack (.real y)) := by
  simpa only [stateExpr, stackExpr, List.foldl_cons, frameExpr, valueExpr,
    List.map_append, List.map_cons, List.map_nil] using
    finiteLaw_stack site (arguments ++ [x]) outcomes success stack shape

theorem valueExpr_probabilities (value : Value) (p : List Rat)
    (read : value.probabilities? = some p) :
    valueExpr value = (p.map fun q => .real (q : ℝ)).foldr Expr.cons .nil := by
  cases value with
  | cons head tail =>
      cases head <;> simp only [Value.probabilities?] at read <;> try contradiction
      cases tailEq : tail.probabilities? with
      | none => simp [tailEq] at read
      | some ps =>
          simp [tailEq] at read
          subst p
          simp [valueExpr, valueExpr_probabilities tail ps tailEq]
  | nil => cases read; simp [valueExpr]
  | _ => cases read
termination_by sizeOf value

theorem discrete_correspondence (kind : DistributionAction) (value : Value) (p : List Rat)
    (read : value.probabilities? = some p) (stack : List Frame) (outcomes : List (Rat × Rat))
    (success : finiteLaw (.discrete p.length) kind p = .ok outcomes)
    (shape : ∀ frame ∈ stack, FrameShape frame) :
    reduce (stateExpr (.deliver value (.discrete kind :: stack))) =
      .sample (kind, .discrete p.length) (outcomeMeasure outcomes)
        (fun y => stackExpr stack (.real y)) := by
  simpa only [stateExpr, stackExpr, List.foldl_cons, frameExpr,
    valueExpr_probabilities value p read, primitiveExpr] using
    finiteLaw_stack (kind, .discrete p.length) p outcomes success stack shape

end Determinize.Proof.FiniteModel
