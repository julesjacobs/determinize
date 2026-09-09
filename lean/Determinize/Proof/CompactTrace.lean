import Determinize.Proof.TraceSoundness
import Determinize.Traces.Main
import Mathlib.MeasureTheory.MeasurableSpace.Embedding

namespace Determinize.Proof.StepTraces
open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr
open SymbolicSoundness.TargetSafety
open scoped ProbabilityTheory
noncomputable section
open Classical

abbrev DrawTrace := Determinize.Traces.Trace

theorem draw_length_measurable : Measurable (List.length : DrawTrace → Nat) :=
  measurable_fst.comp (comap_measurable _)

theorem draw_event_measurable (i : Nat) :
    Measurable (fun trace : DrawTrace => trace.getD i (.uniform, 0)) := by
  have h : Measurable (fun trace : DrawTrace =>
      (trace.length, fun j : Nat => trace.getD j (.uniform, 0))) := comap_measurable _
  exact (measurable_pi_apply i).comp (measurable_snd.comp h)

theorem draw_cons_measurable :
    Measurable (fun p : (Op × ℝ) × DrawTrace => p.1 :: p.2) := by
  apply measurable_comap_iff.mpr
  refine Measurable.prodMk ((draw_length_measurable.comp measurable_snd).add_const 1) ?_
  apply measurable_pi_lambda
  intro i
  cases i with
  | zero => exact measurable_fst
  | succ i => exact (draw_event_measurable i).comp measurable_snd

theorem draw_tail_measurable : Measurable (List.tail : DrawTrace → DrawTrace) := by
  apply measurable_comap_iff.mpr
  refine Measurable.prodMk ?_ ?_
  · simpa using draw_length_measurable.sub_const 1
  · apply measurable_pi_lambda
    intro i
    simpa using draw_event_measurable (i+1)

def emitDraw (event : Event) (trace : DrawTrace) : DrawTrace :=
  match event with | none => trace | some draw => draw :: trace

theorem event_elim_measurable {α β : Type*} [MeasurableSpace α] [MeasurableSpace β]
    (empty : α → β) (draw : (Op × ℝ) × α → β)
    (he : Measurable empty) (hd : Measurable draw) :
    Measurable (fun p : Event × α => p.1.elim (empty p.2) (fun d => draw (d,p.2))) := by
  let f : (Sum (Op × ℝ) PUnit) × α → β := fun p =>
    match p.1 with | .inl d => draw (d,p.2) | .inr _ => empty p.2
  have hf : Measurable f := by
    convert (hd.sumElim (he.comp measurable_snd)).comp
      (MeasurableEquiv.sumProdDistrib (Op × ℝ) PUnit α).measurable using 1
    funext ⟨e,a⟩
    cases e <;> rfl
  have h := hf.comp ((comap_measurable (Equiv.optionEquivSumPUnit.{0} (Op × ℝ))).prodMap (measurable_id (α := α)))
  convert h using 1
  funext ⟨e,a⟩
  cases e <;> rfl

theorem emitDraw_measurable : Measurable (fun p : Event × DrawTrace => emitDraw p.1 p.2) := by
  convert event_elim_measurable id (fun p => p.1 :: p.2) measurable_id draw_cons_measurable using 1
  funext ⟨e,a⟩
  cases e <;> rfl

private noncomputable instance : Encodable Op := Encodable.ofCountable Op

def eventCode (event : Event) : Nat × ℝ :=
  event.elim (0,0) (fun p => (Encodable.encode p.1 + 1, p.2))

theorem eventCode_measurable : Measurable eventCode := by
  have h := event_elim_measurable (fun _ : Unit => ((0 : Nat),(0 : ℝ)))
    (fun p : (Op × ℝ) × Unit => (Encodable.encode p.1.1 + 1, p.1.2))
    measurable_const (((measurable_of_countable (fun op : Op => Encodable.encode op + 1)).comp
      (measurable_fst.comp measurable_fst)).prodMk (measurable_snd.comp measurable_fst))
  exact h.comp (measurable_id.prodMk (measurable_const (a := ())))

theorem eventCode_injective : Function.Injective eventCode := by
  intro a b h
  cases a with
  | none => cases b <;> simp_all [eventCode]
  | some a =>
      cases b with
      | none => simp_all [eventCode]
      | some b =>
          have hc := congrArg Prod.fst h
          have hv := congrArg Prod.snd h
          simp only [eventCode, Option.elim_some, Nat.add_left_inj] at hc hv
          exact congrArg some (Prod.ext (Encodable.encode_injective hc) hv)

def traceCode (trace : Trace) : Nat × (Nat → Nat × ℝ) :=
  (trace.length, fun i => eventCode (trace.getD i none))

theorem traceCode_measurable : Measurable traceCode :=
  trace_length_measurable.prodMk
    (measurable_pi_lambda _ fun i => eventCode_measurable.comp (trace_event_measurable i))

theorem traceCode_injective : Function.Injective traceCode := by
  intro a b h
  have hl := congrArg Prod.fst h
  apply List.ext_getElem hl
  intro i ha hb
  have hi := eventCode_injective (congrFun (congrArg Prod.snd h) i)
  simpa [List.getD, ha, hb] using hi

instance : MeasurableEq Trace := by
  constructor
  have h := measurableSet_eq_fun (traceCode_measurable.comp measurable_fst)
    (traceCode_measurable.comp measurable_snd)
  simpa only [Function.comp_def, traceCode_injective.eq_iff, Set.diagonal] using h

def retainWithin : Nat → Trace → DrawTrace
  | 0, _ => []
  | n+1, trace => emitDraw (trace.headD none) (retainWithin n trace.tail)

theorem retainWithin_measurable (n : Nat) : Measurable (retainWithin n) := by
  induction n with
  | zero => exact measurable_const
  | succ n ih =>
      exact emitDraw_measurable.comp
        (trace_head_measurable.prodMk (ih.comp trace_tail_measurable))

def retain (trace : Trace) : DrawTrace := trace.filterMap id

theorem retain_eq (trace : Trace) : retain trace = retainWithin trace.length trace := by
  induction trace with
  | nil => rfl
  | cons head tail ih => cases head <;> simp_all [retain, retainWithin, emitDraw]

theorem retain_measurable : Measurable retain := by
  rw [funext retain_eq]
  have h : Measurable (fun p : Nat × Trace => retainWithin p.1 p.2) :=
    measurable_from_prod_countable_right retainWithin_measurable
  exact h.comp (trace_length_measurable.prodMk measurable_id)

abbrev Decoded := Bool × Trace

def prependDecoded (event : Event) (result : Decoded) : Decoded :=
  (result.1, event :: result.2)

def decodeAction (recurse : Expr → DrawTrace → Decoded) : Action → DrawTrace → Decoded
  | .next next, tape => prependDecoded none (recurse next tape)
  | .sample site fiber continuation, tape =>
      match generationEvent site 0 with
      | none => prependDecoded none (recurse (continuation (∫ r : ℝ, r ∂fiber)) tape)
      | some (op, _) =>
          let value := (tape.getD 0 (.uniform, 0)).2
          prependDecoded (some (op,value)) (recurse (continuation value) tape.tail)
  | .stuck, _ => (false, [])

def decodeWithin : Nat → Expr → DrawTrace → Decoded
  | 0, e, _ => (e.isValue, [])
  | n+1, e, tape =>
      if e.isValue then (true, [])
      else decodeAction (decodeWithin n) (reduce e) tape

theorem prependDecoded_measurable :
    Measurable (fun p : Event × Decoded => prependDecoded p.1 p.2) :=
  (measurable_fst.comp measurable_snd).prodMk
    (trace_cons_measurable.comp (measurable_fst.prodMk (measurable_snd.comp measurable_snd)))

theorem decodeAction_measurable {α : Type*} [MeasurableSpace α]
    {action : α → Action} (family : MeasurableActionFamily α action)
    (rec : Expr → DrawTrace → Decoded)
    (hr : Measurable (fun p : Expr × DrawTrace => rec p.1 p.2)) :
    Measurable (fun p : α × DrawTrace => decodeAction rec (action p.1) p.2) := by
  classical
  induction family with
  | next hn => exact prependDecoded_measurable.comp (measurable_const.prodMk
      (hr.comp ((hn.comp measurable_fst).prodMk measurable_snd)))
  | @sample site draw cont hc =>
      let := draw.sfinite
      have hm : Measurable (fun a => ∫ r : ℝ, r ∂draw.kernel a) :=
        (stronglyMeasurable_id.integral_kernel (κ := draw.kernel)).measurable
      cases h : generationEvent site 0 with
      | none =>
          simpa only [decodeAction, h, Function.comp_def, id_eq] using prependDecoded_measurable.comp
            ((measurable_const (a := (none : Event))).prodMk
              (hr.comp (((hc.comp (measurable_id.prodMk hm)).comp measurable_fst).prodMk measurable_snd)))
      | some value =>
          have hv : Measurable (fun p : α × DrawTrace => (p.2.getD 0 (.uniform, 0)).2) :=
            (draw_event_measurable 0).snd.comp measurable_snd
          simpa only [decodeAction, h, Function.comp_def, id_eq] using prependDecoded_measurable.comp
            ((event_some_measurable.comp (measurable_const.prodMk hv)).prodMk
              (hr.comp ((hc.comp (measurable_fst.prodMk hv)).prodMk
                (draw_tail_measurable.comp measurable_snd))))
  | stuck => exact measurable_const
  | @piecewise region _ hm yes no hy hn ihy ihn =>
      convert ihy.piecewise (hm.preimage measurable_fst) ihn using 1
      funext p
      by_cases h : p.1 ∈ region <;> simp [Set.piecewise, h]
      all_goals infer_instance

theorem decodeStep_measurable (rec : Expr → DrawTrace → Decoded)
    (hr : Measurable (fun p : Expr × DrawTrace => rec p.1 p.2)) :
    Measurable (fun p : Expr × DrawTrace => decodeAction rec (reduce p.1) p.2) := by
  let : Countable Skeleton :=
    (show Function.Surjective SkeletonCode.decode from
      fun s => ⟨encodeSkeletonCode s, SkeletonCode.decode_encode s⟩).countable
  have localMeasurable (s : Skeleton) :
      Measurable (fun p : SkeletonFiber s × DrawTrace => decodeAction rec (reduce p.1.val) p.2) :=
    decodeAction_measurable
      (MeasurableActionFamily.measurable_reduce primitiveLaws (MeasurableFamily.skeletonFiber s)) rec hr
  have h : Measurable (fun p : Skeleton × (Expr × DrawTrace) =>
      decodeAction rec (reduce (MeasurableActionFamily.toSkeletonFiber p.1 p.2.1).val) p.2.2) := by
    apply measurable_from_prod_countable_right
    intro s
    exact (localMeasurable s).comp
      (((MeasurableActionFamily.measurable_toSkeletonFiber s).comp measurable_fst).prodMk measurable_snd)
  convert h.comp ((measurable_skeleton.comp measurable_fst).prodMk measurable_id) using 1
  funext p
  dsimp only [Function.comp_def, id_eq]
  rw [MeasurableActionFamily.toSkeletonFiber_coe_of_mem _ _ rfl]

theorem decodeWithin_measurable (n : Nat) :
    Measurable (fun p : Expr × DrawTrace => decodeWithin n p.1 p.2) := by
  classical
  let : Countable Skeleton :=
    (show Function.Surjective SkeletonCode.decode from
      fun s => ⟨encodeSkeletonCode s, SkeletonCode.decode_encode s⟩).countable
  have hv : Measurable (fun e : Expr => e.isValue) := by
    rw [show (fun e : Expr => e.isValue) = Expr.isValue ∘ Expr.skeleton from
      funext isValue_eq_skeletonIsValue]
    exact (measurable_of_countable _).comp measurable_skeleton
  induction n with
  | zero => exact (hv.comp measurable_fst).prodMk measurable_const
  | succ n ih =>
      exact measurable_const.piecewise
        (MeasurableActionFamily.valueSet_measurable.preimage measurable_fst)
        (decodeStep_measurable _ ih)

theorem decodeWithin_stable (n m : Nat) (e : Expr) (tape : DrawTrace)
    (le : n ≤ m) (done : (decodeWithin n e tape).1 = true) :
    decodeWithin m e tape = decodeWithin n e tape := by
  induction n generalizing m e tape with
  | zero =>
      change e.isValue = true at done
      cases m <;> simp [decodeWithin, done]
  | succ n ih =>
      cases m with
      | zero => omega
      | succ m =>
          have le' : n ≤ m := Nat.le_of_succ_le_succ le
          by_cases value : e.isValue = true
          · simp [decodeWithin, value]
          · simp only [decodeWithin, value] at done ⊢
            cases h : reduce e with
            | next next =>
                simp only [h, decodeAction, prependDecoded] at done ⊢
                rw [ih m next tape le' done]
            | stuck => simp [h, decodeAction] at done
            | sample site fiber cont =>
                simp only [h, decodeAction] at done ⊢
                cases ev : generationEvent site 0 with
                | none =>
                    simp only [ev, prependDecoded] at done ⊢
                    rw [ih m _ tape le' done]
                | some pair =>
                    rcases pair with ⟨op,r⟩
                    simp only [ev, prependDecoded] at done ⊢
                    rw [ih m _ tape.tail le' done]

def decode (e : Expr) (tape : DrawTrace) : Trace :=
  if h : ∃ n, (decodeWithin n e tape).1 = true then
    (decodeWithin (Nat.find h) e tape).2
  else []

theorem decode_eq (n : Nat) (e : Expr) (tape : DrawTrace)
    (done : (decodeWithin n e tape).1 = true) : decode e tape = (decodeWithin n e tape).2 := by
  have h : ∃ n, (decodeWithin n e tape).1 = true := ⟨n,done⟩
  rw [decode, dif_pos h]
  exact congrArg Prod.snd (decodeWithin_stable (Nat.find h) n e tape
    (Nat.find_min' h done) (Nat.find_spec h)).symm

theorem decode_measurable (e : Expr) : Measurable (decode e) := by
  classical
  let done : Nat → DrawTrace → Prop := fun n t => (decodeWithin n e t).1 = true
  have hm (n : Nat) : MeasurableSet {t | done n t} :=
    measurableSet_eq_fun ((decodeWithin_measurable n).comp (measurable_const.prodMk measurable_id)).fst measurable_const
  have hs : MeasurableSet {t | ∃ n, done n t} := by simpa only [Set.ofPred_exists] using MeasurableSet.iUnion hm
  let region := {t | ∃ n, done n t}
  have hf : Measurable (fun t : region =>
      (decodeWithin (Nat.find t.property) e t.val).2) := by
    apply Measurable.find (f := fun n (t : region) => (decodeWithin n e t.val).2)
      (p := fun n (t : region) => done n t.val) _ _ (fun t => t.property)
    · intro n
      exact ((decodeWithin_measurable n).comp
        (measurable_const.prodMk measurable_subtype_coe)).snd
    · intro n
      exact (hm n).preimage measurable_subtype_coe
  exact hf.dite measurable_const hs

theorem decodeCorrect_measurable (depth : Nat) (e : Expr) :
    MeasurableSet {p : Output | decodeWithin depth e (retain p.1) = (true, p.1)} := by
  have hm : Measurable (fun p : Output => decodeWithin depth e (retain p.1)) :=
    (decodeWithin_measurable depth).comp
      ((measurable_const (a := e)).prodMk (retain_measurable.comp measurable_fst))
  have h := (measurableSet_eq_fun hm.fst (measurable_const (a := true))).inter
    (measurableSet_eq_fun hm.snd measurable_fst)
  simpa only [Prod.ext_iff, Set.ofPred_and] using h

set_option maxHeartbeats 1600000 in
theorem target_decodeWithin (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (safe : SafeConfigAt primitiveLaws depth history expression) :
    ∀ᵐ p ∂targetTraceLaw depth history expression,
      decodeWithin depth (expression.realize (history.meanEnvironment primitiveLaws)).determinize
        (retain p.1) = (true, p.1) := by
  induction depth generalizing n history expression with
  | zero =>
      by_cases value : expression.isValue = true
      · obtain ⟨a,rfl⟩ := wellTyped_real_value safe.2.1 value
        simp [targetTraceLaw, AffineExpr.realize, Expr.determinize, exactMeasure,
          decodeWithin, Expr.isValue]
      · have nv : (expression.realize (history.meanEnvironment primitiveLaws)).determinize.isValue ≠ true := by
          simpa only [determinize_isValue, AffineExpr.realize_isValue] using value
        unfold targetTraceLaw
        generalize he : (expression.realize (history.meanEnvironment primitiveLaws)).determinize = e at nv ⊢
        cases e <;> try simp [exactMeasure]
        case real r => simp_all [Expr.isValue]
  | succ depth ih =>
      rcases safe with ⟨historySafe,typed,sourceSafe⟩
      by_cases value : expression.isValue = true
      · simp [targetTraceLaw, exactMeasure, determinize_isValue, AffineExpr.realize_isValue, value]
      · have nv : (expression.realize (history.meanEnvironment primitiveLaws)).determinize.isValue ≠ true := by
          simpa only [determinize_isValue, AffineExpr.realize_isValue] using value
        have actionTyped := symbolicReduce_wellTyped primitiveLaws typed
        generalize actionEq : symbolicReduce primitiveLaws expression = action at actionTyped
        cases action with
        | next next =>
            have nextTyped := SymbolicAction.wellTyped_next_iff.mp actionTyped
            have nextSafe : ∀ᵐ env ∂history.actualMeasure primitiveLaws,
                PrimitiveDomainSafeAt depth (next.realize env) := by
              filter_upwards [sourceSafe] with env valid
              have reduction : reduce (expression.realize env) = .next (next.realize env) := by
                rw [← symbolicReduce_realize primitiveLaws typed env, actionEq]
                rfl
              have nv' : (expression.realize env).isValue ≠ true := by
                simpa only [AffineExpr.realize_isValue] using value
              simpa only [PrimitiveDomainSafeAt, Bool.eq_false_of_not_eq_true nv', Bool.false_eq_true,
                ↓reduceIte, reduction] using valid
            have reduction : reduce (expression.realize (history.meanEnvironment primitiveLaws)).determinize =
                .next (next.realize (history.meanEnvironment primitiveLaws)).determinize := by
              rw [← symbolicReduce_targetRealize primitiveLaws typed, actionEq]
              rfl
            rw [targetTraceLaw_next _ _ _ _ typed value actionEq,
              ae_map_iff (show Measurable (prepend none) from
                prepend_measurable.comp (measurable_const.prodMk measurable_id)).aemeasurable
                (decodeCorrect_measurable _ _)]
            filter_upwards [ih history next ⟨historySafe,nextTyped,nextSafe⟩] with p hp
            simpa [decodeWithin, nv, reduction, decodeAction, prepend, retain, prependDecoded] using
              congrArg (prependDecoded none) hp
        | sampleE op affine general continuation =>
            let extended := Symbolic.SampleEnv.snoc history op
              (fun i => affine.getD i.1 (0,fun _ => 0)) (fun i => general.getD i.1 0)
            have extension := source_sampleE_safe_extension (MeasurableActionFamily.stepKernel primitiveLaws)
              depth history historySafe expression typed sourceSafe op affine general continuation actionEq
            have extendedSafe : extended.DomainSafe primitiveLaws := extension.1
            have domainMean := SymbolicSoundness.SampleEnv.domain_at_meanEnvironment primitiveLaws history
              historySafe op (fun i => affine.getD i.1 (0,fun _ => 0))
                (fun i => general.getD i.1 0) extendedSafe.2
            have reduction := concrete_target_sampleE primitiveLaws expression typed op affine general
              continuation actionEq (history.meanEnvironment primitiveLaws) domainMean
            rw [targetTraceLaw_sampleE _ _ historySafe _ typed value _ _ _ _ actionEq extendedSafe,
              ae_map_iff (show Measurable (prepend none) from
                prepend_measurable.comp (measurable_const.prodMk measurable_id)).aemeasurable
                (decodeCorrect_measurable _ _)]
            filter_upwards [ih extended continuation extension] with p hp
            simpa [decodeWithin, nv, reduction, decodeAction, generationEvent, prepend,
              retain, prependDecoded, extended, Symbolic.SampleEnv.meanEnvironment] using
              congrArg (prependDecoded none) hp
        | sampleG site fiber continuation =>
            rcases source_sampleG_safe_swap (MeasurableActionFamily.stepKernel primitiveLaws) depth history
              historySafe expression typed sourceSafe fiber continuation actionEq with ⟨_,continuationSafe⟩
            obtain ⟨op,opEq⟩ := sampleG_opSome typed actionEq
            have reduction : reduce (expression.realize (history.meanEnvironment primitiveLaws)).determinize =
                .sample site fiber (fun r => ((continuation r).realize (history.meanEnvironment primitiveLaws)).determinize) := by
              rw [← symbolicReduce_targetRealize primitiveLaws typed, actionEq]
              rfl
            have siteEq : siteOp site = some op := by
              rw [← reduce_site reduction, generationOp_determinize, generationOp_realize, opEq]
            have eventEq : generationEvent site 0 = some (op,0) := by
              rw [generationEvent_eq_entry, siteEq]
              rfl
            rw [targetTraceLaw_sampleG _ _ _ typed value _ _ actionEq op opEq,
              Measure.ae_comp_iff (decodeCorrect_measurable _ _)]
            filter_upwards [continuationSafe] with r valid
            rw [generatedTargetKernel_apply _ _ _ typed actionEq,
              ae_map_iff (show Measurable (prepend (entry (some op) r)) from
                prepend_measurable.comp (measurable_const.prodMk measurable_id)).aemeasurable
                (decodeCorrect_measurable _ _)]
            filter_upwards [ih history (continuation r)
              ⟨historySafe,SymbolicAction.wellTyped_sampleG_iff.mp actionTyped r,valid⟩] with p hp
            simpa [decodeWithin, nv, reduction, decodeAction, eventEq, prepend, retain, entry,
              prependDecoded] using congrArg (prependDecoded (some (op,r))) hp
        | stuck => exact (SymbolicAction.not_wellTyped_stuck actionTyped).elim

theorem target_decode (source : Expr) (typed : Typed [] source (.float .E))
    (tags : (AffineExpr.ofExpr source).SourceTags) (safe : PrimitiveDomainSafe source) :
    ∀ᵐ p ∂jointMeasure source.determinize, decode source.determinize (retain p.1) = p.1 := by
  rw [jointMeasure, Measure.ae_sum_iff]
  intro depth
  have config : SafeConfigAt primitiveLaws depth .nil (AffineExpr.ofExpr source) := by
    refine ⟨trivial, AffineExpr.wellTyped_ofExpr_of_typed typed tags, ?_⟩
    rw [Symbolic.SampleEnv.actualMeasure, ae_dirac_eq]
    simpa only [Filter.eventually_pure, AffineExpr.realize_ofExpr] using safe depth
  have h := target_decodeWithin depth .nil (AffineExpr.ofExpr source) config
  simp only [targetTraceLaw, Symbolic.SampleEnv.meanEnvironment, AffineExpr.realize_ofExpr] at h
  filter_upwards [h] with p hp
  rw [decode_eq depth source.determinize (retain p.1) (congrArg Prod.fst hp)]
  exact congrArg Prod.snd hp

end
end Determinize.Proof.StepTraces
