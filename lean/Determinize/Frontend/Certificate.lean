import Determinize.Frontend.Compile
import Determinize.Frontend.Pretty

namespace Determinize.Frontend
open Checking Spec.Paper

/-- The exported file recomputes the checks in Lean's kernel using `decide +kernel`. -/
def certificateText (text : String) : Except String String := do
  let input ← elaborate (← parse text)
  let (source, certificate) ← infer input
  let some _ := certify input.expression source input.affinities certificate
    | throw "invalid certificate"
  let theoremText := match certificate.ty with
    | .float m =>
      "\ntheorem traceGuarantee (safe : PrimitiveDomainSafe (interpret checked.source)) :\n" ++
      "    PrimitiveDomainSafe (interpret target) ∧\n" ++
      "      traceAndOutputLaw (interpret checked.source) =\n" ++
      "        traceThenOutput (traceLaw (interpret checked.source)) (outputGivenTrace (interpret checked.source)) ∧\n" ++
      "      traceAndOutputLaw (interpret target) =\n" ++
      "        traceThenOutput (traceLaw (interpret checked.source)) (outputGivenTrace (interpret target)) ∧\n" ++
      "      ∀ᵐ trace ∂traceLaw (interpret checked.source),\n" ++
      "        MeasureTheory.Integrable id (outputGivenTrace (interpret checked.source) trace) ∧\n" ++
      "        outputGivenTrace (interpret target) trace =\n" ++
      "          MeasureTheory.Measure.dirac (∫ value : ℝ, value ∂outputGivenTrace (interpret checked.source) trace) :=\n" ++
      s!"  Determinize.Proof.Checking.certified_trace_soundness checked .{prettyAffinity m} (by decide +kernel) safe\n"
    | _ => ""
  return "import Determinize.Proof.Checking.Elaboration\n\n" ++
    "open Determinize.Checking Determinize.Spec.Paper Determinize.Spec.Traces\n\n" ++
    "set_option maxRecDepth 100000\nset_option maxHeartbeats 0\n\n" ++
    s!"def original : Core :=\n  {leanExpression input.expression}\n\n" ++
    s!"def requested : List (Option Affinity) :=\n  {reprStr input.affinities}\n\n" ++
    s!"def annotated : Core :=\n  {leanExpression source}\n\n" ++
    s!"def certificate : Certificate :=\n  {reprStr certificate}\n\n" ++
    "def checked : Certified original requested :=\n" ++
    "  (certify original annotated requested certificate).get (by decide +kernel)\n\n" ++
    "def target : Core := checked.source.determinize\n\n" ++
    "theorem validTyping : Typed [] (interpret checked.source) checked.ty := checked.typed\n\n" ++
    "theorem sameProgram : eraseAnnotations checked.source = eraseAnnotations original := checked.aligned\n" ++
    theoremText ++ "\n#print axioms validTyping\n#print axioms sameProgram\n" ++
    (match certificate.ty with | .float _ => "#print axioms traceGuarantee\n" | _ => "")

end Determinize.Frontend
