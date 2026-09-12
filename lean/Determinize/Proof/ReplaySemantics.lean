import Determinize.Spec.Traces.Main

namespace Determinize.Proof.Traces

open MeasureTheory Determinize.Spec.Paper Determinize.Spec.Traces

/-- Replay `program` for `depth` reduction steps with its G draws read from `trace`
instead of sampled, still integrating every E draw: the law of the real the
replay returns exactly at `depth`. The result is the zero measure when the trace does not fit
the program: a G site that finds no entry or an entry of another primitive, or an
entry left over when the program returns. For `discrete`, the comparison `op = op'`
checks the entire stored distribution, including its number of outcomes and probabilities. -/
noncomputable def outputGivenTraceAt : Nat → Expr → Trace → Measure ℝ
  | 0, .real value, [] => Measure.dirac value
  | 0, _, _ => 0
  | depth + 1, expression, trace =>
      if expression.isValue then 0
      else match reduce expression with
        | .next next => outputGivenTraceAt depth next trace
        | .sample site fiber continuation =>
            match site with
            | (.sample .G, op) =>
                match trace with
                | (op', value) :: rest =>
                    if op = op' then outputGivenTraceAt depth (continuation value) rest else 0
                | [] => 0
            | _ => fiber.bind fun value => outputGivenTraceAt depth (continuation value) trace
        | .stuck => 0

/-- The law of the output of `program` given that its G draws were `trace`: the
program replayed along the trace, at whichever depth it returns. -/
noncomputable def outputGivenTrace (program : Expr) (trace : Trace) : Measure ℝ :=
  Measure.sum fun depth => outputGivenTraceAt depth program trace

/-- Draw a trace from `traces`, then an output from `outputs trace`: the joint law of the pair. -/
noncomputable def traceThenOutput (traces : Measure Trace) (outputs : Trace → Measure ℝ) :
    Measure Output :=
  traces.bind fun trace => (outputs trace).map fun value => (trace, value)

/-- The mean of the source replay law at a fixed generation trace. -/
noncomputable def replayMean (program : Expr) (trace : Trace) : ℝ :=
  ∫ value : ℝ, value ∂outputGivenTrace program trace

end Determinize.Proof.Traces
