import Determinize.Spec.Semantics
import Mathlib.MeasureTheory.Constructions.BorelSpace.Basic

/-!
# Generation traces

A trace records only stochastic generation-mode draws, in execution order.
Reduction depth is used to define termination, but is not part of a trace.
-/

namespace Determinize.Spec.Traces

open MeasureTheory Determinize.Spec.Paper

instance : MeasurableSpace Op := ⊤

abbrev Trace := List (Op × ℝ)
abbrev Output := Trace × ℝ

/-- Length and coordinates give the usual measurable structure on finite lists.
The padding value is irrelevant because length is recorded separately. -/
instance : MeasurableSpace Trace :=
  MeasurableSpace.comap (fun trace : Trace =>
    (trace.length, fun index : Nat => trace.getD index (.uniform, 0))) inferInstance

/-- Record a generation draw; all other actions preserve the suffix trace. -/
def record (site : Mode × Kind × Op) (value : ℝ) (output : Output) : Output :=
  match site with
  | (.G, .stochastic, op) => ((op, value) :: output.1, output.2)
  | _ => output

/-- Joint law of executions first returning a real at exactly `depth`
reduction steps. The recorded trace contains only the G draws among those steps. A rejected
execution has no output, so it contributes no trace either. -/
noncomputable def traceAndOutputLawAt : Nat → Expr → Measure Output
  | 0, .real value => Measure.dirac ([], value)
  | 0, _ => 0
  | depth + 1, expression =>
      if expression.isValue then 0
      else match reduce expression with
        | .next next => traceAndOutputLawAt depth next
        | .sample site fiber continuation => fiber.bind fun value =>
            (traceAndOutputLawAt depth (continuation value)).map (record site value)
        | .stuck => 0

/-- Joint law of terminating generation traces and returned reals. -/
noncomputable def traceAndOutputLaw (program : Expr) : Measure Output :=
  Measure.sum fun depth => traceAndOutputLawAt depth program

/-- Replay `program` for `depth` reduction steps with its general-mode draws read from `trace`
instead of sampled, still integrating every expectation-mode draw: the law of the real the
replay returns exactly at `depth`. The result is the zero measure when the trace does not fit
the program: a general-mode site that finds no entry or an entry of another primitive, or an
entry left over when the program returns. -/
noncomputable def outputGivenTraceAt : Nat → Expr → Trace → Measure ℝ
  | 0, .real value, [] => Measure.dirac value
  | 0, _, _ => 0
  | depth + 1, expression, trace =>
      if expression.isValue then 0
      else match reduce expression with
        | .next next => outputGivenTraceAt depth next trace
        | .sample site fiber continuation =>
            match site with
            | (.G, .stochastic, op) =>
                match trace with
                | (op', value) :: rest =>
                    if op = op' then outputGivenTraceAt depth (continuation value) rest else 0
                | [] => 0
            | _ => fiber.bind fun value => outputGivenTraceAt depth (continuation value) trace
        | .stuck => 0

/-- The law of the output of `program` given that its general-mode draws were `trace`: the
program replayed along the trace, at whichever depth it returns. -/
noncomputable def outputGivenTrace (program : Expr) (trace : Trace) : Measure ℝ :=
  Measure.sum fun depth => outputGivenTraceAt depth program trace

end Determinize.Spec.Traces
