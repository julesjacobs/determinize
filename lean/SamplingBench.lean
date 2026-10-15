import Determinize.Frontend.Compile
import Determinize.Runtime.Eval

open Lean Determinize Determinize.Frontend
open Determinize.Spec.Paper (Core)

private structure Summary where
  returned : Nat := 0
  rejected : Nat := 0
  failed : Nat := 0
  mean : Float := 0
  m2 : Float := 0
  firstFailure : String := ""

private def sample (program : Core) (samples : Nat) (seed : UInt64) (fuel : Nat) :
    IO Summary := do
  let mut result : Summary := {}
  for i in [:samples] do
    match Runtime.runOutcome program (seed + UInt64.ofNat i) fuel with
    | .error message =>
      result := {result with
        failed := result.failed + 1
        firstFailure := if result.firstFailure.isEmpty then message else result.firstFailure}
    | .ok .rejected => result := {result with rejected := result.rejected + 1}
    | .ok (.returned (.number x) _) =>
      if x.isNaN || x.isInf then
        result := {result with
          failed := result.failed + 1
          firstFailure := "non-finite returned value"}
      else
        let count := result.returned + 1
        let delta := x - result.mean
        let mean := result.mean + delta / Float.ofNat count
        result := {result with
          returned := count
          mean
          m2 := result.m2 + delta * (x - mean)}
    | .ok (.returned _ _) =>
      result := {result with
        failed := result.failed + 1
        firstFailure := "non-numeric returned value"}
  return result

private def natural (value : String) : IO Nat :=
  match value.toNat? with
  | some n => pure n
  | none => throw (IO.userError s!"expected a natural number, got {value}")

def main (args : List String) : IO UInt32 := do
  try
    let [file, subject, samplesText, seedText, fuelText, warmupText] := args
      | throw (IO.userError
          "Usage: sampling-bench FILE source|determinized SAMPLES SEED FUEL WARMUP")
    let samples ← natural samplesText
    let seed ← natural seedText
    let fuel ← natural fuelText
    let warmup ← natural warmupText
    unless samples > 0 && seed + samples ≤ 2 ^ 64 do
      throw (IO.userError "samples must be positive and seeds must fit in 64 bits")
    let compiled ← IO.ofExcept (compile (← IO.FS.readFile file))
    let program ← match subject with
      | "source" => pure compiled.source
      | "determinized" => pure compiled.source.determinize
      | _ => throw (IO.userError "subject must be source or determinized")
    let warmed ← sample program warmup (UInt64.ofNat (2 ^ 64 - warmup)) fuel
    unless warmed.failed == 0 do
      throw (IO.userError s!"{warmed.failed} warmup executions failed: {warmed.firstFailure}")
    let started ← IO.monoNanosNow
    let result ← sample program samples (UInt64.ofNat seed) fuel
    let elapsed := (← IO.monoNanosNow) - started
    IO.println (Lean.Json.mkObj [
      ("requested", toJson samples), ("returned", toJson result.returned),
      ("rejected", toJson result.rejected), ("failed", toJson result.failed),
      ("first_failure", toJson result.firstFailure), ("sampling_ns", toJson elapsed),
      ("mean_bits", toJson result.mean.toBits), ("m2_bits", toJson result.m2.toBits)
    ]).compress
    if result.failed > 0 then
      (← IO.getStderr).putStrLn s!"{result.failed}/{samples} executions failed: {result.firstFailure}"
      return 1
    if result.returned == 0 || result.mean.isNaN || result.mean.isInf ||
        result.m2.isNaN || result.m2.isInf then
      throw (IO.userError "no finite estimate available")
    return 0
  catch error =>
    (← IO.getStderr).putStrLn s!"{error}"
    return 1
