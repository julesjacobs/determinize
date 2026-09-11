import Tests.Inference
import Tests.Checking
import Tests.Runtime
import Tests.Corpus
import Tests.FiniteDistribution
import Tests.PrimitiveLaws

open Determinize

def main (args : List String) : IO UInt32 := do
  try
    Tests.parsing
    Tests.inference
    Tests.checking
    Tests.runtime
    Tests.finiteDistributions
    match args with
    | [] => pure ()
    | ["--corpus", path, mode] =>
        Tests.assert (mode == "fast" || mode == "statistical") "invalid corpus mode"
        Tests.corpus path (mode == "statistical")
    | _ => throw (IO.userError "usage: det-tests [--corpus manifest.json fast|statistical]")
    IO.println "All Lean front-end tests passed."
    return 0
  catch e =>
    (← IO.getStderr).putStrLn s!"{e}"
    return 1
