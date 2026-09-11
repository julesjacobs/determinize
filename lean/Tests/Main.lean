import Tests.Inference
import Tests.Checking
import Tests.Runtime
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
    for file in args do
      let text ← IO.FS.readFile file
      match Frontend.compile text with
      | .ok _ => IO.println s!"PASS {file}"
      | .error e => throw (IO.userError s!"{file}: {e}")
    IO.println "All Lean front-end tests passed."
    return 0
  catch e =>
    (← IO.getStderr).putStrLn s!"{e}"
    return 1
