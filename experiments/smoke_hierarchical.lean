import Determinize.Frontend.Compile
import Determinize.Runtime.Eval

open Determinize

def main (args : List String) : IO UInt32 := do
  for path in args do
    let program ← IO.ofExcept (Frontend.compile (← IO.FS.readFile path))
    for target in [false, true] do
      let expression := if target then program.checked.source.determinize else program.checked.source
      match Runtime.runOutcome expression 42 10000000 with
      | .error e => throw (IO.userError s!"{path} target={target}: {e}")
      | .ok .rejected => IO.println s!"{path} target={target}: rejected (valid observation outcome)"
      | .ok (.returned v stats) =>
        IO.println s!"{path} target={target}: {v.display}, draws={stats.draws}"
  return 0
