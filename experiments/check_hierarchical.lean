import Determinize.Frontend.Compile

def main (args : List String) : IO UInt32 := do
  let mut failed := false
  for path in args do
    match Determinize.Frontend.compile (← IO.FS.readFile path) with
    | .ok _ => IO.println s!"PASS {path}: parsed, inferred, certified"
    | .error e =>
      IO.eprintln s!"FAIL {path}: {e}"
      failed := true
  return if failed then 1 else 0
