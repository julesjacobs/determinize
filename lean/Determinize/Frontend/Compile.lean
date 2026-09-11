import Determinize.Frontend.Parser
import Determinize.Frontend.Elaborate
import Determinize.Frontend.Infer
import Determinize.Checking.Elaboration

namespace Determinize.Frontend
open Checking

structure Program where
  input : Input
  checked : Certified input.expression input.affinities

def compile (text : String) : Except String Program := do
  let input ← elaborate (← parse text)
  let (source, certificate) ← infer input
  let some checked := certify input.expression source input.affinities certificate
    | throw "inference produced an invalid typing or elaboration certificate"
  return ⟨input, checked⟩

end Determinize.Frontend
