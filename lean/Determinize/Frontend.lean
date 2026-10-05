import Determinize.Frontend.Affinity
import Determinize.Frontend.Compile
import Determinize.Frontend.Elaborate
import Determinize.Frontend.Infer
import Determinize.Frontend.Parser
import Determinize.Frontend.Pretty
import Determinize.Frontend.Syntax
import Determinize.Frontend.Unify

/-!
# Front end

From source text to a program, in the order `Determinize.Frontend.Compile` runs the steps:
parsing (`Determinize.Frontend.Parser`) into the syntax of `Determinize.Frontend.Syntax`,
name resolution (`Determinize.Frontend.Elaborate`), and affinity inference
(`Determinize.Frontend.Infer`), with the shape unifier `Determinize.Frontend.Unify` and the
affinity solver `Determinize.Frontend.Affinity`. Inference is verified in
`Determinize/Proof/Frontend/`; parsing, name resolution and printing
(`Determinize.Frontend.Pretty`) are not.
-/
