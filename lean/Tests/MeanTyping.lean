import Determinize.Checking.Elaboration

namespace Determinize.Tests.MeanTyping
open Determinize.Spec.Paper Determinize.Checking

private def meanExpr : Core := .uniform .mean (.bvar 0) (.real 1)
private def cert (a : Affinity) : Certificate :=
  .node (.float a) [.node (.float a) [], .node (.float a) []]
example : (check [.float .E] meanExpr (.float .E) (cert .E)).isSome = true := by decide +kernel
example : (check [.float .G] meanExpr (.float .G) (cert .G)).isSome = true := by decide +kernel
example : (check [.float .E] meanExpr (.float .G) (cert .G)).isSome = false := by decide +kernel
example : (Expr.uniform (.sample .E) (.real 0) (.real 2)).determinize =
    .uniform .mean (.real 0) (.real 2) := rfl
example : (Expr.uniform (.sample .G) (.real 0) (.real 2)).determinize =
    .uniform (.sample .G) (.real 0) (.real 2) := rfl

example : (check [.float .E] (.gaussian .mean (.real 0) (.bvar 0)) (.float .E)
    (.node (.float .E) [.node (.float .E) [], .node (.float .G) []])).isSome = false := by
  decide +kernel

example : sampleAffinities (.uniform .mean (.poisson (.sample .E) (.real 2)) (.real 3)) = [.E] := rfl

end Determinize.Tests.MeanTyping
