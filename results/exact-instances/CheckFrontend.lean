import Determinize.Frontend.Compile
open Determinize.Spec.Paper
set_option maxRecDepth 100000
set_option maxHeartbeats 0
namespace Case0
def checkedSource : Expr Rat := (.letE (.lam (.lam (.ite (.letE (.bvar 1) (.letE (.bvar 1) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.letE (.bvar 0) (.letE (.bvar 2) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.bool false)))) (.letE (.fix (.ite (.app (.app (.bvar 2) (.bvar 0)) (.real ((2 : Rat) / 1))) (.real ((0 : Rat) / 1)) (.letE (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 2)))) (.letE (.ite (.bvar 0) (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 2))) (.uniform (.sample .E) (.real ((1 : Rat) / 2)) (.real ((1 : Rat) / 1)))) (.ite (.bvar 1) (.add (.bvar 0) (.app (.bvar 3) (.add (.bvar 2) (.real ((1 : Rat) / 1))))) (.add (.bvar 0) (.app (.bvar 3) (.bvar 2)))))))) (.app (.bvar 0) (.real ((0 : Rat) / 1)))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/add-uniform-with-counter-large-unbounded_variant.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "add-uniform-with-counter-large-unbounded_variant.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case0
namespace Case1
def checkedSource : Expr Rat := (.letE (.fix (.lam (.matchList (.bvar 1) (.nil) (.letE (.gaussian (.sample .E) (.bvar 0) (.bvar 2)) (.letE (.app (.app (.bvar 5) (.bvar 2)) (.bvar 3)) (.cons (.bvar 1) (.bvar 0))))))) (.letE (.fix (.matchList (.bvar 0) (.real ((0 : Rat) / 1)) (.add (.bvar 0) (.app (.bvar 3) (.bvar 1))))) (.letE (.cons (.real ((1 : Rat) / 1)) (.cons (.real ((2 : Rat) / 1)) (.cons (.real ((3 : Rat) / 1)) (.nil)))) (.letE (.app (.app (.bvar 2) (.bvar 0)) (.real ((1 : Rat) / 1))) (.app (.bvar 2) (.bvar 0))))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/addNoise.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "addNoise.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case1
namespace Case2
def checkedSource : Expr Rat := (.letE (.lam (.letE (.fix (.lam (.ite (.lt (.bvar 1) (.bvar 3)) (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 1))) (.app (.app (.bvar 3) (.add (.bvar 2) (.real ((1 : Rat) / 1)))) (.add (.bvar 1) (.bvar 0)))) (.bvar 0)))) (.app (.app (.bvar 0) (.real ((0 : Rat) / 1))) (.real ((0 : Rat) / 1))))) (.app (.bvar 0) (.real ((2 : Rat) / 1))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/aggregate-rv.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "aggregate-rv.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case2
namespace Case3
def checkedSource : Expr Rat := (.letE (.fix (.ite (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 2)))) (.uniform (.sample .E) (.neg (.real ((1 : Rat) / 1))) (.real ((1 : Rat) / 1))) (.ite (.letE (.real ((2 : Rat) / 1)) (.letE (.bvar 1) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.add (.uniform (.sample .E) (.neg (.real ((1 : Rat) / 1))) (.real ((0 : Rat) / 1))) (.app (.bvar 1) (.real ((0 : Rat) / 1)))) (.add (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 1))) (.app (.bvar 1) (.add (.bvar 0) (.real ((1 : Rat) / 1)))))))) (.app (.bvar 0) (.real ((0 : Rat) / 1))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/asym-random-walk_variant.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "asym-random-walk_variant.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case3
namespace Case5
def checkedSource : Expr Rat := (.letE (.fix (.lam (.lam (.matchList (.bvar 2) (.bvar 0) (.letE (.gaussian (.sample .E) (.bvar 2) (.add (.bvar 0) (.neg (.bvar 3)))) (.app (.app (.app (.bvar 6) (.bvar 2)) (.bvar 1)) (.bvar 0))))))) (.letE (.cons (.real ((1 : Rat) / 1)) (.cons (.real ((2 : Rat) / 1)) (.cons (.real ((3 : Rat) / 1)) (.cons (.real ((4 : Rat) / 1)) (.cons (.real ((5 : Rat) / 1)) (.cons (.real ((6 : Rat) / 1)) (.nil))))))) (.app (.app (.app (.bvar 1) (.bvar 0)) (.real ((0 : Rat) / 1))) (.real ((0 : Rat) / 1)))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/gauss_randwalk_finalPos.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "gauss_randwalk_finalPos.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case5
namespace Case6
def checkedSource : Expr Rat := (.letE (.lam (.letE (.uniform (.sample .E) (.neg (.real ((5 : Rat) / 1))) (.neg (.real ((3 : Rat) / 1)))) (.letE (.uniform (.sample .E) (.real ((2 : Rat) / 1)) (.real ((3 : Rat) / 1))) (.letE (.uniform (.sample .E) (.real ((1 : Rat) / 1)) (.real ((5 : Rat) / 1))) (.letE (.uniform (.sample .E) (.neg (.real ((1 : Rat) / 2))) (.real ((1 : Rat) / 2))) (.letE (.fix (.lam (.lam (.lam (.lam (.ite (.lt (.bvar 4) (.bvar 10)) (.letE (.add (.bvar 4) (.real ((1 : Rat) / 1))) (.letE (.add (.bvar 4) (.mul (.real ((1 : Rat) / 100)) (.bvar 3))) (.letE (.add (.add (.add (.add (.mul (.real ((1 : Rat) / 50)) (.bvar 0)) (.mul (.real ((103 : Rat) / 100)) (.bvar 4))) (.neg (.mul (.real ((3 : Rat) / 10)) (.bvar 3)))) (.neg (.mul (.real ((3 : Rat) / 50)) (.bvar 2)))) (.uniform (.sample .E) (.neg (.real ((1 : Rat) / 1))) (.real ((1 : Rat) / 1)))) (.letE (.add (.bvar 4) (.mul (.real ((1 : Rat) / 100)) (.bvar 3))) (.letE (.add (.add (.add (.add (.mul (.real ((1 : Rat) / 25)) (.bvar 2)) (.mul (.real ((7 : Rat) / 100)) (.bvar 1))) (.neg (.mul (.real ((51 : Rat) / 100)) (.bvar 0)))) (.mul (.real ((17 : Rat) / 20)) (.bvar 4))) (.uniform (.sample .E) (.neg (.real ((4 : Rat) / 5))) (.real ((4 : Rat) / 5)))) (.app (.app (.app (.app (.app (.bvar 10) (.bvar 4)) (.bvar 3)) (.bvar 2)) (.bvar 1)) (.bvar 0))))))) (.bvar 1))))))) (.app (.app (.app (.app (.app (.bvar 0) (.real ((0 : Rat) / 1))) (.bvar 4)) (.bvar 3)) (.bvar 2)) (.bvar 1)))))))) (.app (.bvar 0) (.real ((2 : Rat) / 1))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/invpend.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "invpend.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case6
namespace Case7
def checkedSource : Expr Rat := (.letE (.lam (.lam (.fix (.ite (.letE (.bvar 0) (.letE (.real ((0 : Rat) / 1)) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.real ((0 : Rat) / 1)) (.letE (.app (.bvar 1) (.add (.bvar 0) (.neg (.real ((1 : Rat) / 1))))) (.letE (.app (.bvar 4) (.unit)) (.div (.add (.app (.bvar 4) (.bvar 0)) (.mul (.add (.bvar 2) (.neg (.real ((1 : Rat) / 1)))) (.bvar 1))) (.bvar 2)))))))) (.letE (.lam (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 1)))) (.letE (.lam (.add (.mul (.real ((2 : Rat) / 1)) (.bvar 0)) (.real ((1 : Rat) / 1)))) (.app (.app (.app (.bvar 2) (.bvar 1)) (.bvar 0)) (.real ((10 : Rat) / 1))))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/monte_carlo.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "monte_carlo.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case7
namespace Case8
def checkedSource : Expr Rat := (.app (.fix (.ite (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 2)))) (.bvar 0) (.app (.bvar 1) (.add (.add (.real ((1 : Rat) / 1)) (.neg (.bvar 0))) (.gaussian (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 1))))))) (.real ((0 : Rat) / 1)))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/noisy-iteration.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "noisy-iteration.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case8
namespace Case9
def checkedSource : Expr Rat := (.letE (.lam (.letE (.fix (.lam (.lam (.ite (.lt (.bvar 0) (.bvar 4)) (.letE (.discrete (.sample .G) (.cons (.real ((1 : Rat) / 2)) (.cons (.real ((1 : Rat) / 4)) (.nil)))) (.ite (.lt (.bvar 0) (.real ((1 : Rat) / 1))) (.letE (.add (.real ((9 : Rat) / 10)) (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 5)))) (.add (.bvar 0) (.app (.app (.app (.bvar 5) (.bvar 4)) (.bvar 3)) (.add (.bvar 2) (.real ((1 : Rat) / 1)))))) (.ite (.lt (.bvar 0) (.real ((2 : Rat) / 1))) (.letE (.add (.real ((11 : Rat) / 10)) (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 10)))) (.ite (.letE (.bvar 3) (.letE (.bvar 5) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.add (.bvar 0) (.app (.app (.app (.bvar 5) (.bvar 4)) (.add (.bvar 3) (.real ((1 : Rat) / 1)))) (.add (.bvar 2) (.real ((1 : Rat) / 1))))) (.add (.bvar 0) (.app (.app (.app (.bvar 5) (.bvar 4)) (.bvar 3)) (.bvar 2))))) (.letE (.add (.real ((4 : Rat) / 5)) (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 10)))) (.ite (.letE (.bvar 4) (.letE (.bvar 4) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.add (.bvar 0) (.app (.app (.app (.bvar 5) (.add (.bvar 4) (.real ((1 : Rat) / 1)))) (.bvar 3)) (.add (.bvar 2) (.real ((1 : Rat) / 1))))) (.add (.bvar 0) (.app (.app (.app (.bvar 5) (.bvar 4)) (.bvar 3)) (.bvar 2)))))))) (.real ((0 : Rat) / 1)))))) (.app (.app (.app (.bvar 0) (.real ((0 : Rat) / 1))) (.real ((0 : Rat) / 1))) (.real ((0 : Rat) / 1))))) (.app (.bvar 0) (.real ((2 : Rat) / 1))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/pack.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "pack.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case9
namespace Case10
def checkedSource : Expr Rat := (.letE (.fix (.letE (.ite (.bvar 0) (.uniform (.sample .E) (.real ((8 : Rat) / 1)) (.real ((12 : Rat) / 1))) (.uniform (.sample .E) (.real ((2 : Rat) / 1)) (.real ((6 : Rat) / 1)))) (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.bvar 0)) (.letE (.ite (.bvar 2) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((17 : Rat) / 20)))) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((3 : Rat) / 10))))) (.ite (.bvar 0) (.bvar 1) (.letE (.ite (.bvar 3) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((4 : Rat) / 5)))) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 4))))) (.add (.bvar 2) (.app (.bvar 5) (.bvar 0))))))))) (.app (.bvar 0) (.bool true)))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/retransmit.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "retransmit.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case10
namespace Case11
def checkedSource : Expr Rat := (.letE (.lam (.letE (.lam (.letE (.ite (.bvar 0) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((9 : Rat) / 10)))) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((2 : Rat) / 5))))) (.letE (.ite (.bvar 0) (.exponential (.sample .E) (.real ((2 : Rat) / 1))) (.exponential (.sample .E) (.real ((1 : Rat) / 1)))) (.pair (.bvar 1) (.bvar 0))))) (.letE (.lam (.letE (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((19 : Rat) / 20)))) (.letE (.ite (.bvar 0) (.exponential (.sample .E) (.real ((3 : Rat) / 2))) (.exponential (.sample .E) (.real ((3 : Rat) / 4)))) (.pair (.bvar 1) (.bvar 0))))) (.letE (.fix (.lam (.lam (.ite (.lt (.bvar 1) (.real ((1 : Rat) / 1))) (.real ((0 : Rat) / 1)) (.letE (.app (.bvar 2) (.bvar 0)) (.letE (.fst (.bvar 0)) (.letE (.snd (.bvar 1)) (.add (.bvar 0) (.app (.app (.app (.bvar 6) (.bvar 5)) (.add (.bvar 4) (.neg (.real ((1 : Rat) / 1))))) (.bvar 1)))))))))) (.letE (.ite (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 2)))) (.bvar 2) (.bvar 1)) (.app (.app (.app (.bvar 1) (.bvar 0)) (.bvar 4)) (.bool false))))))) (.app (.bvar 0) (.real ((2 : Rat) / 1))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/service_policy.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "service_policy.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case11
namespace Case12
def checkedSource : Expr Rat := (.letE (.lam (.letE (.mul (.real ((5 : Rat) / 1)) (.bernoulli (.sample .E) (.real ((1 : Rat) / 2)))) (.letE (.add (.real ((4 : Rat) / 1)) (.mul (.real ((3 : Rat) / 1)) (.bernoulli (.sample .E) (.real ((1 : Rat) / 2))))) (.letE (.fix (.lam (.lam (.ite (.lt (.bvar 2) (.bvar 6)) (.letE (.uniform (.sample .E) (.neg (.real ((1 : Rat) / 1))) (.real ((1 : Rat) / 1))) (.letE (.uniform (.sample .E) (.neg (.real ((2 : Rat) / 1))) (.real ((2 : Rat) / 1))) (.app (.app (.app (.bvar 5) (.add (.bvar 4) (.real ((1 : Rat) / 1)))) (.add (.bvar 2) (.bvar 1))) (.add (.bvar 3) (.bvar 0))))) (.bvar 1))))) (.app (.app (.app (.bvar 0) (.real ((0 : Rat) / 1))) (.bvar 2)) (.bvar 1)))))) (.app (.bvar 0) (.real ((1 : Rat) / 1))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/swap.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "swap.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case12
namespace Case13
def checkedSource : Expr Rat := (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 1))) (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.bvar 0)) (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.bvar 0)) (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.bvar 0)) (.bvar 0)))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/uniform_nested.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "uniform_nested.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case13
namespace Case14
def checkedSource : Expr Rat := (.letE (.lam (.letE (.fix (.ite (.lt (.bvar 0) (.real ((1 : Rat) / 1))) (.nil) (.letE (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 4)))) (.cons (.bvar 0) (.app (.bvar 2) (.add (.bvar 1) (.neg (.real ((1 : Rat) / 1))))))))) (.letE (.fix (.matchList (.bvar 0) (.real ((0 : Rat) / 1)) (.letE (.ite (.bvar 0) (.uniform (.sample .E) (.real ((3 : Rat) / 2)) (.real ((5 : Rat) / 2))) (.uniform (.sample .E) (.real ((1 : Rat) / 2)) (.real ((3 : Rat) / 2)))) (.add (.bvar 0) (.app (.bvar 4) (.bvar 2)))))) (.letE (.app (.bvar 1) (.bvar 2)) (.app (.bvar 1) (.bvar 0)))))) (.app (.bvar 0) (.real ((2 : Rat) / 1))))
#eval do
  let source ← IO.FS.readFile "../results/exact-instances/instances/workload.det"
  let program ← IO.ofExcept (Determinize.Frontend.compile source)
  if program.source = checkedSource then
    IO.println "workload.det: current frontend agrees"
  else throw (IO.userError "frontend mismatch")
end Case14
