import Determinize.Proof.Corollaries
import Determinize.Proof.Examples
import Determinize.Proof.FiniteModel.Administrative
import Determinize.Proof.FiniteModel.Boundary
import Determinize.Proof.FiniteModel.BoundaryExistence
import Determinize.Proof.FiniteModel.Build
import Determinize.Proof.FiniteModel.BuildModel
import Determinize.Proof.FiniteModel.Contexts
import Determinize.Proof.FiniteModel.Continuation
import Determinize.Proof.FiniteModel.Equality
import Determinize.Proof.FiniteModel.Execution
import Determinize.Proof.FiniteModel.Graph
import Determinize.Proof.FiniteModel.IndexedGraph
import Determinize.Proof.FiniteModel.IndexedReplay
import Determinize.Proof.FiniteModel.Initial
import Determinize.Proof.FiniteModel.Invariants
import Determinize.Proof.FiniteModel.LinearBounds
import Determinize.Proof.FiniteModel.Local
import Determinize.Proof.FiniteModel.MeasureLaws
import Determinize.Proof.FiniteModel.Model
import Determinize.Proof.FiniteModel.Paths
import Determinize.Proof.FiniteModel.Progress
import Determinize.Proof.FiniteModel.Queries
import Determinize.Proof.FiniteModel.Reduction
import Determinize.Proof.FiniteModel.Reification
import Determinize.Proof.FiniteModel.Replay
import Determinize.Proof.FiniteModel.Result
import Determinize.Proof.FiniteModel.Sampling
import Determinize.Proof.FiniteModel.Soundness
import Determinize.Proof.FiniteModel.SparseQueries
import Determinize.Proof.FiniteModel.SparseRow
import Determinize.Proof.FiniteModel.Statistics
import Determinize.Proof.FiniteModel.Substitution
import Determinize.Proof.FiniteModel.Termination
import Determinize.Proof.FiniteModel.Transition
import Determinize.Proof.Frontend.Affinity
import Determinize.Proof.Frontend.Completeness
import Determinize.Proof.Frontend.Decompose
import Determinize.Proof.Frontend.Ground
import Determinize.Proof.Frontend.Inference
import Determinize.Proof.Frontend.Soundness
import Determinize.Proof.Frontend.Typing
import Determinize.Proof.Frontend.Unify
import Determinize.Proof.InterfaceChecks
import Determinize.Proof.LinearAlgebra.Solve
import Determinize.Proof.Normalization
import Determinize.Proof.Primitives.DiscreteLaws
import Determinize.Proof.Primitives.DiscreteRational
import Determinize.Proof.Primitives.FiniteDistributionMeasure
import Determinize.Proof.Primitives.Kernels
import Determinize.Proof.Primitives.Laws
import Determinize.Proof.Primitives.Mass
import Determinize.Proof.Primitives.Moments
import Determinize.Proof.RewardModel.Boundary
import Determinize.Proof.RewardModel.Certificates
import Determinize.Proof.RewardModel.Control
import Determinize.Proof.RewardModel.Correspondence
import Determinize.Proof.RewardModel.Execution
import Determinize.Proof.RewardModel.FiniteIntegrability
import Determinize.Proof.RewardModel.Integrability
import Determinize.Proof.RewardModel.Laws
import Determinize.Proof.RewardModel.Measure
import Determinize.Proof.RewardModel.Moments
import Determinize.Proof.RewardModel.Normalization
import Determinize.Proof.RewardModel.Replay
import Determinize.Proof.RewardModel.Safety
import Determinize.Proof.RewardModel.Soundness
import Determinize.Proof.RewardModel.Stack
import Determinize.Proof.RewardModel.Translation
import Determinize.Proof.Semantics.Cumulative
import Determinize.Proof.Semantics.ExpressionSpace
import Determinize.Proof.Semantics.Internal
import Determinize.Proof.Semantics.Measurability
import Determinize.Proof.Semantics.Ordinary
import Determinize.Proof.Semantics.Rejection
import Determinize.Proof.Semantics.Subtyping
import Determinize.Proof.Semantics.Termination
import Determinize.Proof.Semantics.Typing
import Determinize.Proof.Soundness
import Determinize.Proof.Symbolic.Environment
import Determinize.Proof.Symbolic.Mean
import Determinize.Proof.Symbolic.MeanTraces
import Determinize.Proof.Symbolic.Moments
import Determinize.Proof.Symbolic.Soundness
import Determinize.Proof.Symbolic.Syntax
import Determinize.Proof.Symbolic.TraceGeneration
import Determinize.Proof.Symbolic.TraceLaws
import Determinize.Proof.Symbolic.TraceSamples
import Determinize.Proof.Symbolic.TraceSteps
import Determinize.Proof.Traces.CompactFiberSoundness
import Determinize.Proof.Traces.CompactReplay
import Determinize.Proof.Traces.CompactSoundness
import Determinize.Proof.Traces.CompactTrace
import Determinize.Proof.Traces.ConditionalLaw
import Determinize.Proof.Traces.Detailed
import Determinize.Proof.Traces.Factorization
import Determinize.Proof.Traces.Fibers
import Determinize.Proof.Traces.Labels
import Determinize.Proof.Traces.Mass
import Determinize.Proof.Traces.ReplaySemantics
import Determinize.Proof.Traces.SamplingContinuation
import Determinize.Proof.Traces.Semantics
import Determinize.Proof.Traces.Steps

/-!
# Proofs

The proofs of the theorems in `Determinize.Theorems`. A reviewer need not read them: the build
fails if a theorem uses an axiom other than `propext`, `Classical.choice` and `Quot.sound`, or if
its statement relies on anything from these modules other than proofs.

- `Determinize/Proof/Primitives/`: distribution laws, kernels, masses, and moments.
- `Determinize/Proof/Semantics/`: expression measurability, evaluator kernels, and type safety.
- `Determinize/Proof/Symbolic/`: affine expressions, symbolic reduction, and its invariants.
- `Determinize/Proof/Traces/`: detailed and compact traces, replay, and conditional laws.
- `Determinize.Proof.Soundness`, `Determinize.Proof.Corollaries`,
  `Determinize.Proof.Normalization`: the output-law theorems, derived from trace soundness.
- `Determinize/Proof/Frontend/`: soundness, optimality and completeness of affinity inference.
- `Determinize/Proof/FiniteModel/`, `Determinize/Proof/RewardModel/`: correspondence of finite
  and reward models with programs, and soundness of their certificates.
- `Determinize/Proof/LinearAlgebra/`: Gaussian elimination with a proof of the original
  equations.
- `Determinize.Proof.Examples`, `Determinize.Proof.InterfaceChecks`: coverage examples, and
  checks of the evaluator through public imports only.
-/
