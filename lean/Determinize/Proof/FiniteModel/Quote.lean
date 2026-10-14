import Determinize.Finite.Reward.Graph
import Lean

deriving instance Lean.ToExpr for Except

deriving instance Lean.ToExpr for Determinize.Spec.Paper.Affinity
deriving instance Lean.ToExpr for Determinize.Spec.Paper.DistributionAction
deriving instance Lean.ToExpr for Determinize.Spec.Paper.Op
deriving instance Lean.ToExpr for Determinize.Spec.Paper.Expr
deriving instance Lean.ToExpr for Determinize.Spec.FiniteModel.Subject
deriving instance Lean.ToExpr for Determinize.Spec.FiniteModel.StateKind
deriving instance Lean.ToExpr for Determinize.Finite.Value
deriving instance Lean.ToExpr for Determinize.Finite.Unary
deriving instance Lean.ToExpr for Determinize.Finite.Binary
deriving instance Lean.ToExpr for Determinize.Finite.Frame
deriving instance Lean.ToExpr for Determinize.Finite.State
deriving instance Lean.ToExpr for Determinize.Finite.Edge
deriving instance Lean.ToExpr for Determinize.Finite.Row
deriving instance Lean.ToExpr for Determinize.Finite.Candidate
deriving instance Lean.ToExpr for Determinize.Finite.Reward.Edge
deriving instance Lean.ToExpr for Determinize.Finite.Reward.Row
deriving instance Lean.ToExpr for Determinize.Finite.Reward.Candidate

deriving instance Lean.ToExpr for Determinize.Finite.Failure
deriving instance Lean.ToExpr for Determinize.Finite.Evidence
deriving instance Lean.ToExpr for Determinize.Finite.Reward.Outcome
deriving instance Lean.ToExpr for Determinize.Finite.Reward.Step
