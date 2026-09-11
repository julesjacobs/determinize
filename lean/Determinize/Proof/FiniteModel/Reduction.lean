import Determinize.Proof.FiniteModel.Contexts

namespace Determinize.Proof.FiniteModel
open Statement.Paper Determinize.Finite Checking Binding

/-- A root continuation step and its corresponding paper reduction. -/
def RootStep (before after : State) : Prop :=
  step before = .ok (.next .continue [(1,after)]) ∧
    reduce (stateExpr before) = .next (stateExpr after)

theorem closure_root (body : Core) (environment : List Value) (argument : Value) :
    RootStep (.deliver argument [.right .app (.closure body environment)])
      (.eval body (argument :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simpa [stateExpr, stackExpr, frameExpr, binaryExpr] using closure_beta body environment argument

theorem recursive_root (body : Core) (environment : List Value) (argument : Value) :
    RootStep (.deliver argument [.right .app (.recursive body environment)])
      (.eval body (argument :: .recursive body environment :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simpa [stateExpr, stackExpr, frameExpr, binaryExpr] using recursive_beta body environment argument

theorem let_root (body : Core) (environment : List Value) (value : Value) :
    RootStep (.deliver value [.letBody body environment])
      (.eval body (value :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, reduce, valueExpr_isValue, closure_substitution]

theorem sum_left_root (left right : Core) (environment : List Value) (value : Value) :
    RootStep (.deliver (.inl value) [.matchSum left right environment])
      (.eval left (value :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, valueExpr, reduce, Expr.isValue, valueExpr_isValue,
    closure_substitution]

theorem sum_right_root (left right : Core) (environment : List Value) (value : Value) :
    RootStep (.deliver (.inr value) [.matchSum left right environment])
      (.eval right (value :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, valueExpr, reduce, Expr.isValue, valueExpr_isValue,
    closure_substitution]

theorem list_nil_root (nilCase consCase : Core) (environment : List Value) :
    RootStep (.deliver .nil [.matchList nilCase consCase environment])
      (.eval nilCase environment []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, valueExpr, reduce, Expr.isValue]

theorem list_cons_root (nilCase consCase : Core) (environment : List Value) (head tail : Value) :
    RootStep (.deliver (.cons head tail) [.matchList nilCase consCase environment])
      (.eval consCase (head :: tail :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simp only [stateExpr, stackExpr, List.foldl_cons, List.foldl_nil, frameExpr,
    valueExpr, reduce, Expr.isValue, valueExpr_isValue, Bool.and_self, ↓reduceIte]
  congr 1
  simpa only [environmentExpr] using
    close_two_subst (interpret consCase) (environmentExpr environment) (environmentExpr_closed environment)
      (valueExpr head) (valueExpr tail) (valueExpr_closed head) (valueExpr_closed tail)

theorem branch_root (yes no : Core) (environment : List Value) (condition : Bool) :
    RootStep (.deliver (.bool condition) [.choose yes no environment])
      (.eval (if condition then yes else no) environment []) := by
  cases condition <;> refine ⟨rfl, ?_⟩ <;>
    simp [stateExpr, stackExpr, frameExpr, valueExpr, reduce, Expr.isValue]

theorem fst_root (left right : Value) :
    RootStep (.deliver (.pair left right) [.unary .fst]) (.deliver left []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr, reduce, Expr.isValue, valueExpr_isValue]

theorem snd_root (left right : Value) :
    RootStep (.deliver (.pair left right) [.unary .snd]) (.deliver right []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr, reduce, Expr.isValue, valueExpr_isValue]

theorem pair_root (left right : Value) :
    RootStep (.deliver right [.right .pair left]) (.deliver (.pair left right) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, valueExpr_isValue]

theorem cons_root (head tail : Value) :
    RootStep (.deliver tail [.right .cons head]) (.deliver (.cons head tail) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, valueExpr_isValue]

theorem inl_root (value : Value) :
    RootStep (.deliver value [.unary .inl]) (.deliver (.inl value) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr, reduce, valueExpr_isValue]

theorem inr_root (value : Value) :
    RootStep (.deliver value [.unary .inr]) (.deliver (.inr value) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr, reduce, valueExpr_isValue]

theorem add_root (a b : Rat) :
    RootStep (.deliver (.number b) [.right .add (.number a)]) (.deliver (.number (a+b)) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, Expr.isValue, realValue?]

theorem mul_root (a b : Rat) :
    RootStep (.deliver (.number b) [.right .mul (.number a)]) (.deliver (.number (a*b)) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, Expr.isValue, realValue?]

theorem div_root (a b : Rat) :
    RootStep (.deliver (.number b) [.right .div (.number a)]) (.deliver (.number (a/b)) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, Expr.isValue, realValue?]

theorem neg_root (a : Rat) :
    RootStep (.deliver (.number a) [.unary .neg]) (.deliver (.number (-a)) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr, reduce, Expr.isValue]

theorem lt_root (a b : Rat) :
    RootStep (.deliver (.number b) [.right .lt (.number a)]) (.deliver (.bool (a<b)) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, Expr.isValue, realValue?]

end Determinize.Proof.FiniteModel
