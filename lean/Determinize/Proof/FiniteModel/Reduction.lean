import Determinize.Proof.FiniteModel.Contexts

namespace Determinize.Proof.FiniteModel
open Spec.Paper Determinize.Finite Checking Binding

/-- A root continuation step and its corresponding paper reduction. -/
def RootStep (before after : State) : Prop :=
  step before = .ok (.next .continue [(1, after)]) ∧
    reduce (stateExpr before) = .next (stateExpr after)

theorem rootStep_closure (body : Core) (environment : List Value) (argument : Value) :
    RootStep (.deliver argument [.right .app (.closure body environment)])
      (.eval body (argument :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simpa [stateExpr, stackExpr, frameExpr, binaryExpr]
    using reduce_app_closure body environment argument

theorem rootStep_recursive (body : Core) (environment : List Value) (argument : Value) :
    RootStep (.deliver argument [.right .app (.recursive body environment)])
      (.eval body (argument :: .recursive body environment :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simpa [stateExpr, stackExpr, frameExpr, binaryExpr]
    using reduce_app_recursive body environment argument

theorem rootStep_let (body : Core) (environment : List Value) (value : Value) :
    RootStep (.deliver value [.letBody body environment])
      (.eval body (value :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, reduce, isValue_valueExpr, substHead_close]

theorem rootStep_sum_left (left right : Core) (environment : List Value) (value : Value) :
    RootStep (.deliver (.inl value) [.matchSum left right environment])
      (.eval left (value :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, valueExpr, reduce, Expr.isValue, isValue_valueExpr,
    substHead_close]

theorem rootStep_sum_right (left right : Core) (environment : List Value) (value : Value) :
    RootStep (.deliver (.inr value) [.matchSum left right environment])
      (.eval right (value :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, valueExpr, reduce, Expr.isValue, isValue_valueExpr,
    substHead_close]

theorem rootStep_list_nil (nilCase consCase : Core) (environment : List Value) :
    RootStep (.deliver .nil [.matchList nilCase consCase environment])
      (.eval nilCase environment []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, valueExpr, reduce, Expr.isValue]

theorem rootStep_list_cons (nilCase consCase : Core) (environment : List Value)
    (head tail : Value) :
    RootStep (.deliver (.cons head tail) [.matchList nilCase consCase environment])
      (.eval consCase (head :: tail :: environment) []) := by
  refine ⟨rfl, ?_⟩
  simp only [stateExpr, stackExpr, List.foldl_cons, List.foldl_nil, frameExpr,
    valueExpr, reduce, Expr.isValue, isValue_valueExpr, Bool.and_self, ↓reduceIte]
  congr 1
  simpa only [environmentExpr] using
    close_two_subst (interpret consCase) (environmentExpr environment)
      (scoped_environmentExpr environment)
      (valueExpr head) (valueExpr tail) (scoped_valueExpr head) (scoped_valueExpr tail)

theorem rootStep_branch (yes no : Core) (environment : List Value) (condition : Bool) :
    RootStep (.deliver (.bool condition) [.choose yes no environment])
      (.eval (if condition then yes else no) environment []) := by
  cases condition <;> refine ⟨rfl, ?_⟩ <;>
    simp [stateExpr, stackExpr, frameExpr, valueExpr, reduce, Expr.isValue]

theorem rootStep_fst (left right : Value) :
    RootStep (.deliver (.pair left right) [.unary .fst]) (.deliver left []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr, reduce, Expr.isValue,
    isValue_valueExpr]

theorem rootStep_snd (left right : Value) :
    RootStep (.deliver (.pair left right) [.unary .snd]) (.deliver right []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr, reduce, Expr.isValue,
    isValue_valueExpr]

theorem rootStep_pair (left right : Value) :
    RootStep (.deliver right [.right .pair left]) (.deliver (.pair left right) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, isValue_valueExpr]

theorem rootStep_cons (head tail : Value) :
    RootStep (.deliver tail [.right .cons head]) (.deliver (.cons head tail) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, isValue_valueExpr]

theorem rootStep_inl (value : Value) :
    RootStep (.deliver value [.unary .inl]) (.deliver (.inl value) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr, reduce, isValue_valueExpr]

theorem rootStep_inr (value : Value) :
    RootStep (.deliver value [.unary .inr]) (.deliver (.inr value) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr, reduce, isValue_valueExpr]

theorem rootStep_add (a b : Rat) :
    RootStep (.deliver (.number b) [.right .add (.number a)]) (.deliver (.number (a + b)) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, Expr.isValue, realValue?]

theorem rootStep_mul (a b : Rat) :
    RootStep (.deliver (.number b) [.right .mul (.number a)]) (.deliver (.number (a * b)) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, Expr.isValue, realValue?]

theorem rootStep_div (a b : Rat) (nonzero : b ≠ 0) :
    RootStep (.deliver (.number b) [.right .div (.number a)]) (.deliver (.number (a / b)) []) := by
  refine ⟨by simp [step, binary, nonzero, pure, bind, Except.bind, Except.pure], ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, Expr.isValue, realValue?,
    nonzero]

theorem rootStep_neg (a : Rat) :
    RootStep (.deliver (.number a) [.unary .neg]) (.deliver (.number (-a)) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr, reduce, Expr.isValue]

theorem rootStep_lt (a b : Rat) :
    RootStep (.deliver (.number b) [.right .lt (.number a)]) (.deliver (.bool (a < b)) []) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, Expr.isValue, realValue?]

end Determinize.Proof.FiniteModel
