namespace Determinize.Proof.FiniteModel

inductive Lookup (α : Type) where
  | value (entry : α)
  | branch (split : Nat) (left right : Lookup α)

def Lookup.get (table : Lookup α) (i : Nat) : α :=
  match table with
  | .value entry => entry
  | .branch split left right => if i < split then left.get i else right.get i

def Lookup.Indexed (table : Lookup α) (label : α → Nat) (lo hi : Nat) : Prop :=
  match table with
  | .value entry => hi = lo + 1 ∧ label entry = lo
  | .branch split left right =>
    lo < split ∧ split < hi ∧ left.Indexed label lo split ∧ right.Indexed label split hi

private def indexedDecidable (table : Lookup α) (label : α → Nat) (lo hi : Nat) :
    Decidable (table.Indexed label lo hi) :=
  match table with
  | .value _ => inferInstanceAs (Decidable (_ ∧ _))
  | .branch split left right =>
    letI := indexedDecidable left label lo split
    letI := indexedDecidable right label split hi
    inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _))

instance (table : Lookup α) (label : α → Nat) (lo hi : Nat) :
    Decidable (table.Indexed label lo hi) := indexedDecidable table label lo hi

theorem Lookup.get_index (table : Lookup α) (label : α → Nat) (lo hi : Nat)
    (valid : table.Indexed label lo hi) (i : Nat) (lower : lo ≤ i) (upper : i < hi) :
    label (table.get i) = i := by
  induction table generalizing lo hi with
  | value a =>
    have endEq := valid.1
    have h : i = lo := by omega
    exact valid.2.trans h.symm
  | branch split left right ihLeft ihRight =>
    by_cases before : i < split
    · simpa [Lookup.get, before] using ihLeft lo split valid.2.2.1 lower before
    · simpa [Lookup.get, before] using
        ihRight split hi valid.2.2.2 (by omega) upper

theorem lookup_evidence_all {n : Nat} (p : Fin n → Prop)
    (table : Lookup (Subtype p))
    (indexed : table.Indexed (fun e ↦ e.val.val) 0 n) : ∀ i, p i := by
  intro i
  have same := table.get_index (fun e ↦ e.val.val) 0 n indexed i.val (Nat.zero_le _) i.isLt
  have index : (table.get i.val).val = i := Fin.ext same
  exact index ▸ (table.get i.val).property

end Determinize.Proof.FiniteModel
