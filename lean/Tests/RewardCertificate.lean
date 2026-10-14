import Determinize.Proof.RewardModel.Sparse
import Determinize.Proof.RewardModel.Native

namespace Determinize.Tests.RewardCertificate
open Proof.RewardModel

private abbrev model : Spec.RewardModel.Model where
  size := 2
  initial := 0
  kind := fun i ↦ if i = 0 then .transient else .returned 0
  edges := fun i ↦ if i = 0 then [⟨1, 1/2, 0⟩, ⟨1, 1/2, 2⟩] else [⟨1, 1, 0⟩]
  nonnegative := by decide +kernel
  normalized := by decide +kernel

private abbrev certificate : Certificate 2 :=
  Certificate.ofVectors #v[false, false] #v[1, 0] #v[1, 1] #v[1, 1] #v[1, 0] #v[2, 0] #v[0, 0]

private theorem valid : ∀ i, certificate.StateValid model i := by decide +kernel

example : (⟨1, 1, 2⟩ : Spec.FiniteModel.OutputStatistics).Matches model.outputMeasure :=
  certificate.statistics model valid

example : ¬ ∀ i, ({certificate with first := fun i ↦ #v[2, 0][i]}).StateValid model i := by decide +kernel
example : ¬ ∀ i, ({certificate with second := fun i ↦ #v[1, 0][i]}).StateValid model i := by decide +kernel
example : ¬ ∀ i, ({certificate with dead := fun i ↦ #v[true, false][i]}).StateValid model i := by
  decide +kernel
example : ¬ ∀ i, ({certificate with rank := fun i ↦ #v[0, 0][i]}).StateValid model i := by decide +kernel
example : ¬ ∀ i, ({certificate with next := fun i ↦ #v[0, 1][i]}).StateValid model i := by decide +kernel
example : ¬ ∀ i, ({certificate with rejection := fun i ↦ #v[1, 0][i]}).StateValid model i := by
  decide +kernel

end Determinize.Tests.RewardCertificate

#print axioms Determinize.Proof.RewardModel.solution_outputStatistics
#print axioms Determinize.Proof.RewardModel.Vectors.verify_sound
