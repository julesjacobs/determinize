import Determinize.Proof.RewardModel.Control

namespace Determinize.Spec.RewardModel

abbrev Model.control (model : Model) : FiniteModel.Model where
  size := model.size
  initial := model.initial
  kind := model.kind
  transition := controlWeight model
  nonnegative := Proof.RewardModel.controlWeight_nonnegative model
  normalized := Proof.RewardModel.controlWeight_normalized model

end Determinize.Spec.RewardModel
