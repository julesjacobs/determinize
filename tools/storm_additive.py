"""Additive output equations and their Lean certificate, used by storm.py."""
from collections import defaultdict
from fractions import Fraction
from pathlib import Path


def read_edges(prefix, size, controller, labels):
    lines = Path(str(prefix) + ".additive.edges").read_text().splitlines()
    if not lines or lines[0] != "additive-rewards 1":
        raise ValueError("unsupported additive reward format")
    n = size - 1
    edges = []
    weights = defaultdict(Fraction)
    for line in lines[1:]:
        source, target, probability, reward = line.split()
        i, j, p, r = int(source), int(target), Fraction(probability), Fraction(reward)
        if not (0 <= i < n and 0 <= j < n and p > 0):
            raise ValueError("invalid additive edge")
        edges.append((i, j, p, r))
        weights[i, j] += p
    expected = defaultdict(Fraction)
    terminals = labels["returned"] | labels["rejected"]
    for i, j, p in controller:
        if i < n:
            expected[i, i if i in terminals else j] += p
    if weights != expected:
        raise ValueError("additive edges disagree with the probability controller")
    for i, j, p, r in edges:
        if i in terminals and (i != j or r != 0):
            raise ValueError("terminal additive rows must have zero reward")
    return edges


def moments(query, size, labels, outputs, dead, edges):
    mass = query([int(i in labels["returned"]) for i in range(size)])
    rejection = query([int(i in labels["rejected"]) for i in range(size)])
    stopped = labels["returned"] | labels["rejected"] | dead

    def signed(rhs):
        positive = query([max(q, 0) for q in rhs])
        negative = query([max(-q, 0) for q in rhs])
        return [p - n for p, n in zip(positive, negative)]

    rhs = list(outputs)
    for i, j, p, r in edges:
        if i not in stopped:
            rhs[i] += p * r * mass[j]
    first = signed(rhs)
    rhs = [q*q for q in outputs]
    for i, j, p, r in edges:
        if i not in stopped:
            rhs[i] += p * (2*r*first[j] + r*r*mass[j])
    second = signed(rhs)
    return {"mass": mass, "rejection": rejection, "first": first, "second": second}


def certificate_text(prefix, values, dead, ranks, following):
    def rat(q):
        return f"(({q.numerator} : Rat) / {q.denominator})"

    text = Path(str(prefix) + ".replay.lean").read_text().replace(
        "import Determinize.Proof.RewardModel.Soundness",
        "import Determinize.Proof.RewardModel.Soundness\nimport Determinize.Proof.RewardModel.Moments")
    for name, vector in values.items():
        text += f"\ndef {name}Values : Vector Rat model.size := ⟨#[{', '.join(map(rat, vector))}], by rfl⟩\n"
    text += f"\ndef deadStates : Vector Bool model.size := ⟨#[{', '.join(str(b).lower() for b in dead)}], by rfl⟩\n"
    text += f"def ranks : Vector Nat model.size := ⟨#[{', '.join(map(str, ranks))}], by rfl⟩\n"
    destinations = ', '.join(f"⟨{j}, by decide +kernel⟩" for j in following)
    text += f"def nextStates : Vector (Fin model.size) model.size := ⟨#[{destinations}], by rfl⟩\n"
    text += """
def boundary : Determinize.Proof.FiniteModel.Boundary model.control where
  dead := fun i => deadStates[i]
  rank := fun i => ranks[i]
  closed := by decide +kernel

def solution : Reward.Solution model where
  boundary := boundary
  paths := ⟨fun i => ranks[i], fun i => nextStates[i]⟩
  pathsValid := by decide +kernel
  mass := fun i => massValues[i]
  massValid := by decide +kernel
  first := fun i => firstValues[i]
  firstValid := by decide +kernel
  second := fun i => secondValues[i]
  secondValid := by decide +kernel
  rejection := fun i => rejectionValues[i]
  rejectionValid := by decide +kernel

abbrev statistics := solution.statistics

theorem checkedResult : Determinize.Spec.RewardModel.ResultMatches model
    (checkedSubject.program checkedSource) statistics :=
  Determinize.Proof.RewardModel.solution_result model _ modelMatches solution

theorem integrability : MeasureTheory.Integrable (fun x : ℝ => x)
    (bigStepMeasure (checkedSubject.program checkedSource)) ∧
    MeasureTheory.Integrable (fun x : ℝ => x^2) (bigStepMeasure (checkedSubject.program checkedSource)) := by
  simpa only [modelMatches.2] using Determinize.Proof.RewardModel.outputMeasure_integrable model

theorem outputStatistics : statistics.Matches (bigStepMeasure (checkedSubject.program checkedSource)) := by
  simpa only [modelMatches.2] using Determinize.Proof.RewardModel.solution_statistics model solution

theorem conditionalVariance (positive : 0 < statistics.returnMass) :
    ProbabilityTheory.variance id ((bigStepMeasure (checkedSubject.program checkedSource) Set.univ)⁻¹ •
      bigStepMeasure (checkedSubject.program checkedSource)) =
      ((statistics.secondMoment / statistics.returnMass - (statistics.firstMoment / statistics.returnMass)^2 : Rat) : ℝ) := by
  simpa only [modelMatches.2] using Determinize.Proof.RewardModel.solution_conditional_variance model solution positive

theorem terminationProbabilities : (⟨solution.mass model.initial, solution.rejection model.initial,
    1-solution.mass model.initial-solution.rejection model.initial⟩ : TerminationStatistics).Matches model.control :=
  Determinize.Proof.RewardModel.solution_termination model solution

#print axioms checkedResult
#print axioms integrability
#print axioms outputStatistics
#print axioms conditionalVariance
#print axioms terminationProbabilities
"""
    return text
