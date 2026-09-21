#!/usr/bin/env python3
"""Obtain exact output moments from Storm and certify them against the Lean model."""
import argparse
from collections import deque
from fractions import Fraction
import importlib.metadata
import json
import os
import re
import signal
from pathlib import Path
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[1]
PROPERTY = 'R=? [ F "done" ]'


def read_model(prefix):
    lines = Path(str(prefix) + ".tra").read_text().splitlines()
    if not lines or lines[0] != "dtmc":
        raise ValueError("expected an explicit DTMC")
    edges = [(int(i), int(j), Fraction(q)) for i, j, q in
             (line.split() for line in lines[1:])]
    size = 1 + max(max(i, j) for i, j, _ in edges)
    if {i for i, _, _ in edges} != set(range(size)):
        raise ValueError("missing transition rows")
    labels = Path(str(prefix) + ".lab").read_text().splitlines()
    if labels[0] != "#DECLARATION" or labels[2] != "#END":
        raise ValueError("expected explicit label declarations")
    sets = {name: set() for name in labels[1].split()}
    for line in labels[3:]:
        state, *names = line.split()
        for name in names:
            sets[name].add(int(state))
    if sets["done"] != {size - 1} or len(sets["init"]) != 1:
        raise ValueError("unexpected sink or initial-state mapping")
    if not sets["init"] <= set(range(size - 1)):
        raise ValueError("initial state is the synthetic sink")
    rewards = [Fraction(0)] * size
    for sign, factor in [("positive", 1), ("negative", -1)]:
        for line in Path(str(prefix) + f".{sign}.state.rew").read_text().splitlines():
            state, reward = line.split()
            rewards[int(state)] += factor * Fraction(reward)
    return size, edges, sets, rewards


def boundary(size, edges, labels):
    predecessors = [set() for _ in range(size)]
    for i, j, p in edges:
        if p > 0:
            predecessors[j].add(i)

    def reverse_search(roots):
        ranks = {i: 0 for i in roots}
        following = {i: i for i in roots}
        queue = deque(sorted(roots))
        while queue:
            j = queue.popleft()
            for i in sorted(predecessors[j]):
                if i not in ranks:
                    ranks[i] = ranks[j] + 1
                    following[i] = j
                    queue.append(i)
        return ranks, following

    terminals = labels["returned"] | labels["rejected"] | labels["done"]
    reachable, _ = reverse_search(terminals)
    dead = set(range(size)) - reachable.keys()
    ranks, following = reverse_search(terminals | dead)
    return dead, [ranks[i] for i in range(size - 1)], [following[i] for i in range(size - 1)]


def storm_worker(prefix, additive=False):
    import stormpy
    import stormpy.info

    size, edges, labels, outputs = read_model(prefix)
    dead, ranks, following = boundary(size, edges, labels)
    builder = stormpy.ExactSparseMatrixBuilder(rows=size, columns=size, entries=len(edges))
    for i, j, probability in sorted(edges, key=lambda edge: edge[:2]):
        builder.add_next_value(i, j, stormpy.Rational(str(probability)))
    transitions = builder.build()
    labeling = stormpy.StateLabeling(size)
    for label, states in labels.items():
        labeling.add_label(label)
        for state in states | (dead if label == "done" else set()):
            labeling.add_label_to_state(label, state)

    def query(rewards):
        vector = [stormpy.Rational(str(q)) for q in rewards]
        components = stormpy.SparseExactModelComponents(transitions, labeling,
            {"": stormpy.SparseExactRewardModel(optional_state_reward_vector=vector)})
        model = stormpy.SparseExactDtmc(components)
        prop, = stormpy.parse_properties(PROPERTY)
        result = stormpy.model_checking(model, prop, only_initial_states=False)
        values = [Fraction(str(q)) for q in result.get_values()]
        if len(values) != size or values[-1] != 0:
            raise ValueError("unexpected Storm vector dimensions or sink value")
        return values[:-1]

    if additive:
        import storm_additive
        additive_edges = storm_additive.read_edges(prefix, size, edges, labels)
        values = storm_additive.moments(query, size, labels, outputs, dead, additive_edges)
    else:
        positive = query([max(q, 0) for q in outputs])
        negative = query([max(-q, 0) for q in outputs])
        values = {
            "mass": query([int(i in labels["returned"]) for i in range(size)]),
            "rejection": query([int(i in labels["rejected"]) for i in range(size)]),
            "first": [p - n for p, n in zip(positive, negative)],
            "second": query([q*q for q in outputs]),
        }
    results = {"storm_version": stormpy.info.storm_version(),
               "storm_build_type": stormpy.info.storm_build_type(),
               "values": {name: [str(q) for q in vector] for name, vector in values.items()},
               "dead": [i in dead for i in range(size - 1)],
               "rank": ranks, "next": following}
    Path(str(prefix) + ".storm-values.json").write_text(json.dumps(results) + "\n")


def bind_report(text, answer, additive):
    def rat(q):
        return f"(({q.numerator} : Rat) / {q.denominator})"

    def optional(q):
        return "none" if q is None else f"some {rat(q)}"

    mass, first, second, rejection = (answer[k] for k in ("mass", "first", "second", "rejection"))
    rejection_term = "solution.rejection model.initial" if additive else "(termination.statistics model).rejectionProbability"
    text += f"""
theorem reportedStatistics :
    statistics.returnMass = {rat(mass)} ∧
    statistics.firstMoment = {rat(first)} ∧
    statistics.secondMoment = {rat(second)} ∧
    {rejection_term} = {rat(rejection)} ∧
    1 - statistics.returnMass - {rejection_term} = {rat(1-mass-rejection)} ∧
    statistics.conditionalMean = {optional(first/mass if mass else None)} ∧
    statistics.conditionalVariance = {optional(second/mass-(first/mass)**2 if mass else None)} := by
  decide +kernel

#print axioms reportedStatistics
"""
    return text, answer


def certificate_text(prefix, result, additive=False):
    size, _, labels, _ = read_model(prefix)
    n = size - 1
    dead = result["dead"]
    values = result["values"]
    if len(dead) != n or any(type(b) is not bool for b in dead):
        raise ValueError("invalid divergent-state vector")
    expected = {"mass", "first", "second", "rejection"}
    if set(values) != expected or any(len(v) != n for v in values.values()):
        raise ValueError("invalid moment vector dimensions")
    ranks, following = result["rank"], result["next"]
    if len(ranks) != n or any(type(r) is not int or r < 0 for r in ranks):
        raise ValueError("invalid path ranks")
    if len(following) != n or any(type(j) is not int or not 0 <= j < n for j in following):
        raise ValueError("invalid path successors")
    if any(type(q) is not str for vector in values.values() for q in vector):
        raise ValueError("expected exact rational strings")
    values = {name: [Fraction(q) for q in vector] for name, vector in values.items()}
    if additive:
        import storm_additive
        text = storm_additive.certificate_text(prefix, values, dead, ranks, following)
        initial, = labels["init"]
        return bind_report(text, {name: vector[initial] for name, vector in values.items()}, additive)

    def rat(q):
        return f"(({q.numerator} : Rat) / {q.denominator})"

    text = Path(str(prefix) + ".replay.lean").read_text().replace(
        "import Determinize.Checking.FiniteModel", "import Determinize.Checking.Statistics")
    text += "\nopen Determinize.Proof.FiniteModel\n"
    for name, vector in values.items():
        text += f"\ndef {name}Values : Vector Rat model.size := ⟨#[{', '.join(map(rat, vector))}], by decide +kernel⟩\n"
    text += f"\ndef deadStates : Vector Bool model.size := ⟨#[{', '.join(str(b).lower() for b in dead)}], by decide +kernel⟩\n"
    text += f"\ndef ranks : Vector Nat model.size := ⟨#[{', '.join(map(str, ranks))}], by decide +kernel⟩\n"
    destinations = ', '.join(f"⟨{j}, by decide +kernel⟩" for j in following)
    text += f"\ndef nextStates : Vector (Fin model.size) model.size := ⟨#[{destinations}], by rfl⟩\n"
    state_proofs = "\n".join(
        f"theorem result_{i} : resultClaim (⟨{i}, by decide +kernel⟩ : Fin model.size) := by\n  decide +kernel\n"
        for i in range(n))
    evidence_entries = ", ".join(f"⟨⟨{i}, by decide +kernel⟩, result_{i}⟩" for i in range(n))
    text += f"""
def result : MomentCertificate model where
  dead := fun i => deadStates[i]
  rank := fun i => ranks[i]
  next := fun i => nextStates[i]
  values := fun moment i => (match moment with
    | .mass => massValues | .first => firstValues | .second => secondValues)[i]

def termination : TerminationCertificate model := ⟨result, fun i => rejectionValues[i]⟩

abbrev resultClaim (i : Fin model.size) : Prop := candidate.QueryStateValid graphValid termination i
{state_proofs}
def resultEvidence : Vector (Subtype (fun i : Fin model.size => resultClaim i)) model.size := ⟨#[{evidence_entries}], by rfl⟩
theorem resultIndices : ∀ i : Fin model.size, (resultEvidence[i]).val = i := by decide +kernel
theorem allResults (i : Fin model.size) : resultClaim i := (resultIndices i) ▸ (resultEvidence[i]).property

theorem terminationAccepted : Determinize.Checking.checkTermination model termination = true :=
  (Determinize.Checking.checkTermination_valid model termination).mpr
    (sparseResults_valid candidate graphValid termination allResults)

theorem resultAccepted : Determinize.Checking.checkStatistics model result = true := by
  exact (Determinize.Checking.checkStatistics_valid model result).mpr
    ((Determinize.Checking.checkTermination_valid model termination).mp terminationAccepted).1

def statistics := result.statistics model

theorem outputStatistics : statistics.Matches (bigStepMeasure (checkedSubject.program checkedSource)) :=
  Determinize.Checking.checked_statistics ⟨model, modelMatches⟩ result resultAccepted

theorem conditionalVariance (positive : 0 < statistics.returnMass) :
    ProbabilityTheory.variance id ((bigStepMeasure (checkedSubject.program checkedSource) Set.univ)⁻¹ •
      bigStepMeasure (checkedSubject.program checkedSource)) =
      ((statistics.secondMoment / statistics.returnMass - (statistics.firstMoment / statistics.returnMass)^2 : Rat) : ℝ) :=
  Determinize.Checking.checked_conditionalVariance ⟨model, modelMatches⟩ result resultAccepted positive

theorem terminationProbabilities : (termination.statistics model).Matches model :=
  Determinize.Checking.checked_termination model termination terminationAccepted

#print axioms terminationProbabilities
#print axioms resultAccepted
#print axioms outputStatistics
#print axioms conditionalVariance
"""
    initial, = labels["init"]
    return bind_report(text, {name: vector[initial] for name, vector in values.items()}, additive)


def checked_axioms(output, additive=False):
    reports = {name: {item.strip() for item in axioms.split(",") if item.strip()}
               for name, axioms in re.findall(r"'([^']+)' depends on axioms:\s*\[([^]]*)\]", output)}
    for name in re.findall(r"'([^']+)' does not depend on any axioms", output):
        reports[name] = set()
    required = {"outputStatistics", "terminationProbabilities", "conditionalVariance", "reportedStatistics"}
    if not required <= reports.keys():
        raise ValueError("missing certificate axiom reports")
    if additive:
        required |= {"integrability", "checkedResult", "modelMatches"}
        if not required <= reports.keys():
            raise ValueError("missing additive certificate axiom reports")
    allowed = {"propext", "Classical.choice", "Quot.sound"}
    for name, axioms in reports.items():
        if axioms - allowed:
            raise ValueError(f"unexpected axioms for {name}: {sorted(axioms - allowed)}")
    return {name: sorted(reports[name]) for name in sorted(required)}


def run_command(argv, *, cwd, timeout):
    with subprocess.Popen(argv, cwd=cwd, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, start_new_session=True) as process:
        try:
            stdout, stderr = process.communicate(timeout=timeout)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.communicate()
            raise
        return subprocess.CompletedProcess(argv, process.returncode, stdout, stderr)


def run(args):
    prefix = args.prefix.resolve()
    report = {"status": "running", "subject": args.subject, "property": PROPERTY,
              "mode": "additive" if getattr(args, "additive", False) else "terminal",
              "engine": "stormpy sparse exact DTMC, rational arithmetic", "result_source": "storm",
              "timeout_seconds": args.timeout, "stage": "model export", "commands": []}
    report_path = Path(str(prefix) + ".storm.json")
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(report, indent=2) + "\n")

    def command(argv, cwd=ROOT):
        report["commands"].append([str(arg) for arg in argv])
        started = time.monotonic()
        completed = run_command(argv, cwd=cwd, timeout=args.timeout)
        report.setdefault("logs", []).append({"stdout": completed.stdout, "stderr": completed.stderr,
                                             "exit_code": completed.returncode,
                                             "seconds": time.monotonic() - started})
        if completed.returncode:
            raise RuntimeError(completed.stderr or completed.stdout or f"exit {completed.returncode}")
        return completed

    try:
        report["stormpy_version"] = importlib.metadata.version("stormpy")
        mode = ["--additive"] if getattr(args, "additive", False) else []
        command([args.binary, *mode, "--check", "--export", prefix, "--subject", args.subject,
                 "--max-states", str(getattr(args, "max_states", 10000)), args.file.resolve()])
        report["stage"] = "Storm"
        values_path = Path(str(prefix) + ".storm-values.json")
        values_path.unlink(missing_ok=True)
        command([sys.executable, Path(__file__).resolve(), "--worker", prefix, *mode])
        values = json.loads(values_path.read_text())
        text, answer = certificate_text(prefix, values, additive=bool(mode))
        certificate = Path(str(prefix) + ".storm.lean")
        certificate.write_text(text)
        if getattr(args, "skip_certificate", False):
            report.update(kernel_checked=False, certificate_check_skipped=True)
        else:
            report["stage"] = "kernel check"
            checked = command(["lake", "env", "lean", certificate], ROOT / "lean")
            report["axioms"] = checked_axioms(checked.stdout + checked.stderr, additive=bool(mode))
            report["kernel_checked"] = True
        report.update(storm_version=values["storm_version"],
                      storm_build_type=values["storm_build_type"],
                      exact_answer=str(answer["first"]), return_mass=str(answer["mass"]),
                      second_moment=str(answer["second"]))
        p = answer["mass"]
        report["termination_statistics_scope"] = "graph"
        report["rejection_probability"] = str(answer["rejection"])
        report["divergence_probability"] = str(1-p-answer["rejection"])
        report["conditional_mean"] = str(answer["first"] / p) if p else None
        report["conditional_variance"] = str(answer["second"] / p - (answer["first"] / p)**2) if p else None
        if getattr(args, "compare", False):
            report["stage"] = "solver comparison"
            command([args.binary, *mode, "--check", "--result", prefix, "--subject", args.subject,
                     "--max-states", str(getattr(args, "max_states", 10000)), args.file.resolve()])
            internal = json.loads(Path(str(prefix) + ".result.json").read_text())
            for external, key in [("first", "answer"), ("mass", "return_mass"), ("second", "second_moment"),
                                  ("rejection", "rejection_probability")]:
                if answer[external] != Fraction(internal[key]):
                    raise RuntimeError("Storm result disagrees with the internal solver")
        report["status"] = "completed"
        label = "Storm result" if getattr(args, "skip_certificate", False) else "Lean-certified Storm result"
        print(f"{label} ({args.subject}): {answer['first']}; return mass {p}")
        return 0
    except (OSError, RuntimeError, ValueError, ZeroDivisionError, KeyError, TypeError, subprocess.TimeoutExpired,
            importlib.metadata.PackageNotFoundError) as error:
        report.update(status="failed", error=str(error))
        print(str(error), file=sys.stderr)
        return 1
    finally:
        report_path.write_text(json.dumps(report, indent=2) + "\n")


def main():
    if len(sys.argv) in (3, 4) and sys.argv[1] == "--worker":
        if len(sys.argv) == 4 and sys.argv[3] != "--additive":
            raise ValueError("unknown worker mode")
        storm_worker(Path(sys.argv[2]), additive=len(sys.argv) == 4)
        return 0
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", type=Path)
    parser.add_argument("--prefix", required=True, type=Path)
    parser.add_argument("--subject", choices=("source", "determinized"), default="determinized")
    parser.add_argument("--binary", type=Path, default=ROOT / "lean/.lake/build/bin/determinize")
    parser.add_argument("--max-states", type=int, default=10000)
    parser.add_argument("--additive", action="store_true", help="extract outer evaluated additions as rewards")
    parser.add_argument("--skip-certificate", action="store_true", help="skip the Lean kernel certificate check")
    parser.add_argument("--compare", action="store_true", help="also compare with the internal solver")
    parser.add_argument("--timeout", type=float, default=120)
    return run(parser.parse_args())


if __name__ == "__main__":
    sys.exit(main())
