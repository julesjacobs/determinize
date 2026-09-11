#!/usr/bin/env python3
"""Compare Storm's explicit-model result with a kernel-checked Lean certificate."""
import argparse
from fractions import Fraction
import importlib.metadata
import json
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
PROPERTY = 'R=? [ F "done" ]'


def storm_worker(prefix):
    import stormpy
    import stormpy.info

    lines = Path(str(prefix) + ".tra").read_text().splitlines()
    if lines[0] != "dtmc":
        raise ValueError("expected an explicit DTMC")
    edges = [(int(i), int(j), stormpy.Rational(q))
             for i, j, q in (line.split() for line in lines[1:])]
    size = 1 + max(max(i, j) for i, j, _ in edges)
    builder = stormpy.ExactSparseMatrixBuilder(rows=size, columns=size, entries=len(edges))
    for i, j, probability in sorted(edges, key=lambda edge: edge[:2]):
        builder.add_next_value(i, j, probability)
    transitions = builder.build()
    labeling = stormpy.StateLabeling(size)
    labels = Path(str(prefix) + ".lab").read_text().splitlines()
    if labels[0] != "#DECLARATION" or labels[2] != "#END":
        raise ValueError("expected explicit label declarations")
    for label in labels[1].split():
        labeling.add_label(label)
    for line in labels[3:]:
        state, *names = line.split()
        for name in names:
            labeling.add_label_to_state(name, int(state))
    results = {"storm_version": stormpy.info.storm_version(),
               "storm_build_type": stormpy.info.storm_build_type()}
    for sign in ("positive", "negative"):
        rewards = [stormpy.Rational(0) for _ in range(size)]
        for line in Path(str(prefix) + f".{sign}.state.rew").read_text().splitlines():
            state, reward = line.split()
            rewards[int(state)] = stormpy.Rational(reward)
        components = stormpy.SparseExactModelComponents(transitions, labeling,
            {"": stormpy.SparseExactRewardModel(optional_state_reward_vector=rewards)})
        model = stormpy.SparseExactDtmc(components)
        initial, = model.initial_states
        prop, = stormpy.parse_properties(PROPERTY)
        results[sign] = str(stormpy.model_checking(model, prop).at(initial))
    Path(str(prefix) + ".storm-values.json").write_text(json.dumps(results) + "\n")


def run(args):
    prefix = args.prefix.resolve()
    report = {"status": "running", "subject": args.subject, "property": PROPERTY,
              "engine": "stormpy sparse exact DTMC, rational arithmetic",
              "timeout_seconds": args.timeout, "stage": "certificate generation", "commands": []}
    report_path = Path(str(prefix) + ".storm.json")
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(report, indent=2) + "\n")

    def command(argv, cwd=ROOT):
        report["commands"].append([str(arg) for arg in argv])
        result = subprocess.run(argv, cwd=cwd, text=True, capture_output=True,
                                timeout=args.timeout)
        report.setdefault("logs", []).append({"stdout": result.stdout, "stderr": result.stderr,
                                             "exit_code": result.returncode})
        if result.returncode:
            raise RuntimeError(result.stderr or result.stdout or f"exit {result.returncode}")

    try:
        report["stormpy_version"] = importlib.metadata.version("stormpy")
        command([args.binary, "--result", prefix, "--subject", args.subject,
                 "--max-result-states", str(args.max_result_states), args.file.resolve()])
        report["stage"] = "kernel check"
        command(["lake", "env", "lean", str(prefix) + ".result.lean"], ROOT / "lean")
        report["kernel_checked"] = True
        report["stage"] = "Storm"
        values_path = Path(str(prefix) + ".storm-values.json")
        values_path.unlink(missing_ok=True)
        command([sys.executable, Path(__file__).resolve(), "--worker", prefix])
        values = json.loads(values_path.read_text())
        exact = json.loads(Path(str(prefix) + ".result.json").read_text())["answer"]
        answer = Fraction(values["positive"]) - Fraction(values["negative"])
        report.update(values, exact_answer=exact, storm_answer=str(answer))
        if answer != Fraction(exact):
            raise RuntimeError("Storm result disagrees with the certified exact answer")
        report["status"] = "completed"
        print(f"Certified expected terminal reward ({args.subject}): {exact}; Storm agrees")
        return 0
    except (OSError, RuntimeError, ValueError, subprocess.TimeoutExpired,
            importlib.metadata.PackageNotFoundError) as error:
        report.update(status="failed", error=str(error))
        print(str(error), file=sys.stderr)
        return 1
    finally:
        report_path.write_text(json.dumps(report, indent=2) + "\n")


def main():
    if len(sys.argv) == 3 and sys.argv[1] == "--worker":
        storm_worker(Path(sys.argv[2]))
        return 0
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", type=Path)
    parser.add_argument("--prefix", required=True, type=Path)
    parser.add_argument("--subject", choices=("source", "determinized"), default="determinized")
    parser.add_argument("--binary", type=Path, default=ROOT / "lean/.lake/build/bin/determinize")
    parser.add_argument("--max-result-states", type=int, default=256)
    parser.add_argument("--timeout", type=float, default=120)
    return run(parser.parse_args())


if __name__ == "__main__":
    sys.exit(main())
