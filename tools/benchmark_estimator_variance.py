#!/usr/bin/env python3
"""Measure estimator variance and sampling time with the compiled Lean runtime.

Build with `cd lean && lake build sampling-bench`, then run this script with
Python and matplotlib. Use --resume after interruption or --plot-only to
regenerate summaries and figures from the saved per-replicate measurements.
"""

from __future__ import annotations

import argparse
from collections import defaultdict
import csv
from datetime import datetime, timezone
import hashlib
import json
import math
from pathlib import Path
import platform
import statistics
import struct
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
MODELS = ("branching", "hmm", "load")
SIZES = (1000, 3000, 10000, 30000, 100000)


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def command_output(args):
    result = subprocess.run(args, cwd=ROOT, text=True, capture_output=True, check=True)
    return result.stdout.strip()


def git_commit():
    try:
        return command_output(["git", "rev-parse", "HEAD"])
    except (OSError, subprocess.CalledProcessError):
        return None


def machine():
    result = {"system": platform.platform(), "architecture": platform.machine()}
    if platform.system() == "Darwin":
        for name, key in (("cpu", "machdep.cpu.brand_string"),
                          ("memory_bytes", "hw.memsize"), ("model", "hw.model")):
            result[name] = command_output(["sysctl", "-n", key])
    return result


def trials(models, sizes, replicates):
    seed = 0
    for model in models:
        for size in sizes:
            for replicate in range(replicates):
                subjects = ("source", "determinized")
                if replicate % 2:
                    subjects = subjects[::-1]
                for subject in subjects:
                    yield {"benchmark": model, "samples": size, "replicate": replicate,
                           "subject": subject, "seed": seed}
                seed += size


def key(row):
    return tuple(row[name] for name in ("benchmark", "samples", "replicate", "subject"))


def decode_float(bits):
    return struct.unpack("!d", int(bits).to_bytes(8, "big"))[0]


def validate_measurement(measured, samples):
    if measured["failed"] or measured["first_failure"]:
        raise ValueError(f'{measured["failed"]}/{samples} executions failed: '
                         f'{measured["first_failure"]}')
    if measured["requested"] != samples or \
            measured["returned"] + measured["rejected"] != samples:
        raise ValueError("incomplete execution counts")
    if measured["returned"] < 2 or measured["sampling_ns"] <= 0:
        raise ValueError("insufficient returned values or invalid elapsed time")
    mean = decode_float(measured["mean_bits"])
    m2 = decode_float(measured["m2_bits"])
    if not math.isfinite(mean) or not math.isfinite(m2) or m2 < 0:
        raise ValueError("non-finite or invalid moments")
    return {**measured, "estimate": mean, "output_variance": m2 / measured["returned"],
            "sampling_seconds": measured["sampling_ns"] / 1e9}


def summarize(rows):
    groups = defaultdict(list)
    for row in rows:
        if row["status"] != "completed":
            raise ValueError(f"cannot summarize failed trial {key(row)}")
        groups[row["benchmark"], row["samples"], row["subject"]].append(row)
    result = []
    for (model, size, subject), group in sorted(groups.items()):
        estimates = [row["estimate"] for row in group]
        elapsed = [row["sampling_seconds"] for row in group]
        variance = statistics.variance(estimates)
        result.append({
            "benchmark": model, "samples": size, "subject": subject,
            "replicates": len(group),
            "mean_returned": statistics.mean(row["returned"] for row in group),
            "mean_rejected": statistics.mean(row["rejected"] for row in group),
            "mean_estimate": statistics.mean(estimates),
            "estimator_variance": variance,
            "estimator_variance_se": variance * math.sqrt(2 / (len(group) - 1)),
            "plug_in_estimator_variance": statistics.mean(
                row["output_variance"] / (row["returned"] - 1) for row in group),
            "mean_sampling_seconds": statistics.mean(elapsed),
            "sampling_seconds_stdev": statistics.stdev(elapsed),
        })
    return result


def plots(summary, output):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    plt.rcParams.update({"font.size": 11, "axes.labelsize": 11, "legend.fontsize": 10})
    for model in sorted({row["benchmark"] for row in summary}):
        fig, ax = plt.subplots(figsize=(3.2, 2.8), layout="constrained")
        for subject, label, marker, color in (
            ("source", "Source", "o", "#2463a6"),
            ("determinized", "Determinized", "s", "#d56a28"),
        ):
            rows = [row for row in summary
                    if row["benchmark"] == model and row["subject"] == subject]
            ax.errorbar([row["mean_sampling_seconds"] for row in rows],
                        [row["estimator_variance"] for row in rows],
                        yerr=[row["estimator_variance_se"] for row in rows],
                        marker=marker, color=color, linewidth=1.5, markersize=4,
                        capsize=3, label=label)
        ax.set(xscale="log", yscale="log", xlabel="Sampling time (s)",
               ylabel=r"$\mathrm{Var}(\hat{\mu}_N)$")
        ax.grid(True, which="major", alpha=.25)
        ax.legend()
        fig.savefig(output / f"{model}.png", dpi=220)
        plt.close(fig)


def save_summary(rows, output):
    summary = summarize(rows)
    with (output / "summary.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(summary[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(summary)
    plots(summary, output)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path,
                        default=ROOT / "lean/.lake/build/bin/sampling-bench")
    parser.add_argument("--output", type=Path, default=ROOT / "results/estimator-variance")
    parser.add_argument("--models", nargs="+", choices=MODELS, default=MODELS)
    parser.add_argument("--sizes", nargs="+", type=int, default=SIZES)
    parser.add_argument("--replicates", type=int, default=50)
    parser.add_argument("--fuel", type=int, default=100000)
    parser.add_argument("--warmup", type=int, default=1000)
    parser.add_argument("--timeout", type=float, default=180)
    parser.add_argument("--resume", action="store_true")
    parser.add_argument("--plot-only", action="store_true")
    args = parser.parse_args()
    if args.replicates < 2 or min(args.sizes) < 2 or args.fuel < 1 or \
            args.warmup < 0 or args.timeout <= 0:
        parser.error("need at least two replicates/samples, positive fuel/timeout, nonnegative warmup")
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    runs_path = output / "runs.jsonl"
    metadata_path = output / "experiment.json"
    if args.plot_only:
        rows = [json.loads(line) for line in runs_path.read_text().splitlines()]
        metadata = json.loads(metadata_path.read_text())
        if metadata["status"] != "completed":
            raise ValueError("experiment is incomplete")
        save_summary(rows, output)
        return 0
    config = {
        "models": list(args.models), "sizes": list(args.sizes), "replicates": args.replicates,
        "fuel": args.fuel, "warmup": args.warmup, "timeout_seconds": args.timeout,
        "seed_policy": "Disjoint consecutive seed blocks per replicate and size; paired subjects.",
        "subject_order": "Alternate source-first and determinized-first by replicate.",
        "timing": "Runtime loop and online moments only; excludes parsing, determinization, warmup, output.",
        "variance_se": "Normal approximation: sample variance * sqrt(2 / (replicates - 1)).",
        "binary_sha256": sha256(args.binary),
        "model_sha256": {model: sha256(ROOT / "benchmarks/approx" / f"{model}.det")
                         for model in args.models},
        "harness_sha256": sha256(ROOT / "lean/SamplingBench.lean"),
        "runner_sha256": sha256(Path(__file__)),
    }
    expected = list(trials(args.models, args.sizes, args.replicates))
    if args.resume:
        metadata = json.loads(metadata_path.read_text())
        if metadata["configuration"] != config or metadata["machine"] != machine():
            raise ValueError("resume configuration or machine differs from saved experiment")
        rows = [json.loads(line) for line in runs_path.read_text().splitlines()]
        if any(row["status"] != "completed" for row in rows):
            raise ValueError("saved experiment contains a failed trial; rerun into a new directory")
    else:
        if runs_path.exists():
            raise ValueError(f"{runs_path} exists; use --resume or choose another output directory")
        metadata = {
            "started_at": datetime.now(timezone.utc).isoformat(), "machine": machine(),
            "configuration": config, "git_commit": git_commit(),
            "command": sys.argv, "binary": str(args.binary.resolve()),
            "lean_toolchain": (ROOT / "lean/lean-toolchain").read_text().strip(),
            "python": sys.version, "status": "running",
        }
        rows = []
    completed = {key(row) for row in rows}
    if len(completed) != len(rows) or not completed <= {key(row) for row in expected}:
        raise ValueError("unexpected or duplicate trials in saved experiment")
    metadata["status"] = "running"
    metadata_path.write_text(json.dumps(metadata, indent=2) + "\n")
    with runs_path.open("a") as stream:
        for trial in expected:
            if key(trial) in completed:
                continue
            command = [str(args.binary.resolve()),
                       str(ROOT / "benchmarks/approx" / f'{trial["benchmark"]}.det'),
                       trial["subject"], str(trial["samples"]), str(trial["seed"]),
                       str(args.fuel), str(args.warmup)]
            row = {**trial, "status": "failed"}
            try:
                process = subprocess.run(command, capture_output=True, text=True,
                                         timeout=args.timeout, check=False)
                row.update({"returncode": process.returncode, "stderr": process.stderr.strip()})
                if process.stdout.strip():
                    row.update(json.loads(process.stdout))
                else:
                    raise ValueError(f"sampler exited {process.returncode} without measurements: "
                                     f"{process.stderr.strip()}")
                row.update(validate_measurement(row, trial["samples"]))
                if process.returncode:
                    raise ValueError(f"sampler exited {process.returncode}: {process.stderr.strip()}")
                row["status"] = "completed"
            except Exception as error:
                row["error"] = str(error)
                stream.write(json.dumps(row, allow_nan=False) + "\n")
                stream.flush()
                metadata.update(status="failed", error=str(error), failed_trial=trial)
                metadata_path.write_text(json.dumps(metadata, indent=2) + "\n")
                print(f"ERROR: {key(trial)}: {error}", file=sys.stderr, flush=True)
                return 1
            stream.write(json.dumps(row, allow_nan=False) + "\n")
            stream.flush()
            rows.append(row)
            if len(rows) % 10 == 0:
                print(f'{len(rows)}/{len(expected)} trials: {trial["benchmark"]} '
                      f'N={trial["samples"]} replicate={trial["replicate"] + 1}', flush=True)
    metadata.update(status="completed", finished_at=datetime.now(timezone.utc).isoformat(),
                    completed_trials=len(rows),
                    requested_executions=sum(row["requested"] for row in rows),
                    rejected_executions=sum(row["rejected"] for row in rows),
                    failed_executions=sum(row["failed"] for row in rows))
    metadata_path.write_text(json.dumps(metadata, indent=2) + "\n")
    save_summary(rows, output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
