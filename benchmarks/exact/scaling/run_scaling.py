#!/usr/bin/env python3
"""Generate, time, and plot frontend-only determinization scaling.

Run: python3 benchmarks/exact/scaling/run_scaling.py
Edit SIZES and REPETITIONS below to change the experiment. Generated .det
files stay in this directory; measurements and PNG/PDF plots go to results/scaling.
The CLI's internal timer includes parsing, elaboration, inference, and replacement,
but excludes process startup, file I/O, formatting, sampling, and exact analysis.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import tempfile

# Experiment settings: k counts nested uniform draws or explicit input-list elements.
SIZES = [10, 25, 50, 100, 200, 400]
REPETITIONS = 10
WARMUPS = 1
TIMEOUT_SECONDS = 180

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
BINARY = ROOT / "lean/.lake/build/bin/determinize"
OUTPUT = ROOT / "results/scaling"
FAMILIES = {
    "uniform-nested": "Uniform nested",
    "gauss-random-walk": "Gaussian random walk",
    "add-noise": "Add noise",
}


def generate(family: str, k: int) -> str:
    lines = []
    if family == "uniform-nested":
        lines.append("let x0 = 1 in")
        for i in range(1, k + 1):
            lines.append(f"let x{i} = uniform(0, x{i - 1}) in")
        lines.append(f"x{k}")
    elif family in ("gauss-random-walk", "add-noise"):
        filename = ("gauss_randwalk_finalPos.det" if family == "gauss-random-walk"
                    else "addNoise.det")
        original = (ROOT / "benchmarks/exact" / filename).read_text()
        body = original[original.rfind("*)") + 2:].strip()
        binding = "times" if family == "gauss-random-walk" else "database"
        values = " :: ".join(str(i) for i in range(1, k + 1)) + " :: []"
        body, count = re.subn(rf"let {binding} = [^\n]+ in",
                             f"let {binding} = {values} in", body)
        if count != 1:
            raise ValueError(f"Could not find the explicit {binding} list in {filename}")
        return body + "\n"
    else:
        raise ValueError(family)
    return "\n".join(lines) + "\n"


def measure(path: Path, family: str, k: int, binary: Path) -> int:
    run = subprocess.run([str(binary), "--det-only", str(path)], cwd=ROOT,
                         text=True, capture_output=True, timeout=TIMEOUT_SECONDS)
    if run.returncode:
        raise RuntimeError((run.stderr or run.stdout).strip())
    timing = re.search(r"^Determinization time \(ns\): (\d+)$", run.stdout, re.M)
    if timing is None:
        raise RuntimeError("Missing internal timing; rebuild with lake build determinize.")
    counts = re.findall(
        r"Sampling sites (before|after) determinization: discrete=(\d+), continuous=(\d+)",
        run.stdout,
    )
    expected = {"before": (0, k if family == "uniform-nested" else 1),
                "after": (0, 0)}
    observed = {when: (int(d), int(c)) for when, d, c in counts}
    if observed != expected:
        raise RuntimeError(f"Unexpected inferred sampling sites: {observed}; expected {expected}")
    return int(timing.group(1))


def percentile(values: list[float], fraction: float) -> float:
    ordered = sorted(values)
    index = fraction * (len(ordered) - 1)
    lo = int(index)
    hi = min(lo + 1, len(ordered) - 1)
    return ordered[lo] + (ordered[hi] - ordered[lo]) * (index - lo)


def write_csv(path: Path, rows: list[dict]) -> None:
    if not rows:
        return
    with path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def plot(rows: list[dict]) -> None:
    os.environ.setdefault("MPLCONFIGDIR", str(Path(tempfile.gettempdir()) / "determinize-matplotlib"))
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    for family, label in FAMILIES.items():
        series = [row for row in rows if row["family"] == family]
        if not series:
            continue
        x = [row["k"] for row in series]
        y = [row["median_ms"] for row in series]
        low = [row["q25_ms"] for row in series]
        high = [row["q75_ms"] for row in series]
        fig, ax = plt.subplots(figsize=(6, 4))
        line, = ax.plot(x, y, "o-", label=label, linewidth=1, markersize=4)
        ax.fill_between(x, low, high, alpha=0.35, color=line.get_color())
        ax.set_xlabel("Number of nested uniform draws (k)" if family == "uniform-nested"
                      else "Number of input list elements (k)",
                      fontsize=14)
        ax.set_ylabel("Determinization time (ms)", fontsize=14)
        ax.tick_params(axis="both", labelsize=12, width=1.3, length=5)
        for spine in ax.spines.values():
            spine.set_linewidth(1.3)
        ax.grid(alpha=0.25)
        fig.tight_layout()
        for extension in ("png", "pdf"):
            fig.savefig(OUTPUT / f"{family}.{extension}", dpi=200)
        plt.close(fig)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sizes", nargs="+", type=int, default=SIZES)
    parser.add_argument("--repetitions", type=int, default=REPETITIONS)
    parser.add_argument("--no-build", action="store_true")
    parser.add_argument("--binary", type=Path, default=BINARY)
    args = parser.parse_args()
    if args.repetitions < 1 or any(k < 1 for k in args.sizes):
        parser.error("sizes and repetitions must be positive")
    binary = args.binary.resolve()
    if not args.no_build:
        subprocess.run(["lake", "build", "determinize"], cwd=ROOT / "lean", check=True)
    if not binary.is_file():
        parser.error(f"missing executable: {binary}")
    OUTPUT.mkdir(parents=True, exist_ok=True)
    raw, summaries, failures = [], [], []
    for family in FAMILIES:
        for k in sorted(set(args.sizes)):
            source = generate(family, k)
            path = HERE / f"{family}-{k}.det"
            path.write_text(source)
            size = len(source.encode("utf-8"))
            base = {"family": family, "k": k, "source_bytes": size}
            print(f"{family}: k={k}, {size} bytes", flush=True)
            samples = []
            try:
                for _ in range(WARMUPS):
                    measure(path, family, k, binary)
                for repetition in range(args.repetitions):
                    ns = measure(path, family, k, binary)
                    samples.append(ns / 1_000_000)
                    raw.append({**base, "repetition": repetition + 1,
                                "nanoseconds": ns, "milliseconds": ns / 1_000_000})
                summaries.append({**base, "repetitions": len(samples),
                                  "median_ms": statistics.median(samples),
                                  "q25_ms": percentile(samples, 0.25),
                                  "q75_ms": percentile(samples, 0.75)})
                print(f"  median {statistics.median(samples):.3f} ms", flush=True)
            except (RuntimeError, subprocess.TimeoutExpired) as error:
                failures.append({**base, "error": str(error)})
                print(f"  FAILED: {error}", flush=True)
            write_csv(OUTPUT / "runs.csv", raw)
            write_csv(OUTPUT / "summary.csv", summaries)
            (OUTPUT / "experiment.json").write_text(json.dumps({
                "sizes": sorted(set(args.sizes)), "repetitions": args.repetitions,
                "warmups": WARMUPS, "timeout_seconds": TIMEOUT_SECONDS,
                "timed_phases": ["parse", "elaborate", "infer", "determinize"],
                "size_metric": "k: nested uniform draws or explicit input-list elements",
                "binary": str(binary), "failures": failures,
            }, indent=2) + "\n")
    if summaries:
        plot(summaries)
    print(f"Saved measurements and plots to {OUTPUT}")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
