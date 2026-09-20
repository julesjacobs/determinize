#!/usr/bin/env python3
"""Run the exact finite and sampling benchmark directories."""

from __future__ import annotations

import argparse
import json
import math
import os
import re
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
LEAN = ROOT / "lean"
DEFAULT_BINARY = LEAN / ".lake" / "build" / "bin" / "determinize"
FINITE_DIR = ROOT / "benchmarks" / "exact" / "finite-after-det"
INFINITE_DIR = ROOT / "benchmarks" / "exact" / "infinite-after-det"
STORM = ROOT / "tools" / "storm.py"


@dataclass(frozen=True)
class SampleSummary:
    returned: int
    samples: int
    mean: float | None
    variance: float | None
    rejected: int
    failure: str | None


def positive(value: str) -> int:
    parsed = int(value)
    if parsed <= 0:
        raise argparse.ArgumentTypeError("expected a positive integer")
    return parsed


def heading(title: str) -> None:
    print()
    print("=" * 78)
    print(title)
    print("=" * 78)


def compact_error(text: str, limit: int = 500) -> str:
    lines = [line.strip() for line in text.splitlines() if line.strip()]
    result = " | ".join(lines[-4:]) if lines else "command failed without an error message"
    return result if len(result) <= limit else result[: limit - 3] + "..."


def parse_number(text: str | None) -> float | None:
    if text is None or text.startswith("unavailable"):
        return None
    try:
        value = float(text)
    except ValueError:
        return None
    return value if math.isfinite(value) else None


def parse_sample_summary(output: str, label: str, next_label: str | None) -> SampleSummary | None:
    start = output.find(f"{label}:")
    if start < 0:
        return None
    end = output.find(f"{next_label}:", start) if next_label else -1
    chunk = output[start:] if end < 0 else output[start:end]
    returned = re.search(rf"{re.escape(label)}: (\d+)/(\d+) runs returned a value", chunk)
    if returned is None:
        return None
    moments = re.search(
        r"empirical mean among returned values: ([^;\n]+); variance: ([^\n]+)", chunk
    )
    rejected = re.search(r"rejected observations: (\d+)", chunk)
    failure = re.search(r"first failure: ([^\n]+)", chunk)
    return SampleSummary(
        returned=int(returned.group(1)),
        samples=int(returned.group(2)),
        mean=parse_number(moments.group(1).strip()) if moments else None,
        variance=parse_number(moments.group(2).strip()) if moments else None,
        rejected=int(rejected.group(1)) if rejected else 0,
        failure=failure.group(1).strip() if failure else None,
    )


def format_float(value: float | None) -> str:
    return "n/a" if value is None else f"{value:.10g}"


def variance_factor(source: float | None, target: float | None) -> str:
    if source is None or target is None:
        return "n/a"
    if target == 0:
        return "infinite" if source > 0 else "n/a (both variances are zero)"
    return f"{source / target:.6g}x"


def print_sample(label: str, summary: SampleSummary) -> None:
    suffix = f", rejected {summary.rejected}" if summary.rejected else ""
    print(f"  {label:<14} returned {summary.returned}/{summary.samples}{suffix}")
    print(f"  {'':14} mean {format_float(summary.mean)}, variance {format_float(summary.variance)}")
    if summary.failure:
        print(f"  {'':14} first failure: {summary.failure}")


def run_infinite(models: list[Path], args: argparse.Namespace) -> int:
    heading(f"INFINITE-AFTER-DET: EMPIRICAL SAMPLING ({len(models)} benchmarks)")
    print(f"samples={args.samples}, seed={args.seed}, fuel={args.fuel}")
    failures = 0
    for index, model in enumerate(models, 1):
        print(f"\n[{index}/{len(models)}] {model.name}")
        command = [
            str(args.binary), "--check", "--samples", str(args.samples),
            "--seed", str(args.seed), "--fuel", str(args.fuel), str(model),
        ]
        started = time.monotonic()
        try:
            completed = subprocess.run(
                command, cwd=ROOT, text=True, capture_output=True,
                timeout=args.sample_timeout,
            )
        except subprocess.TimeoutExpired:
            failures += 1
            print(f"  ERROR: sampling timed out after {args.sample_timeout:g}s")
            continue
        elapsed = time.monotonic() - started
        if completed.returncode:
            failures += 1
            print(f"  ERROR: {compact_error(completed.stderr or completed.stdout)}")
            continue
        source = parse_sample_summary(completed.stdout, "Source", "Determinized")
        target = parse_sample_summary(completed.stdout, "Determinized", None)
        if source is None or target is None:
            failures += 1
            print("  ERROR: could not parse sampler output")
            print(f"  {compact_error(completed.stdout)}")
            continue
        print_sample("Source", source)
        print_sample("Determinized", target)
        print(f"  Variance reduction: {variance_factor(source.variance, target.variance)}")
        print(f"  Sampling time:      {elapsed:.3f}s")
    return failures


def transition_count(path: Path) -> int | None:
    try:
        with path.open() as lines:
            return max(sum(1 for _ in lines) - 1, 0)
    except OSError:
        return None


def run_finite(models: list[Path], args: argparse.Namespace) -> int:
    heading(f"FINITE-AFTER-DET: EXACT STORM RESULTS ({len(models)} benchmarks)")
    print(
        "subject=determinized, additive=yes, certificate=skipped, "
        f"max-states={args.max_states}, timeout={args.storm_timeout:g}s"
    )
    failures = 0
    storm_python = os.environ.get("STORM_PYTHON", sys.executable)
    with tempfile.TemporaryDirectory(prefix="determinize-bench-") as temporary:
        temp = Path(temporary)
        for index, model in enumerate(models, 1):
            print(f"\n[{index}/{len(models)}] {model.name}")
            prefix = temp / model.stem
            command = [
                storm_python, str(STORM), str(model), "--prefix", str(prefix),
                "--binary", str(args.binary), "--subject", "determinized",
                "--additive", "--skip-certificate",
                "--max-states", str(args.max_states),
                "--timeout", str(args.storm_timeout),
            ]
            completed = subprocess.run(command, cwd=ROOT, text=True, capture_output=True)
            report_path = Path(str(prefix) + ".storm.json")
            try:
                report = json.loads(report_path.read_text())
            except (OSError, json.JSONDecodeError):
                report = {}
            if completed.returncode or report.get("status") != "completed":
                failures += 1
                error = report.get("error") or completed.stderr or completed.stdout
                print(f"  ERROR at {report.get('stage', 'unknown stage')}: {compact_error(str(error))}")
                logs = report.get("logs", [])
                if logs:
                    print(
                        "  Completed phase time: "
                        f"{sum(float(log.get('seconds', 0)) for log in logs):.3f}s"
                    )
                continue

            values_path = Path(str(prefix) + ".storm-values.json")
            try:
                values = json.loads(values_path.read_text())
                program_states = len(values["values"]["mass"])
                storm_states = program_states + 1
            except (OSError, json.JSONDecodeError, KeyError, TypeError):
                program_states = None
                storm_states = None

            logs = report.get("logs", [])
            export_seconds = float(logs[0].get("seconds", 0)) if len(logs) >= 1 else None
            storm_seconds = float(logs[1].get("seconds", 0)) if len(logs) >= 2 else None
            measured = [value for value in (export_seconds, storm_seconds) if value is not None]
            total_seconds = sum(measured) if len(measured) == 2 else None
            transitions = transition_count(Path(str(prefix) + ".tra"))

            print(f"  Expected value:     {report.get('exact_answer', 'n/a')}")
            print(f"  Return mass:        {report.get('return_mass', 'n/a')}")
            print(f"  Conditional mean:   {report.get('conditional_mean', 'n/a')}")
            if storm_states is None:
                print("  States:             n/a")
            else:
                print(
                    f"  States:             {storm_states} Storm states "
                    f"({program_states} program states + done sink)"
                )
            print(f"  Transitions:        {transitions if transitions is not None else 'n/a'}")
            if total_seconds is None:
                print("  Measured time:      n/a")
            else:
                print(f"  Measured time:      {total_seconds:.3f}s")
                print(f"    model export:     {export_seconds:.3f}s")
                print(f"    Storm solve:      {storm_seconds:.3f}s")
    return failures


def build(binary: Path) -> None:
    print("Building determinize once (excluded from benchmark timings)...", flush=True)
    completed = subprocess.run(
        ["lake", "build", "determinize"], cwd=LEAN, text=True, capture_output=True
    )
    if completed.returncode:
        raise SystemExit(compact_error(completed.stderr or completed.stdout))
    if not binary.is_file():
        raise SystemExit(f"determinize binary not found after build: {binary}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--finite", action="store_true", help="run only finite-after-det benchmarks")
    parser.add_argument("--infinite", action="store_true", help="run only infinite-after-det benchmarks")
    parser.add_argument("--samples", type=positive, default=1000, help="samples per infinite benchmark (default: 1000)")
    parser.add_argument("--seed", type=int, default=0, help="initial sampler seed (default: 0)")
    parser.add_argument("--fuel", type=positive, default=100000, help="fuel per sample (default: 100000)")
    parser.add_argument("--sample-timeout", type=positive, default=600, help="timeout per sampling benchmark in seconds")
    parser.add_argument("--max-states", type=positive, default=100000, help="finite exploration state limit")
    parser.add_argument("--storm-timeout", type=positive, default=600, help="timeout per Storm phase in seconds")
    parser.add_argument("--binary", type=Path, default=DEFAULT_BINARY, help="determinize executable")
    parser.add_argument("--no-build", action="store_true", help="use the existing determinize executable")
    args = parser.parse_args()

    run_only = args.finite or args.infinite
    do_finite = args.finite or not run_only
    do_infinite = args.infinite or not run_only
    args.binary = args.binary.resolve()

    if not args.no_build:
        build(args.binary)
    elif not args.binary.is_file():
        raise SystemExit(f"determinize binary not found: {args.binary}")

    failures = 0
    if do_finite:
        failures += run_finite(sorted(FINITE_DIR.glob("*.det")), args)
    if do_infinite:
        failures += run_infinite(sorted(INFINITE_DIR.glob("*.det")), args)

    heading("SUMMARY")
    if failures:
        print(f"Completed with {failures} benchmark failure(s).")
        return 1
    print("All selected benchmarks completed successfully.")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except KeyboardInterrupt:
        print("\nInterrupted.", file=sys.stderr)
        raise SystemExit(130)
