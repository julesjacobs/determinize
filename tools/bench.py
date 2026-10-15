#!/usr/bin/env python3
"""Run the exact (finite state) and approx (sampling) benchmark suites.

Each .det benchmark is run through determinize (Storm for exact/, sampling for
approx/). When a matching .prob (Polar) or .psi (lambda PSI) file sits next to
it, that tool is run too and its expectation and runtime are printed right
under the determinize output. Every tool invocation has the same timeout.

Usage:

1. Create a fresh enviornment:

uv venv --python 3.12 <env name>
uv pip install \
  --python <env name>/bin/python \
  -r tools/storm-requirements.txt

2. Run benchmarks:

env STORM_PYTHON=<env name>/bin/python \
  python3 tools/bench.py [--all | --exact | --approx] [--det] [--polar] [--psi]

Exact results checked by Lean's kernel, each benchmark in its own memory-capped scope
(Linux, systemd user session), written to a separate file:
  python3 tools/bench.py --exact --det --check-certificates --memory-limit 40G \
    --timeout 3600 --results results/finite-after-det-checked.json

For frontend timings without sampling or exact analysis:
  python3 tools/bench.py --exact --det-only
This times parsing, elaboration, type inference, and mean replacement inside Lean;
process startup, file reading, formatting, and backend analysis are excluded.

Polar runs with polar/.venv/bin/python (override with POLAR_PYTHON). Each .prob
file names its Polar arguments on its first line:
    # Polar: python polar.py <file>.prob --goals "E(x)" --at_n 2
"""

from __future__ import annotations

import argparse
from contextlib import nullcontext
import json
import math
import os
import re
import shlex
import signal
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
LEAN = ROOT / "lean"
DEFAULT_BINARY = LEAN / ".lake" / "build" / "bin" / "determinize"
EXACT_DIR = ROOT / "benchmarks" / "exact"
APPROX_DIR = ROOT / "benchmarks" / "approx"
STORM = ROOT / "tools" / "storm.py"
POLAR_DIR = ROOT / "polar"
POLAR_VENV_PYTHON = POLAR_DIR / ".venv" / "bin" / "python"
RESULTS_DIR = ROOT / "results"
EXACT_RESULTS = RESULTS_DIR / "finite-after-det.json"
APPROX_RESULTS = RESULTS_DIR / "infinite-after-det.json"
DET_ONLY_RESULTS = RESULTS_DIR / "determinization-only.json"
DEFAULT_TIMEOUT = 180


@dataclass(frozen=True)
class SampleSummary:
    returned: int
    samples: int
    mean: float | None
    variance: float | None
    rejected: int
    failure: str | None


@dataclass(frozen=True)
class SampleSites:
    source_discrete: int
    source_continuous: int
    determinized_discrete: int
    determinized_continuous: int


@dataclass(frozen=True)
class ToolRun:
    """Outcome of running an external tool with a timeout."""
    returncode: int | None  # None when the tool timed out
    stdout: str
    stderr: str
    seconds: float


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


def run_tool(command: list[str], cwd: Path, timeout: float) -> ToolRun:
    """Run a command, killing its whole process group on timeout."""
    started = time.monotonic()
    process = subprocess.Popen(
        command, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        start_new_session=True,
    )
    try:
        stdout, stderr = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        stdout, stderr = process.communicate()
        return ToolRun(None, stdout, stderr, time.monotonic() - started)
    return ToolRun(process.returncode, stdout, stderr, time.monotonic() - started)


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
    print(f"    {label:<14} returned {summary.returned}/{summary.samples}{suffix}")
    print(f"    {'':14} mean {format_float(summary.mean)}, variance {format_float(summary.variance)}")
    if summary.failure:
        print(f"    {'':14} first failure: {summary.failure}")


def sample_summary_json(summary: SampleSummary) -> dict[str, object]:
    result: dict[str, object] = {
        "returned": summary.returned,
        "requested": summary.samples,
        "mean": summary.mean,
        "variance": summary.variance,
    }
    if summary.rejected:
        result["rejected"] = summary.rejected
    failed = summary.samples - summary.returned - summary.rejected
    if failed > 0:
        result["failed"] = failed
    if summary.failure:
        result["first_failure"] = summary.failure
    return result


def sampling_sites_json(sites: SampleSites) -> dict[str, dict[str, int]]:
    return {
        "before": {
            "discrete": sites.source_discrete,
            "continuous": sites.source_continuous,
        },
        "after": {
            "discrete": sites.determinized_discrete,
            "continuous": sites.determinized_continuous,
        },
    }


def variance_factor_json(source: float | None, target: float | None) -> float | str | None:
    if source is None or target is None or (source == 0 and target == 0):
        return None
    if target == 0:
        return "infinite"
    return source / target


def save_results(path: Path, payload: dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2) + "\n")
    print(f"\nSaved results: {path}")


def transition_count(path: Path) -> int | None:
    try:
        with path.open() as lines:
            return max(sum(1 for _ in lines) - 1, 0)
    except OSError:
        return None


def count_sample_sites(model: Path, args: argparse.Namespace) -> SampleSites:
    completed = subprocess.run(
        [str(args.binary), "--check", "--sample-sites", str(model)],
        cwd=ROOT,
        text=True,
        capture_output=True,
        timeout=args.timeout,
    )
    if completed.returncode:
        raise RuntimeError(compact_error(completed.stderr or completed.stdout))
    pattern = re.compile(
        r"Sampling sites (before|after) determinization: "
        r"discrete=(\d+), continuous=(\d+)"
    )
    counts = {
        when: (int(discrete), int(continuous))
        for when, discrete, continuous in pattern.findall(completed.stdout)
    }
    if set(counts) != {"before", "after"}:
        raise RuntimeError("could not parse static sampling-site counts")
    return SampleSites(*counts["before"], *counts["after"])


def print_sample_sites(sites: SampleSites) -> None:
    print(
        "    Sampling sites before: "
        f"{sites.source_discrete} discrete, {sites.source_continuous} continuous"
    )
    print(
        "    Sampling sites after:  "
        f"{sites.determinized_discrete} discrete, "
        f"{sites.determinized_continuous} continuous"
    )


# ---------------------------------------------------------------------------
# determinize
# ---------------------------------------------------------------------------

def det_only(model: Path, args: argparse.Namespace) -> dict[str, object]:
    """Time the Lean frontend without running a program or constructing a model."""
    print("  Det (frontend only)")
    run = run_tool([str(args.binary), "--det-only", str(model)], ROOT, args.timeout)
    if run.returncode is None:
        print(f"    TIMEOUT after {args.timeout:g}s")
        return {"status": "timeout"}
    if run.returncode:
        error = compact_error(run.stderr or run.stdout)
        print(f"    ERROR: {error}")
        return {"status": "failed", "error": error}
    timing = re.search(r"^Determinization time \(ns\): (\d+)$", run.stdout, re.MULTILINE)
    if timing is None:
        error = "missing frontend timing; rebuild the determinize executable"
        print(f"    ERROR: {error}")
        return {"status": "failed", "error": error}
    nanoseconds = int(timing.group(1))
    seconds = nanoseconds / 1_000_000_000
    print(f"    Determinization time: {seconds:.9f}s")
    return {"status": "completed", "nanoseconds": nanoseconds, "seconds": seconds}


def run_det_only_suite(directory: Path, args: argparse.Namespace) -> int:
    models = sorted(directory.glob("*.det"))
    heading(f"DETERMINIZATION ONLY: {directory.name} ({len(models)} benchmarks)")
    print("Timed: parsing, elaboration, type inference, and mean replacement.")
    results = []
    for index, model in enumerate(models, 1):
        print(f"\n[{index}/{len(models)}] {model.name}")
        results.append({"benchmark": model.name, **det_only(model, args)})
    failures = sum(result["status"] != "completed" for result in results)
    payload = {
        "suite": directory.name,
        "configuration": {"mode": "det-only", "timeout_seconds": args.timeout,
                          "timed_phases": ["parse", "elaborate", "infer", "determinize"]},
        "benchmarks": results,
        "failures": failures,
    }
    destination = (args.output_dir / f"{directory.name}-determinization-only.json"
                   if args.output_dir else
                   DET_ONLY_RESULTS.with_name(f"{directory.name}-determinization-only.json"))
    save_results(destination, payload)
    return failures


def det_exact(model: Path, args: argparse.Namespace, temp: Path) -> dict[str, object]:
    """Determinize and solve one finite state benchmark with Storm."""
    print("  Det (Storm)")
    try:
        sites = count_sample_sites(model, args)
    except (RuntimeError, subprocess.TimeoutExpired) as error:
        print(f"    ERROR counting sampling sites: {error}")
        return {"status": "failed", "stage": "sampling-site count", "error": str(error)}
    result: dict[str, object] = {"sampling_sites": sampling_sites_json(sites)}
    print_sample_sites(sites)

    storm_python = os.environ.get("STORM_PYTHON", sys.executable)
    prefix = temp / model.stem
    command = [
        storm_python, str(STORM), str(model), "--prefix", str(prefix),
        "--binary", str(args.binary), "--subject", "determinized",
        "--max-states", str(args.max_states),
        "--timeout", str(args.timeout),
    ]
    if not args.check_certificates:
        command.append("--skip-certificate")
    if args.memory_limit:
        command = ["systemd-run", "--user", "--scope", "--quiet",
                   "-p", f"MemoryMax={args.memory_limit}", "-p", "MemorySwapMax=0", *command]
    completed = subprocess.run(command, cwd=ROOT, text=True, capture_output=True)
    report_path = Path(str(prefix) + ".storm.json")
    try:
        report = json.loads(report_path.read_text())
    except (OSError, json.JSONDecodeError):
        report = {}
    if completed.returncode or report.get("status") != "completed":
        error = report.get("error") or completed.stderr or completed.stdout
        if args.memory_limit and (completed.returncode < 0 or completed.returncode >= 128):
            error = f"killed, most likely at the {args.memory_limit} memory limit"
        stage = str(report.get("stage", "unknown stage"))
        error = compact_error(str(error))
        result.update(status="failed", stage=stage, error=error)
        print(f"    ERROR at {stage}: {error}")
        logs = report.get("logs", [])
        if logs:
            completed_seconds = sum(float(log.get("seconds", 0)) for log in logs)
            result["completed_phase_seconds"] = completed_seconds
            print(f"    Completed phase time: {completed_seconds:.3f}s")
        return result

    values_path = Path(str(prefix) + ".storm-values.json")
    try:
        values = json.loads(values_path.read_text())
        program_states = len(values["values"]["mass"])
        storm_states = program_states + 1
    except (OSError, json.JSONDecodeError, KeyError, TypeError):
        program_states = None
        storm_states = None

    print(f"    Certificate:        {'kernel checked' if report.get('kernel_checked') else 'generated, unchecked'}")
    if args.output_dir:
        print(f"    Report:             {report_path}")
    logs = report.get("logs", [])
    kernel_seconds = (float(logs[2]["seconds"])
                      if report.get("kernel_checked") and len(logs) >= 3 else None)
    if kernel_seconds is not None:
        print(f"    Kernel check time:  {kernel_seconds:.3f}s")
    export_seconds = float(logs[0].get("seconds", 0)) if len(logs) >= 1 else None
    storm_seconds = float(logs[1].get("seconds", 0)) if len(logs) >= 2 else None
    measured = [value for value in (export_seconds, storm_seconds) if value is not None]
    total_seconds = sum(measured) if len(measured) == 2 else None
    transitions = transition_count(Path(str(prefix) + ".tra"))

    print(f"    Target expectation: {report.get('exact_answer', 'n/a')}")
    print(f"    Return mass:        {report.get('return_mass', 'n/a')}")
    print(f"    Conditional mean:   {report.get('conditional_mean', 'n/a')}")
    if storm_states is None:
        print("    States:             n/a")
    else:
        print(
            f"    States:             {storm_states} Storm states "
            f"({program_states} program states + done sink)"
        )
    print(f"    Transitions:        {transitions if transitions is not None else 'n/a'}")
    if total_seconds is None:
        print("    Measured time:      n/a")
    else:
        print(f"    Measured time:      {total_seconds:.3f}s")
        print(f"      model export:     {export_seconds:.3f}s")
        print(f"      Storm solve:      {storm_seconds:.3f}s")
    result.update(
        status="completed",
        certificate=("kernel checked" if report.get("kernel_checked")
                     else "generated, unchecked"),
        target_expectation=report.get("exact_answer"),
        return_mass=report.get("return_mass"),
        conditional_mean=report.get("conditional_mean"),
        states={
            "storm": storm_states,
            "program": program_states,
            "done_sink": 1 if storm_states is not None else None,
        },
        transitions=transitions,
        timing_seconds={
            "measured": total_seconds,
            "model_export": export_seconds,
            "storm_solve": storm_seconds,
            **({"kernel_check": kernel_seconds} if kernel_seconds is not None else {}),
        },
    )
    return result


def det_approx(model: Path, args: argparse.Namespace) -> dict[str, object]:
    """Sample one benchmark before and after determinization."""
    print("  Det (sampling)")
    try:
        sites = count_sample_sites(model, args)
    except (RuntimeError, subprocess.TimeoutExpired) as error:
        print(f"    ERROR counting sampling sites: {error}")
        return {"status": "failed", "stage": "sampling-site count", "error": str(error)}
    result: dict[str, object] = {"sampling_sites": sampling_sites_json(sites)}
    print_sample_sites(sites)

    command = [
        str(args.binary), "--check", "--samples", str(args.samples),
        "--seed", str(args.seed), "--fuel", str(args.fuel), str(model),
    ]
    run = run_tool(command, ROOT, args.timeout)
    if run.returncode is None:
        error = f"sampling timed out after {args.timeout:g}s"
        result.update(status="failed", stage="sampling", error=error)
        print(f"    ERROR: {error}")
        return result
    if run.returncode:
        error = compact_error(run.stderr or run.stdout)
        result.update(status="failed", stage="sampling", error=error,
                      sampling_seconds=run.seconds)
        print(f"    ERROR: {error}")
        return result
    source = parse_sample_summary(run.stdout, "Source", "Determinized")
    target = parse_sample_summary(run.stdout, "Determinized", None)
    if source is None or target is None:
        result.update(status="failed", stage="output parsing",
                      error="could not parse sampler output", sampling_seconds=run.seconds)
        print("    ERROR: could not parse sampler output")
        print(f"    {compact_error(run.stdout)}")
        return result
    print_sample("Source", source)
    print_sample("Determinized", target)
    print(f"    Sampling time:      {run.seconds:.3f}s")
    result.update(
        source=sample_summary_json(source),
        determinized=sample_summary_json(target),
        variance_reduction_factor=None,
        sampling_seconds=run.seconds,
    )
    errors = []
    for label, summary in (("Source", source), ("Determinized", target)):
        failed = summary.samples - summary.returned - summary.rejected
        if failed > 0:
            error = f"{label}: {failed}/{summary.samples} executions failed"
            if summary.failure:
                error += f"; first failure: {summary.failure}"
            errors.append(error)
        elif summary.failure:
            errors.append(f"{label}: {summary.failure}")
        elif summary.returned + summary.rejected != summary.samples:
            errors.append(f"{label}: returned and rejected counts account for "
                          f"{summary.returned + summary.rejected}/{summary.samples} runs")
        if summary.samples != args.samples:
            errors.append(f"{label}: sampled {summary.samples}/{args.samples} requested runs")
    if errors:
        error = "; ".join(errors)
        result.update(status="failed", stage="sampling", error=error)
        print(f"    ERROR: {error}")
        print("    Variance reduction: n/a (sampling failed)")
    else:
        result.update(status="completed",
                      variance_reduction_factor=variance_factor_json(source.variance, target.variance))
        print(f"    Variance reduction: {variance_factor(source.variance, target.variance)}")
    return result


# ---------------------------------------------------------------------------
# Polar and lambda PSI
# ---------------------------------------------------------------------------

def companion(model: Path, suffix: str) -> Path | None:
    """The .prob/.psi file next to a .det benchmark with the same name, if any."""
    path = model.with_suffix(suffix)
    return path if path.is_file() else None


def print_tool_result(label: str, source: Path | None, result: dict[str, object]) -> None:
    status = result["status"]
    if status == "unsupported":
        print(f"  {label:<18} unsupported")
        return
    print(f"  {label} ({source.name})")
    if status == "completed":
        print(f"    Expectation:        {result['expectation']}")
        print(f"    Runtime:            {result['seconds']:.3f}s")
    elif status == "timeout":
        print(f"    TIMEOUT after {result['seconds']:.0f}s")
    else:
        print(f"    ERROR: {result['error']}")
        print(f"    Runtime:            {result['seconds']:.3f}s")


def polar_arguments(prob: Path) -> list[str] | None:
    """Arguments after the file name in the '# Polar: python polar.py ...' header."""
    with prob.open() as lines:
        first = lines.readline()
    match = re.match(r"#\s*Polar:\s*python\s+polar\.py\s+(.*)", first)
    if match is None:
        return None
    return shlex.split(match.group(1))[1:]


def format_polar_value(line: str) -> str:
    """Turn 'E(x | n=1) = 11/2 ≅ 5.5' into '11/2 ≅ 5.5'.

    Long symbolic closed forms are dropped in favour of the numeric value, and a
    negligible imaginary part left over from complex roots is removed.
    """
    value = line.split(" = ", 1)[1].strip()
    if "≅" not in value:
        return value
    exact, approx = (part.strip() for part in value.split("≅", 1))
    approx = re.sub(r"\s*[+-]\s*[0-9.]+(e-\d+)?\*I$", "", approx)
    return f"{exact} ≅ {approx}" if len(exact) <= 40 else f"≅ {approx}"


def run_polar(prob: Path | None, args: argparse.Namespace) -> dict[str, object]:
    if prob is None:
        return {"status": "unsupported"}
    arguments = polar_arguments(prob)
    if arguments is None:
        return {"status": "failed", "seconds": 0.0,
                "error": "missing '# Polar: python polar.py ...' header on line 1"}
    python = os.environ.get("POLAR_PYTHON") or (
        str(POLAR_VENV_PYTHON) if POLAR_VENV_PYTHON.is_file() else sys.executable)
    run = run_tool([python, "polar.py", str(prob), *arguments], POLAR_DIR, args.timeout)
    if run.returncode is None:
        return {"status": "timeout", "seconds": run.seconds}
    goals = [line for line in run.stdout.splitlines() if line.startswith("E(")]
    if run.returncode or not goals:
        return {"status": "failed", "seconds": run.seconds,
                "error": compact_error(run.stderr or run.stdout)}
    return {"status": "completed", "seconds": run.seconds,
            "expectation": format_polar_value(goals[-1])}


def run_psi(psi: Path | None, args: argparse.Namespace) -> dict[str, object]:
    if psi is None:
        return {"status": "unsupported"}
    run = run_tool([args.psi_binary, str(psi), "--expectation"], psi.parent, args.timeout)
    if run.returncode is None:
        return {"status": "timeout", "seconds": run.seconds}
    values = [line.split(" = ", 1)[1].strip()
              for line in run.stdout.splitlines() if " = " in line]
    if run.returncode or not values:
        return {"status": "failed", "seconds": run.seconds,
                "error": compact_error(run.stderr or run.stdout)}
    return {"status": "completed", "seconds": run.seconds, "expectation": values[-1]}


# ---------------------------------------------------------------------------
# Suites
# ---------------------------------------------------------------------------

def run_suite(name: str, directory: Path, exact: bool, args: argparse.Namespace) -> int:
    """Run one suite; returns the number of determinize failures."""
    models = sorted(directory.glob("*.det"))
    tools = [label for label, on in (("det", args.det), ("polar", args.polar),
                                     ("psi", args.psi)) if on]
    heading(f"{name} ({len(models)} benchmarks; tools: {', '.join(tools)})")
    print(f"timeout={args.timeout:g}s per tool", end="")
    if args.det and exact:
        print(f", additive=yes, kernel-check={args.check_certificates}, "
              f"max-states={args.max_states}")
        print("Statistics describe the target; source safety and integrability are not checked here.")
    elif args.det:
        print(f", samples={args.samples}, seed={args.seed}, fuel={args.fuel}")
    else:
        print()

    for path in sorted([*directory.glob("*.prob"), *directory.glob("*.psi")]):
        if not path.with_suffix(".det").is_file():
            print(f"WARNING: {path.name} has no matching .det benchmark and is skipped")

    failures = 0
    counts = {tool: {"completed": 0, "timeout": 0, "failed": 0, "unsupported": 0}
              for tool in ("polar", "psi")}
    results: list[dict[str, object]] = []
    if args.output_dir:
        args.output_dir.mkdir(parents=True, exist_ok=True)
    workspace = (nullcontext(args.output_dir) if args.output_dir
                 else tempfile.TemporaryDirectory(prefix="determinize-bench-"))
    with workspace as temporary:
        temp = Path(temporary)
        for index, model in enumerate(models, 1):
            result: dict[str, object] = {"benchmark": model.name}
            print(f"\n[{index}/{len(models)}] {model.name}")
            if args.det:
                det = det_exact(model, args, temp) if exact else det_approx(model, args)
                if det.get("status") != "completed":
                    failures += 1
                result.update(det)
            for tool, label, source, runner in (
                ("polar", "Polar", companion(model, ".prob"), run_polar),
                ("psi", "λPSI", companion(model, ".psi"), run_psi),
            ):
                if not getattr(args, tool):
                    continue
                outcome = runner(source, args)
                print_tool_result(label, source, outcome)
                counts[tool][str(outcome["status"])] += 1
                result[tool] = {**outcome, **({"file": source.name} if source else {})}
            results.append(result)

    for tool, label in (("polar", "Polar"), ("psi", "λPSI")):
        if getattr(args, tool):
            c = counts[tool]
            print(f"\n{label}: {c['completed']} completed, {c['timeout']} timed out, "
                  f"{c['failed']} failed, {c['unsupported']} unsupported")
    if args.det:
        save_results(args.results or (EXACT_RESULTS if exact else APPROX_RESULTS), {
            "suite": "exact" if exact else "approx",
            "configuration": {
                "timeout_seconds": args.timeout,
                **({"subject": "determinized", "additive": True,
                    "check_certificates": args.check_certificates,
                    "max_states": args.max_states,
                    **({"memory_limit": args.memory_limit} if args.memory_limit else {})}
                   if exact else
                   {"samples": args.samples, "seed": args.seed, "fuel": args.fuel}),
            },
            "benchmarks": results,
            "failures": failures,
        })
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
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    suites = parser.add_argument_group("suites (default: --all)")
    suites.add_argument("--all", action="store_true", help="run both exact/ and approx/")
    suites.add_argument("--exact", "--finite-state", dest="exact", action="store_true",
                        help="run benchmarks/exact (finite state after determinization)")
    suites.add_argument("--approx", "--infinite-state", dest="approx", action="store_true",
                        help="run benchmarks/approx (sampling suite)")
    tools = parser.add_argument_group("tools (default: all three)")
    tools.add_argument("--det", action="store_true", help="run determinize")
    tools.add_argument("--det-only", action="store_true",
                       help="time only the Lean frontend; skip sampling, models, Storm, Polar, and PSI")
    tools.add_argument("--polar", action="store_true", help="run Polar on .prob files")
    tools.add_argument("--psi", action="store_true", help="run lambda PSI on .psi files")
    parser.add_argument("--timeout", type=positive, default=DEFAULT_TIMEOUT,
                        help=f"timeout per tool invocation in seconds (default: {DEFAULT_TIMEOUT})")
    parser.add_argument("--samples", type=positive, default=1000, help="samples per approx benchmark (default: 1000)")
    parser.add_argument("--seed", type=int, default=0, help="initial sampler seed (default: 0)")
    parser.add_argument("--fuel", type=positive, default=100000, help="fuel per sample (default: 100000)")
    parser.add_argument("--max-states", type=positive, default=100000, help="finite exploration state limit")
    parser.add_argument("--binary", type=Path, default=DEFAULT_BINARY, help="determinize executable")
    parser.add_argument("--psi-binary", default="psisolver", help="lambda PSI executable (default: psisolver)")
    parser.add_argument("--check-certificates", action="store_true", help="independently kernel-check exact results (timed separately)")
    parser.add_argument("--output-dir", type=Path, help="retain exact models, certificates, and JSON reports")
    parser.add_argument("--no-build", action="store_true", help="use the existing determinize executable")
    parser.add_argument("--memory-limit", metavar="SIZE",
                        help="run each exact benchmark in a systemd user scope with this MemoryMax (e.g. 40G)")
    parser.add_argument("--results", type=Path,
                        help="write the suite's JSON here instead of the default results file")
    args = parser.parse_args()

    if args.det_only:
        if args.polar or args.psi:
            parser.error("--det-only cannot be combined with --polar or --psi")
        args.det = True

    run_both = args.all or not (args.exact or args.approx)
    do_exact = args.exact or run_both
    do_approx = args.approx or run_both
    if args.results and do_exact and do_approx:
        parser.error("--results needs a single suite: --exact or --approx")
    if not (args.det or args.polar or args.psi):
        args.det = args.polar = args.psi = True
    args.binary = args.binary.resolve()

    if args.det:
        if not args.no_build:
            build(args.binary)
        elif not args.binary.is_file():
            raise SystemExit(f"determinize binary not found: {args.binary}")

    failures = 0
    if args.det_only:
        if do_exact:
            failures += run_det_only_suite(EXACT_DIR, args)
        if do_approx:
            failures += run_det_only_suite(APPROX_DIR, args)
    elif do_exact:
        failures += run_suite("EXACT: FINITE STATE AFTER DETERMINIZATION", EXACT_DIR, True, args)
    if do_approx and not args.det_only:
        failures += run_suite("APPROX: SAMPLING", APPROX_DIR, False, args)

    heading("SUMMARY")
    if failures:
        print(f"Completed with {failures} determinize failure(s).")
        return 1
    print("All selected benchmarks completed.")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except KeyboardInterrupt:
        print("\nInterrupted.", file=sys.stderr)
        raise SystemExit(130)
