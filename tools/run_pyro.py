#!/usr/bin/env python3
"""Compare source and determinized `.det` models with Pyro inference."""

from __future__ import annotations

import argparse
import json
import math
import statistics
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Sequence

import pyro
import torch
from pyro.infer import Importance, SMCFilter

from det_to_pyro import (
    RuntimeProgram,
    SequentialGuide,
    SequentialModel,
    default_proposals_path,
    eliminated_random_variables,
    load_proposals,
    parse_file,
)


torch.set_default_dtype(torch.float64)


@dataclass
class Trial:
    subject: str
    algorithm: str
    seed: int
    estimate: float
    output_variance: float
    ess: float
    min_step_ess: float | None
    resamples: int | None
    seconds: float


class TrackingSMCFilter(SMCFilter):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.ess_history: list[float] = []
        self.resample_count = 0

    @torch.no_grad()
    def _maybe_importance_resample(self):
        probabilities = self.state._log_weights.softmax(0)
        ess = probabilities.new_tensor(1.0) / probabilities.square().sum()
        self.ess_history.append(float(ess))
        return super()._maybe_importance_resample()

    @torch.no_grad()
    def _importance_resample(self, probabilities):
        self.resample_count += 1
        return super()._importance_resample(probabilities)


def normalized(log_weights: torch.Tensor) -> torch.Tensor:
    if torch.isnan(log_weights).any():
        raise RuntimeError("inference produced NaN log weights")
    if torch.isneginf(log_weights).all():
        raise RuntimeError("all particles have zero weight; use a better proposal or more particles")
    return torch.softmax(log_weights, dim=0)


def moments(values: torch.Tensor, weights: torch.Tensor) -> tuple[float, float, float]:
    values = values.reshape(-1).to(dtype=torch.float64)
    weights = weights.reshape(-1).to(dtype=torch.float64)
    if values.numel() != weights.numel():
        raise RuntimeError(
            f"query has {values.numel()} values but inference returned {weights.numel()} weights"
        )
    mean = (weights * values).sum()
    variance = (weights * (values - mean).square()).sum()
    ess = 1.0 / weights.square().sum()
    return float(mean), float(variance), float(ess)


def run_importance(
    runtime: RuntimeProgram, particles: int, seed: int
) -> tuple[float, float, float, None, None]:
    pyro.clear_param_store()
    pyro.set_rng_seed(seed)
    posterior = Importance(runtime.model, guide=runtime.guide, num_samples=particles).run()
    log_weights = torch.stack(
        [torch.as_tensor(weight).detach().reshape(()) for weight in posterior.log_weights]
    )
    values = torch.stack(
        [trace.nodes["_RETURN"]["value"].detach().reshape(()) for trace in posterior.exec_traces]
    )
    estimate, variance, ess = moments(values, normalized(log_weights))
    return estimate, variance, ess, None, None


def run_smc(
    runtime: RuntimeProgram,
    particles: int,
    seed: int,
    ess_threshold: float,
) -> tuple[float, float, float, float, int]:
    pyro.clear_param_store()
    pyro.set_rng_seed(seed)
    model = SequentialModel(runtime)
    guide = SequentialGuide(runtime)
    smc = TrackingSMCFilter(
        model,
        guide,
        num_particles=particles,
        max_plate_nesting=0,
        ess_threshold=ess_threshold,
    )
    smc.init()
    for _ in range(1, len(model.chunks)):
        smc.step()
    weights = normalized(smc.state._log_weights.detach())
    values = smc.state["__query"].detach()
    estimate, variance, ess = moments(values, weights)
    minimum = min(smc.ess_history) if smc.ess_history else ess
    return estimate, variance, ess, minimum, smc.resample_count


def run_trial(
    runtime: RuntimeProgram,
    algorithm: str,
    particles: int,
    seed: int,
    ess_threshold: float,
) -> Trial:
    started = time.perf_counter()
    if algorithm == "importance":
        estimate, variance, ess, minimum, resamples = run_importance(
            runtime, particles, seed
        )
    else:
        estimate, variance, ess, minimum, resamples = run_smc(
            runtime, particles, seed, ess_threshold
        )
    elapsed = time.perf_counter() - started
    return Trial(
        subject=runtime.subject,
        algorithm=algorithm,
        seed=seed,
        estimate=estimate,
        output_variance=variance,
        ess=ess,
        min_step_ess=minimum,
        resamples=resamples,
        seconds=elapsed,
    )


def summarize(trials: list[Trial], truth: float | None) -> dict[str, float | int | None]:
    estimates = [trial.estimate for trial in trials]
    result: dict[str, float | int | None] = {
        "runs": len(trials),
        "mean_estimate": statistics.fmean(estimates),
        "estimator_variance": statistics.variance(estimates) if len(estimates) > 1 else None,
        "mean_output_variance": statistics.fmean(t.output_variance for t in trials),
        "mean_ess": statistics.fmean(t.ess for t in trials),
        "mean_seconds": statistics.fmean(t.seconds for t in trials),
    }
    minimum = [t.min_step_ess for t in trials if t.min_step_ess is not None]
    resamples = [t.resamples for t in trials if t.resamples is not None]
    result["mean_min_step_ess"] = statistics.fmean(minimum) if minimum else None
    result["mean_resamples"] = statistics.fmean(resamples) if resamples else None
    result["rmse"] = (
        math.sqrt(statistics.fmean((estimate - truth) ** 2 for estimate in estimates))
        if truth is not None
        else None
    )
    return result


def format_number(value) -> str:
    if value is None:
        return "n/a"
    if isinstance(value, int):
        return str(value)
    return f"{value:.6g}"


def print_summary(subject: str, summary: dict[str, float | int | None]) -> None:
    print(f"{subject}:")
    for key in [
        "mean_estimate", "estimator_variance", "rmse", "mean_output_variance",
        "mean_ess", "mean_min_step_ess", "mean_resamples", "mean_seconds",
    ]:
        if summary.get(key) is not None:
            print(f"  {key}: {format_number(summary[key])}")


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("model", type=Path)
    parser.add_argument("--algorithm", choices=["importance", "smc"], required=True)
    parser.add_argument(
        "--subject", choices=["both", "source", "determinized"], default="both"
    )
    parser.add_argument("--particles", type=int, default=100)
    parser.add_argument("--runs", type=int, default=10)
    parser.add_argument("--seed", type=int, default=1)
    parser.add_argument("--truth", type=float)
    parser.add_argument("--ess-threshold", type=float, default=0.5)
    parser.add_argument("--proposals", type=Path)
    parser.add_argument("--json-out", type=Path)
    args = parser.parse_args(argv)
    if args.particles < 2 or args.runs < 1:
        parser.error("--particles must be at least 2 and --runs must be positive")
    if not 0 < args.ess_threshold <= 1:
        parser.error("--ess-threshold must be in (0,1]")

    program = parse_file(args.model)
    proposal_path = args.proposals or default_proposals_path(args.model)
    proposals = load_proposals(proposal_path, args.model.stem)
    subjects = (
        ["source", "determinized"] if args.subject == "both" else [args.subject]
    )
    runtimes = {
        subject: RuntimeProgram(program, subject, proposals) for subject in subjects
    }
    all_trials: dict[str, list[Trial]] = {}
    summaries: dict[str, dict[str, float | int | None]] = {}

    print(
        f"model={args.model} algorithm={args.algorithm} "
        f"particles={args.particles} runs={args.runs}"
    )
    # Exclude one-time PyTorch/Pyro dispatcher initialization from either
    # subject's timing. This is especially visible for small SMC programs.
    for offset, subject in enumerate(subjects):
        run_trial(
            runtimes[subject],
            args.algorithm,
            min(args.particles, 4),
            args.seed + 10_000_000 + offset,
            args.ess_threshold,
        )
    for subject in subjects:
        runtime = runtimes[subject]
        trials: list[Trial] = []
        for run in range(args.runs):
            trial = run_trial(
                runtime,
                args.algorithm,
                args.particles,
                args.seed + run,
                args.ess_threshold,
            )
            trials.append(trial)
        all_trials[subject] = trials
        summaries[subject] = summarize(trials, args.truth)
        print_summary(subject, summaries[subject])
        if summaries[subject]["mean_ess"] < 0.1 * args.particles:
            print(
                "  warning: mean ESS is below 10% of particles; "
                "estimator comparisons are unreliable"
            )

    comparison: dict[str, float | int | None] = {}
    if {"source", "determinized"} <= summaries.keys():
        eliminated = eliminated_random_variables(program)
        comparison["random_variables_eliminated"] = len(eliminated)
        source_variance = summaries["source"]["estimator_variance"]
        target_variance = summaries["determinized"]["estimator_variance"]
        if source_variance is not None and target_variance is not None:
            comparison["variance_reduction_factor"] = (
                math.inf if target_variance == 0 and source_variance > 0
                else source_variance / target_variance if target_variance > 0
                else None
            )
        source_time = summaries["source"]["mean_seconds"]
        target_time = summaries["determinized"]["mean_seconds"]
        comparison["mean_runtime_ratio"] = source_time / target_time
        print("comparison:")
        for key, value in comparison.items():
            label = (
                "random variables eliminated"
                if key == "random_variables_eliminated"
                else key
            )
            print(f"  {label}: {format_number(value)}")

    if args.json_out:
        payload = {
            "model": str(args.model),
            "algorithm": args.algorithm,
            "particles": args.particles,
            "runs": args.runs,
            "summaries": summaries,
            "comparison": comparison,
            "trials": {
                subject: [asdict(trial) for trial in trials]
                for subject, trials in all_trials.items()
            },
        }
        args.json_out.parent.mkdir(parents=True, exist_ok=True)
        args.json_out.write_text(json.dumps(payload, indent=2, allow_nan=True) + "\n")
        print(f"wrote {args.json_out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
