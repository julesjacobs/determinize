#!/usr/bin/env python3
"""Benchmark source and Lean-determinized .det programs through Pyro.

The Lean CLI is the source of truth for parsing, affinity inference, and
determinization.  This script parses the CLI's annotated source and actual
``Expr.determinize`` output, executes both with Pyro, and compares Importance
Sampling and independent Metropolis-Hastings estimates.
"""

from __future__ import annotations

import argparse
import json
import math
import re
import statistics
import subprocess
import sys
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Iterable, Literal, Sequence

try:
    import pyro
    import pyro.distributions as dist
    import torch
    from pyro import poutine
except ImportError as error:  # pragma: no cover - exercised by users without extras
    raise SystemExit(
        "Pyro is required; install tools/pyro-requirements.txt"
    ) from error


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_MODEL_DIR = ROOT / "benchmarks" / "approx"
DEFAULT_BINARY = ROOT / "lean" / ".lake" / "build" / "bin" / "determinize"
torch.set_default_dtype(torch.float64)


class BenchmarkError(RuntimeError):
    pass


@dataclass(frozen=True)
class Token:
    text: str
    offset: int


@dataclass(frozen=True)
class Expr:
    kind: str
    value: Any = None
    children: tuple["Expr", ...] = ()


@dataclass
class Closure:
    parameter: str
    body: Expr
    environment: dict[str, Any]
    function: str | None = None


@dataclass(frozen=True)
class SumValue:
    side: Literal["left", "right"]
    value: Any


@dataclass(frozen=True)
class CompiledPair:
    checked_type: str
    source_text: str
    target_text: str
    source: Expr
    target: Expr


@dataclass
class Trial:
    subject: str
    algorithm: str
    seed: int
    estimate: float
    output_variance: float
    ess: float
    seconds: float
    acceptance_rate: float | None = None


_NUMBER = re.compile(r"(?:\d+\.\d*|\.\d+|\d+)(?:[eE][+-]?\d+)?")
_IDENTIFIER = re.compile(r"[A-Za-z_][A-Za-z0-9_']*")
_SAMPLE_PRIMITIVES = {
    "uniform", "gauss", "poisson", "exponential", "bernoulli", "beta",
    "gamma", "discrete", "discrete_list",
}
_MEAN_PRIMITIVES = {f"mean_{name}" for name in _SAMPLE_PRIMITIVES}
_PRIMITIVES = _SAMPLE_PRIMITIVES | _MEAN_PRIMITIVES | {"observe"}
_INFIX = {
    "<": (30, 31), "<=": (30, 31), "::": (40, 40),
    "+": (50, 51), "-": (50, 51), "*": (60, 61), "/": (60, 61),
}


def tokenize(source: str) -> list[Token]:
    result: list[Token] = []
    index = 0
    while index < len(source):
        if source[index].isspace():
            index += 1
            continue
        pair = source[index:index + 2]
        if pair in {"=>", "::", "<="}:
            result.append(Token(pair, index))
            index += 2
            continue
        number = _NUMBER.match(source, index)
        if number:
            result.append(Token(number.group(), index))
            index = number.end()
            continue
        identifier = _IDENTIFIER.match(source, index)
        if identifier:
            result.append(Token(identifier.group(), index))
            index = identifier.end()
            continue
        if source[index] in "()[],|=+-*/<":
            result.append(Token(source[index], index))
            index += 1
            continue
        raise BenchmarkError(f"unexpected character {source[index]!r} at offset {index}")
    result.append(Token("<eof>", len(source)))
    return result


class Parser:
    def __init__(self, source: str):
        self.tokens = tokenize(source)
        self.position = 0

    def peek(self) -> str:
        return self.tokens[self.position].text

    def take(self, expected: str | None = None) -> str:
        token = self.tokens[self.position]
        if expected is not None and token.text != expected:
            raise BenchmarkError(
                f"expected {expected!r}, got {token.text!r} at offset {token.offset}"
            )
        self.position += 1
        return token.text

    def parse(self) -> Expr:
        result = self.expression()
        self.take("<eof>")
        return result

    def expression(self, minimum_precedence: int = 0) -> Expr:
        token = self.take()
        if token == "let":
            name = self.take()
            self.take("=")
            value = self.expression()
            self.take("in")
            left = Expr("let", name, (value, self.expression()))
        elif token == "fun":
            parameter = self.take()
            self.take("=>")
            left = Expr("lambda", parameter, (self.expression(),))
        elif token == "rec":
            function = self.take()
            parameter = self.take()
            self.take("=>")
            left = Expr("fix", (function, parameter), (self.expression(),))
        elif token == "if":
            condition = self.expression()
            self.take("then")
            yes = self.expression()
            self.take("else")
            left = Expr("if", None, (condition, yes, self.expression()))
        elif token == "match":
            value = self.expression()
            self.take("with")
            if self.peek() == "|":
                self.take("|")
            if self.peek() == "[":
                self.take("[")
                self.take("]")
                self.take("=>")
                empty = self.expression()
                self.take("|")
                head = self.take()
                self.take("::")
                tail = self.take()
                self.take("=>")
                left = Expr("match_list", (head, tail), (value, empty, self.expression()))
            else:
                self.take("inl")
                left_name = self.take()
                self.take("=>")
                left_case = self.expression()
                self.take("|")
                self.take("inr")
                right_name = self.take()
                self.take("=>")
                left = Expr(
                    "match_sum", (left_name, right_name),
                    (value, left_case, self.expression()),
                )
        elif token == "(":
            if self.peek() == ")":
                self.take(")")
                left = Expr("unit")
            else:
                first = self.expression()
                if self.peek() == ",":
                    self.take(",")
                    left = Expr("pair", None, (first, self.expression()))
                else:
                    left = first
                self.take(")")
        elif token == "[":
            self.take("]")
            left = Expr("nil")
        elif token == "-":
            left = Expr("neg", None, (self.expression(70),))
        elif token in {"fst", "snd", "inl", "inr"}:
            left = Expr(token, None, (self.expression(81),))
        elif token in {"true", "false"}:
            left = Expr("bool", token == "true")
        elif _NUMBER.fullmatch(token):
            left = Expr("number", float(token))
        elif _IDENTIFIER.fullmatch(token):
            if token in _SAMPLE_PRIMITIVES and self.peek() == "[":
                self.take("[")
                affinity = self.take()
                if affinity not in {"E", "G"}:
                    raise BenchmarkError(f"invalid affinity {affinity!r}")
                self.take("]")
                left = self.primitive(token, affinity)
            elif token in _PRIMITIVES and self.peek() == "(":
                left = self.primitive(token, None)
            else:
                left = Expr("variable", token)
        else:
            raise BenchmarkError(f"expected expression, got {token!r}")

        while True:
            operator = self.peek()
            info = _INFIX.get(operator)
            if info is not None:
                left_precedence, right_precedence = info
                if left_precedence < minimum_precedence:
                    break
                self.take()
                right = self.expression(right_precedence)
                left = Expr("cons" if operator == "::" else "binary", operator, (left, right))
                continue
            if minimum_precedence <= 80 and self.starts_atom(operator):
                left = Expr("apply", None, (left, self.expression(81)))
                continue
            break
        return left

    def primitive(self, name: str, affinity: str | None) -> Expr:
        self.take("(")
        arguments: list[Expr] = []
        if self.peek() != ")":
            arguments.append(self.expression())
            while self.peek() == ",":
                self.take(",")
                arguments.append(self.expression())
        self.take(")")
        return Expr("primitive", (name, affinity), tuple(arguments))

    @staticmethod
    def starts_atom(token: str) -> bool:
        return token in {"(", "["} or bool(_NUMBER.fullmatch(token)) or (
            bool(_IDENTIFIER.fullmatch(token)) and token not in {
                "in", "then", "else", "with", "let", "if", "match", "fun", "rec",
            }
        )


def parse_core(source: str) -> Expr:
    return Parser(source).parse()


def compile_with_lean(path: Path, binary: Path) -> CompiledPair:
    if not binary.is_file():
        raise BenchmarkError(
            f"Lean CLI not found at {binary}; run '(cd lean && lake build determinize)'"
        )
    completed = subprocess.run(
        [str(binary), str(path.resolve())], cwd=ROOT, text=True,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False,
    )
    if completed.returncode:
        detail = (completed.stderr or completed.stdout).strip()
        raise BenchmarkError(f"Lean frontend rejected the model: {detail}")
    match = re.fullmatch(
        r"Checked: ([^\n]+)\nAnnotated source:\n(.+)\nDeterminized:\n(.+)\n?",
        completed.stdout, flags=re.DOTALL,
    )
    if not match:
        raise BenchmarkError("could not read annotated/determinized output from Lean CLI")
    checked_type, source_text, target_text = match.groups()
    return CompiledPair(
        checked_type, source_text, target_text,
        parse_core(source_text), parse_core(target_text),
    )


class PyroProgram:
    def __init__(self, expression: Expr):
        self.expression = expression
        self.phase: Literal["model", "guide"] = "model"
        self.site = 0

    @staticmethod
    def tensor(value: float | int | bool) -> torch.Tensor:
        if isinstance(value, bool):
            return torch.tensor(value)
        return torch.as_tensor(value, dtype=torch.get_default_dtype())

    def fresh(self, prefix: str) -> str:
        result = f"{prefix}_{self.site:05d}"
        self.site += 1
        return result

    def model(self):
        self.phase = "model"
        self.site = 0
        result = self.evaluate(self.expression, {})
        if not isinstance(result, torch.Tensor) or result.numel() != 1:
            raise BenchmarkError("benchmark must return one numeric value")
        return result.reshape(())

    def guide(self):
        self.phase = "guide"
        self.site = 0
        self.evaluate(self.expression, {})

    def evaluate(self, expression: Expr, environment: dict[str, Any]):
        kind = expression.kind
        if kind == "number":
            return self.tensor(expression.value)
        if kind == "bool":
            return self.tensor(expression.value)
        if kind == "unit":
            return None
        if kind == "nil":
            return []
        if kind == "variable":
            return environment[expression.value]
        if kind == "neg":
            return -self.evaluate(expression.children[0], environment)
        if kind == "binary":
            left = self.evaluate(expression.children[0], environment)
            right = self.evaluate(expression.children[1], environment)
            return {
                "+": lambda: left + right, "-": lambda: left - right,
                "*": lambda: left * right, "/": lambda: left / right,
                "<": lambda: left < right, "<=": lambda: left <= right,
            }[expression.value]()
        if kind == "pair":
            return tuple(self.evaluate(child, environment) for child in expression.children)
        if kind in {"fst", "snd"}:
            pair = self.evaluate(expression.children[0], environment)
            return pair[0 if kind == "fst" else 1]
        if kind in {"inl", "inr"}:
            return SumValue(
                "left" if kind == "inl" else "right",
                self.evaluate(expression.children[0], environment),
            )
        if kind == "cons":
            head = self.evaluate(expression.children[0], environment)
            tail = self.evaluate(expression.children[1], environment)
            return [head, *tail]
        if kind == "let":
            value = self.evaluate(expression.children[0], environment)
            local = dict(environment)
            local[expression.value] = value
            return self.evaluate(expression.children[1], local)
        if kind == "lambda":
            return Closure(expression.value, expression.children[0], dict(environment))
        if kind == "fix":
            function, parameter = expression.value
            return Closure(parameter, expression.children[0], dict(environment), function)
        if kind == "apply":
            function = self.evaluate(expression.children[0], environment)
            argument = self.evaluate(expression.children[1], environment)
            if not isinstance(function, Closure):
                raise BenchmarkError("attempted to apply a non-function")
            local = dict(function.environment)
            if function.function is not None:
                local[function.function] = function
            local[function.parameter] = argument
            return self.evaluate(function.body, local)
        if kind == "if":
            condition = self.evaluate(expression.children[0], environment)
            branch = expression.children[1] if bool(condition.item()) else expression.children[2]
            return self.evaluate(branch, environment)
        if kind == "match_list":
            values = self.evaluate(expression.children[0], environment)
            if not values:
                return self.evaluate(expression.children[1], environment)
            head, tail = expression.value
            local = dict(environment)
            local[head], local[tail] = values[0], values[1:]
            return self.evaluate(expression.children[2], local)
        if kind == "match_sum":
            value = self.evaluate(expression.children[0], environment)
            if not isinstance(value, SumValue):
                raise BenchmarkError("sum match received a non-sum value")
            left_name, right_name = expression.value
            local = dict(environment)
            if value.side == "left":
                local[left_name] = value.value
                return self.evaluate(expression.children[1], local)
            local[right_name] = value.value
            return self.evaluate(expression.children[2], local)
        if kind == "primitive":
            return self.evaluate_primitive(expression, environment)
        raise BenchmarkError(f"unsupported core expression {kind!r}")

    def arguments(self, expression: Expr, environment: dict[str, Any]) -> list[Any]:
        return [self.evaluate(child, environment) for child in expression.children]

    @staticmethod
    def probabilities(values: Iterable[Any], *, remainder: bool) -> torch.Tensor:
        items = list(values)
        if not items:
            if remainder:
                return torch.ones(1, dtype=torch.get_default_dtype())
            raise BenchmarkError("discrete requires at least one probability")
        probabilities = torch.stack(items)
        if bool((probabilities < 0).any()):
            raise BenchmarkError("discrete probabilities must be nonnegative")
        if remainder:
            rest = 1 - probabilities.sum()
            if float(rest) < -1e-10:
                raise BenchmarkError("discrete probabilities sum to more than one")
            probabilities = torch.cat((probabilities, rest.clamp_min(0).reshape(1)))
        return probabilities

    def distribution(self, name: str, arguments: list[Any]):
        if name == "uniform":
            if float(arguments[0]) == float(arguments[1]):
                return dist.Delta(arguments[0])
            return dist.Uniform(arguments[0], arguments[1])
        if name == "gauss":
            if float(arguments[1]) == 0:
                return dist.Delta(arguments[0])
            return dist.Normal(arguments[0], arguments[1].sqrt())
        if name == "poisson":
            return dist.Poisson(arguments[0])
        if name == "exponential":
            return dist.Exponential(arguments[0])
        if name == "bernoulli":
            return dist.Bernoulli(probs=arguments[0])
        if name == "beta":
            return dist.Beta(arguments[0], arguments[1])
        if name == "gamma":
            return dist.Gamma(arguments[0], arguments[1])
        if name == "discrete":
            return dist.Categorical(probs=self.probabilities(arguments, remainder=False))
        if name == "discrete_list":
            return dist.Categorical(probs=self.probabilities(arguments[0], remainder=True))
        raise BenchmarkError(f"unsupported distribution {name!r}")

    def mean(self, name: str, arguments: list[Any]):
        if name == "uniform":
            if float(arguments[0]) > float(arguments[1]):
                raise BenchmarkError("mean_uniform is outside its parameter domain")
            return (arguments[0] + arguments[1]) / 2
        if name == "gauss":
            if float(arguments[1]) < 0:
                raise BenchmarkError("mean_gauss is outside its parameter domain")
            return arguments[0]
        if name == "poisson":
            if float(arguments[0]) < 0:
                raise BenchmarkError("mean_poisson is outside its parameter domain")
            return arguments[0]
        if name == "bernoulli":
            if not 0 <= float(arguments[0]) <= 1:
                raise BenchmarkError("mean_bernoulli is outside its parameter domain")
            return arguments[0]
        if name == "exponential":
            if float(arguments[0]) <= 0:
                raise BenchmarkError("mean_exponential is outside its parameter domain")
            return 1 / arguments[0]
        if name == "beta":
            if float(arguments[0]) <= 0 or float(arguments[1]) <= 0:
                raise BenchmarkError("mean_beta is outside its parameter domain")
            return arguments[0] / (arguments[0] + arguments[1])
        if name == "gamma":
            if float(arguments[0]) <= 0 or float(arguments[1]) <= 0:
                raise BenchmarkError("mean_gamma is outside its parameter domain")
            return arguments[0] / arguments[1]
        if name == "discrete":
            probabilities = self.probabilities(arguments, remainder=False)
        elif name == "discrete_list":
            probabilities = self.probabilities(arguments[0], remainder=True)
        else:
            raise BenchmarkError(f"unsupported mean primitive {name!r}")
        support = torch.arange(probabilities.numel(), dtype=probabilities.dtype)
        return (probabilities * support).sum()

    def evaluate_primitive(self, expression: Expr, environment: dict[str, Any]):
        name, _affinity = expression.value
        arguments = self.arguments(expression, environment)
        if name == "observe":
            site = self.fresh("observe")
            if self.phase == "model":
                condition = arguments[0]
                log_factor = self.tensor(0.0 if bool(condition.item()) else -math.inf)
                pyro.factor(site, log_factor)
            return None
        if name.startswith("mean_"):
            return self.mean(name.removeprefix("mean_"), arguments)
        distribution = self.distribution(name, arguments)
        return pyro.sample(self.fresh(name), distribution)


def normalized(log_weights: torch.Tensor) -> torch.Tensor:
    if torch.isnan(log_weights).any():
        raise BenchmarkError("inference produced NaN weights")
    if torch.isneginf(log_weights).all():
        raise BenchmarkError("all samples were rejected; increase --samples")
    return torch.softmax(log_weights, dim=0)


def weighted_moments(values: torch.Tensor, weights: torch.Tensor) -> tuple[float, float, float]:
    mean = (values * weights).sum()
    variance = (weights * (values - mean).square()).sum()
    ess = 1 / weights.square().sum()
    return float(mean), float(variance), float(ess)


def importance_draws(runtime: PyroProgram, samples: int) -> tuple[torch.Tensor, torch.Tensor]:
    proposals = [one_proposal(runtime) for _ in range(samples)]
    return (
        torch.tensor([proposal[0] for proposal in proposals]),
        torch.tensor([proposal[1] for proposal in proposals]),
    )


def run_importance(runtime: PyroProgram, samples: int) -> tuple[float, float, float, None]:
    values, log_weights = importance_draws(runtime, samples)
    mean, variance, ess = weighted_moments(values, normalized(log_weights))
    return mean, variance, ess, None


def autocorrelation_ess(values: Sequence[float]) -> float:
    count = len(values)
    if count < 2:
        return float(count)
    mean = statistics.fmean(values)
    centered = [value - mean for value in values]
    variance = sum(value * value for value in centered) / count
    if variance == 0:
        return float(count)
    correlations: list[float] = []
    for lag in range(1, count):
        covariance = sum(
            centered[index] * centered[index + lag] for index in range(count - lag)
        ) / (count - lag)
        correlations.append(covariance / variance)
    total = 0.0
    for index in range(0, len(correlations), 2):
        pair = correlations[index:index + 2]
        pair_sum = sum(pair)
        if pair_sum <= 0:
            break
        total += pair_sum
    return min(float(count), count / max(1.0, 1 + 2 * total))


def one_proposal(runtime: PyroProgram) -> tuple[float, float]:
    guide_trace = poutine.trace(runtime.guide).get_trace()
    model_trace = poutine.trace(poutine.replay(runtime.model, trace=guide_trace)).get_trace()
    guide_trace.compute_log_prob()
    model_trace.compute_log_prob()
    value = model_trace.nodes["_RETURN"]["value"].detach().reshape(())
    log_weight = model_trace.log_prob_sum() - guide_trace.log_prob_sum()
    return float(value), float(log_weight)


def run_mcmc(
    runtime: PyroProgram, samples: int, warmup: int, max_initial_attempts: int,
) -> tuple[float, float, float, float]:
    current_value = 0.0
    current_weight = -math.inf
    for _ in range(max_initial_attempts):
        current_value, current_weight = one_proposal(runtime)
        if math.isfinite(current_weight):
            break
    else:
        raise BenchmarkError(
            f"MCMC could not find an accepted initial trace in {max_initial_attempts} attempts"
        )

    chain: list[float] = []
    accepted = 0
    total_steps = warmup + samples
    for step in range(total_steps):
        candidate_value, candidate_weight = one_proposal(runtime)
        log_acceptance = min(0.0, candidate_weight - current_weight)
        uniform = max(torch.rand(()).item(), sys.float_info.min)
        if math.log(uniform) < log_acceptance:
            current_value, current_weight = candidate_value, candidate_weight
            accepted += 1
        if step >= warmup:
            chain.append(current_value)
    mean = statistics.fmean(chain)
    variance = statistics.pvariance(chain)
    return mean, variance, autocorrelation_ess(chain), accepted / total_steps


def run_trial(
    runtime: PyroProgram, subject: str, algorithm: str, samples: int,
    warmup: int, seed: int, max_initial_attempts: int,
) -> Trial:
    pyro.clear_param_store()
    pyro.set_rng_seed(seed)
    started = time.perf_counter()
    if algorithm == "importance":
        estimate, variance, ess, acceptance = run_importance(runtime, samples)
    else:
        estimate, variance, ess, acceptance = run_mcmc(
            runtime, samples, warmup, max_initial_attempts,
        )
    return Trial(
        subject, algorithm, seed, estimate, variance, ess,
        time.perf_counter() - started, acceptance,
    )


def summarize(
    trials: list[Trial], truth: float | None = None,
) -> dict[str, float | int | None]:
    estimates = [trial.estimate for trial in trials]
    acceptances = [trial.acceptance_rate for trial in trials if trial.acceptance_rate is not None]
    return {
        "runs": len(trials),
        "mean_estimate": statistics.fmean(estimates),
        "estimator_variance": statistics.variance(estimates) if len(estimates) > 1 else None,
        "mean_output_variance": statistics.fmean(t.output_variance for t in trials),
        "mean_ess": statistics.fmean(t.ess for t in trials),
        "mean_seconds": statistics.fmean(t.seconds for t in trials),
        "mean_acceptance_rate": statistics.fmean(acceptances) if acceptances else None,
        "rmse": (
            math.sqrt(statistics.fmean((estimate - truth) ** 2 for estimate in estimates))
            if truth is not None else None
        ),
    }


def ratio(numerator: float | int | None, denominator: float | int | None) -> float | None:
    if numerator is None or denominator is None:
        return None
    if denominator == 0:
        return math.inf if numerator > 0 else None
    return float(numerator / denominator)


def comparison(
    summaries: dict[str, dict[str, float | int | None]], samples: int,
) -> dict[str, Any]:
    source, target = summaries["source"], summaries["determinized"]
    return {
        "output_variance_reduction_factor": ratio(
            source["mean_output_variance"], target["mean_output_variance"]
        ),
        "estimator_variance_reduction_factor": ratio(
            source["estimator_variance"], target["estimator_variance"]
        ),
        "ess_ratio": ratio(target["mean_ess"], source["mean_ess"]),
        "rmse_reduction_factor": ratio(source["rmse"], target["rmse"]),
        "rmse_vs_samples": {
            subject: {"samples": samples, "rmse": summary["rmse"]}
            for subject, summary in summaries.items()
        },
        "rmse_vs_wall_clock_time": {
            subject: {
                "mean_seconds": summary["mean_seconds"], "rmse": summary["rmse"],
            }
            for subject, summary in summaries.items()
        },
    }


def format_number(value: Any) -> str:
    if value is None:
        return "n/a"
    if isinstance(value, int):
        return str(value)
    return f"{value:.6g}"


def resolve_models(arguments: Sequence[Path], model_dir: Path) -> list[Path]:
    if not arguments:
        return sorted(model_dir.glob("*.det"))
    result: list[Path] = []
    for argument in arguments:
        candidate = argument if argument.is_file() else model_dir / argument
        if candidate.suffix != ".det":
            candidate = candidate.with_suffix(".det")
        if not candidate.is_file():
            raise BenchmarkError(f"model not found: {argument}")
        result.append(candidate)
    return result


def resolve_truths(
    truth: float | None, truth_file: Path | None, models: Sequence[Path],
) -> dict[str, float]:
    if truth is not None:
        if len(models) != 1:
            raise BenchmarkError("--truth requires exactly one model")
        return {models[0].stem: truth}
    if truth_file is None:
        return {}
    try:
        raw = json.loads(truth_file.read_text())
    except (OSError, json.JSONDecodeError) as error:
        raise BenchmarkError(f"could not read truth file {truth_file}: {error}") from error
    if not isinstance(raw, dict):
        raise BenchmarkError("truth file must contain a JSON object")
    result: dict[str, float] = {}
    for name, value in raw.items():
        if isinstance(value, bool) or not isinstance(value, (int, float)):
            raise BenchmarkError(f"truth for {name!r} must be numeric")
        result[str(name)] = float(value)
    return result


def print_summary(subject: str, summary: dict[str, Any]) -> None:
    print(f"    {subject}:")
    for key in (
        "mean_estimate", "mean_output_variance", "estimator_variance",
        "rmse", "mean_ess", "mean_acceptance_rate", "mean_seconds",
    ):
        if summary.get(key) is not None:
            print(f"      {key}: {format_number(summary[key])}")


def print_comparison(compared: dict[str, Any]) -> None:
    print("    comparison:")
    for key in (
        "output_variance_reduction_factor", "estimator_variance_reduction_factor",
        "ess_ratio", "rmse_reduction_factor",
    ):
        print(f"      {key}: {format_number(compared[key])}")
    if compared["rmse_reduction_factor"] is None:
        print("      RMSE unavailable: provide --truth or --truth-file")
        return
    print("      RMSE vs samples:")
    for subject, point in compared["rmse_vs_samples"].items():
        print(
            f"        {subject}: samples={point['samples']}, "
            f"rmse={format_number(point['rmse'])}"
        )
    print("      RMSE vs wall-clock time:")
    for subject, point in compared["rmse_vs_wall_clock_time"].items():
        print(
            f"        {subject}: mean_seconds={format_number(point['mean_seconds'])}, "
            f"rmse={format_number(point['rmse'])}"
        )


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("models", nargs="*", type=Path)
    parser.add_argument("--model-dir", type=Path, default=DEFAULT_MODEL_DIR)
    parser.add_argument(
        "--algorithm", choices=("both", "importance", "mcmc"), default="both"
    )
    parser.add_argument("--samples", type=int, default=1000)
    parser.add_argument("--runs", type=int, default=5)
    parser.add_argument("--warmup", type=int, default=200)
    parser.add_argument("--seed", type=int, default=1)
    parser.add_argument("--max-initial-attempts", type=int, default=10000)
    parser.add_argument("--binary", type=Path, default=DEFAULT_BINARY)
    parser.add_argument("--dump-dir", type=Path)
    parser.add_argument("--json-out", type=Path)
    truth_group = parser.add_mutually_exclusive_group()
    truth_group.add_argument(
        "--truth", type=float, help="reference expectation for a single model"
    )
    truth_group.add_argument(
        "--truth-file", type=Path,
        help="JSON object mapping model stems to reference expectations",
    )
    args = parser.parse_args(argv)
    if args.samples < 2 or args.runs < 1 or args.warmup < 0:
        parser.error("--samples must be at least 2; --runs positive; --warmup nonnegative")
    if args.max_initial_attempts < 1:
        parser.error("--max-initial-attempts must be positive")

    try:
        models = resolve_models(args.models, args.model_dir)
        truths = resolve_truths(args.truth, args.truth_file, models)
    except BenchmarkError as error:
        parser.error(str(error))
    if not models:
        parser.error("no .det models found")
    algorithms = ["importance", "mcmc"] if args.algorithm == "both" else [args.algorithm]
    payload: dict[str, Any] = {
        "configuration": {
            "algorithms": algorithms, "samples": args.samples, "runs": args.runs,
            "warmup": args.warmup, "seed": args.seed, "binary": str(args.binary),
            "truths": truths,
        },
        "models": {},
    }
    failed = False

    for model_index, model in enumerate(models):
        print(f"model: {model}")
        record: dict[str, Any] = {}
        payload["models"][model.stem] = record
        try:
            compiled = compile_with_lean(model, args.binary)
            record["checked_type"] = compiled.checked_type
            truth = truths.get(model.stem)
            record["truth"] = truth
            if not compiled.checked_type.startswith("float["):
                raise BenchmarkError(
                    f"model is open or non-scalar ({compiled.checked_type}); "
                    "apply its top-level function to concrete arguments"
                )
            if args.dump_dir:
                args.dump_dir.mkdir(parents=True, exist_ok=True)
                (args.dump_dir / f"{model.stem}.source.det").write_text(compiled.source_text + "\n")
                (args.dump_dir / f"{model.stem}.determinized.det").write_text(compiled.target_text + "\n")
            record["algorithms"] = {}
            for algorithm_index, algorithm in enumerate(algorithms):
                print(f"  algorithm: {algorithm}")
                summaries: dict[str, dict[str, Any]] = {}
                trial_record: dict[str, list[dict[str, Any]]] = {}
                for subject_index, (subject, expression) in enumerate((
                    ("source", compiled.source), ("determinized", compiled.target),
                )):
                    runtime = PyroProgram(expression)
                    trials = []
                    for run in range(args.runs):
                        seed = (
                            args.seed + model_index * 1_000_000
                            + algorithm_index * 100_000 + run
                        )
                        trials.append(run_trial(
                            runtime, subject, algorithm, args.samples, args.warmup,
                            seed, args.max_initial_attempts,
                        ))
                    summaries[subject] = summarize(trials, truth)
                    trial_record[subject] = [asdict(trial) for trial in trials]
                    print_summary(subject, summaries[subject])
                compared = comparison(summaries, args.samples)
                print_comparison(compared)
                record["algorithms"][algorithm] = {
                    "summaries": summaries, "comparison": compared, "trials": trial_record,
                }
        except Exception as error:  # continue benchmarking the remaining files
            failed = True
            record["error"] = str(error)
            print(f"  error: {error}", file=sys.stderr)

    if args.json_out:
        args.json_out.parent.mkdir(parents=True, exist_ok=True)
        args.json_out.write_text(json.dumps(payload, indent=2, allow_nan=True) + "\n")
        print(f"wrote {args.json_out}")
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
