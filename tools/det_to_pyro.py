#!/usr/bin/env python3
"""Compile the declarative `.det` model subset to executable Pyro.

The supported backend forms are documented in
``experiments/pyro_models/README.md``. This module also exposes an interpreter
used by ``tools/run_pyro.py`` for Pyro Importance and SMCFilter experiments.
"""

from __future__ import annotations

import argparse
import functools
import json
import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Literal, Mapping, Sequence


Subject = Literal["source", "determinized"]


class CompileError(ValueError):
    pass


@dataclass(frozen=True)
class Token:
    text: str
    offset: int


@dataclass(frozen=True)
class Number:
    text: str


@dataclass(frozen=True)
class Variable:
    name: str


@dataclass(frozen=True)
class Unary:
    operator: str
    operand: "Expression"


@dataclass(frozen=True)
class Binary:
    operator: str
    left: "Expression"
    right: "Expression"


@dataclass(frozen=True)
class Call:
    name: str
    arguments: tuple["Expression", ...]
    affinity: str | None = None


Expression = Number | Variable | Unary | Binary | Call


@dataclass(frozen=True)
class Binding:
    name: str
    expression: Expression
    index: int


@dataclass(frozen=True)
class Program:
    bindings: tuple[Binding, ...]
    result: Expression
    source_name: str = "model"


_NUMBER = re.compile(r"(?:\d+\.\d*|\.\d+|\d+)(?:[eE][+-]?\d+)?")
_IDENTIFIER = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
_BINARY_PRECEDENCE = {"<": 10, "<=": 10, "+": 20, "-": 20, "*": 30, "/": 30}
_DISTRIBUTIONS = {
    "uniform", "gauss", "gaussian", "poisson", "exponential",
    "bernoulli", "beta", "gamma", "flat", "log_uniform",
}
_OBSERVATIONS = {"observe", "observe_gauss", "observe_poisson"}


def _strip_comments(source: str) -> str:
    output: list[str] = []
    index = 0
    depth = 0
    while index < len(source):
        pair = source[index:index + 2]
        if pair == "(*":
            depth += 1
            output.extend("  ")
            index += 2
        elif pair == "*)" and depth:
            depth -= 1
            output.extend("  ")
            index += 2
        else:
            output.append("\n" if depth and source[index] == "\n" else " " if depth else source[index])
            index += 1
    if depth:
        raise CompileError("unterminated comment")
    return "".join(output)


def tokenize(source: str) -> list[Token]:
    source = _strip_comments(source)
    result: list[Token] = []
    index = 0
    while index < len(source):
        if source[index].isspace():
            index += 1
            continue
        if source.startswith("<=", index):
            result.append(Token("<=", index))
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
        if source[index] in "()[],=+-*/<":
            result.append(Token(source[index], index))
            index += 1
            continue
        raise CompileError(f"unexpected character {source[index]!r} at offset {index}")
    result.append(Token("<eof>", len(source)))
    return result


class Parser:
    def __init__(self, tokens: Sequence[Token], source_name: str):
        self.tokens = tokens
        self.position = 0
        self.source_name = source_name

    def peek(self) -> Token:
        return self.tokens[self.position]

    def take(self, expected: str | None = None) -> Token:
        token = self.peek()
        if expected is not None and token.text != expected:
            raise CompileError(
                f"{self.source_name}:{token.offset}: expected {expected!r}, got {token.text!r}"
            )
        self.position += 1
        return token

    def parse(self) -> Program:
        bindings: list[Binding] = []
        while self.peek().text == "let":
            self.take("let")
            name = self.take().text
            if not _IDENTIFIER.fullmatch(name):
                raise CompileError(f"expected binding name, got {name!r}")
            self.take("=")
            expression = self.expression()
            self.take("in")
            bindings.append(Binding(name, expression, len(bindings)))
        result = self.expression()
        self.take("<eof>")
        return Program(tuple(bindings), result, self.source_name)

    def expression(self, minimum_precedence: int = 0) -> Expression:
        token = self.peek().text
        if token == "-":
            self.take("-")
            left: Expression = Unary("-", self.expression(40))
        elif token == "(":
            self.take("(")
            left = self.expression()
            self.take(")")
        elif _NUMBER.fullmatch(token):
            left = Number(self.take().text)
        elif _IDENTIFIER.fullmatch(token):
            name = self.take().text
            affinity = None
            if self.peek().text == "[":
                self.take("[")
                affinity = self.take().text
                if affinity not in {"E", "G"}:
                    raise CompileError(f"expected E or G affinity, got {affinity!r}")
                self.take("]")
            if self.peek().text == "(":
                self.take("(")
                arguments: list[Expression] = []
                if self.peek().text != ")":
                    arguments.append(self.expression())
                    while self.peek().text == ",":
                        self.take(",")
                        arguments.append(self.expression())
                self.take(")")
                left = Call(name, tuple(arguments), affinity)
            elif affinity is not None:
                raise CompileError(f"affinity annotation on non-call {name!r}")
            elif name == "true":
                left = Number("1")
            elif name == "false":
                left = Number("0")
            else:
                left = Variable(name)
        else:
            raise CompileError(
                f"{self.source_name}:{self.peek().offset}: expected expression, got {token!r}"
            )

        while True:
            operator = self.peek().text
            precedence = _BINARY_PRECEDENCE.get(operator)
            if precedence is None or precedence < minimum_precedence:
                break
            self.take()
            right = self.expression(precedence + 1)
            left = Binary(operator, left, right)
        return left


def parse(source: str, source_name: str = "model") -> Program:
    program = Parser(tokenize(source), source_name).parse()
    validate(program)
    return program


def parse_file(path: str | Path) -> Program:
    path = Path(path)
    return parse(path.read_text(), str(path))


def references(expression: Expression) -> set[str]:
    if isinstance(expression, Number):
        return set()
    if isinstance(expression, Variable):
        return {expression.name}
    if isinstance(expression, Unary):
        return references(expression.operand)
    if isinstance(expression, Binary):
        return references(expression.left) | references(expression.right)
    result: set[str] = set()
    for argument in expression.arguments:
        result |= references(argument)
    return result


def validate(program: Program) -> None:
    bound: set[str] = set()
    for binding in program.bindings:
        missing = references(binding.expression) - bound
        if missing:
            raise CompileError(
                f"{program.source_name}: unbound variable(s) in {binding.name}: {sorted(missing)}"
            )
        if binding.name != "_":
            if binding.name in bound:
                raise CompileError(f"{program.source_name}: duplicate binding {binding.name!r}")
            bound.add(binding.name)
        _validate_expression(binding.expression, top_level=True)
    missing = references(program.result) - bound
    if missing:
        raise CompileError(f"{program.source_name}: unbound result variable(s): {sorted(missing)}")
    _validate_expression(program.result, top_level=False)


def _validate_expression(expression: Expression, *, top_level: bool) -> None:
    if isinstance(expression, (Number, Variable)):
        return
    if isinstance(expression, Unary):
        _validate_expression(expression.operand, top_level=False)
        return
    if isinstance(expression, Binary):
        _validate_expression(expression.left, top_level=False)
        _validate_expression(expression.right, top_level=False)
        return
    if expression.name in _DISTRIBUTIONS | _OBSERVATIONS and not top_level:
        raise CompileError(f"{expression.name} is only supported as a let-bound top-level action")
    if expression.name not in _DISTRIBUTIONS | _OBSERVATIONS | {"exp"}:
        raise CompileError(f"unsupported call {expression.name!r}")
    if expression.name in _DISTRIBUTIONS and expression.affinity not in {"E", "G"}:
        raise CompileError(f"distribution {expression.name} requires [E] or [G]")
    if expression.name not in _DISTRIBUTIONS and expression.affinity is not None:
        raise CompileError(f"{expression.name} does not accept an affinity")
    expected = {
        "uniform": 2, "gauss": 2, "gaussian": 2, "poisson": 1,
        "exponential": 1, "bernoulli": 1, "beta": 2, "gamma": 2,
        "flat": 0, "log_uniform": 0, "observe": 1,
        "observe_gauss": 3, "observe_poisson": 2, "exp": 1,
    }[expression.name]
    if len(expression.arguments) != expected:
        raise CompileError(
            f"{expression.name} expects {expected} argument(s), got {len(expression.arguments)}"
        )
    for argument in expression.arguments:
        _validate_expression(argument, top_level=False)


def is_distribution(expression: Expression) -> bool:
    return isinstance(expression, Call) and expression.name in _DISTRIBUTIONS


def is_observation(expression: Expression) -> bool:
    return isinstance(expression, Call) and expression.name in _OBSERVATIONS


def load_proposals(path: str | Path | None, model_name: str) -> dict[str, Any]:
    if path is None:
        return {}
    data = json.loads(Path(path).read_text())
    if model_name in data and isinstance(data[model_name], dict):
        return data[model_name]
    return data


def python_expression(expression: Expression) -> str:
    if isinstance(expression, Number):
        return f"_t({expression.text})"
    if isinstance(expression, Variable):
        return expression.name
    if isinstance(expression, Unary):
        return f"(-{python_expression(expression.operand)})"
    if isinstance(expression, Binary):
        return f"({python_expression(expression.left)} {expression.operator} {python_expression(expression.right)})"
    if expression.name == "exp":
        return f"torch.exp({python_expression(expression.arguments[0])})"
    raise CompileError(f"action {expression.name!r} cannot occur inside a numerical expression")


def distribution_python(call: Call) -> str:
    args = [python_expression(argument) for argument in call.arguments]
    name = "gauss" if call.name == "gaussian" else call.name
    if name == "uniform":
        return f"dist.Uniform({args[0]}, {args[1]})"
    if name == "gauss":
        return f"dist.Normal({args[0]}, torch.sqrt({args[1]}))"
    if name == "poisson":
        return f"dist.Poisson({args[0]})"
    if name == "exponential":
        return f"dist.Exponential({args[0]})"
    if name == "bernoulli":
        return f"dist.Bernoulli(probs={args[0]})"
    if name == "beta":
        return f"dist.Beta({args[0]}, {args[1]})"
    if name == "gamma":
        return f"dist.Gamma({args[0]}, {args[1]})"
    if name == "flat":
        return "dist.ImproperUniform(constraints.real, torch.Size(), torch.Size())"
    if name == "log_uniform":
        return "dist.ImproperUniform(constraints.positive, torch.Size(), torch.Size())"
    raise CompileError(f"unsupported distribution {call.name!r}")


def mean_python(call: Call) -> str:
    args = [python_expression(argument) for argument in call.arguments]
    name = "gauss" if call.name == "gaussian" else call.name
    if name == "uniform":
        return f"(({args[0]} + {args[1]}) / _t(2))"
    if name == "gauss":
        return args[0]
    if name in {"poisson", "bernoulli"}:
        return args[0]
    if name == "exponential":
        return f"(_t(1) / {args[0]})"
    if name == "beta":
        return f"({args[0]} / ({args[0]} + {args[1]}))"
    if name == "gamma":
        return f"({args[0]} / {args[1]})"
    raise CompileError(f"improper distribution {name} cannot be determinized")


def proposal_python(name: str, call: Call, proposals: Mapping[str, Any]) -> str:
    if call.name not in {"flat", "log_uniform"}:
        return distribution_python(call)
    proposal = proposals.get(name)
    if not isinstance(proposal, Mapping):
        raise CompileError(
            f"improper site {name!r} requires an entry in the proposal configuration"
        )
    distribution = proposal.get("distribution")
    if distribution == "normal" and call.name == "flat":
        return f"dist.Normal(_t({proposal['loc']!r}), _t({proposal['scale']!r}))"
    if distribution == "lognormal" and call.name == "log_uniform":
        median = float(proposal["median"])
        log_scale = float(proposal["log_scale"])
        if median <= 0 or log_scale <= 0:
            raise CompileError(f"invalid lognormal proposal for {name!r}")
        return f"dist.LogNormal(_t({math.log(median)!r}), _t({log_scale!r}))"
    raise CompileError(f"proposal for {name!r} is incompatible with {call.name}")


def generate_python(program: Program, subject: Subject, proposals: Mapping[str, Any]) -> str:
    model_lines: list[str] = []
    guide_lines: list[str] = []
    observation_index = 0
    condition_index = 0
    for binding in program.bindings:
        expression = binding.expression
        name = binding.name
        if is_distribution(expression):
            assert isinstance(expression, Call)
            site = name if name != "_" else f"sample_{binding.index:04d}"
            if subject == "determinized" and expression.affinity == "E":
                model_lines.append(f"    {name} = {mean_python(expression)}")
                guide_lines.append(f"    {name} = {mean_python(expression)}")
            else:
                model_lines.append(
                    f"    {name} = pyro.sample({site!r}, {distribution_python(expression)})"
                )
                guide_lines.append(
                    f"    {name} = pyro.sample({site!r}, {proposal_python(site, expression, proposals)})"
                )
                if expression.name == "log_uniform":
                    model_lines.append(
                        f"    pyro.factor({site + '__log_uniform'!r}, -torch.log({name}))"
                    )
        elif is_observation(expression):
            assert isinstance(expression, Call)
            if expression.name == "observe_gauss":
                value, mean, variance = map(python_expression, expression.arguments)
                site = f"obs_gauss_{observation_index:04d}"
                model_lines.append(
                    f"    pyro.sample({site!r}, dist.Normal({mean}, torch.sqrt({variance})), obs={value})"
                )
            elif expression.name == "observe_poisson":
                value, rate = map(python_expression, expression.arguments)
                site = f"obs_poisson_{observation_index:04d}"
                model_lines.append(
                    f"    pyro.sample({site!r}, dist.Poisson({rate}), obs={value})"
                )
            else:
                condition = python_expression(expression.arguments[0])
                site = f"condition_{condition_index:04d}"
                model_lines.append(
                    f"    pyro.factor({site!r}, torch.where({condition}, _t(0), _NEG_INF))"
                )
                condition_index += 1
            guide_lines.append(f"    {name} = None")
            observation_index += 1
        else:
            value = python_expression(expression)
            model_lines.append(f"    {name} = {value}")
            guide_lines.append(f"    {name} = {value}")

    result = python_expression(program.result)
    header = f'''# Generated by tools/det_to_pyro.py from {program.source_name}.
# Subject: {subject}
import torch
import pyro
import pyro.distributions as dist
from torch.distributions import constraints

torch.set_default_dtype(torch.float64)
_NEG_INF = torch.tensor(float("-inf"))

def _t(value):
    return torch.as_tensor(value, dtype=torch.get_default_dtype())

'''
    model = (
        "def model():\n" + "\n".join(model_lines) +
        f"\n    query = {result}\n    pyro.deterministic('_query', query)\n    return query\n\n"
    )
    guide = "def guide():\n" + "\n".join(guide_lines) + "\n    return None\n"
    return header + model + guide


@functools.cache
def _runtime_modules():
    try:
        import torch
        import pyro
        import pyro.distributions as dist
        from torch.distributions import constraints
    except ImportError as error:
        raise RuntimeError(
            "Pyro backend requires torch and pyro-ppl; install tools/pyro-requirements.txt"
        ) from error
    torch.set_default_dtype(torch.float64)
    return torch, pyro, dist, constraints


class RuntimeProgram:
    """An executable Pyro model/guide pair compiled from a parsed program."""

    def __init__(
        self,
        program: Program,
        subject: Subject,
        proposals: Mapping[str, Any] | None = None,
    ):
        self.program = program
        self.subject = subject
        self.proposals = dict(proposals or {})

    @staticmethod
    def tensor(value: float | int):
        torch, _, _, _ = _runtime_modules()
        return torch.as_tensor(value, dtype=torch.get_default_dtype())

    def expression(self, expression: Expression, environment: Mapping[str, Any]):
        torch, _, _, _ = _runtime_modules()
        if isinstance(expression, Number):
            return self.tensor(float(expression.text))
        if isinstance(expression, Variable):
            return environment[expression.name]
        if isinstance(expression, Unary):
            return -self.expression(expression.operand, environment)
        if isinstance(expression, Binary):
            left = self.expression(expression.left, environment)
            right = self.expression(expression.right, environment)
            if expression.operator == "+":
                return left + right
            if expression.operator == "-":
                return left - right
            if expression.operator == "*":
                return left * right
            if expression.operator == "/":
                return left / right
            if expression.operator == "<":
                return left < right
            if expression.operator == "<=":
                return left <= right
            raise CompileError(f"unsupported operator {expression.operator!r}")
        if expression.name == "exp":
            return torch.exp(self.expression(expression.arguments[0], environment))
        raise CompileError(f"cannot numerically evaluate action {expression.name!r}")

    def distribution(self, call: Call, environment: Mapping[str, Any]):
        _, _, dist, constraints = _runtime_modules()
        arguments = [self.expression(argument, environment) for argument in call.arguments]
        name = "gauss" if call.name == "gaussian" else call.name
        if name == "uniform":
            return dist.Uniform(arguments[0], arguments[1])
        if name == "gauss":
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
        if name == "flat":
            return dist.ImproperUniform(constraints.real, (), ())
        if name == "log_uniform":
            return dist.ImproperUniform(constraints.positive, (), ())
        raise CompileError(f"unsupported distribution {call.name!r}")

    def mean(self, call: Call, environment: Mapping[str, Any]):
        arguments = [self.expression(argument, environment) for argument in call.arguments]
        name = "gauss" if call.name == "gaussian" else call.name
        if name == "uniform":
            return (arguments[0] + arguments[1]) / 2
        if name == "gauss":
            return arguments[0]
        if name in {"poisson", "bernoulli"}:
            return arguments[0]
        if name == "exponential":
            return 1 / arguments[0]
        if name == "beta":
            return arguments[0] / (arguments[0] + arguments[1])
        if name == "gamma":
            return arguments[0] / arguments[1]
        raise CompileError(f"improper distribution {name} cannot be determinized")

    def proposal(self, site: str, call: Call, environment: Mapping[str, Any]):
        _, _, dist, _ = _runtime_modules()
        if call.name not in {"flat", "log_uniform"}:
            return self.distribution(call, environment)
        proposal = self.proposals.get(site)
        if not isinstance(proposal, Mapping):
            raise CompileError(
                f"improper site {site!r} requires an entry in the proposal configuration"
            )
        if call.name == "flat" and proposal.get("distribution") == "normal":
            return dist.Normal(self.tensor(proposal["loc"]), self.tensor(proposal["scale"]))
        if call.name == "log_uniform" and proposal.get("distribution") == "lognormal":
            median = float(proposal["median"])
            log_scale = float(proposal["log_scale"])
            if median <= 0 or log_scale <= 0:
                raise CompileError(f"invalid lognormal proposal for {site!r}")
            return dist.LogNormal(self.tensor(math.log(median)), self.tensor(log_scale))
        raise CompileError(f"proposal for {site!r} is incompatible with {call.name}")

    def binding(
        self,
        binding: Binding,
        environment: dict[str, Any],
        phase: Literal["model", "guide"],
    ):
        torch, pyro, dist, _ = _runtime_modules()
        expression = binding.expression
        name = binding.name
        if is_distribution(expression):
            assert isinstance(expression, Call)
            site = name if name != "_" else f"sample_{binding.index:04d}"
            if self.subject == "determinized" and expression.affinity == "E":
                value = self.mean(expression, environment)
            elif phase == "guide":
                value = pyro.sample(site, self.proposal(site, expression, environment))
            else:
                value = pyro.sample(site, self.distribution(expression, environment))
                if expression.name == "log_uniform":
                    pyro.factor(f"{site}__log_uniform", -torch.log(value))
        elif is_observation(expression):
            assert isinstance(expression, Call)
            if phase == "guide":
                value = None
            elif expression.name == "observe_gauss":
                observed = self.expression(expression.arguments[0], environment)
                mean = self.expression(expression.arguments[1], environment)
                variance = self.expression(expression.arguments[2], environment)
                value = pyro.sample(
                    f"obs_gauss_{binding.index:04d}",
                    dist.Normal(mean, variance.sqrt()),
                    obs=observed,
                )
            elif expression.name == "observe_poisson":
                observed = self.expression(expression.arguments[0], environment)
                rate = self.expression(expression.arguments[1], environment)
                value = pyro.sample(
                    f"obs_poisson_{binding.index:04d}", dist.Poisson(rate), obs=observed
                )
            else:
                condition = self.expression(expression.arguments[0], environment)
                zero = torch.zeros_like(condition, dtype=torch.get_default_dtype())
                negative_infinity = torch.full_like(zero, float("-inf"))
                pyro.factor(
                    f"condition_{binding.index:04d}",
                    torch.where(condition, zero, negative_infinity),
                )
                value = None
        else:
            value = self.expression(expression, environment)
        if name != "_" and value is not None:
            environment[name] = value
        return value

    def execute(
        self,
        bindings: Sequence[Binding],
        environment: dict[str, Any],
        phase: Literal["model", "guide"],
    ) -> None:
        for binding in bindings:
            self.binding(binding, environment, phase)

    def model(self):
        _, pyro, _, _ = _runtime_modules()
        environment: dict[str, Any] = {}
        self.execute(self.program.bindings, environment, "model")
        query = self.expression(self.program.result, environment)
        pyro.deterministic("_query", query)
        return query

    def guide(self):
        environment: dict[str, Any] = {}
        self.execute(self.program.bindings, environment, "guide")
        return None

    def chunks(self) -> list[tuple[Binding, ...]]:
        chunks: list[tuple[Binding, ...]] = []
        current: list[Binding] = []
        for binding in self.program.bindings:
            current.append(binding)
            if is_observation(binding.expression):
                chunks.append(tuple(current))
                current = []
        # A final chunk records post-observation samples and the scalar query.
        chunks.append(tuple(current))
        return chunks


class SequentialModel:
    """Adapter from a straight-line RuntimeProgram to Pyro's SMCFilter API."""

    def __init__(self, runtime: RuntimeProgram):
        self.runtime = runtime
        self.chunks = runtime.chunks()
        self.index = 0

    def _run(self, state) -> None:
        environment = dict(state)
        bindings = self.chunks[self.index]
        self.runtime.execute(bindings, environment, "model")
        for binding in bindings:
            if binding.name != "_" and binding.name in environment:
                state[binding.name] = environment[binding.name]
        if self.index == len(self.chunks) - 1:
            state["__query"] = self.runtime.expression(self.runtime.program.result, environment)

    def init(self, state) -> None:
        self.index = 0
        self._run(state)

    def step(self, state) -> None:
        self.index += 1
        self._run(state)


class SequentialGuide:
    def __init__(self, runtime: RuntimeProgram):
        self.runtime = runtime
        self.chunks = runtime.chunks()
        self.index = 0

    def _run(self, state) -> None:
        environment = dict(state)
        self.runtime.execute(self.chunks[self.index], environment, "guide")

    def init(self, state) -> None:
        self.index = 0
        self._run(state)

    def step(self, state) -> None:
        self.index += 1
        self._run(state)


def default_proposals_path(model_path: Path) -> Path | None:
    candidate = model_path.parent / "proposals.json"
    return candidate if candidate.exists() else None


def main(argv: Sequence[str] | None = None) -> int:
    argument_parser = argparse.ArgumentParser(description=__doc__)
    argument_parser.add_argument("model", type=Path)
    argument_parser.add_argument(
        "--subject", choices=["source", "determinized"], required=True
    )
    argument_parser.add_argument("--output", "-o", type=Path, required=True)
    argument_parser.add_argument("--proposals", type=Path)
    args = argument_parser.parse_args(argv)

    program = parse_file(args.model)
    proposal_path = args.proposals or default_proposals_path(args.model)
    proposals = load_proposals(proposal_path, args.model.stem)
    generated = generate_python(program, args.subject, proposals)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(generated)
    print(f"Wrote {args.subject} Pyro model: {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
