# Shared .det test corpus

The programs and expected answers belong to the language. `cases.toml` is the
authoritative test manifest; Lean runs the programs and checks their results.

From the repository root:

```sh
./lean/test.sh                # Unit tests, corpus fast checks, kernel certificates
./lean/test.sh --statistical  # Unit tests and sampled source/target moment checks
./lean/test.sh --all          # Both suites and kernel certificates
```

Requirements: the repository's Lean/Lake toolchain and Python 3.11+ (standard
library only). `run.py` validates TOML and passes JSON to the compiled Lean runner
in `lean/Tests/Corpus.lean`. Lean performs parsing, inference, certificate checking,
execution, and numerical assertions. The suite also checks that incorrect results,
incorrect moments, runtime failures, and corpus omissions make the tools fail. Temporary manifests and certificates are
created outside the repository and deleted automatically.

## Layout

- `execution/`: exact numeric or structural results, draw counts, and runtime errors.
- `statistical/`: analytical source and determinized means and variances.
- `typing/accept/`, `typing/reject/`: acceptance, inferred types/modes, or rejection stage.
- `../examples/`: reader-facing examples, registered in the same manifest.
- `../lean/Tests/`: test runner and internal parser/checker/runtime unit tests.

`legacy/` contains the original `det/` programs, moved without changing their
contents. Former Lean fixtures are in `typing/accept/`. Existing examples remain
in place. `examples/loops/example.det` is explicitly excluded because it contains
informal sketches rather than one executable program.

Every `.det` under `tests/` and `examples/` must occur exactly once in the manifest
or its exclusions. Missing files, duplicate entries, unknown fields, missing
expectations, and unregistered files fail manifest validation. The fast suite
compiles statistical cases too; it skips only their sampling loops.

## Adding a case

Create a `.det` file containing only the program, then register it:

```toml
[[case]]
file = "tests/execution/my-case.det"
suite = "execution"
outcome = "accept"
expected_type = "float[E]"
source = { number = 7 }
target = { number = 7 }
```

Numeric results have an absolute tolerance of `1e-10` unless overridden. `value`
checks the displayed form of a Boolean, unit, pair, sum, or list. `draws` can accompany
a result. `error` expects a runtime diagnostic substring. `fuel` limits evaluation
steps per run; the default is 100,000.

A rejection case uses `outcome = "reject"` and `stage = "parse"`, `"elaboration"`,
`"inference"`, or `"certificate"`. Acceptance cases may assert `expected_type` and
`modes` (sample annotations in syntax traversal order). `kernel = true` independently
checks an exported certificate using `decide +kernel` in the fast/full suite.

Compilation-only cases deliberately make no claim about termination or parameter
domains. For example, `typing/accept/legacy/funny.det` can choose invalid distribution
parameters; acceptance must not be interpreted as successful execution.

## Statistical ground truth

Each case specifies 20,000 samples, a fixed seed, a per-run step limit, and separate
source/target moments. Seeds vary deterministically across trials. The runner uses
Welford's algorithm and the unbiased sample variance. It checks finiteness and,
where specified, support bounds and integer-valued outputs. Runtime failures fail
the test; there is no rejection filtering or averaging only successful runs.

Tolerances were chosen from analytical moments, before observing the sampled
results. With variance `v`, central fourth moment `m4`, and sample count `n`:

```text
mean tolerance     = 7 sqrt(v / n)
variance tolerance = 7 sqrt((m4 - ((n-3)/(n-1)) v²) / n)
```

A numerical floor of `1e-10` handles deterministic outputs. These are conservative
moment checks, not a proof of distributional correctness or a comprehensive RNG
test. Fixed seeds make failures reproducible. Do not loosen tolerances to accommodate
a failing run without investigating the implementation and analytical answer.

The manifest's `derivation` refers to the rows below. Unless specified, the target
is constant at the source mean and has zero variance.

| Derivation | Source mean | Source variance | Source central fourth moment / bound |
| --- | ---: | ---: | ---: |
| uniform: U(-1,3) | 1 | 4/3 | 16/5 |
| gaussian: N(3,4), second parameter is variance | 3 | 4 | 48 |
| poisson: rate 4 | 4 | 4 | 52 |
| poisson-large: rate 45, exercises splitting | 45 | 45 | 6120 |
| exponential: rate 2 | 1/2 | 1/4 | 9/16 |
| gamma: shape 3, rate 2 | 3/2 | 3/4 | 45/16 |
| gamma-small: shape 1/2, rate 2 | 1/4 | 1/8 | 15/64 |
| beta: parameters 2,3 | 2/5 | 1/25 | 33/8750 |
| affine: 2 U(0,1) + 3 | 4 | 1/3 | 1/5 |
| nested: X + Y, X ~ U(0,1), Y given X ~ U(X,2) | 7/4 | 55/144 | bound: 16 × 55/144 |
| mixed: independent G and E uniforms on (0,1) | 1 | 1/6 | 1/15 |
| branch: probability 1/4 of U(0,2), otherwise U(0,1) | 5/8 | 37/192 | bound: 1 |
| degenerate: U(2,2) + N(3,0) + Poisson(0) | 5 | 0 | 0 |
| bernoulli-e: probability 3/10, E draw | 3/10 | 21/100 | 777/10000 |
| bernoulli-mixed: uniform G probability, E Bernoulli | 1/2 | 1/4 | 1/16 |
| bernoulli-nested: uniform probability in [0,1], E draws | 1/2 | 1/4 | 1/16 |
| bernoulli: probability 3/10, retained G draw | 3/10 | 21/100 | 777/10000 |
| discrete-e: probabilities 1/4,1/4,1/2, E draw | 5/4 | 11/16 | 197/256 |
| discrete: probabilities 1/4,1/4,1/2 for outcomes 0,1,2 | 5/4 | 11/16 | 197/256 |

For `mixed`, the target is G + 1/2: variance 1/12, central fourth moment 1/80.
For `branch`, the target is 1 with probability 1/4, otherwise 1/2: variance 3/64,
central fourth moment 21/4096. The G-mode Bernoulli and discrete targets retain the
source moments. For `bernoulli-nested`, averaging the conditional success probability
gives a Bernoulli(1/2) source and a constant 1/2 target. For `bernoulli-mixed`, the
target retains U(0,1): variance 1/12 and central fourth moment 1/80. These expectations test each execution separately against ground truth;
agreement between two implementations is not the oracle.

For the nested case, total variance gives `Var(1 + 3X/2) + E[(2-X)²/12] = 55/144`.
Its bounded support gives the conservative fourth-moment bound shown above. The
branch bound follows from bounded support and is also conservative. Distribution
formulas use central fourth moments: Poisson `λ + 3λ²`, gamma `3a(a+2)/b⁴`, and
beta `(3 + excess-kurtosis) × variance²`.

## CLI, model, and result integration

`test_workflows.py` checks the public `run.sh` entry points and relative paths.
`test_export.py` checks exact model exports and independent paper-correspondence
certificates. `test_results.py` checks exact expected rewards, malformed certificates,
nonabsorption, resource limits, and failure reports. Set `STORM_PYTHON` to an
interpreter with `tools/storm-requirements.txt` installed for real Storm comparisons.

`./det.sh` delegates to the Lean suite. OCaml and its generated baselines have
been retired; their recovery location and intentional behavioral differences are
in [migration-audit.md](../migration-audit.md).

The coupling prototype's motivating examples use `nested`, `coupling-branch`, and
`coupling-mixed`. Simulator projection/trace tests read these same files.
For `coupling-branch`, the source is an equal mixture of U(0,1) and U(2,4): mean
7/4, variance 85/48, fourth central moment 5761/1280. The target is an equal mixture
of 1/2 and 3: variance 25/16, fourth central moment 625/256.
For `coupling-mixed`, the source moments are those of `nested`, and the target is
1 + (3/2)U(0,1): mean 7/4, variance 3/16, fourth central moment 81/1280.
The same analytical tolerance formula above applies.
