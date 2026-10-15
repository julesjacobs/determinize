# Estimator variance experiment

Build and run from the repository root (Python requires matplotlib):

```sh
(cd lean && lake build sampling-bench)
python3 tools/benchmark_estimator_variance.py --output /tmp/estimator-variance-rerun
```

The defaults run branching, HMM, and load with 1,000, 3,000, 10,000, 30,000,
and 100,000 requested executions, and 50 independent replicates per size.
Source and determinized executions use matching seeds; seed ranges are disjoint
between replicates and sizes. Subject order alternates between replicates.
Each separate process performs 1,000 untimed warmup executions before timing
the sampling loop and online moment calculation. Parsing, type inference,
determinization, process startup, and output are excluded from sampling time.

Rejected observations contribute no output. Each mean uses all returned values
from the requested executions. Any execution failure, step-limit exhaustion,
process timeout, non-finite result, or incomplete count stops the experiment
with an error. The failed trial and available diagnostics are saved.

`runs.jsonl` contains every replicate, including requested, returned, rejected,
and failed counts, the seed, exact binary64 moments, and elapsed nanoseconds.
`experiment.json` records the configuration, machine, source and executable
hashes, and totals. `summary.csv` and the PNGs are derived from these records.
Estimator variance uses the sample variance of the 50 means (denominator 49).
Error bars use the normal approximation `variance * sqrt(2 / 49)`. The plug-in
estimate divides each unbiased output variance by that replicate's returned
count, then averages across replicates.

Use `--resume` to continue an interrupted run with identical configuration,
or `--plot-only` to regenerate summaries and plots without rerunning sampling.
Use `--output` for a separate experiment; existing measurements are never
silently overwritten.

## Recorded rerun

The committed measurements use Lean 4.34.1 on an Apple M4 Max with 48 GiB RAM.
The sampling harness was compiled against the existing build of the unchanged
runtime and frontend, without rebuilding the proof library. From that build's
`lean` directory, the commands were:

```sh
lake env lean -R "$TASK_WORKTREE/lean" -c /tmp/SamplingBench.c \
  "$TASK_WORKTREE/lean/SamplingBench.lean"
python3 - <<'PY'
from pathlib import Path
arguments = Path('.lake/build/bin/determinize.rsp').read_text().splitlines()
assert arguments[0].endswith('/Main.c.o.export"')
Path('/tmp/SamplingBench.rsp').write_text(
    '\n'.join(['"/tmp/SamplingBench.c"'] + arguments[1:]) + '\n')
PY
lake env leanc -O3 -DNDEBUG -o /tmp/sampling-bench @/tmp/SamplingBench.rsp
```

`TASK_WORKTREE` denotes the checkout containing these sources. The response
file links the same compiled dependencies as the main executable, replacing
only its entry point. The experiment was then run from that checkout with
`python3 tools/benchmark_estimator_variance.py --binary /tmp/sampling-bench`.
The executable and both harness sources are identified by SHA-256 hashes in
`experiment.json`.

After measurement, the Python runner was changed to permit reproduction from
an archive without Git metadata and to write LF-terminated CSV rows. Its
recorded hash identifies the runner used for measurement; these two changes
do not affect sampling, seeds, timing, or statistics.
