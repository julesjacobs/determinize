# Paper evaluation

The current RQ1, RQ2, and RQ3 measurements were collected on a MacBook Pro with
an Apple M4 Max and 48 GiB RAM. Run the experiments sequentially, without a
concurrent build or another benchmark. The Lean toolchain is pinned in
`lean/lean-toolchain`; plot generation requires Python and matplotlib.

From the repository root:

```sh
(cd lean && lake build determinize sampling-bench)
python3 tools/bench.py --exact --det-only --no-build
python3 benchmarks/exact/scaling/run_scaling.py --repetitions 10 --no-build
python3 tools/bench.py --approx --det --samples 50000 --seed 0 --fuel 100000 \
  --timeout 180 --no-build
```

- RQ1's table uses `exact-determinization-only.json`. Each time covers parsing,
  elaboration, inference, and mean replacement, measured inside the executable.
- RQ1's scaling plots use `scaling/runs.csv` and `scaling/summary.csv`: ten
  measurements after one warm-up run for each program size.
- RQ1's exact results use [`exact-instances/`](exact-instances/README.md):
  explicit small instances, exact moments, native Lean checks for 14 of 15
  benchmarks, and the recorded state-limit failure for `dreckon`. These runs
  are separate from the frontend timings and do not establish success for
  every parameter value.
- RQ2 uses `infinite-after-det.json`: 50,000 attempted executions of each source
  and determinized program. Observation rejections are recorded separately;
  any failed execution invalidates the benchmark.
- RQ3 uses `estimator-variance/`, including individual trials and their seeds.
  Its runner is `tools/benchmark_estimator_variance.py`; see its command-line
  help for the complete experiment configuration.

`finite-after-det.json` and `approx-results.csv` are historical experiments;
they are not the source of the current evaluation's tables or timing plots.
