# Exact analysis of explicit RQ1 instances

These runs cover one explicit instance of each of the 15 RQ1 benchmarks at
commit `23d7d5f710aa38a92ec0246e4ee2f5dce9f6351e`. Fourteen completed and passed
the `by storm` check. `dreckon` reached the 1,000-state exploration limit
(1,001 states discovered); it has no exact result or checked theorem here.
The runs establish results for these instances, not for every parameter value.

For function-valued benchmarks, the inputs are `aggregate-rv 2`, `invpend 2`,
`pack 2`, `service_policy 2`, `swap 1`, and `workload 2`. The other inputs are
the original closed programs. The instance files retain the source program
and append its argument; the tool then performs determinization. Packaged copies
strip trailing whitespace and use exactly one final newline. The manifest retains
both the original diagnostic input hashes and the normalized copy hashes.

`manifest.json` records all outcomes, source and instance SHA-256 hashes,
parameter choices, state counts, exact return masses and first and second
moments, tool versions, limits, and checker output. Every successful instance
has return mass one. The first and second moments describe the **determinized**
program. Expectation preservation for the source additionally requires the
soundness theorem's hypotheses; source and determinized second moments can differ.

`instances/` contains the inputs. `solutions/` preserves Storm's exact rational
vectors. `theorems/` contains compact Lean theorem inputs: compiling one invokes
Storm again and checks its solution against the program semantics. These files
use `native_decide`, whose trust includes the Lean compiler and runtime as well
as the kernel. They are not kernel-only portable certificates. The recorded
axiom reports include the corresponding `native_decide` axiom and no `sorryAx`.

The export executable used for the initial solve preceded changes to the
frontend executable's entry point and export code; its hash is recorded.
Each successful theorem independently reconstructs and checks the model using
the current Lean imports. `CheckFrontend.lean` also recompiles all 14 successful
instances with the current frontend and compares each Core expression with
the literal in the theorem. That comparison passed. Recorded timing data is
omitted because these were diagnostic runs, not timing experiments.

## Check the saved artifact

From the repository root:

```sh
python3 results/exact-instances/verify.py
```

This checks the hashes, parameter applications, theorem statistics, recorded
outcomes, and agreement between the literals used by the frontend comparison
and the theorem inputs. It does not rerun Lean or Storm.

## Reproduce the checks

Use the pinned Lean toolchain and a Python environment with `stormpy==1.14.0`.
Build the project once with `cd lean && lake build`; run subsequent commands
sequentially. From `lean/`, set `STORM_PYTHON` to that Python executable and run:

```sh
lake lean ../results/exact-instances/CheckFrontend.lean
for theorem in ../results/exact-instances/theorems/*.native.lean; do
  lake lean "$theorem" || exit 1
done
```

The theorem inputs fix a 1,000-state limit and a 15-second Storm subprocess
timeout. The recorded checks also had a 30-second outer timeout per theorem.
Storm is rerun here; the saved solution vectors document the original solve.

To reproduce the initial model export and solve for any instance, run from
the repository root, using a fresh output directory:

```sh
python3 tools/storm.py results/exact-instances/instances/dreckon.det \
  --prefix /tmp/exact-instances-replay/dreckon \
  --skip-certificate --max-states 1000 --timeout 15
```

Use the Python environment containing Storm. For `dreckon`, a nonzero exit
reporting the exploration limit is the recorded outcome. For a successful
instance, this command alone produces an unchecked solution; the corresponding
Lean theorem command above performs the native check. The original solve
commands had a 35-second outer timeout. Generated output belongs in a fresh
directory, separate from this saved artifact.
