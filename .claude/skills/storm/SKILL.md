---
name: storm
description: Generate exact finite-model and expected-reward certificates in Lean and compare them with Storm using pinned stormpy.
paths:
  - "lean/Determinize/Finite/**"
  - "lean/Determinize/Checking/Result.lean"
  - "tools/storm.py"
  - "run.sh"
---

## Run

Install `tools/storm-requirements.txt` in a separate Python environment, then:

```sh
STORM_PYTHON=/path/to/python ./run.sh --storm FILE.det --prefix /tmp/model --subject source
```

`run.sh` builds the Lean CLI. The wrapper generates `.result.lean`, independently
checks it with Lean's kernel, loads rational explicit data through Storm's exact
sparse-matrix API, and requires exact agreement. `.storm.json` records both
versions, the engine/property/commands, status, and failures/timeouts.

## Meaning and limits

- Default subject: determinized. A source claim requires a source model or the
  determinization theorem's additional premises.
- Query: expected terminal reward, unnormalized; rejection contributes zero.
- Terminals pay once before a fresh `done` sink. Signed rewards use separate
  positive/negative files and `R=? [ F "done" ]`.
- Numeric terminals only; encode Boolean rewards explicitly with `if b then 1 else 0`.
- Retained stochastic Bernoulli/discrete draws are supported. Residual continuous
  and Poisson draws fail; their determinized means are exact rational operations.
- Incomplete exploration fails. Result certification requires a uniform absorption
  bound; a valid divergent model need not admit a result certificate.
- Dense solving defaults to 256 states (`--max-result-states`). Each wrapper
  subprocess defaults to a 120-second timeout (`--timeout`).
- Storm's default explicit parser rejects fractional literals. Keep the exact
  matrix adapter; do not convert certificates to rounded decimal data.
- Storm and serialization are unverified. Lean checks candidate model/result
  evidence against the core program; see `lean/finite-model-contract.md`.

For certificate generation without Storm use `./run.sh --result PREFIX FILE.det`.
