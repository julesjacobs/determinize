# Anglican expectation benchmarks

These `.det` programs follow Wood, van de Meent, and Mansinghka (2014),
[A New Approach to Probabilistic Programming Inference](https://probprog.github.io/anglican/assets/pdf/wood-aistats-2014.pdf),
Programs 5.1--5.4. Their queries have been changed to posterior-predictive
expectations. The three conversions below retain the supplied model parameters,
observations, and E/G annotations:

- `dp_mixture.det`: Program 5.2's uncollapsed CRP with concentration 1.72,
  variance `10 / gamma[G](1, 10)`, and component mean `gauss[G](0, variance)`.
  Counts and sampled component parameters are stored in a list and threaded
  through calls instead of using implicit memoization. An existing component
  reuses its parameters; a new component samples them once. The ten observations
  are unchanged. The query continues the CRP for `H = 60` predictive observations
  and sums them. `H` was previously undefined; it is now an editable binding.
- `branching.det`: Program 5.3's recursive Fibonacci function, Poisson(4) prior,
  branch-specific Poisson(4) draw, and Poisson observation of 6. Tests `n < 1`
  and `n < 2` implement its two base cases on nonnegative integer arguments.
  The query returns a Poisson replicate, as in the supplied expectation variant.
- `marsaglia.det`: Program 5.4's recursive polar rejection sampler, parameters
  `(1, 5)`, and observations 9 and 8 with variance 2. The test `s < 1` and
  formula are retained verbatim in mathematical meaning, including the paper's
  omission of an explicit `s > 0` guard. The query returns a Normal replicate.

`gauss` and `observe_gauss` take a **variance** as their second distribution
parameter; this backend's `gamma` takes shape and rate. The transformations
preserve the parameter expressions already in these `.det` files.

Run from the repository root (requires `tools/pyro-requirements.txt`):

```sh
./run_pyro.sh experiments/pyro_models/dp_mixture.det --algorithm importance --particles 100 --runs 3
./run_pyro.sh experiments/pyro_models/branching.det --algorithm importance --particles 100 --runs 3
./run_pyro.sh experiments/pyro_models/marsaglia.det --algorithm importance --particles 100 --runs 3
```

By default each command runs both source and determinized models. All conditioning
uses retained G values. Only E predictive samples are replaced by their means.
Finite-sample estimates can differ; these commands are execution examples, not
convergence guarantees or reproductions of the paper's PMCMC experiments.

The Pyro frontend supports native `let`, `fun x =>`, `rec f x =>`, curried
application, pairs with `fst`/`snd`, unit, lists, and recursive list matching.
`observe_gauss`, `observe_poisson`, `sqrt`, and `log` are Pyro-backend extensions;
these files are not currently executable by the Lean CLI or covered by its
kernel-checked determinization theorem.

HMM continues to use the existing static lowering and vectorized SMC backend.
The other three programs execute scalar recursive traces at runtime, preserving
sample-dependent branches, cluster-list growth, and rejection loops. They support
importance sampling; vectorized SMC explicitly rejects them. Dynamic execution
uses the host Python recursion stack and does not truncate the model's latent
support or replace Marsaglia with a built-in Normal sampler. Dynamic eliminated
sample counts are reported as `n/a`, rather than mistaking a syntactic site count
for the number of random variables in a trace.

Python export also supports both subjects:

```sh
python3 tools/det_to_pyro.py experiments/pyro_models/marsaglia.det --subject source -o /tmp/marsaglia.py
```

Exports for dynamic programs are entry points importing this checkout's runtime;
they are not standalone deployments. Static exports retain their existing format.

Validation:

```sh
python3 -m unittest discover -s tests -p 'test_anglican_models.py' -v
python3 -m unittest discover -s tests -p 'test_pyro_backend.py' -v
```
