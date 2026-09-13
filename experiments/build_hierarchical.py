"""Generate standalone .det estimators from audited paper data.

Generation uses Python only for fixed data preprocessing. Inference is in .det.
See hierarchical.md for source versions, query choices, and limitations.
"""
from pathlib import Path
import csv
import json

ROOT = Path(__file__).resolve().parent


def det_list(xs):
    return "(" + " :: ".join(str(x) for x in xs) + " :: [])" if xs else "[]"


COMMON = """let at = rec at xs => fun i =>
  match xs with [] => 0 | x :: rest =>
    if i < 1 then x else at rest (i - 1)
in
let sum = rec sum xs =>
  match xs with [] => 0 | x :: rest => x + sum rest
in
let map = fun f => rec go xs =>
  match xs with [] => [] | x :: rest => f x :: go rest
in
"""


def rats():
    # Table 3, Gelfand et al., Stanford technical report 421, p. 13.
    weights = [
        [151,199,246,283,320], [145,199,249,293,354],
        [147,214,263,312,328], [155,200,237,272,297],
        [135,188,230,280,323], [159,210,252,298,331],
        [141,189,231,275,305], [159,201,248,297,338],
        [177,236,285,340,376], [134,182,220,260,296],
        [160,208,261,313,352], [143,188,220,273,314],
        [154,200,244,289,325], [171,221,270,326,358],
        [163,216,242,281,312], [160,207,248,288,324],
        [142,187,234,280,316], [156,203,243,283,317],
        [157,212,259,307,336], [152,203,246,286,321],
        [154,205,253,298,334], [139,190,225,267,302],
        [146,191,229,272,302], [157,211,250,285,323],
        [132,185,237,286,331], [160,207,257,303,345],
        [169,216,261,295,333], [157,205,248,289,316],
        [137,180,219,258,291], [153,200,244,286,324],
    ]
    ages = [8, 15, 22, 29, 36]
    stats = []
    with (ROOT / "rats_data.csv").open("w") as f:
        writer = csv.writer(f)
        writer.writerow(["rat", "age", "weight", "observed"])
        for i, row in enumerate(weights, 1):
            n = 5 if i <= 5 else 4 if i <= 10 else 3 if i <= 20 else 2 if i <= 25 else 1
            x, y = ages[:n], row[:n]
            stats.append([n, sum(x), sum(a*a for a in x), sum(y),
                          sum(a*b for a,b in zip(x,y)), sum(b*b for b in y)])
            writer.writerows((i, age, value, int(j < n)) for j,(age,value) in enumerate(zip(ages,row)))
    assert sum(s[0] for s in stats) == 90
    header = """(* Gelfand, Hills, Racine-Poon and Smith (1990), section 6.
   Primary source: Stanford technical report 421 (1989), pp. 13-16, eqs. (5)-(6).
   https://www.leman.stat.vt.edu/VTCourses/Gelfand_Smith_rats.pdf
   CORRELATED random effects; uncentered ages, unlike the BUGS Rats variant.
   Flat population-mean prior C^-1=0; p(sigma^2) proportional to 1/sigma^2.
   Sigma^-1 ~ Wishart((rho R)^-1,rho), rho=2, R=diag(100,0.1).
   90 observed weights; remaining 60 excluded from all Gibbs sufficient stats.
   Query: mean of rat 26's missing weights at ages 15,22,29,36.
   50 Gibbs sweeps follow the report's missing-data iteration count;
   a finite chain is NOT an exact posterior sample. Initial values are ours.
   gauss takes VARIANCE; gamma takes SHAPE,RATE. Only final predictions are E.
   See hierarchical.md for the scalar Wishart derivation and validation. *)
"""
    code = header + COMMON.replace('let map =', 'let map_data =') + """let map_effects = fun f => rec go xs =>
  match xs with [] => [] | x :: rest => f x :: go rest
in
"""
    code += "let data =\n  " + " ::\n  ".join(det_list(s) for s in stats) + " :: []\nin\n"
    code += """(* Draw a bivariate normal with precision (p11,p12,p22)
   and natural parameter (h1,h2), without square roots. *)
let normal2 = fun p11 => fun p12 => fun p22 => fun h1 => fun h2 =>
  let determinant = p11 * p22 - p12 * p12 in
  let ma = (p22 * h1 - p12 * h2) / determinant in
  let mb = (p11 * h2 - p12 * h1) / determinant in
  let a = gauss[G](ma, p22 / determinant) in
  let b = gauss[G](mb - (p12 / p22) * (a - ma), 1 / p22) in
  (a, b)
in
(* State: ((population mean pair, precision entries), residual precision).
   Effects are drawn afresh conditional on this state in every sweep. *)
let step = fun state =>
  let mu = fst (fst state) in
  let precision = snd (fst state) in
  let l11 = at precision 0 in
  let l12 = at precision 1 in
  let l22 = at precision 2 in
  let tau = snd state in
  let effects = map (fun d => normal2
    (l11 + tau * at d 0) (l12 + tau * at d 1) (l22 + tau * at d 2)
    (l11 * fst mu + l12 * snd mu + tau * at d 3)
    (l12 * fst mu + l22 * snd mu + tau * at d 4)) data in
  let sa = sum (map (fun ab => fst ab) effects) in
  let sb = sum (map (fun ab => snd ab) effects) in
  let newmu = normal2 (30 * l11) (30 * l12) (30 * l22)
    (l11 * sa + l12 * sb) (l12 * sa + l22 * sb) in
  let s11 = 200 + sum (map (fun ab =>
    let r = fst ab - fst newmu in r * r) effects) in
  let s12 = sum (map (fun ab =>
    (fst ab - fst newmu) * (snd ab - snd newmu)) effects) in
  let s22 = 0.2 + sum (map (fun ab =>
    let r = snd ab - snd newmu in r * r) effects) in
  (* Wishart(S^-1,32), via conditional column decomposition. *)
  let v11 = s22 / (s11 * s22 - s12 * s12) in
  let regression = -s12 / s22 in
  let schur = 1 / s22 in
  let w11 = gamma[G](16, 1 / (2 * v11)) in
  let w12 = gauss[G](regression * w11, schur * w11) in
  let w22 = w12 * w12 / w11 + gamma[G](15.5, 1 / (2 * schur)) in
  let residual_sum = rec go ds => fun es =>
    match ds with [] => 0 | d :: dr =>
      match es with [] => 0 | ab :: er =>
        let a = fst ab in let b = snd ab in
        at d 5 - 2 * a * at d 3 - 2 * b * at d 4
          + a * a * at d 0 + 2 * a * b * at d 1
          + b * b * at d 2 + go dr er
  in
  let newtau = gamma[G](45, (residual_sum data effects) / 2) in
  ((newmu, w11 :: w12 :: w22 :: []), newtau)
in
let run = rec run n => fun state =>
  if n < 1 then state else run (n - 1) (step state)
in
let sweeps = 50 in
let state = run sweeps (((100, 6), 0.01 :: 0 :: 10 :: []), 0.01) in
let mu = fst (fst state) in
let precision = snd (fst state) in
let tau = snd state in
let l11 = at precision 0 in
let l12 = at precision 1 in
let l22 = at precision 2 in
(* Rat 26 has just y=160 at age 8. Reconstruct its conditional effects. *)
let ab = normal2 (l11 + tau) (l12 + 8 * tau) (l22 + 64 * tau)
  (l11 * fst mu + l12 * snd mu + 160 * tau)
  (l12 * fst mu + l22 * snd mu + 1280 * tau)
in
let a = fst ab in let b = snd ab in
let y15 = gauss[E](a + 15 * b, 1 / tau) in
let y22 = gauss[E](a + 22 * b, 1 / tau) in
let y29 = gauss[E](a + 29 * b, 1 / tau) in
let y36 = gauss[E](a + 36 * b, 1 / tau) in
(y15 + y22 + y29 + y36) / 4
"""
    code = code.replace('map (fun d', 'map_data (fun d').replace('map (fun ab', 'map_effects (fun ab')
    (ROOT / "rats.det").write_text(code)


def radon():
    source = json.loads((ROOT / "radon_data.json").read_text())
    assert len(source['county']) == 919 and len(source['uranium']) == 85
    groups = []
    for county, uranium in enumerate(source['uranium']):
        xy = [(x,y) for c,x,y in zip(source['county'],source['floor'],source['log_radon']) if c == county]
        groups.append([len(xy),sum(x for x,y in xy),sum(x*x for x,y in xy),
                       sum(y for x,y in xy),sum(x*y for x,y in xy),uranium])
    code = """(* Gorinova, Moore & Hoffman (ICML 2020), section 6.1, Radon.
   https://proceedings.mlr.press/v119/gorinova20a/gorinova20a.pdf
   Exact variant: authors' get_radon('MN'), NOT get_radon_model_stddvs.
   mu,a,b ~ N(0,1); m_c ~ N(mu+a*u_c,1); y_i ~ N(m_c+b*x_i,1).
   All second parameters above are standard deviations (here equal to 1).
   919 households, 85 counties. Fixed transformed data match the authors'
   load_radon_data, including float32 casts and log(activity+0.1).
   Data row: n,sum(x),sum(x*x),sum(y),sum(x*y),u; sufficient statistics.
   This file implements Gibbs inference, not the paper's HMC algorithm.
   Query: posterior mean of the average county intercept (equal county weights).
   Finite Gibbs runs have convergence error. Initial values/100 sweeps are ours.
   Final conditional county draws are E and do not feed the chain. *)
""" + COMMON
    code += 'let data =\n  ' + ' ::\n  '.join(det_list(s) for s in groups) + ' :: []\nin\n'
    code += """let county_means = fun globals =>
  let mu = at globals 0 in let a = at globals 1 in let b = at globals 2 in
  map (fun d => gauss[G]((mu + a * at d 5 + at d 3 - b * at d 1)
      / (1 + at d 0), 1 / (1 + at d 0))) data
in
let sum_with = fun f => rec go ds => fun ms =>
  match ds with [] => 0 | d :: dr =>
    match ms with [] => 0 | m :: mr => f d m + go dr mr
in
let step = fun globals =>
  let ms = county_means globals in
  let olda = at globals 1 in
  let mu = gauss[G]((sum ms - olda * sum (map (fun d => at d 5) data)) / 86, 1 / 86) in
  let aprec = 1 + sum (map (fun d => at d 5 * at d 5) data) in
  let a = gauss[G]((sum_with (fun d => fun m => at d 5 * (m - mu)) data ms) / aprec, 1 / aprec) in
  let bprec = 1 + sum (map (fun d => at d 2) data) in
  let b = gauss[G]((sum_with (fun d => fun m => at d 4 - m * at d 1) data ms) / bprec, 1 / bprec) in
  mu :: a :: b :: []
in
let run = rec run n => fun globals =>
  if n < 1 then globals else run (n - 1) (step globals)
in
let sweeps = 100 in
let globals = run sweeps (0 :: 0 :: 0 :: []) in
let mu = at globals 0 in let a = at globals 1 in let b = at globals 2 in
let finish = rec finish ds =>
  match ds with [] => 0 | d :: rest =>
    let m = gauss[E]((mu + a * at d 5 + at d 3 - b * at d 1)
      / (1 + at d 0), 1 / (1 + at d 0)) in
    m + finish rest
in
finish data / 85
"""
    (ROOT / "radon.det").write_text(code)


if __name__ == "__main__":
    rats()
    radon()
