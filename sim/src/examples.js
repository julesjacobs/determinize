export const examples = [
  {
    name: "Uniform mean",
    source: "let a = uniform(0, 1) in\na + 2",
  },
  {
    name: "Gamma dependency",
    source: "let x = gamma(1, 2) in\nlet y = gamma(x, 8) in\ny + 1",
  },
  {
    name: "G multiplication",
    source: "uniform(0, 1) * uniform(1, 2) + uniform(2, 3) * 3",
  },
  {
    name: "Explicit E/G modes",
    source: "let u = uniform[E](0, 1) in\nlet b = beta[G](3, 2) in\nlet g = gamma[E](u, b) in\n2 * g + 1",
  },
  {
    name: "Symbolic affine samples",
    source: "let u = uniform[E](0, 1) in\nlet y = uniform[E](u, 2) in\n2 * u + y - 1",
  },
  {
    name: "Bad E-branching",
    source: "let x = uniform[E](0, 1) in\nlet y = uniform[G](0, 1) in\nif x < 0.5 then x + y else x - y",
  },
  {
    name: "Pairs",
    source: "let x = uniform(0, 1) in\nlet y = gauss(x, 1) in\n(fst (x, y)) + snd (x, y)",
  },
  {
    name: "Sums",
    source: "match inl uniform(0, 1) with inl x => x + 1 | inr y => y + 2",
  },
  {
    name: "Lists",
    source: "match uniform(0, 1) :: [] with [] => 0 | x :: xs => x + 1",
  },
  {
    name: "Observe",
    source: "let x = uniform[G](0, 1) in\nlet _ = observe(x < 0.8) in\nx",
  },
  {
    name: "Recursive function",
    source: "let f = rec f n =>\n  if n <= 0 then gamma(1, 2) else gamma(f (n - 1), 2)\nin\nf 4",
  },
];
