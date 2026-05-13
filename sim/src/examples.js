export const examples = [
  {
    name: "Uniform mean",
    source: "let a = uniform(0, 1) in\na + 2",
  },
  {
    name: "Dependent uniform",
    source: "let x = uniform(0, 1) in\nlet y = uniform(x, 2) in\nx + y",
  },
  {
    name: "Symbolic affine samples",
    source: "let u = uniform(0, 1) in\nlet v = uniform(u, 2) in\nu * 2 + v - 1",
  },
  {
    name: "Nonlinear use",
    source: "let x = uniform(0, 1) in\nx * x + uniform(0, 1)",
  },
  {
    name: "Mixed residual randomness",
    source: "let u = uniform(0, 1) in\nlet b = beta(9, 1) in\nlet g = gamma(u, b) in\ng * 2 + 1",
  },
  {
    name: "Pairs",
    source: "let x = uniform(0, 1) in\nlet p = (x, uniform(x, 2)) in\nfst p + snd p",
  },
  {
    name: "List sum",
    source: "let sum = rec sum xs =>\n  match xs with [] => 0 | x :: rest => x + sum rest\nin\nsum (uniform(0, 1) :: uniform(1, 2) :: gamma(1, 2) :: [])",
  },
  {
    name: "Random list sum",
    source: "let sum = rec sum xs =>\n  match xs with [] => 0 | x :: rest => x + sum rest\nin\nlet draw = rec draw _ =>\n  if flip(0.5) then [] else uniform(0, 1) :: draw 0\nin\nsum (draw 0)",
  },
  {
    name: "Observe",
    source: "let x = uniform(0, 1) in\nlet y = uniform(0, x) in\nlet _ = observe(x < 0.8) in\nx + y",
  },
  {
    name: "Bad E-branching",
    source: "let x = uniform[E](0, 1) in\nlet y = uniform[G](0, 1) in\nif x < 0.5 then x + y else x - y",
  },
  {
    name: "Recursive function",
    source: "let f = rec f n =>\n  if n <= 0 then 1 else gamma(f (n - 1), uniform(1, 2))\nin\nf 4",
  },
];
