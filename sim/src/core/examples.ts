// The simulator's examples: programs from examples/, which Lean's corpus manifest
// (tests/cases.toml) also checks, in the order the simulator's gallery lists them. The first four
// are the paper's introduction in its order, and the paper's own counterexample follows its source
// program.

import geometricAddition from "../../../examples/loops/geometric-addition.det";
import dungeon from "../../../examples/paper/dungeon.det";
import gaussRandomWalk from "../../../examples/paper/gauss-random-walk.det";
import noisyIteration from "../../../examples/paper/noisy-iteration.det";
import noisyProduct from "../../../examples/paper/noisy-product.det";
import badEBranching from "../../../examples/simulator/bad-e-branching.det";
import noisyProductAllE from "../../../examples/simulator/noisy-product-all-e.det";
import observe from "../../../examples/simulator/observe.det";
import randomListSum from "../../../examples/simulator/random-list-sum.det";
import recursiveGamma from "../../../examples/simulator/recursive-gamma.det";

export interface Example {
  /** The file's path under examples/, without `.det`. */
  id: string;
  title: string;
  /** One sentence on what the example shows. */
  explanation: string;
  /** Whether the paper presents the program. */
  fromPaper: boolean;
  /** The program, without the file's final line break. */
  source: string;
}

export const examples: Example[] = [
  {
    id: "paper/noisy-product",
    title: "Noisy product",
    explanation: "A signal and a noisy measurement of it; the measurement becomes its mean.",
    fromPaper: true,
    source: noisyProduct.trimEnd(),
  },
  {
    id: "simulator/noisy-product-all-e",
    title: "Noisy product, both draws E",
    explanation:
      "Lean rejects this program; replacing both draws anyway returns 1/4 instead of 1/3.",
    fromPaper: true,
    source: noisyProductAllE.trimEnd(),
  },
  {
    id: "paper/gauss-random-walk",
    title: "Gaussian random walk",
    explanation:
      "A step function passed to reduce; every position becomes its mean, so the path stays at zero.",
    fromPaper: true,
    source: gaussRandomWalk.trimEnd(),
  },
  {
    id: "paper/dungeon",
    title: "Dungeon crawl",
    explanation:
      "Exit coins stay random and decide the recursion; jackpots become their mean payouts.",
    fromPaper: true,
    source: dungeon.trimEnd(),
  },
  {
    id: "paper/noisy-iteration",
    title: "Noisy iteration",
    explanation: "Without the Gaussian noise, the loop only visits x = 0 and x = 1.",
    fromPaper: true,
    source: noisyIteration.trimEnd(),
  },
  {
    id: "simulator/observe",
    title: "Observe",
    explanation:
      "Runs that fail an observation are rejected; statistics are over the returned runs.",
    fromPaper: false,
    source: observe.trimEnd(),
  },
  {
    id: "simulator/bad-e-branching",
    title: "Bad E-branching",
    explanation: "An E draw decides a branch, and Lean rejects the program.",
    fromPaper: false,
    source: badEBranching.trimEnd(),
  },
  {
    id: "simulator/recursive-gamma",
    title: "Recursive gamma",
    explanation: "Recursion through a distribution's parameter.",
    fromPaper: false,
    source: recursiveGamma.trimEnd(),
  },
  {
    id: "simulator/random-list-sum",
    title: "Random list sum",
    explanation: "A list of random length whose elements become their means.",
    fromPaper: false,
    source: randomListSum.trimEnd(),
  },
  {
    id: "loops/geometric-addition",
    title: "Geometric addition",
    explanation: "A loop that runs a random number of times.",
    fromPaper: false,
    source: geometricAddition.trimEnd(),
  },
];
