// The simulator's examples: programs from examples/, which Lean's corpus manifest
// (tests/cases.toml) also checks, in the order the simulator lists them.

import dungeon from "../../../examples/paper/dungeon.det";
import gaussRandomWalk from "../../../examples/paper/gauss-random-walk.det";
import noisyIteration from "../../../examples/paper/noisy-iteration.det";
import noisyProduct from "../../../examples/paper/noisy-product.det";
import sumOfSquares from "../../../examples/paper/sum-of-squares.det";
import badEBranching from "../../../examples/simulator/bad-e-branching.det";
import noisyProductAllE from "../../../examples/simulator/noisy-product-all-e.det";
import observe from "../../../examples/simulator/observe.det";
import randomListSum from "../../../examples/simulator/random-list-sum.det";
import recursiveGamma from "../../../examples/simulator/recursive-gamma.det";
import mixedModes from "../../../examples/symbolic/mixed-affinities.det";
import nestedUniform from "../../../examples/symbolic/nested-uniform.det";

export interface Example {
  /** The file's path under examples/, without `.det`. */
  id: string;
  title: string;
  explanation: string;
  source: string;
}

export const examples: Example[] = [
  {
    id: "paper/noisy-product",
    title: "Noisy product",
    explanation:
      "The measurement y is replaced by its mean x, so x * y becomes x * x, with the same mean 1/3.",
    source: noisyProduct,
  },
  {
    id: "paper/gauss-random-walk",
    title: "Gaussian random walk",
    explanation:
      "Each position is drawn around the previous one inside a higher-order reduce; every draw is replaced by its mean.",
    source: gaussRandomWalk,
  },
  {
    id: "paper/dungeon",
    title: "Dungeon",
    explanation:
      "Each room is left with probability 1/4; the rare loot draw is replaced by its mean, the exits stay random.",
    source: dungeon,
  },
  {
    id: "paper/noisy-iteration",
    title: "Noisy iteration",
    explanation:
      "The Gaussian noise of each step is replaced by its mean 0; the coin flips that end the loop stay random.",
    source: noisyIteration,
  },
  {
    id: "simulator/noisy-product-all-e",
    title: "Noisy product, both draws E",
    explanation:
      "Lean rejects marking both draws E: replacing both by their means returns 1/4 instead of 1/3.",
    source: noisyProductAllE,
  },
  {
    id: "simulator/bad-e-branching",
    title: "Branching on an E draw",
    explanation:
      "Lean rejects branching on an [E] draw: its mean cannot decide which branch a run takes.",
    source: badEBranching,
  },
  {
    id: "simulator/observe",
    title: "Observe",
    explanation:
      "observe rejects the runs with x ≥ 0.8; x stays random because the condition compares it, y is replaced by its mean.",
    source: observe,
  },
  {
    id: "symbolic/nested-uniform",
    title: "Dependent draws",
    explanation:
      "The lower bound of the second draw is the first; both are replaced by their means.",
    source: nestedUniform,
  },
  {
    id: "symbolic/mixed-affinities",
    title: "A G draw and its dependent",
    explanation:
      "The [G] draw stays random; the draw whose lower bound it sets is replaced by its mean.",
    source: mixedModes,
  },
  {
    id: "paper/sum-of-squares",
    title: "Sum of squares",
    explanation: "x * x is not linear in x, so x stays random.",
    source: sumOfSquares,
  },
  {
    id: "simulator/random-list-sum",
    title: "Random list sum",
    explanation:
      "A recursive function builds a list of random length; its elements are replaced by their means, the flips that end it stay random.",
    source: randomListSum,
  },
  {
    id: "simulator/recursive-gamma",
    title: "Recursive gamma",
    explanation:
      "Each recursive call sets the shape of a gamma draw, which is replaced by its mean; its rate stays random.",
    source: recursiveGamma,
  },
];
