// Contraction of finite models to their branching, returning, rejecting and diverging states keeps
// every exact statistic: each complete model of test/fixtures/lean-exact.json, of both subjects in
// both modes, is contracted, turned back into a model and solved, and must give the Lean CLI's
// fractions, or, where the model has more states than the solver takes, the port's own solve of the
// whole model. Every branching node of those chains has the draw it branches at, and no node of a
// determinized program draws an E site, since determinization replaced those by their means. The
// unit tests check the chain's shape on small models.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import type { Chain } from "../src/core/finite/chain.ts";
import { contract } from "../src/core/finite/chain.ts";
import { explore, subjectProgram } from "../src/core/finite/explore.ts";
import type { Draw } from "../src/core/finite/machine.ts";
import type { Model, StateKind } from "../src/core/finite/model.ts";
import type { Rational } from "../src/core/finite/rational.ts";
import { fraction, isZero, one, rational, zero } from "../src/core/finite/rational.ts";
import type { RewardEdge, RewardModel } from "../src/core/finite/reward.ts";
import { exploreAdditive, solveRewardStatistics } from "../src/core/finite/reward.ts";
import type { Solved } from "../src/core/finite/statistics.ts";
import { resultFields, solveStatistics } from "../src/core/finite/statistics.ts";
import type { ExactFixture, ExactOutcome, ExactSubject } from "./lean-exact.ts";

const root = new URL("../../", import.meta.url);
const fixture: ExactFixture = JSON.parse(
  readFileSync(new URL("fixtures/lean-exact.json", import.meta.url), "utf8"),
);

const q = (num: number, den = 1) => rational(BigInt(num), BigInt(den));

/** The model of `chain`: a state per node, and a new initial state where the entry adds a
 * reward. A returning or rejecting node loops to itself, as a terminal state does, and so does a
 * diverging one, as a transient state. */
function modelOf(chain: Chain, additive: boolean): Model | RewardModel {
  const offset = isZero(chain.entry) ? 0 : 1;
  const kind: StateKind[] = [];
  const edges: RewardEdge[][] = [];
  if (offset) {
    kind.push({ kind: "transient" });
    edges.push([{ target: 1, probability: one, reward: chain.entry }]);
  }
  for (const edge of chain.edges) {
    assert.equal(
      chain.nodes[edge.from].kind,
      "branch",
      "an edge leaves a node that doesn't branch",
    );
  }
  for (const [number, node] of chain.nodes.entries()) {
    const state = number + offset;
    const loop = [{ target: state, probability: one, reward: zero }];
    if (node.kind === "returned") kind.push({ kind: "returned", reward: node.value });
    else if (node.kind === "rejected") kind.push({ kind: "rejected" });
    else kind.push({ kind: "transient" });
    edges.push(
      node.kind === "branch"
        ? chain.edges
            .filter((edge) => edge.from === number)
            .map(({ to, probability, reward }) => ({ target: to + offset, probability, reward }))
        : loop,
    );
  }
  const size = kind.length;
  if (additive) return { size, kind, edges };
  assert.ok(isZero(chain.entry), "a plain model's chain adds no reward");
  return {
    size,
    kind,
    rows: edges.map((row) => {
      assert.ok(
        row.every(({ reward }) => isZero(reward)),
        "a plain model's chain adds no reward",
      );
      assert.equal(new Set(row.map(({ target }) => target)).size, row.length);
      return row
        .map(({ target, probability }) => ({ target, probability }))
        .sort((a, b) => a.target - b.target);
    }),
  };
}

function solve(model: Model | RewardModel, maxStates?: number): Solved {
  return "edges" in model
    ? solveRewardStatistics(model, maxStates)
    : solveStatistics(model, maxStates);
}

/** The statistics of a solve, without what depends on the number of states. */
function statistics(solved: Solved) {
  if (!solved.ok) return { message: solved.message };
  const { states: _, rank_bound: __, ...fields } = resultFields(solved.result);
  return fields;
}

/** The statistics in a fixture's result. */
function fixtureStatistics(outcome: ExactOutcome) {
  if (!("result" in outcome)) return null;
  const { result } = outcome;
  return {
    answer: result.answer,
    return_mass: result.return_mass,
    rejection_probability: result.rejection_probability,
    divergence_probability: result.divergence_probability,
    second_moment: result.second_moment,
    conditional_mean: result.conditional_mean,
    conditional_variance: result.conditional_variance,
  };
}

/** Whether the CLI explored the whole model: it has a result, or more states than it solves. */
function complete(outcome: ExactOutcome) {
  return "result" in outcome || outcome.message.startsWith("exact solver state limit exceeded");
}

function exploration(file: string, subject: ExactSubject, additive: boolean) {
  const analysis = analyze(readFileSync(new URL(file, root), "utf8"));
  if (!analysis.ok) throw new Error(`${file}: the simulator rejects a case Lean accepts`);
  const program = subjectProgram(analysis.program, subject);
  return additive
    ? exploreAdditive(program, analysis.program.source)
    : explore(program, analysis.program.source);
}

// Every outcome of the fixture is accounted for: a complete model is compared, and every other one
// is checked to be no complete model for the port either, so that none is left out unseen.
const outcomes = fixture.cases.length * 4;
let compared = 0;
let incomplete = 0;

for (const entry of fixture.cases) {
  for (const mode of ["plain", "additive"] as const) {
    for (const subject of ["source", "determinized"] as const) {
      const outcome = entry[mode][subject];
      if (!complete(outcome)) {
        test(`${entry.file}, ${subject}, ${mode}, has no complete model`, () => {
          assert.ok(!("result" in outcome));
          assert.match(outcome.message, /^(Exploration failed|Incomplete exploration)/);
          const explored = exploration(entry.file, subject, mode === "additive");
          assert.notEqual(explored.kind, "complete");
          incomplete += 1;
        });
        continue;
      }
      test(`the chain of ${entry.file}, ${subject}, ${mode}, has the model's statistics`, () => {
        const explored = exploration(entry.file, subject, mode === "additive");
        assert.equal(explored.kind, "complete");
        if (explored.kind !== "complete") return;
        const chain = contract(explored.model);
        assert.equal(chain.states, explored.model.size);
        const solved = statistics(solve(modelOf(chain, mode === "additive")));
        assert.deepEqual(
          solved,
          fixtureStatistics(outcome) ?? statistics(solve(explored.model, Number.POSITIVE_INFINITY)),
        );
        compared += 1;
      });
      test(`the chain of ${entry.file}, ${subject}, ${mode}, branches at draws`, () => {
        const explored = exploration(entry.file, subject, mode === "additive");
        assert.equal(explored.kind, "complete");
        if (explored.kind !== "complete") return;
        const { model, draws } = explored;
        for (const node of contract(model).nodes) {
          const draw: Draw | null = draws[node.state];
          if (node.kind !== "branch") continue;
          assert.ok(draw, `state ${node.state} branches without a draw`);
          // The draw's mode is its site's, and every site it may stand for has that mode.
          assert.ok(draw.sites.length > 0);
          for (const site of [draw.at, ...draw.sites]) {
            assert.ok("site" in site && site.site === draw.mode, `${site.kind} at ${site.from}`);
          }
          assert.ok(draw.mode === "G" || (draw.mode === "E" && subject === "source"));
        }
      });
    }
  }
}

test("the chains of every complete model of the fixture were compared", (t) => {
  t.diagnostic(`compared ${compared} complete models; ${incomplete} more have none`);
  assert.equal(compared + incomplete, outcomes);
  assert.ok(compared > 0 && incomplete > 0);
});

/** A model of `rows`, each a list of [target, probability], with the kinds of its terminal
 * states; terminal states loop to themselves. */
function plain(rows: [number, Rational][][], terminal: Record<number, StateKind>): Model {
  return {
    size: rows.length,
    kind: rows.map((_, state) => terminal[state] ?? { kind: "transient" }),
    rows: rows.map((row, state) =>
      terminal[state]
        ? [{ target: state, probability: one }]
        : row.map(([target, probability]) => ({ target, probability })),
    ),
  };
}

/** A reward model of `rows`, each a list of [target, probability, reward]. */
function additive(
  rows: [number, Rational, Rational][][],
  terminal: Record<number, StateKind>,
): RewardModel {
  return {
    size: rows.length,
    kind: rows.map((_, state) => terminal[state] ?? { kind: "transient" }),
    edges: rows.map((row, state) =>
      terminal[state]
        ? [{ target: state, probability: one, reward: zero }]
        : row.map(([target, probability, reward]) => ({ target, probability, reward })),
    ),
  };
}

/** The chain in a readable form: each node's kind (with its value) and each edge as
 * "from→to probability +reward". */
function shape(chain: Chain) {
  return {
    entry: fraction(chain.entry),
    nodes: chain.nodes.map((node) =>
      node.kind === "returned" ? `returned ${fraction(node.value)}` : node.kind,
    ),
    edges: chain.edges.map(
      (edge) =>
        `${edge.from}→${edge.to} ${fraction(edge.probability)}${isZero(edge.reward) ? "" : ` +${fraction(edge.reward)}`}`,
    ),
  };
}

const returns = (value: number): StateKind => ({ kind: "returned", reward: q(value) });

/** Checks that `chain` has the statistics of `model`. */
function keepsStatistics(model: Model | RewardModel, chain: Chain) {
  assert.deepEqual(statistics(solve(modelOf(chain, "edges" in model))), statistics(solve(model)));
}

test("a branching initial state is node 0, and steps lead to the returns", () => {
  const model = plain(
    [
      [
        [1, q(1, 3)],
        [2, q(2, 3)],
      ],
      [[3, one]],
      [[4, one]],
      [],
      [],
    ],
    { 3: returns(1), 4: returns(2) },
  );
  const chain = contract(model);
  assert.deepEqual(shape(chain), {
    entry: "0",
    nodes: ["branch", "returned 1", "returned 2"],
    edges: ["0→1 1/3", "0→2 2/3"],
  });
  assert.deepEqual(
    chain.nodes.map((node) => node.state),
    [0, 3, 4],
  );
  keepsStatistics(model, chain);
});

test("nodes are numbered breadth first, each node's edges in its state's order", () => {
  // 0 draws 1 or 4; 1 draws 2 or 3; breadth first, 4 comes before 2 and 3.
  const half = q(1, 2);
  const model = plain(
    [
      [
        [1, half],
        [4, half],
      ],
      [
        [2, half],
        [3, half],
      ],
      [],
      [],
      [],
    ],
    { 2: returns(2), 3: returns(3), 4: returns(4) },
  );
  const chain = contract(model);
  assert.deepEqual(shape(chain), {
    entry: "0",
    nodes: ["branch", "branch", "returned 4", "returned 2", "returned 3"],
    edges: ["0→1 1/2", "0→2 1/2", "1→3 1/2", "1→4 1/2"],
  });
  keepsStatistics(model, chain);
});

test("a branch whose outcome steps back to it loops to itself", () => {
  const model = plain(
    [
      [
        [1, q(1, 2)],
        [2, q(1, 2)],
      ],
      [[0, one]],
      [],
    ],
    { 2: returns(0) },
  );
  const chain = contract(model);
  assert.deepEqual(shape(chain), {
    entry: "0",
    nodes: ["branch", "returned 0"],
    edges: ["0→0 1/2", "0→1 1/2"],
  });
  keepsStatistics(model, chain);
});

test("a cycle of steps diverges, as one node however it is entered", () => {
  // State 0 flips to 1 or 3; 1 steps to 2, which steps into the cycle 3 → 4 → 3.
  const model = plain(
    [
      [
        [1, q(1, 2)],
        [3, q(1, 2)],
      ],
      [[2, one]],
      [[3, one]],
      [[4, one]],
      [[3, one]],
    ],
    {},
  );
  const chain = contract(model);
  assert.deepEqual(shape(chain), {
    entry: "0",
    nodes: ["branch", "diverges"],
    edges: ["0→1 1"],
  });
  keepsStatistics(model, chain);
  const loop = contract(plain([[[1, one]], [[1, one]]], {}));
  assert.deepEqual(shape(loop), { entry: "0", nodes: ["diverges"], edges: [] });
});

test("rejection is a node of its own", () => {
  const model = plain(
    [
      [
        [1, q(1, 4)],
        [2, q(3, 4)],
      ],
      [[3, one]],
      [],
      [],
    ],
    { 2: returns(5), 3: { kind: "rejected" } },
  );
  const chain = contract(model);
  assert.deepEqual(shape(chain), {
    entry: "0",
    nodes: ["branch", "rejected", "returned 5"],
    edges: ["0→1 1/4", "0→2 3/4"],
  });
  keepsStatistics(model, chain);
});

test("a program without a random choice is a single node", () => {
  const model = plain([[[1, one]], [[2, one]], []], { 2: returns(7) });
  const chain = contract(model);
  assert.deepEqual(shape(chain), { entry: "0", nodes: ["returned 7"], edges: [] });
  assert.equal(chain.states, 3);
  keepsStatistics(model, chain);
});

test("parallel edges merge in the plain mode, and the branch stays a node", () => {
  const model = plain(
    [
      [
        [1, q(1, 2)],
        [2, q(1, 2)],
      ],
      [[3, one]],
      [[3, one]],
      [],
    ],
    { 3: returns(4) },
  );
  const chain = contract(model);
  assert.deepEqual(shape(chain), {
    entry: "0",
    nodes: ["branch", "returned 4"],
    edges: ["0→1 1"],
  });
  keepsStatistics(model, chain);
});

test("in the additive mode, parallel edges stay apart where their rewards differ", () => {
  const model = additive(
    [
      [
        [1, q(1, 2), q(1)],
        [2, q(1, 4), q(5)],
        [3, q(1, 4), q(3)],
      ],
      [[4, one, zero]],
      [[4, one, zero]],
      [[4, one, q(2)]],
      [],
    ],
    { 4: returns(0) },
  );
  const chain = contract(model);
  assert.deepEqual(shape(chain), {
    entry: "0",
    nodes: ["branch", "returned 0"],
    edges: ["0→1 1/2 +1", "0→1 1/2 +5"],
  });
  keepsStatistics(model, chain);
});

test("the reward on the way to the first node is the entry's", () => {
  const model = additive(
    [
      [[1, one, q(3)]],
      [[2, one, q(1, 2)]],
      [
        [3, q(1, 2), zero],
        [4, q(1, 2), q(2)],
      ],
      [],
      [[3, one, q(1)]],
    ],
    { 3: returns(1) },
  );
  const chain = contract(model);
  assert.deepEqual(shape(chain), {
    entry: "7/2",
    nodes: ["branch", "returned 1"],
    edges: ["0→1 1/2", "0→1 1/2 +3"],
  });
  keepsStatistics(model, chain);
});
