// A port of Lean's `Runtime/Eval.lean`: the big-step evaluator that Lean's CLI samples with. It
// evaluates a program over doubles with closures, one unit of fuel per evaluated subexpression,
// and draws from two SplitMix64 streams, one for E draws and one for G draws. Lean's evaluator is
// recursive; this one keeps its continuation on an explicit stack, so that a long loop can't
// overflow JavaScript's call stack, and otherwise takes the same steps in the same order.
import type { Action, Program } from "../compiler/core.ts";
import { toNumber } from "../compiler/rational.ts";
import { makeStreams } from "./rng.ts";
import type { Op } from "./sampling.ts";
import { SamplingError, sample } from "./sampling.ts";

/** Lean's default fuel, the evaluator's limit on evaluated subexpressions. */
export const defaultFuel = 100000;

/** A program prepared for evaluation: literals as doubles, sites with their op and index. */
export type Node =
  | { t: "bvar"; index: number }
  | { t: "reject" | "unit" | "nil" }
  | { t: "bool"; value: boolean }
  | { t: "real"; value: number }
  | { t: "lam" | "fix" | "fst" | "snd" | "inl" | "inr" | "neg"; a: Node }
  | { t: "app" | "pair" | "cons" | "letE" | "add" | "mul" | "div" | "lt"; a: Node; b: Node }
  | { t: "matchSum" | "matchList" | "ite"; a: Node; b: Node; c: Node }
  /** `index` is the site's position in Lean's `Expr.sites`. */
  | { t: "site"; op: Op; action: Action; index: number; args: Node[] };

/** A runtime value, as Lean's `Value`. */
export type Value =
  | { tag: "unit" | "nil" }
  | { tag: "bool"; value: boolean }
  | { tag: "number"; value: number }
  | { tag: "pair"; a: Value; b: Value }
  | { tag: "inl" | "inr"; value: Value }
  | { tag: "cons"; head: Value; tail: Value }
  | { tag: "closure" | "recursive"; body: Node; env: Env };

/** An environment, innermost binding first, as Lean's `List Value`. */
export type Env = { head: Value; tail: Env } | null;

/** A G draw: the site's index, its op and the value drawn. */
export interface GDraw {
  site: number;
  op: Op;
  value: number;
}

/** What a run did, as Lean's `runOutcome`: a failure is its `Except` error. `draws` counts the
 * draws of either mode. */
export type Outcome =
  | { kind: "returned"; value: Value; draws: number }
  | { kind: "rejected" }
  | { kind: "failed"; message: string };

const siteOps: Record<string, Op> = {
  uniform: "uniform",
  gaussian: "gaussian",
  beta: "beta",
  gamma: "gamma",
  poisson: "poisson",
  discrete: "discrete",
  bernoulli: "bernoulli",
  exponential: "exponential",
};

/** `program` prepared for `run`; literals become `Number(num) / Number(den)`, as Lean's
 * `Float.ofInt q.num / Float.ofNat q.den`. */
export function prepare(program: Program): Node {
  let index = 0;
  const go = (e: Program): Node => {
    switch (e.kind) {
      case "bvar":
        return { t: "bvar", index: e.index };
      case "reject":
      case "unit":
      case "nil":
        return { t: e.kind };
      case "bool":
        return { t: "bool", value: e.value };
      case "real":
        return { t: "real", value: toNumber(e.value) };
      case "lam":
      case "fix":
      case "fst":
      case "snd":
      case "inl":
      case "inr":
      case "neg":
        return { t: e.kind, a: go(e.a) };
      case "app":
      case "pair":
      case "cons":
      case "letE":
      case "add":
      case "mul":
      case "div":
      case "lt":
        return { t: e.kind, a: go(e.a), b: go(e.b) };
      case "matchSum":
      case "matchList":
      case "ite":
        return { t: e.kind, a: go(e.a), b: go(e.b), c: go(e.c) };
      default: {
        // Lean's `Expr.sites` lists a site before the sites of its arguments.
        const site = { t: "site", op: siteOps[e.kind], action: e.site, index: index++ } as const;
        return { ...site, args: "b" in e ? [go(e.a), go(e.b)] : [go(e.a)] };
      }
    }
  };
  return go(program);
}

/** The sample sites of a prepared program, in the order of Lean's `Expr.sites`. */
export function siteNodes(node: Node, out: (Node & { t: "site" })[] = []) {
  if (node.t === "site") out.push(node);
  const children = node.t === "site" ? [...node.args] : [];
  if ("a" in node) children.push(node.a);
  if ("b" in node) children.push(node.b);
  if ("c" in node) children.push(node.c);
  for (const child of children) siteNodes(child, out);
  return out;
}

class Failure {
  declare message: string;
  constructor(message: string) {
    this.message = message;
  }
}

const unitValue: Value = { tag: "unit" };
const nilValue: Value = { tag: "nil" };

function number(v: Value): number {
  if (v.tag === "number") return v.value;
  throw new Failure("expected a numeric value");
}

function checkedNumber(x: number): Value {
  if (!Number.isFinite(x)) throw new Failure("nonfinite arithmetic result");
  return { tag: "number", value: x };
}

function numbers(v: Value): number[] {
  const out: number[] = [];
  let rest = v;
  while (rest.tag === "cons") {
    out.push(number(rest.head));
    rest = rest.tail;
  }
  if (rest.tag !== "nil") throw new Failure("expected a list of probabilities");
  return out;
}

/** What the evaluator still has to do with a subexpression's value, as the recursion of Lean's
 * `eval` would. */
type Frame =
  | { f: "argument"; env: Env; e: Node }
  | { f: "apply"; fn: Value }
  | { f: "second"; t: "pair" | "cons"; env: Env; e: Node }
  | { f: "pair" | "cons"; first: Value }
  | { f: "fst" | "snd" | "inl" | "inr" | "neg" }
  | { f: "matchSum" | "matchList" | "ite"; env: Env; b: Node; c: Node }
  | { f: "letE"; env: Env; body: Node }
  | { f: "right"; t: "add" | "mul" | "div" | "lt"; env: Env; e: Node }
  | { f: "operator"; t: "add" | "mul" | "div" | "lt"; left: number }
  | { f: "parameter"; site: Node & { t: "site" }; env: Env }
  | { f: "draw"; site: Node & { t: "site" }; first: number | null };

export interface RunOptions {
  fuel?: number;
  /** Receives every G draw, in execution order. */
  onGDraw?: (draw: GDraw) => void;
}

/**
 * Lean's `runOutcome`: runs `program` at `seed`, a UInt64, with the E stream seeded by
 * `seed xor 0x517cc1b727220a95` and the G stream by `seed`.
 */
export function run(program: Node, seed: bigint, options: RunOptions = {}): Outcome {
  let fuel = options.fuel ?? defaultFuel;
  const { rngE: eStream, rngG: gStream } = makeStreams(seed);
  let draws = 0;
  const stack: Frame[] = [];
  let env: Env = null;
  let e: Node | null = program;
  let value: Value = unitValue;

  function draw(site: Node & { t: "site" }, args: number[]): Value {
    try {
      if (site.action === "mean") {
        return { tag: "number", value: sample(site.op, true, args, eStream) };
      }
      const drawn = sample(site.op, false, args, site.action === "G" ? gStream : eStream);
      draws += 1;
      if (site.action === "G") options.onGDraw?.({ site: site.index, op: site.op, value: drawn });
      return { tag: "number", value: drawn };
    } catch (error) {
      if (error instanceof SamplingError) throw new Failure(error.message);
      throw error;
    }
  }

  try {
    for (;;) {
      if (e !== null) {
        if (fuel === 0) throw new Failure("step limit reached");
        fuel -= 1;
        const node: Node = e;
        e = null;
        switch (node.t) {
          case "bvar": {
            let rest = env;
            for (let i = 0; i < node.index && rest; i++) rest = rest.tail;
            if (!rest) throw new Failure("unbound runtime variable");
            value = rest.head;
            break;
          }
          case "reject":
            return { kind: "rejected" };
          case "unit":
            value = unitValue;
            break;
          case "nil":
            value = nilValue;
            break;
          case "bool":
            value = { tag: "bool", value: node.value };
            break;
          case "real":
            value = checkedNumber(node.value);
            break;
          case "lam":
            value = { tag: "closure", body: node.a, env };
            break;
          case "fix":
            value = { tag: "recursive", body: node.a, env };
            break;
          case "app":
            stack.push({ f: "argument", env, e: node.b });
            e = node.a;
            break;
          case "pair":
          case "cons":
            stack.push({ f: "second", t: node.t, env, e: node.b });
            e = node.a;
            break;
          case "fst":
          case "snd":
          case "inl":
          case "inr":
          case "neg":
            stack.push({ f: node.t });
            e = node.a;
            break;
          case "matchSum":
          case "matchList":
          case "ite":
            stack.push({ f: node.t, env, b: node.b, c: node.c });
            e = node.a;
            break;
          case "letE":
            stack.push({ f: "letE", env, body: node.b });
            e = node.a;
            break;
          case "add":
          case "mul":
          case "div":
          case "lt":
            stack.push({ f: "right", t: node.t, env, e: node.b });
            e = node.a;
            break;
          case "site":
            stack.push(
              node.args.length === 2
                ? { f: "parameter", site: node, env }
                : { f: "draw", site: node, first: null },
            );
            e = node.args[0];
            break;
        }
        continue;
      }
      const frame = stack.pop();
      if (!frame) return { kind: "returned", value, draws };
      switch (frame.f) {
        case "argument":
          stack.push({ f: "apply", fn: value });
          env = frame.env;
          e = frame.e;
          break;
        case "apply": {
          const fn = frame.fn;
          if (fn.tag === "closure") env = { head: value, tail: fn.env };
          else if (fn.tag === "recursive") env = { head: value, tail: { head: fn, tail: fn.env } };
          else throw new Failure("application of nonfunction");
          e = fn.body;
          break;
        }
        case "second":
          stack.push({ f: frame.t, first: value });
          env = frame.env;
          e = frame.e;
          break;
        case "pair":
          value = { tag: "pair", a: frame.first, b: value };
          break;
        case "cons":
          value = { tag: "cons", head: frame.first, tail: value };
          break;
        case "fst":
        case "snd":
          if (value.tag !== "pair") throw new Failure(`${frame.f} of nonpair`);
          value = frame.f === "fst" ? value.a : value.b;
          break;
        case "inl":
        case "inr":
          value = { tag: frame.f, value };
          break;
        case "neg":
          value = checkedNumber(-number(value));
          break;
        case "matchSum":
          if (value.tag === "inl") e = frame.b;
          else if (value.tag === "inr") e = frame.c;
          else throw new Failure("sum match of nonsum");
          env = { head: value.value, tail: frame.env };
          break;
        case "matchList":
          if (value.tag === "nil") {
            env = frame.env;
            e = frame.b;
          } else if (value.tag === "cons") {
            env = { head: value.head, tail: { head: value.tail, tail: frame.env } };
            e = frame.c;
          } else {
            throw new Failure("list match of nonlist");
          }
          break;
        case "ite":
          if (value.tag !== "bool") throw new Failure("expected a Boolean value");
          env = frame.env;
          e = value.value ? frame.b : frame.c;
          break;
        case "letE":
          env = { head: value, tail: frame.env };
          e = frame.body;
          break;
        case "right":
          stack.push({ f: "operator", t: frame.t, left: number(value) });
          env = frame.env;
          e = frame.e;
          break;
        case "operator": {
          const a = frame.left;
          const b = number(value);
          if (frame.t === "add") value = checkedNumber(a + b);
          else if (frame.t === "mul") value = checkedNumber(a * b);
          else if (frame.t === "div") {
            if (b === 0) throw new Failure("division by zero");
            value = checkedNumber(a / b);
          } else value = { tag: "bool", value: a < b };
          break;
        }
        case "parameter":
          stack.push({ f: "draw", site: frame.site, first: number(value) });
          env = frame.env;
          e = frame.site.args[1];
          break;
        case "draw": {
          const { site, first } = frame;
          if (first !== null) value = draw(site, [first, number(value)]);
          else if (site.op === "discrete") value = draw(site, numbers(value));
          else value = draw(site, [number(value)]);
          break;
        }
      }
    }
  } catch (error) {
    if (error instanceof Failure) return { kind: "failed", message: error.message };
    throw error;
  }
}

/** A double as Lean's `toString` prints it: C's `%f`, six decimals rounded half to even. */
export function displayFloat(x: number): string {
  if (Number.isNaN(x)) return "NaN";
  if (!Number.isFinite(x)) return x > 0 ? "inf" : "-inf";
  const negative = x < 0 || Object.is(x, -0);
  // |x| = mantissa · 2^exponent exactly.
  const view = new DataView(new ArrayBuffer(8));
  view.setFloat64(0, Math.abs(x));
  const bits = view.getBigUint64(0);
  const biased = Number(bits >> 52n);
  const fraction = bits & ((1n << 52n) - 1n);
  const mantissa = biased === 0 ? fraction : fraction | (1n << 52n);
  const exponent = (biased === 0 ? 1 : biased) - 1075;
  const scaled = mantissa * 1000000n;
  let millionths: bigint;
  if (exponent >= 0) {
    millionths = scaled << BigInt(exponent);
  } else {
    const divisor = 1n << BigInt(-exponent);
    millionths = scaled / divisor;
    const twice = 2n * (scaled % divisor);
    if (twice > divisor || (twice === divisor && millionths % 2n === 1n)) millionths += 1n;
  }
  const digits = millionths.toString().padStart(7, "0");
  return `${negative ? "-" : ""}${digits.slice(0, -6)}.${digits.slice(-6)}`;
}

/** A value as Lean's `Value.display` prints it. */
export function display(v: Value): string {
  switch (v.tag) {
    case "unit":
      return "()";
    case "nil":
      return "[]";
    case "bool":
      return String(v.value);
    case "number":
      return displayFloat(v.value);
    case "pair":
      return `(${display(v.a)}, ${display(v.b)})`;
    case "inl":
    case "inr":
      return `${v.tag} (${display(v.value)})`;
    case "cons":
      return `${display(v.head)} :: ${display(v.tail)}`;
    case "closure":
    case "recursive":
      return "<function>";
  }
}
