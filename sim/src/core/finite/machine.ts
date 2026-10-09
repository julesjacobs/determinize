// A port of Lean's `Finite/Machine.lean` and `Finite/Supported.lean`: the CEK machine over exact
// rationals whose reachable states make a program's finite model. One step of a state gives its
// successors with their probabilities, its reward when it returns, or Lean's failure. Only
// Bernoulli and finite discrete draws are sampled; every mean is taken, as a rational.
//
// Values, environments, frames and states are hash-consed: a `Machine` builds each structure once,
// so that structurally equal states, which Lean's exploration identifies, are the same object, and
// an id names each. A draw's frame also records the site it evaluates, for the panel's message;
// the site isn't part of the frame's structure, so of equal frames the first one's site is kept.
import type { Action, Program } from "../compiler/core.ts";
import type { Rational } from "./rational.ts";
import {
  add,
  div,
  fraction,
  integer,
  isZero,
  le,
  lt,
  mul,
  neg,
  one,
  sub,
  zero,
} from "./rational.ts";

/** A program node with its structure's id: Lean's `Core`, whose equality ignores the spans. */
export interface Code {
  id: number;
  /** The first node of the program with this structure. */
  node: Program;
  /** The node's subexpressions, in Lean's constructor order. */
  kids: Code[];
}

export type Value = { id: number } & (
  | { tag: "unit" | "nil" }
  | { tag: "bool"; value: boolean }
  | { tag: "number"; value: Rational }
  | { tag: "pair"; a: Value; b: Value }
  | { tag: "inl" | "inr"; value: Value }
  | { tag: "cons"; head: Value; tail: Value }
  | { tag: "closure" | "recursive"; body: Code; env: Env }
);

/** A list, first element first, as Lean's `List`. */
export type List<T> = { id: number; head: T; tail: List<T> } | null;
export type Env = List<Value>;
export type Stack = List<Frame>;

export type Unary = "fst" | "snd" | "inl" | "inr" | "neg";
export type Binary = "app" | "pair" | "cons" | "add" | "mul" | "div" | "lt";

/** A primitive distribution, as Lean's `Op`. */
export type Op =
  | "uniform"
  | "gaussian"
  | "poisson"
  | "exponential"
  | "beta"
  | "gamma"
  | "bernoulli"
  | { discrete: number };

export type Frame = { id: number } & (
  | { tag: "unary"; op: Unary }
  | { tag: "left"; op: Binary; right: Code; env: Env }
  | { tag: "right"; op: Binary; left: Value }
  | { tag: "choose"; yes: Code; no: Code; env: Env }
  | { tag: "letBody"; body: Code; env: Env }
  | { tag: "matchSum"; left: Code; right: Code; env: Env }
  | { tag: "matchList"; nilCase: Code; consCase: Code; env: Env }
  | { tag: "discrete"; action: Action; at: Program }
  | {
      tag: "draw";
      action: Action;
      op: Op;
      pending: Code[];
      env: Env;
      args: Rational[];
      at: Program;
    }
);

/** A frame without its id: what `Machine.makeFrame` builds a frame from. */
export type FrameData = Frame extends infer F ? (F extends unknown ? Omit<F, "id"> : never) : never;

export type State = { id: number } & (
  | { tag: "eval"; expr: Code; env: Env; stack: Stack }
  | { tag: "deliver"; value: Value; stack: Stack }
  | { tag: "rejected" }
);

/** Lean's `Failure`, and the draw it occurred at, where it occurred at one. */
export class Failure {
  readonly kind: "invalid" | "unsupported" | "nonnumericResult";
  readonly detail: string;
  readonly at: Program | null;
  /** The sites that the failing state may stand for, `at` among them, in syntax order. */
  readonly sites: Program[];

  constructor(
    kind: "invalid" | "unsupported" | "nonnumericResult",
    detail: string,
    at: Program | null = null,
    sites: Program[] = at ? [at] : [],
  ) {
    this.kind = kind;
    this.detail = detail;
    this.at = at;
    this.sites = sites;
  }

  /** As Lean's `Failure.message`. */
  get message(): string {
    if (this.kind === "invalid") return `invalid execution: ${this.detail}`;
    if (this.kind === "unsupported") return `unsupported execution: ${this.detail}`;
    return "export requires a numeric terminal result";
  }
}

/** One step's result, as Lean's `Step`: the successors with their probabilities, in Lean's order,
 * or the end of a run. */
export type Step =
  | { kind: "next"; successors: [Rational, State][] }
  | { kind: "returned"; reward: Rational }
  | { kind: "rejected" };

/** An op as Lean's derived `Repr` prints it at the top level, as in Lean's messages. */
export function opRepr(op: Op): string {
  return typeof op === "string"
    ? `Determinize.Spec.Paper.Op.${op}`
    : `Determinize.Spec.Paper.Op.discrete ${op.discrete}`;
}

/** As Lean's `supportedDraw`: every mean, and of the draws only Bernoulli and finite discrete. */
export function supportedDraw(action: Action, op: Op): boolean {
  return action === "mean" || op === "bernoulli" || typeof op !== "string";
}

/** Lean's `remainderDistribution`: the probabilities with the remainder to 1 appended. */
function remainderDistribution(probabilities: Rational[]): Rational[] {
  let total = zero;
  for (const p of probabilities) total = add(total, p);
  const all = [...probabilities, sub(one, total)];
  if (all.some((p) => lt(p, zero))) throw new Failure("invalid", "negative discrete weight");
  return all;
}

/** Lean's `finiteLaw`: the exact law of a draw or the value of a mean, from evaluated
 * parameters. */
export function finiteLaw(op: Op, action: Action, args: Rational[]): [Rational, Rational][] {
  if (typeof op !== "string") {
    if (args.length !== op.discrete) throw new Failure("invalid", "primitive arity");
    const probabilities = remainderDistribution(args);
    if (action === "mean") {
      let mean = zero;
      for (const [i, p] of probabilities.entries()) mean = add(mean, mul(p, integer(i)));
      return [[one, mean]];
    }
    return probabilities.map((p, i) => [p, integer(i)]);
  }
  const [a, b] = args;
  const fail = (what: string): never => {
    throw new Failure("invalid", what);
  };
  let mean: Rational;
  if (op === "uniform" && args.length === 2)
    mean = le(a, b) ? div(add(a, b), integer(2)) : fail("uniform bounds");
  else if (op === "gaussian" && args.length === 2)
    mean = le(zero, b) ? a : fail("gaussian variance");
  else if (op === "poisson" && args.length === 1) mean = le(zero, a) ? a : fail("poisson rate");
  else if (op === "exponential" && args.length === 1) {
    mean = lt(zero, a) ? div(one, a) : fail("exponential rate");
  } else if (op === "beta" && args.length === 2) {
    mean = lt(zero, a) && lt(zero, b) ? div(a, add(a, b)) : fail("beta parameters");
  } else if (op === "gamma" && args.length === 2) {
    mean = lt(zero, a) && lt(zero, b) ? div(a, b) : fail("gamma parameters");
  } else if (op === "bernoulli" && args.length === 1) {
    mean = le(zero, a) && le(a, one) ? a : fail("bernoulli probability");
  } else return fail("primitive arity");
  if (!supportedDraw(action, op)) throw new Failure("unsupported", `stochastic ${opRepr(op)}`);
  if (action === "mean") return [[one, mean]];
  return [
    [sub(one, a), zero],
    [a, one],
  ];
}

const binaryKinds = new Set(["app", "pair", "cons", "add", "mul", "div", "lt"]);
const unaryKinds = new Set(["fst", "snd", "inl", "inr", "neg"]);
const twoParameters = new Set(["uniform", "gaussian", "beta", "gamma"]);
const oneParameter = new Set(["poisson", "exponential", "bernoulli"]);

/** A node's subexpressions, in Lean's constructor order. */
function kidsOf(program: Program): Program[] {
  if ("c" in program) return [program.a, program.b, program.c];
  if ("b" in program) return [program.a, program.b];
  if ("a" in program) return [program.a];
  return [];
}

/** Whether every variable of `program` is bound, as Lean's `Binding.Scoped 0`; without
 * recursion, so that a deeply nested program can't overflow the call stack. */
export function scoped(program: Program): boolean {
  const pending: [Program, number][] = [[program, 0]];
  for (let next = pending.pop(); next; next = pending.pop()) {
    const [node, depth] = next;
    if (node.kind === "bvar" && node.index >= depth) return false;
    const binds =
      node.kind === "lam"
        ? [1]
        : node.kind === "fix"
          ? [2]
          : node.kind === "letE"
            ? [0, 1]
            : node.kind === "matchSum"
              ? [0, 1, 1]
              : node.kind === "matchList"
                ? [0, 0, 2]
                : [];
    for (const [i, kid] of kidsOf(node).entries()) pending.push([kid, depth + (binds[i] ?? 0)]);
  }
  return true;
}

/** The sample sites of `program`, in syntax order, without recursion. */
function sitesOf(program: Program): Program[] {
  const found: Program[] = [];
  const pending = [program];
  for (let node = pending.pop(); node; node = pending.pop()) {
    if ("site" in node) found.push(node);
    pending.push(...kidsOf(node).reverse());
  }
  return found;
}

/** The machine of one exploration: it builds each structure once, and steps states. */
export class Machine {
  private readonly table = new Map<string, { id: number }>();
  private readonly codes = new WeakMap<Program, Code>();
  private count = 0;

  private intern<T extends object>(key: string, make: (id: number) => T): T {
    const known = this.table.get(key);
    if (known) return known as T;
    const made = make(this.count++);
    this.table.set(key, made as { id: number });
    return made;
  }

  /** `program` as the machine's code; equal structures give the same code. */
  code(program: Program): Code {
    // Bottom up, without recursion, so that a deeply nested program can't overflow the call stack.
    const pending: [Program, boolean][] = [[program, false]];
    for (let next = pending.pop(); next; next = pending.pop()) {
      const [node, ready] = next;
      if (this.codes.has(node)) continue;
      const kids = kidsOf(node);
      if (!ready) {
        pending.push([node, true], ...kids.map((kid): [Program, boolean] => [kid, false]));
        continue;
      }
      const codes = kids.map((kid) => this.codes.get(kid) as Code);
      let key = `c${node.kind}`;
      if (node.kind === "bvar") key += node.index;
      else if (node.kind === "bool") key += node.value;
      else if (node.kind === "real") key += fraction(node.value);
      if ("site" in node) key += node.site;
      for (const kid of codes) key += `,${kid.id}`;
      this.codes.set(
        node,
        this.intern(key, (id): Code => ({ id, node, kids: codes })),
      );
    }
    return this.codes.get(program) as Code;
  }

  /** The program that `initial` runs. */
  private root: Program | null = null;

  /**
   * The sites that a failure at the site `at` may stand for. Exploration identifies equal states,
   * as Lean's does, so a frame stands for every site of the same structure; a discrete draw's frame
   * keeps only its action, so it stands for every discrete site with that action.
   */
  sitesLike(at: Program): Program[] {
    if (!this.root) return [at];
    const sites = sitesOf(this.root);
    if (at.kind === "discrete") {
      return sites.filter((site) => site.kind === "discrete" && site.site === at.site);
    }
    const code = this.code(at);
    return sites.filter((site) => this.code(site) === code);
  }

  /** The state that runs `program`, as Lean's `initialState`. */
  initial(program: Program): State {
    this.root = program;
    return this.evalState(this.code(program), null, null);
  }

  private value<V extends Value>(key: string, make: (id: number) => V): V {
    return this.intern(`v${key}`, make);
  }

  readonly unit: Value = this.value("unit", (id) => ({ id, tag: "unit" }));
  readonly nil: Value = this.value("nil", (id) => ({ id, tag: "nil" }));

  bool(value: boolean): Value {
    return this.value(`b${value}`, (id) => ({ id, tag: "bool", value }));
  }

  number(value: Rational): Value {
    return this.value(`n${fraction(value)}`, (id) => ({ id, tag: "number", value }));
  }

  pair(a: Value, b: Value): Value {
    return this.value(`p${a.id},${b.id}`, (id) => ({ id, tag: "pair", a, b }));
  }

  injection(tag: "inl" | "inr", value: Value): Value {
    return this.value(`${tag}${value.id}`, (id) => ({ id, tag, value }));
  }

  cell(head: Value, tail: Value): Value {
    return this.value(`k${head.id},${tail.id}`, (id) => ({ id, tag: "cons", head, tail }));
  }

  function(tag: "closure" | "recursive", body: Code, env: Env): Value {
    return this.value(`${tag}${body.id},${env?.id ?? ""}`, (id) => ({ id, tag, body, env }));
  }

  env(head: Value, tail: Env): Env {
    return this.intern(`e${head.id},${tail?.id ?? ""}`, (id) => ({ id, head, tail }));
  }

  push(frame: Frame, tail: Stack): Stack {
    return this.intern(`s${frame.id},${tail?.id ?? ""}`, (id) => ({ id, head: frame, tail }));
  }

  private frame<F extends Frame>(key: string, make: (id: number) => F): F {
    return this.intern(`f${key}`, make);
  }

  evalState(expr: Code, env: Env, stack: Stack): State {
    return this.intern(`E${expr.id},${env?.id ?? ""},${stack?.id ?? ""}`, (id) => ({
      id,
      tag: "eval",
      expr,
      env,
      stack,
    }));
  }

  deliver(value: Value, stack: Stack): State {
    return this.intern(`D${value.id},${stack?.id ?? ""}`, (id) => ({
      id,
      tag: "deliver",
      value,
      stack,
    }));
  }

  readonly rejected: State = this.intern("R", (id) => ({ id, tag: "rejected" }));

  /** `frame` pushed on `stack`, built once per structure. */
  private framed(frame: FrameData, stack: Stack): Stack {
    return this.push(this.makeFrame(frame), stack);
  }

  makeFrame(frame: FrameData): Frame {
    const env = (e: Env) => e?.id ?? "";
    const f = frame;
    switch (f.tag) {
      case "unary":
        return this.frame(`u${f.op}`, (id) => ({ ...f, id }));
      case "left":
        return this.frame(`l${f.op},${f.right.id},${env(f.env)}`, (id) => ({ ...f, id }));
      case "right":
        return this.frame(`r${f.op},${f.left.id}`, (id) => ({ ...f, id }));
      case "choose":
        return this.frame(`c${f.yes.id},${f.no.id},${env(f.env)}`, (id) => ({ ...f, id }));
      case "letBody":
        return this.frame(`b${f.body.id},${env(f.env)}`, (id) => ({ ...f, id }));
      case "matchSum":
        return this.frame(`m${f.left.id},${f.right.id},${env(f.env)}`, (id) => ({ ...f, id }));
      case "matchList":
        return this.frame(`L${f.nilCase.id},${f.consCase.id},${env(f.env)}`, (id) => ({
          ...f,
          id,
        }));
      case "discrete":
        return this.frame(`d${f.action}`, (id) => ({ ...f, id }));
      case "draw": {
        const op = typeof f.op === "string" ? f.op : `discrete${f.op.discrete}`;
        const pending = f.pending.map((code) => code.id).join(";");
        const args = f.args.map(fraction).join(";");
        return this.frame(`w${f.action},${op},${pending},${env(f.env)},${args}`, (id) => ({
          ...f,
          id,
        }));
      }
    }
  }

  /** Lean's `draw`: the outcomes of a draw, each delivered to `stack`. */
  private draw(action: Action, op: Op, args: Rational[], stack: Stack, at: Program): Step {
    let outcomes: [Rational, Rational][];
    try {
      outcomes = finiteLaw(op, action, args);
    } catch (error) {
      if (error instanceof Failure) {
        throw new Failure(error.kind, error.detail, at, this.sitesLike(at));
      }
      throw error;
    }
    return {
      kind: "next",
      successors: outcomes.map(([p, x]) => [p, this.deliver(this.number(x), stack)]),
    };
  }

  private binary(op: Binary, left: Value, right: Value, stack: Stack): State {
    if (op === "app" && left.tag === "closure") {
      return this.evalState(left.body, this.env(right, left.env), stack);
    }
    if (op === "app" && left.tag === "recursive") {
      return this.evalState(left.body, this.env(right, this.env(left, left.env)), stack);
    }
    if (op === "pair") return this.deliver(this.pair(left, right), stack);
    if (op === "cons") return this.deliver(this.cell(left, right), stack);
    if (left.tag === "number" && right.tag === "number") {
      const [a, b] = [left.value, right.value];
      if (op === "add") return this.deliver(this.number(add(a, b)), stack);
      if (op === "mul") return this.deliver(this.number(mul(a, b)), stack);
      if (op === "div") {
        if (isZero(b)) throw new Failure("invalid", "division by zero");
        return this.deliver(this.number(div(a, b)), stack);
      }
      if (op === "lt") return this.deliver(this.bool(lt(a, b)), stack);
    }
    throw new Failure("invalid", "binary operand types");
  }

  private unary(op: Unary, value: Value): Value {
    if (op === "fst" && value.tag === "pair") return value.a;
    if (op === "snd" && value.tag === "pair") return value.b;
    if (op === "inl" || op === "inr") return this.injection(op, value);
    if (op === "neg" && value.tag === "number") return this.number(neg(value.value));
    throw new Failure("invalid", "unary operand type");
  }

  /** One transition, as Lean's `step`; a failure is thrown as a `Failure`. */
  step(state: State): Step {
    if (state.tag === "rejected") return { kind: "rejected" };
    if (state.tag === "eval") return this.next(this.evaluate(state.expr, state.env, state.stack));
    const { value, stack } = state;
    if (stack === null) {
      if (value.tag === "number") return { kind: "returned", reward: value.value };
      throw new Failure("nonnumericResult", "");
    }
    const frame = stack.head;
    const rest = stack.tail;
    switch (frame.tag) {
      case "unary":
        return this.next(this.deliver(this.unary(frame.op, value), rest));
      case "left":
        return this.next(
          this.evalState(
            frame.right,
            frame.env,
            this.framed({ tag: "right", op: frame.op, left: value }, rest),
          ),
        );
      case "right":
        return this.next(this.binary(frame.op, frame.left, value, rest));
      case "letBody":
        return this.next(this.evalState(frame.body, this.env(value, frame.env), rest));
      case "choose":
        if (value.tag !== "bool") throw new Failure("invalid", "non-Boolean condition");
        return this.next(this.evalState(value.value ? frame.yes : frame.no, frame.env, rest));
      case "matchSum":
        if (value.tag === "inl") {
          return this.next(this.evalState(frame.left, this.env(value.value, frame.env), rest));
        }
        if (value.tag === "inr") {
          return this.next(this.evalState(frame.right, this.env(value.value, frame.env), rest));
        }
        throw new Failure("invalid", "sum match operand");
      case "matchList":
        if (value.tag === "nil") return this.next(this.evalState(frame.nilCase, frame.env, rest));
        if (value.tag === "cons") {
          const env = this.env(value.head, this.env(value.tail, frame.env));
          return this.next(this.evalState(frame.consCase, env, rest));
        }
        throw new Failure("invalid", "list match operand");
      case "discrete": {
        const probabilities: Rational[] = [];
        let cell = value;
        while (cell.tag === "cons" && cell.head.tag === "number") {
          probabilities.push(cell.head.value);
          cell = cell.tail;
        }
        if (cell.tag !== "nil") throw new Failure("invalid", "expected a list of probabilities");
        const op = { discrete: probabilities.length };
        return this.draw(frame.action, op, probabilities, rest, frame.at);
      }
      case "draw": {
        if (value.tag !== "number") throw new Failure("invalid", "nonnumeric primitive parameter");
        const args = [...frame.args, value.value];
        const [next, ...pending] = frame.pending;
        if (!next) return this.draw(frame.action, frame.op, args, rest, frame.at);
        const drawing = this.framed({ ...frame, pending, args }, rest);
        return this.next(this.evalState(next, frame.env, drawing));
      }
    }
  }

  private next(state: State): Step {
    return { kind: "next", successors: [[one, state]] };
  }

  private evaluate(expr: Code, env: Env, stack: Stack): State {
    const node = expr.node;
    const [a, b, c] = expr.kids;
    switch (node.kind) {
      case "bvar": {
        let cell = env;
        for (let i = 0; i < node.index && cell; i++) cell = cell.tail;
        if (!cell) throw new Failure("invalid", "unbound variable");
        return this.deliver(cell.head, stack);
      }
      case "reject":
        return this.rejected;
      case "unit":
        return this.deliver(this.unit, stack);
      case "bool":
        return this.deliver(this.bool(node.value), stack);
      case "real":
        return this.deliver(this.number(node.value), stack);
      case "nil":
        return this.deliver(this.nil, stack);
      case "lam":
        return this.deliver(this.function("closure", a, env), stack);
      case "fix":
        return this.deliver(this.function("recursive", a, env), stack);
      case "ite":
        return this.evalState(a, env, this.framed({ tag: "choose", yes: b, no: c, env }, stack));
      case "letE":
        return this.evalState(a, env, this.framed({ tag: "letBody", body: b, env }, stack));
      case "matchSum":
        return this.evalState(
          a,
          env,
          this.framed({ tag: "matchSum", left: b, right: c, env }, stack),
        );
      case "matchList":
        return this.evalState(
          a,
          env,
          this.framed({ tag: "matchList", nilCase: b, consCase: c, env }, stack),
        );
      case "discrete":
        return this.evalState(
          a,
          env,
          this.framed({ tag: "discrete", action: node.site, at: node }, stack),
        );
      default:
        break;
    }
    if (binaryKinds.has(node.kind)) {
      const op = node.kind as Binary;
      return this.evalState(a, env, this.framed({ tag: "left", op, right: b, env }, stack));
    }
    if (unaryKinds.has(node.kind)) {
      return this.evalState(a, env, this.framed({ tag: "unary", op: node.kind as Unary }, stack));
    }
    if ("site" in node && (twoParameters.has(node.kind) || oneParameter.has(node.kind))) {
      const pending = twoParameters.has(node.kind) ? [b] : [];
      const frame = {
        tag: "draw" as const,
        action: node.site,
        op: node.kind as Op,
        pending,
        env,
        args: [],
        at: node,
      };
      return this.evalState(a, env, this.framed(frame, stack));
    }
    throw new Error(`unexpected program node ${node.kind}`);
  }
}
