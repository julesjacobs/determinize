// The size of a state as Lean's exploration measures it against its byte limit:
// `(reprStr state).utf8ByteSize`, the state printed by its derived `Repr` instance and laid out by
// `Std.Format.pretty` at Lean's default width of 120 columns. Every character of it is ASCII, so
// the bytes are its characters.
//
// The layout is a port of `Format.pretty`'s algorithm (`Init/Data/Format/Basic.lean`) for the
// formats that derived `Repr` instances build: text, line breaks, nesting and all-or-none groups.
// Laying out every state would cost as much as printing it, so `exceeds` first takes an upper
// bound that hash-consing makes cheap: the width of the state on one line plus, for every
// possible line break, the indentation it would add. Only a state whose bound exceeds the limit
// is laid out.
import type { Code, Env, Frame, List, Op, State, Value } from "./machine.ts";
import type { Rational } from "./rational.ts";

/** Lean's `max_prec`: a constructor's argument at it is parenthesized. */
const maxPrec = 1024;
/** Lean's `Format.defWidth`. */
export const width = 120;

export type Format =
  | { t: "text"; s: string }
  | { t: "line" }
  | { t: "nest"; n: number; f: Format }
  | { t: "append"; a: Format; b: Format }
  | { t: "group"; f: Format };

const line: Format = { t: "line" };
const text = (s: string): Format => ({ t: "text", s });
const append = (a: Format, b: Format): Format => ({ t: "append", a, b });
const nest = (n: number, f: Format): Format => ({ t: "nest", n, f });
const group = (f: Format): Format => ({ t: "group", f });

/** Lean's `Format.bracket`. */
function bracket(left: string, f: Format, right: string): Format {
  return group(nest(left.length, append(append(text(left), f), text(right))));
}

/** A derived `Repr` instance's format of constructor `name` applied to `args`, at `prec`. */
function applied(name: string, args: Format[], prec: number): Format {
  let body = text(name);
  for (const arg of args) body = append(append(body, line), arg);
  const f = group(nest(prec >= maxPrec ? 1 : 2, body));
  return prec >= maxPrec ? bracket("(", f, ")") : f;
}

/** Lean's `List.repr` for elements that aren't `ReprAtom`s. Lean's `joinSep` nests its appends to
 * the left; these nest to the right, which lays out the same and keeps `spaceUptoLine`'s recursion
 * as shallow as the line is wide. */
function list(items: Format[]): Format {
  if (items.length === 0) return text("[]");
  let joined = items[items.length - 1];
  for (let i = items.length - 2; i >= 0; i--) {
    joined = append(items[i], append(append(text(","), line), joined));
  }
  return bracket("[", joined, "]");
}

function rat(q: Rational): string {
  return q.den === 1n ? String(q.num) : `(${q.num} : Rat)/${q.den}`;
}

const actionName = "Determinize.Spec.Paper.DistributionAction";

function actionFormat(action: "E" | "G" | "mean", prec: number): Format {
  if (action === "mean") return applied(`${actionName}.mean`, [], prec);
  const affinity = applied(`Determinize.Spec.Paper.Affinity.${action}`, [], maxPrec);
  return applied(`${actionName}.sample`, [affinity], prec);
}

function opFormat(op: Op, prec: number): Format {
  if (typeof op === "string") return applied(`Determinize.Spec.Paper.Op.${op}`, [], prec);
  return applied("Determinize.Spec.Paper.Op.discrete", [text(String(op.discrete))], prec);
}

function items<T>(cells: List<T>): T[] {
  const out: T[] = [];
  for (let cell = cells; cell; cell = cell.tail) out.push(cell.head);
  return out;
}

/** `combine` of every code below `root`, its subexpressions' results first, computed without
 * recursion; codes in `known` keep theirs. */
function bottomUp<R>(
  root: Code,
  combine: (code: Code, kids: R[]) => R,
  known: Map<Code, R> = new Map(),
): Map<Code, R> {
  const pending: [Code, boolean][] = root.kids.map((kid) => [kid, false]);
  for (let next = pending.pop(); next; next = pending.pop()) {
    const [code, ready] = next;
    if (known.has(code)) continue;
    if (!ready) {
      pending.push([code, true], ...code.kids.map((kid): [Code, boolean] => [kid, false]));
      continue;
    }
    known.set(
      code,
      combine(
        code,
        code.kids.map((kid) => known.get(kid) as R),
      ),
    );
  }
  return known;
}

/** A code's format, from its subexpressions' formats at max_prec. */
function codeFormat(code: Code, kids: Format[], prec: number): Format {
  const node = code.node;
  const args: Format[] = [];
  if (node.kind === "bvar") args.push(text(String(node.index)));
  else if (node.kind === "bool") args.push(text(String(node.value)));
  else if (node.kind === "real") args.push(text(rat(node.value)));
  if ("site" in node) args.push(actionFormat(node.site, maxPrec));
  args.push(...kids);
  return applied(`Determinize.Spec.Paper.Expr.${node.kind}`, args, prec);
}

/** The formats of a state and its parts, as Lean's derived `Repr` instances build them. */
export const formats = {
  code(code: Code, prec: number): Format {
    // Each subexpression at max_prec, bottom up, so that a deep program can't overflow the stack.
    const made = bottomUp<Format>(code, (kid, kids) => codeFormat(kid, kids, maxPrec));
    return codeFormat(
      code,
      code.kids.map((kid) => made.get(kid) as Format),
      prec,
    );
  },

  value(value: Value, prec: number): Format {
    const name = `Determinize.Finite.Value.${value.tag}`;
    switch (value.tag) {
      case "unit":
      case "nil":
        return applied(name, [], prec);
      case "bool":
        return applied(name, [text(String(value.value))], prec);
      case "number":
        return applied(name, [text(rat(value.value))], prec);
      case "pair":
        return applied(
          name,
          [formats.value(value.a, maxPrec), formats.value(value.b, maxPrec)],
          prec,
        );
      case "inl":
      case "inr":
        return applied(name, [formats.value(value.value, maxPrec)], prec);
      case "cons":
        return applied(
          name,
          [formats.value(value.head, maxPrec), formats.value(value.tail, maxPrec)],
          prec,
        );
      case "closure":
      case "recursive":
        return applied(name, [formats.code(value.body, maxPrec), formats.env(value.env)], prec);
    }
  },

  env(env: Env): Format {
    return list(items(env).map((value) => formats.value(value, 0)));
  },

  frame(frame: Frame, prec: number): Format {
    const name = `Determinize.Finite.Frame.${frame.tag}`;
    const code = (c: Code) => formats.code(c, maxPrec);
    const unary = (op: string) => applied(`Determinize.Finite.Unary.${op}`, [], maxPrec);
    const binary = (op: string) => applied(`Determinize.Finite.Binary.${op}`, [], maxPrec);
    switch (frame.tag) {
      case "unary":
        return applied(name, [unary(frame.op)], prec);
      case "left":
        return applied(name, [binary(frame.op), code(frame.right), formats.env(frame.env)], prec);
      case "right":
        return applied(name, [binary(frame.op), formats.value(frame.left, maxPrec)], prec);
      case "choose":
        return applied(name, [code(frame.yes), code(frame.no), formats.env(frame.env)], prec);
      case "letBody":
        return applied(name, [code(frame.body), formats.env(frame.env)], prec);
      case "matchSum":
        return applied(name, [code(frame.left), code(frame.right), formats.env(frame.env)], prec);
      case "matchList":
        return applied(
          name,
          [code(frame.nilCase), code(frame.consCase), formats.env(frame.env)],
          prec,
        );
      case "discrete":
        return applied(name, [actionFormat(frame.action, maxPrec)], prec);
      case "draw": {
        const site = bracket(
          "(",
          append(
            append(actionFormat(frame.action, 0), append(text(","), line)),
            opFormat(frame.op, 0),
          ),
          ")",
        );
        const pending = list(frame.pending.map((c) => formats.code(c, 0)));
        const args = list(frame.args.map((q) => text(rat(q))));
        return applied(name, [site, pending, formats.env(frame.env), args], prec);
      }
    }
  },

  stack(stack: List<Frame>): Format {
    return list(items(stack).map((frame) => formats.frame(frame, 0)));
  },

  state(state: State, prec = 0): Format {
    const name = `Determinize.Finite.State.${state.tag}`;
    if (state.tag === "rejected") return applied(name, [], prec);
    if (state.tag === "eval") {
      return applied(
        name,
        [formats.code(state.expr, maxPrec), formats.env(state.env), formats.stack(state.stack)],
        prec,
      );
    }
    return applied(name, [formats.value(state.value, maxPrec), formats.stack(state.stack)], prec);
  },
};

// The layout, as `Format.prettyM` with `be`, `pushGroup` and `spaceUptoLine`.

interface Space {
  foundLine: boolean;
  space: number;
}

function merge(w: number, first: Space, second: (w: number) => Space): Space {
  if (first.space > w || first.foundLine) return first;
  const rest = second(w - first.space);
  return { foundLine: rest.foundLine, space: first.space + rest.space };
}

function spaceUptoLine(f: Format, flatten: boolean, w: number): Space {
  switch (f.t) {
    case "text":
      return { foundLine: false, space: f.s.length };
    case "line":
      return flatten ? { foundLine: false, space: 1 } : { foundLine: true, space: 0 };
    case "append":
      return merge(w, spaceUptoLine(f.a, flatten, w), (rest) => spaceUptoLine(f.b, flatten, rest));
    case "nest":
      return spaceUptoLine(f.f, flatten, w);
    case "group":
      return spaceUptoLine(f.f, true, w);
  }
}

type Items = { f: Format; indent: number; tail: Items } | null;
/** A group: whether it is flattened (null outside every group), and its pending items. */
type Groups = { flatten: boolean | null; items: Items; tail: Groups } | null;

function spaceUptoLineGroups(groups: Groups, w: number): Space {
  if (!groups) return { foundLine: false, space: 0 };
  if (!groups.items) return spaceUptoLineGroups(groups.tail, w);
  const { f, tail } = groups.items;
  return merge(w, spaceUptoLine(f, groups.flatten === true, w), (rest) =>
    spaceUptoLineGroups({ ...groups, items: tail }, rest),
  );
}

function pushGroup(items: Items, groups: Groups, column: number, w: number): Groups {
  const room = Math.max(0, w - column);
  const own = spaceUptoLineGroups({ flatten: true, items, tail: null }, room);
  const total = merge(room, own, (rest) => spaceUptoLineGroups(groups, rest));
  return { flatten: total.space <= room, items, tail: groups };
}

/** What the layout of `f` writes: its text, or, without `out`, only its length. */
function layout(f: Format, out: string[] | null): number {
  let length = 0;
  let column = 0;
  let groups: Groups = { flatten: null, items: { f, indent: 0, tail: null }, tail: null };
  const write = (s: string) => {
    length += s.length;
    column += s.length;
    out?.push(s);
  };
  const newline = (indent: number) => {
    length += 1 + indent;
    column = indent;
    out?.push(`\n${" ".repeat(indent)}`);
  };
  while (groups) {
    const items: Items = groups.items;
    if (!items) {
      groups = groups.tail;
      continue;
    }
    const { f: item, indent, tail } = items;
    const rest = (next: Items): Groups => ({
      flatten: groups?.flatten ?? null,
      items: next,
      tail: groups?.tail ?? null,
    });
    switch (item.t) {
      case "append":
        groups = rest({ f: item.a, indent, tail: { f: item.b, indent, tail } });
        break;
      case "nest":
        groups = rest({ f: item.f, indent: indent + item.n, tail });
        break;
      case "text":
        write(item.s);
        groups = rest(tail);
        break;
      case "line":
        if (groups.flatten === true) write(" ");
        else newline(indent);
        groups = rest(tail);
        break;
      case "group":
        if (groups.flatten === true) groups = rest({ f: item.f, indent, tail });
        else groups = pushGroup({ f: item.f, indent, tail: null }, rest(tail), column, width);
        break;
    }
  }
  return length;
}

/** `f` laid out as `Format.pretty` lays it out at width 120. */
export function pretty(f: Format): string {
  const out: string[] = [];
  layout(f, out);
  return out.join("");
}

/** The length of `pretty(f)`. */
export function prettyLength(f: Format): number {
  return layout(f, null);
}

/** An upper bound of a format's length: its width on one line (`flat`), and the number of its
 * line breaks (`lines`) and their indentation (`indents`) inside it. */
interface Bound {
  flat: number;
  lines: number;
  indents: number;
}

const none: Bound = { flat: 0, lines: 0, indents: 0 };
const textBound = (s: string): Bound => ({ flat: s.length, lines: 0, indents: 0 });

/** The bound of `applied(name, args, prec)`. */
function constructorBound(name: string, args: Bound[], prec: number): Bound {
  let { flat, lines, indents } = textBound(name);
  for (const arg of args) {
    flat += 1 + arg.flat;
    lines += 1 + arg.lines;
    indents += arg.indents;
  }
  indents += (prec >= maxPrec ? 1 : 2) * lines;
  return prec >= maxPrec
    ? { flat: flat + 2, lines, indents: indents + lines }
    : { flat, lines, indents };
}

/** The bound of `list` of elements whose bounds add up to `sum`. */
function listBound(count: number, sum: Bound): Bound {
  if (count === 0) return textBound("[]");
  const lines = sum.lines + count - 1;
  return { flat: sum.flat + 2 * (count - 1) + 2, lines, indents: sum.indents + lines };
}

function plus(a: Bound, b: Bound): Bound {
  return { flat: a.flat + b.flat, lines: a.lines + b.lines, indents: a.indents + b.indents };
}

/** Sizes of the states of one machine, with the bounds of its shared parts kept by their ids. */
export class StateSizes {
  private readonly bounds = new Map<string, Bound>();
  private readonly lists = new Map<number, { count: number; sum: Bound }>();

  private remember(key: string, make: () => Bound): Bound {
    const known = this.bounds.get(key);
    if (known) return known;
    const made = make();
    this.bounds.set(key, made);
    return made;
  }

  /** The bounds of codes at max_prec, as their subexpressions are. */
  private readonly codes = new Map<Code, Bound>();

  private code(code: Code, prec: number): Bound {
    return this.remember(`${code.id}@${prec}`, () => {
      // Bottom up, so that a deep program can't overflow the call stack.
      bottomUp(code, (kid, kids) => this.codeBound(kid, kids, maxPrec), this.codes);
      return this.codeBound(
        code,
        code.kids.map((kid) => this.codes.get(kid) as Bound),
        prec,
      );
    });
  }

  private codeBound(code: Code, kids: Bound[], prec: number): Bound {
    const node = code.node;
    const args: Bound[] = [];
    if (node.kind === "bvar") args.push(textBound(String(node.index)));
    else if (node.kind === "bool") args.push(textBound(String(node.value)));
    else if (node.kind === "real") args.push(textBound(rat(node.value)));
    if ("site" in node) args.push(this.formatBound(actionFormat(node.site, maxPrec)));
    args.push(...kids);
    return constructorBound(`Determinize.Spec.Paper.Expr.${node.kind}`, args, prec);
  }

  private value(value: Value, prec: number): Bound {
    return this.remember(`${value.id}@${prec}`, () => {
      const name = `Determinize.Finite.Value.${value.tag}`;
      switch (value.tag) {
        case "unit":
        case "nil":
          return constructorBound(name, [], prec);
        case "bool":
          return constructorBound(name, [textBound(String(value.value))], prec);
        case "number":
          return constructorBound(name, [textBound(rat(value.value))], prec);
        case "pair":
          return constructorBound(
            name,
            [this.value(value.a, maxPrec), this.value(value.b, maxPrec)],
            prec,
          );
        case "inl":
        case "inr":
          return constructorBound(name, [this.value(value.value, maxPrec)], prec);
        case "cons":
          return constructorBound(
            name,
            [this.value(value.head, maxPrec), this.value(value.tail, maxPrec)],
            prec,
          );
        case "closure":
        case "recursive":
          return constructorBound(
            name,
            [this.code(value.body, maxPrec), this.list(value.env, (v) => this.value(v, 0))],
            prec,
          );
      }
    });
  }

  /** The bound of a list, from the sums of its suffixes, kept by their cells' ids. */
  private list<T>(cells: List<T>, element: (item: T) => Bound): Bound {
    const suffix = (cell: List<T>): { count: number; sum: Bound } => {
      if (!cell) return { count: 0, sum: none };
      const known = this.lists.get(cell.id);
      if (known) return known;
      // Iteratively, so that a long list can't overflow the call stack.
      const pending: NonNullable<List<T>>[] = [];
      let start: List<T> = cell;
      while (start && !this.lists.has(start.id)) {
        pending.push(start);
        start = start.tail;
      }
      let after = start
        ? (this.lists.get(start.id) ?? { count: 0, sum: none })
        : { count: 0, sum: none };
      for (const at of pending.reverse()) {
        after = { count: after.count + 1, sum: plus(element(at.head), after.sum) };
        this.lists.set(at.id, after);
      }
      return after;
    };
    const { count, sum } = suffix(cells);
    return listBound(count, sum);
  }

  private formatBound(f: Format): Bound {
    switch (f.t) {
      case "text":
        return textBound(f.s);
      case "line":
        return { flat: 1, lines: 1, indents: 0 };
      case "append":
        return plus(this.formatBound(f.a), this.formatBound(f.b));
      case "nest": {
        const inner = this.formatBound(f.f);
        return { ...inner, indents: inner.indents + f.n * inner.lines };
      }
      case "group":
        return this.formatBound(f.f);
    }
  }

  private frame(frame: Frame): Bound {
    return this.remember(`${frame.id}@0`, () => {
      const name = `Determinize.Finite.Frame.${frame.tag}`;
      const code = (c: Code) => this.code(c, maxPrec);
      const env = (e: Env) => this.list(e, (v) => this.value(v, 0));
      const atom = (kind: string, op: string) => textBound(`(Determinize.Finite.${kind}.${op})`);
      switch (frame.tag) {
        case "unary":
          return constructorBound(name, [atom("Unary", frame.op)], 0);
        case "left":
          return constructorBound(
            name,
            [atom("Binary", frame.op), code(frame.right), env(frame.env)],
            0,
          );
        case "right":
          return constructorBound(
            name,
            [atom("Binary", frame.op), this.value(frame.left, maxPrec)],
            0,
          );
        case "choose":
          return constructorBound(name, [code(frame.yes), code(frame.no), env(frame.env)], 0);
        case "letBody":
          return constructorBound(name, [code(frame.body), env(frame.env)], 0);
        case "matchSum":
          return constructorBound(name, [code(frame.left), code(frame.right), env(frame.env)], 0);
        case "matchList":
          return constructorBound(
            name,
            [code(frame.nilCase), code(frame.consCase), env(frame.env)],
            0,
          );
        case "discrete":
          return constructorBound(name, [this.formatBound(actionFormat(frame.action, maxPrec))], 0);
        case "draw": {
          const site = bracket(
            "(",
            append(
              append(actionFormat(frame.action, 0), append(text(","), line)),
              opFormat(frame.op, 0),
            ),
            ")",
          );
          const pending = listBound(
            frame.pending.length,
            frame.pending.map((c) => this.code(c, 0)).reduce(plus, none),
          );
          const args = this.formatBound(list(frame.args.map((q) => text(rat(q)))));
          return constructorBound(name, [this.formatBound(site), pending, env(frame.env), args], 0);
        }
      }
    });
  }

  /** An upper bound of the length of `reprStr state`. */
  bound(state: State): number {
    const total = this.remember(`${state.id}@0`, () => {
      const name = `Determinize.Finite.State.${state.tag}`;
      const stack = (s: List<Frame>) => this.list(s, (frame) => this.frame(frame));
      if (state.tag === "rejected") return constructorBound(name, [], 0);
      if (state.tag === "eval") {
        return constructorBound(
          name,
          [
            this.code(state.expr, maxPrec),
            this.list(state.env, (v) => this.value(v, 0)),
            stack(state.stack),
          ],
          0,
        );
      }
      return constructorBound(name, [this.value(state.value, maxPrec), stack(state.stack)], 0);
    });
    return total.flat + total.indents;
  }

  /** Whether `reprStr state` is longer than `limit` bytes. */
  exceeds(state: State, limit: number): boolean {
    if (this.bound(state) <= limit) return false;
    return prettyLength(formats.state(state)) > limit;
  }
}
