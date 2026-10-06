import type { Expr, Mode } from "./ast.ts";
import type { Input } from "./core.ts";
import { sites } from "./core.ts";
import { annotate } from "./determinize.ts";
import { elaborate } from "./elaborate.ts";
import { CompileError } from "./errors.ts";
import { infer } from "./infer.ts";
import { parse } from "./parser.ts";
import { prettyExpr } from "./pretty.ts";
import type { Ty } from "./types.ts";
import { formatType, prettyType } from "./types.ts";

/** The stage of Lean's front end that rejects a program: `Frontend/Parser.lean`,
 * `Elaborate.lean` or `Infer.lean`. */
export type Stage = "parse" | "elaboration" | "inference";

export interface Diagnostic {
  from?: number;
  to?: number;
  message: string;
}

/** A highlighted range of the source, with its type and its hover text. */
export interface SpanInfo {
  from: number;
  to: number;
  kind: "identifier" | "distribution" | "expr";
  type: string;
  mode: Mode | undefined;
  text: string;
}

export type Analysis =
  | {
      ok: true;
      /** The program's type, printed as Lean's `prettyType` prints it. */
      type: string;
      /** The mode of every sample site, in the order of Lean's `Expr.sites`. */
      affinities: Mode[];
      ast: Expr;
      /** The program with the inferred mode at every site. */
      annotated: Expr;
      /** The annotated program with every E site replaced by its mean. */
      determinized: Expr;
      pretty: { annotated: string; determinized: string };
      spans: SpanInfo[];
    }
  /** `stage` is null when the simulator itself failed. */
  | { ok: false; stage: Stage | null; diagnostics: Diagnostic[] };

const distributionKinds = new Set([
  "Uniform",
  "Gauss",
  "Exponential",
  "Gamma",
  "Beta",
  "Flip",
  "Bernoulli",
  "Poisson",
  "DiscreteWeights",
  "DiscreteList",
]);

function spanInfo(source: Expr, ty: Ty): SpanInfo {
  const type = formatType(ty);
  const mode = ty.tag === "float" ? ty.mode : undefined;
  const distribution = distributionKinds.has(source.kind);
  const kindName = source.kind.startsWith("Discrete") ? "Discrete" : source.kind;
  let text = `${kindName}: ${type}`;
  if (distribution && mode === "E") text += "\ndeterminizes to its expectation";
  if (distribution && mode === "G") text += "\nsampled normally";
  return {
    from: source.from,
    to: source.to,
    kind: source.kind === "Var" ? "identifier" : distribution ? "distribution" : "expr",
    type,
    mode,
    text,
  };
}

export function analyze(source: string): Analysis {
  let stage: Stage = "parse";
  try {
    const ast = parse(source);
    stage = "elaboration";
    const input = elaborate(ast);
    stage = "inference";
    const result = infer(input);
    if (!result.ok) {
      const { at, message } = result.failure;
      return { ok: false, stage, diagnostics: [{ from: at.from, to: at.to, message }] };
    }
    const siteModes = new Map<Expr, Mode>();
    for (const site of sites<Mode | null>(input)) {
      const mode = result.modes.get(site);
      if (site.source && mode) siteModes.set(site.source, mode);
    }
    const modeOf = (site: Expr): Mode => {
      const mode = siteModes.get(site);
      if (!mode) throw new Error("a sample site without a mode");
      return mode;
    };
    const annotated = annotate(ast, modeOf);
    const determinized = annotate(ast, modeOf, true);
    const spans = [...result.types]
      .filter((entry): entry is [Input & { source: Expr }, Ty] => entry[0].source !== null)
      .map(([node, ty]) => spanInfo(node.source, ty))
      .sort((a, b) => a.to - a.from - (b.to - b.from));
    return {
      ok: true,
      type: prettyType(result.type),
      affinities: sites<Mode | null>(input).map((site) => result.modes.get(site) ?? "G"),
      ast,
      annotated,
      determinized,
      pretty: { annotated: prettyExpr(annotated), determinized: prettyExpr(determinized) },
      spans,
    };
  } catch (error) {
    if (error instanceof CompileError) {
      return {
        ok: false,
        stage,
        diagnostics: [{ from: error.from, to: error.to, message: error.message }],
      };
    }
    return {
      ok: false,
      stage: null,
      diagnostics: [{ message: error instanceof Error ? error.message : String(error) }],
    };
  }
}
