import type { Expr, Mode, TypedExpr } from "./ast.ts";
import { determinize } from "./determinize.ts";
import { CompileError } from "./errors.ts";
import type { SpanInfo } from "./infer.ts";
import { collectSpans, defaultModes, inferProgram, typedChildren } from "./infer.ts";
import { parse } from "./parser.ts";
import { prettyExpr, prettyTyped } from "./pretty.ts";
import { formatLeanType, zonk } from "./types.ts";

/** The stage of Lean's front end that rejects a program: `Frontend/Parser.lean`,
 * `Elaborate.lean` or `Infer.lean`. */
export type Stage = "parse" | "elaboration" | "inference";

export interface Diagnostic {
  from?: number;
  to?: number;
  message: string;
}

export type Analysis =
  | {
      ok: true;
      /** The program's type, printed as Lean's `prettyType` prints it. */
      type: string;
      /** The mode of every sample site, in the order of Lean's `Expr.sites`. */
      affinities: Mode[];
      ast: Expr;
      typedAstRaw: TypedExpr;
      typedAstDefaulted: TypedExpr;
      determinizedAst: Expr;
      pretty: {
        parsed: string;
        elaboratedRaw: string;
        elaboratedDefaulted: string;
        determinized: string;
      };
      spans: SpanInfo[];
    }
  /** `stage` is null when the simulator itself failed. */
  | { ok: false; stage: Stage | null; diagnostics: Diagnostic[] };

export function analyze(source: string): Analysis {
  let stage: Stage = "parse";
  try {
    const ast = parse(source);
    stage = "inference";
    const typedAstRaw = inferProgram(ast);
    const elaboratedRaw = prettyTyped(typedAstRaw);
    defaultModes(typedAstRaw);
    const elaboratedDefaulted = prettyTyped(typedAstRaw);
    const determinizedAst = determinize(typedAstRaw);
    const determinized = prettyExpr(determinizedAst);
    const spans = collectSpans(typedAstRaw)
      .filter((span) => span.from != null && span.to != null && span.to >= span.from)
      .sort((a, b) => a.to - a.from - (b.to - b.from));

    return {
      ok: true,
      type: formatLeanType(typedAstRaw.typ),
      affinities: sites(typedAstRaw),
      ast,
      typedAstRaw,
      typedAstDefaulted: typedAstRaw,
      determinizedAst,
      pretty: {
        parsed: prettyExpr(ast),
        elaboratedRaw,
        elaboratedDefaulted,
        determinized,
      },
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

/** The modes of the sample sites in syntax order; `flip` samples a Bernoulli at G. */
function sites(te: TypedExpr, out: Mode[] = []): Mode[] {
  if (te.kind === "Flip") out.push("G");
  else if ("mode" in te) {
    const ty = zonk(te.typ);
    if (ty.tag === "Float" && ty.mode.mode) out.push(ty.mode.mode);
  }
  for (const child of typedChildren(te)) sites(child, out);
  return out;
}
