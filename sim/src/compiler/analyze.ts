import type { Expr, TypedExpr } from "./ast.ts";
import { determinize } from "./determinize.ts";
import { CompileError } from "./errors.ts";
import type { SpanInfo } from "./infer.ts";
import { collectSpans, defaultModes, inferProgram } from "./infer.ts";
import { parse } from "./parser.ts";
import { prettyExpr, prettyTyped } from "./pretty.ts";

export interface Diagnostic {
  from?: number;
  to?: number;
  message: string;
}

export type Analysis =
  | {
      ok: true;
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
  | { ok: false; diagnostics: Diagnostic[] };

export function analyze(source: string): Analysis {
  try {
    const ast = parse(source);
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
        diagnostics: [{ from: error.from, to: error.to, message: error.message }],
      };
    }
    return {
      ok: false,
      diagnostics: [{ message: error instanceof Error ? error.message : String(error) }],
    };
  }
}
