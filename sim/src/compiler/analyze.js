import { CompileError } from "./errors.js";
import { determinize } from "./determinize.js";
import { collectSpans, defaultModes, inferProgram } from "./infer.js";
import { parse } from "./parser.js";
import { prettyExpr, prettyTyped } from "./pretty.js";

export function analyze(source) {
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
      .sort((a, b) => (a.to - a.from) - (b.to - b.from));

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
      return { ok: false, diagnostics: [{ from: error.from, to: error.to, message: error.message }] };
    }
    return { ok: false, diagnostics: [{ message: error?.message ?? String(error) }] };
  }
}
