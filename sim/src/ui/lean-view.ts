// Why Lean rejects the source, as the simulator's port of its front end finds it, in an alert
// under the source editor; and the sample sites of a program in words.
import type { ReadonlySignal } from "@preact/signals-core";
import { effect } from "@preact/signals-core";
import type { Analysis, Stage } from "../core/compiler/analyze.ts";
import type { Program } from "../core/compiler/core.ts";
import { sampleSites } from "../core/compiler/core.ts";
import { normalizeDiagnostics } from "./editor.ts";

/** The sample sites of a program as `--sample-sites` counts them, in words. */
export function describeSites(program: Program) {
  const { discrete, continuous } = sampleSites(program);
  const parts = [
    ...(discrete > 0 ? [`${discrete} discrete`] : []),
    ...(continuous > 0 || discrete === 0 ? [`${continuous} continuous`] : []),
  ];
  return parts.join(", ");
}

const stageNames: Record<Stage, string> = {
  parse: "parsing",
  elaboration: "elaboration",
  inference: "inference",
};

/** The line of `offset` in `text`, from 1. */
function lineOf(text: string, offset: number) {
  return text.slice(0, offset).split("\n").length;
}

function paragraph(...children: (string | Node)[]) {
  const p = document.createElement("p");
  p.append(...children);
  return p;
}

function code(text: string) {
  const element = document.createElement("code");
  element.textContent = text;
  return element;
}

/**
 * The alert for a program that Lean rejects: the stage, and at inference Lean's class of error,
 * whose exact message the simulator's port doesn't reproduce elsewhere; then the simulator's own
 * message with its line.
 */
function renderAlert(alert: HTMLElement, result: Analysis & { ok: false }, source: string) {
  const [diagnostic] = normalizeDiagnostics(result, source);
  const at = diagnostic ? `Line ${lineOf(source, diagnostic.from)}: ` : "";
  const message = `${at}${diagnostic?.message ?? "no message"}.`.replace(/\.\.$/, ".");
  if (result.stage === null) {
    const strong = document.createElement("strong");
    strong.textContent = "The simulator failed on this program.";
    alert.replaceChildren(
      paragraph(
        strong,
        " ",
        code("./run.sh --check program.det"),
        " shows whether Lean accepts it.",
      ),
      paragraph(message),
    );
    return;
  }
  const strong = document.createElement("strong");
  strong.textContent = "Lean rejects this program";
  const kind =
    result.stage === "inference"
      ? code(
          result.counterexample
            ? "inconsistent E/G constraints"
            : "incompatible or infinite type shapes",
        )
      : null;
  alert.replaceChildren(
    paragraph(
      strong,
      ` at ${stageNames[result.stage]}${kind ? ": " : "."}`,
      ...(kind ? [kind, "."] : []),
    ),
    paragraph(message),
  );
}

/** Shows in `alert` why Lean rejects the source, while it does. */
export function mountLeanView(
  alert: HTMLElement,
  analysis: ReadonlySignal<Analysis>,
  source: ReadonlySignal<string>,
) {
  effect(() => {
    const result = analysis.value;
    const text = source.peek();
    const empty = text.trim() === "";
    alert.hidden = result.ok || empty;
    if (!result.ok && !empty) renderAlert(alert, result, text);
  });
}
