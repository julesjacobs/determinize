// What Lean's front end reports about the source, as the simulator's port of it computes it: why
// Lean rejects the program, or its checked type, the sample sites before and after
// determinization, and the annotated program as Lean prints it with the source's names. For a
// program that Lean rejects only for a mode conflict, the sample sites of its counterexample too.
import type { ReadonlySignal } from "@preact/signals-core";
import { effect } from "@preact/signals-core";
import type { Analysis, Stage } from "../core/compiler/analyze.ts";
import type { Program } from "../core/compiler/core.ts";
import { sampleSites } from "../core/compiler/core.ts";
import { sourcePretty } from "../core/compiler/print.ts";
import { normalizeDiagnostics } from "./editor.ts";

export interface LeanViewElements {
  /** Why Lean rejects the program, under the source editor. */
  alert: HTMLElement;
  /** The checked type, under the source editor. */
  checked: HTMLElement;
  type: HTMLElement;
  sourceSites: HTMLElement;
  determinizedSites: HTMLElement;
  annotated: HTMLDetailsElement;
  annotatedProgram: HTMLElement;
  /** That the simulator computes these with its port of Lean's front end. */
  note: HTMLElement;
}

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

export function mountLeanView(
  elements: LeanViewElements,
  analysis: ReadonlySignal<Analysis>,
  source: ReadonlySignal<string>,
) {
  effect(() => {
    const result = analysis.value;
    const text = source.peek();
    const empty = text.trim() === "";
    elements.alert.hidden = result.ok || empty;
    if (!result.ok && !empty) renderAlert(elements.alert, result, text);
    const programs = result.ok ? result.program : result.counterexample?.program;
    elements.checked.hidden = !result.ok;
    elements.type.textContent = result.ok ? result.type : "";
    elements.sourceSites.hidden = !programs;
    elements.determinizedSites.hidden = !programs;
    elements.annotated.hidden = !result.ok;
    elements.note.hidden = !programs;
    if (!programs) return;
    elements.sourceSites.lastElementChild?.replaceChildren(describeSites(programs.source));
    elements.determinizedSites.lastElementChild?.replaceChildren(
      describeSites(programs.determinized),
    );
    if (result.ok) elements.annotatedProgram.textContent = sourcePretty(programs.source);
  });
}
