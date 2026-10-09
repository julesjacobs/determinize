// The exact values in the statistics table: each program's finite model, explored by the exact
// worker, with its mean, variance and probability of returning as fractions over their decimals,
// below the estimates from the runs; or why the program has none, in the cells that its values
// would take, with the additive mode to try where exploration stops at Lean's state limit. The
// rows keep their heights whatever exploration finds, so that nothing moves when it ends.
import { computed, effect } from "@preact/signals-core";
import type { Exact, ExactState, Range } from "../core/exact.ts";
import { minus, thin } from "./charts.ts";
import { formatStat } from "./distribution-view.ts";
import { escapeHtml } from "./html.ts";
import type { Store } from "./store.ts";

/** Why a draw has no finite model: a continuous law, or Poisson's infinitely many outcomes. */
const draws: Record<string, string> = {
  uniform: "uniform draw{at} is continuous",
  gaussian: "Gaussian draw{at} is continuous",
  exponential: "exponential draw{at} is continuous",
  beta: "beta draw{at} is continuous",
  gamma: "gamma draw{at} is continuous",
  poisson: "Poisson draw{at} has infinitely many outcomes",
};

/** Lean's limits as the reasons name them. */
const limits = {
  states: "More than 10 000 states, Lean's limit.",
  edges: "More than 100 000 transitions, Lean's limit.",
  stateBytes: "A state larger than 1 000 000 bytes, Lean's limit.",
};

/** The fraction longer than which a value is set smaller. */
const longFraction = 10;

/** What the row "Model" says of a program. */
export function modelText(state: ExactState): string {
  switch (state.kind) {
    case "exploring":
      return state.discovered > 0 ? `${thin(state.discovered)} states…` : "computing…";
    case "solving":
      return "solving…";
    case "finite":
    case "too many":
      return `${thin(state.states)} states`;
    case "limit":
      return "too large";
    case "unsolved":
    case "error":
      return "–";
    default:
      return "none";
  }
}

/** The lines of `sites` in `source`, as a phrase: " on line 3", " on line 1 or 3". */
function onLines(sites: Range[], source: string) {
  const lines = [...new Set(sites.map(({ from }) => source.slice(0, from).split("\n").length))];
  lines.sort((a, b) => a - b);
  const last = lines.pop();
  if (last === undefined) return "";
  return ` on line ${lines.length > 0 ? `${lines.join(", ")} or ${last}` : last}`;
}

/** Why a program has no exact values, in `source`; null where it has them or may still. */
export function reasonText(state: ExactState, source: string): string | null {
  switch (state.kind) {
    case "exploring":
    case "solving":
    case "finite":
      return null;
    case "draw": {
      const why = draws[state.distribution] ?? `${state.distribution} draw{at} has no finite model`;
      const at = onLines(state.sites, source);
      return `${at ? "The" : "A"} ${why.replace("{at}", at)}.`;
    }
    case "fails":
      return `A run fails${onLines(state.sites, source)}: ${state.detail}.`;
    case "not a number":
      return "The output isn't a number.";
    case "limit":
      return limits[state.limit];
    case "too many":
      return `More states than the ${thin(state.maxStates)} that Lean solves exactly; Lean passes larger models to the Storm model checker.`;
    case "unsolved":
      return `The exact solver failed: ${state.message}.`;
    case "error":
      return `The simulator couldn't compute them: ${state.message}`;
  }
}

/** A value's cell: its fraction over its decimal, or a probability of returning over its
 * percentage, or over the probability of rejection where there is one; an integer has no second
 * line. Empty while exploration runs, a dash where the program never returns. */
function valueCell(value: Exact | null | undefined, rejected?: Exact) {
  if (value === undefined) return '<td class="exact-cell"></td>';
  if (value === null) return '<td class="exact-cell"><span class="frac">–</span></td>';
  const { fraction, value: number } = value;
  // A long fraction is set smaller, and where even that is cut short, its title has it whole.
  const long = fraction.length > longFraction ? ` long" title="${escapeHtml(fraction)}` : "";
  const below =
    rejected && rejected.fraction !== "0"
      ? `${minus(rejected.fraction)} rejected`
      : !fraction.includes("/")
        ? "\u00a0"
        : rejected
          ? `${Number((100 * number).toFixed(2))} %`
          : formatStat(number);
  return `<td class="exact-cell"><span class="frac${long}">${escapeHtml(minus(fraction))}</span> <span class="dec sub">${escapeHtml(below)}</span></td>`;
}

export function mountExactView(
  table: HTMLElement,
  store: Pick<Store, "exact" | "additive" | "analysis" | "samples" | "samplesReady">,
) {
  const $ = <E extends Element>(id: string) => table.querySelector(`#${id}`) as E;
  const group = $<HTMLElement>("exact");
  const additive = $<HTMLInputElement>("additive");
  const rows = ["exact-mean", "exact-variance", "exact-returned"].map((id) => $<HTMLElement>(id));
  // The rows' headers link their Lean definitions; where neither program has values, they are
  // plain and muted, so that the rows read as the one statement beside them.
  const headers = rows.map((row) => row.querySelector("th") as HTMLElement);
  const linked = headers.map((header) => header.innerHTML);
  const models = {
    source: $<HTMLElement>("model-source"),
    determinized: $<HTMLElement>("model-det"),
  };

  additive.addEventListener("change", () => {
    store.additive.value = additive.checked;
  });
  table.addEventListener("click", (event) => {
    if (event.target instanceof HTMLElement && event.target.closest(".try-additive")) {
      store.additive.value = true;
    }
  });

  // The program whose estimates the table shows, which changes with a new program, not a batch.
  const shown = computed(() => store.samples.value.source);
  /** The rows as last written, so that a new batch of runs rewrites nothing. */
  let written = "";

  effect(() => {
    additive.checked = store.additive.value;
    const exact = store.exact.value;
    const result = store.analysis.value;
    // The rows follow the estimates above them, which keep the earlier program's until its runs
    // come in. A program that Lean accepts, and whose output is a number, has exact values.
    if (!store.samplesReady.value) return;
    if (!exact || exact.source !== shown.value || !result.ok || !result.type.startsWith("float")) {
      group.hidden = true;
      group.removeAttribute("aria-busy");
      return;
    }
    group.hidden = false;
    if (exact.done) group.removeAttribute("aria-busy");
    else group.setAttribute("aria-busy", "true");
    const programs = [exact.programs.source, exact.programs.determinized];
    const reasons = programs.map((state) => {
      const reason = reasonText(state, exact.source);
      // Where exploration stops at the state limit, the additive mode may need fewer states.
      if (reason === null || state.kind !== "limit" || state.limit !== "states") return reason;
      return exact.additive ? reason : reason.replace(/\.$/, "; try the additive mode.");
    });
    const why = (text: string, span: number) =>
      `<td class="exact-why" rowspan="3"${span > 1 ? ` colspan="${span}"` : ""}><p class="sub">${escapeHtml(text).replace("try the additive mode", '<button class="try-additive" type="button">try the additive mode</button>')}</p></td>`;
    const cells = programs.map((state, i) => {
      if (state.kind === "finite") {
        return [
          valueCell(state.mean),
          valueCell(state.variance),
          valueCell(state.returnProbability, state.rejectionProbability),
        ];
      }
      const reason = reasons[i];
      if (reason === null) {
        return [undefined, undefined, undefined].map((value) => valueCell(value));
      }
      // Both programs without a model for the same reason say it once.
      if (reason === reasons[1 - i]) return i === 0 ? [why(reason, 2), "", ""] : ["", "", ""];
      return [why(reason, 1), "", ""];
    });
    const texts = programs.map(modelText);
    const without = programs.every((state) => reasonText(state, exact.source) !== null);
    const html = JSON.stringify([texts, cells, without]);
    if (html === written) return;
    written = html;
    const keys = ["source", "determinized"] as const;
    for (const [i, key] of keys.entries()) {
      models[key].textContent = texts[i];
      const kind = programs[i].kind;
      models[key].classList.toggle("none", kind !== "finite" && kind !== "too many");
    }
    for (const [i, row] of rows.entries()) {
      for (const cell of row.querySelectorAll("td")) cell.remove();
      row.insertAdjacentHTML("beforeend", cells[0][i] + cells[1][i]);
      if (without) headers[i].textContent = headers[i].textContent?.trim() ?? "";
      else headers[i].innerHTML = linked[i];
      headers[i].classList.toggle("none", without);
    }
  });
}
