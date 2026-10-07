// The step table: the step table's run, frame by frame, with each frame's checks in a popover and
// the symbols and values that correspond to each other highlighted together.
import { effect } from "@preact/signals-core";
import type { Expr } from "../core/compiler/ast.ts";
import { prettyExpr } from "../core/compiler/pretty.ts";
import { formatNumber } from "../core/format.ts";
import { prettyAffine } from "../core/runtime/affine.ts";
import { distributionName } from "../core/runtime/distributions.ts";
import type { Binding, CoupledTrace, Frame } from "../core/runtime/semantics.ts";
import { domainErrorMessage, frameOk, hasDomainError, sigmaMeans } from "../core/trace.ts";
import { counterexampleLabel, escapeHtml } from "./html.ts";
import type { Store } from "./store.ts";
import type { TraceOptions } from "./trace-expr.ts";
import { changedPath, renderHighlightedText, renderTraceExpr } from "./trace-expr.ts";

export interface TraceViewElements {
  table: HTMLElement;
  status: HTMLElement;
}

export function mountTraceView(
  elements: TraceViewElements,
  store: Pick<Store, "trace" | "activeStep">,
) {
  const portal = document.createElement("div");
  portal.className = "floating-check-popover";
  portal.setAttribute("role", "tooltip");
  document.body.append(portal);

  /** The check whose popover is open. */
  let activeCheck: Element | null = null;
  let hideTimer: ReturnType<typeof setTimeout> | undefined;
  let activeCorrespondence: string | null = null;

  /** The step of the row that holds `check`. */
  function stepOf(check: Element) {
    const row = check.closest(".coupling-row");
    return row?.parentElement ? Array.prototype.indexOf.call(row.parentElement.children, row) : -1;
  }

  function showStep(check: Element) {
    clearTimeout(hideTimer);
    const step = stepOf(check);
    if (step >= 0) store.activeStep.value = step;
  }

  function scheduleHide() {
    clearTimeout(hideTimer);
    hideTimer = setTimeout(() => {
      store.activeStep.value = null;
    }, 120);
  }

  elements.table.addEventListener("pointerover", (event) => {
    const corr =
      event.target instanceof Element ? event.target.closest<HTMLElement>(".corr-item") : null;
    if (corr) showCorrespondence(corr);
    const check = event.target instanceof Element ? event.target.closest(".step-check") : null;
    if (check) showStep(check);
  });

  elements.table.addEventListener("pointerout", (event) => {
    const corr =
      event.target instanceof Element ? event.target.closest<HTMLElement>(".corr-item") : null;
    if (corr) {
      const next =
        event.relatedTarget instanceof Element
          ? event.relatedTarget.closest<HTMLElement>(".corr-item")
          : null;
      if (!next || next.dataset.corr !== corr.dataset.corr) hideCorrespondence();
    }
    const check = event.target instanceof Element ? event.target.closest(".step-check") : null;
    if (!check) return;
    const next = event.relatedTarget instanceof Element ? event.relatedTarget : null;
    if (next && (check.contains(next) || portal.contains(next))) return;
    scheduleHide();
  });

  elements.table.addEventListener("focusin", (event) => {
    const corr =
      event.target instanceof Element ? event.target.closest<HTMLElement>(".corr-item") : null;
    if (corr) showCorrespondence(corr);
    const check = event.target instanceof Element ? event.target.closest(".step-check") : null;
    if (check) showStep(check);
  });

  elements.table.addEventListener("focusout", (event) => {
    const corr = event.target instanceof Element ? event.target.closest(".corr-item") : null;
    if (corr) hideCorrespondence();
    const next = event.relatedTarget instanceof Element ? event.relatedTarget : null;
    if (next && (elements.table.contains(next) || portal.contains(next))) return;
    scheduleHide();
  });

  portal.addEventListener("pointerenter", () => clearTimeout(hideTimer));
  portal.addEventListener("pointerleave", scheduleHide);
  window.addEventListener(
    "scroll",
    () => {
      if (activeCheck) positionCheckPopover(portal, activeCheck);
    },
    true,
  );
  window.addEventListener("resize", () => {
    if (activeCheck) positionCheckPopover(portal, activeCheck);
  });
  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape") store.activeStep.value = null;
  });

  effect(() => {
    const state = store.trace.value;
    store.activeStep.value = null;
    if (state.kind === "run") {
      renderCoupling(elements, state.trace);
      return;
    }
    // Lean rejects the program for a reason other than a mode conflict, or the run failed.
    elements.table.innerHTML = "";
    elements.status.textContent = state.kind === "not run" ? "Not run" : "Trace unavailable";
    elements.status.className = "status error";
  });

  effect(() => {
    const step = store.activeStep.value;
    clearTimeout(hideTimer);
    if (activeCheck) activeCheck.classList.remove("popover-open");
    activeCheck = null;
    const check =
      step === null
        ? null
        : elements.table.querySelectorAll(".coupling-row")[step]?.querySelector(".step-check");
    const source = check?.querySelector(".check-popover-source");
    if (!check || !source) {
      portal.classList.remove("visible");
      return;
    }
    activeCheck = check;
    portal.innerHTML = source.innerHTML;
    portal.classList.add("visible");
    check.classList.add("popover-open");
    positionCheckPopover(portal, check);
  });

  function showCorrespondence(anchor: HTMLElement) {
    const symbol = anchor.dataset.corr;
    if (!symbol) return;
    const scope = anchor.closest(".coupling-row") ?? elements.table;
    const key = `${symbol}:${rowIndex(scope)}`;
    if (activeCorrespondence === key) return;
    hideCorrespondence();
    activeCorrespondence = key;
    for (const item of scope.querySelectorAll(`[data-corr="${cssEscape(symbol)}"]`)) {
      item.classList.add("corr-active");
    }
  }

  function hideCorrespondence() {
    if (!activeCorrespondence) return;
    for (const item of elements.table.querySelectorAll(".corr-active"))
      item.classList.remove("corr-active");
    activeCorrespondence = null;
  }

  function rowIndex(scope: Element) {
    return scope instanceof HTMLElement
      ? String(Array.prototype.indexOf.call(scope.parentElement?.children ?? [], scope))
      : "all";
  }
}

function renderCoupling(elements: TraceViewElements, coupled: CoupledTrace) {
  const terminalDomainError = coupled.frames.some(hasDomainError);
  if (coupled.counterexample) {
    elements.status.textContent = `seed ${coupled.seed} - counterexample`;
    elements.status.className = "status warning";
  } else {
    elements.status.textContent = `seed ${coupled.seed} - ${coupled.ok ? (terminalDomainError ? "checked domain error" : "checked") : "failed"}`;
    elements.status.className = `status ${coupled.ok ? (terminalDomainError ? "warning" : "ok") : "error"}`;
  }
  elements.table.innerHTML =
    (coupled.counterexample ? `<p class="counterexample-label">${counterexampleLabel}</p>` : "") +
    `
    <div class="coupling-table-head">
      <span></span>
      <span>Original</span>
      <span>Symbolic</span>
      <span>Determinized</span>
    </div>
    <div class="coupling-table-body">
  ` +
    coupled.frames
      .map((frame, index, frames) => {
        const previous = index > 0 ? frames[index - 1] : null;
        const sigma = sigmaView(frame.sigma);
        const sigmaLines = Math.max(1, Math.min(4, sigma.lineCount));
        const ok = frameOk(frame);
        const domainError = hasDomainError(frame);
        return `
        <section class="coupling-row ${ok ? "" : "failed"} ${ok && domainError ? "domain-error-row" : ""}" style="--sigma-lines: ${sigmaLines}">
          <div class="step-rail">
            <span>${frame.step}</span>
            ${stepCheck(frame, coupled)}
          </div>
          ${couplingCell(frame.original, "", "original", { focusPath: changedPath(previous?.original, frame.original), valueBySymbol: frame.sampleBySymbol, valueLabel: "sampled value for" })}
          ${couplingCell(frame.symbolic, sigma.html, "symbolic", { focusPath: changedPath(previous?.symbolic, frame.symbolic) })}
          ${couplingCell(frame.determinized, "", "determinized", { focusPath: changedPath(previous?.determinized, frame.determinized), valueBySymbol: sigma.meanBySymbol, valueLabel: "mean substituted for" })}
        </section>
      `;
      })
      .join("") +
    "</div>";
}

function stepCheck(frame: Frame, coupled: CoupledTrace) {
  const ok = frameOk(frame);
  const domainError = hasDomainError(frame);
  const label = domainError && ok ? "ERR" : ok ? "OK" : "FAIL";
  const aria =
    domainError && ok
      ? "Coupling checks reached a shared domain error"
      : ok
        ? "Coupling checks passed"
        : "Coupling check failed";
  return `
    <span class="step-check ${ok ? (domainError ? "domain" : "ok") : "fail"}" tabindex="0" aria-label="${aria}">
      ${label}
      <span class="check-popover-source">
        ${checkPopoverContent(frame, coupled, ok, domainError)}
      </span>
    </span>
  `;
}

function checkPopoverContent(
  frame: Frame,
  coupled: CoupledTrace,
  ok: boolean,
  domainError: boolean,
) {
  const originalTarget = frame.originalTarget ? prettyExpr(frame.originalTarget) : "not available";
  const determinizedTarget = frame.determinizedTarget
    ? prettyExpr(frame.determinizedTarget)
    : "not available";
  return `
    <strong>${domainError && ok ? "All traces reached the same domain error at this symbolic step." : ok ? "Coupling checks passed at this symbolic step." : "Coupling check failed at this symbolic step."}</strong>
    ${domainError ? `<span class="domain-error-note">${escapeHtml(domainErrorMessage(frame))}</span>` : ""}
    <span>The source trace must match the symbolic state after sampling stored E-bindings with the same E-randomness.</span>
    <code>${escapeHtml(originalTarget)}</code>
    <span>The determinized trace must match the symbolic state after replacing stored E-bindings by their means.</span>
    <code>${escapeHtml(determinizedTarget)}</code>
    <span>Source sync: ${frame.originalOk ? `${frame.originalMicroSteps} step${frame.originalMicroSteps === 1 ? "" : "s"}` : `failed${frame.originalError ? `: ${escapeHtml(frame.originalError)}` : ""}`}</span>
    <span>Determinized sync: ${frame.determinizedOk ? `${frame.determinizedMicroSteps} step${frame.determinizedMicroSteps === 1 ? "" : "s"}` : `failed${frame.determinizedError ? `: ${escapeHtml(frame.determinizedError)}` : ""}`}</span>
    ${frame.consistencyOk === false ? `<span>Terminal consistency: failed: ${escapeHtml(frame.consistencyError)}</span>` : ""}
    ${frame.symbolicOk === false ? `<span>Symbolic next step failed: ${escapeHtml(frame.symbolicError)}</span>` : ""}
    ${coupled.counterexample ? `<em>${counterexampleLabel}</em>` : ""}
  `;
}

function couplingCell(expr: Expr, meta: string, tone: string, traceOptions: TraceOptions = {}) {
  return `
    <article class="coupling-cell ${tone}">
      <div class="sigma-strip ${meta ? "" : "blank"}">${meta || "&nbsp;"}</div>
      <pre class="code-view">${renderTraceExpr(expr, traceOptions)}</pre>
    </article>
  `;
}

function sigmaView(sigma: Binding[]) {
  if (sigma.length === 0) return { html: "", lineCount: 0, meanBySymbol: {} };
  const meanBySymbol: Record<string, number> = {};
  const lines = sigmaMeans(sigma).map(({ binding, mean, error }) => {
    meanBySymbol[binding.name] = mean;
    const args = binding.args.map((arg) => renderHighlightedText(prettyAffine(arg))).join(", ");
    return `<span class="sigma-binding corr-item" data-corr="${escapeHtml(binding.name)}" tabindex="0"><span class="sigma-definition"><span class="tok-sym">${escapeHtml(binding.name)}</span> ~ <span class="tok-dist">${distributionName(binding.kind)}</span>(${args})</span><span class="sigma-mean">E[<span class="tok-sym">${escapeHtml(binding.name)}</span>] = ${meanMarkup(binding.name, mean, error)}</span></span>`;
  });
  return { html: lines.join("\n"), lineCount: lines.length, meanBySymbol };
}

function meanMarkup(symbol: string, mean: number, error: string | null = null) {
  if (error) {
    return `<span class="sigma-mean-error" title="${escapeHtml(error)}">domain error</span>`;
  }
  const value = formatNumber(mean);
  return `<span class="corr-item sigma-mean-value" data-corr="${escapeHtml(symbol)}" title="mean substituted for ${escapeHtml(symbol)}">${escapeHtml(value)}</span>`;
}

function positionCheckPopover(portal: HTMLElement, check: Element) {
  const anchor = check.getBoundingClientRect();
  const popover = portal.getBoundingClientRect();
  const margin = 10;
  const preferredLeft = anchor.right + 10;
  const left =
    preferredLeft + popover.width <= window.innerWidth - margin
      ? preferredLeft
      : Math.max(margin, anchor.left - popover.width - 10);
  const centeredTop = anchor.top + anchor.height / 2 - popover.height / 2;
  const top = clamp(centeredTop, margin, window.innerHeight - popover.height - margin);
  portal.style.left = `${left}px`;
  portal.style.top = `${top}px`;
}

function clamp(value: number, min: number, max: number) {
  return Math.max(min, Math.min(max, value));
}

function cssEscape(value: string) {
  if (window.CSS?.escape) return window.CSS.escape(value);
  return String(value).replace(/["\\]/g, "\\$&");
}
