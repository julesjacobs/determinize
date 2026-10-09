// The example gallery, opened from the header's Example button: the examples grouped by why the
// theorems don't cover them, those that the simulator's check finds no premise failing first, then
// those that fail only in floating point.
// Each group is an ordered list, because its order is a reading order. Each entry is a link that
// shares the example's program at seed 1, with one sentence, whether the paper presents it, and a
// chip that names its reason, described on hover and focus. "New program" links to an
// empty program. Below 768 px the gallery opens as a modal dialog, above as a menu under the
// button.
import { effect } from "@preact/signals-core";
import { analyze } from "../core/compiler/analyze.ts";
import type { Example } from "../core/examples.ts";
import { examples } from "../core/examples.ts";
import { encodeShare } from "../core/share.ts";
import { noRuns } from "../core/statistics.ts";
import type { Store } from "./store.ts";
import { premiseVerdict } from "./verdict.ts";

export interface GalleryElements {
  button: HTMLButtonElement;
  title: HTMLElement;
  dialog: HTMLDialogElement;
  list: HTMLElement;
  close: HTMLButtonElement;
}

/** The seed of the examples' links. */
const gallerySeed = 1;

/** Why the theorems don't cover a program, or may not: the group it falls in, its chip and what
 * the chip says on hover and focus, worded as the simulator's check words it. */
export interface Reason {
  group: string;
  chip: string;
  description: string;
  /** Whether the check finds the premise open rather than failing. */
  open?: true;
}

/** The groups of examples with a reason, in the gallery's order. */
const groups = [
  "Fails only in floating point",
  "Lean rejects the written modes",
  "The output isn't float[E]",
  "A run fails a domain check",
  "Lean rejects the program",
] as const;

/** The reason of an example that fails a premise, from its analysis as the simulator's check
 * finds it or, for a failure that only runs show, from the example, as for one that fails only in
 * floating point; none for one that the check finds failing no premise, in floating point or not. */
export function reasonOf(example: Example): Reason | null {
  const analysis = analyze(example.source);
  const type = premiseVerdict(analysis, noRuns, gallerySeed).type;
  if (!analysis.ok) {
    return analysis.counterexample
      ? {
          group: groups[1],
          chip: "modes rejected",
          description: `Type float[E]: ${type.text}. The simulator runs the counterexample, which replaces the [E] draws anyway.`,
        }
      : {
          group: groups[4],
          chip: "rejected",
          description: `Type float[E]: ${type.text}, so it doesn't run.`,
        };
  }
  if (type.status === "fails") {
    return {
      group: groups[2],
      chip: "not float[E]",
      description: `Type float[E]: ${type.text}, about which the theorems say nothing.`,
    };
  }
  if (example.fails) {
    return {
      group: groups[3],
      chip: "a run fails",
      description: `Domain safety: fails, as a run fails with “${example.fails.message}”. ${example.fails.why}`,
    };
  }
  if (example.floatFailure) {
    return {
      group: groups[0],
      chip: "underflow",
      open: true,
      description: `Domain safety: found failing in floating point at a parameter of exactly 0, with “${example.floatFailure.message}”. ${example.floatFailure.why}`,
    };
  }
  return null;
}

/** A chip naming `reason`, with its description as a tooltip below it, inside `dialog`, while the
 * pointer is over either or the chip has the focus; Escape dismisses it. */
function chip(reason: Reason, id: string, dialog: HTMLElement) {
  const wrap = document.createElement("span");
  wrap.className = "chip-wrap";
  const button = document.createElement("button");
  button.type = "button";
  button.className = reason.open ? "chip open" : "chip";
  button.textContent = reason.chip;
  button.setAttribute("aria-describedby", id);
  const tip = document.createElement("span");
  tip.id = id;
  tip.className = "chip-tip";
  tip.setAttribute("role", "tooltip");
  tip.textContent = reason.description;
  // Clicks pass through the description to the entries it covers, so the pointer keeps it open
  // by being over its box, and has a moment to cross from the chip onto it.
  let leaving: ReturnType<typeof setTimeout> | undefined;
  const stay = () => {
    clearTimeout(leaving);
    leaving = undefined;
  };
  const leave = () => {
    leaving ??= setTimeout(hide, 300);
  };
  const track = (event: PointerEvent) => {
    const box = tip.getBoundingClientRect();
    const { clientX: x, clientY: y } = event;
    if (x >= box.left && x <= box.right && y >= box.top && y <= box.bottom) stay();
    else leave();
  };
  const show = () => {
    stay();
    document.removeEventListener("pointermove", track);
    if (button.classList.contains("dismissed") || wrap.classList.contains("open")) return;
    for (const other of dialog.querySelectorAll(".chip-wrap.open")) other.classList.remove("open");
    wrap.classList.add("open");
    keepInside(tip, dialog);
  };
  const hide = () => {
    stay();
    document.removeEventListener("pointermove", track);
    wrap.classList.remove("open");
  };
  button.addEventListener("keydown", (event) => {
    if (event.key !== "Escape" || !wrap.classList.contains("open")) return;
    // Escape hides the description first; the next one closes the gallery.
    event.preventDefault();
    event.stopPropagation();
    button.classList.add("dismissed");
    hide();
  });
  wrap.addEventListener("pointerenter", show);
  button.addEventListener("focus", show);
  wrap.addEventListener("pointerleave", () => {
    button.classList.remove("dismissed");
    if (document.activeElement === button || !wrap.classList.contains("open")) return;
    leave();
    document.addEventListener("pointermove", track);
  });
  button.addEventListener("blur", () => {
    button.classList.remove("dismissed");
    if (!wrap.matches(":hover")) hide();
  });
  wrap.append(button, tip);
  return wrap;
}

/** Moves `tip` inside the content box of `bounds`: left as far as it passes the right edge, and
 * above its chip if it passes the bottom. */
function keepInside(tip: HTMLElement, bounds: HTMLElement) {
  tip.style.left = "";
  tip.classList.remove("above");
  const box = tip.getBoundingClientRect();
  const area = bounds.getBoundingClientRect();
  const style = getComputedStyle(bounds);
  const left = area.left + bounds.clientLeft + Number.parseFloat(style.paddingLeft);
  const right =
    left +
    bounds.clientWidth -
    Number.parseFloat(style.paddingLeft) -
    Number.parseFloat(style.paddingRight);
  const shift = Math.max(Math.min(0, right - box.right), left - box.left);
  if (shift !== 0) tip.style.left = `${shift}px`;
  const bottom = area.top + bounds.clientTop + bounds.clientHeight;
  if (box.bottom > bottom) tip.classList.add("above");
}

export function mountGallery(
  elements: GalleryElements,
  store: Pick<Store, "exampleId" | "source">,
) {
  const { button, dialog, list } = elements;
  const blank = document.createElement("a");
  blank.className = "button-link";
  blank.textContent = "New program";
  blank.href = "#";
  void encodeShare({ source: "", seed: gallerySeed, example: "" }).then((hash) => {
    blank.href = hash;
  });
  elements.close.before(blank);

  const entries = examples.map((example) => ({ example, reason: reasonOf(example) }));
  const headings = ["The check finds no premise failing", ...groups];
  const links: HTMLAnchorElement[] = [];
  for (const [index, heading] of headings.entries()) {
    const members = entries.filter(({ reason }) =>
      index === 0 ? reason === null : reason?.group === heading,
    );
    if (members.length === 0) continue;
    const group = document.createElement("section");
    group.className = "g-group";
    const title = document.createElement("h3");
    title.className = "g-heading";
    title.id = `g-group-${index}`;
    title.textContent = heading;
    const items = document.createElement("ol");
    items.setAttribute("aria-labelledby", title.id);
    for (const { example, reason } of members) {
      const item = document.createElement("li");
      const link = document.createElement("a");
      link.textContent = example.title;
      link.dataset.example = example.id;
      // Until its fragment is computed, a link opens the example in the current state's place.
      link.href = "#";
      const shared = { source: example.source, seed: gallerySeed, example: example.id };
      void encodeShare(example.additive ? { ...shared, additive: true } : shared).then((hash) => {
        link.href = hash;
      });
      item.append(link);
      if (reason) item.append(" ", chip(reason, `g-tip-${links.length}`, dialog));
      const line = document.createElement("span");
      line.className = "g-line";
      line.textContent = ` ${example.explanation}`;
      item.append(line);
      if (example.fromPaper) {
        const origin = document.createElement("span");
        origin.className = "g-origin";
        origin.textContent = " From the paper.";
        item.append(origin);
      }
      items.append(item);
      links.push(link);
    }
    group.append(title, items);
    list.append(group);
  }

  effect(() => {
    const example = examples.find((entry) => entry.id === store.exampleId.value);
    const edited = !example || example.source !== store.source.value;
    const blank = store.source.value.trim() === "";
    elements.title.textContent = !edited ? example.title : blank ? "New program" : "Your program";
    for (const link of links) {
      const current = !edited && link.dataset.example === example.id;
      link.parentElement?.toggleAttribute("aria-current", current);
      if (current) link.setAttribute("aria-current", "true");
      else link.removeAttribute("aria-current");
    }
  });

  const narrow = window.matchMedia("(max-width: 767px)");
  function open() {
    if (narrow.matches) dialog.showModal();
    else {
      // The menu opens under its button, wherever the header wraps it.
      dialog.style.top = `${button.getBoundingClientRect().bottom + window.scrollY + 8}px`;
      dialog.show();
    }
    button.setAttribute("aria-expanded", "true");
    (list.querySelector<HTMLElement>('a[aria-current="true"]') ?? links[0]).focus();
  }
  /** Whether closing returns the focus to the button: not when a click elsewhere closed it. */
  let refocus = true;
  function close(returnFocus = true) {
    if (!dialog.open) return;
    refocus = returnFocus;
    dialog.close();
  }
  dialog.addEventListener("close", () => {
    button.setAttribute("aria-expanded", "false");
    if (refocus) button.focus();
    refocus = true;
  });
  button.addEventListener("click", () => (dialog.open ? close() : open()));
  elements.close.addEventListener("click", () => close());
  // A menu closes with Escape, as a modal dialog does, and with a click outside it.
  dialog.addEventListener("keydown", (event) => {
    if (event.key === "Escape") {
      event.preventDefault();
      close();
    }
  });
  document.addEventListener("click", (event) => {
    if (!dialog.open || dialog.matches(":modal")) return;
    const target = event.target instanceof Node ? event.target : null;
    if (target && !dialog.contains(target) && !button.contains(target)) close(false);
  });
  // The menu covers what lies below it, so it closes too when the focus leaves it, as with Tab.
  dialog.addEventListener("focusout", (event) => {
    if (!dialog.open || dialog.matches(":modal")) return;
    const next = event.relatedTarget instanceof Node ? event.relatedTarget : null;
    if (next && !dialog.contains(next) && !button.contains(next)) close(false);
  });
  dialog.addEventListener("click", (event) => {
    // A click on the backdrop of the modal dialog lands on the dialog itself.
    if (event.target === dialog) close();
    const link = event.target instanceof Element ? event.target.closest("a") : null;
    if (!link) return;
    // The link of the state shown changes no fragment, so it opens nothing.
    if (link.getAttribute("href") === window.location.hash) event.preventDefault();
    close();
  });
}
