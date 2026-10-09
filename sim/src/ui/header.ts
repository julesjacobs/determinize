// The header's tools: Copy link copies the address of the current state, Glossary shows the
// terms the page uses, and below 600 px the tools sit behind a More button.
import { encodeShare } from "../core/share.ts";
import type { SharedStore } from "./url.ts";
import { sharedStateOf } from "./url.ts";

export interface HeaderElements {
  more: HTMLButtonElement;
  tools: HTMLElement;
  copy: HTMLButtonElement;
  copyStatus: HTMLElement;
  glossaryButton: HTMLButtonElement;
  glossary: HTMLElement;
}

/** How long the outcome of Copy link stays. */
const statusMs = 4000;

/** A disclosure: `button` shows and hides `panel`. */
function disclose(
  button: HTMLButtonElement,
  panel: HTMLElement,
  onToggle?: (open: boolean) => void,
) {
  button.addEventListener("click", () => {
    const open = button.getAttribute("aria-expanded") !== "true";
    button.setAttribute("aria-expanded", String(open));
    panel.hidden = !open;
    onToggle?.(open);
  });
}

export function mountHeader(elements: HeaderElements, store: SharedStore) {
  disclose(elements.glossaryButton, elements.glossary);
  // The tools show beside the title from 600 px; below, More shows and hides them.
  elements.tools.classList.add("collapsible");
  elements.more.addEventListener("click", () => {
    const open = elements.more.getAttribute("aria-expanded") !== "true";
    elements.more.setAttribute("aria-expanded", String(open));
    elements.tools.classList.toggle("open", open);
  });

  let timer: ReturnType<typeof setTimeout> | undefined;
  function report(message: string) {
    clearTimeout(timer);
    elements.copyStatus.textContent = message;
    timer = setTimeout(() => {
      elements.copyStatus.textContent = "";
    }, statusMs);
  }
  elements.copy.addEventListener("click", async () => {
    const hash = await encodeShare(sharedStateOf(store));
    const url = new URL(window.location.href);
    url.hash = hash;
    try {
      await navigator.clipboard.writeText(url.href);
      report("Link copied.");
    } catch {
      report("The browser didn't allow copying; the address bar holds the same link.");
    }
  });
}
