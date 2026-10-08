// The introduction: what the reader is looking at, until they hide it; the page remembers that.
import { readPref, writePref } from "./prefs.ts";

export function mountIntro(intro: HTMLElement, hide: HTMLButtonElement) {
  intro.hidden = readPref("intro") === "hidden";
  hide.addEventListener("click", () => {
    intro.hidden = true;
    writePref("intro", "hidden");
    document.querySelector<HTMLElement>("#example-button")?.focus();
  });
}
