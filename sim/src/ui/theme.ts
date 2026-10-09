// The theme: the system's light or dark scheme unless the viewer picks one, which sets
// data-theme on :root, where site/tokens.css turns it into color-scheme. The page remembers the
// choice; the inline script in index.html applies it before the first paint.
import type { Signal } from "@preact/signals-core";
import { effect } from "@preact/signals-core";
import { readPref, writePref } from "./prefs.ts";
import type { Theme } from "./store.ts";

/** The remembered theme, or the system's. */
export function storedTheme(): Theme {
  const stored = readPref("theme");
  return stored === "light" || stored === "dark" ? stored : "system";
}

export function mountTheme(select: HTMLSelectElement, theme: Signal<Theme>) {
  effect(() => {
    const value = theme.value;
    select.value = value;
    if (value === "system") delete document.documentElement.dataset.theme;
    else document.documentElement.dataset.theme = value;
    writePref("theme", value === "system" ? null : value);
  });
  select.addEventListener("change", () => {
    theme.value = select.value === "light" || select.value === "dark" ? select.value : "system";
  });
  // Back from the landing page, where the reader may have picked another theme, shows the page
  // from the cache; its theme then follows the stored one. A page loaded again shows the stored
  // theme too, as the select's autocomplete="off" keeps the browser from restoring it.
  window.addEventListener("pageshow", (event) => {
    if (event.persisted) theme.value = storedTheme();
  });
}
