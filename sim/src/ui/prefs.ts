// The viewer's settings that the page remembers in localStorage: the theme and whether the
// introduction is hidden. Storage can be missing or refuse access
// (private windows, blocked site data), and the page then works without it.

const prefix = "determinize:";

export function readPref(key: string): string | null {
  try {
    return window.localStorage.getItem(prefix + key);
  } catch {
    return null;
  }
}

/** Remembers `value` under `key`, or forgets the key for null. */
export function writePref(key: string, value: string | null) {
  try {
    if (value === null) window.localStorage.removeItem(prefix + key);
    else window.localStorage.setItem(prefix + key, value);
  } catch {
    // Without storage, the setting lasts as long as the page.
  }
}
