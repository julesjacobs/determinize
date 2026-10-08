// The example gallery: an ordered list, because its order is a reading order, opened from the
// header's Example button. Each entry is a link that shares the example's program at seed 1, with
// one sentence and whether the paper presents it; "New program" links to an empty one. Below
// 768 px the list opens as a modal dialog, above as a menu under the button.
import { effect } from "@preact/signals-core";
import { examples } from "../core/examples.ts";
import { encodeShare } from "../core/share.ts";
import type { Store } from "./store.ts";

export interface GalleryElements {
  button: HTMLButtonElement;
  title: HTMLElement;
  dialog: HTMLDialogElement;
  list: HTMLOListElement;
  close: HTMLButtonElement;
}

/** The seed of the examples' links. */
const gallerySeed = 1;

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
  const links = examples.map((example) => {
    const item = document.createElement("li");
    const link = document.createElement("a");
    link.textContent = example.title;
    link.dataset.example = example.id;
    // Until its fragment is computed, a link opens the example in the current state's place.
    link.href = "#";
    void encodeShare({ source: example.source, seed: gallerySeed, example: example.id }).then(
      (hash) => {
        link.href = hash;
      },
    );
    const line = document.createElement("span");
    line.className = "g-line";
    line.textContent = ` ${example.explanation}`;
    item.append(link, line);
    if (example.fromPaper) {
      const origin = document.createElement("span");
      origin.className = "g-origin";
      origin.textContent = " From the paper.";
      item.append(origin);
    }
    list.append(item);
    return link;
  });

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
    else dialog.show();
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
