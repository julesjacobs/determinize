// The simulator's interface as WCAG 2.2 AA asks of it, in its states: axe finds no violation in
// light or dark, Tab reaches every control with a visible focus that nothing covers, Escape and
// Tab leave the editor, every control is at least 24 × 24 px, nothing moves under
// prefers-reduced-motion, and nothing scrolls sideways at 390 px.
import { deflateRawSync } from "node:zlib";
import { AxeBuilder } from "@axe-core/playwright";
import type { Page } from "@playwright/test";
import { expect, test } from "@playwright/test";

declare global {
  interface Window {
    /** The layout shifts without recent input since the page started recording them. */
    layoutShifts: number[];
  }
}

/** The simulator's address for `source` at seed 1, with no example chosen. */
function linkTo(source: string) {
  const json = JSON.stringify({ source, seed: 1, example: "" });
  return `sim/#v1=${deflateRawSync(json).toString("base64url")}`;
}

/** Waits until neither the step table nor a batch is being computed. */
async function settled(page: Page) {
  await expect(page.locator("[aria-busy=true]")).toHaveCount(0, { timeout: 30_000 });
}

/** Runs both programs 1000 times. */
async function runBoth(page: Page) {
  await page.evaluate(async () => {
    (await window.DeterminizeSim.ready).sampleCount.value = 1000;
  });
  await page.getByRole("button", { name: "Run both", exact: true }).click();
  await settled(page);
}

async function pick(page: Page, title: string) {
  await page.getByRole("button", { name: /^Example: / }).click();
  await page
    .getByRole("dialog", { name: "Examples" })
    .getByRole("link", { name: title, exact: true })
    .click();
  await expect(page.locator("#example-title")).toHaveText(title);
  await settled(page);
}

/** The simulator's states, each reached from a freshly opened page. */
const states: { name: string; open: (page: Page) => Promise<void> }[] = [
  {
    name: "first visit",
    open: async (page) => {
      await page.goto("sim/");
      await settled(page);
    },
  },
  {
    name: "noisy product after a run",
    open: async (page) => {
      await page.goto("sim/");
      await runBoth(page);
      await page.getByRole("button", { name: "Step", exact: true }).click();
    },
  },
  {
    name: "the plot against the G draw",
    open: async (page) => {
      await page.goto("sim/");
      await runBoth(page);
      await page.getByRole("radio", { name: "Against x, the G draw" }).check();
    },
  },
  {
    name: "the symbolic state, the glossary and the annotated source",
    open: async (page) => {
      await page.goto("sim/");
      await settled(page);
      await page.getByLabel("Show symbolic state").check();
      const more = page.getByRole("button", { name: "More" });
      if (await more.isVisible()) await more.click();
      await page.getByRole("button", { name: "Glossary" }).click();
      await page.getByText("Annotated source", { exact: true }).click();
    },
  },
  {
    name: "the gallery",
    open: async (page) => {
      await page.goto("sim/");
      await settled(page);
      await page.getByRole("button", { name: /^Example: / }).click();
    },
  },
  {
    name: "the counterexample",
    open: async (page) => {
      await page.goto("sim/");
      await pick(page, "Noisy product, both draws E");
      await runBoth(page);
    },
  },
  {
    name: "a list output",
    open: async (page) => {
      await page.goto("sim/");
      await pick(page, "Gaussian random walk");
      await runBoth(page);
    },
  },
  {
    name: "a rejected program",
    open: async (page) => {
      await page.goto(linkTo("true + 1"));
    },
  },
  {
    name: "an empty program",
    open: async (page) => {
      await page.goto(linkTo(""));
    },
  },
];

for (const { name, open } of states) {
  for (const colorScheme of ["light", "dark"] as const) {
    test(`axe finds no violation: ${name}, ${colorScheme}`, async ({ page }) => {
      await page.emulateMedia({ colorScheme });
      await open(page);
      await page.evaluate(() => document.fonts.ready);
      const results = await new AxeBuilder({ page })
        .withTags(["wcag2a", "wcag2aa", "wcag21aa", "wcag22aa"])
        .analyze();
      expect(results.violations).toEqual([]);
    });
  }

  test(`nothing scrolls sideways at 390 px: ${name}`, async ({ page }) => {
    await page.setViewportSize({ width: 390, height: 844 });
    await open(page);
    const { scrollWidth, innerWidth } = await page.evaluate(() => ({
      scrollWidth: document.documentElement.scrollWidth,
      innerWidth: window.innerWidth,
    }));
    expect(scrollWidth).toBeLessThanOrEqual(innerWidth);
  });
}

/** The controls that the keyboard must reach: everything focusable that a reader can see. */
const focusable =
  'a[href], button:not([disabled]), select:not([disabled]), input:not([disabled]), summary, [tabindex="0"], .cm-content';

for (const width of [390, 1440]) {
  test(`Tab reaches every control with a visible focus that nothing covers, at ${width} px`, async ({
    page,
  }) => {
    test.setTimeout(60_000);
    await page.setViewportSize({ width, height: 844 });
    await page.goto("sim/");
    await runBoth(page);
    if (width < 600) await page.getByRole("button", { name: "More" }).click();
    const expected = await page.evaluate((selector) => {
      const shown = (element: Element) => {
        const box = element.getBoundingClientRect();
        return box.width > 0 && box.height > 0 && getComputedStyle(element).visibility !== "hidden";
      };
      // Tab stops at one radio of a group, the checked one; the arrow keys move within it.
      const tabStop = (element: Element) =>
        !(element instanceof HTMLInputElement && element.type === "radio") ||
        element.checked ||
        !document.querySelector(`input[type=radio][name="${element.name}"]:checked`);
      return [...document.querySelectorAll(selector)]
        .filter((element) => shown(element) && tabStop(element))
        .filter((element) => !element.closest("[inert], dialog:not([open])"))
        .map((element, index) => {
          element.setAttribute("data-walk", String(index));
          return String(index);
        });
    }, focusable);
    expect(expected.length).toBeGreaterThan(20);
    const reached = new Set<string>();
    await page.locator("body").focus();
    for (let i = 0; i < expected.length + 20; i++) {
      await page.keyboard.press("Tab");
      // The editor marks its focus in the next frame.
      await page.evaluate(
        () => new Promise((done) => requestAnimationFrame(() => requestAnimationFrame(done))),
      );
      const focus = await page.evaluate(() => {
        const element = document.activeElement;
        if (!element || element === document.body) return null;
        // Where the focus shows: the editor around its content, the label of a hidden radio.
        const indicator =
          element.closest(".cm-editor") ??
          (element.matches(".seg input") ? element.nextElementSibling : element);
        const style = indicator ? getComputedStyle(indicator) : null;
        const box = element.getBoundingClientRect();
        const x = Math.min(Math.max(box.left + box.width / 2, 0), window.innerWidth - 1);
        const y = Math.min(
          Math.max(box.top + Math.min(box.height / 2, 12), 0),
          window.innerHeight - 1,
        );
        const top = document.elementFromPoint(x, y);
        return {
          walk: element.getAttribute("data-walk"),
          name: `${element.tagName.toLowerCase()}#${element.id || element.textContent?.trim().slice(0, 30)}`,
          outline: style
            ? `${style.outlineStyle} ${Number.parseFloat(style.outlineWidth)}`
            : "none 0",
          covered:
            !top || !(element.contains(top) || top.contains(element) || indicator?.contains(top)),
        };
      });
      if (!focus) continue;
      expect(focus.outline, `${focus.name} shows its focus`).toMatch(/^(solid|auto) [2-9]/);
      expect(focus.covered, `${focus.name} is not covered`).toBe(false);
      if (focus.walk !== null) reached.add(focus.walk);
      if (reached.size === expected.length) break;
    }
    const missed = expected.filter((walk) => !reached.has(walk));
    const names = await Promise.all(
      missed.map((walk) =>
        page.locator(`[data-walk="${walk}"]`).evaluate((e) => e.outerHTML.slice(0, 80)),
      ),
    );
    expect(names).toEqual([]);
  });
}

test("a batch moves nothing on the page at 390 px", async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto("sim/");
  // A first batch shows the distributions; shifts count only for what is on screen, and only when
  // they don't follow an input within half a second (`hadRecentInput`), so the second batch starts
  // from a script with the band in view.
  const runBoth = () => page.evaluate(async () => (await window.DeterminizeSim.ready).runBoth());
  await settled(page);
  await runBoth();
  await settled(page);
  await page.evaluate(() => document.querySelector("#distributions")?.scrollIntoView());
  await page.evaluate(() => {
    window.layoutShifts = [];
    new PerformanceObserver((list) => {
      for (const entry of list.getEntries() as (PerformanceEntry & {
        value: number;
        hadRecentInput: boolean;
      })[]) {
        if (!entry.hadRecentInput) window.layoutShifts.push(entry.value);
      }
    }).observe({ type: "layout-shift", buffered: false });
  });
  await runBoth();
  await settled(page);
  await page.waitForTimeout(500);
  const shifts = await page.evaluate(() => window.layoutShifts);
  test.info().annotations.push({ type: "layout shifts", description: JSON.stringify(shifts) });
  expect(shifts.reduce((sum, value) => sum + value, 0)).toBeLessThan(0.01);
});

test("Escape, then Tab, leaves the editor", async ({ page }) => {
  await page.goto("sim/");
  await settled(page);
  await page.getByRole("textbox", { name: "Source program" }).click();
  await page.keyboard.press("Escape");
  await page.keyboard.press("Tab");
  expect(await page.evaluate(() => document.activeElement?.closest(".cm-editor") ?? null)).toBe(
    null,
  );
});

for (const width of [390, 1440]) {
  test(`every control is at least 24 by 24 px, at ${width} px`, async ({ page }) => {
    await page.setViewportSize({ width, height: 844 });
    await page.goto("sim/");
    await runBoth(page);
    if (width < 600) await page.getByRole("button", { name: "More" }).click();
    await page.getByRole("radio", { name: "Against x, the G draw" }).check();
    // Links within sentences are exempt (WCAG 2.5.8, "inline"); the rest are measured, and a
    // checkbox or radio by the label that it shares its target with.
    const small = await page.evaluate(() => {
      const targets = [
        ...document.querySelectorAll(
          "button, select, input:not([type=checkbox]):not([type=radio]), summary, .seg span, a.home, table.stats a",
        ),
        ...[
          ...document.querySelectorAll("input[type=checkbox], input[type=radio]:not(.seg input)"),
        ].map((input) => input.closest("label") ?? input),
      ];
      return targets
        .filter((element) => element.getBoundingClientRect().width > 0)
        .map((element) => {
          const box = element.getBoundingClientRect();
          return { name: element.outerHTML.slice(0, 60), width: box.width, height: box.height };
        })
        .filter(({ width: w, height: h }) => w < 24 || h < 24);
    });
    expect(small).toEqual([]);
  });
}

test("with reduced motion, a step changes without a transition", async ({ page }) => {
  await page.emulateMedia({ reducedMotion: "reduce" });
  await page.goto("sim/");
  await settled(page);
  await page.getByRole("button", { name: "Step", exact: true }).click();
  const motion = await page.evaluate(() => ({
    animations: document.getAnimations().length,
    transition: getComputedStyle(document.querySelector(".step") as Element).transitionDuration,
  }));
  expect(motion.animations).toBe(0);
  expect(motion.transition.split(",").every((duration) => Number.parseFloat(duration) === 0)).toBe(
    true,
  );
});

test("without reduced motion, a step cross-fades in 160 ms", async ({ page }) => {
  await page.emulateMedia({ reducedMotion: "no-preference" });
  await page.goto("sim/");
  await settled(page);
  const transition = await page.evaluate(
    () => getComputedStyle(document.querySelector(".step") as Element).transitionDuration,
  );
  expect(transition.split(",").map((duration) => duration.trim())).toContain("0.16s");
});
