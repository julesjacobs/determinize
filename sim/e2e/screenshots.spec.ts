// Full-page screenshots of the landing page, the not-found page and the simulator's states at the
// widths of the design, in light and dark, for review. They are taken only when SCREENSHOTS names
// a directory: SCREENSHOTS=DIR playwright test screenshots
import { deflateRawSync } from "node:zlib";
import type { Page } from "@playwright/test";
import { expect, test } from "@playwright/test";

const directory = process.env.SCREENSHOTS;
test.skip(!directory, "SCREENSHOTS names no directory");

const pages = [
  { name: "landing", path: "./", widths: [390, 1280] },
  { name: "404", path: "404.html", widths: [390, 1280] },
];

/** A simulator state: an example of the gallery or a program of its own, and what to do there. */
interface State {
  name: string;
  example?: string;
  source?: string;
  /** Wait for the 10 000 runs that sampling brings both programs to. */
  run?: boolean;
  /** Steps to move forward in the step table. */
  steps?: number;
  /** Before anything else: keep the introduction of a first visit. */
  firstVisit?: boolean;
  finally?: (page: Page) => Promise<void>;
}

const states: State[] = [
  { name: "noisy-product", example: "Noisy product", run: true, steps: 3 },
  {
    name: "noisy-product-against",
    example: "Noisy product",
    run: true,
    steps: 3,
    finally: (page) => page.getByRole("radio", { name: "Against x, the G draw" }).check(),
  },
  { name: "first-visit", example: "Noisy product", firstVisit: true },
  { name: "counterexample", example: "Noisy product, both draws E", run: true, steps: 1 },
  { name: "observe", example: "Observe", run: true, steps: 3 },
  { name: "recursive", example: "Recursive gamma", run: true, steps: 4 },
  { name: "random-walk", example: "Gaussian random walk", run: true },

  { name: "rejected", source: "true + 1" },
  { name: "empty", source: "" },
];

/** The simulator's address for the program `source` at seed 1, with no example chosen. */
function linkTo(source: string) {
  const json = JSON.stringify({ source, seed: 1, example: "" });
  return `sim/#v1=${deflateRawSync(json).toString("base64url")}`;
}

/** Loads `path` with a fixed Math.random sequence, so that the simulator's runs repeat. */
async function open(page: Page, path: string, firstVisit: boolean) {
  await page.addInitScript((first) => {
    let state = 1;
    Math.random = () => {
      state = (state * 48271) % 2147483647;
      return state / 2147483647;
    };
    if (!first) {
      try {
        localStorage.setItem("determinize:intro", "hidden");
      } catch {}
    }
  }, firstVisit);
  await page.goto(path);
  await page.evaluate(() => document.fonts.ready);
}

for (const { name, path, widths } of pages) {
  for (const width of widths) {
    for (const colorScheme of ["light", "dark"] as const) {
      test(`${name} at ${width} px, ${colorScheme}`, async ({ page }) => {
        await page.setViewportSize({ width, height: 900 });
        await page.emulateMedia({ colorScheme, reducedMotion: "reduce" });
        await open(page, path, true);
        await page.screenshot({
          path: `${directory}/${name}-${width}-${colorScheme}.png`,
          fullPage: true,
        });
      });
    }
  }
}

for (const state of states) {
  for (const width of [390, 768, 1440]) {
    for (const colorScheme of ["light", "dark"] as const) {
      test(`sim-${state.name} at ${width} px, ${colorScheme}`, async ({ page }) => {
        test.setTimeout(60_000);
        await page.setViewportSize({ width, height: 900 });
        await page.emulateMedia({ colorScheme, reducedMotion: "reduce" });
        const firstVisit = state.firstVisit ?? false;
        await open(page, state.source === undefined ? "sim/" : linkTo(state.source), firstVisit);
        if (state.example && state.example !== "Noisy product") {
          await page.getByRole("button", { name: /^Example: / }).click();
          await page
            .getByRole("dialog")
            .getByRole("link", { name: state.example, exact: true })
            .click();
          await expect(page.locator("#example-title")).toHaveText(state.example);
        }
        await expect(page.locator("[aria-busy]")).toHaveCount(0, { timeout: 30_000 });
        if (state.run) {
          await expect(page.locator("[aria-busy]")).toHaveCount(0, { timeout: 30_000 });
        }
        // With the scrubber's arrow keys, so that the step region follows the current row.
        const scrubber = page.locator("#scrubber");
        for (let i = 0; i < (state.steps ?? 0); i++) await scrubber.press("ArrowRight");
        await scrubber.blur();
        await state.finally?.(page);
        // No hover marks in the picture; the distributions band redraws in the next frame.
        await page.mouse.move(0, 0);
        await page.evaluate(() => new Promise((done) => requestAnimationFrame(() => done(null))));
        await page.screenshot({
          path: `${directory}/sim-${state.name}-${width}-${colorScheme}.png`,
          fullPage: true,
        });
      });
    }
  }
}
