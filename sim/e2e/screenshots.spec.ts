// Full-page screenshots of the landing page, the not-found page and the simulator at the widths
// of the design, in light and dark, and of simulator states, for review. They are taken only when
// SCREENSHOTS names a directory: SCREENSHOTS=DIR playwright test screenshots
import type { Page } from "@playwright/test";
import { expect, test } from "@playwright/test";

const directory = process.env.SCREENSHOTS;
test.skip(!directory, "SCREENSHOTS names no directory");

const pages = [
  { name: "landing", path: "./", widths: [390, 1280] },
  { name: "404", path: "404.html", widths: [390, 1280] },
  { name: "sim", path: "sim/", widths: [390, 1280] },
];

/** Simulator states: an example after "Run 200". */
const states = [
  { name: "sim-noisy-product-run", example: "Noisy product" },
  { name: "sim-dungeon-run", example: "Dungeon" },
  { name: "sim-all-e-run", example: "Noisy product, both draws E" },
];

/** Loads `path` with a fixed Math.random sequence, so that the simulator's runs repeat. */
async function open(page: Page, path: string) {
  await page.addInitScript(() => {
    let state = 1;
    Math.random = () => {
      state = (state * 48271) % 2147483647;
      return state / 2147483647;
    };
  });
  await page.goto(path);
  await page.evaluate(() => document.fonts.ready);
}

for (const { name, path, widths } of pages) {
  for (const width of widths) {
    for (const colorScheme of ["light", "dark"] as const) {
      test(`${name} at ${width} px, ${colorScheme}`, async ({ page }) => {
        await page.setViewportSize({ width, height: 900 });
        await page.emulateMedia({ colorScheme, reducedMotion: "reduce" });
        await open(page, path);
        await page.screenshot({
          path: `${directory}/${name}-${width}-${colorScheme}.png`,
          fullPage: true,
        });
      });
    }
  }
}

for (const { name, example } of states) {
  test(name, async ({ page }) => {
    await page.setViewportSize({ width: 1280, height: 900 });
    await page.emulateMedia({ reducedMotion: "reduce" });
    await open(page, "sim/");
    await page.getByLabel("Example").selectOption({ label: example });
    await page.getByRole("button", { name: "Run 200" }).click();
    await expect(page.locator("[aria-busy=true]")).toHaveCount(0);
    await page.screenshot({ path: `${directory}/${name}.png`, fullPage: true });
  });
}
