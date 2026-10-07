// Full-page screenshots of the landing page, the not-found page and the simulator at the widths
// of the design, in light and dark, for review. They are taken only when SCREENSHOTS names a
// directory: SCREENSHOTS=DIR playwright test screenshots
import { test } from "@playwright/test";

const directory = process.env.SCREENSHOTS;
test.skip(!directory, "SCREENSHOTS names no directory");

const pages = [
  { name: "landing", path: "./", widths: [390, 1280] },
  { name: "404", path: "404.html", widths: [390, 1280] },
  { name: "sim", path: "sim/", widths: [390, 1280] },
];

for (const { name, path, widths } of pages) {
  for (const width of widths) {
    for (const colorScheme of ["light", "dark"] as const) {
      test(`${name} at ${width} px, ${colorScheme}`, async ({ page }) => {
        // The simulator draws its seed with Math.random; a fixed sequence makes runs comparable.
        await page.addInitScript(() => {
          let state = 1;
          Math.random = () => {
            state = (state * 48271) % 2147483647;
            return state / 2147483647;
          };
        });
        await page.setViewportSize({ width, height: 900 });
        await page.emulateMedia({ colorScheme, reducedMotion: "reduce" });
        await page.goto(path);
        await page.evaluate(() => document.fonts.ready);
        await page.screenshot({
          path: `${directory}/${name}-${width}-${colorScheme}.png`,
          fullPage: true,
        });
      });
    }
  }
}
