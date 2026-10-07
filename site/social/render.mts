// Renders card.html at 1200 × 630 px into site/social.png, the landing page's og:image. Run it in
// the site shell, whose sim/node_modules holds Playwright and whose Chromium it drives:
//
//   node site/social/render.mts
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import type * as Playwright from "@playwright/test";

// The packages are in sim/node_modules.
const require = createRequire(new URL("../../sim/package.json", import.meta.url));
const { chromium } = require("@playwright/test") as typeof Playwright;

const browser = await chromium.launch();
try {
  const page = await browser.newPage({ viewport: { width: 1200, height: 630 } });
  await page.goto(new URL("card.html", import.meta.url).href);
  await page.evaluate(() => document.fonts.ready);
  const path = fileURLToPath(new URL("../social.png", import.meta.url));
  await page.screenshot({ path });
  console.log(`Wrote ${path}.`);
} finally {
  await browser.close();
}
