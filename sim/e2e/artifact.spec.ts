// The anonymized archive of artifact/build.py, unpacked into the directory that ARTIFACT_DIR names,
// opened from disk as a reviewer opens it: the landing page, the simulator and the documentation
// load without a server, request nothing from another host but the documentation's MathJax and
// polyfill, and link to one another and to the source pages, which mark the lines that a link
// names. artifact.yml runs these tests; without ARTIFACT_DIR they are skipped.
import { pathToFileURL } from "node:url";
import type { Page } from "@playwright/test";
import { expect, test } from "@playwright/test";

const directory = process.env.ARTIFACT_DIR;
const website = directory ? pathToFileURL(`${directory}/website/`).href : "";
const cdns = ["cdn.jsdelivr.net", "cdnjs.cloudflare.com"];

test.skip(!directory, "ARTIFACT_DIR names no unpacked archive");

/** Records the hosts that the page requests, and requests nothing from them. */
async function hosts(page: Page) {
  const requested = new Set<string>();
  await page.route(/^https?:/, (route) => {
    requested.add(new URL(route.request().url()).host);
    return route.abort();
  });
  return requested;
}

test("the landing page loads from disk and links the parts of the website", async ({ page }) => {
  const errors: string[] = [];
  page.on("pageerror", (error) => errors.push(error.message));
  const requested = await hosts(page);
  await page.goto(new URL("index.html", website).href);
  await expect(page.getByRole("heading", { level: 1 })).toHaveText("Determinize");
  const links = page.locator(".masthead .links a");
  await expect(links).toHaveText(["Simulator", "API documentation", "Source"]);
  const hrefs = await links.evaluateAll((all) => all.map((a) => (a as HTMLAnchorElement).href));
  for (const href of hrefs) {
    expect(href.startsWith(website) && href.endsWith(".html")).toBe(true);
  }
  expect([...requested]).toEqual([]);
  expect(errors).toEqual([]);
});

test("the simulator runs from disk and links back", async ({ page }) => {
  const errors: string[] = [];
  page.on("pageerror", (error) => errors.push(error.message));
  const requested = await hosts(page);
  await page.goto(new URL("index.html", website).href);
  await page.getByRole("link", { name: "Simulator", exact: true }).click();
  await expect(page).toHaveURL(/\/website\/sim\/index\.html$/);
  await page.waitForFunction(() => window.DeterminizeSim !== undefined);
  await expect
    .poll(
      () =>
        page.evaluate(async () => {
          const trace = (await window.DeterminizeSim.ready).trace.value;
          return trace.kind === "run" && trace.overview.ok;
        }),
      { timeout: 30_000 },
    )
    .toBe(true);
  const docs = page.locator('a[href^="../docs/"]').first();
  expect(await docs.evaluate((a) => (a as HTMLAnchorElement).href)).toMatch(/\/website\/docs\//);
  await page.locator('a[href="../index.html"]').first().click();
  await expect(page.getByRole("heading", { level: 1 })).toHaveText("Determinize");
  expect([...requested]).toEqual([]);
  expect(errors).toEqual([]);
});

test("the documentation opens from disk, and its source links mark the lines", async ({ page }) => {
  const requested = await hosts(page);
  await page.goto(new URL("index.html", website).href);
  await page.getByRole("link", { name: "API documentation" }).click();
  await expect(page).toHaveURL(/\/website\/docs\/Determinize\.html$/);
  await page.goto(new URL("docs/Determinize/Theorems.html", website).href);
  const source = page.locator(".gh_link a").first();
  const href = await source.getAttribute("href");
  expect(href).toMatch(/^\.\.\/\.\.\/source\/lean\/Determinize\/Theorems\.lean\.html#L\d+-L\d+$/);
  await source.click();
  await expect(page).toHaveURL(/\/website\/source\/lean\/Determinize\/Theorems\.lean\.html#L/);
  const [, first, last] = /#L(\d+)-L(\d+)$/.exec(href ?? "") ?? [];
  await expect(page.locator(".line.marked")).toHaveCount(Number(last) - Number(first) + 1);
  await expect(page.locator(`#L${first}`)).toBeInViewport();
  expect([...requested].filter((host) => !cdns.includes(host))).toEqual([]);
});

test("the source index lists the Lean files", async ({ page }) => {
  await page.goto(new URL("source/index.html", website).href);
  await page.getByRole("link", { name: "lean/Determinize/Theorems.lean" }).click();
  await expect(page.locator(".line")).not.toHaveCount(0);
});
