// The landing page and the not-found page of the assembled site: they load, work without
// JavaScript, fit a 390 px screen, request nothing from another origin and have no axe
// violations. The landing page stays within its size budget and has its link-preview metadata,
// and the simulator links back to it.
import { existsSync } from "node:fs";
import { gzipSync } from "node:zlib";
import { AxeBuilder } from "@axe-core/playwright";
import type { Page } from "@playwright/test";
import { expect, test } from "@playwright/test";

const pages = [
  { path: "./", heading: "Determinize", figures: 2 },
  { path: "404.html", heading: "Page not found", figures: 0 },
];
const hasDocs = existsSync(new URL("../../_preview/determinize/docs", import.meta.url));

/** The URLs that loading `path` requests. */
async function load(page: Page, path: string) {
  const requested: string[] = [];
  page.on("request", (request) => requested.push(request.url()));
  const response = await page.goto(path);
  expect(response?.ok()).toBe(true);
  await page.evaluate(() => document.fonts.ready);
  return requested;
}

for (const { path, heading, figures } of pages) {
  test.describe(path, () => {
    test("loads, and requests nothing from another origin", async ({ page, baseURL }) => {
      const requested = await load(page, path);
      await expect(page.getByRole("heading", { level: 1 })).toHaveText(heading);
      const origin = new URL(baseURL ?? "").origin;
      expect(requested.filter((url) => new URL(url).origin !== origin)).toEqual([]);
    });

    test("has no horizontal scroll at 390 px", async ({ page }) => {
      await page.setViewportSize({ width: 390, height: 844 });
      await load(page, path);
      const { scrollWidth, innerWidth } = await page.evaluate(() => ({
        scrollWidth: document.documentElement.scrollWidth,
        innerWidth: window.innerWidth,
      }));
      expect(scrollWidth).toBeLessThanOrEqual(innerWidth);
    });

    for (const colorScheme of ["light", "dark"] as const) {
      test(`has no axe violations in ${colorScheme}`, async ({ page }) => {
        await page.emulateMedia({ colorScheme });
        await load(page, path);
        const results = await new AxeBuilder({ page })
          .withTags(["wcag2a", "wcag2aa", "wcag21aa", "wcag22aa"])
          .analyze();
        expect(results.violations).toEqual([]);
      });
    }
  });

  test.describe(`${path} without JavaScript`, () => {
    test.use({ javaScriptEnabled: false });

    test("shows all its content, and its links work", async ({ page, request, baseURL }) => {
      await load(page, path);
      await expect(page.getByRole("heading", { level: 1 })).toHaveText(heading);
      expect(await page.locator("script").count()).toBe(0);
      // Each figure has a wide and a narrow chart, of which CSS shows one.
      await expect(page.locator("figure")).toHaveCount(figures);
      for (const figure of await page.locator("figure").all()) {
        await expect(figure.getByRole("img")).toHaveCount(1);
      }
      const origin = new URL(baseURL ?? "").origin;
      const links = await page
        .locator("a[href]")
        .evaluateAll((anchors) => anchors.map((a) => (a as HTMLAnchorElement).href));
      const local = [...new Set(links.map((href) => href.split("#")[0]))].filter(
        (href) => new URL(href).origin === origin && (hasDocs || !href.includes("/docs/")),
      );
      expect(local.length).toBeGreaterThan(0);
      for (const href of local) {
        expect((await request.get(href)).ok(), href).toBe(true);
      }
    });
  });
}

test("the landing page stays within its size budget", async ({ page }) => {
  // Bytes as GitHub Pages sends them: text compressed, fonts as they are.
  const sizes = { html: 0, css: 0, font: 0, other: 0 };
  const counted: Promise<void>[] = [];
  page.on("response", (response) => {
    counted.push(
      response.body().then((body) => {
        const type = response.headers()["content-type"] ?? "";
        if (type.startsWith("text/html")) sizes.html += gzipSync(body).length;
        else if (type.startsWith("text/css")) sizes.css += gzipSync(body).length;
        else if (type.startsWith("font/")) sizes.font += body.length;
        else sizes.other += body.length;
      }),
    );
  });
  await load(page, "./");
  await page.waitForLoadState("networkidle");
  await Promise.all(counted);
  test.info().annotations.push({ type: "bytes", description: JSON.stringify(sizes) });
  expect(sizes.html).toBeLessThanOrEqual(18_000);
  expect(sizes.css).toBeLessThanOrEqual(8_000);
  expect(sizes.other).toBe(0);
  expect(sizes.html + sizes.css + sizes.font).toBeLessThanOrEqual(100_000);
});

test("the landing page describes itself for link previews", async ({ page, request, baseURL }) => {
  const canonical = "https://julesjacobs.com/determinize/";
  await load(page, "./");
  await expect(page.locator('link[rel="canonical"]')).toHaveAttribute("href", canonical);
  await expect(page.locator('meta[property="og:url"]')).toHaveAttribute("content", canonical);
  await expect(page.locator('meta[name="twitter:card"]')).toHaveAttribute(
    "content",
    "summary_large_image",
  );
  await expect(page.locator('meta[name^="citation_"]')).toHaveCount(0);
  const image = (await page.locator('meta[property="og:image"]').getAttribute("content")) ?? "";
  expect(image.startsWith(canonical)).toBe(true);
  const png = await (
    await request.get(new URL(image.slice(canonical.length), baseURL).href)
  ).body();
  // The PNG header's IHDR chunk holds the width and the height at bytes 16 and 20.
  expect([png.readUInt32BE(16), png.readUInt32BE(20)]).toEqual([1200, 630]);
  expect(png.length).toBeLessThanOrEqual(150_000);
});

test("the simulator links back to the landing page", async ({ page }) => {
  await page.goto("sim/");
  await page.getByRole("link", { name: "Project page" }).click();
  await expect(page).toHaveURL(/\/determinize\/$/);
  await expect(page.getByRole("heading", { level: 1 })).toHaveText("Determinize");
});
