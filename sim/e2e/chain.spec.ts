// The figure of the programs' Markov chains: shown and hidden by its button wherever a program's
// chain is a diagram, its Graphviz bundle loaded only then, drawn on the page's thread from disk;
// the "i" that says what it is; its nodes and its table's rows highlighting their sites in the
// panes; the rewards on the way; the sentences in place of a drawing; the link that carries it
// open; its layout at 390 and 1440 px, its labels' widths and its accessibility.
import { readFileSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { deflateRawSync, inflateRawSync } from "node:zlib";
import { AxeBuilder } from "@axe-core/playwright";
import type { Page } from "@playwright/test";
import { expect, test } from "@playwright/test";
import { advance, numberAdvance, numberGap, padding } from "../src/core/chain-layout.ts";
import type { Store } from "../src/ui/store.ts";

declare global {
  interface Window {
    DeterminizeSim: { ready: Promise<Store> };
  }
}

const simulator = "sim/";
const fromDisk = pathToFileURL(fileURLToPath(new URL("../dist/index.html", import.meta.url))).href;

/** How long a layout may take on CI's runners: the first starts Graphviz's WebAssembly. */
const layoutTimeout = 30_000;

const errors = new WeakMap<Page, string[]>();
test.beforeEach(({ page }) => {
  const seen: string[] = [];
  errors.set(page, seen);
  page.on("pageerror", (error) => seen.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error") seen.push(message.text());
  });
});
test.afterEach(({ page }) => {
  expect(errors.get(page)).toEqual([]);
});

/** The fragment of a link to `source` at seed 1, with `more` of the shared state. */
function fragment(source: string, more: Record<string, unknown> = {}) {
  const json = JSON.stringify({ source, seed: 1, example: "", ...more });
  return `#v1=${deflateRawSync(json).toString("base64url")}`;
}

async function pick(page: Page, title: string) {
  await page.getByRole("button", { name: /^Example: / }).click();
  await page
    .getByRole("dialog", { name: "Examples" })
    .getByRole("link", { name: title, exact: true })
    .click();
  await expect(page.locator("#example-title")).toHaveText(title);
}

const figure = (page: Page) => page.locator("#chains");

/** A sum of six coin flips, carried as an argument: a lattice of 28 nodes, under the cap. */
const lattice =
  "(rec f n => fun acc => if n < 1 then acc else f (n - 1) (acc + bernoulli[G](0.5))) 6 0";

/** Waits until the figure is open and drawn. */
async function drawn(page: Page) {
  await expect(figure(page)).toBeVisible();
  await expect(figure(page)).not.toHaveAttribute("aria-busy", { timeout: layoutTimeout });
}

const toggle = (page: Page) => page.locator("#chains-toggle");

/** Opens the figure with its button, where it isn't open, and waits for its drawings. */
async function show(page: Page) {
  await expect(toggle(page)).not.toHaveClass(/\babsent\b/, { timeout: layoutTimeout });
  if ((await toggle(page).getAttribute("aria-expanded")) !== "true") await toggle(page).click();
  await drawn(page);
}

/** The numbers of the lines that an editor highlights. */
function linkedLines(page: Page, editor: string) {
  return page
    .locator(`${editor} .cm-line`)
    .evaluateAll((lines) =>
      lines.flatMap((line, i) => (line.classList.contains("cm-linked") ? [i + 1] : [])),
    );
}

/** The text of a drawing's nodes, each as its lines. */
function nodeTexts(page: Page, column: string) {
  return page
    .locator(`${column} .node`)
    .evaluateAll((nodes) =>
      nodes.map((node) =>
        [...node.querySelectorAll("text:not(.number)")].map((text) => text.textContent),
      ),
    );
}

test("its button shows and hides both programs' chains, wherever one of them has one", async ({
  page,
}) => {
  await page.goto(simulator);
  // The noisy product has no finite model, so no chain, and the button keeps its room unseen.
  await expect(page.locator("#exact")).toBeVisible();
  await expect(page.locator("[aria-busy=true]")).toHaveCount(0, { timeout: 30_000 });
  await expect(toggle(page)).toHaveClass(/\babsent\b/);
  await expect(toggle(page)).toBeHidden();
  await expect(figure(page)).toBeHidden();
  await pick(page, "Noisy iteration");
  await expect(toggle(page)).toHaveText("Show the Markov chains");
  await expect(toggle(page)).toHaveAttribute("aria-expanded", "false");
  await expect(toggle(page)).toHaveAttribute("aria-controls", "chains");
  await expect(figure(page)).toBeHidden();
  await toggle(page).click();
  await drawn(page);
  await expect(toggle(page)).toHaveText("Hide the Markov chains");
  await expect(toggle(page)).toHaveAttribute("aria-expanded", "true");
  await expect(page.locator("#chain-source")).toHaveText(
    "The Gaussian draw on line 3 is continuous.",
  );
  await expect(page.locator("#chain-count-det")).toHaveText("68 machine states, drawn as 4");
  expect(await nodeTexts(page, "#chain-det")).toEqual([
    ["flip[G]", "line 2"],
    ["flip[G]", "line 2"],
    ["0"],
    ["1"],
  ]);
  await toggle(page).click();
  await expect(figure(page)).toBeHidden();
  await expect(toggle(page)).toHaveAttribute("aria-expanded", "false");
  // The Gaussian random walk's determinized program is a single node: no random choice remains.
  // Its source has none, for the reason that the exact rows give.
  await pick(page, "Gaussian random walk");
  await expect(page.locator("#model-det")).toHaveText("438 states");
  await show(page);
  await expect(page.locator("#chain-det")).toHaveText(
    "No random choice remains: it returns 0 with probability 1.",
  );
  await expect(page.locator("#chain-source")).toHaveText(
    "The Gaussian draw on line 22 is continuous.",
  );
  await expect(page.locator("#chains .chain-svg")).toHaveCount(0);
  await expect(page.locator(".chain-tables")).toBeHidden();

  // The showcase opens with them.
  await pick(page, "Train delay");
  await drawn(page);
  // The E draws branch only in the source.
  const modes = (column: string) =>
    page
      .locator(`${column} .node.branch`)
      .evaluateAll((nodes) => nodes.map((node) => (node.classList.contains("e") ? "E" : "G")));
  expect(await modes("#chain-source")).toEqual(["G", "E", "E"]);
  expect(await modes("#chain-det")).toEqual(["G"]);
  // Both drawings put the coin's first outcome, node 2, left of its second, node 3.
  for (const column of ["#chain-source", "#chain-det"]) {
    const [second, third] = await Promise.all(
      [1, 2].map((node) =>
        page
          .locator(`${column} .node[data-node="${node}"]`)
          .evaluate((g) => g.getBoundingClientRect().x),
      ),
    );
    expect(second, column).toBeLessThan(third);
  }
});

test("the i beside the title says what the drawings are, on hover and on a click", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Retransmission");
  await show(page);
  const info = page.getByRole("button", { name: "About the Markov chains" });
  const about = page.locator("#chains-about");
  await expect(about).toBeHidden();
  await expect(figure(page)).toHaveAccessibleDescription(/finite model .*RewardModel/);
  await info.hover();
  await expect(about).toBeVisible();
  await expect(about).toContainText("a run returns the sum of the + labels on its path");
  await expect(about.getByRole("link", { name: "RewardModel" })).toHaveAttribute(
    "href",
    /Spec\/RewardModel\/Model\.html#Determinize\.Spec\.RewardModel\.Model$/,
  );
  await page.mouse.move(0, 0);
  await expect(about).toBeHidden();
  // A click keeps it open; Escape closes it, and so does a click elsewhere.
  await info.click();
  await expect(info).toHaveAttribute("aria-expanded", "true");
  await page.mouse.move(0, 0);
  await page.waitForTimeout(400);
  await expect(about).toBeVisible();
  await page.keyboard.press("Escape");
  await expect(about).toBeHidden();
  await expect(info).toHaveAttribute("aria-expanded", "false");
  await info.click();
  await expect(about).toBeVisible();
  await page.locator("#dist-title").click();
  await expect(about).toBeHidden();
  // Its box stays within the page at 390 px.
  await page.setViewportSize({ width: 390, height: 844 });
  await info.click();
  const box = await about.boundingBox();
  expect(box && box.x >= 0 && box.x + box.width <= 390).toBe(true);
});

test("the chain bundle is loaded only when a reader first shows the chains", async ({ page }) => {
  const requested: string[] = [];
  page.on("request", (request) => requested.push(new URL(request.url()).pathname));
  const workers: string[] = [];
  page.on("worker", (worker) => workers.push(new URL(worker.url()).pathname));
  await page.goto(simulator);
  await pick(page, "Noisy iteration");
  await expect(toggle(page)).not.toHaveClass(/\babsent\b/);
  await expect(page.locator("[aria-busy=true]")).toHaveCount(0, { timeout: 30_000 });
  const bundle = (path: string) => path.endsWith("/chain-worker.js");
  expect(requested.filter(bundle)).toEqual([]);
  expect(workers.filter(bundle)).toEqual([]);
  await show(page);
  expect(workers.filter(bundle)).toEqual(["/determinize/sim/chain-worker.js"]);
  // Another program's chains are laid out by the same worker. Its gallery link, like every
  // example's but the showcase's, opens with the figure closed.
  await pick(page, "Retransmission");
  await expect(figure(page)).toBeHidden();
  await show(page);
  await expect(page.locator("#chain-det .node")).toHaveCount(8);
  expect(workers.filter(bundle)).toHaveLength(1);
});

test("from disk, the chains are drawn on the page's thread", async ({ page }) => {
  const train =
    "let express = flip(0.3) in\nlet delay = discrete(0.25, 0.5, *) in\nif express then delay else 4 * delay";
  const workers: string[] = [];
  page.on("worker", (worker) => workers.push(worker.url()));
  await page.goto(`${fromDisk}${fragment(train, { chains: true })}`);
  await drawn(page);
  await expect(page.locator("#chain-source .node")).toHaveCount(8);
  await expect(page.locator("#chain-det .node")).toHaveCount(3);
  expect(workers).toEqual([]);
});

test("a node and a table's row highlight their sites in both panes", async ({ page }) => {
  await page.goto(simulator);
  await pick(page, "Train delay");
  await drawn(page);
  const delay = page.locator('#chain-source .node[data-node="1"]');
  await delay.hover();
  await expect(delay).toHaveClass(/\blinked\b/);
  // The E draw on line 2, and in the determinized pane the mean that replaced it.
  await expect.poll(() => linkedLines(page, "#editor")).toEqual([2]);
  await expect.poll(() => linkedLines(page, "#determinized-editor")).toEqual([2]);
  // A return highlights no site.
  await page.locator('#chain-source .node[data-node="3"]').hover();
  await expect.poll(() => linkedLines(page, "#editor")).toEqual([]);
  await page.mouse.move(0, 0);
  await expect(page.locator("#chains .node.linked")).toHaveCount(0);

  // The table's rows: the coin's transition to the determinized program's return of 4.
  await page.locator(".chain-tables > summary").click();
  const row = page.locator('#chain-table-det tbody tr[data-edge="0"]');
  await expect(row).toContainText("flip[G], line 1");
  await expect(row).toContainText("returns 4");
  await row.focus();
  await expect(page.locator("#chain-det .node.linked")).toHaveCount(2);
  await expect.poll(() => linkedLines(page, "#editor")).toEqual([1]);
  await expect.poll(() => linkedLines(page, "#determinized-editor")).toEqual([1]);
  // The pointer passing over the drawing and leaving the figure returns to the focused row.
  await delay.hover();
  await expect.poll(() => linkedLines(page, "#editor")).toEqual([2]);
  await page.mouse.move(0, 0);
  await expect(page.locator("#chain-det .node.linked")).toHaveCount(2);
  await expect.poll(() => linkedLines(page, "#editor")).toEqual([1]);
});

test("the determinized program's nodes name the lines of its own pane", async ({ page }) => {
  await page.goto(simulator);
  await pick(page, "Retransmission");
  await show(page);
  const nodes = page.locator("#chain-det .node.branch");
  await expect(nodes).toHaveCount(6);
  const lines = await page.locator("#determinized-editor .cm-line").allTextContents();
  for (const where of await nodes.locator(".where").allTextContents()) {
    const line = Number(/^line (\d+)$/.exec(where)?.[1]);
    expect(lines[line - 1], where).toContain("bernoulli[G]");
  }
  // Hovering one highlights that line of the pane.
  const first = nodes.first();
  const line = Number((await first.locator(".where").textContent())?.replace("line ", ""));
  await first.hover();
  await expect.poll(() => linkedLines(page, "#determinized-editor")).toEqual([line]);
  // A node that stands for two sites names both of their lines, in each program's own pane.
  const rejection = readFileSync(
    new URL("../../examples/loops/geometric-rejection.det", import.meta.url),
    "utf8",
  );
  await page.goto(`${simulator}${fragment(rejection, { chains: true })}`);
  await drawn(page);
  const where = (column: string) => page.locator(`${column} .node.branch .where`).allTextContents();
  expect(new Set(await where("#chain-source"))).toEqual(new Set(["lines 2 or 3"]));
  const [one, two] = (await where("#chain-det"))[0].match(/\d+/g)?.map(Number) ?? [];
  const pane = await page.locator("#determinized-editor .cm-line").allTextContents();
  for (const n of [one, two]) expect(pane[n - 1]).toContain("bernoulli[G](0.5)");
  expect(new Set(await where("#chain-det")).size).toBe(1);
});

test("the figure hides where no program has a chain, and comes back open", async ({ page }) => {
  await page.goto(simulator);
  await pick(page, "Train delay");
  await drawn(page);
  const commit = (source: string) =>
    page.evaluate(async (text) => {
      const store = await window.DeterminizeSim.ready;
      store.source.value = text;
      store.commitSource();
    }, source);
  await commit("let x = uniform(0, 1) in x * x");
  await expect(page.locator("#model-det")).toHaveText("none");
  await expect(figure(page)).toBeHidden();
  await expect(toggle(page)).toHaveClass(/\babsent\b/);
  // A program that draws forever is a single node with its loop, drawn.
  await commit("1 + (rec f n => if flip(0.5) then f n else f n) 0");
  await drawn(page);
  await expect(page.locator("#chain-source .node.branch")).toHaveCount(1);
  // In the additive mode, the entry adds the outer 1.
  await expect(page.locator("#chain-source .edge-label")).toHaveText(["+1", "1"]);
});

test("the transition tables are one tab stop each, whose rows move with the arrow keys", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Train delay");
  await drawn(page);
  // The pointer rests where no node comes under it when the page scrolls.
  await page.mouse.move(0, 0);
  const summary = page.locator(".chain-tables > summary");
  await summary.focus();
  await page.keyboard.press("Enter");
  await page.keyboard.press("Tab");
  const source = page.locator("#chain-table-source tbody tr");
  await expect(source.first()).toBeFocused();
  await expect(source.first()).toContainText("start");
  await page.keyboard.press("ArrowDown");
  await expect(source.nth(1)).toBeFocused();
  await expect(page.locator('#chain-source .node.linked[data-node="0"]')).toHaveCount(1);
  await page.keyboard.press("End");
  await expect(source.last()).toBeFocused();
  await expect(source.last()).toContainText("returns 2");
  await page.keyboard.press("Home");
  await expect(source.first()).toBeFocused();
  await page.keyboard.press("ArrowDown");
  // The next tab stop is the other table, at its first row; back again, at the row left.
  await page.keyboard.press("Tab");
  await expect(page.locator("#chain-table-det tbody tr").first()).toBeFocused();
  await page.keyboard.press("Shift+Tab");
  await expect(source.nth(1)).toBeFocused();
  await page.keyboard.press("Tab");
  await page.keyboard.press("Tab");
  await expect(page.locator("#chains .node.linked")).toHaveCount(0);
});

test("the rewards on the way are drawn as + labels, which the key explains", async ({ page }) => {
  await page.goto(
    `${simulator}${fragment("3 + (if flip(0.25) then 0 - 2 else 5)", { chains: true })}`,
  );
  await drawn(page);
  expect(await nodeTexts(page, "#chain-source")).toEqual([["flip[G]", "line 1"], ["+5"], ["−2"]]);
  await expect(page.locator("#chain-source .edge-label")).toHaveText(["+3", "3/4", "1/4"]);
  await expect(page.locator("#chain-key")).toContainText("the sum of the + labels on its path");
  // Where nothing is added on the way, a return holds the whole output, and the key says nothing
  // of + labels.
  await pick(page, "Train delay");
  await drawn(page);
  expect(await nodeTexts(page, "#chain-det")).toEqual([["flip[G]", "line 1"], ["4"], ["1"]]);
  await expect(page.locator("#chain-key")).not.toContainText("+ labels");
});

test("a chain of one node is a sentence beside the other, and a chain over the cap its counts", async ({
  page,
}) => {
  // The determinized program keeps no random choice, so its chain is a single node.
  const doubled = "let d = discrete(0.25, 0.5, *) in 2 * d";
  await page.goto(`${simulator}${fragment(doubled, { chains: true })}`);
  await drawn(page);
  await expect(page.locator("#chain-det")).toHaveText(
    "No random choice remains: it returns 2 with probability 1.",
  );
  await expect(page.locator("#chain-count-det")).toHaveText(/^\d+ machine states$/);
  await expect(page.locator("#chain-source .node")).toHaveCount(4);

  // A sum of ten coin flips, carried as an argument: a node for each count of flips and sum.
  const flips =
    "(rec f n => fun acc => if n < 1 then acc else f (n - 1) (acc + bernoulli[G](0.5))) 10 0";
  await page.goto(`${simulator}${fragment(flips, { chains: true })}`);
  await drawn(page);
  await expect(page.locator("#chain-source")).toHaveText(
    "Its states contract to 66 nodes and 110 transitions, more than this figure draws (30 nodes and 60 transitions).",
  );
  await expect(page.locator("#chains .chain-svg")).toHaveCount(0);
  await expect(page.locator("#chain-key")).toBeHidden();
});

test("the page's link carries the open figure", async ({ page, context }) => {
  await page.goto(simulator);
  await pick(page, "Noisy iteration");
  await show(page);
  const shared = () =>
    page
      .evaluate(() => window.location.hash)
      .then((hash) => {
        const json = inflateRawSync(Buffer.from(hash.slice("#v1=".length), "base64url"));
        return JSON.parse(json.toString()) as Record<string, unknown>;
      });
  await expect.poll(async () => (await shared()).chains).toBe(true);
  const reopened = await context.newPage();
  await reopened.goto(page.url());
  await drawn(reopened);
  await expect(reopened.locator("#chain-det .node")).toHaveCount(4);
  await toggle(page).click();
  await expect.poll(async () => (await shared()).chains).toBeUndefined();
});

test("at 390 px the showcase's figure scrolls nowhere but in its own regions", async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto(simulator);
  await pick(page, "Train delay");
  await drawn(page);
  expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(390);
  // A wider chain scrolls in its region, which takes the focus and is named, from where runs start.
  await page.goto(`${simulator}${fragment(lattice, { chains: true })}`);
  await drawn(page);
  const region = page.locator("#chain-source .chain-scroll");
  await expect(region).toHaveAttribute("tabindex", "0");
  await expect(region).toHaveAttribute("role", "region");
  await expect(region).toHaveAttribute("aria-label", "Chain of the source");
  const view = await region.evaluate((element) => {
    const start = element.querySelector(".start")?.getBoundingClientRect();
    const box = element.getBoundingClientRect();
    return { scrolls: element.scrollWidth > element.clientWidth, start: start?.x, box };
  });
  expect(view.scrolls).toBe(true);
  expect(view.start).toBeGreaterThan(view.box.x);
  expect(view.start).toBeLessThan(view.box.x + view.box.width);
  expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(390);
});

test("at 1440 px a drawing wider than its column stacks the columns", async ({ page }) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await page.goto(simulator);
  await pick(page, "Train delay");
  await drawn(page);
  await expect(figure(page)).not.toHaveClass(/\bstacked\b/);
  await page.goto(`${simulator}${fragment(lattice, { chains: true })}`);
  await drawn(page);
  await expect(figure(page)).toHaveClass(/\bstacked\b/);
  for (const column of ["#chain-source", "#chain-det"]) {
    const region = page.locator(`${column} .chain-scroll`);
    expect(await region.evaluate((element) => element.scrollWidth <= element.clientWidth)).toBe(
      true,
    );
  }
});

test("every label fits its node or the room that Graphviz left for it", async ({ page }) => {
  await page.goto(simulator);
  // The code face itself, not a fallback of another width.
  const loaded = () =>
    page.evaluate(async () => {
      await document.fonts.ready;
      return [...document.fonts].some(
        (face) =>
          face.family.replaceAll('"', "") === "Determinize Mono" && face.status === "loaded",
      );
    });
  expect(await loaded()).toBe(true);
  for (const title of ["Train delay", "Retransmission", "Noisy iteration"]) {
    await pick(page, title);
    await show(page);
    const measured = await page.locator("#chains .chain-svg").evaluateAll(
      (svgs, room) =>
        svgs.map((svg) => {
          const found: string[] = [];
          for (const text of svg.querySelectorAll<SVGTextElement>("text")) {
            const length = text.getComputedTextLength();
            const chars = (text.textContent ?? "").length;
            const node = text.closest(".node");
            let fits: boolean;
            // An edge label is as wide as dot measured it, no narrower and no wider.
            if (!node) fits = Math.abs(length - chars * room.advance) <= 0.5;
            else if (text.classList.contains("number")) {
              fits = length <= chars * room.numberAdvance + 0.5;
            } else {
              const box = (node.querySelector(".shape") as SVGGraphicsElement).getBBox();
              const digits = (node.querySelector(".number")?.textContent ?? "").length;
              const inside =
                box.width - 2 * room.padding - digits * room.numberAdvance - room.numberGap;
              fits = length <= inside + 0.5;
            }
            if (!fits) found.push(`${text.textContent}: ${length.toFixed(2)}`);
          }
          return { texts: svg.querySelectorAll("text").length, found };
        }),
      { advance, numberAdvance, numberGap, padding: padding.x },
    );
    const what = title;
    expect(measured.length, what).toBeGreaterThan(0);
    expect(Math.min(...measured.map(({ texts }) => texts)), what).toBeGreaterThan(5);
    expect(
      measured.flatMap(({ found }) => found),
      what,
    ).toEqual([]);
  }
});

test("the open figure has no axe violations, in either theme", async ({ page }) => {
  test.slow();
  await page.goto(simulator);
  await pick(page, "Train delay");
  await drawn(page);
  await page.locator(".chain-tables > summary").click();
  for (const theme of ["light", "dark"] as const) {
    await page.locator("#theme").selectOption(theme);
    const results = await new AxeBuilder({ page })
      .include("#distributions")
      .withTags(["wcag2a", "wcag2aa", "wcag21aa", "wcag22aa"])
      .analyze();
    expect(results.violations, theme).toEqual([]);
  }
});

test("under forced colours, the drawings take the system's colours", async ({ page }) => {
  await page.emulateMedia({ forcedColors: "active" });
  await page.goto(simulator);
  await pick(page, "Train delay");
  await drawn(page);
  const paint = (selector: string, property: "fill" | "stroke") =>
    page
      .locator(selector)
      .first()
      .evaluate((element, name) => getComputedStyle(element)[name], property);
  const canvas = await page.evaluate(() => {
    const probe = document.createElement("div");
    probe.style.background = "Canvas";
    probe.style.color = "CanvasText";
    document.body.append(probe);
    const { backgroundColor, color } = getComputedStyle(probe);
    probe.remove();
    return { background: backgroundColor, text: color };
  });
  // The determinized program's solid fill and the modes' colours give way; shapes, letters and
  // weights still tell them apart.
  expect(await paint("#chain-det .shape", "fill")).toBe(canvas.background);
  expect(await paint("#chain-det .shape", "stroke")).toBe(canvas.text);
  expect(await paint("#chain-source .label.e", "fill")).toBe(canvas.text);
  expect(await paint("#chain-det .label.g", "fill")).toBe(canvas.text);
  expect(await paint("#chain-source .edge", "stroke")).toBe(canvas.text);
  const highlight = await page.evaluate(() => {
    const probe = document.createElement("div");
    probe.style.background = "Highlight";
    document.body.append(probe);
    const { backgroundColor } = getComputedStyle(probe);
    probe.remove();
    return backgroundColor;
  });
  // A hovered node takes the highlight, and a focused row an outline in it, its text unchanged;
  // the node's number stays readable on its chip.
  const numberOnChip = async () => [
    await paint("#chains .node.linked .number", "fill"),
    await paint("#chains .node.linked .number-chip", "fill"),
  ];
  await page.locator('#chain-det .node[data-node="0"]').hover();
  expect(await paint("#chain-det .node.linked .shape", "fill")).toBe(highlight);
  const [number, chip] = await numberOnChip();
  expect(number).not.toBe(chip);
  await page.mouse.move(0, 0);
  await page.locator(".chain-tables > summary").click();
  const row = page.locator("#chain-table-source tbody tr").first();
  await row.focus();
  const look = await row.evaluate((element) => {
    const style = getComputedStyle(element);
    return { outline: style.outlineColor, background: style.backgroundColor };
  });
  expect(look).toEqual({ outline: highlight, background: canvas.background });
  const [focusedNumber, focusedChip] = await numberOnChip();
  expect(focusedNumber).not.toBe(focusedChip);
});

test("a first opening shows at once what needs no layout, busy until the drawings come", async ({
  page,
}) => {
  // The chain bundle comes two seconds late, as on a slow connection.
  let release = () => {};
  const held = new Promise<void>((resolve) => {
    release = resolve;
  });
  await page.route("**/chain-worker.js*", async (route) => {
    await held;
    await route.continue();
  });
  await page.goto(simulator);
  await pick(page, "Noisy iteration");
  await expect(toggle(page)).not.toHaveClass(/\babsent\b/);
  await toggle(page).click();
  await expect(figure(page)).toHaveAttribute("aria-busy", "true");
  await expect(page.locator("#chain-source")).toHaveText(
    "The Gaussian draw on line 3 is continuous.",
  );
  await expect(page.locator("#chain-det")).toHaveText("Laying out the chain…");
  await expect(page.locator(".chain-tables")).toBeHidden();
  release();
  await drawn(page);
  await expect(page.locator("#chain-det .node")).toHaveCount(4);
  expect(await figure(page).getAttribute("aria-busy")).toBeNull();
  await expect(page.locator(".chain-tables")).toBeVisible();
});

test("a drawing that lands for a program already replaced leaves the figure busy", async ({
  page,
}) => {
  // Layout requests wait while `hold` is set, until `held` sends them.
  await page.addInitScript(() => {
    const send = Worker.prototype.postMessage;
    const scope = window as unknown as { hold?: boolean; held?: () => void };
    Worker.prototype.postMessage = function (this: Worker, data: unknown, ...rest: []) {
      if (scope.hold && (data as { type?: string })?.type === "chain-layout") {
        scope.held = () => send.call(this, data, ...rest);
        return;
      }
      return send.call(this, data, ...rest);
    } as typeof send;
  });
  await page.goto(simulator);
  await pick(page, "Train delay");
  await drawn(page);
  const commit = (source: string) =>
    page.evaluate(async (text) => {
      const store = await window.DeterminizeSim.ready;
      store.source.value = text;
      store.commitSource();
    }, source);
  await page.evaluate(() => {
    (window as unknown as { hold: boolean }).hold = true;
  });
  const replaced = "3 + (if flip(0.25) then 0 - 2 else 5)";
  await commit(replaced);
  // Its values are done, and its layout is held.
  await expect
    .poll(() =>
      page.evaluate(async () => {
        const exact = (await window.DeterminizeSim.ready).exact.value;
        return exact?.done ? exact.source : null;
      }),
    )
    .toBe(replaced);
  await expect.poll(() => page.evaluate(() => !!(window as { held?: unknown }).held)).toBe(true);
  // Each change of the figure: whether it is busy, the source's nodes, and the checked program.
  await page.evaluate(async () => {
    const store = await window.DeterminizeSim.ready;
    const seen: [boolean, number, string][] = [];
    (window as unknown as { seen: typeof seen }).seen = seen;
    const figure = document.querySelector("#chains") as HTMLElement;
    new MutationObserver(() => {
      const nodes = figure.querySelectorAll("#chain-source .node").length;
      seen.push([figure.getAttribute("aria-busy") === "true", nodes, store.checkedSource.value]);
    }).observe(figure, { attributes: true, childList: true, subtree: true });
  });
  // A newer program, and at once the held layout of the one it replaces.
  const newer = "if flip(0.5) then (if flip(0.5) then 1 else 2) else 3";
  await page.evaluate(async (text) => {
    const scope = window as unknown as { hold: boolean; held: () => void };
    const store = await window.DeterminizeSim.ready;
    store.source.value = text;
    store.commitSource();
    scope.hold = false;
    scope.held();
  }, newer);
  await drawn(page);
  await expect(page.locator("#chain-source .node")).toHaveCount(5);
  const seen = await page.evaluate(
    () => (window as unknown as { seen: [boolean, number, string][] }).seen,
  );
  // Whatever the order of the answers: a drawing other than the newer program's is busy.
  const stale = seen.filter(([, nodes, checked]) => checked === newer && nodes !== 5 && nodes > 0);
  expect(
    stale.every(([busy]) => busy),
    JSON.stringify(seen),
  ).toBe(true);
});

test("a new program's drawing replaces the last once it is laid out", async ({ page }) => {
  await page.goto(simulator);
  await pick(page, "Train delay");
  await drawn(page);
  // Each change of the figure: whether it is busy, and the nodes that the source's column draws.
  await page.evaluate(() => {
    const seen: [boolean, number][] = [];
    (window as unknown as { seen: typeof seen }).seen = seen;
    const figure = document.querySelector("#chains") as HTMLElement;
    new MutationObserver(() => {
      const nodes = figure.querySelectorAll("#chain-source .node").length;
      seen.push([figure.hasAttribute("aria-busy"), nodes]);
    }).observe(figure, { attributes: true, childList: true, subtree: true });
  });
  await page.evaluate(async () => {
    const store = await window.DeterminizeSim.ready;
    store.source.value = "3 + (if flip(0.25) then 0 - 2 else 5)";
    store.commitSource();
  });
  await expect(page.locator("#chain-source .node")).toHaveCount(3, { timeout: layoutTimeout });
  await drawn(page);
  const seen = await page.evaluate(() => (window as unknown as { seen: [boolean, number][] }).seen);
  expect(
    seen.some(([busy, nodes]) => busy && nodes === 8),
    JSON.stringify(seen),
  ).toBe(true);
  expect(
    seen.every(([, nodes]) => nodes === 8 || nodes === 3),
    JSON.stringify(seen),
  ).toBe(true);
});
