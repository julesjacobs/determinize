// The simulator in the assembled preview, where it samples in a worker, and opened from disk,
// where it samples on the page's own thread: every example runs, batches stream without long
// tasks and stop when the program changes, links restore the program, the seed and the example,
// Tab leaves the editor, and axe finds no violation.
import { readFileSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { deflateRawSync, inflateRawSync } from "node:zlib";
import { AxeBuilder } from "@axe-core/playwright";
import type { Page, Worker } from "@playwright/test";
import { expect, test } from "@playwright/test";
import type { Store } from "../src/ui/store.ts";

declare global {
  interface Window {
    /** The bundle's exports; `ready` resolves to the page's store. */
    DeterminizeSim: { ready: Promise<Store> };
    /** The durations of the long tasks since `watchLongTasks`. */
    longTasks: number[];
    /** The returned runs that the statistics show at each change, with the runs sampled then. */
    returnedTexts: string[];
    /** The verdict's text at each change, since a test started watching. */
    verdictTexts: string[];
  }
}

const simulator = "sim/";
const fromDisk = pathToFileURL(fileURLToPath(new URL("../dist/index.html", import.meta.url))).href;

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

/** Waits until no batch of runs is in progress. */
async function settled(page: Page, timeout?: number) {
  await expect(page.locator("[aria-busy=true]")).toHaveCount(0, { timeout });
}

/** Opens the example with the title `title` from the gallery. */
async function pick(page: Page, title: string) {
  await page.getByRole("button", { name: /^Example: / }).click();
  await page
    .getByRole("dialog", { name: "Examples" })
    .getByRole("link", { name: title, exact: true })
    .click();
  await expect(page.locator("#example-title")).toHaveText(title);
}

/** Sets how many runs of each program sampling brings the runs to, through the page's store. */
async function setRunCount(page: Page, count: number) {
  await page.evaluate(async (n) => {
    (await window.DeterminizeSim.ready).sampleCount.value = n;
  }, count);
  await expect(page.locator("#runs")).toHaveValue(String(count));
}

/** The number of runs of each program so far. */
function runs(page: Page) {
  return page.evaluate(
    async () => (await window.DeterminizeSim.ready).samples.value.original.summary.runs,
  );
}

/** Sets the number of runs, and waits until sampling has brought the runs to it. */
async function sampled(page: Page, count: number, timeout?: number) {
  await setRunCount(page, count);
  await expect.poll(() => runs(page), { timeout }).toBe(count);
  await settled(page, timeout);
}

/** Records the duration of every long task on the page's main thread from now on. */
async function watchLongTasks(page: Page) {
  await page.evaluate(observeLongTasks);
}

function observeLongTasks() {
  window.longTasks = [];
  new PerformanceObserver((list) => {
    for (const entry of list.getEntries()) window.longTasks.push(entry.duration);
  }).observe({ type: "longtask" });
}

/** The long tasks during each of `actions`, done one after the other, and how long the middle one
 * of their longest tasks took: a regression slows every action, a busy machine only some. */
async function longTasksPer(page: Page, actions: (() => Promise<void>)[]) {
  const each: number[][] = [];
  for (const action of actions) {
    const from = await page.evaluate(() => window.longTasks.length);
    await action();
    each.push(await page.evaluate((n) => window.longTasks.slice(n).map(Math.round), from));
  }
  const longest = each.map((tasks) => Math.max(0, ...tasks)).sort((a, b) => a - b);
  return { median: longest[Math.floor(longest.length / 2)], each: JSON.stringify(each) };
}

const status = (page: Page) => page.locator("#steps-status");

/** How long a step table may take to compute: seconds for a long run on CI's runners. */
const traceTimeout = 30_000;

/** The step table's run as the page's store has it: its seed, its steps and whether every step
 * check passed; null while there is none. */
function stepRun(page: Page) {
  return page.evaluate(async () => {
    const trace = (await window.DeterminizeSim.ready).trace.value;
    if (trace.kind !== "run") return null;
    const { seed, frameCount, ok } = trace.overview;
    return { seed, steps: frameCount - 1, ok };
  });
}

/** Waits until the step table shows a run, at `seed` if given, whose step checks all pass. */
async function passed(page: Page, seed?: number) {
  await expect
    .poll(() => stepRun(page), { timeout: traceTimeout })
    .toMatchObject(seed === undefined ? { ok: true } : { seed, ok: true });
}

for (const [where, url, worker] of [
  ["in the preview, with a worker", simulator, true],
  ["from disk, on the page's thread", fromDisk, false],
] as const) {
  test(`runs an example ${where}`, async ({ page }) => {
    const workers: string[] = [];
    page.on("worker", (started) => workers.push(started.url()));
    await page.goto(url);
    await expect
      .poll(() => stepRun(page), { timeout: traceTimeout })
      .toEqual({ seed: 1, steps: 5, ok: true });
    await expect(status(page)).toHaveText("");
    await sampled(page, 200);
    expect(await runs(page)).toBe(200);
    expect(workers.map((started) => new URL(started).pathname.split("/").at(-1)).sort()).toEqual(
      worker ? ["exact-worker.js", "trace-worker.js", "worker.js"] : [],
    );
  });
}

test("runs every example", async ({ page }) => {
  await page.goto(simulator);
  await sampled(page, 20);
  const titles = await page.locator("#gallery-list a").allTextContents();
  expect(titles.length).toBeGreaterThan(0);
  for (const title of titles) {
    await pick(page, title);
    await expect.poll(() => stepRun(page), { timeout: traceTimeout }).not.toBeNull();
    await expect(page.locator(".step").first()).toBeVisible();
    const before = await stepRun(page);
    // Runs 1 to 19 follow run 0, which the step table keeps showing.
    await expect.poll(() => runs(page)).toBe(20);
    await settled(page);
    // Every example shows its distributions: histograms, or a number's absence explained.
    await expect(page.locator("#dist-grid:visible, #list-out:visible")).toHaveCount(1);
    expect(await stepRun(page)).toEqual(before);
  }
});

test("a 200-run and a 5000-run batch leave no long task over 200 ms", async ({ page }) => {
  test.setTimeout(120_000);
  await page.goto(simulator);
  await pick(page, "Dungeon crawl");
  // The step table shows the new run once, before the batches that this test measures.
  await expect(page.locator('.step[data-step="0"] .cell-source')).toContainText("crawl");
  await watchLongTasks(page);
  await sampled(page, 200);
  await pick(page, "Noisy product");
  await sampled(page, 5000);
  const longTasks = await page.evaluate(() => window.longTasks);
  test.info().annotations.push({ type: "long tasks (ms)", description: JSON.stringify(longTasks) });
  expect(Math.max(0, ...longTasks)).toBeLessThanOrEqual(200);

  // The observer reports a task of 300 ms, so the measurement above could have seen one.
  await page.evaluate(() =>
    setTimeout(() => {
      const start = performance.now();
      while (performance.now() - start < 300);
    }),
  );
  await expect
    .poll(() => page.evaluate(() => Math.max(0, ...window.longTasks)))
    .toBeGreaterThan(250);
});

test("sampling shows a first batch soon, goes on to the run count, and further for a larger one", async ({
  page,
}) => {
  test.setTimeout(60_000);
  await page.goto(simulator);
  await pick(page, "Gaussian random walk");
  await settled(page, 30_000);
  // A new seed starts again from its first run, with a first batch of 1000 runs.
  const first = await page.evaluate(async () => {
    const store = await window.DeterminizeSim.ready;
    store.runAt(7);
    const running = store.running.value;
    return running && { end: running.end, target: running.target };
  });
  expect(first).toEqual({ end: 1000, target: 10000 });
  await expect.poll(() => runs(page), { timeout: 30_000 }).toBe(10000);
  await sampled(page, 30000, 50_000);
});

test("a heavy program stops sampling at its time budget, and goes on on request", async ({
  page,
}) => {
  test.setTimeout(60_000);
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto(simulator);
  await pick(page, "Gaussian random walk");
  const progress = page.locator("#progress-text");
  // Idle, the progress line says nothing that the run count doesn't, and keeps its line.
  await expect(page.locator("#progress")).toHaveClass(/\bidle\b/);
  expect((await progress.textContent())?.trim()).toBe("");
  expect(await progress.evaluate((line) => line.getBoundingClientRect().height)).toBeGreaterThan(
    10,
  );
  // At 390 px the head takes two rows, each with something to see: the title and Runs, then
  // Resample beside the progress.
  const tops = await page.evaluate(() =>
    ["#dist-title", ".runs", "#resample", "#progress"].map((selector) =>
      Math.round((document.querySelector(selector) as Element).getBoundingClientRect().top),
    ),
  );
  expect(Math.abs(tops[0] - tops[1]), `title and Runs: ${tops}`).toBeLessThanOrEqual(12);
  expect(tops[2], `Resample below the title: ${tops}`).toBeGreaterThan(tops[0] + 12);
  expect(Math.abs(tops[2] - tops[3]), `Resample and the progress: ${tops}`).toBeLessThanOrEqual(12);
  await setRunCount(page, 1_000_000);
  await expect(progress).toHaveText(/^Stopped after 5 s at [\d\s]+ of 1\s000\s000 runs\.$/, {
    timeout: 20_000,
  });
  await settled(page);
  // Its longest line wraps rather than being cut, at 390 px.
  expect(await progress.evaluate((text) => text.scrollWidth <= text.clientWidth)).toBe(true);
  const stopped = await runs(page);
  expect(stopped).toBeLessThan(1_000_000);
  await page.getByRole("button", { name: "Continue" }).click();
  await expect.poll(() => runs(page)).toBeGreaterThan(stopped);
  await page.getByRole("button", { name: "Stop", exact: true }).click();
  await expect(progress).toHaveText(/^Stopped at [\d\s]+ of 1\s000\s000 runs\.$/);
  await expect(page.getByRole("button", { name: "Continue" })).toBeVisible();
});

test("Resample samples from a new seed", async ({ page }) => {
  await page.goto(simulator);
  await sampled(page, 1000);
  await page.getByRole("button", { name: "Resample" }).click();
  await expect(page.locator("#seed")).not.toHaveValue("1");
  const seed = await page.locator("#seed").inputValue();
  await expect.poll(() => runs(page)).toBe(1000);
  await settled(page);
  await passed(page, Number(seed));
});

test("an edit undone within the pause goes on sampling", async ({ page }) => {
  await page.goto(simulator);
  // The random walk samples slowly enough that 100 000 runs are still sampling at the edit.
  await pick(page, "Gaussian random walk");
  await setRunCount(page, 100_000);
  await expect.poll(() => runs(page)).toBeGreaterThan(1000);
  // An edit and its undoing, then the commit that the pause would make: the program is the same,
  // so its sampling goes on.
  const after = await page.evaluate(async () => {
    const store = await window.DeterminizeSim.ready;
    const source = store.source.value;
    const before = {
      commits: store.commits.value,
      runs: store.samples.value.original.summary.runs,
    };
    store.source.value = `${source} `;
    store.source.value = source;
    store.commitSource();
    return {
      commitsRose: store.commits.value > before.commits,
      sameSource: store.samples.value.source === source,
      kept: store.samples.value.original.summary.runs >= before.runs,
      running: store.running.value !== null || store.paused.value !== null,
    };
  });
  expect(after).toEqual({ commitsRose: true, sameSource: true, kept: true, running: true });
});

test("an edit during sampling drops the old program's runs and samples the new one", async ({
  page,
}) => {
  test.setTimeout(60_000);
  await page.goto(simulator);
  await setRunCount(page, 100000);
  await expect.poll(() => runs(page)).toBeGreaterThan(20000);
  await page.locator(".cm-content").first().click();
  await page.keyboard.press("Control+End");
  await page.keyboard.type(" ");
  await expect
    .poll(() =>
      page.evaluate(async () => {
        const store = await window.DeterminizeSim.ready;
        return (
          store.samples.value.source === store.source.value && store.source.value.endsWith(" ")
        );
      }),
    )
    .toBe(true);
  // The runs are the new program's, and none of the old program's batches adds to them.
  await expect.poll(() => runs(page), { timeout: 50_000 }).toBe(100000);
  await settled(page);
  expect(await runs(page)).toBe(100000);
});

for (const title of ["Dungeon crawl", "Gaussian random walk"]) {
  test(`typing while sampling takes under 200 ms an edit, as a median: ${title}`, async ({
    page,
  }) => {
    test.setTimeout(60_000);
    await page.goto(simulator);
    await pick(page, title);
    await settled(page, 30_000);
    await watchLongTasks(page);
    await page.locator(".cm-content").first().click();
    await page.keyboard.press("Control+End");
    // Pauses longer than the analysis's delay, so that each edit samples the edited program.
    const edits = ["\n", " ", "\n", " ", "\n", " ", "\n"].map((text) => async () => {
      await page.keyboard.type(text);
      await page.waitForTimeout(800);
    });
    const { median, each } = await longTasksPer(page, edits);
    test.info().annotations.push({ type: "long tasks per edit (ms)", description: each });
    expect(median, `long tasks per edit (ms): ${each}`).toBeLessThanOrEqual(200);
    await settled(page, 30_000);
  });
}

test("a link restores the program, the seed and the example", async ({ page, context }) => {
  await page.goto(simulator);
  expect(new URL(page.url()).hash).toBe("");
  await pick(page, "Dungeon crawl");
  await page.getByRole("button", { name: "New seed" }).click();
  await page.locator(".cm-content").first().click();
  await page.keyboard.press("Control+End");
  await page.keyboard.type("\n(* shared *)");
  await expect.poll(() => new URL(page.url()).hash).toMatch(/^#v1=[A-Za-z0-9_-]+$/);
  const store = () =>
    page.evaluate(async () => {
      const { source, seed, exampleId } = await window.DeterminizeSim.ready;
      return { source: source.value, seed: seed.value, example: exampleId.value };
    });
  const state = await store();
  expect(state.source.endsWith("\n(* shared *)")).toBe(true);
  expect(state.example).toBe("paper/dungeon");
  await expect
    .poll(async () => {
      const hash = new URL(page.url()).hash.slice("#v1=".length);
      return JSON.parse(inflateRawSync(Buffer.from(hash, "base64url")).toString("utf8"));
    })
    .toEqual(state);

  const opened = await context.newPage();
  await opened.goto(page.url());
  await expect(opened.locator("#example-title")).toHaveText("Your program");
  await passed(opened, state.seed);
  expect(await opened.evaluate(async () => (await window.DeterminizeSim.ready).source.value)).toBe(
    state.source,
  );
});

test("a link navigated to is restored, though the page rewrites its fragment before the event", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Dungeon crawl");
  await expect.poll(() => new URL(page.url()).hash).toMatch(/^#v1=/);
  const link = linkTo("uniform(0, 1)", 1);
  await page.evaluate(
    (hash) => {
      const own = window.location.hash;
      window.location.hash = hash;
      // The page's delayed write of its own fragment, between the navigation and its event.
      history.replaceState(history.state, "", own);
    },
    link.slice(link.indexOf("#")),
  );
  await expect
    .poll(() =>
      page.evaluate(async () => {
        const { source, seed } = await window.DeterminizeSim.ready;
        return { source: source.value, seed: seed.value };
      }),
    )
    .toEqual({ source: "uniform(0, 1)", seed: 1 });
});

test("a variance that overflows shows as Lean's CLI prints it", async ({ page }) => {
  await page.goto(linkTo("uniform(0, 1e200)", 1));
  await sampled(page, 10);
  await expect(page.locator("#variance-source")).toHaveText(
    "unavailable (floating-point overflow)",
  );
  await expect(page.locator("#factor")).toContainText(
    "A variance is unavailable (floating-point overflow)",
  );
});

test("the determinized program shows, and the statistics give the sample sites it leaves", async ({
  page,
}) => {
  await page.goto(simulator);
  const determinized = page.getByRole("textbox", { name: "Determinized program" });
  await expect(determinized).toContainText("let y = mean_gauss(x, 1) in");
  await expect(determinized).toHaveAttribute("aria-readonly", "true");
  await sampled(page, 10);
  await expect(page.locator("#sites-text")).toHaveText(
    "2 continuous in the source, → 1 continuous determinized",
  );
  // Lean prints nothing for a program that it rejects for another reason than a mode conflict.
  await page.locator(".cm-content").first().click();
  await page.keyboard.press("Control+End");
  await page.keyboard.type(" +");
  await expect(page.locator("#source-alert")).toHaveText(
    /^Lean rejects this program at parsing\.\s*Line \d+: /,
  );
  await expect(page.locator("#determinized-empty")).toHaveText(
    "No determinized program: Lean rejects the source.",
  );
});

test("the program panes line up in every state, with what they say below their boxes", async ({
  page,
}) => {
  test.setTimeout(90_000);
  const example = (path: string) =>
    readFileSync(new URL(`../../examples/${path}.det`, import.meta.url), "utf8");
  const states = [
    ["the noisy product", linkTo(example("paper/noisy-product"), 1)],
    ["a counterexample", linkTo(example("simulator/noisy-product-all-e"), 1)],
    ["a rejected program", linkTo("let x =", 1)],
    ["an output that is not float[E]", linkTo(example("paper/gauss-random-walk"), 1)],
    ["the blank program", linkTo("", 1)],
  ];
  for (const width of [390, 768, 1440]) {
    await page.setViewportSize({ width, height: 900 });
    for (const [name, url] of states) {
      await page.goto("about:blank");
      await page.goto(url);
      await page.evaluate(async () => {
        await window.DeterminizeSim.ready;
        await document.fonts.ready;
      });
      const panes = await page.evaluate(() =>
        [...document.querySelectorAll(".pane")].map((pane) => {
          const shown = (element: Element) => element.getBoundingClientRect().height > 0;
          const box = [...pane.querySelectorAll(".cm-editor, .pane-empty")].find(shown);
          const title = (pane.querySelector(".pane-title") as Element).getBoundingClientRect();
          const rect = (box as Element).getBoundingClientRect();
          return {
            top: Math.round(rect.top),
            bottom: Math.round(rect.bottom),
            height: Math.round(rect.height),
            gap: Math.round(rect.top - title.bottom),
            warnings: [...pane.querySelectorAll(".pane-below .alert")]
              .filter(shown)
              .map((warning) => Math.round(warning.getBoundingClientRect().top)),
          };
        }),
      );
      const at = `${name} at ${width} px`;
      const [source, determinized] = panes;
      expect(determinized.height, at).toBe(source.height);
      if (width === 1440) expect(determinized.top, at).toBe(source.top);
      for (const pane of panes) {
        expect(pane.gap, `the label right above its box, ${at}`).toBeLessThanOrEqual(12);
        for (const warning of pane.warnings) {
          expect(warning, `a warning below its box, ${at}`).toBeGreaterThanOrEqual(pane.bottom);
        }
      }
      if (name === "a counterexample" || name === "a rejected program") {
        expect(panes.flatMap((pane) => pane.warnings).length, at).toBeGreaterThan(0);
      }
    }
  }
});

test("a program rejected for its modes shows its counterexample in place of the determinized one", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Noisy product, both draws E");
  await expect(page.locator("#source-alert")).toHaveText(
    /^Lean rejects this program at inference: inconsistent E\/G constraints\./,
  );
  await expect(page.locator("#determinized-title")).toHaveText("Replacing the [E] draws anyway");
  await expect(status(page)).toHaveText("");
  await expect(page.locator("#verdict-lead")).toHaveText(
    "This is the counterexample: what replacing the [E] draws anyway does, which the theorems don't cover. The simulator's check:",
  );
  // The check's lead is a line of its own, not split across two.
  await page.setViewportSize({ width: 1440, height: 900 });
  expect(
    await page
      .locator("#verdict-lead .verdict-check")
      .evaluate((check) => check.getClientRects().length),
  ).toBe(1);
  await expect(page.locator("#determinized-pane")).toHaveClass(/\buncovered\b/);
  await expect(page.getByRole("textbox", { name: "Determinized program" })).toContainText(
    "let x = mean_uniform(0, 1) in",
  );
});

test("the runs' outcomes and the run's G trace show", async ({ page }) => {
  await page.goto(simulator);
  await expect(page.locator("#g-trace")).toHaveText(
    /^\[\(uniform, [-0-9.e]+\)\], in both programs$/,
  );
  await sampled(page, 200);
  await expect(page.locator("#returned-source")).toHaveText("200 of 200");
  // Idle, the progress bar keeps its room but doesn't show.
  await expect(page.locator("#progress-bar")).toBeHidden();

  await page.goto(linkTo("1/0", 3));
  await sampled(page, 10);
  await expect(page.locator("#returned-source")).toContainText("0 of 10");
  await expect(page.locator("#returned-source")).toContainText("10 failed");
  await expect(page.locator("#first-failure")).toContainText(
    "First failure in the source: division by zero",
  );
});

test("after an edit, the statistics show the new program from its first slice of runs", async ({
  page,
}) => {
  await page.goto(simulator);
  await sampled(page, 1000);
  await page.evaluate(async () => {
    const store = await window.DeterminizeSim.ready;
    const returned = document.querySelector("#returned-source") as HTMLElement;
    window.returnedTexts = [];
    new MutationObserver(() =>
      window.returnedTexts.push(
        `${store.samples.value.original.summary.runs}: ${returned.textContent}`,
      ),
    ).observe(returned, { subtree: true, childList: true, characterData: true });
    // About a millisecond a run, so that its first batch of 1 000 takes many slices.
    store.source.value =
      "let w = (rec f n => if n <= 0 then 0 else f (n - 1)) 3000 in\nuniform(0, 1) + w";
    store.commitSource();
  });
  await expect.poll(() => page.evaluate(() => window.returnedTexts.length)).toBeGreaterThan(0);
  const [first] = await page.evaluate(() => window.returnedTexts);
  expect(Number.parseInt(first, 10), first).toBeLessThan(1000);
});

test("a program that Lean rejects has no runs", async ({ page }) => {
  await page.goto(linkTo("let x =", 1));
  await expect(page.locator("#dist-empty")).toHaveText(
    "No distributions: Lean rejects the program, so neither program runs.",
  );
  await expect(page.locator("#premise-type .finding")).toHaveText(
    "fails. Lean rejects the program at parsing.",
  );
  await expect(page.locator("#premise-safe")).toBeHidden();
  await expect(page.locator("#determinized-pane")).toHaveClass(/\buncovered\b/);
  await expect(page.getByRole("button", { name: "Resample" })).toBeDisabled();
});

test("a program the simulator fails on doesn't blame Lean", async ({ page }) => {
  // Too deeply nested for the simulator, which recurses over the program.
  await page.goto(linkTo(Array(10000).fill("1").join(" + "), 1));
  await expect(page.locator("#source-alert")).toContainText(
    "The simulator failed on this program. ./run.sh --check program.det shows whether Lean accepts it.",
  );
  await expect(page.locator("#dist-empty")).toHaveText(
    "No distributions: the simulator failed on this program, so neither program runs.",
  );
});

test("the share of returned runs links returnProbability only for a float program", async ({
  page,
}) => {
  await page.goto(simulator);
  await sampled(page, 10);
  // In the estimates; the exact values' Returned links returnMass.
  const returned = page.locator("#stats tbody").first().getByRole("link", { name: "Returned" });
  await expect(returned).toHaveCount(1);
  await page.goto(linkTo("let x = uniform(0, 1) in x < 0.5", 1));
  await sampled(page, 10);
  await expect(page.locator("#list-source")).toContainText("10 of 10 runs returned.");
  await expect(returned).toHaveCount(0);
});

test("each histogram sits under its program's column, on one axis", async ({ page }) => {
  await page.goto(simulator);
  await sampled(page, 1000);
  const layout = (width: number) =>
    page.setViewportSize({ width, height: 900 }).then(async () => {
      await expect(page.locator("#hist-det svg")).toBeVisible();
      return page.evaluate(() => {
        const box = (selector: string) =>
          (document.querySelector(selector) as Element).getBoundingClientRect();
        const ticks = (selector: string) =>
          [...document.querySelectorAll(`${selector} .tick-label`)].map((tick) => tick.textContent);
        return {
          source: box("#hist-source svg"),
          determinized: box("#hist-det svg"),
          columns: [...document.querySelectorAll(".step-head span")].map(
            (column) => column.getBoundingClientRect().left,
          ),
          ticks: [ticks("#hist-source"), ticks("#hist-det")],
        };
      });
    });
  // Under their columns wherever the step table has them side by side, stacked elsewhere.
  for (const width of [768, 1440]) {
    const wide = await layout(width);
    expect(Math.abs(wide.source.left - wide.columns[1]), `at ${width} px`).toBeLessThanOrEqual(1);
    expect(
      Math.abs(wide.determinized.left - wide.columns[4]),
      `at ${width} px`,
    ).toBeLessThanOrEqual(1);
    expect(Math.abs(wide.source.top - wide.determinized.top)).toBeLessThanOrEqual(1);
    expect(wide.source.width).toBeCloseTo(wide.determinized.width, 0);
    expect(wide.ticks[0].length).toBeGreaterThan(1);
    expect(wide.ticks[1]).toEqual(wide.ticks[0]);
  }
  const stacked = await layout(390);
  expect(stacked.determinized.top).toBeGreaterThan(stacked.source.bottom);
  expect(stacked.determinized.left).toBeCloseTo(stacked.source.left, 0);
  expect(stacked.ticks[1]).toEqual(stacked.ticks[0]);
});

test("at 1440 px the caption sits right under its chart, however tall the statistics", async ({
  page,
}) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await page.goto(simulator);
  await sampled(page, 1000);
  // The exact values' reason makes the statistics taller than the histograms.
  await expect(page.locator("#exact .exact-why")).toBeVisible();
  /** The room between the bottom of `charts` and the caption's top, the grid's row gap, and
   * whether the statistics reach below the caption. */
  const gap = (charts: string) =>
    page.evaluate((selector) => {
      const bottom = Math.max(
        ...[...document.querySelectorAll(selector)].map(
          (chart) => chart.getBoundingClientRect().bottom,
        ),
      );
      const caption = document.querySelector("#chart-caption") as Element;
      const side = document.querySelector(".dist-side") as Element;
      return {
        room: caption.getBoundingClientRect().top - bottom,
        rowGap: Number.parseFloat(getComputedStyle(caption.parentElement as Element).rowGap),
        taller: side.getBoundingClientRect().bottom > caption.getBoundingClientRect().bottom,
      };
    }, charts);
  const histograms = await gap("#hist-source, #hist-det");
  expect(histograms.taller).toBe(true);
  expect(histograms.room).toBeCloseTo(histograms.rowGap, 0);
  await page.getByRole("radio", { name: "Against x, the G draw" }).check();
  await expect(page.locator("#chart-body svg")).toBeVisible();
  const plot = await gap("#chart-body");
  expect(plot.room).toBeCloseTo(plot.rowGap, 0);
});

test("the histograms show the returned runs and count those that observe rejects", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Observe");
  await sampled(page, 1000);
  for (const slot of ["#hist-source", "#hist-det"]) {
    await expect(page.locator(`${slot} .hist-note`)).toHaveText(
      /^Shows the [\d\s]+ of 1\s000 runs that returned; [\d\s]+ were rejected by observe\.$/,
    );
  }
  await expect(page.locator("#returned-source")).toContainText("rejected by observe");
});

test("below the determinized program, the simulator's check shows nothing while nothing is found", async ({
  page,
}) => {
  await page.goto(simulator);
  await sampled(page, 1000);
  await expect(page.locator("#verdict")).toBeHidden();
  await expect(page.locator("#determinized-pane")).not.toHaveClass(/\buncovered\b/);
});

test("the check's warning lists only the premises found failing or open, with their links", async ({
  page,
}) => {
  const shown = () =>
    page
      .locator("#verdict li:visible")
      .evaluateAll((lis) => lis.map((li) => li.textContent?.replace(/\s+/g, " ").trim()));
  // A counterexample: Lean rejects the written modes, and its runs show nothing more.
  await page.goto(simulator);
  await pick(page, "Noisy product, both draws E");
  await sampled(page, 1000);
  expect(await shown()).toEqual(["Type float[E]: fails. Lean rejects the written modes."]);
  await expect(page.locator("#verdict li:visible a")).toHaveAttribute("href", /Paper\.Typed$/);
  // An output that isn't float[E].
  const walk = new URL("../../examples/paper/gauss-random-walk.det", import.meta.url);
  await page.goto(linkTo(readFileSync(walk, "utf8"), 1));
  await sampled(page, 1000);
  expect(await shown()).toEqual([
    "Type float[E]: fails. The output has type [(float[E] * float[E])].",
  ]);
  await expect(page.locator("#determinized-pane")).toHaveClass(/\buncovered\b/);
});

test("a premise that the check finds failing is a warning, with the run that witnesses it", async ({
  page,
}) => {
  // A third of the runs draw a negative variance.
  await page.goto(linkTo("let v = uniform(-0.5, 1) in\ngauss(0, v)", 1));
  await sampled(page, 1000);
  const verdict = page.locator("#verdict");
  await expect(verdict).toBeVisible();
  await expect(page.locator("#verdict-lead strong")).toHaveText("The simulator's check:");
  // The witness, whose seed Lean's CLI takes; the statistics give Lean's message.
  await expect(page.locator("#premise-safe .finding")).toHaveText(
    "fails. Run 2 failed: gaussian requires variance ≥ 0 (seed 3).",
  );
  // A negative variance is no matter of floating point.
  await expect(page.locator("#first-failure")).toContainText(
    "First failure in the source: gaussian requires variance ≥ 0",
  );
  await expect(page.locator("#first-failure")).not.toContainText("floating point");
  // The theorems don't cover the program, so the means are compared, not the variances.
  await expect(page.locator("#factor")).toHaveText(
    /^Means(?: differ)?: −?[0-9.]+ and −?[0-9.]+\.$/,
  );
  await expect(page.locator("#premise-safe")).toHaveAttribute("data-status", "fails");
  await expect(page.locator("#verdict li:visible")).toHaveCount(1);
  await expect(page.locator("#determinized-pane")).toHaveClass(/\buncovered\b/);
  // The frame is an outline, which moves nothing.
  const outline = await page
    .locator("#determinized-pane")
    .evaluate((pane) => getComputedStyle(pane).outlineColor);
  expect(outline).not.toBe("rgba(0, 0, 0, 0)");
});

test("the gallery's Gaussian bound fails domain safety, with its witness and the frame", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Gaussian bound");
  await sampled(page, 1000);
  await expect(page.locator("#verdict")).toBeVisible();
  await expect(page.locator("#premise-safe")).toHaveAttribute("data-status", "fails");
  await expect(page.locator("#premise-safe .finding")).toHaveText(
    "fails. Run 6 failed: uniform requires lower ≤ upper (seed 7).",
  );
  await expect(page.locator("#determinized-pane")).toHaveClass(/\buncovered\b/);
  // About one run in six draws a bound below 0.
  await expect(page.locator("#returned-source")).toContainText("167 failed");
});

test("a run that fails at an inexact 0 leaves domain safety open, with no frame", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Recursive gamma");
  await sampled(page, 1000);
  // An earlier gamma draw underflows to 0 in floating point, which the real-valued semantics
  // doesn't reach: the check couldn't confirm domain safety, a note rather than a warning, with
  // no frame.
  const verdict = page.locator("#verdict");
  await expect(verdict).toBeVisible();
  await expect(verdict).toHaveClass(/\bopen\b/);
  await expect(verdict).not.toHaveClass(/\balert\b/);
  await expect(page.locator("#verdict-lead")).toHaveText(
    "The simulator's check couldn't confirm a premise:",
  );
  await expect(page.locator("#verdict li:visible")).toHaveCount(1);
  await expect(page.locator("#premise-safe")).toHaveAttribute("data-status", "open");
  await expect(page.locator("#premise-safe .finding")).toHaveText(
    "open. Run 9 failed at exactly 0, in floating point: gamma requires positive shape and rate (seed 10), which may be an underflow the real-valued semantics doesn't reach.",
  );
  // The note under the statistics is muted too, and says that the run failed in floating point.
  const note = page.locator("#first-failure");
  await expect(note).toContainText(
    "First run to fail in floating point in the source: gamma requires positive shape and rate",
  );
  await expect(note).toHaveClass(/\bnote\b/);
  await expect(note).not.toHaveClass(/\balert\b/);
  await expect(page.locator("#determinized-pane")).not.toHaveClass(/\buncovered\b/);
  // The runs that returned leave out those that failed, so the means are compared, and why.
  await expect(page.locator("#factor")).toHaveText(
    /^Means(?: differ)?: [0-9.]+ and [0-9.]+\.[\d\s]+ of the source's runs failed in floating point and are left out, so its returned runs aren't comparable with the determinized program's\.$/,
  );
});

test("with a failing and an open finding, the warning leads and each says its status", async ({
  page,
}) => {
  // Lean rejects the modes, and the counterexample's runs divide by an inexact 0 half the time.
  const program =
    "let x = uniform[E](0, 1) in\nlet y = gaussian[E](x, 1) in\nlet z = uniform(0, 1) in\nif z < 0.5 then x * y else 1 / (z - z)";
  await page.goto(linkTo(program, 1));
  await sampled(page, 100);
  await expect(page.locator("#verdict")).toHaveClass(/\balert\b/);
  await expect(page.locator("#verdict-lead")).toContainText("This is the counterexample");
  await expect(page.locator("#premise-type .finding")).toHaveText(
    /^fails\. Lean rejects the written modes/,
  );
  await expect(page.locator("#premise-safe .finding")).toHaveText(
    /^open\. Run \d+ failed at exactly 0/,
  );
});

test("a switch from a counterexample to a program Lean accepts clears its warning at once", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Noisy product, both draws E");
  await sampled(page, 1000);
  await expect(page.locator("#verdict")).toBeVisible();
  // A status line that stays in the page announces which premises fail or are open.
  await expect(page.locator("#verdict-status")).toHaveText(
    "This is the counterexample. The simulator's check: Type float[E]: fails.",
  );
  // As soon as the analysis has run, before any run of the new program: the type's finding needs
  // no runs, and the earlier runs found nothing.
  const state = await page.evaluate(async () => {
    const store = await window.DeterminizeSim.ready;
    store.source.value = "let x = uniform(0, 1) in\nlet y = gaussian(x, 1) in\nx * y";
    store.commitSource();
    return {
      ok: store.analysis.value.ok,
      ready: store.samplesReady.value,
      hidden: (document.querySelector("#verdict") as HTMLElement).hidden,
      framed: document.querySelector("#determinized-pane")?.classList.contains("uncovered"),
    };
  });
  expect(state).toEqual({ ok: true, ready: false, hidden: true, framed: false });
});

test("the check's status line changes once while a program samples, not with every batch", async ({
  page,
}) => {
  await page.goto(simulator);
  await sampled(page, 1000);
  await page.evaluate(async () => {
    const store = await window.DeterminizeSim.ready;
    const status = document.querySelector("#verdict-status") as HTMLElement;
    const returns = document.querySelector("#premise-returns .finding") as HTMLElement;
    window.verdictTexts = [];
    window.returnedTexts = [];
    const watch = (element: HTMLElement, texts: string[]) =>
      new MutationObserver(() => texts.push(element.textContent ?? "")).observe(element, {
        subtree: true,
        childList: true,
        characterData: true,
      });
    watch(status, window.verdictTexts);
    watch(returns, window.returnedTexts);
    // Every run stops at the step limit, so the return probability is open and its count grows.
    store.source.value = "let f = rec f n => if n < 0 then uniform(0, 1) else f (n + 1) in f 0";
    store.commitSource();
  });
  // Across a few batches, however fast the machine: the return finding's count changes with each,
  // the status line once.
  await expect.poll(() => page.evaluate(() => window.returnedTexts.length)).toBeGreaterThan(3);
  await expect(page.locator("#premise-returns .finding")).toContainText(
    "stopped at the runtime's limits",
  );
  expect(await page.evaluate(() => window.verdictTexts)).toEqual([
    "The simulator's check couldn't confirm a premise: Positive return probability: open.",
  ]);
});

test("a new program shows none of the previous program's run-based findings", async ({ page }) => {
  await page.goto(simulator);
  await pick(page, "Gaussian bound");
  await sampled(page, 1000);
  await expect(page.locator("#premise-safe")).toBeVisible();
  // Right after the counterexample's commit, before its first runs: only the analysis's finding.
  const shown = await page.evaluate(async () => {
    const store = await window.DeterminizeSim.ready;
    store.source.value = "let x = uniform[E](0, 1) in\nlet y = gaussian[E](x, 1) in\nx * y";
    store.commitSource();
    return {
      ready: store.samplesReady.value,
      items: [...document.querySelectorAll("#verdict li")]
        .filter((li) => !(li as HTMLElement).hidden)
        .map((li) => li.id),
    };
  });
  expect(shown).toEqual({ ready: false, items: ["premise-type"] });
});

test("a literal division by zero fails domain safety, with its witness and the frame", async ({
  page,
}) => {
  // Lean's corpus has it as tests/execution/division-zero.det.
  await page.goto(linkTo("1/0", 1));
  await sampled(page, 10);
  await expect(page.locator("#verdict")).toBeVisible();
  await expect(page.locator("#premise-safe .finding")).toHaveText(
    "fails. Run 0 failed: division by zero (seed 1).",
  );
  await expect(page.locator("#premise-returns .finding")).toHaveText(
    "fails. No run returned in 10 runs, so it likely fails.",
  );
  await expect(page.locator("#determinized-pane")).toHaveClass(/\buncovered\b/);
});

test("a counterexample compares the means instead of the variances", async ({ page }) => {
  await page.goto(simulator);
  await pick(page, "Noisy product, both draws E");
  await sampled(page, 1000);
  await expect(page.locator("#factor")).toHaveText(/^Means differ: [0-9.]+ and 0\.2500\.$/);
  await expect(page.locator("#sites-text")).toHaveText(
    "2 continuous in the source, → 0 continuous in the counterexample",
  );
});

test("the check keeps its earlier verdict until runs of the new program after run 0 come in", async ({
  page,
}) => {
  await page.goto(simulator);
  await sampled(page, 1000);
  await expect(page.locator("#verdict")).toBeHidden();
  await page.evaluate(async () => {
    const store = await window.DeterminizeSim.ready;
    const verdict = document.querySelector("#verdict") as HTMLElement;
    window.verdictTexts = [];
    new MutationObserver(() => window.verdictTexts.push(verdict.textContent ?? "")).observe(
      verdict,
      { subtree: true, childList: true, characterData: true },
    );
    // A program whose runs observe rejects, slowly enough that a keystroke 5 ms after its commit
    // cancels its first batch.
    const program =
      "let w = (rec f n => if n <= 0 then 0 else f (n - 1)) 3000 in\nlet _ = observe(false) in\nw";
    store.source.value = program;
    store.commitSource();
    setTimeout(() => {
      store.source.value = `${program} `;
    }, 5);
  });
  await expect(page.locator("#verdict")).toBeVisible({ timeout: 30_000 });
  await settled(page);
  const texts = await page.evaluate(() => window.verdictTexts);
  expect(texts.length).toBeGreaterThan(0);
  expect(texts.filter((text) => /\bin 1 run\b/.test(text))).toEqual([]);
});

test("a histogram's mean label stays inside its chart, beside a clipped bar too", async ({
  page,
}) => {
  // The determinized program's runs all return 0.5, near the left of an axis to about 4: a
  // clipped bar, with its label, right of the mean.
  for (const width of [390, 1440]) {
    await page.setViewportSize({ width, height: 900 });
    await page.goto(linkTo("let x = gauss(1, 1) in\nuniform(0, x)", 1));
    await sampled(page, 1000);
    const outside = await page.locator(".chart .mean-label").evaluateAll((labels) =>
      labels
        .map((label) => {
          const box = label.getBoundingClientRect();
          const chart = (label.closest("svg") as Element).getBoundingClientRect();
          return box.left < chart.left || box.right > chart.right ? label.textContent : null;
        })
        .filter((text) => text !== null),
    );
    expect(outside, `${width} px`).toEqual([]);
  }
});

test("a histogram's labels of cut bars and of the mean don't overlap", async ({ page }) => {
  // The determinized walk returns sums of the means 1/2 and -1/2: bars at 0, 0.5 and 1 are cut,
  // and the mean lies between them.
  for (const width of [390, 1440]) {
    await page.setViewportSize({ width, height: 900 });
    await page.goto(simulator);
    await pick(page, "Asymmetric random walk");
    await sampled(page, 1000);
    const overlaps = await page.locator("#hist-det svg").evaluate((svg) => {
      const boxes = [...svg.querySelectorAll(".clip-label, .mean-label")].map((label) => ({
        text: label.textContent,
        box: label.getBoundingClientRect(),
      }));
      const found: string[] = [];
      for (const [i, a] of boxes.entries()) {
        for (const b of boxes.slice(i + 1)) {
          const apart =
            a.box.right <= b.box.left ||
            b.box.right <= a.box.left ||
            a.box.bottom <= b.box.top ||
            b.box.bottom <= a.box.top;
          if (!apart) found.push(`${a.text} / ${b.text}`);
        }
      }
      return { count: boxes.length, found };
    });
    expect(overlaps.count, `${width} px`).toBeGreaterThan(2);
    expect(overlaps.found, `${width} px`).toEqual([]);
  }
});

test("the plot against a G draw shows each run, and a click steps through it", async ({ page }) => {
  await page.goto(simulator);
  await sampled(page, 1000);
  await page.getByRole("radio", { name: "Against x, the G draw" }).check();
  await expect(page.locator(".chart.conditional")).toBeVisible();
  await expect(page.locator("#chart-caption")).toContainText("1 000 shown");
  const canvas = page.locator(".plot canvas");
  await canvas.scrollIntoViewIfNeeded();
  const box = await canvas.boundingBox();
  if (!box) throw new Error("the plot has no canvas");
  // The determinized runs lie on the curve x * x, so some run is near the middle of the x axis.
  const readout = page.locator("#plot-readout");
  let y = box.y;
  for (; y < box.y + box.height; y += 3) {
    await page.mouse.move(box.x + box.width / 2, y);
    if (await readout.textContent()) break;
  }
  await expect(readout).toHaveText(/^Run \d+: draw /);
  await page.mouse.click(box.x + box.width / 2, y);
  await expect(status(page)).toHaveText(/^Run \d+ of the runs, at seed \d+: /);
  await page.getByRole("button", { name: "Show run 0" }).click();
  await expect(status(page)).toHaveText("");
  await passed(page, 1);
});

/** The simulator's address for `source` at `seed`, with no example chosen. */
function linkTo(source: string, seed: number) {
  const json = JSON.stringify({ source, seed, example: "" });
  return `${simulator}#v1=${deflateRawSync(json).toString("base64url")}`;
}

test("a long run shows its steps a page at a time, and the controls reach every page", async ({
  page,
}) => {
  const irwinHall = new URL("../../examples/loops/irwin_hall.det", import.meta.url);
  await page.goto(linkTo(readFileSync(irwinHall, "utf8"), 3));
  await expect
    .poll(() => stepRun(page), { timeout: traceTimeout })
    .toEqual({ seed: 3, steps: 1606, ok: true });
  const note = page.locator(".page-note");
  await expect(note).toHaveText(
    /^Steps 0 to \d+ of 1606 are shown; the step controls reach the others\.$/,
  );
  const end = Number((await note.textContent())?.match(/to (\d+)/)?.[1]);
  await expect(page.locator(".step[data-step]")).toHaveCount(end + 1);
  await page.getByRole("button", { name: "End", exact: true }).click();
  await expect(page.locator("#step-of")).toHaveText("Step 1606 of 1606");
  await expect(note).toHaveText(/^Steps \d+ to 1606 of 1606 are shown/);
  const current = page.locator('.step[aria-current="step"]');
  await expect(current).toHaveAttribute("data-step", "1606");
  // The current row shows σ's latest bindings and a button for the earlier ones, which shows them
  // all in place; so do the terms of its long symbolic value.
  const bindings = await current.locator(".sigma-line").count();
  const earlier = current.locator(".sigma .expander");
  await expect(earlier).toHaveText(`… ${bindings - 12} earlier bindings`);
  await expect(current.locator(".sigma-line:visible")).toHaveCount(12);
  await earlier.focus();
  await page.keyboard.press("Enter");
  await expect(earlier).toHaveAttribute("aria-expanded", "true");
  await expect(current.locator(".sigma-line:visible")).toHaveCount(bindings);
  await expect(page.locator("#step-of")).toHaveText("Step 1606 of 1606");
  const terms = current.locator(".affine .expander").first();
  await expect(terms).toHaveText(/^\+ … \d+ more terms …$/);
  await terms.focus();
  await page.keyboard.press(" ");
  await expect(current.locator(".affine-rest").first()).toBeVisible();
  await expect(terms).toHaveText("fewer terms");
  // The keys move the step from an expander too, and the focus goes back to the region. Once the
  // row is no longer current, it shows its latest bindings again.
  await page.keyboard.press("ArrowUp");
  await expect(page.locator("#step-of")).toHaveText("Step 1605 of 1606");
  await expect(page.locator("#step-table")).toBeFocused();
  const last = page.locator('.step[data-step="1606"]');
  await expect(last.locator(".sigma-line:visible")).toHaveCount(3);
  await expect(last.locator(".affine-rest").first()).toBeHidden();
  // Screen readers hear what a row leaves out.
  await expect(last.locator(".sigma-more")).toHaveText(/^… \d+ earlier bindings not shown$/);
  // Folded, the current row fits the region.
  const fits = await page.evaluate(() => {
    const row = document.querySelector('.step[aria-current="step"]') as HTMLElement;
    return row.offsetHeight <= (document.querySelector("#step-table") as HTMLElement).clientHeight;
  });
  expect(fits).toBe(true);
});

test("a long state shows its first lines, and in the current row all of them", async ({ page }) => {
  await page.goto(simulator);
  await pick(page, "Dungeon crawl");
  await expect(page.locator('.step[data-step="0"] .cell-source')).toContainText("crawl");
  const row = page.locator('.step[data-step="1"]');
  const source = row.locator(".cell-source .state");
  await expect(source.locator(".sl:visible")).toHaveCount(3);
  await expect(source.locator(".more")).toBeVisible();
  await row.click();
  await expect(row).toHaveAttribute("aria-current", "step");
  await expect(source.locator(".more")).toBeHidden();
  const lines = await source.locator(".sl").count();
  expect(lines).toBeGreaterThan(3);
  await expect(source.locator(".sl:visible")).toHaveCount(lines);
  await expect(source).toContainText("bernoulli");
  // Once another row is current, the row shows its first lines again.
  await page.locator("#step-table").focus();
  await page.keyboard.press("ArrowDown");
  await expect(row).not.toHaveAttribute("aria-current", "step");
  await expect(source.locator(".sl:visible")).toHaveCount(3);
  await expect(source.locator(".more")).toBeVisible();
});

test("a number links to a symbol only where it stands for it", async ({ page }) => {
  await page.goto(simulator);
  await passed(page, 1);
  // At the E draw, x's G draw and v1's mean are equal; only the mean stands for v1.
  const counterparts = page.locator('.step[data-step="3"] .cell-det [data-corr="v1"]');
  await expect(counterparts).toHaveCount(1);
  await expect(counterparts).toHaveAttribute("title", "mean substituted for v1");
  // A draw's value stands for its symbol until arithmetic combines it with others; the dungeon's
  // sums of loot and its literal 0 stand for none.
  await pick(page, "Dungeon crawl");
  await expect(page.locator('.step[data-step="0"] .cell-source')).toContainText("crawl");
  const most = await page
    .locator(".step[data-step]")
    .evaluateAll((rows) =>
      Math.max(
        ...rows.map(
          (row) => row.querySelectorAll('.cell-source [title^="sampled value for"]').length,
        ),
      ),
    );
  expect(most).toBeLessThanOrEqual(1);
});

test("the region holds the flagship's run whole, and a rule shows while rows follow below", async ({
  page,
}) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await page.goto(simulator);
  await expect(page.locator('.step[data-step="5"]')).toBeAttached();
  const region = page.locator("#step-table");
  expect(await region.evaluate((element) => element.scrollHeight <= element.clientHeight)).toBe(
    true,
  );
  await expect(region).not.toHaveClass(/\bmore-below\b/);
  await pick(page, "Dungeon crawl");
  await expect(page.locator('.step[data-step="0"] .cell-source')).toContainText("crawl");
  await expect(region).toHaveClass(/\bmore-below\b/);
  // Rows render as they come into view, so the bottom is where scrolling stops.
  await expect
    .poll(() =>
      region.evaluate((element) => {
        element.scrollTo({ top: element.scrollHeight, behavior: "instant" });
        return element.classList.contains("more-below");
      }),
    )
    .toBe(false);
});

test("stepping across pages, hovering and scrubbing move neither the bar nor the page", async ({
  page,
}) => {
  test.setTimeout(60_000);
  const irwinHall = new URL("../../examples/loops/irwin_hall.det", import.meta.url);
  await page.goto(linkTo(readFileSync(irwinHall, "utf8"), 3));
  await passed(page);
  const end = Number((await page.locator(".page-note").textContent())?.match(/to (\d+)/)?.[1]);
  const scrubber = page.locator("#scrubber");
  // The reader's view: the bar at the top of the window, the region below it.
  await page.evaluate(() =>
    document.querySelector("#transport")?.scrollIntoView({ block: "start" }),
  );
  const where = () =>
    page.evaluate(() => {
      const box = (document.querySelector("#scrubber") as Element).getBoundingClientRect();
      const region = (document.querySelector("#step-table") as Element).getBoundingClientRect();
      const row = document.querySelector('.step[aria-current="step"]')?.getBoundingClientRect();
      return {
        bar: [Math.round(box.x), Math.round(box.y), Math.round(box.width)],
        scrollY: window.scrollY,
        // In view: whole, or from its top when it is taller than the region.
        rowInRegion:
          !!row &&
          row.top >= region.top - 1 &&
          (row.bottom <= region.bottom + 1 ||
            (row.height > region.height && row.top < region.top + 60)),
      };
    });
  const before = await where();
  // The current row may be on a page still on its way, or scrolling into view.
  const unmoved = async () => {
    await expect.poll(async () => (await where()).rowInRegion).toBe(true);
    const now = await where();
    expect(now.bar).toEqual(before.bar);
    expect(now.scrollY).toBe(before.scrollY);
  };
  const region = page.locator("#step-table");
  await region.evaluate((element) => (element as HTMLElement).focus({ preventScroll: true }));
  await page.keyboard.press("ArrowDown");
  await expect(page.locator("#step-of")).toHaveText("Step 1 of 1606");
  await unmoved();
  // Hovering a row links its span in the editors, which scroll by themselves if at all.
  const shown = await page.evaluate(() => {
    const region = (document.querySelector("#step-table") as Element).getBoundingClientRect();
    for (const row of document.querySelectorAll<HTMLElement>(
      ".step[data-step]:not([aria-current])",
    )) {
      const box = row.getBoundingClientRect();
      if (box.top > region.top + 40 && box.bottom < Math.min(region.bottom, window.innerHeight))
        return { step: Number(row.dataset.step), x: box.x + 24, y: box.y + box.height / 2 };
    }
    return null;
  });
  if (!shown) throw new Error("no other row in view");
  await page.mouse.move(shown.x, shown.y);
  await expect
    .poll(() => page.evaluate(async () => (await window.DeterminizeSim.ready).hoveredStep.value))
    .toBe(shown.step);
  await unmoved();
  await region.evaluate((element) => (element as HTMLElement).focus({ preventScroll: true }));
  await page.keyboard.press("End");
  await expect(page.locator("#step-of")).toHaveText("Step 1606 of 1606");
  await unmoved();
  // A step from the last step of the first page goes on onto the next page.
  await page.evaluate(async (step) => {
    (await window.DeterminizeSim.ready).currentStep.value = step;
  }, end);
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("ArrowDown");
  await expect(page.locator("#step-of")).toHaveText(`Step ${end + 2} of 1606`);
  await unmoved();
  const box = await scrubber.boundingBox();
  if (!box) throw new Error("no scrubber");
  await page.mouse.move(box.x + 4, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width * 0.6, box.y + box.height / 2, { steps: 8 });
  await page.mouse.up();
  await expect(page.locator("#step-of")).not.toHaveText(`Step ${end + 2} of 1606`);
  await unmoved();
  // A new run starts at its first step, which the region brings into view.
  await page.getByRole("button", { name: "New seed" }).click();
  await expect(page.locator("#step-of")).toHaveText(/^Step 0 of /);
  await expect.poll(async () => (await where()).rowInRegion).toBe(true);
  expect((await where()).scrollY).toBe(before.scrollY);
  // The editors keep their height, so the steps start at the same place for every program; the
  // step region takes a short run's height and stops at its cap for a long one, so the
  // distributions start higher after a short run and never further down than the cap allows. The
  // programs: a run of one step, a 3-line program, the gallery's longest example and a recursion
  // 2000 deep; the last two have long runs.
  const example = (path: string) =>
    readFileSync(new URL(`../../examples/${path}.det`, import.meta.url), "utf8");
  const longest = example("simulator/gauss-random-walk");
  const programs = [
    "uniform(0, 1)",
    example("paper/noisy-product"),
    longest,
    "let u = uniform(0, 1) in (rec f n => if n < 1 then u else 1 + f (n - 1)) 2000",
  ];
  const layout = () =>
    page.evaluate(() => {
      const top = (selector: string) =>
        Math.round(
          (document.querySelector(selector) as Element).getBoundingClientRect().top +
            window.scrollY,
        );
      const rem = Number.parseFloat(getComputedStyle(document.documentElement).fontSize);
      const bar = (document.querySelector("#transport") as HTMLElement).offsetHeight;
      return {
        steps: top("#steps"),
        distributions: top("#distributions"),
        region: (document.querySelector("#step-table") as HTMLElement).offsetHeight,
        cap: Math.max(12 * rem, Math.min(40 * rem, window.innerHeight - bar - 3 * rem)),
      };
    });
  for (const width of [390, 1440]) {
    await page.setViewportSize({ width, height: 844 });
    const shown: Awaited<ReturnType<typeof layout>>[] = [];
    for (const program of programs) {
      await page.goto(linkTo(program, 1));
      await expect(page.locator("[aria-busy=true]")).toHaveCount(0, { timeout: 30_000 });
      await expect.poll(() => stepRun(page), { timeout: traceTimeout }).toMatchObject({ seed: 1 });
      shown.push(await layout());
      if (program !== longest) continue;
      // Stepping with the arrow keys scrolls the source editor to the lines the steps reduce,
      // never the page.
      const followed = await page.evaluate(async () => {
        const region = document.querySelector("#step-table") as HTMLElement;
        const scroller = document.querySelector("#editor .cm-scroller") as Element;
        region.focus({ preventScroll: true });
        const scrollY = window.scrollY;
        let most = 0;
        for (let step = 0; step < 80; step++) {
          region.dispatchEvent(new KeyboardEvent("keydown", { key: "ArrowDown", bubbles: true }));
          await new Promise((done) => requestAnimationFrame(() => requestAnimationFrame(done)));
          if (window.scrollY !== scrollY) return { most, pageScrolled: true };
          most = Math.max(most, scroller.scrollTop);
        }
        return { most, pageScrolled: false };
      });
      expect(followed.pageScrolled, `at ${width} px`).toBe(false);
      expect(followed.most, `at ${width} px`).toBeGreaterThan(0);
    }
    test
      .info()
      .annotations.push({ type: `layout at ${width} px`, description: JSON.stringify(shown) });
    const short = shown[0];
    const long = shown.slice(2);
    for (const run of shown) {
      expect(Math.abs(run.steps - short.steps), `steps at ${width} px`).toBeLessThanOrEqual(4);
      expect(run.region, `the region at ${width} px`).toBeLessThanOrEqual(run.cap + 1);
    }
    for (const run of long) {
      expect(
        Math.abs(run.region - run.cap),
        `a long run's region at ${width} px`,
      ).toBeLessThanOrEqual(1);
      expect(short.region, `a short run's region at ${width} px`).toBeLessThan(run.region);
      expect(short.distributions, `distributions at ${width} px`).toBeLessThan(run.distributions);
    }
  }
});

test("an edit and its new run leave the source editor at the caret", async ({ page }) => {
  const walk = new URL("../../examples/paper/gauss-random-walk.det", import.meta.url);
  await page.goto(linkTo(readFileSync(walk, "utf8"), 1));
  await passed(page);
  const source = page.getByRole("textbox", { name: "Source program" });
  await source.click();
  await page.keyboard.press("Control+End");
  await page.keyboard.type(" ");
  await page.mouse.move(0, 0);
  const scroller = page.locator("#editor .cm-scroller");
  const atCaret = await scroller.evaluate((element) => element.scrollTop);
  expect(atCaret).toBeGreaterThan(0);
  // The edit is checked and run again, which moves the highlight to the new run's first step.
  await expect
    .poll(() =>
      page.evaluate(async () =>
        (await window.DeterminizeSim.ready).checkedSource.value.endsWith(" "),
      ),
    )
    .toBe(true);
  await expect(page.locator("[aria-busy=true]")).toHaveCount(0, { timeout: 30_000 });
  await passed(page);
  expect(await scroller.evaluate((element) => element.scrollTop)).toBe(atCaret);
});

test("hovering a span in the source marks the rows that reduce it", async ({ page }) => {
  await page.goto(simulator);
  await passed(page);
  const scrollY = await page.evaluate(() => window.scrollY);
  const lines = page.getByRole("textbox", { name: "Source program" }).locator(".cm-line");
  // The x of `x * y`: the smallest span that a step reduces around it is the product (step 5).
  const product = await lines.nth(2).boundingBox();
  if (!product) throw new Error("no third line");
  await page.mouse.move(product.x + 3, product.y + product.height / 2);
  await expect(page.locator(".step.linked")).toHaveAttribute("data-step", "5");
  // The first `let` is reduced by step 2, which binds x.
  const first = await lines.nth(0).boundingBox();
  if (!first) throw new Error("no first line");
  await page.mouse.move(first.x + 3, first.y + first.height / 2);
  await expect(page.locator(".step.linked")).toHaveAttribute("data-step", "2");
  expect(await page.evaluate(() => window.scrollY)).toBe(scrollY);
});

test("hovering a node in the determinized program marks the rows that reduce it", async ({
  page,
}) => {
  await page.goto(simulator);
  await passed(page);
  const scrollY = await page.evaluate(() => window.scrollY);
  const lines = page.getByRole("textbox", { name: "Determinized program" }).locator(".cm-line");
  // The x of `x * y`, not a sample site: the smallest span that a step reduces around it is the
  // product (step 5).
  const product = await lines.nth(2).boundingBox();
  if (!product) throw new Error("no third line");
  await page.mouse.move(product.x + 3, product.y + product.height / 2);
  await expect(page.locator(".step.linked")).toHaveAttribute("data-step", "5");
  expect(await page.evaluate(() => window.scrollY)).toBe(scrollY);
});

test("a run that doesn't end stops at the step table's limit", async ({ page }) => {
  await page.goto(linkTo("(rec f x => f x) ()", 1));
  await expect(status(page)).toHaveText(
    "Seed 1: the table stops after 20000 steps; Lean's fuel may still let the run return.",
  );
});

test("paging through a long run with a large σ takes under 200 ms a page, as a median", async ({
  page,
}) => {
  test.setTimeout(90_000);
  const program =
    "let f = rec f n => if n <= 0 then 0 else gauss[E](0, 1) + uniform(0, 1) + f (n - 1) in f 300";
  await page.goto(linkTo(program, 1));
  await passed(page, 1);
  await settled(page, 60_000);
  await page.locator("#step-table").focus();
  await page.keyboard.press("End");
  await expect(page.locator("#steps")).not.toHaveAttribute("aria-busy", "true");
  await watchLongTasks(page);
  const pages = Array.from({ length: 10 }, () => async () => {
    await page.keyboard.press("PageUp");
    await page.waitForTimeout(200);
  });
  const { median, each } = await longTasksPer(page, pages);
  test.info().annotations.push({ type: "long tasks per page (ms)", description: each });
  expect(median, `long tasks per page (ms): ${each}`).toBeLessThanOrEqual(200);
});

test("a deep recursion stops where the step table grows too large, without a long task", async ({
  page,
}) => {
  const deep = "let u = uniform(0, 1) in (rec f n => if n < 1 then u else 1 + f (n - 1)) 2000";
  // From the start of the page, which opens with this program.
  await page.addInitScript(observeLongTasks);
  await page.goto(linkTo(deep, 1));
  // The step table's worker computes the table up to its size bound, which takes about 2 s.
  await expect(status(page)).toHaveText(
    /^Seed 1: the table stops after \d+ steps; its states grew too large to show\.$/,
    { timeout: 20_000 },
  );
  await expect(page.getByRole("button", { name: "Resample" })).toBeEnabled();
  const longTasks = await page.evaluate(() => window.longTasks);
  test.info().annotations.push({ type: "long tasks (ms)", description: JSON.stringify(longTasks) });
  expect(Math.max(0, ...longTasks)).toBeLessThanOrEqual(200);
});

test("a newer program replaces the step table's computation in flight", async ({ page }) => {
  const deep = "let u = uniform(0, 1) in (rec f n => if n < 1 then u else 1 + f (n - 1)) 2000";
  const workers: string[] = [];
  page.on("worker", (started) => workers.push(new URL(started.url()).pathname));
  await page.goto(linkTo(deep, 1));
  // In one task of the page, so that the deep recursion, which takes its worker about 2 s, is
  // still being computed when the newer program arrives.
  await page.evaluate(async () => {
    const store = await window.DeterminizeSim.ready;
    if (store.trace.value.kind !== "computing") throw new Error("the deep recursion is computed");
    store.source.value = "1 + 2";
    store.commitSource();
  });
  await expect
    .poll(() => stepRun(page), { timeout: traceTimeout })
    .toEqual({ seed: 1, steps: 1, ok: true });
  // The worker that computed the deep recursion was terminated and replaced.
  expect(workers.filter((path) => path.endsWith("/trace-worker.js"))).toHaveLength(2);
});

/** Both programs' exact values as the page's store has them: their outcomes' kinds once both
 * explorations have ended, else null. */
function exactKinds(page: Page) {
  return page.evaluate(async () => {
    const exact = (await window.DeterminizeSim.ready).exact.value;
    if (!exact?.done) return null;
    return [exact.programs.source.kind, exact.programs.determinized.kind];
  });
}

test("exploring the dungeon in its worker to Lean's state limit leaves no long task, as a median", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Dungeon crawl");
  await sampled(page, 200);
  await expect.poll(() => exactKinds(page), { timeout: 30_000 }).toEqual(["limit", "limit"]);
  const limits = await page.evaluate(async () => {
    const { programs } = (await window.DeterminizeSim.ready).exact.value ?? {};
    return [programs?.source, programs?.determinized].map((state) =>
      state?.kind === "limit" ? [state.limit, state.discovered] : null,
    );
  });
  expect(limits).toEqual([
    ["states", 10001],
    ["states", 10001],
  ]);
  // Each switch of the mode explores both programs again, to the same limit in both modes, while
  // the runs and the step table stay.
  await watchLongTasks(page);
  const switches = Array.from({ length: 5 }, () => async () => {
    await page.evaluate(async () => {
      const store = await window.DeterminizeSim.ready;
      store.additive.value = !store.additive.value;
    });
    await expect.poll(() => exactKinds(page), { timeout: 30_000 }).toEqual(["limit", "limit"]);
  });
  const { median, each } = await longTasksPer(page, switches);
  test.info().annotations.push({ type: "long tasks per exploration (ms)", description: each });
  expect(median, `long tasks per exploration (ms): ${each}`).toBeLessThanOrEqual(200);
});

test("a newer program replaces a worker busy with a long step of an exploration", async ({
  page,
}) => {
  const workers: Worker[] = [];
  page.on("worker", (started) => {
    if (new URL(started.url()).pathname.endsWith("/exact-worker.js")) workers.push(started);
  });
  await page.goto(simulator);
  await expect.poll(() => exactKinds(page)).not.toBe(null);
  expect(workers).toHaveLength(1);
  const [busy] = workers;
  // The worker's next slice of an exploration is a step that doesn't end: it says that it has
  // begun, then keeps the worker's thread.
  await busy.evaluate(() => {
    const defer = self.setTimeout;
    self.setTimeout = ((_task: () => void, delay?: number) => {
      self.setTimeout = defer;
      return defer(() => {
        console.log("a long step");
        for (;;) {
          // The step goes on until the page terminates the worker.
        }
      }, delay);
    }) as typeof setTimeout;
  });
  const commit = (program: string) =>
    page.evaluate(async (source) => {
      const store = await window.DeterminizeSim.ready;
      store.source.value = source;
      store.commitSource();
    }, program);
  const step = busy.waitForEvent("console");
  await commit("uniform(0, 1) * 2");
  expect((await step).text()).toBe("a long step");
  expect(await exactKinds(page)).toBe(null);
  const closed = busy.waitForEvent("close");
  await commit("1 + 2");
  await expect.poll(() => exactKinds(page), { timeout: 10_000 }).toEqual(["finite", "finite"]);
  // The busy worker was terminated and replaced.
  await closed;
  expect(workers).toHaveLength(2);
});

test("an edit within an exploration's first frame replaces it with the new program's", async ({
  page,
}) => {
  const workers: string[] = [];
  page.on("worker", (started) => {
    const path = new URL(started.url()).pathname;
    if (path.endsWith("/exact-worker.js")) workers.push(path);
  });
  await page.goto(simulator);
  await expect.poll(() => exactKinds(page)).not.toBe(null);
  const started = workers.length;
  const dungeon = readFileSync(
    new URL("../../examples/paper/dungeon.det", import.meta.url),
    "utf8",
  );
  // Both edits in one task of the page, so that the dungeon's exploration hasn't reported yet.
  await page.evaluate(async (program) => {
    const store = await window.DeterminizeSim.ready;
    store.source.value = program;
    store.commitSource();
    store.source.value = "1 + 2";
    store.commitSource();
  }, dungeon);
  await expect.poll(() => exactKinds(page)).toEqual(["finite", "finite"]);
  const exact = await page.evaluate(async () => (await window.DeterminizeSim.ready).exact.value);
  expect(exact?.source).toBe("1 + 2");
  // The worker that had started on the dungeon was replaced.
  expect(workers.slice(started)).toEqual(["/determinize/sim/exact-worker.js"]);
});

test("from disk, the finite models are explored on the page's thread", async ({ page }) => {
  await page.goto(fromDisk);
  await pick(page, "Noisy iteration");
  await expect.poll(() => exactKinds(page)).toEqual(["draw", "finite"]);
});

test("a link to an example the gallery doesn't have selects none", async ({ page }) => {
  const shared = { source: "1 + 2", seed: 7, example: "paper/no-such-example" };
  const json = JSON.stringify(shared);
  await page.goto(`${simulator}#v1=${deflateRawSync(json).toString("base64url")}`);
  await passed(page, 7);
  await expect(page.locator("#example-title")).toHaveText("Your program");
  expect(await page.evaluate(async () => (await window.DeterminizeSim.ready).source.value)).toBe(
    shared.source,
  );
});

test("a link to more than 64 KiB opens the first example and says so", async ({ page }) => {
  const json = JSON.stringify({ source: "x".repeat(70_000), seed: 1, example: "paper/dungeon" });
  await page.goto(`${simulator}#v1=${deflateRawSync(json).toString("base64url")}`);
  await expect(page.getByRole("alert")).toHaveText(
    "This link's program is larger than 64 KiB. The simulator opened its first example.",
  );
  await expect(page.locator("#example-title")).toHaveText("Noisy product");
  await expect.poll(() => stepRun(page), { timeout: traceTimeout }).toMatchObject({ seed: 1 });
});

test("a second unreadable link replaces the first one's notice", async ({ page }) => {
  await page.goto(`${simulator}#v1=not-a-link`);
  await expect(page.getByRole("alert")).toHaveText(
    "This link could not be read. The simulator opened its first example.",
  );
  await page.evaluate(() => {
    window.location.hash = "#v1=still-not-a-link";
  });
  await expect(page.getByRole("alert")).toHaveText("This link could not be read.");
});

test("a readable link clears an unreadable one's notice", async ({ page }) => {
  await page.goto(`${simulator}#v1=not-a-link`);
  await expect(page.getByRole("alert")).toHaveCount(1);
  const source = await page.evaluate(async () => (await window.DeterminizeSim.ready).source.value);
  const json = JSON.stringify({ source, seed: 7, example: "paper/noisy-product" });
  const hash = `#v1=${deflateRawSync(json).toString("base64url")}`;
  await page.evaluate((next) => {
    window.location.hash = next;
  }, hash);
  await expect.poll(() => stepRun(page), { timeout: traceTimeout }).toMatchObject({ seed: 7 });
  await expect(page.getByRole("alert")).toHaveCount(0);
});

test("diagnostics follow the text", async ({ page }) => {
  await page.goto(simulator);
  await pick(page, "Bad E-branching");
  const marks = page.locator(".cm-lintRange-error, .cm-lintPoint");
  await expect(marks).toHaveCount(1);
  await expect(page.locator(".cm-lint-marker-error")).toHaveCount(1);
  await page.locator(".cm-content").first().click();
  await page.keyboard.press("Control+A");
  await page.keyboard.type("1 + 2");
  await expect(marks).toHaveCount(0);
  await page.keyboard.press("Control+Z");
  await expect(marks).toHaveCount(1);
});

test("Tab moves the focus out of the editor", async ({ page }) => {
  await page.goto(simulator);
  await page.locator(".cm-content").first().click();
  await page.keyboard.press("Tab");
  // The determinized program's pane may take the focus next; the source editor has let it go.
  expect(await page.evaluate(() => document.activeElement?.closest("#editor") ?? null)).toBe(null);
});

test("the step controls move the current step, and both panes highlight what it reduces", async ({
  page,
}) => {
  await page.goto(simulator);
  await expect(page.locator("#step-of")).toHaveText("Step 0 of 5");
  await expect(page.getByRole("button", { name: "Start", exact: true })).toBeDisabled();
  // The scrubber's arrow keys move one step, as the step region's do.
  await page.locator("#scrubber").focus();
  for (let i = 0; i < 3; i++) await page.keyboard.press("ArrowRight");
  await expect(page.locator("#step-of")).toHaveText("Step 3 of 5");
  await expect(page.locator('.step[aria-current="step"]')).toHaveAttribute("data-step", "3");
  // Step 3 draws y: the source's line 2 and its counterpart, the mean, are highlighted.
  const source = page.getByRole("textbox", { name: "Source program" });
  const determinized = page.getByRole("textbox", { name: "Determinized program" });
  await expect(source.locator(".cm-linked")).toHaveText(["let y = gaussian[E](x, 1) in"]);
  await expect(determinized.locator(".cm-linked")).toHaveText(["let y = mean_gauss(x, 1) in"]);
  await expect(page.locator('.step[data-step="3"]')).toContainText("E draw, source only: y = ");
  await page.locator("#step-table").focus();
  await page.keyboard.press("ArrowUp");
  await expect(page.locator("#step-of")).toHaveText("Step 2 of 5");
  await page.keyboard.press("End");
  await expect(page.locator("#step-of")).toHaveText("Step 5 of 5");
  await expect(page.getByRole("button", { name: "End", exact: true })).toBeDisabled();
  await page.getByRole("button", { name: "Start", exact: true }).click();
  await expect(page.locator("#step-of")).toHaveText("Step 0 of 5");
  await page.locator("#scrubber").fill("1");
  await expect(page.locator("#step-of")).toHaveText("Step 1 of 5");
  await expect(page.locator('.step[data-step="1"] .cell-draw')).toContainText("x ← ");
});

test("the parts of a row that correspond share a tint, and hovering one outlines the others", async ({
  page,
}) => {
  await page.goto(simulator);
  await passed(page);
  // Step 3 draws y: the source's sample, the symbol v1, the determinized program's mean and the
  // mean call that computes it.
  const row = page.locator('.step[data-step="3"]');
  const parts = row.locator(".corr-step");
  await expect(row.locator(".cell-source .corr-step")).toHaveText(["0.5235", "0.5235"]);
  await expect(row.locator(".cell-sym .corr-step")).toHaveText(["v1"]);
  await expect(row.locator(".cell-det .corr-step")).toHaveText(["0.5666", "mean_gauss(0.5666, 1)"]);
  const tints = await parts.evaluateAll((elements) =>
    elements.map((element) => getComputedStyle(element).backgroundColor),
  );
  expect(new Set(tints).size).toBe(1);
  expect(tints[0]).not.toBe("rgba(0, 0, 0, 0)");
  await row.locator(".cell-sym .corr-step").hover();
  await expect(row.locator(".corr-step.corr-active")).toHaveCount(await parts.count());
  // A step that rewrites a larger state as a whole has no part to mark; a state that is a value,
  // the run's result, is marked whole.
  await expect(page.locator('.step[data-step="4"] .corr-step')).toHaveCount(0);
  await expect(page.locator('.step[data-step="5"] .cell-source .corr-step')).toHaveText(["0.2966"]);
  await expect(page.locator('.step[data-step="5"] .cell-det .corr-step')).toHaveText(["0.321"]);
});

test("hovering a sample site marks the rows that draw it and its counterpart", async ({ page }) => {
  await page.goto(simulator);
  await passed(page);
  await page.locator(".cm-mode-hint").nth(1).hover();
  const determinized = page.getByRole("textbox", { name: "Determinized program" });
  await expect(determinized.locator(".cm-linked")).toHaveText(["let y = mean_gauss(x, 1) in"]);
  await expect(page.locator(".step.linked")).toHaveCount(1);
  await expect(page.locator(".step.linked")).toHaveAttribute("data-step", "3");
});

test("in a row with σ, every column's code starts where the symbolic program does", async ({
  page,
}) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await page.goto(simulator);
  await passed(page);
  // Step 1 draws x, whose G draw lines up too.
  for (const step of [0, 1, 3, 5]) {
    const tops = await page.locator(`.step[data-step="${step}"]`).evaluate((row) =>
      [".cell-source .state", ".cell-draw code", ".cell-sym .state", ".cell-det .state"]
        .map((selector) => row.querySelector(selector))
        .filter((element) => element !== null)
        .map((element) => Math.round(element.getBoundingClientRect().top)),
    );
    expect(new Set(tops).size, `step ${step}: ${tops}`).toBe(1);
  }
});

test("every row shows the symbolic state, with the whole of σ above its program", async ({
  page,
}) => {
  // A link from when the symbolic state had a toggle still opens, with the symbolic state shown.
  const product = new URL("../../examples/paper/noisy-product.det", import.meta.url);
  const json = JSON.stringify({
    source: readFileSync(product, "utf8").trimEnd(),
    seed: 1,
    example: "paper/noisy-product",
    symbolic: false,
  });
  await page.goto(`${simulator}#v1=${deflateRawSync(json).toString("base64url")}`);
  await passed(page, 1);
  await expect(page.locator("#notices")).toBeEmpty();
  await expect(page.locator("#example-title")).toHaveText("Noisy product");
  await expect(page.locator(".step[data-step] .cell-sym")).toHaveCount(6);
  // An empty σ shows nothing.
  await expect(page.locator('.step[data-step="0"] .sigma')).toHaveCount(0);
  // Step 3 binds v1, which σ keeps in the rows after it.
  await expect(page.locator('.step[data-step="3"] .sigma-new')).toContainText("v1 ~ gauss(");
  await expect(page.locator('.step[data-step="4"] .sigma')).toContainText("v1 ~ gauss(");
  await expect(page.locator('.step[data-step="4"] .sigma-new')).toHaveCount(0);
  await expect(page.locator('.step[data-step="3"] .cell-sym .state')).toContainText(
    "let y = v1 in",
  );
  // A column of its own where three columns of code fit; below the row's other cells elsewhere.
  const cells = async (width: number) => {
    await page.setViewportSize({ width, height: 900 });
    return page.evaluate(() => {
      const box = (cell: string) =>
        (
          document.querySelector(`.step[data-step="3"] .cell-${cell}`) as Element
        ).getBoundingClientRect();
      return { source: box("source"), symbolic: box("sym"), determinized: box("det") };
    });
  };
  const wide = await cells(1440);
  // σ heads the symbolic column, above the line where every column's code starts.
  expect(wide.symbolic.top).toBeLessThan(wide.source.top);
  expect(wide.symbolic.left).toBeGreaterThan(wide.source.right);
  expect(wide.symbolic.right).toBeLessThan(wide.determinized.left);
  const middle = await cells(768);
  expect(middle.symbolic.top).toBeGreaterThanOrEqual(middle.source.bottom);
  expect(middle.symbolic.width).toBeGreaterThan(middle.source.width);
});

/** The linear channels of a CSS `rgb()` colour. */
function linear(css: string) {
  return (css.match(/[\d.]+/g) ?? []).slice(0, 3).map((channel) => {
    const value = Number(channel) / 255;
    return value <= 0.04045 ? value / 12.92 : ((value + 0.055) / 1.055) ** 2.4;
  });
}

/** The contrast of two CSS `rgb()` colours, as WCAG computes it. */
function contrast(one: string, other: string) {
  const luminance = (css: string) => {
    const [r, g, b] = linear(css);
    return 0.2126 * r + 0.7152 * g + 0.0722 * b;
  };
  const [light, dark] = [luminance(one), luminance(other)].sort((x, y) => y - x);
  return (light + 0.05) / (dark + 0.05);
}

/** The chroma of a CSS `rgb()` colour in OKLCH: how colourful it is. */
function chroma(css: string) {
  const [r, g, b] = linear(css);
  const l = Math.cbrt(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b);
  const m = Math.cbrt(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b);
  const s = Math.cbrt(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b);
  const a = 1.9779984951 * l - 2.428592205 * m + 0.4505937099 * s;
  const bb = 0.0259040371 * l + 0.7827717662 * m - 0.808675766 * s;
  return Math.hypot(a, bb);
}

for (const colorScheme of ["light", "dark"] as const) {
  test(`code has syntax colours, quieter than the modes, in ${colorScheme}`, async ({ page }) => {
    await page.emulateMedia({ colorScheme });
    await page.goto(simulator);
    await passed(page);
    const colours = await page.evaluate(() => {
      const colour = (element: Element | undefined | null) =>
        element ? getComputedStyle(element).color : "";
      const token = (root: string, text: string) =>
        [...document.querySelectorAll(`${root} .cm-line span`)].find(
          (span) => span.textContent === text,
        );
      return {
        syntax: [
          colour(token("#editor", "let")),
          colour(token("#editor", "uniform")),
          colour(token("#editor", "0")),
          colour(token("#determinized-editor", "let")),
          colour(document.querySelector(".state .tok-keyword")),
          colour(document.querySelector(".state .tok-dist")),
          colour(document.querySelector(".state .tok-number")),
        ],
        modes: [
          colour(document.querySelector("#editor .cm-mode-e")),
          colour(document.querySelector("#editor .cm-mode-g")),
          colour(document.querySelector(".tok-mode-e")),
          colour(document.querySelector(".tok-mode-g")),
          colour(document.querySelector("#determinized-editor .cm-mean")),
        ],
        ink: getComputedStyle(document.body).color,
        ground: getComputedStyle(document.body).backgroundColor,
      };
    });
    for (const value of [...colours.syntax, ...colours.modes]) expect(value).not.toBe("");
    // Names stand out less than the G marks, at 4.5:1 at least.
    const name = contrast(colours.syntax[1], colours.ground);
    expect(name).toBeLessThan(contrast(colours.modes[1], colours.ground));
    expect(name).toBeGreaterThanOrEqual(4.5);
    // Keywords, names and numbers each have a colour of their own.
    expect(new Set(colours.syntax.slice(0, 3)).size).toBe(3);
    expect(colours.syntax).not.toContain(colours.ink);
    const quietest = Math.min(...colours.modes.map(chroma));
    for (const value of colours.syntax) expect(chroma(value)).toBeLessThan(0.7 * quietest);
  });
}

test("inferred modes show as hints, and hovers give each site's reason", async ({ page }) => {
  await page.goto(simulator);
  const hints = page.locator(".cm-mode-hint");
  await expect(hints).toHaveText(["[G]", "[E]"]);
  await hints.nth(1).hover();
  const tooltip = page.locator(".cm-tooltip-hover");
  await expect(tooltip).toContainText(
    "E: replaced by its mean. The output depends on this draw affinely. (Mode: affinity in the Lean development.)",
  );
  await expect(tooltip).toContainText("Type: float[E]");
});

for (const colorScheme of ["light", "dark"] as const) {
  test(`axe finds no violation in ${colorScheme}`, async ({ page }) => {
    await page.emulateMedia({ colorScheme });
    await page.goto(simulator);
    await passed(page);
    const results = await new AxeBuilder({ page })
      .withTags(["wcag2a", "wcag2aa", "wcag21aa", "wcag22aa"])
      .analyze();
    expect(results.violations).toEqual([]);
  });
}

test("the gallery lists the examples as links, a menu on wide screens and a dialog on narrow ones", async ({
  page,
}) => {
  await page.goto(simulator);
  const button = page.getByRole("button", { name: "Example: Noisy product" });
  await button.click();
  const gallery = page.getByRole("dialog", { name: "Examples" });
  await expect(gallery).toBeVisible();
  expect(await gallery.evaluate((dialog) => dialog.matches(":modal"))).toBe(false);
  await expect(gallery.getByRole("listitem")).toHaveCount(13);
  await expect(gallery.getByRole("listitem").first()).toContainText(
    "Noisy product A signal and a noisy measurement of it; the measurement becomes its mean. From the paper.",
  );
  await expect(gallery.getByRole("link").first()).toHaveAttribute("href", /^#v1=/);
  await page.keyboard.press("Escape");
  await expect(gallery).toBeHidden();
  await expect(button).toBeFocused();

  await page.setViewportSize({ width: 390, height: 844 });
  await button.click();
  expect(await gallery.evaluate((dialog) => dialog.matches(":modal"))).toBe(true);
  await gallery.getByRole("link", { name: "Observe", exact: true }).click();
  await expect(gallery).toBeHidden();
  await expect(page.getByRole("button", { name: "Example: Observe" })).toBeVisible();
  await passed(page);
});

test("the gallery groups the examples by the premise they fail, each with a chip that says why", async ({
  page,
}) => {
  await page.goto(simulator);
  await page.getByRole("button", { name: /^Example: / }).click();
  const gallery = page.getByRole("dialog", { name: "Examples" });
  await expect(gallery.locator(".g-heading")).toHaveText([
    "The check finds no premise failing",
    "Fails only in floating point",
    "Lean rejects the written modes",
    "A run fails a domain check",
  ]);
  await expect(gallery.getByRole("list").first().locator(".chip")).toHaveCount(0);
  // The chip's description shows on keyboard focus, and Escape hides it before it closes the
  // gallery.
  await gallery.getByRole("link", { name: "Recursive gamma", exact: true }).focus();
  await page.keyboard.press("Tab");
  const chip = gallery.getByRole("button", { name: "underflow" });
  await expect(chip).toBeFocused();
  await expect(chip).toHaveAccessibleDescription(
    /^Domain safety: found failing in floating point at a parameter of exactly 0, with “gamma requires positive shape and rate”\. An earlier gamma draw underflows to 0/,
  );
  const tip = gallery.getByRole("tooltip");
  await expect(tip).toBeVisible();
  // An open finding's chip has a note's muted outline; a failing one, the warning's crimson.
  const border = (name: string) =>
    gallery
      .getByRole("button", { name })
      .first()
      .evaluate((button) => getComputedStyle(button).borderColor);
  expect(await border("underflow")).not.toBe(await border("a run fails"));
  await page.keyboard.press("Escape");
  await expect(tip).toBeHidden();
  await expect(gallery).toBeVisible();
  await page.keyboard.press("Escape");
  await expect(gallery).toBeHidden();
  // And on hover, right below the chip, so that the pointer can move onto it and it stays.
  await page.getByRole("button", { name: /^Example: / }).click();
  const rejected = gallery.getByRole("button", { name: "modes rejected" }).first();
  await rejected.hover();
  const tooltip = gallery.getByRole("tooltip");
  await expect(tooltip).toHaveText(/^Type float\[E\]: Lean rejects the written modes\./);
  const chipBox = await rejected.boundingBox();
  const tipBox = await tooltip.boundingBox();
  if (!chipBox || !tipBox) throw new Error("no chip or no description");
  expect(tipBox.y).toBeLessThanOrEqual(chipBox.y + chipBox.height + 1);
  await page.mouse.move(tipBox.x + 8, tipBox.y + tipBox.height / 2, { steps: 4 });
  await expect(tooltip).toBeVisible();
});

test("a click on an entry that a chip's description covers opens the entry", async ({ page }) => {
  await page.goto(simulator);
  await page.getByRole("button", { name: /^Example: / }).click();
  const gallery = page.getByRole("dialog", { name: "Examples" });
  await gallery.getByRole("button", { name: "underflow" }).hover();
  const tip = gallery.getByRole("tooltip");
  await expect(tip).toBeVisible();
  const tipBox = await tip.boundingBox();
  if (!tipBox) throw new Error("no description");
  let covered = null;
  for (const link of await gallery.locator(".g-group a").all()) {
    const box = await link.boundingBox();
    if (!box) continue;
    const [x, y] = [box.x + box.width / 2, box.y + box.height / 2];
    if (
      x >= tipBox.x &&
      x <= tipBox.x + tipBox.width &&
      y >= tipBox.y &&
      y <= tipBox.y + tipBox.height
    ) {
      covered = link;
      break;
    }
  }
  if (!covered) throw new Error("the description covers no entry");
  const title = await covered.textContent();
  await covered.click();
  await expect(gallery).toBeHidden();
  await expect(page.getByRole("button", { name: `Example: ${title}` })).toBeVisible();
});

test("at 768 px the gallery's menu shows every example without scrolling", async ({ page }) => {
  await page.setViewportSize({ width: 768, height: 900 });
  await page.goto(simulator);
  await page.getByRole("button", { name: /^Example: / }).click();
  const gallery = page.getByRole("dialog", { name: "Examples" });
  await expect(gallery).toBeVisible();
  expect(await gallery.evaluate((menu) => menu.scrollHeight <= menu.clientHeight)).toBe(true);
});

test("at 1440 px the gallery's two columns start at the same height", async ({ page }) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await page.goto(simulator);
  await page.getByRole("button", { name: /^Example: / }).click();
  const gallery = page.getByRole("dialog", { name: "Examples" });
  await expect(gallery).toBeVisible();
  // The top of each column's first heading, by the column's left edge.
  const tops = await gallery.locator(".g-heading").evaluateAll((headings) => {
    const columns = new Map<number, number>();
    for (const heading of headings) {
      const box = heading.getBoundingClientRect();
      if (!columns.has(Math.round(box.left))) columns.set(Math.round(box.left), box.top);
    }
    return [...columns.values()];
  });
  expect(tops).toHaveLength(2);
  expect(tops[1]).toBe(tops[0]);
});

test("at 390 px every chip's description stays inside the gallery", async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto(simulator);
  await page.getByRole("button", { name: /^Example: / }).click();
  const gallery = page.getByRole("dialog", { name: "Examples" });
  for (const chip of await gallery.locator(".chip").all()) {
    await chip.scrollIntoViewIfNeeded();
    await chip.hover();
    const tip = gallery.getByRole("tooltip");
    await expect(tip).toBeVisible();
    const inside = await tip.evaluate((element) => {
      const box = element.getBoundingClientRect();
      const dialog = (element.closest("dialog") as Element).getBoundingClientRect();
      return (
        box.left >= dialog.left &&
        box.right <= dialog.right &&
        box.top >= dialog.top &&
        box.bottom <= dialog.bottom
      );
    });
    expect(inside, (await chip.textContent()) ?? "").toBe(true);
  }
});

test("New program opens an empty editor whose placeholder shows the syntax", async ({ page }) => {
  await page.goto(simulator);
  await passed(page);
  await page.getByRole("button", { name: /^Example: / }).click();
  await page
    .getByRole("dialog", { name: "Examples" })
    .getByRole("link", { name: "New program" })
    .click();
  await expect(page.locator("#example-title")).toHaveText("New program");
  const source = page.getByRole("textbox", { name: "Source program" });
  await expect(source.locator(".cm-placeholder")).toContainText("let y = gauss[E](x, 1) in");
  const hint = page.locator("#source-hint");
  await expect(hint).toBeVisible();
  await expect(hint).toContainText("gets its mode from inference");
  await source.click();
  await page.keyboard.type("uniform(0, 1)");
  await expect(hint).toBeHidden();
  await expect(page.locator("#example-title")).toHaveText("Your program");
});

test("the introduction hides on request, and stays hidden", async ({ page }) => {
  await page.goto(simulator);
  const intro = page.getByRole("region", { name: "Introduction" });
  await expect(intro).toContainText("Draws marked E are replaced by their means.");
  await intro.getByRole("button", { name: "Hide" }).click();
  await expect(intro).toBeHidden();
  await page.reload();
  await passed(page);
  await expect(intro).toBeHidden();
});

test("the glossary opens from the header, and Copy link copies the state's address", async ({
  page,
  context,
}) => {
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);
  await page.goto(simulator);
  await page.getByRole("button", { name: "Glossary" }).click();
  await expect(page.getByRole("region", { name: "Glossary" })).toContainText(
    "affinity in the Lean development",
  );
  await page.getByRole("button", { name: "Copy link" }).click();
  await expect(page.locator("#copy-status")).toHaveText("Link copied.");
  const copied = await page.evaluate(() => navigator.clipboard.readText());
  expect(copied).toMatch(/\/determinize\/sim\/#v1=[A-Za-z0-9_-]+$/);
});

test("the header's tools sit behind More on narrow screens", async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto(simulator);
  await expect(page.getByRole("button", { name: "Copy link" })).toBeHidden();
  await page.getByRole("button", { name: "More" }).click();
  await expect(page.getByRole("button", { name: "Copy link" })).toBeVisible();
});

test("the theme select overrides the system's scheme, and the page remembers it", async ({
  page,
}) => {
  await page.emulateMedia({ colorScheme: "light" });
  await page.goto(simulator);
  const ground = () => page.evaluate(() => getComputedStyle(document.body).backgroundColor);
  expect(await ground()).toBe("rgb(243, 244, 241)");
  await page.getByLabel("Theme").selectOption("dark");
  expect(await ground()).toBe("rgb(21, 22, 25)");
  await page.reload();
  await expect(page.getByLabel("Theme")).toHaveValue("dark");
  expect(await ground()).toBe("rgb(21, 22, 25)");
  await page.getByLabel("Theme").selectOption("system");
  expect(await ground()).toBe("rgb(243, 244, 241)");
});
