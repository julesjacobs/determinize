// The simulator in the assembled preview, where it samples in a worker, and opened from disk,
// where it samples on the page's own thread: every example runs, batches stream without long
// tasks and stop when the program changes, links restore the program, the seed and the example,
// Tab leaves the editor, and axe finds no violation.
import { readFileSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { deflateRawSync, inflateRawSync } from "node:zlib";
import { AxeBuilder } from "@axe-core/playwright";
import type { Page } from "@playwright/test";
import { expect, test } from "@playwright/test";
import type { Store } from "../src/ui/store.ts";

declare global {
  interface Window {
    /** The bundle's exports; `ready` resolves to the page's store. */
    DeterminizeSim: { ready: Promise<Store> };
    /** The durations of the long tasks since `watchLongTasks`. */
    longTasks: number[];
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

/** Sets how many runs of each program "Run both" brings the runs to, through the page's store. */
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

/** Clicks "Run both" and waits until its runs have arrived. */
async function runBoth(page: Page, timeout?: number) {
  await page.getByRole("button", { name: "Run both", exact: true }).click();
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

const status = (page: Page) => page.locator("#steps-status");
const checked = /, and every step check passed\.$/;

for (const [where, url, worker] of [
  ["in the preview, with a worker", simulator, true],
  ["from disk, on the page's thread", fromDisk, false],
] as const) {
  test(`runs an example ${where}`, async ({ page }) => {
    const workers: string[] = [];
    page.on("worker", (started) => workers.push(started.url()));
    await page.goto(url);
    await expect(status(page)).toHaveText("Seed 1: 5 steps, and every step check passed.");
    await setRunCount(page, 200);
    await runBoth(page);
    expect(await runs(page)).toBe(200);
    expect(workers.map((started) => new URL(started).pathname.split("/").at(-1)).sort()).toEqual(
      worker ? ["trace-worker.js", "worker.js"] : [],
    );
  });
}

test("runs every example", async ({ page }) => {
  await page.goto(simulator);
  await setRunCount(page, 20);
  const titles = await page.locator("#gallery-list a").allTextContents();
  expect(titles.length).toBeGreaterThan(0);
  for (const title of titles) {
    await pick(page, title);
    await expect(status(page)).toHaveText(/^Seed \d+: /);
    await expect(page.locator(".step").first()).toBeVisible();
    const before = await status(page).textContent();
    await runBoth(page);
    // Runs 1 to 19 follow run 0, which the step table keeps showing.
    expect(await runs(page)).toBe(20);
    await expect(status(page)).toHaveText(before ?? "");
  }
});

test("a 200-run and a 5000-run batch leave no long task over 200 ms", async ({ page }) => {
  test.setTimeout(120_000);
  await page.goto(simulator);
  await pick(page, "Dungeon crawl");
  await watchLongTasks(page);
  await setRunCount(page, 200);
  await runBoth(page);
  expect(await runs(page)).toBe(200);

  await pick(page, "Noisy product");
  await setRunCount(page, 5000);
  await runBoth(page);
  expect(await runs(page)).toBe(5000);
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

test("a second Run during a batch adds its runs as well", async ({ page }) => {
  test.setTimeout(60_000);
  await page.goto(simulator);
  // 100000 runs of the Gaussian random walk take seconds, so the first batch is still running at
  // the second click.
  await pick(page, "Gaussian random walk");
  await setRunCount(page, 100000);
  await page.getByRole("button", { name: "Run both", exact: true }).click();
  await expect(page.locator("#distributions[aria-busy=true]")).toHaveCount(1);
  const runningAtSecondClick = await page.evaluate(async () => {
    const running = (await window.DeterminizeSim.ready).running.value !== null;
    (document.querySelector("#run-both") as HTMLButtonElement).click();
    return running;
  });
  expect(runningAtSecondClick).toBe(true);
  await settled(page, 50_000);
  // The first click brings the runs to 100000, the second adds as many again.
  expect(await runs(page)).toBe(200000);
});

test("editing during a run discards its stale batches", async ({ page }) => {
  await page.goto(simulator);
  await pick(page, "Gaussian random walk");
  await setRunCount(page, 100000);
  await page.getByRole("button", { name: "Run both", exact: true }).click();
  await expect.poll(() => runs(page)).toBeGreaterThan(1);
  await page.locator(".cm-content").first().click();
  await page.keyboard.press("Control+End");
  await page.keyboard.type(" ");
  await settled(page);
  // The edited program's run in the step table is its first run, and no other arrives.
  await expect.poll(() => runs(page)).toBe(1);
  await page.waitForTimeout(1000);
  expect(await runs(page)).toBe(1);
});

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
  await expect(status(opened)).toHaveText(
    new RegExp(`^Seed ${state.seed}: .*every step check passed\\.$`),
  );
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
  await setRunCount(page, 10);
  await runBoth(page);
  await expect(page.locator("#variance-source")).toHaveText(
    "unavailable (floating-point overflow)",
  );
  await expect(page.locator("#factor")).toContainText(
    "A variance is unavailable (floating-point overflow)",
  );
});

test("Lean's output shows the checked type, the sample sites and both programs", async ({
  page,
}) => {
  await page.goto(simulator);
  await expect(page.locator("#checked-type")).toHaveText("float[E]");
  await expect(page.locator("#source-sites")).toHaveText("Sample sites: 2 continuous");
  await expect(page.locator("#determinized-sites")).toHaveText("Sample sites: 1 continuous");
  await page.getByText("Annotated source", { exact: true }).click();
  await expect(page.locator("#annotated-program")).toContainText("gauss[E](x, 1)");
  const determinized = page.getByRole("textbox", { name: "Determinized program" });
  await expect(determinized).toContainText("let y = mean_gauss(x, 1) in");
  await expect(determinized).toHaveAttribute("aria-readonly", "true");
  // Lean prints nothing for a program that it rejects for another reason than a mode conflict.
  await page.locator(".cm-content").first().click();
  await page.keyboard.press("Control+End");
  await page.keyboard.type(" +");
  await expect(page.locator("#source-alert")).toHaveText(
    /^Lean rejects this program at parsing\.\s*Line \d+: /,
  );
  await expect(page.locator("#checked")).toBeHidden();
  await expect(page.locator("#determinized-empty")).toHaveText(
    "No determinized program: Lean rejects the source.",
  );
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
  await expect(page.locator("#counterexample-label")).toHaveText(
    "Lean rejects this program; this is what replacing its [E] draws anyway does",
  );
  await expect(page.getByRole("textbox", { name: "Determinized program" })).toContainText(
    "let x = mean_uniform(0, 1) in",
  );
});

test("the runs' outcomes, the command that reports them and the run's G trace show", async ({
  page,
}) => {
  await page.goto(simulator);
  const honesty = page.locator("#honesty");
  await expect(honesty).not.toContainText("--samples");
  await expect(page.locator("#g-trace")).toHaveText(
    /^\[\(uniform, [-0-9.e]+\)\], in both programs$/,
  );
  await setRunCount(page, 200);
  await runBoth(page);
  await expect(page.locator("#returned-source")).toHaveText("200 of 200");
  await expect(honesty).toContainText(
    "./run.sh --seed 1 --samples 200 examples/paper/noisy-product.det",
  );

  await page.goto(linkTo("1/0", 3));
  await setRunCount(page, 10);
  await runBoth(page);
  await expect(page.locator("#returned-source")).toContainText("0 of 10");
  await expect(page.locator("#returned-source")).toContainText("10 failed");
  await expect(page.locator("#first-failure")).toContainText(
    "First failure in the source: division by zero",
  );
  await expect(honesty).toContainText("with the program saved as program.det");
});

test("a program that Lean rejects has no runs and no command", async ({ page }) => {
  await page.goto(linkTo("let x =", 1));
  await expect(page.locator("#dist-empty")).toHaveText("Nothing to run: Lean rejects the program.");
  await expect(page.getByRole("button", { name: "Run both", exact: true })).toBeDisabled();
  await expect(page.locator("#honesty")).not.toContainText("--samples");
});

test("a program the simulator fails on doesn't blame Lean", async ({ page }) => {
  // Too deeply nested for the simulator, which recurses over the program.
  await page.goto(linkTo(Array(10000).fill("1").join(" + "), 1));
  await expect(page.locator("#source-alert")).toContainText(
    "The simulator failed on this program. ./run.sh --check program.det shows whether Lean accepts it.",
  );
  await expect(page.locator("#dist-empty")).toHaveText(
    "Nothing to run: the simulator failed on this program.",
  );
});

test("the share of returned runs links returnProbability only for a float program", async ({
  page,
}) => {
  await page.goto(simulator);
  await setRunCount(page, 10);
  await runBoth(page);
  const returned = page.getByRole("link", { name: "Returned" });
  await expect(returned).toHaveCount(1);
  await page.goto(linkTo("let x = uniform(0, 1) in x < 0.5", 1));
  await setRunCount(page, 10);
  await runBoth(page);
  await expect(page.locator("#checked-type")).toHaveText("bool");
  await expect(page.locator("#list-source")).toContainText("10 of 10 runs returned.");
  await expect(returned).toHaveCount(0);
});

test("notices say which premises of the theorems are not met", async ({ page }) => {
  await page.goto(simulator);
  await setRunCount(page, 10);
  await runBoth(page);
  const premises = page.locator("#premises");
  await expect(premises).toContainText("Domain safety: not established by typing.");
  await expect(page.locator("#premises-other")).toBeHidden();
  await pick(page, "Gaussian random walk");
  await runBoth(page);
  await expect(page.locator("#checked-type")).toHaveText("[(float[E] * float[E])]");
  await expect(page.locator("#premises-other")).toContainText(
    "No theorem applies to this output. Its type is [(float[E] * float[E])], not float[E]",
  );
  // A counterexample shows no theorem notice, and compares the means instead.
  await pick(page, "Noisy product, both draws E");
  await runBoth(page);
  await expect(premises).toBeHidden();
  await expect(page.locator("#factor")).toHaveText(/^Means differ: [0-9.]+ and 0\.2500\.$/);
});

test("the plot against a G draw shows each run, and a click steps through it", async ({ page }) => {
  await page.goto(simulator);
  await setRunCount(page, 1000);
  await runBoth(page);
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
  await expect(status(page)).toHaveText(/^Seed 1: /);
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
  await expect(status(page)).toHaveText("Seed 3: 1606 steps, and every step check passed.");
  const note = page.locator(".page-note");
  await expect(note).toHaveText(
    /^Steps 0 to \d+ of 1606 are shown; the step controls reach the others\.$/,
  );
  const end = Number((await note.textContent())?.match(/to (\d+)/)?.[1]);
  await expect(page.locator(".step[data-step]")).toHaveCount(end + 1);
  await page.getByRole("button", { name: "Last" }).click();
  await expect(page.locator("#step-of")).toHaveText("Step 1606 of 1606");
  await expect(note).toHaveText(/^Steps \d+ to 1606 of 1606 are shown/);
  await expect(page.locator('.step[aria-current="step"]')).toHaveAttribute("data-step", "1606");
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

test("stepping, playing across pages and scrubbing move neither the bar nor the page", async ({
  page,
}) => {
  test.setTimeout(60_000);
  const irwinHall = new URL("../../examples/loops/irwin_hall.det", import.meta.url);
  await page.goto(linkTo(readFileSync(irwinHall, "utf8"), 3));
  await expect(status(page)).toHaveText(checked);
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
        rowInRegion: !!row && row.top >= region.top - 1 && row.bottom <= region.bottom + 1,
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
  await page.getByRole("button", { name: "Step", exact: true }).click();
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
  await page.evaluate(() =>
    (document.querySelector("#step-table") as HTMLElement).focus({ preventScroll: true }),
  );
  await page.keyboard.press("End");
  await expect(page.locator("#step-of")).toHaveText("Step 1606 of 1606");
  await unmoved();
  // Play from the last step of the first page goes on onto the next page.
  await page.evaluate(async (step) => {
    (await window.DeterminizeSim.ready).currentStep.value = step;
  }, end);
  await page.getByRole("button", { name: "Play" }).click();
  await expect(page.locator("#step-of")).toHaveText(`Step ${end + 2} of 1606`, { timeout: 4000 });
  await expect(page.getByRole("button", { name: "Pause" })).toBeVisible();
  await page.getByRole("button", { name: "Pause" }).click();
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
  const longest = example("paper/gauss-random-walk");
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
      await expect(status(page)).toHaveText(/^Seed 1: /);
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
  await expect(status(page)).toHaveText(checked);
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
  await expect(status(page)).toHaveText(checked);
  expect(await scroller.evaluate((element) => element.scrollTop)).toBe(atCaret);
});

test("hovering a span in the source marks the rows that reduce it", async ({ page }) => {
  await page.goto(simulator);
  await expect(status(page)).toHaveText(checked);
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
  await expect(status(page)).toHaveText(checked);
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
  await expect(page.getByRole("button", { name: "Run both", exact: true })).toBeEnabled();
  const longTasks = await page.evaluate(() => window.longTasks);
  test.info().annotations.push({ type: "long tasks (ms)", description: JSON.stringify(longTasks) });
  expect(Math.max(0, ...longTasks)).toBeLessThanOrEqual(200);
});

test("a newer program replaces the step table's computation in flight", async ({ page }) => {
  const deep = "let u = uniform(0, 1) in (rec f n => if n < 1 then u else 1 + f (n - 1)) 2000";
  const workers: string[] = [];
  page.on("worker", (started) => workers.push(new URL(started.url()).pathname));
  await page.goto(linkTo(deep, 1));
  await expect(status(page)).toHaveText("Computing the steps of this run…");
  await page.locator(".cm-content").first().click();
  await page.keyboard.press("Control+A");
  await page.keyboard.type("1 + 2");
  await expect(status(page)).toHaveText(/^Seed 1: .*every step check passed\.$/);
  // The worker that computed the deep recursion was terminated and replaced.
  expect(workers.filter((path) => path.endsWith("/trace-worker.js"))).toHaveLength(2);
});

test("a link to an example the gallery doesn't have selects none", async ({ page }) => {
  const shared = { source: "1 + 2", seed: 7, example: "paper/no-such-example" };
  const json = JSON.stringify(shared);
  await page.goto(`${simulator}#v1=${deflateRawSync(json).toString("base64url")}`);
  await expect(status(page)).toHaveText(/^Seed 7: /);
  await expect(status(page)).toHaveText(checked);
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
  await expect(status(page)).toHaveText(/^Seed 1: /);
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
  await expect(status(page)).toHaveText(/^Seed 7: /);
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
  expect(await page.evaluate(() => document.activeElement?.closest(".cm-editor") ?? null)).toBe(
    null,
  );
});

test("the step controls move the current step, and both panes highlight what it reduces", async ({
  page,
}) => {
  await page.goto(simulator);
  await expect(page.locator("#step-of")).toHaveText("Step 0 of 5");
  for (let i = 0; i < 3; i++) await page.getByRole("button", { name: "Step", exact: true }).click();
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
  await expect(page.getByRole("button", { name: "Last" })).toBeDisabled();
  await page.locator("#scrubber").fill("1");
  await expect(page.locator("#step-of")).toHaveText("Step 1 of 5");
  await expect(page.locator('.step[data-step="1"] .cell-draw')).toContainText("x ← ");
});

test("hovering a sample site marks the rows that draw it and its counterpart", async ({ page }) => {
  await page.goto(simulator);
  await expect(status(page)).toHaveText(checked);
  await page.locator(".cm-mode-hint").nth(1).hover();
  const determinized = page.getByRole("textbox", { name: "Determinized program" });
  await expect(determinized.locator(".cm-linked")).toHaveText(["let y = mean_gauss(x, 1) in"]);
  await expect(page.locator(".step.linked")).toHaveCount(1);
  await expect(page.locator(".step.linked")).toHaveAttribute("data-step", "3");
});

test("the symbolic state shows on request, and the page remembers the choice", async ({ page }) => {
  await page.goto(simulator);
  await expect(status(page)).toHaveText(checked);
  await expect(page.locator(".cell-sym")).toHaveCount(0);
  await page.getByLabel("Show symbolic state").check();
  await expect(page.locator("#symbolic-note")).toBeVisible();
  await expect(page.locator('.step[data-step="3"] .cell-sym')).toContainText("v1");
  await page.reload();
  await expect(page.getByLabel("Show symbolic state")).toBeChecked();
  await expect(page.locator(".cell-sym").first()).toBeVisible();
});

test("Play advances a step per second until Pause", async ({ page }) => {
  await page.goto(simulator);
  await expect(status(page)).toHaveText(checked);
  const play = page.getByRole("button", { name: "Play" });
  await play.click();
  await expect(page.getByRole("button", { name: "Pause" })).toHaveAttribute("aria-pressed", "true");
  await expect(page.locator("#step-of")).toHaveText("Step 1 of 5", { timeout: 2500 });
  await page.getByRole("button", { name: "Pause" }).click();
  const paused = await page.locator("#step-of").textContent();
  await page.waitForTimeout(1500);
  await expect(page.locator("#step-of")).toHaveText(paused ?? "");
});

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
    await expect(status(page)).toHaveText(checked);
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
  await expect(gallery.getByRole("listitem")).toHaveCount(10);
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
  await expect(page.locator("#checked-type")).toHaveText("float[E]");
});

test("the introduction hides on request, and stays hidden", async ({ page }) => {
  await page.goto(simulator);
  const intro = page.getByRole("region", { name: "Introduction" });
  await expect(intro).toContainText("Draws marked E are replaced by their means.");
  await intro.getByRole("button", { name: "Hide" }).click();
  await expect(intro).toBeHidden();
  await page.reload();
  await expect(page.locator("#checked-type")).toHaveText("float[E]");
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

test("a first visit asks for a prediction before the first runs, and repeats it after them", async ({
  page,
}) => {
  await page.goto(simulator);
  const form = page.locator("#predict");
  await expect(form).toContainText(
    "Before you run them: what will 10 000 runs of each program show?",
  );
  await setRunCount(page, 200);
  await form.getByLabel("the same as the source's").check();
  await form.getByLabel("smaller").check();
  await form.getByRole("button", { name: "Run both and compare" }).click();
  await settled(page);
  await expect(form).toBeHidden();
  await expect(page.locator("#prediction")).toHaveText(
    "You predicted the same mean and a smaller variance.",
  );
  await page.reload();
  await expect(page.locator("#checked-type")).toHaveText("float[E]");
  await expect(form).toBeHidden();
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
