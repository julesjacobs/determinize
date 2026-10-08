// The simulator in the assembled preview, where it samples in a worker, and opened from disk,
// where it samples on the page's own thread: every example runs, batches stream without long
// tasks and stop when the program changes, links restore the program, the seed and the example,
// Tab leaves the editor, and axe finds only the step table's known contrast violation.
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

/** Sets how many runs "Run N" adds, through the page's store. */
async function setRunCount(page: Page, count: number) {
  await page.evaluate(async (n) => {
    (await window.DeterminizeSim.ready).sampleCount.value = n;
  }, count);
  await expect(page.getByRole("button", { name: `Run ${count}` })).toBeVisible();
}

/** Records the duration of every long task on the page's main thread from now on. */
async function watchLongTasks(page: Page) {
  await page.evaluate(() => {
    window.longTasks = [];
    new PerformanceObserver((list) => {
      for (const entry of list.getEntries()) window.longTasks.push(entry.duration);
    }).observe({ type: "longtask" });
  });
}

const status = (page: Page) => page.locator("#coupling-status");
const samples = (page: Page) => page.locator("#distribution-status");

for (const [where, url, worker] of [
  ["in the preview, with a worker", simulator, true],
  ["from disk, on the page's thread", fromDisk, false],
] as const) {
  test(`runs an example ${where}`, async ({ page }) => {
    const workers: string[] = [];
    page.on("worker", (started) => workers.push(started.url()));
    await page.goto(url);
    await expect(status(page)).toHaveText("seed 2026 - checked");
    await page.getByRole("button", { name: "Run 200" }).click();
    await settled(page);
    await expect(samples(page)).toHaveText("201 runs");
    expect(workers.map((started) => new URL(started).pathname.split("/").at(-1))).toEqual(
      worker ? ["worker.js"] : [],
    );
  });
}

test("runs every example", async ({ page }) => {
  await page.goto(simulator);
  await setRunCount(page, 20);
  const titles = await page.locator("#example-select option").allTextContents();
  expect(titles.length).toBeGreaterThan(0);
  for (const title of titles) {
    await page.getByLabel("Example").selectOption({ label: title });
    await expect(status(page)).toHaveText(/^seed \d+ - /);
    await expect(page.locator(".coupling-row").first()).toBeVisible();
    const before = await status(page).textContent();
    await page.getByRole("button", { name: "Run 20" }).click();
    await settled(page);
    // Runs 1 to 20 follow run 0, which the step table keeps showing.
    await expect(samples(page)).toHaveText("21 runs");
    await expect(status(page)).toHaveText(before ?? "");
  }
});

test("Run 200 and a 5000-run batch leave no long task over 200 ms", async ({ page }) => {
  test.setTimeout(120_000);
  await page.goto(simulator);
  await page.getByLabel("Example").selectOption({ label: "Dungeon" });
  await watchLongTasks(page);
  await page.getByRole("button", { name: "Run 200" }).click();
  await settled(page);
  await expect(samples(page)).toHaveText("201 runs");

  await page.getByLabel("Example").selectOption({ label: "Noisy product" });
  await setRunCount(page, 5000);
  await page.getByRole("button", { name: "Run 5000" }).click();
  await settled(page);
  await expect(samples(page)).toHaveText("5001 runs");
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
  await page.getByLabel("Example").selectOption({ label: "Gaussian random walk" });
  await setRunCount(page, 100000);
  const run = page.getByRole("button", { name: "Run 100000" });
  await run.click();
  await expect(page.locator("[aria-busy=true]")).toHaveCount(1);
  const runningAtSecondClick = await page.evaluate(async () => {
    const running = (await window.DeterminizeSim.ready).running.value !== null;
    (document.querySelector("#many-coupling") as HTMLButtonElement).click();
    return running;
  });
  expect(runningAtSecondClick).toBe(true);
  await settled(page, 50_000);
  await expect(samples(page)).toHaveText("200001 runs");
});

test("editing during a run discards its stale batches", async ({ page }) => {
  await page.goto(simulator);
  await page.getByLabel("Example").selectOption({ label: "Gaussian random walk" });
  await setRunCount(page, 100000);
  await page.getByRole("button", { name: "Run 100000" }).click();
  await expect(samples(page)).not.toHaveText("1 run");
  await page.locator(".cm-content").first().click();
  await page.keyboard.press("Control+End");
  await page.keyboard.type(" ");
  await settled(page);
  // The edited program's run in the step table is its first run, and no other arrives.
  await expect(samples(page)).toHaveText("1 run");
  await page.waitForTimeout(1000);
  await expect(samples(page)).toHaveText("1 run");
});

test("a link restores the program, the seed and the example", async ({ page, context }) => {
  await page.goto(simulator);
  expect(new URL(page.url()).hash).toBe("");
  await page.getByLabel("Example").selectOption({ label: "Dungeon" });
  await page.getByRole("button", { name: "Rerun" }).click();
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
  await expect(opened.locator("#example-select option:checked")).toHaveText("Dungeon");
  await expect(status(opened)).toHaveText(`seed ${state.seed} - checked`);
  expect(await opened.evaluate(async () => (await window.DeterminizeSim.ready).source.value)).toBe(
    state.source,
  );
});

test("a variance that overflows shows as Lean's CLI prints it", async ({ page }) => {
  await page.goto(linkTo("uniform(0, 1e200)", 1));
  await setRunCount(page, 10);
  await page.getByRole("button", { name: "Run 10" }).click();
  await settled(page);
  await expect(page.locator(".dist-card.original")).toContainText(
    "unavailable (floating-point overflow)",
  );
  await expect(page.locator(".variance-ratio-card")).toContainText(
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
  await page.getByLabel("Example").selectOption({ label: "Noisy product, both draws E" });
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

test("the runs' outcomes, the command that reports them and run 0's G trace show", async ({
  page,
}) => {
  await page.goto(simulator);
  const outcomes = page.locator(".run-outcomes");
  await expect(outcomes).toContainText("--seed 2026 --samples 1 examples/paper/noisy-product.det");
  await expect(page.locator("#g-trace")).toHaveText(
    /^\[\(uniform, [-0-9.e]+\)\], in both programs$/,
  );
  await page.getByRole("button", { name: "Run 200" }).click();
  await settled(page);
  await expect(outcomes).toContainText(
    "Source: 201 of 201 runs returned a value, 0 were rejected by an observation, and 0 failed.",
  );
  await expect(outcomes).toContainText(
    "./run.sh --seed 2026 --samples 201 examples/paper/noisy-product.det",
  );

  await page.goto(linkTo("1/0", 3));
  await expect(outcomes).toContainText(
    "Source: 0 of 1 runs returned a value, 0 were rejected by an observation, and 1 failed. " +
      "The first failure: division by zero.",
  );
  await expect(outcomes).toContainText("with the program saved as program.det");
});

test("a program that Lean rejects has no runs and no command", async ({ page }) => {
  await page.goto(linkTo("let x =", 1));
  const outcomes = page.locator(".run-outcomes");
  await expect(outcomes).toHaveText("Lean rejects this program, so it has no runs.");
  await expect(outcomes).not.toContainText("--samples");
});

test("a program the simulator fails on doesn't blame Lean", async ({ page }) => {
  // Too deeply nested for the simulator, which recurses over the program.
  await page.goto(linkTo(Array(10000).fill("1").join(" + "), 1));
  await expect(page.locator(".run-outcomes")).toHaveText(
    "The simulator failed on this program, so it shows no runs. ./run.sh --check program.det shows whether Lean accepts it.",
  );
});

test("the share of returned runs links returnProbability only for a float program", async ({
  page,
}) => {
  await page.goto(simulator);
  const share = page.locator(".run-outcomes").getByRole("link", { name: /runs returned a value/ });
  await expect(share).toHaveCount(2);
  await page.getByLabel("Example").selectOption({ label: "Gaussian random walk" });
  await expect(page.locator("#checked-type")).toHaveText("[(float[E] * float[E])]");
  await expect(page.locator(".run-outcomes")).toContainText("1 of 1 runs returned a value");
  await expect(share).toHaveCount(0);
});

test("notices say which premises of the theorems are not met", async ({ page }) => {
  await page.goto(simulator);
  const type = page.locator("#premise-type");
  const safety = page.locator("#premise-safety");
  await expect(safety).toContainText("Typing establishes neither the domain safety");
  await expect(type).toBeHidden();
  await page.getByLabel("Example").selectOption({ label: "Gaussian random walk" });
  await expect(page.locator("#checked-type")).toHaveText("[(float[E] * float[E])]");
  await expect(type).toContainText("Its type is not float[E]");
  await expect(safety).toBeVisible();
  // A counterexample shows no theorem notice.
  await page.getByLabel("Example").selectOption({ label: "Noisy product, both draws E" });
  await expect(type).toBeHidden();
  await expect(safety).toBeHidden();
});

/** The simulator's address for `source` at `seed`, with no example chosen. */
function linkTo(source: string, seed: number) {
  const json = JSON.stringify({ source, seed, example: "" });
  return `${simulator}#v1=${deflateRawSync(json).toString("base64url")}`;
}

test("a long run shows its steps a page at a time", async ({ page }) => {
  const irwinHall = new URL("../../examples/loops/irwin_hall.det", import.meta.url);
  await page.goto(linkTo(readFileSync(irwinHall, "utf8"), 3));
  const pager = page.getByRole("navigation", { name: "Pages of the step table" });
  await expect(pager).toContainText("Steps 0–199 of 1607");
  await expect(page.locator(".coupling-row")).toHaveCount(200);
  await pager.getByRole("button", { name: "Next" }).click();
  await expect(pager).toContainText("Steps 200–399 of");
  await expect(page.locator(".coupling-row").first()).toHaveAttribute("data-step", "200");
  await pager.getByRole("button", { name: "Last" }).click();
  await expect(pager.getByRole("button", { name: "Last" })).toBeDisabled();
  await expect(status(page)).toHaveText("seed 3 - checked");
});

test("a run that doesn't end stops at the step table's limit", async ({ page }) => {
  await page.goto(linkTo("(rec f x => f x) ()", 1));
  await expect(status(page)).toHaveText("seed 1 - stopped after 20000 steps");
});

test("a deep recursion stops where the step table grows too large to show", async ({ page }) => {
  const deep = "let u = uniform(0, 1) in (rec f n => if n < 1 then u else 1 + f (n - 1)) 2000";
  await page.goto(linkTo(deep, 1));
  // The page computes the table up to its size bound, which takes about 2 s in Node.
  await expect(status(page)).toHaveText(
    /^seed 1 - stopped after \d+ steps: the table grew too large to show$/,
    { timeout: 20_000 },
  );
  await expect(page.getByRole("button", { name: "Run 200" })).toBeEnabled();
});

test("a link to an example the gallery doesn't have selects none", async ({ page }) => {
  const shared = { source: "1 + 2", seed: 7, example: "paper/no-such-example" };
  const json = JSON.stringify(shared);
  await page.goto(`${simulator}#v1=${deflateRawSync(json).toString("base64url")}`);
  await expect(status(page)).toHaveText("seed 7 - checked");
  await expect(page.locator("#example-select option:checked")).toHaveCount(0);
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
  await expect(page.locator("#example-select option:checked")).toHaveText("Noisy product");
  await expect(status(page)).toHaveText("seed 2026 - checked");
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
  await expect(status(page)).toHaveText("seed 7 - checked");
  await expect(page.getByRole("alert")).toHaveCount(0);
});

test("diagnostics follow the text", async ({ page }) => {
  await page.goto(simulator);
  await page.getByLabel("Example").selectOption({ label: "Branching on an E draw" });
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

test("a frame's checks open on hover and close with Escape", async ({ page }) => {
  await page.goto(simulator);
  const popover = page.getByRole("tooltip").filter({ hasText: "The step checks passed" });
  await page.locator(".step-check").nth(1).hover();
  await expect(popover).toBeVisible();
  await page.keyboard.press("Escape");
  await expect(popover).toBeHidden();
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

test("axe finds only the step table's contrast violation", async ({ page }) => {
  await page.goto(simulator);
  await expect(status(page)).toHaveText("seed 2026 - checked");
  const results = await new AxeBuilder({ page })
    .withTags(["wcag2a", "wcag2aa", "wcag21aa", "wcag22aa"])
    .analyze();
  // The step table's highlighted values lack contrast.
  expect(results.violations.map((violation) => violation.id)).toEqual(["color-contrast"]);
});
