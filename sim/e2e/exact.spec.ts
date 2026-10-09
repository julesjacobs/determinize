// The exact values in the statistics table: the noisy iteration's, a finite model under the
// estimates and the reason why the source has none; the layout, which stays put whatever
// exploration finds; and the additive mode, which the state limit offers and links carry.
import { readFileSync } from "node:fs";
import { deflateRawSync, inflateRawSync } from "node:zlib";
import type { Page } from "@playwright/test";
import { expect, test } from "@playwright/test";
import type { ExactState } from "../src/core/exact.ts";
import type { Store } from "../src/ui/store.ts";

declare global {
  interface Window {
    DeterminizeSim: { ready: Promise<Store> };
  }
}

const simulator = "sim/";

/** The simulator's address for `source` at seed 1, with no example chosen. */
function linkTo(source: string) {
  const json = JSON.stringify({ source, seed: 1, example: "" });
  return `${simulator}#v1=${deflateRawSync(json).toString("base64url")}`;
}

async function settled(page: Page) {
  await expect(page.locator("[aria-busy=true]")).toHaveCount(0, { timeout: 30_000 });
}

/** Waits until the exact values show the current program's explorations, ended. */
async function explored(page: Page) {
  await expect(page.locator("#exact")).toBeVisible();
  await settled(page);
}

async function pick(page: Page, title: string) {
  await page.getByRole("button", { name: /^Example: / }).click();
  await page
    .getByRole("dialog", { name: "Examples" })
    .getByRole("link", { name: title, exact: true })
    .click();
  await expect(page.locator("#example-title")).toHaveText(title);
}

/** The text of the exact values' cells, row by row. */
function exactRows(page: Page) {
  return page
    .locator("#exact tr:not(.exact-head)")
    .evaluateAll((rows) =>
      rows.map((row) =>
        [...row.querySelectorAll("th, td")].map((cell) =>
          (cell.textContent ?? "").replace(/\s+/g, " ").trim(),
        ),
      ),
    );
}

test("the noisy iteration's exact values sit under the estimates, the source's reason beside them", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Noisy iteration");
  await explored(page);
  expect(await exactRows(page)).toEqual([
    ["Model", "none", "68 states"],
    ["Mean", "The Gaussian draw on line 3 is continuous.", "1/3 0.3333"],
    ["Variance", "2/9 0.2222"],
    ["Returned", "1"],
  ]);
  // Each exact value links the Lean definition it is.
  for (const [name, declaration] of [
    ["Mean", "conditionalMean"],
    ["Variance", "conditionalVariance"],
    ["Returned", "returnMass"],
  ]) {
    await expect(page.locator("#exact").getByRole("link", { name, exact: true })).toHaveAttribute(
      "href",
      new RegExp(
        `Spec/FiniteModel/Statistics\\.html#Determinize\\.Spec\\.FiniteModel\\.OutputStatistics\\.${declaration}$`,
      ),
    );
  }
  // The glossary names Lean's certificate check.
  await expect(page.locator('#glossary a[href*="checkStatistics"]')).toHaveText("checkStatistics");
  await expect(page.locator("#exact")).not.toContainText("certif");
});

/** Pairs of explorations of both programs that exploration may end in, with what they show. */
const outcomes: { programs: [ExactState, ExactState]; shows: RegExp }[] = [
  {
    programs: [
      { kind: "exploring", discovered: 0 },
      { kind: "exploring", discovered: 120 },
    ],
    shows: /computing…/,
  },
  {
    programs: [
      {
        kind: "limit",
        limit: "states",
        discovered: 10001,
        message: "Incomplete exploration",
      },
      { kind: "limit", limit: "stateBytes", discovered: 12, message: "Incomplete exploration" },
    ],
    shows: /More than 10 000 states, Lean's limit; try the additive mode\./,
  },
  {
    programs: [
      { kind: "too many", states: 9999, maxStates: 256, message: "exact solver state limit" },
      {
        kind: "fails",
        detail: "expected a list of probabilities",
        sites: [{ from: 0, to: 1 }],
        state: 4,
        message: "Exploration failed",
      },
    ],
    shows:
      /More states than the 256 that Lean solves exactly; Lean passes larger models to the Storm model checker\..*A run fails on line 1: expected a list of probabilities\./,
  },
  {
    programs: [
      { kind: "not a number", state: 3, message: "Exploration failed" },
      { kind: "unsolved", message: "singular value equations; no absorption certificate" },
    ],
    shows: /The output isn't a number\..*singular value equations; no absorption certificate/,
  },
  {
    programs: [
      {
        kind: "draw",
        distribution: "poisson",
        sites: [{ from: 0, to: 1 }],
        state: 2,
        message: "…",
      },
      {
        kind: "draw",
        distribution: "poisson",
        sites: [{ from: 0, to: 1 }],
        state: 2,
        message: "…",
      },
    ],
    shows:
      /none\s+none\s+Mean\s+The Poisson draw on line 1 has infinitely many outcomes\.\s+Variance/,
  },
  {
    programs: [
      {
        kind: "finite",
        states: 204,
        returnProbability: { fraction: "1", value: 1 },
        rejectionProbability: { fraction: "0", value: 0 },
        mean: { fraction: "9740/1651", value: 9740 / 1651 },
        variance: { fraction: "16036620/2725801", value: 16036620 / 2725801 },
      },
      {
        kind: "finite",
        states: 3,
        returnProbability: { fraction: "0", value: 0 },
        rejectionProbability: { fraction: "1", value: 1 },
        mean: null,
        variance: null,
      },
    ],
    shows: /204 states\s+3 states/,
  },
  {
    programs: [
      { kind: "solving", states: 256 },
      {
        kind: "finite",
        states: 9,
        returnProbability: { fraction: "1/2", value: 0.5 },
        // A probability of rejection whose double is 0 still shows.
        rejectionProbability: { fraction: `1/1${"0".repeat(400)}`, value: 0 },
        mean: { fraction: "1", value: 1 },
        variance: { fraction: "0", value: 0 },
      },
    ],
    shows: /solving….*1\/2\s+1\/10+ rejected/s,
  },
];

for (const width of [390, 768, 1440]) {
  test(`at ${width} px, the exact values take the same room whatever exploration finds`, async ({
    page,
  }) => {
    await page.setViewportSize({ width, height: 900 });
    await page.goto(simulator);
    await pick(page, "Noisy iteration");
    await explored(page);
    const measure = () =>
      page.evaluate(() => {
        const group = document.querySelector("#exact") as Element;
        const after = document.querySelector("#as-table") as Element;
        return {
          height: group.getBoundingClientRect().height,
          after: after.getBoundingClientRect().top + window.scrollY,
        };
      });
    const found = await measure();
    for (const { programs, shows } of outcomes) {
      await page.evaluate(async ([source, determinized]) => {
        const store = await window.DeterminizeSim.ready;
        const exact = store.exact as unknown as { value: Store["exact"]["value"] };
        if (!exact.value) throw new Error("the noisy iteration has no exact values");
        exact.value = {
          ...exact.value,
          programs: { source, determinized },
          done: source.kind !== "exploring",
        };
      }, programs);
      await expect(page.locator("#exact")).toHaveText(shows);
      expect(await measure(), JSON.stringify(programs)).toEqual(found);
    }
  });
}

test("the walk opens in the additive mode, the state limit offers it, and links carry it", async ({
  page,
  context,
}) => {
  await page.goto(simulator);
  await pick(page, "Asymmetric random walk");
  await explored(page);
  const additive = page.getByRole("checkbox", { name: "Additive mode" });
  await expect(additive).toBeChecked();
  const finite = [
    ["Model", "none", "199 states"],
    ["Mean", "The uniform draw on line 6 is continuous.", "5/14 0.3571"],
    ["Variance", "17/98 0.1735"],
    ["Returned", "1"],
  ];
  expect(await exactRows(page)).toEqual(finite);
  // Without the additive mode, the determinized program's states exceed Lean's limit.
  await additive.uncheck();
  await explored(page);
  await expect(page.locator("#model-det")).toHaveText("too large");
  await expect(page.locator("#exact")).toContainText(
    "More than 10 000 states, Lean's limit; try the additive mode.",
  );
  await page.getByRole("button", { name: "try the additive mode" }).click();
  await expect(additive).toBeChecked();
  await explored(page);
  expect(await exactRows(page)).toEqual(finite);
  await expect
    .poll(() => {
      const hash = new URL(page.url()).hash.slice("#v1=".length);
      if (!hash) return null;
      return JSON.parse(inflateRawSync(Buffer.from(hash, "base64url")).toString("utf8")).additive;
    })
    .toBe(true);
  const opened = await context.newPage();
  await opened.goto(page.url());
  await explored(opened);
  await expect(opened.getByRole("checkbox", { name: "Additive mode" })).toBeChecked();
  await expect(opened.locator("#model-det")).toHaveText("199 states");
  // Another example opens in its own mode.
  await pick(page, "Noisy iteration");
  await expect(additive).not.toBeChecked();
});

test("where neither program has values, the rows' headers are plain and muted", async ({
  page,
}) => {
  await page.goto(simulator);
  await explored(page);
  // The noisy product's uniform draw stays random in both programs.
  await expect(page.locator("#exact")).toContainText("The uniform draw on line 1 is continuous.");
  const headers = page.locator("#exact .exact-value th");
  await expect(headers).toHaveText(["Mean", "Variance", "Returned"]);
  await expect(headers.locator("a")).toHaveCount(0);
  await expect(headers.first()).toHaveClass(/\bnone\b/);
  await pick(page, "Noisy iteration");
  await explored(page);
  await expect(headers.locator("a")).toHaveCount(3);
  await expect(headers.first()).not.toHaveClass(/\bnone\b/);
});

test("a counterexample, a list output and a program Lean rejects have no exact values", async ({
  page,
}) => {
  await page.goto(simulator);
  await pick(page, "Noisy product, both draws E");
  await settled(page);
  await expect(page.locator("#dist-grid")).toBeVisible();
  await expect(page.locator("#exact")).toBeHidden();
  const walk = readFileSync(
    new URL("../../examples/paper/gauss-random-walk.det", import.meta.url),
    "utf8",
  );
  await page.goto(linkTo(walk));
  await settled(page);
  await expect(page.locator("#list-out")).toBeVisible();
  await expect(page.locator("#exact")).toBeHidden();
  await page.goto(linkTo("true + 1"));
  await expect(page.locator("#exact")).toBeHidden();
  expect(await page.evaluate(async () => (await window.DeterminizeSim.ready).exact.value)).toBe(
    null,
  );
});
