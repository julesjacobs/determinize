// Every link from the simulator to the API documentation is absolute and canonical, and the page
// itself links each one, so that the deploy's link check, which reads the page's HTML, checks
// every target.
import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import test from "node:test";
import { leanLinks } from "../src/ui/links.ts";

const sim = new URL("../", import.meta.url);
const page = readFileSync(new URL("index.html", sim), "utf8");
const docs = "https://julesjacobs.com/determinize/docs/";

test("the page links every documentation link of the views", () => {
  const sources = readdirSync(new URL("src", sim), { recursive: true, encoding: "utf8" })
    .filter((file) => file.endsWith(".ts"))
    .map((file) => readFileSync(new URL(`src/${file}`, sim), "utf8"));
  const literal = sources.flatMap(
    (text) =>
      text.match(/https:\/\/julesjacobs\.com\/determinize\/docs\/[^"`'\s)]*\.html[^"`'\s)]*/g) ??
      [],
  );
  for (const url of [...Object.values(leanLinks), ...literal]) {
    assert.ok(url.startsWith(docs), url);
    assert.ok(page.includes(`href="${url}"`), `sim/index.html doesn't link ${url}`);
  }
});

test("the page links the documentation by canonical URLs", () => {
  for (const [, href] of page.matchAll(/href="([^"]*docs[^"]*)"/g)) {
    assert.ok(href.startsWith(docs), href);
  }
});
