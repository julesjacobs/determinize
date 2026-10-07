// Browser tests of the assembled site in _preview/ (check.sh site assembles it), served by esbuild
// under the same /determinize/ prefix as GitHub Pages. Chromium comes from the site dev shell.
import { defineConfig } from "@playwright/test";

const port = 8471;

export default defineConfig({
  testDir: "e2e",
  forbidOnly: !!process.env.CI,
  reporter: process.env.CI ? "github" : "list",
  use: {
    baseURL: `http://127.0.0.1:${port}/determinize/`,
  },
  webServer: {
    command: `esbuild --servedir=../_preview --serve=127.0.0.1:${port} --log-level=warning`,
    url: `http://127.0.0.1:${port}/determinize/`,
    reuseExistingServer: false,
  },
});
