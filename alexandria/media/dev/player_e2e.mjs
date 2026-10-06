// Drives the proof players of a running Clerk page in Chromium:
//   node dev/player_e2e.mjs <url> <out-dir>
// Counts the WebAssembly modules the page instantiates (one per raster
// kernel), plays chosen steps of each player and screenshots them.
// PLAYWRIGHT names a directory holding node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://localhost:7778/", out = "player-e2e"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(out, { recursive: true });

// CHROMIUM: a browser executable, when the installed one is not Playwright's own.
const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1200, height: 900 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
await page.addInitScript(() => {
  window.__wasm = { modules: 0, instances: 0 };
  const M = WebAssembly.Module, I = WebAssembly.Instance;
  WebAssembly.Module = function (b) { window.__wasm.modules++; return new M(b); };
  WebAssembly.Instance = function (m, i) { window.__wasm.instances++; return new I(m, i); };
});
await page.goto(url, { waitUntil: "networkidle" });
await page.waitForTimeout(3000);

// [player index, step index (0-based), ms to play before the shot];
// SHOTS (JSON) replaces the default, which suits archimedes_circle.
const shots = process.env.SHOTS ? JSON.parse(process.env.SHOTS) :
              [[0, 0, 2500], [0, 2, 3500], [0, 4, 3000], [0, 4, 6500], [0, 9, 6500],
               [1, 2, 2000], [1, 3, 2500], [1, 4, 3500],
               [2, 1, 2600], [2, 4, 3900], [2, 7, 3200], [2, 10, 2500]];
const players = page.locator("ol:has(li em)");
const frames = page.locator("svg[viewBox]").filter({ has: page.locator("g") });
// A shot's optional fourth value sets the player's first control slider
// (the second range input, after the scrubber) the way a drag would.
for (const [p, s, ms, control] of shots) {
  const fig = players.nth(p).locator("xpath=preceding-sibling::div[1]");
  if (control !== undefined) {
    await fig.locator("input[type=range]").nth(1).evaluate((el, v) => {
      const set = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value").set;
      set.call(el, String(v));
      el.dispatchEvent(new Event("input", { bubbles: true }));
    }, control);
  }
  await players.nth(p).locator("li").nth(s).click();
  await page.waitForTimeout(ms);
  await fig.screenshot({ path: `${out}/p${p}-s${s}-${ms}${control !== undefined ? `-n${control}` : ""}.png` });
}
console.log(JSON.stringify({ wasm: await page.evaluate(() => window.__wasm), players: await players.count(), errors }));
await browser.close();
