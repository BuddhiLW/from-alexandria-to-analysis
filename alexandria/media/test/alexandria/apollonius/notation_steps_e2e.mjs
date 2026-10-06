// Drives the step-by-step notation figure (Apollonius I.11-13) of a static
// build in Chromium:
//   node notation_steps_e2e.mjs <notebook-url> <shots-dir>
// Checks: no page errors, a MathBox canvas, eight sentences in the step
// list, WebAssembly modules instantiated (emmy-viewers' raster glue), and
// that stepping 1..8 changes the scene. Each step is clicked in the list,
// played to its end, and the canvas screenshot to <shots-dir>/step-N.png;
// consecutive shots must differ. Prints one JSON line; exit 1 on failure.
// PLAYWRIGHT names a directory holding node_modules/playwright; CHROMIUM a
// browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";
import { createHash } from "node:crypto";

const [url, shots = "notation-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1300, height: 950 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text().slice(0, 300)); });
await page.addInitScript(() => {
  const W = globalThis.WebAssembly;
  const counts = (globalThis.__wasm = { instantiate: 0, instance: 0 });
  const inst = W.instantiate.bind(W);
  W.instantiate = (...a) => { counts.instantiate++; return inst(...a); };
  W.Instance = new Proxy(W.Instance, {
    construct(t, a, nt) { counts.instance++; return Reflect.construct(t, a, nt); },
  });
});

await page.goto(url, { waitUntil: "networkidle" });
const root = page.locator("[data-notation-step]").first();
await root.waitFor({ timeout: 60000 });
const figure = page.locator("[data-figure=apollonius-notation]").first();
await figure.scrollIntoViewIfNeeded();
await page.waitForTimeout(2000);

const canvases = await page.locator("[data-figure=apollonius-notation] canvas").count();
const items = await page.locator("li[data-step]").count();
const durations = [7000, 5000, 5000, 6000, 6500, 3500, 5000, 9000];
const hashes = [];
const steps = [];
for (let i = 1; i <= 8; i++) {
  await page.locator(`li[data-step="${i}"]`).click();
  await page.waitForTimeout(durations[i - 1] + 900);
  const shown = await root.getAttribute("data-notation-step");
  const current = await page.locator(`li[data-step="${i}"]`).getAttribute("data-current");
  const buf = await figure.screenshot({ path: `${shots}/step-${i}.png` });
  hashes.push(createHash("sha1").update(buf).digest("hex").slice(0, 12));
  const readout = (await page.locator("[data-notation-step] [data-y], [data-notation-step] [data-x], [data-notation-step] [data-p], [data-notation-step] [data-d]").allInnerTexts()).join(" | ");
  steps.push({ step: i, shown, current, readout });
}
const wasm = await page.evaluate(() => globalThis.__wasm);
const distinct = hashes.every((h, i) => i === 0 || h !== hashes[i - 1]);
const result = { url, errors, canvases, items, hashes, distinct, wasm, steps };
const ok = errors.length === 0 && canvases > 0 && items === 8 && distinct &&
  steps.every((s) => s.shown === String(s.step) && s.current === "true") &&
  wasm.instantiate + wasm.instance > 0;
console.log(JSON.stringify({ ok, ...result }));
await browser.close();
process.exit(ok ? 0 : 1);
