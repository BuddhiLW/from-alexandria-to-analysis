// Drives the step-by-step application of areas (Euclid I.44, VI.27-29, II.5
// to Apollonius' three names) of a static build in Chromium:
//   node areas_steps_e2e.mjs <notebook-url> <shots-dir>
// Checks: no page errors, the SVG figure, ten sentences, WebAssembly
// modules instantiated (emmy-viewers' raster glue), each step clicked,
// played to its end and screenshot to <shots-dir>/step-N.png (consecutive
// shots differ), the readout of each step, the Emmy identities, and the
// tied Leva panel at step 5: S typed to 24 moves y to the root 4 (same side
// of a/2), y typed to 2 moves S to 16, S typed to 27 leaves no root and y
// where it was. Prints one JSON line; exit 1 on failure. PLAYWRIGHT names a
// directory holding node_modules/playwright; CHROMIUM a browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";
import { createHash } from "node:crypto";

const [url, shots = "areas-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
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
const root = page.locator("[data-areas-step]").first();
await root.waitFor({ timeout: 60000 });
const figure = page.locator("[data-figure=apollonius-areas]").first();
await root.scrollIntoViewIfNeeded();
await page.waitForTimeout(1500);

const items = await page.locator("li[data-step]").count();
const durations = [7000, 5000, 5500, 6000, 16000, 7000, 7500, 4500, 6500, 6000];
const hashes = [];
const steps = [];
for (let i = 1; i <= durations.length; i++) {
  await page.locator(`li[data-step="${i}"]`).click();
  await page.waitForTimeout(durations[i - 1] + 800);
  const shown = await root.getAttribute("data-areas-step");
  const buf = await figure.screenshot({ path: `${shots}/step-${i}.png` });
  await root.screenshot({ path: `${shots}/widget-${i}.png` });
  hashes.push(createHash("sha1").update(buf).digest("hex").slice(0, 12));
  const readout = (await page.locator("[data-readout]").allInnerTexts()).join(" ").replace(/\s+/g, " ").slice(0, 200);
  const roots = await page.locator("[data-roots]").first().getAttribute("data-roots", { timeout: 500 }).catch(() => null);
  const quote = (await page.locator("[data-quote]").count()) > 0;
  steps.push({ step: i, shown, readout, roots, quote });
}

// the tied panel at step 5
await page.locator('li[data-step="5"]').click();
await page.waitForTimeout(durations[4] + 800);
const inputs = page.locator("[data-areas-step] input:not([type=range])");
const n = await inputs.count();
const find = async (prefix) => {
  for (let j = 0; j < n; j++) if ((await inputs.nth(j).inputValue()).startsWith(prefix)) return inputs.nth(j);
  return null;
};
const readYS = async () => {
  const r = page.locator("[data-readout]").first();
  return { y: await r.getAttribute("data-y"), S: await r.getAttribute("data-s"),
           roots: await page.locator("[data-roots]").first().getAttribute("data-roots") };
};
const typeInto = async (input, v) => { await input.fill(String(v)); await input.press("Enter"); await page.waitForTimeout(700); };
const tie = { inputs: n, start: await readYS() };
// inputs in schema order: y then S
const yIn = inputs.nth(0), sIn = inputs.nth(1);
await typeInto(sIn, 24); tie.S24 = await readYS();
await root.screenshot({ path: `${shots}/tie-S24.png` });
await typeInto(yIn, 2); tie.y2 = await readYS();
await root.screenshot({ path: `${shots}/tie-y2.png` });
await typeInto(sIn, 27); tie.S27 = await readYS();
await root.screenshot({ path: `${shots}/tie-S27.png` });

const identities = await page.locator("[data-identity]").count();
const proved = await page.locator("[data-identity]", { hasText: "proved by Emmy" }).count();
const wasm = await page.evaluate(() => globalThis.__wasm);
const distinct = hashes.every((h, i) => i === 0 || h !== hashes[i - 1]);
const tieOk = tie.S24.y === "4.000" && tie.y2.S === "16.000" && tie.S27.roots === "— —" && tie.S27.y === "2.000";
const ok = errors.length === 0 && items === 10 && distinct && wasm.instance + wasm.instantiate > 0
  && identities === 11 && proved === 11 && tieOk;
console.log(JSON.stringify({ ok, items, distinct, wasm, identities, proved, tieOk, tie, steps, errors: errors.slice(0, 8) }));
await browser.close();
process.exit(ok ? 0 : 1);
