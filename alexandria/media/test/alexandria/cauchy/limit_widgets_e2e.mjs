// Drives the Cauchy notebook of a static build in Chromium:
//   node limit_widgets_e2e.mjs <notebook-url> <shots-dir>
// Checks: no page errors, no remote scripts, WebAssembly instantiated,
// the eps-delta widget ties eps <-> delta both ways (moving eps re-solves
// delta, moving delta re-solves eps, each with one solve and no echo
// loop), the jump closes delta on 0, the ratio widget's series picker and
// n slider move the root/ratio readouts and U <-> N round-trip, the
// stepped players render their sentences. Screenshots to <shots-dir>.
// Prints one JSON line; exit 1 on failure. PLAYWRIGHT names a directory
// holding node_modules/playwright; CHROMIUM a browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "cauchy-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1300, height: 1000 } });
const errors = [];
const remote = [];
const origin = new URL(url).origin;
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text().slice(0, 300)); });
page.on("request", (r) => {
  if (r.resourceType() === "script" && !r.url().startsWith(origin) && !r.url().startsWith("data:")) remote.push(r.url());
});
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
const eps = page.locator("[data-figure=cauchy-eps-delta]").first();
await eps.waitFor({ timeout: 60000 });
await eps.scrollIntoViewIfNeeded();
await page.waitForTimeout(1500);

const attrs = async (loc, names) => Object.fromEntries(await Promise.all(names.map(async (n) => [n, await loc.getAttribute(n)])));
const epsAttrs = ["data-f", "data-a", "data-eps", "data-delta", "data-w", "data-closed"];
const solves = () => page.evaluate(() => globalThis.__cauchySolves || 0);
const ratioSolves = () => page.evaluate(() => globalThis.__ratioSolves || 0);

// Leva inputs by key (Leva mounts one panel for the page; the keys are unique)
async function setLeva(root, key, value) {
  const input = page.locator(`input[id="${key}"], input[id$=".${key}"]`).first();
  await input.click({ clickCount: 3 });
  await input.fill(String(value));
  await input.press("Enter");
  await page.waitForTimeout(900);
}
async function pick(root, key, label) {
  const sel = page.locator(`select[id="${key}"], select[id$=".${key}"]`).first();
  await sel.selectOption({ label });
  await page.waitForTimeout(900);
}

const r = { url };
r.start = await attrs(eps, epsAttrs);
await eps.screenshot({ path: `${shots}/eps-delta-start.png` });

let s0 = await solves();
await setLeva(eps, "eps", 0.5);
r.afterEps = await attrs(eps, epsAttrs);
r.epsSolves = (await solves()) - s0;
const closedOk = Math.abs(Number(r.afterEps["data-delta"]) - (Math.sqrt(1 + 0.5) - 1)) < 1e-6;

s0 = await solves();
await setLeva(eps, "delta", 0.05);
r.afterDelta = await attrs(eps, epsAttrs);
r.deltaSolves = (await solves()) - s0;
const inverseOk = Math.abs(Number(r.afterDelta["data-eps"]) - (1.05 * 1.05 - 1)) < 1e-3;
await eps.screenshot({ path: `${shots}/eps-delta-tied.png` });

await pick(eps, "f", "a jump at 1");
await setLeva(eps, "eps", 0.3);
r.jump = await attrs(eps, [...epsAttrs]);
r.jumpVerdict = await eps.locator("[data-verdict]").first().getAttribute("data-verdict");
await eps.screenshot({ path: `${shots}/eps-delta-jump.png` });

await pick(eps, "f", "sin x");
await setLeva(eps, "a", 1.4);
r.sine = await attrs(eps, epsAttrs);
await eps.screenshot({ path: `${shots}/eps-delta-sine.png` });

// The ratio widget
const ratio = page.locator("[data-figure=cauchy-ratio-root]").first();
await ratio.scrollIntoViewIfNeeded();
const rAttrs = ["data-series", "data-n", "data-u", "data-nn", "data-root", "data-ratio", "data-verdict"];
r.ratioStart = await attrs(ratio, rAttrs);
await ratio.screenshot({ path: `${shots}/ratio-start.png` });
await setLeva(ratio, "n", 40);
r.ratioN = await attrs(ratio, rAttrs);
let q0 = await ratioSolves();
await setLeva(ratio, "U", 0.6);
r.ratioU = await attrs(ratio, rAttrs);
await setLeva(ratio, "N", 30);
r.ratioNN = await attrs(ratio, rAttrs);
r.ratioSolves = (await ratioSolves()) - q0;
await ratio.screenshot({ path: `${shots}/ratio-tied.png` });
await pick(ratio, "series", "2ⁿ/n²");
r.ratioGrowth = await attrs(ratio, rAttrs);
await ratio.screenshot({ path: `${shots}/ratio-diverges.png` });
await pick(ratio, "series", "1/n");
r.ratioHarmonic = await attrs(ratio, rAttrs);

// stepped players: the limit and ch. II figures render their sentences
const sentences = await page.locator("li", { hasText: "quantité variable" }).count();
const ch2 = await page.locator("li", { hasText: "décroît indéfiniment avec celle de α" }).count();
const limitFig = page.locator("li", { hasText: "quantité variable" }).first();
await limitFig.scrollIntoViewIfNeeded();
await page.locator("li", { hasText: "En Géométrie, la surface du cercle" }).first().click();
await page.waitForTimeout(8500);
await page.locator("li", { hasText: "En Géométrie, la surface du cercle" }).first().locator("xpath=../..").screenshot({ path: `${shots}/limit-circle.png` });
const abel = await page.locator("[data-control=n]").count();

r.wasm = await page.evaluate(() => globalThis.__wasm);
r.errors = errors; r.remote = remote;
const ok = errors.length === 0 && remote.length === 0 && r.wasm.instantiate + r.wasm.instance > 0 &&
  closedOk && inverseOk && r.epsSolves >= 1 && r.epsSolves <= 2 && r.deltaSolves >= 1 && r.deltaSolves <= 2 &&
  r.jumpVerdict === "jump" && Number(r.jump["data-delta"]) < 1e-9 &&
  r.sine["data-closed"] !== null && Number(r.sine["data-delta"]) > 0 &&
  r.ratioN["data-n"] === "40" && r.ratioN["data-root"] !== r.ratioStart["data-root"] &&
  Number(r.ratioU["data-nn"]) > Number(r.ratioStart["data-nn"]) &&
  Number(r.ratioNN["data-u"]) > 0.6 && Number(r.ratioNN["data-u"]) < 1 && r.ratioNN["data-nn"] === "30" &&
  r.ratioGrowth["data-verdict"] === "diverges" && r.ratioHarmonic["data-verdict"] === "open" &&
  sentences > 0 && ch2 > 0 && abel > 0;
console.log(JSON.stringify({ ok, closedOk, inverseOk, sentences, ch2, abel, ...r }));
await browser.close();
process.exit(ok ? 0 : 1);
