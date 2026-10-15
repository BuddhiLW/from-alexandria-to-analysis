// Browser check of the non-Euclidean notebook (Lobachevsky, Bolyai,
// Beltrami) on a static build:
//   node dev/non_euclidean_e2e.mjs <url> <out-dir>
// Zero page errors, no remote <script>, wasm modules instantiated (raster
// kernels). Angle of parallelism: setting x re-solves Pi and t (one solve
// per edit), Pi = 90 at x -> 0 named Euclid's case, setting Pi re-solves x,
// every preset pressed. Triangle playground: dragging a vertex changes the
// angle sum, area = pi - S, the near-centre / ideal presets name
// "euclidean" / "ideal", the Klein toggle keeps the true sum, the
// invariant badge stays green, reset. Parallels playground: drag P, the
// "limiting" preset names the limiting parallel, alpha small names
// "meets", large names "misses", reset. Pseudosphere: K = -1 at R = 1,
// K = -4 at R = 0.5, the sphere names "sphere" with K = +1, MathBox
// canvas present, the invariant badge green. Screenshots into <out-dir>.
// PLAYWRIGHT names a directory holding node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:8095/non-euclidean/", out = "target/non-euclidean-e2e"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(out, { recursive: true });

const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1300, height: 1000 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
await page.addInitScript(() => {
  window.__wasm = { modules: 0 };
  const M = WebAssembly.Module;
  WebAssembly.Module = function (b) { window.__wasm.modules++; return new M(b); };
  const ci = WebAssembly.instantiate;
  WebAssembly.instantiate = function (...a) { window.__wasm.modules++; return ci.apply(this, a); };
});
await page.goto(url, { waitUntil: "networkidle", timeout: 120000 });
await page.waitForTimeout(8000);

const remote = await page.evaluate(() =>
  [...document.querySelectorAll("script[src]")].map((s) => s.src).filter((s) => !s.startsWith(location.origin)));
const sel = (kind, id) => page.locator(`[data-${kind}="${id}"]`).first();
const state = async (el, keys) => Object.fromEntries(await Promise.all(keys.map(async (k) => [k, await el.getAttribute(k)])));
const shot = async (el, name) => { await el.scrollIntoViewIfNeeded(); await page.waitForTimeout(700);
  await el.screenshot({ path: `${out}/${name}.png` }); };
async function setInput(el, key, v) {
  const inp = el.locator(`input[id="${key}"], input[id$=".${key}"]`).first();
  await inp.fill(String(v));
  await inp.press("Enter");
  await page.waitForTimeout(900);
}
const preset = async (el, p) => { await el.locator(`[data-preset="${p}"]`).click(); await page.waitForTimeout(900); };
const presetNames = (el) => el.locator("[data-preset]").evaluateAll((bs) => bs.map((b) => b.getAttribute("data-preset")));
async function drag(el, nth, dx, dy) {
  const h = el.locator(".mafs-movable-point").nth(nth);
  await h.scrollIntoViewIfNeeded();
  const b = await h.boundingBox();
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2);
  await page.mouse.down();
  await page.mouse.move(b.x + b.width / 2 + dx, b.y + b.height / 2 + dy, { steps: 8 });
  await page.mouse.up();
  await page.waitForTimeout(900);
}
const r = { url, remote };

try { // the angle of parallelism, tied
  const el = sel("figure", "lobachevsky-tie"), keys = ["data-case", "data-x", "data-pi", "data-t"];
  await el.scrollIntoViewIfNeeded();
  r.tie = { start: await state(el, keys) };
  const before = await page.evaluate(() => window.__lobachevskySolves || 0);
  await setInput(el, "x", 2);
  r.tie.x2 = { ...(await state(el, keys)), solves: (await page.evaluate(() => window.__lobachevskySolves || 0)) - before };
  await setInput(el, "Pi", 60);
  r.tie.pi60 = await state(el, keys);
  r.tie.presets = {};
  for (const p of await presetNames(el)) { await preset(el, p); r.tie.presets[p] = await state(el, keys); }
  await preset(el, "near");
  await shot(el, "tie-euclid");
  await preset(el, "reset");
  r.tie.reset = await state(el, keys);
  await shot(el, "tie");
} catch (e) { r.tie = String(e); }

try { // the triangle
  const el = sel("playground", "hyperbolic-triangle"), keys = ["data-case", "data-sum", "data-area", "data-model"];
  await el.scrollIntoViewIfNeeded();
  r.tri = { start: await state(el, keys) };
  r.tri.badge = await el.locator("[data-invariant]").getAttribute("data-invariant");
  await drag(el, 0, 50, 30);
  r.tri.dragged = await state(el, keys);
  r.tri.presets = {};
  for (const p of await presetNames(el)) { await preset(el, p); r.tri.presets[p] = await state(el, keys); }
  await preset(el, "a large triangle");
  const poinc = await state(el, keys);
  await el.locator('[data-model="1"]').click(); await page.waitForTimeout(900);
  r.tri.klein = { poincare: poinc, klein: await state(el, keys), badge: await el.locator("[data-klein-badge]").getAttribute("data-klein-badge") };
  await shot(el, "triangle-klein");
  await el.locator('[data-model="0"]').click(); await page.waitForTimeout(600);
  await preset(el, "vertices to the rim: an ideal triangle");
  await shot(el, "triangle-ideal");
  await preset(el, "reset");
  r.tri.reset = await state(el, keys);
  r.tri.badgeEnd = await el.locator("[data-invariant]").getAttribute("data-invariant");
  await shot(el, "triangle");
} catch (e) { r.tri = String(e); }

try { // the parallels
  const el = sel("playground", "hyperbolic-parallels"), keys = ["data-case", "data-pi", "data-gap", "data-d"];
  await el.scrollIntoViewIfNeeded();
  r.par = { start: await state(el, keys) };
  await drag(el, 0, 30, -40);
  r.par.dragged = await state(el, keys);
  await preset(el, "limiting");
  r.par.limiting = await state(el, keys);
  await setInput(el, "alpha", 0.1);
  r.par.meets = await state(el, keys);
  await setInput(el, "alpha", 1.57);
  r.par.misses = await state(el, keys);
  r.par.presets = {};
  for (const p of await presetNames(el)) { await preset(el, p); r.par.presets[p] = await state(el, keys); }
  await preset(el, "far");
  await shot(el, "parallels-far");
  await preset(el, "reset");
  r.par.reset = await state(el, keys);
  await shot(el, "parallels");
} catch (e) { r.par = String(e); }

try { // the pseudosphere
  const el = sel("figure", "beltrami-pseudosphere"), keys = ["data-case", "data-k"];
  await el.scrollIntoViewIfNeeded();
  r.ps = { start: await state(el, keys), canvases: await el.locator("canvas").count() };
  r.ps.presets = {};
  for (const p of await presetNames(el)) { await preset(el, p); r.ps.presets[p] = await state(el, keys); }
  await preset(el, "reset");
  await preset(el, "R2");
  r.ps.R2 = await state(el, keys);
  await preset(el, "reset");
  await preset(el, "sphere");
  r.ps.sphere = await state(el, keys);
  await preset(el, "reset");
  r.ps.reset = await state(el, keys);
  r.ps.badge = await el.locator("[data-invariant]").getAttribute("data-invariant");
  await shot(el, "pseudosphere");
} catch (e) { r.ps = String(e); }

r.wasm = await page.evaluate(() => window.__wasm.modules);
r.errors = errors;
await page.screenshot({ path: `${out}/page.png`, fullPage: false });
const near = (a, b, t) => Math.abs(Number(a) - b) < t;
const T = r.tie || {}, R = r.tri || {}, P = r.par || {}, S = r.ps || {};
const PI = Math.PI, deg = 180 / PI;
r.checks = {
  base: errors.length === 0 && remote.length === 0 && r.wasm > 0,
  // wasm's atan is good to about 1e-4 rad (the gauss e2e allows the same)
  tieForward: near(T.x2?.["data-pi"], 2 * Math.atan(Math.exp(-2)) * deg, 0.02) && near(T.x2?.["data-t"], Math.exp(-2), 1e-6),
  tieOneSolve: T.x2?.solves === 1,
  tieInverse: near(T.pi60?.["data-x"], -Math.log(Math.tan(PI / 6)), 1e-3),
  tieEuclid: T.presets?.near?.["data-case"] === "euclid" && near(T.presets?.near?.["data-pi"], 90, 1),
  tieFar: T.presets?.far?.["data-case"] === "far",
  tieReset: near(T.reset?.["data-x"], 0.8, 1e-9),
  triDrag: R.dragged && R.dragged["data-sum"] !== R.start?.["data-sum"],
  gaussBonnet: [R.start, R.dragged, ...Object.values(R.presets || {})].every((s) => s && near(Number(s["data-area"]) + Number(s["data-sum"]), PI, 1e-9)),
  triEuclid: R.presets?.["near the centre: almost Euclidean"]?.["data-case"] === "euclidean",
  triIdeal: R.presets?.["vertices to the rim: an ideal triangle"]?.["data-case"] === "ideal",
  triLarge: R.presets?.["a large triangle"]?.["data-case"] === "hyperbolic",
  klein: R.klein?.klein?.["data-model"] === "1" && near(R.klein?.klein?.["data-sum"], Number(R.klein?.poincare?.["data-sum"]), 1e-6) && R.klein?.badge === "drawn",
  triBadge: R.badge === "pass" && R.badgeEnd === "pass",
  triReset: R.reset?.["data-case"] === R.start?.["data-case"] && near(R.reset?.["data-sum"], Number(R.start?.["data-sum"]), 1e-3),
  parDrag: P.dragged && P.dragged["data-d"] !== P.start?.["data-d"],
  parLimiting: P.limiting?.["data-case"] === "limiting",
  parMeets: P.meets?.["data-case"] === "meets",
  parMisses: P.misses?.["data-case"] === "misses",
  parReset: near(P.reset?.["data-d"], Number(P.start?.["data-d"]), 1e-9),
  psK: S.start?.["data-case"] === "pseudosphere" && near(S.start?.["data-k"], -1, 1e-6),
  psR2: near(S.R2?.["data-k"], -4, 1e-5),
  psSphere: S.sphere?.["data-case"] === "sphere" && near(S.sphere?.["data-k"], 1, 1e-5),
  psBadge: S.badge === "pass",
  mathbox: S.canvases > 0,
};
r.ok = Object.values(r.checks).every(Boolean);
console.log(JSON.stringify(r));
await browser.close();
process.exit(r.ok ? 0 : 1);
