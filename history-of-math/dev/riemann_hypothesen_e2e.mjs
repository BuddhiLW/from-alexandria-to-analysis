// Browser check of the Riemann notebook (Ueber die Hypothesen, 1854) on a
// static build:
//   node dev/riemann_hypothesen_e2e.mjs <url> <out-dir>
// Zero page errors, no remote <script>, wasm modules instantiated (raster
// kernels). Line-element playground: Euclid's plane is flat; setting
// alpha names positive / negative curvature with K = alpha and the green
// constant-curvature badge; the tied det A re-solves g22 (one solve per
// edit); the hill turns the badge red; a degenerate form names
// "degenerate"; dragging P moves the readout; every preset pressed, reset
// restores the plane. Geodesic playground: sphere converges, saddle
// diverges, plane parallel, torus outside converges / inside diverges,
// RK4 keeps the speed 1, dragging P changes the paths, every preset
// pressed, MathBox canvas present. Screenshots into <out-dir>.
// PLAYWRIGHT names a directory holding node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:8094/riemann-hypothesen/", out = "target/riemann-e2e"] = process.argv.slice(2);
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
const fig = (id) => page.locator(`[data-figure="${id}"]`).first();
const attr = (id, a) => fig(id).getAttribute(a);
const shot = async (id, name = id) => { await fig(id).scrollIntoViewIfNeeded(); await page.waitForTimeout(700);
  await fig(id).screenshot({ path: `${out}/${name}.png` }); };
async function setInput(id, key, v) {
  const inp = fig(id).locator(`input[id="${key}"], input[id$=".${key}"]`).first();
  await inp.fill(String(v));
  await inp.press("Enter");
  await page.waitForTimeout(900);
}
const preset = async (id, p) => { await fig(id).locator(`[data-preset="${p}"]`).click(); await page.waitForTimeout(900); };
const presetNames = (id) => fig(id).locator("[data-preset]").evaluateAll((bs) => bs.map((b) => b.getAttribute("data-preset")));
async function drag(id, nth, dx, dy) {
  const h = fig(id).locator(".mafs-movable-point").nth(nth);
  await h.scrollIntoViewIfNeeded();
  const b = await h.boundingBox();
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2);
  await page.mouse.down();
  await page.mouse.move(b.x + b.width / 2 + dx, b.y + b.height / 2 + dy, { steps: 8 });
  await page.mouse.up();
  await page.waitForTimeout(900);
}
const r = { url, remote };
const state = async (id, keys) => Object.fromEntries(await Promise.all(keys.map(async (k) => [k, await attr(id, k)])));

try { // the line element
  const id = "riemann-metric", keys = ["data-case", "data-k", "data-det", "data-constant"];
  await fig(id).scrollIntoViewIfNeeded();
  r.metric = { plane: await state(id, keys) };
  await setInput(id, "alpha", 1);
  r.metric.pos = await state(id, keys);
  await setInput(id, "alpha", -1);
  r.metric.neg = await state(id, keys);
  const before = await page.evaluate(() => window.__riemannSolves || 0);
  await setInput(id, "d", 2);
  r.metric.tied = { solves: (await page.evaluate(() => window.__riemannSolves || 0)) - before,
                    d: await fig(id).locator("[data-d]").innerText(), ...(await state(id, keys)) };
  await setInput(id, "h", 1.5);
  r.metric.hill = await state(id, keys);
  const k0 = await attr(id, "data-k");
  await drag(id, 0, 60, -40);
  r.metric.dragged = { before: k0, after: await attr(id, "data-k") };
  r.metric.presets = {};
  for (const p of await presetNames(id)) { await preset(id, p); r.metric.presets[p] = await state(id, keys); }
  await preset(id, "make det g ≤ 0: no line element");
  await shot(id, "riemann-metric-degenerate");
  await preset(id, "reset");
  r.metric.reset = await state(id, keys);
  await shot(id, "riemann-metric");
} catch (e) { r.metric = String(e); }

try { // geodesics
  const id = "riemann-geodesics", keys = ["data-case", "data-ratio", "data-k", "data-speed", "data-kind"];
  await fig(id).scrollIntoViewIfNeeded();
  r.geo = { start: await state(id, keys), presets: {} };
  for (const p of await presetNames(id)) { await preset(id, p); r.geo.presets[p] = await state(id, keys); }
  await preset(id, "reset");
  const before = await attr(id, "data-ratio");
  await drag(id, 0, 40, 30);
  r.geo.dragged = { before, after: await attr(id, "data-ratio") };
  r.geo.canvases = await fig(id).locator("canvas").count();
  await preset(id, "torus, inside (K < 0): they diverge");
  await shot(id, "riemann-geodesics-torus");
  await preset(id, "sphere: two great circles from P meet again at length π");
  await shot(id, "riemann-geodesics");
} catch (e) { r.geo = String(e); }

r.wasm = await page.evaluate(() => window.__wasm.modules);
r.errors = errors;
await page.screenshot({ path: `${out}/page.png` });
const near = (a, b, t) => Math.abs(Number(a) - b) < t;
const M = r.metric || {}, G = r.geo || {}, P = G.presets || {};
const caseOf = (p) => String(P[p]?.["data-case"]);
r.checks = {
  base: errors.length === 0 && remote.length === 0 && r.wasm > 0,
  plane: String(M.plane?.["data-case"]).startsWith("flat") && M.plane?.["data-constant"] === "true",
  pos: String(M.pos?.["data-case"]).startsWith("positive") && near(M.pos?.["data-k"], 1, 1e-4) && M.pos?.["data-constant"] === "true",
  neg: String(M.neg?.["data-case"]).startsWith("negative") && near(M.neg?.["data-k"], -1, 1e-4),
  tied: M.tied?.solves === 1 && near(M.tied?.["data-det"], 0, 1e9),
  hill: M.hill?.["data-constant"] === "false",
  drag: M.dragged && M.dragged.before !== M.dragged.after,
  degenerate: String(M.presets?.["make det g ≤ 0: no line element"]?.["data-case"]).startsWith("degenerate"),
  reset: String(M.reset?.["data-case"]).startsWith("flat"),
  sphere: caseOf("sphere: two great circles from P meet again at length π").startsWith("converge"),
  saddle: caseOf("saddle: the same opening, the geodesics fly apart").startsWith("diverge"),
  torusOut: caseOf("torus, outside (K > 0): they converge").startsWith("converge"),
  torusIn: caseOf("torus, inside (K < 0): they diverge").startsWith("diverge"),
  plane2: caseOf("plane: Euclid's straight lines, the gap grows as 2L sin δ").startsWith("parallel"),
  speed: Object.values(P).every((s) => near(s["data-speed"], 1, 1e-4)),
  geoDrag: G.dragged && G.dragged.before !== G.dragged.after,
  mathbox: G.canvases > 0,
};
r.ok = Object.values(r.checks).every(Boolean);
console.log(JSON.stringify(r));
await browser.close();
process.exit(r.ok ? 0 : 1);
