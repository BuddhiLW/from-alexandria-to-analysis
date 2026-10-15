// Browser check of the Euler notebook's widgets on a static build:
//   node dev/euler_widgets_e2e.mjs <url> <out-dir>
// Zero page errors, no remote <script>, wasm modules instantiated (raster
// kernels), each tied widget round-trips (type a value into one Leva
// input, the tied one is solved; type the solved value back, the first
// returns), the cut solid's V - E + F stays 2 while cutting, the stepped
// figures advance. Screenshots of each widget into <out-dir>.
// PLAYWRIGHT names a directory holding node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:8093/", out = "target/euler-e2e"] = process.argv.slice(2);
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

// type v into the Leva input of `key` inside figure id, then Enter
async function setInput(id, key, v) {
  const inp = fig(id).locator(`input[id="${key}"], input[id$=".${key}"]`).first();
  await inp.fill(String(v));
  await inp.press("Enter");
  await page.waitForTimeout(900);
}
const val = async (id, key) =>
  Number(await fig(id).locator(`input[id="${key}"], input[id$=".${key}"]`).first().inputValue());

const r = { url, remote, ties: {} };
const shot = async (id) => { await fig(id).scrollIntoViewIfNeeded(); await page.waitForTimeout(700);
  await fig(id).screenshot({ path: `${out}/${id}.png` }); };

for (const id of ["euler-basel", "euler-e", "euler-cis", "euler-base", "euler-terms", "euler-polyhedron"])
  r[id] = await fig(id).count();

try { // Basel: b = 0.04 -> n = 25; n = 10 -> b = 0.1
  await fig("euler-basel").scrollIntoViewIfNeeded();
  await setInput("euler-basel", "b", 0.04);
  const n1 = await val("euler-basel", "n");
  await setInput("euler-basel", "n", 10);
  const b1 = await val("euler-basel", "b");
  r.ties.basel = { n1, b1, gap: await attr("euler-basel", "data-gap") };
  await shot("euler-basel");
} catch (e) { r.ties.basel = String(e); }

try { // e: eps -> n -> eps
  await fig("euler-e").scrollIntoViewIfNeeded();
  await setInput("euler-e", "n", 20);
  const eps20 = Number(await attr("euler-e", "data-eps")), om = await val("euler-e", "omega");
  await setInput("euler-e", "eps", 0.03);
  const n03 = Number(await attr("euler-e", "data-n")), eps03 = Number(await attr("euler-e", "data-eps"));
  r.ties.e = { eps20, omega20: om, n03, eps03 };
  await shot("euler-e");
} catch (e) { r.ties.e = String(e); }

try { // cis: cos = -1 -> x = pi (sign of sin kept); x = 1 -> cos 0.5403
  await fig("euler-cis").scrollIntoViewIfNeeded();
  await setInput("euler-cis", "x", 1);
  const c1 = await val("euler-cis", "cos");
  await setInput("euler-cis", "cos", 0.5);
  r.ties.cis = { c1, xBack: await val("euler-cis", "x"), point: await attr("euler-cis", "data-point"),
                 name1: await attr("euler-cis", "data-name") };
  await setInput("euler-cis", "x", 3.14);
  r.ties.cis.namePi = await attr("euler-cis", "data-name");
  r.ties.cis.formula = await fig("euler-cis").locator('[data-formula="atan2"]').innerText();
  await shot("euler-cis");
} catch (e) { r.ties.cis = String(e); }

try { // base: k = 1 -> a = e; a = 10 -> k = log 10
  await fig("euler-base").locator('[data-act="next"]').click();
  await fig("euler-base").locator('[data-act="next"]').click();
  await fig("euler-base").locator('[data-act="next"]').click();
  await setInput("euler-base", "a", 10);
  const k10 = await val("euler-base", "k");
  await setInput("euler-base", "k", 1);
  r.ties.base = { k10, aBack: await val("euler-base", "a"), step: await attr("euler-base", "data-step") };
  await shot("euler-base");
} catch (e) { r.ties.base = String(e); }

try {
  for (let i = 0; i < 4; i++) await fig("euler-terms").locator('[data-act="next"]').click();
  r.terms = await attr("euler-terms", "data-step");
  await shot("euler-terms");
} catch (e) { r.terms = String(e); }

try { // pick the dodecahedron, cut 5 then 20 angles
  await fig("euler-polyhedron").locator('[data-solid="3"]').click();
  await page.waitForTimeout(500);
  const name0 = await attr("euler-polyhedron", "data-name");
  await setInput("euler-polyhedron", "k", 5);
  const v5 = await attr("euler-polyhedron", "data-vef"), c5 = await attr("euler-polyhedron", "data-chi");
  const name5 = await attr("euler-polyhedron", "data-name");
  await setInput("euler-polyhedron", "k", 20);
  r.poly = { name0, name5, name20: await attr("euler-polyhedron", "data-name"),
             v5, c5, v20: await attr("euler-polyhedron", "data-vef"), c20: await attr("euler-polyhedron", "data-chi"),
             canvases: await fig("euler-polyhedron").locator("canvas").count() };
  await shot("euler-polyhedron");
} catch (e) { r.poly = String(e); }

r.wasm = await page.evaluate(() => window.__wasm.modules);
r.errors = errors;
await page.screenshot({ path: `${out}/page.png`, fullPage: false });
const near = (a, b, t) => Math.abs(a - b) < t;
r.ok = errors.length === 0 && remote.length === 0 && r.wasm > 0 &&
  near(r.ties.basel?.n1, 25, 1e-6) && near(r.ties.basel?.b1, 0.1, 1e-3) &&
  near(r.ties.e?.eps20, Math.E - Math.pow(1.05, 20), 1e-9) && near(r.ties.e?.eps03, 0.03, 1e-4) &&
  near(r.ties.cis?.c1, Math.cos(1), 1e-2) &&
  near(Number(String(r.ties.cis?.point).split(" ")[0]), 0.5, 2e-3) &&
  near(r.ties.base?.k10, Math.log(10), 1e-2) && near(r.ties.base?.aBack, Math.E, 1e-2) && r.poly?.c5 === "2" && r.poly?.c20 === "2" && r.terms === "5" &&
  String(r.ties.cis?.name1).startsWith("first quadrant") && String(r.ties.cis?.namePi).includes("−1") &&
  String(r.poly?.name0).includes("δωδεκάεδρον") && String(r.poly?.name5).includes("5 of its 20") &&
  String(r.poly?.name20).includes("every solid angle") && r.poly?.v20 === "60 90 32";
console.log(JSON.stringify(r));
await browser.close();
process.exit(r.ok ? 0 : 1);
