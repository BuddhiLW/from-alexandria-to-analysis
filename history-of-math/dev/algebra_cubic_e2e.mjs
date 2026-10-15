// Browser check of the algebra_cubic notebook on a static build:
//   node dev/algebra_cubic_e2e.mjs <url> <out-dir>
// Zero page errors, no remote <script>, wasm modules instantiated (raster
// kernels); al-Khwarizmi's player steps and his widget gives x = 3 for ten
// roots and thirty-nine, the inverse (x moved) gives c; Cardano's widget
// gives x = 2 for the cube and six things equal twenty, with its cube drawn;
// on the complex plane p and q are moved through D = 0 into the irreducible
// case (x^3 = 15x + 4: x = 4, the case named live, conjugate cube roots) and
// back out, the root checked back in the equation each time; Viete's widget
// shows the coefficients. Screenshots into <out-dir>. PLAYWRIGHT names a
// directory holding node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:8097/algebra-cubic/", out = "target/algebra-cubic-e2e"] = process.argv.slice(2);
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
const sel = (id, s) => fig(id).locator(s).first();
const shot = async (id, name = id) => { await fig(id).scrollIntoViewIfNeeded(); await page.waitForTimeout(800);
  await fig(id).screenshot({ path: `${out}/${name}.png` }); };
async function setInput(id, key, v) {
  const inp = fig(id).locator(`input[id="${key}"], input[id$=".${key}"]`).first();
  await inp.fill(String(v));
  await inp.press("Enter");
  await page.waitForTimeout(1000);
}
const preset = async (id, p) => { await sel(id, `[data-preset="${p}"]`).click(); await page.waitForTimeout(1000); };
const num = (s) => Number(String(s).replace("−", "-"));
const r = { url, remote };

try { // al-Khwarizmi
  const id = "khwarizmi-square";
  await fig(id).scrollIntoViewIfNeeded();
  const x0 = num(await sel(id, "[data-square-root]").getAttribute("data-square-root"));
  await sel(id, '[data-act="prev"]').click(); await sel(id, '[data-act="prev"]').click();
  const stepText = await sel(id, "[data-step-text]").innerText();
  await setInput(id, "x", 4);
  const c4 = await sel(id, "[data-square-formula]").getAttribute("data-formula");
  await setInput(id, "b", 6);
  const x6 = num(await sel(id, "[data-square-root]").getAttribute("data-square-root"));
  await preset(id, 0);
  r.square = { x0, stepText, c4, x6, back: num(await sel(id, "[data-square-root]").getAttribute("data-square-root")),
               solves: await page.evaluate(() => globalThis.__tie_khwarizmi_square) };
  await shot(id, "square");
} catch (e) { r.square = String(e); }

try { // Cardano's cube
  const id = "cardano-cube";
  await fig(id).scrollIntoViewIfNeeded();
  r.cube = { rule: await sel(id, "[data-rule]").getAttribute("data-formula"),
             check: num(await sel(id, "[data-check]").getAttribute("data-check")),
             case: await sel(id, "[data-case]").getAttribute("data-case"),
             canvases: await fig(id).locator("canvas").count(),
             noCube: await fig(id).locator("[data-no-cube]").count() };
  await shot(id, "cardano-cube");
  await preset(id, 2);
  r.cube.preset9 = await sel(id, "[data-rule]").getAttribute("data-formula");
} catch (e) { r.cube = String(e); }

try { // Bombelli: move p and q through D = 0 into the irreducible case
  const id = "bombelli-plane";
  await fig(id).scrollIntoViewIfNeeded();
  const read = async () => ({ case: await sel(id, "[data-case]").getAttribute("data-case"),
    rule: await sel(id, "[data-rule]").getAttribute("data-formula"),
    eq: await sel(id, "[data-equation]").getAttribute("data-formula"),
    check: num(await sel(id, "[data-check]").getAttribute("data-check")) });
  const start = await read();
  await setInput(id, "p", 6); await setInput(id, "q", 20);
  const real = await read();
  const walk = [];
  for (const p of [0, -6, -12, -15]) { await setInput(id, "p", p); walk.push({ p, ...(await read()) }); }
  await setInput(id, "q", 4);
  const irr = await read();
  await shot(id, "bombelli-irreducible");
  await setInput(id, "q", 16); await setInput(id, "p", -12);
  const dbl = await read();
  await setInput(id, "q", 25);
  const out1 = await read();
  await preset(id, 0);
  r.plane = { start, real, walk, irr, dbl, out1, back: await read(),
              solves: await page.evaluate(() => globalThis.__tie_bombelli_plane) };
} catch (e) { r.plane = String(e); }

try { // Viete
  const id = "viete-roots";
  await fig(id).scrollIntoViewIfNeeded();
  const c0 = await sel(id, "[data-coefficients]").getAttribute("data-formula");
  await setInput(id, "r1", 2);
  r.viete = { c0, c1: await sel(id, "[data-coefficients]").getAttribute("data-formula"),
              grades: await page.locator("[data-grade]").count(),
              fails: await page.locator('[data-grade="fails"]').count() };
  await shot(id, "viete");
} catch (e) { r.viete = String(e); }

r.badgesFail = await page.getByText("FAILS:").count();
r.wasm = await page.evaluate(() => window.__wasm.modules);
r.errors = errors;
await page.screenshot({ path: `${out}/page.png`, fullPage: false });
const near = (a, b, t) => Math.abs(a - b) < t;
const S = r.square || {}, C = r.cube || {}, P = r.plane || {}, V = r.viete || {};
const has = (s, t) => String(s).replace(/\s/g, "").includes(t);
r.ok = errors.length === 0 && remote.length === 0 && r.wasm > 0 && r.badgesFail === 0 &&
  near(S.x0, 3, 1e-6) && has(S.stepText, "quadrateremains") && has(S.c4, "x^2+10.00\\,x=56.00") &&
  near(S.x6, Math.sqrt(65) - 3, 1e-6) /* b = 6, c held at 56 */ && near(S.back, 3, 1e-6) &&
  has(C.rule, "=2.0000") && near(C.check, 0, 1e-6) && C.case === "one-real" && C.canvases > 0 && C.noCube === 0 &&
  has(C.preset9, "=2.0000") &&
  P.start?.case === "irreducible" && has(P.start?.rule, "=4.0000") && P.real?.case === "one-real" &&
  P.walk?.some((w) => w.case === "one-real") && P.walk?.[3]?.case === "irreducible" &&
  P.irr?.case === "irreducible" && has(P.irr?.rule, "2.0000+1.0000\\,i") && has(P.irr?.rule, "=4.0000") &&
  near(P.irr?.check, 0, 5e-3) /* wasm cos/sin ~1e-5 on the cube roots */ && P.dbl?.case === "double" && has(P.dbl?.rule, "=4.0000") &&
  P.out1?.case === "one-real" && near(P.out1?.check, 0, 1e-6) && P.back?.case === "irreducible" &&
  has(V.c0, "(2.000)") && V.c0 !== V.c1 && V.grades > 0 && V.fails === 0;
console.log(JSON.stringify(r));
await browser.close();
process.exit(r.ok ? 0 : 1);
