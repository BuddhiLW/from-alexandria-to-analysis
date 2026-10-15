// Browser check of the Gauss notebook on a static build:
//   node dev/gauss_e2e.mjs <url> <out-dir>
// Zero page errors, no remote <script>, wasm modules instantiated (raster
// kernels); the 17-gon player steps to the end; the n-gon playground names
// the case live (n = 17 constructible, 7 and 9 not, 51 constructible),
// reads 2cos(2pi/n) from its kernel, follows a drag of P1, presses every
// preset; the egregium playground keeps K = K0 under a bending and breaks
// it under a stretch, every preset pressed, reset restores the helicoid.
// Screenshots into <out-dir>. PLAYWRIGHT names a directory holding
// node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:8093/gauss/", out = "target/gauss-e2e"] = process.argv.slice(2);
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
const r = { url, remote };

try { // the period tower
  await fig("gauss-periods").scrollIntoViewIfNeeded();
  for (let i = 0; i < 6; i++) await fig("gauss-periods").locator('[data-act="next"]').click();
  r.periods = { step: await attr("gauss-periods", "data-step"),
                formula: await fig("gauss-periods").locator('[data-formula="period-step"]').innerText() };
  await shot("gauss-periods");
} catch (e) { r.periods = String(e); }

try { // the n-gon
  const id = "gauss-ngon";
  await fig(id).scrollIntoViewIfNeeded();
  const c17 = await attr(id, "data-case"), tc17 = Number(await attr(id, "data-twocos"));
  await setInput(id, "n", 7);
  const c7 = await attr(id, "data-case"), k7 = await attr(id, "data-constructible");
  const presets = {};
  for (const p of ["9", "51", "7", "17"]) { await preset(id, p); presets[p] = { n: await attr(id, "data-n"), ok: await attr(id, "data-constructible"), name: await attr(id, "data-case") }; }
  await shot(id, "gauss-ngon-17");
  // drag P1 (the movable point) to the angle 2pi/5
  const box = await fig(id).locator("svg").first().boundingBox();
  const handle = fig(id).locator(".mafs-movable-point").first();
  const hb = await handle.boundingBox();
  const cx = box.x + box.width / 2, cy = box.y + box.height / 2, R = box.width / 2.6;
  await page.mouse.move(hb.x + hb.width / 2, hb.y + hb.height / 2);
  await page.mouse.down();
  await page.mouse.move(cx + R * Math.cos(2 * Math.PI / 5), cy - R * Math.sin(2 * Math.PI / 5), { steps: 12 });
  await page.mouse.up();
  await page.waitForTimeout(800);
  const dragN = await attr(id, "data-n");
  await preset(id, "reset");
  r.ngon = { c17, tc17, c7, k7, presets, dragN, resetN: await attr(id, "data-n") };
  await shot(id, "gauss-ngon");
} catch (e) { r.ngon = String(e); }

try { // the theorema egregium
  const id = "gauss-egregium";
  await fig(id).scrollIntoViewIfNeeded();
  const k0 = { case: await attr(id, "data-case"), K: Number(await attr(id, "data-k")) };
  await setInput(id, "t", 0.6);
  const bent = { case: await attr(id, "data-case"), K: Number(await attr(id, "data-k")), K0: Number(await attr(id, "data-k0")),
                 inv: await attr(id, "data-invariant") };
  await setInput(id, "s", 1.5);
  const stretched = { case: await attr(id, "data-case"), inv: await attr(id, "data-invariant"),
                      badge: await fig(id).locator('[data-hypothesis="egregium"]').innerText() };
  const presets = {};
  for (const p of ["catenoid", "stretch", "cylinder", "lift", "play"]) {
    await preset(id, p);
    presets[p] = { case: await attr(id, "data-case"), inv: await attr(id, "data-invariant"),
                   K: Number(await attr(id, "data-k")), Ks: Number(await attr(id, "data-ks")) };
    if (p === "cylinder" || p === "stretch") await shot(id, `gauss-egregium-${p}`);
  }
  await preset(id, "reset");
  r.egregium = { k0, bent, stretched, presets, reset: await attr(id, "data-case"),
                 canvases: await fig(id).locator("canvas").count() };
  await shot(id, "gauss-egregium");
} catch (e) { r.egregium = String(e); }

r.wasm = await page.evaluate(() => window.__wasm.modules);
r.errors = errors;
await page.screenshot({ path: `${out}/page.png` });
const near = (a, b, t) => Math.abs(a - b) < t;
const E = r.egregium || {}, N = r.ngon || {};
r.ok = errors.length === 0 && remote.length === 0 && r.wasm > 0 &&
  r.periods?.step === "4" && String(r.periods?.formula).replace(/\s/g, "").includes("0.932472") &&
  String(N.c17).startsWith("constructible") && near(N.tc17, 2 * Math.cos(2 * Math.PI / 17), 1e-4) /* wasm cos: ~1e-5 */ &&
  String(N.c7).startsWith("not constructible") && N.k7 === "false" &&
  N.presets?.["9"]?.ok === "false" && String(N.presets?.["9"]?.name).includes("3²") &&
  N.presets?.["51"]?.ok === "true" && N.dragN === "5" && N.resetN === "17" &&
  String(E.k0?.case).includes("helicoid") && E.bent?.inv === "true" && near(E.bent?.K, E.bent?.K0, 1e-9) &&
  String(E.bent?.case).includes("Theorema egregium") &&
  E.stretched?.inv === "false" && String(E.stretched?.case).startsWith("stretched") &&
  E.presets?.catenoid?.inv === "true" && E.presets?.stretch?.inv === "false" &&
  E.presets?.cylinder?.inv === "true" && near(E.presets?.cylinder?.K, 0, 1e-9) &&
  E.presets?.lift?.inv === "false" && near(E.presets?.lift?.K, E.presets?.lift?.Ks, 1e-6) &&
  String(E.reset).includes("helicoid") && E.canvases > 0;
console.log(JSON.stringify(r));
await browser.close();
process.exit(r.ok ? 0 : 1);
