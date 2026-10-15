// Browser check of the Erlangen notebook (Klein 1872) on a static build:
//   node dev/klein_erlangen_e2e.mjs <url> <out-dir>
// Zero page errors, no remote <script>, wasm modules instantiated (raster
// kernels). Hierarchy player: the properties kept fall 8 -> 6 -> 3 -> 1
// along Euclid < similarity < affine < projective. Playground: at the start
// (identity) all eight properties are invariant; a slider of each group
// changes the map and the live case name; dragging a vertex changes the
// figure; dragging the red handle translates (a Euclidean motion keeps
// length); every "try this" preset gives the expected invariant set (shear:
// parallels, midpoint, cross-ratio; projective: cross-ratio only; Moebius:
// angle, cross-ratio, circle, hyperbolic length) and case name; reset.
// Screenshots into <out-dir>. PLAYWRIGHT names a directory holding
// node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:8096/klein-erlangen/", out = "target/klein-erlangen-e2e"] = process.argv.slice(2);
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
const click = async (loc) => { await loc.first().click(); await page.waitForTimeout(800); };
async function slide(el, param, v) {
  await el.locator(`input[data-param="${param}"]`).first().evaluate((inp, v) => {
    const set = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value").set;
    set.call(inp, String(v));
    inp.dispatchEvent(new Event("input", { bubbles: true }));
    inp.dispatchEvent(new Event("change", { bubbles: true }));
  }, v);
  await page.waitForTimeout(800);
}
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
const badges = (el) => el.locator("tr[data-property]").evaluateAll((rs) =>
  Object.fromEntries(rs.map((r) => [r.getAttribute("data-property"), r.getAttribute("data-invariant")])));
const r = { url, remote };

try { // the hierarchy player
  const el = sel("figure", "erlangen-hierarchy"), keys = ["data-step", "data-kept"];
  await el.scrollIntoViewIfNeeded();
  r.player = [await state(el, keys)];
  for (let i = 0; i < 3; i++) { await click(el.locator('[data-act="next"]')); r.player.push(await state(el, keys)); }
  await shot(el, "hierarchy-projective");
  await click(el.locator('[data-act="first"]'));
  r.playerBack = await state(el, keys);
} catch (e) { r.player = String(e); }

try { // the playground
  const el = sel("playground", "erlangen"), keys = ["data-group", "data-case", "data-kept"];
  await el.scrollIntoViewIfNeeded();
  r.pg = { start: await state(el, keys), startBadges: await badges(el) };
  await slide(el, "theta", 0.8);
  r.pg.rotated = await state(el, keys);
  await drag(el, 3, 40, -30); // the translation handle
  r.pg.translated = { ...(await state(el, keys)), badges: await badges(el) };
  await shot(el, "playground-motion");
  await drag(el, 0, 30, 25); // vertex A
  r.pg.vertex = { ...(await state(el, keys)), badges: await badges(el) };
  await click(el.locator('button[data-group$="similarity"]'));
  await slide(el, "s", 1.7);
  r.pg.scaled = { ...(await state(el, keys)), badges: await badges(el) };
  await click(el.locator('button[data-group$="affine"]'));
  await slide(el, "k", 0.7);
  r.pg.sheared = { ...(await state(el, keys)), badges: await badges(el) };
  await click(el.locator('button[data-group$="projective"]'));
  await slide(el, "p", 0.3);
  r.pg.projected = { ...(await state(el, keys)), badges: await badges(el) };
  r.pg.presets = {};
  for (const p of ["shear", "projective", "moebius", "similarity", "subgroup"]) {
    await click(el.locator(`[data-preset="${p}"]`));
    r.pg.presets[p] = { ...(await state(el, keys)), badges: await badges(el) };
    await shot(el, `playground-${p}`);
  }
  await click(el.locator('[data-preset="moebius"]'));
  await drag(el, 3, 25, 20); // the disk point a
  r.pg.moebiusDrag = { ...(await state(el, keys)), badges: await badges(el) };
  await click(el.locator('[data-preset="reset"]'));
  r.pg.reset = await state(el, keys);
} catch (e) { r.pg = String(e); }

r.table = await page.locator('[data-table="erlangen-invariants"] tbody tr').count();
r.failedBadges = await page.locator("text=FAILS:").count();
r.wasm = await page.evaluate(() => window.__wasm.modules);
r.errors = errors;
await page.screenshot({ path: `${out}/page.png`, fullPage: false });
const P = r.player || [], G = r.pg || {}, S = G.presets || {};
const kept = (s) => s?.["data-kept"];
const kc = (s) => (kept(s) ? kept(s).split(",").length : 0);
r.checks = {
  base: errors.length === 0 && remote.length === 0 && r.wasm > 0,
  noFailedBadge: r.failedBadges === 0,
  table: r.table === 5,
  player: Array.isArray(P) && P.map((s) => s["data-kept"]).join(" ") === "7 6 3 1" && r.playerBack?.["data-step"] === "0",
  start: kc(G.start) === 8 && G.start?.["data-case"] === "identity" && Object.values(G.startBadges || {}).every((v) => v === "yes"),
  rotated: G.rotated?.["data-case"] === "euclid",
  translated: G.translated?.["data-case"] === "euclid" && G.translated?.badges?.length === "yes" && G.translated?.badges?.hyp === "no",
  vertexDrag: G.vertex?.badges?.length === "yes",
  scaled: G.scaled?.["data-case"] === "similarity" && G.scaled?.badges?.length === "no" && G.scaled?.badges?.angle === "yes",
  sheared: G.sheared?.["data-case"] === "affine" && G.sheared?.badges?.angle === "no" && G.sheared?.badges?.midpoint === "yes" && G.sheared?.badges?.parallel === "yes",
  projected: G.projected?.["data-case"] === "projective" && G.projected?.badges?.parallel === "no" && G.projected?.badges?.["cross-ratio"] === "yes",
  presetShear: kept(S.shear) === "3,4,5" && S.shear?.["data-case"] === "affine",
  presetProjective: kept(S.projective) === "5" && S.projective?.["data-case"] === "projective",
  presetMoebius: kept(S.moebius) === "1,5,6,7" && S.moebius?.["data-case"] === "hyperbolic",
  presetSimilarity: kept(S.similarity)?.startsWith("1,2,3,4,5,6") && S.similarity?.["data-case"] === "similarity",
  presetSubgroup: S.subgroup?.["data-group"] === "projective" && S.subgroup?.["data-case"] === "euclid",
  moebiusDrag: G.moebiusDrag?.badges?.hyp === "yes" && G.moebiusDrag?.badges?.length === "no",
  reset: G.reset?.["data-case"] === "identity" && kc(G.reset) === 8,
};
r.ok = Object.values(r.checks).every(Boolean);
console.log(JSON.stringify(r));
await browser.close();
process.exit(r.ok ? 0 : 1);
