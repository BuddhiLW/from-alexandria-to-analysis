// Drives the Copernicus notebook of a static build in Chromium:
//   node copernicus_e2e.mjs <copernicus-url> <shots-dir>
// (serve the build over http, e.g. python3 -m http.server in the out dir).
// Retrograde playground: Mars starts "outer planet ... loops backwards" with
// at least one arc and an RK4 gap < 1e-3; the run animates the motion name
// through retrograde and direct; dragging the planet changes a (the
// nearest-point rate moves); the frame slider f redraws the path; dragging
// the time marker moves λ′; Venus/Mercury read "inner planet", the faster
// outer planet "no retrograde"; reset restores Mars.
// Synodic tie: Leva S re-solves P (one tie solve per edit, residual ~0);
// presets switch outer/inner; reset restores Mars.
// Side by side: Mars at m = 1 is "same sky" (worst < 1e-4); dragging k
// leaves the sky the same; m = 0.6 / 1.4 presets part the lines of sight.
// Order: Copernicus' order holds at start; the "disagree"/"broken" presets
// rename it; dragging Saturn inside the Earth's orbit reads "broken" and
// reorders the table; reset restores.
// Every page: 0 page errors, no remote scripts, WebAssembly in use.
// Prints one JSON line; exit 1 on failure. PLAYWRIGHT names a directory
// holding node_modules/playwright; CHROMIUM a browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "copernicus-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });
const browser = await chromium.launch({
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});

const page = await browser.newPage({ viewport: { width: 1300, height: 950 } });
const errors = [], remote = [];
const origin = new URL(url).origin;
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text().slice(0, 300)); });
page.on("request", (r) => { if (r.resourceType() === "script" && !r.url().startsWith(origin)) remote.push(r.url()); });
await page.addInitScript(() => {
  const W = globalThis.WebAssembly;
  const counts = (globalThis.__wasm = { instantiate: 0, instance: 0 });
  const inst = W.instantiate.bind(W);
  W.instantiate = (...a) => { counts.instantiate++; return inst(...a); };
  W.Instance = new Proxy(W.Instance, { construct(t, a, nt) { counts.instance++; return Reflect.construct(t, a, nt); } });
});
await page.goto(url, { waitUntil: "networkidle" });
await page.waitForTimeout(3000);

const wait = (ms = 500) => page.waitForTimeout(ms);
const num = async (loc, attr) => Number(await loc.getAttribute(attr));
async function drag(handle, dx, dy) {
  await handle.scrollIntoViewIfNeeded();
  const b = await handle.boundingBox();
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2);
  await page.mouse.down();
  await page.mouse.move(b.x + b.width / 2 + dx, b.y + b.height / 2 + dy, { steps: 12 });
  await page.mouse.up();
  await wait(700);
}
// Leva gives each input the id of its schema key
async function levaSet(root, key, value) {
  const input = root.locator(`input[id="${key}"]`).first();
  await input.fill(String(value));
  await input.press("Enter");
  await wait(900);
}
async function pressAll(root, attr = "data-case", ms = 900) {
  const out = {};
  for (const name of await root.locator("[data-preset]").evaluateAll((b) => b.map((x) => x.getAttribute("data-preset")))) {
    await root.locator(`[data-preset="${name}"]`).click(); await wait(ms);
    out[name] = await root.getAttribute(attr);
  }
  return out;
}
const svgOf = (root) => root.locator("svg").evaluateAll((s) => s.map((x) => x.innerHTML).join("|"));
const res = {};

// ------------------------------------------------------------ retrograde playground
{
  const r = (res.retrograde = {});
  const pg = page.locator("[data-playground=copernicus-retrograde]").first();
  await pg.scrollIntoViewIfNeeded();
  await wait(1500);
  r.case = await pg.getAttribute("data-case");
  r.arcs = await num(pg, "data-arcs");
  r.gap = await num(pg, "data-gap");
  r.nearest = await num(pg, "data-nearest");
  const motions = new Set();
  for (let k = 0; k < 40; k++) { await wait(300); motions.add(await pg.getAttribute("data-motion")); }
  r.motions = [...motions];
  // drag the planet outward: a changes, so the nearest-point rate moves
  const handles = pg.locator(".mafs-movable-point-hitbox");
  await drag(handles.first(), 60, 0);
  r.nearestAfterDrag = await num(pg, "data-nearest");
  r.planetMoves = r.nearestAfterDrag !== r.nearest;
  // the frame slider: f = 0 (Earth) to f = 1 (Sun) redraws the path
  r.frame0 = await num(pg, "data-frame");
  const before = await svgOf(pg);
  await levaSet(pg, "f", 1);
  r.frame1 = await num(pg, "data-frame");
  r.frameRedraws = before !== (await svgOf(pg));
  // the time marker on the rate plot
  const lam0 = await pg.getAttribute("data-lambda-dot");
  await drag(handles.last(), 120, 0);
  r.markerMoves = (await pg.getAttribute("data-lambda-dot")) !== lam0;
  r.presets = await pressAll(pg);
  const P = r.presets;
  r.ok = r.case.startsWith("outer planet") && r.arcs >= 1 && r.gap < 1e-3 && r.nearest < 0
    && r.motions.includes("retrograde") && r.motions.includes("direct")
    && r.planetMoves && r.frame0 !== 1 && r.frame1 === 1 && r.frameRedraws && r.markerMoves
    && P["Mars (V.1)"].startsWith("outer planet") && P["Jupiter (V.1)"].startsWith("outer planet")
    && P["Saturn (V.1)"].startsWith("outer planet")
    && P["Venus, an inner planet"].startsWith("inner planet") && P["Mercury"].startsWith("inner planet")
    && P["Outer, but faster than the Earth"].startsWith("no retrograde")
    && P["Reset to Copernicus' Mars"].startsWith("outer planet");
  await pg.screenshot({ path: `${shots}/retrograde.png` });
}

// ------------------------------------------------------------ synodic, tied
{
  const r = (res.synodic = {});
  const w = page.locator("[data-figure=copernicus-synodic]").first();
  await w.scrollIntoViewIfNeeded();
  r.side0 = await w.getAttribute("data-side");
  const P0 = await num(w, "data-p");
  const solves0 = await page.evaluate(() => globalThis.__copernicusSolves || 0);
  await levaSet(w, "S", 3);
  const solves1 = await page.evaluate(() => globalThis.__copernicusSolves || 0);
  r.S = await num(w, "data-s");
  r.P = [P0, await num(w, "data-p")];
  r.solves = solves1 - solves0;
  r.residual = Math.abs(await num(w, "data-residual"));
  // E = 1, S = 3, outer: 1/P = 1 - 1/3 -> P = 1.5
  r.presets = await pressAll(w, "data-side");
  r.ok = r.side0 === "outer" && Math.abs(r.S - 3) < 1e-6 && Math.abs(r.P[1] - 1.5) < 1e-3 && r.solves >= 1
    && r.residual < 1e-6
    && r.presets["Venus (V.1)"] === "inner" && r.presets["Mercury (V.1)"] === "inner"
    && r.presets["Saturn (V.1)"] === "outer" && r.presets["Reset to Mars"] === "outer";
  await w.screenshot({ path: `${shots}/synodic.png` });
}

// ------------------------------------------------------------ side by side
{
  const r = (res.side = {});
  const pg = page.locator("[data-playground=copernicus-side-by-side]").first();
  await pg.scrollIntoViewIfNeeded();
  await wait(1000);
  r.case = await pg.getAttribute("data-case");
  r.worst = await num(pg, "data-worst");
  const svg0 = await svgOf(pg);
  await levaSet(pg, "t", 1.3);
  r.timeMoves = (await svgOf(pg)) !== svg0;
  // k is free: drag the grey deferent point, the sky stays the same
  const kHandle = pg.locator(".mafs-movable-point-hitbox").last();
  await drag(kHandle, 40, 0);
  r.worstAfterK = await num(pg, "data-worst");
  r.presets = await pressAll(pg);
  const P = r.presets;
  r.ok = r.case.startsWith("same sky") && r.worst < 1e-4 && r.worstAfterK < 1e-4 && r.timeMoves
    && P["Mars, same sky (m = 1)"].startsWith("same sky")
    && P["Mars at Ptolemy's scale k = 2"].startsWith("same sky")
    && P["Venus (inner)"].startsWith("same sky")
    && P["Jupiter, epicycle too small (m = 0.6)"].includes("smaller")
    && P["Mars, epicycle too large (m = 1.4)"].includes("larger")
    && P["Reset"].startsWith("same sky");
  await pg.screenshot({ path: `${shots}/side-by-side.png` });
}

// ------------------------------------------------------------ order of the planets
{
  const r = (res.order = {});
  const pg = page.locator("[data-playground=copernicus-order]").first();
  await pg.scrollIntoViewIfNeeded();
  r.case = await pg.getAttribute("data-case");
  r.order = await pg.getAttribute("data-order");
  // drag Saturn (first handle) onto Venus's orbit, inside the Earth's
  const hs = pg.locator(".mafs-movable-point-hitbox");
  await hs.first().scrollIntoViewIfNeeded();
  const sb = await hs.first().boundingBox(), vb = await hs.nth(3).boundingBox();
  await drag(hs.first(), vb.x - sb.x, vb.y - sb.y);
  r.dragged = await pg.getAttribute("data-order");
  r.draggedCase = await pg.getAttribute("data-case");
  r.presets = await pressAll(pg);
  const P = r.presets;
  r.ok = r.case.startsWith("Copernicus' order") && r.order === "Mercury,Venus,Earth,Mars,Jupiter,Saturn"
    && r.dragged !== r.order && r.draggedCase.startsWith("broken")
    && P["Ptolemy's epicycles, Copernicus' periods"].startsWith("Copernicus' order")
    && P["Make Mars's epicycle smaller than Jupiter's"].startsWith("the order by distance")
    && P["Give Venus a period longer than the Earth's"].startsWith("the order by distance")
    && P["Swap the periods of Jupiter and Saturn"].startsWith("the order by distance")
    && P["Reset to Ptolemy's epicycles and Copernicus' periods"].startsWith("Copernicus' order");
  await pg.screenshot({ path: `${shots}/order.png` });
}

// ------------------------------------------------------------ table
{
  const r = (res.table = {});
  r.rows = await page.locator("[data-table=copernicus-periods] tbody tr").count();
  r.ok = r.rows === 5;
}

const wasm = await page.evaluate(() => globalThis.__wasm);
await page.screenshot({ path: `${shots}/page.png`, fullPage: false });
res.page = { errors, remote, wasm, ok: errors.length === 0 && remote.length === 0 && (wasm.instantiate + wasm.instance) > 0 };
const ok = Object.values(res).every((x) => x.ok);
console.log(JSON.stringify({ ok, ...res }));
await browser.close();
process.exit(ok ? 0 : 1);
