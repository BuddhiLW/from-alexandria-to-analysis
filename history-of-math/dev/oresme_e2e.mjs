// Drives the Oresme notebook of a static build in Chromium:
//   node oresme_e2e.mjs <oresme-url> <shots-dir>
// (serve the build over http, e.g. python3 -m http.server in the out dir).
// Players: every stepped player runs to the end (▶▶) and redraws its svg.
// Latitude playground: starts "uniformly-difform-from-none", badge green;
// dragging the middle degree bends it to "difformly-difform", badge red;
// every "try this" preset renames the figure; the strip sum follows n;
// reset restores Oresme's triangle.
// Mean degree (tied): the Leva t field re-solves d (one tie solve per edit,
// d = vm t); "Break it" turns the badge red and the case "difformly-difform";
// dragging the corner C moves t; from rest the quarters read 1 : 3 : 5 : 7.
// Series playground: H(16) = 3.380729; p = 2 converges (badge red, sum
// bounded < pi^2/6); dragging the black marker changes N; Oresme's
// convergent series sums to 2 - (N + 2)/2^N; every preset pressed.
// Every page: 0 page errors, no remote scripts, WebAssembly in use.
// Prints one JSON line; exit 1 on failure. PLAYWRIGHT names a directory
// holding node_modules/playwright; CHROMIUM a browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "oresme-shots"] = process.argv.slice(2);
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
  await wait();
}
// Leva gives each input the id of its schema key; the label shows a
// descriptive text ("t extension (time)"), so address the input by key.
async function levaSet(root, key, value) {
  const input = root.locator(`input[id="${key}"]`).first();
  await input.fill(String(value));
  await input.press("Enter");
  await wait(700);
}
async function pressAll(root, attr = "data-case") {
  const out = {};
  for (const name of await root.locator("[data-preset]").evaluateAll((b) => b.map((x) => x.getAttribute("data-preset")))) {
    await root.locator(`[data-preset="${name}"]`).click(); await wait(700);
    out[name] = await root.getAttribute(attr);
  }
  return out;
}
const close = (a, b, eps = 1e-6) => Math.abs(a - b) < eps;
const res = {};

// ------------------------------------------------------------ players
{
  const r = (res.players = {});
  const ff = page.locator("button:has-text('▶▶')");
  r.count = await ff.count();
  r.moved = [];
  for (let i = 0; i < r.count; i++) {
    const b = ff.nth(i);
    await b.scrollIntoViewIfNeeded();
    const before = await page.locator("svg").evaluateAll((s) => s.map((x) => x.innerHTML.length).join(","));
    await b.click(); await wait(1200);
    const after = await page.locator("svg").evaluateAll((s) => s.map((x) => x.innerHTML.length).join(","));
    r.moved.push(before !== after);
  }
  r.ok = r.count >= 3 && r.moved.every(Boolean);
}

// ------------------------------------------------------------ latitude playground
{
  const r = (res.latitude = {});
  const pg = page.locator("[data-playground=oresme-latitude]").first();
  await pg.scrollIntoViewIfNeeded();
  r.start = await pg.getAttribute("data-case");
  r.startBadge = await pg.locator("[data-invariant]").getAttribute("data-invariant");
  const area0 = await num(pg, "data-area");
  const handles = pg.locator(".mafs-movable-point-hitbox");
  r.handles = await handles.count();
  await drag(handles.nth(2), 0, -70);
  r.dragged = await pg.getAttribute("data-case");
  r.draggedBadge = await pg.locator("[data-invariant]").getAttribute("data-invariant");
  r.areaMoved = (await num(pg, "data-area")) !== area0;
  r.presets = await pressAll(pg);
  await pg.locator(`[data-preset="Refine the strips: n = 200"]`).click(); await wait();
  r.strips = await num(pg, "data-strips");
  r.stripVsBoole = Math.abs((await num(pg, "data-area")) - (await num(pg, "data-boole")));
  await pg.locator(`[data-preset="Reset to Oresme's triangle"]`).click(); await wait();
  r.resetCase = await pg.getAttribute("data-case");
  r.resetBadge = await pg.locator("[data-invariant]").getAttribute("data-invariant");
  r.resetArea = await num(pg, "data-area");
  r.ok = r.start === "uniformly-difform-from-none" && r.startBadge === "pass" && r.handles >= 5
    && r.dragged === "difformly-difform" && r.draggedBadge === "fail" && r.areaMoved
    && r.presets["Uniform: a rectangle"] === "uniform"
    && r.presets["Uniformly difform, from a degree: a trapezium"] === "uniformly-difform"
    && r.presets["Bend it: an arch (difformly difform)"] === "difformly-difform"
    && r.strips === 200 && r.stripVsBoole < 1e-3
    && r.resetCase === "uniformly-difform-from-none" && r.resetBadge === "pass" && close(r.resetArea, 8, 1e-9);
  await pg.screenshot({ path: `${shots}/latitude-playground.png` });
}

// ------------------------------------------------------------ mean degree, tied
{
  const r = (res.mean = {});
  const w = page.locator("[data-figure=oresme-mean]").first();
  await w.scrollIntoViewIfNeeded();
  r.start = await w.getAttribute("data-case");
  r.d0 = await num(w, "data-distance");
  r.odd = await w.locator("[data-odd]").innerText().catch(() => "");
  const solves0 = await page.evaluate(() => globalThis.__oresmeSolves || 0);
  await levaSet(w, "t", 6);
  const solves1 = await page.evaluate(() => globalThis.__oresmeSolves || 0);
  r.solves = solves1 - solves0;
  r.t1 = await num(w, "data-t");
  r.d1 = await num(w, "data-distance");
  r.rect1 = await num(w, "data-rect");
  const handles = w.locator(".mafs-movable-point-hitbox");
  const t1 = r.t1;
  await drag(handles.nth(1), -60, 0);
  r.tDragged = await num(w, "data-t");
  r.presets = await pressAll(w);
  await w.locator(`[data-preset="Break it: bend the top up"]`).click(); await wait();
  r.brokenCase = await w.getAttribute("data-case");
  r.brokenBadge = await w.locator("[data-invariant]").getAttribute("data-invariant");
  // bend b = 1.5, t = 4: the strips leave the rectangle by 2bt/3 = 4
  r.bendGap = (await num(w, "data-distance")) - (await num(w, "data-rect"));
  await w.locator(`[data-preset="Reset"]`).click(); await wait();
  r.resetCase = await w.getAttribute("data-case");
  r.resetBadge = await w.locator("[data-invariant]").getAttribute("data-invariant");
  r.ok = r.start === "uniformly-difform-from-none" && close(r.d0, 8, 1e-6) && r.odd.includes("1.00 : 3.00 : 5.00 : 7.00")
    && r.solves >= 1 && close(r.t1, 6, 1e-6) && close(r.d1, r.rect1, 1e-6) && r.d1 !== r.d0
    && r.tDragged !== t1
    && r.presets["Uniform: v₀ = v₁"] === "uniform" && r.presets["A trapezium: from v₀ = 2"] === "uniformly-difform"
    && r.brokenCase === "difformly-difform" && r.brokenBadge === "fail" && Math.abs(r.bendGap - 4) < 0.01
    && r.resetCase === "uniformly-difform-from-none" && r.resetBadge === "pass";
  await w.screenshot({ path: `${shots}/mean-tied.png` });
}

// ------------------------------------------------------------ series playground
{
  const r = (res.series = {});
  const pg = page.locator("[data-playground=oresme-series]").first();
  await pg.scrollIntoViewIfNeeded();
  r.start = await pg.getAttribute("data-case");
  r.h16 = await num(pg, "data-sum");
  r.startBadge = await pg.locator("[data-invariant]").getAttribute("data-invariant");
  const n0 = await num(pg, "data-n");
  await drag(pg.locator(".mafs-movable-point-hitbox").last(), 120, 0);
  r.nDragged = await num(pg, "data-n");
  r.presets = await pressAll(pg);
  await pg.locator(`[data-preset="p = 2: the groups shrink, the sum converges"]`).click(); await wait();
  r.p2Badge = await pg.locator("[data-invariant]").getAttribute("data-invariant");
  r.p2Sum = await num(pg, "data-sum");
  await pg.locator(`[data-preset="Oresme's convergent series Σ n/2ⁿ"]`).click(); await wait();
  r.oresmeSum = await num(pg, "data-sum");
  await pg.locator(`[data-preset="Push on: N = 65536, the sum passes 1 + 16/2"]`).click(); await wait(1200);
  r.h65536 = await num(pg, "data-sum");
  await pg.locator(`[data-preset="Reset"]`).click(); await wait();
  r.resetCase = await pg.getAttribute("data-case");
  r.ok = r.start === "harmonic" && close(r.h16, 3.380728993, 1e-6) && r.startBadge === "pass"
    && r.nDragged !== n0
    && r.presets["p = 2: the groups shrink, the sum converges"] === "converges"
    && r.presets["p = 1/2: diverges faster"] === "faster"
    && r.presets["Oresme's convergent series Σ n/2ⁿ"] === "oresme"
    && r.p2Badge === "fail" && r.p2Sum < Math.PI ** 2 / 6
    && close(r.oresmeSum, 2 - 34 / 2 ** 32, 1e-9) && r.h65536 > 9
    && r.resetCase === "harmonic";
  await pg.screenshot({ path: `${shots}/series-playground.png` });
}

const wasm = await page.evaluate(() => globalThis.__wasm);
await page.screenshot({ path: `${shots}/page.png`, fullPage: false });
res.page = { errors, remote, wasm, ok: errors.length === 0 && remote.length === 0 && (wasm.instantiate + wasm.instance) > 0 };
const ok = Object.values(res).every((x) => x.ok);
console.log(JSON.stringify({ ok, ...res }));
await browser.close();
process.exit(ok ? 0 : 1);
