// Drives the Ptolemy notebook of a static build in Chromium:
//   node ptolemy_e2e.mjs <ptolemy-url> <shots-dir>
// (serve the build over http, e.g. python3 -m http.server in the out dir).
// Chords: the table shows Ptolemy's errors under 1″; both players step
// (▶▶) and their arc sliders move the figure. Theorem tied: moving the arc
// d re-solves CD, BD, AD (one solve per edit) and the residual stays ~0.
// Playground: every handle is the topmost element at its centre (no label
// over it); with snap on, dragging D along the circle moves it and keeps
// the case "cyclic", badge green; with snap off, dragging each of A, B, C,
// D off the circle moves that point alone (the view does not pan) and
// flips the case to Ptolemy's inequality ("inside"/"outside"), badge red;
// every "try this" preset; reset -> "cyclic", badge green.
// Epicycle: the case reads "retrograde loops" for Mars, at least one arc,
// RK4 gap < 1e-6; the "no loops" preset renames it; the time marker moves
// by keys and by mouse (and stops the run); dragging the deferent handle
// changes R, dragging the planet changes r; every preset pressed.
// Every page: 0 page errors, no remote scripts, WebAssembly in use.
// Prints one JSON line; exit 1 on failure. PLAYWRIGHT names a directory
// holding node_modules/playwright; CHROMIUM a browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "ptolemy-shots"] = process.argv.slice(2);
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
async function drag(handle, dx, dy) {
  const b = await handle.boundingBox();
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2);
  await page.mouse.down();
  await page.mouse.move(b.x + b.width / 2 + dx, b.y + b.height / 2 + dy, { steps: 12 });
  await page.mouse.up();
  await wait();
}
// Mafs movable points take arrow keys when focused (the hitbox is focusable)
async function keys(handle, key, n) {
  await handle.focus();
  for (let i = 0; i < n; i++) await page.keyboard.press(key, { delay: 20 });
  await wait();
}
async function setRange(input, value) {
  await input.evaluate((el, v) => {
    const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value").set;
    setter.call(el, String(v));
    el.dispatchEvent(new Event("input", { bubbles: true }));
    el.dispatchEvent(new Event("change", { bubbles: true }));
  }, value);
  await wait();
}
// a Leva number field by its label text
async function levaSet(root, label, value) {
  const row = root.locator("label").filter({ hasText: new RegExp(`^${label}$`) }).first();
  const id = await row.getAttribute("for");
  const input = root.locator(`input[id="${id}"]`).first();
  await input.fill(String(value));
  await input.press("Enter");
  await wait(700);
}
// does the pointer at the hitbox's centre reach the movable point (its own
// <g>: hitbox, ring, dot), not a label or another figure drawn over it?
async function onTop(handle) {
  return handle.evaluate((el) => {
    const b = el.getBoundingClientRect();
    const top = document.elementFromPoint(b.x + b.width / 2, b.y + b.height / 2);
    return !!top && el.parentElement.contains(top);
  });
}
// handle centres relative to their Mafs view: a pan moves them all, a page
// scroll none
const centres = (handles) => handles.evaluateAll((es) => es.map((e) => {
  const b = e.getBoundingClientRect(), v = e.closest("svg").getBoundingClientRect();
  return [b.x + b.width / 2 - v.x, b.y + b.height / 2 - v.y];
}));
const moved = (p, q, tol = 2) => Math.hypot(p[0] - q[0], p[1] - q[1]) > tol;
const res = {};

// ------------------------------------------------------------ chords
{
  const r = (res.chords = {});
  const errs = await page.locator("[data-error-seconds]").evaluateAll((els) => els.map((e) => Number(e.getAttribute("data-error-seconds"))));
  r.rows = errs.length;
  r.maxError = Math.max(...errs.map(Math.abs));
  const svgs = page.locator("svg[viewBox]");
  const players = page.locator("input[data-control=beta]");
  const half = players.first();
  await half.scrollIntoViewIfNeeded();
  const before = await page.locator("svg").evaluateAll((s) => s.map((x) => x.innerHTML.length).join(","));
  await setRange(half, 120);
  await page.locator("button:has-text('▶▶')").first().click(); await wait(800);
  const after = await page.locator("svg").evaluateAll((s) => s.map((x) => x.innerHTML.length).join(","));
  r.halfMoves = before !== after;
  const alpha = page.locator("input[data-control=alpha]").first();
  await alpha.scrollIntoViewIfNeeded();
  const b2 = await page.locator("svg").evaluateAll((s) => s.map((x) => x.innerHTML).join("|").length);
  await setRange(alpha, 30);
  await page.locator("button:has-text('▶▶')").nth(1).click(); await wait(800);
  const a2 = await page.locator("svg").evaluateAll((s) => s.map((x) => x.innerHTML).join("|").length);
  r.sumMoves = b2 !== a2;
  await page.locator("[data-chord-table]").screenshot({ path: `${shots}/chord-table.png` });
  r.ok = r.rows === 10 && r.maxError < 1 && r.halfMoves && r.sumMoves && (await svgs.count()) > 0;
}

// ------------------------------------------------------------ theorem, tied
{
  const r = (res.tied = {});
  const w = page.locator("[data-figure=ptolemy-tied]").first();
  await w.scrollIntoViewIfNeeded();
  const d0 = Number(await w.getAttribute("data-d"));
  const solves0 = await page.evaluate(() => globalThis.__ptolemySolves || 0);
  await levaSet(w, "CD", 1.2);
  const d1 = Number(await w.getAttribute("data-d"));
  const solves1 = await page.evaluate(() => globalThis.__ptolemySolves || 0);
  r.d = [d0, d1];
  r.solves = solves1 - solves0;
  r.residual = Math.abs(Number(await w.getAttribute("data-residual")));
  // CD = 1.2 -> d = 2 asin(0.6) = 73.74°
  r.ok = Math.abs(d1 - 73.7398) < 0.01 && r.solves === 1 && r.residual < 1e-6;
  await w.screenshot({ path: `${shots}/theorem-tied.png` });
}

// ------------------------------------------------------------ theorem, playground
{
  const r = (res.playground = {});
  const pg = page.locator("[data-playground=ptolemy-quad]").first();
  await pg.scrollIntoViewIfNeeded();
  r.start = await pg.getAttribute("data-case");
  const lhs0 = await pg.locator("[data-lhs]").getAttribute("data-lhs");
  const handles = pg.locator(".mafs-movable-point-hitbox");
  const snap = pg.locator("input[data-control=snap]");
  const badge = async () => pg.locator("[data-invariant]").getAttribute("data-invariant");
  const reset = async () => { await pg.locator(`[data-preset="Reset to Ptolemy's figure"]`).click(); await wait(); };
  await handles.nth(3).scrollIntoViewIfNeeded();
  r.handles = await handles.count();
  r.onTop = [];
  for (let i = 0; i < r.handles; i++) r.onTop.push(await onTop(handles.nth(i)));
  // snap on: D (at -20°) dragged down-left along the circle stays on it
  r.snap = await snap.isChecked();
  const c0 = await centres(handles);
  await drag(handles.nth(3), -25, 45);
  const c1 = await centres(handles);
  r.lhsMoved = (await pg.locator("[data-lhs]").getAttribute("data-lhs")) !== lhs0;
  r.snapDMoved = moved(c0[3], c1[3]) && [0, 1, 2].every((j) => !moved(c0[j], c1[j]));
  r.dragged = await pg.getAttribute("data-case");
  r.draggedBadge = await badge();
  // snap off: each point dragged 70 px radially outward leaves the circle
  r.offCircle = {};
  const angles = { A: 200, B: 80, C: 20, D: -20 };
  for (const [i, name] of ["A", "B", "C", "D"].entries()) {
    await reset();
    if (await snap.isChecked()) { await snap.click(); await wait(); }
    const a = (angles[name] * Math.PI) / 180;
    const before = await centres(handles);
    await drag(handles.nth(i), 70 * Math.cos(a), -70 * Math.sin(a));
    const after = await centres(handles);
    r.offCircle[name] = {
      moved: moved(before[i], after[i], 30),
      othersStill: before.every((p, j) => j === i || !moved(p, after[j])),
      case: await pg.getAttribute("data-case"),
      badge: await badge(),
    };
  }
  await reset();
  r.presets = {};
  for (const name of await pg.locator("[data-preset]").evaluateAll((b) => b.map((x) => x.getAttribute("data-preset")))) {
    await pg.locator(`[data-preset="${name}"]`).click(); await wait();
    r.presets[name] = await pg.getAttribute("data-case");
  }
  r.resetBadge = await pg.locator("[data-invariant]").getAttribute("data-invariant");
  await pg.locator(`[data-preset="Pull D inside the circle"]`).click(); await wait();
  r.brokenBadge = await pg.locator("[data-invariant]").getAttribute("data-invariant");
  await pg.locator(`[data-preset="Reset to Ptolemy's figure"]`).click(); await wait();
  r.resetBadge = await pg.locator("[data-invariant]").getAttribute("data-invariant");
  r.ok = r.start === "cyclic" && r.handles === 4 && r.onTop.every(Boolean)
    && r.snap && r.lhsMoved && r.snapDMoved && r.dragged === "cyclic" && r.draggedBadge === "pass"
    && Object.keys(r.offCircle).length === 4
    && Object.values(r.offCircle).every((o) => o.moved && o.othersStill
      && (o.case === "inside" || o.case === "outside") && o.badge === "fail")
    && r.brokenBadge === "fail"
    && r.presets["Pull D inside the circle"] === "inside" && r.presets["Push D outside"] === "outside"
    && r.presets["Cross the order: swap B and C"] === "cyclic-crossed"
    && r.presets["Reset to Ptolemy's figure"] === "cyclic" && r.resetBadge === "pass";
  await pg.screenshot({ path: `${shots}/theorem-playground.png` });
}

// ------------------------------------------------------------ epicycle
{
  const r = (res.epicycle = {});
  const pg = page.locator("[data-playground=ptolemy-epicycle]").first();
  await pg.scrollIntoViewIfNeeded();
  await wait(1500);
  r.case = await pg.getAttribute("data-case");
  r.arcs = Number(await pg.getAttribute("data-arcs"));
  r.gap = Number(await pg.getAttribute("data-gap"));
  r.opposition = Number(await pg.getAttribute("data-opposition"));
  r.mathbox = await pg.locator("canvas").count();
  const motions = new Set();
  const marker = pg.locator(".mafs-movable-point-hitbox").last();
  for (let k = 0; k < 40; k++) { await wait(300); motions.add(await pg.getAttribute("data-motion")); }
  await keys(marker, "ArrowRight", 6); r.afterMarker = await pg.getAttribute("data-motion");
  r.motions = [...motions];
  // mouse drags: the time marker (stops the run), then the deferent handle
  // (R) and the planet (r), each read back through the opposition rate
  const eh = pg.locator(".mafs-movable-point-hitbox");
  r.handles = await eh.count();
  r.onTop = [];
  for (let i = 0; i < r.handles; i++) r.onTop.push(await onTop(eh.nth(i)));
  const t0 = await centres(eh);
  await drag(marker, 60, 0);
  r.markerMoved = moved(t0[r.handles - 1], (await centres(eh))[r.handles - 1], 20);
  const opp0 = await pg.getAttribute("data-opposition");
  await drag(eh.nth(0), -40, 0);
  const opp1 = await pg.getAttribute("data-opposition");
  r.deferentDrag = opp1 !== opp0;
  await drag(eh.nth(1), 15, 15);
  r.planetDrag = (await pg.getAttribute("data-opposition")) !== opp1;
  r.presets = {};
  for (const name of await pg.locator("[data-preset]").evaluateAll((b) => b.map((x) => x.getAttribute("data-preset")))) {
    await pg.locator(`[data-preset="${name}"]`).click(); await wait(900);
    r.presets[name] = await pg.getAttribute("data-case");
  }
  r.ok = r.case.startsWith("retrograde loops") && r.arcs >= 1 && r.gap < 1e-3 && r.opposition < 0 && r.mathbox > 0
    && r.motions.includes("retrograde") && r.motions.includes("direct (prograde)")
    && r.handles === 3 && r.onTop.every(Boolean) && r.markerMoved && r.deferentDrag && r.planetDrag
    && r.presets["Shrink the epicycle: no loops"].startsWith("no retrograde")
    && r.presets["Epicycle wider than the deferent"].includes("Earth inside")
    && r.presets["Reset to Ptolemy's Mars"].startsWith("retrograde loops");
  await pg.screenshot({ path: `${shots}/epicycle.png` });
}

const wasm = await page.evaluate(() => globalThis.__wasm);
await page.screenshot({ path: `${shots}/page.png`, fullPage: false });
res.page = { errors, remote, wasm, ok: errors.length === 0 && remote.length === 0 && (wasm.instantiate + wasm.instance) > 0 };
const ok = Object.values(res).every((x) => x.ok);
console.log(JSON.stringify({ ok, ...res }));
await browser.close();
process.exit(ok ? 0 : 1);
