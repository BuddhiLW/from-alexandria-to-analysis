// Drives the Greek playgrounds of a static build in Chromium:
//   node greek_playgrounds_e2e.mjs <euclid-url> <delian-url> <shots-dir>
// (serve the build over http, e.g. python3 -m http.server in the out dir).
// Euclid: I.1 (drag B, collapse preset names the coincident case); I.47
// (drag B behind A: the case goes II.12 and the badge red; presets acute,
// meno, flat, euclid; layer toggles hide/show the windmill); application
// (k presets name ellipse/parabola/hyperbola; asking past VI.27's greatest
// turns the badge red; drag X across B changes the name). Delian: means
// (drag b: the ratio and the case change; preset "theta" puts P on all
// three loci, badge green); conchoid (RQ = l at every drag of R; cusp and
// no-loop presets rename; neusis preset turns the badge green); cissoid
// (drag u: the three ratios stay equal; the Delian preset names "delian");
// archytas (drag B and P's foot in the plan: the state and the case move;
// on-k puts P on the cone; eight/equal rename; no-chord turns the badge red).
// Every page: 0 page errors, no remote scripts, WebAssembly in use.
// Prints one JSON line; exit 1 on failure. PLAYWRIGHT names a directory
// holding node_modules/playwright; CHROMIUM a browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [euclidUrl, delianUrl, shots = "greek-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });
const browser = await chromium.launch({
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});

async function open(url) {
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
  await page.waitForTimeout(2000);
  return { page, errors, remote };
}

const wait = (page, ms = 400) => page.waitForTimeout(ms);
const attr = (loc, a) => loc.getAttribute(a);
const caseOf = (pg) => attr(pg, "data-case");
const badge = (pg) => pg.locator("[data-badge]").first().getAttribute("data-badge");
async function drag(page, handle, dx, dy) {
  await handle.scrollIntoViewIfNeeded();
  await wait(page, 200);
  const b = await handle.boundingBox();
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2);
  await page.mouse.down();
  await page.mouse.move(b.x + b.width / 2 + dx, b.y + b.height / 2 + dy, { steps: 10 });
  await page.mouse.up();
  await wait(page);
}
async function preset(pg, name) { await pg.locator(`[data-preset=${name}]`).click(); await wait(pg.page()); }
const nums = (s) => (s.match(/−?\d+\.\d+/g) || []).map((t) => Number(t.replace("−", "-")));

const res = {};
// ------------------------------------------------------------ Euclid
{
  const { page, errors, remote } = await open(euclidUrl);
  const r = (res.euclid = {});
  const i1 = page.locator("[data-playground=euclid-i1]").first();
  await i1.scrollIntoViewIfNeeded();
  await drag(page, i1.locator("[data-drag=B]"), 60, -40);
  r.i1 = { afterDrag: await caseOf(i1), readout: await i1.locator("[data-readout]").innerText() };
  await preset(i1, "collapse"); r.i1.collapse = await caseOf(i1);
  await preset(i1, "euclid"); r.i1.reset = await caseOf(i1);
  const [ab, ac, bc] = nums(r.i1.readout);
  r.i1.ok = r.i1.afterDrag === "intersect" && Math.abs(ab - ac) < 1e-3 && Math.abs(ab - bc) < 1e-3
    && r.i1.collapse === "coincident" && r.i1.reset === "intersect";
  await i1.screenshot({ path: `${shots}/euclid-i1.png` });

  const p47 = page.locator("[data-playground=euclid-i47]").first();
  await p47.scrollIntoViewIfNeeded();
  r.i47 = { start: await caseOf(p47), startBadge: await badge(p47) };
  await drag(page, p47.locator("[data-drag=B]"), -70, 0);
  r.i47.dragged = { kase: await caseOf(p47), badge: await badge(p47), state: await attr(p47, "data-state") };
  await p47.screenshot({ path: `${shots}/euclid-i47-dragged.png` });
  for (const k of ["acute", "obtuse", "meno", "flat", "euclid"]) {
    await preset(p47, k); r.i47[k] = await caseOf(p47);
  }
  await p47.locator("[data-layer-toggle=windmill]").click(); await wait(page);
  r.i47.windmillHidden = (await p47.locator("[data-layer=windmill]").count()) === 0;
  await p47.locator("[data-layer-toggle=windmill]").click(); await wait(page);
  r.i47.windmillShown = (await p47.locator("[data-layer=windmill]").count()) === 1;
  const wm = nums(await p47.locator("[data-readout=windmill]").innerText());
  const sq = nums(await p47.locator("[data-readout=squares]").innerText());
  r.i47.ok = r.i47.start === "right" && r.i47.startBadge === "ok"
    && r.i47.dragged.kase === "obtuse" && r.i47.dragged.badge === "broken"
    && r.i47.acute === "acute" && r.i47.obtuse === "obtuse" && r.i47.meno === "right" && r.i47.flat === "flat"
    && r.i47.euclid === "right" && r.i47.windmillHidden && r.i47.windmillShown
    && Math.abs(wm[0] - sq[2]) < 1e-3 && Math.abs(wm[1] - sq[1]) < 1e-3;
  await p47.screenshot({ path: `${shots}/euclid-i47.png` });

  const ap = page.locator("[data-playground=euclid-application]").first();
  await ap.scrollIntoViewIfNeeded();
  r.app = { start: await caseOf(ap), startBadge: await badge(ap) };
  for (const k of ["i44", "exceed", "greatest", "impossible"]) {
    await preset(ap, k); r.app[k] = { kase: await caseOf(ap), badge: await badge(ap) };
    await ap.screenshot({ path: `${shots}/euclid-application-${k}.png` });
  }
  await preset(ap, "euclid");
  await drag(page, ap.locator("[data-drag=X]"), 160, 0);
  r.app.draggedX = await caseOf(ap);
  r.app.ok = r.app.start === "ellipse" && r.app.startBadge === "ok"
    && r.app.i44.kase === "parabola" && r.app.exceed.kase === "hyperbola"
    && r.app.greatest.badge === "ok" && r.app.impossible.badge === "broken"
    && r.app.draggedX === "hyperbola";
  r.wasm = await page.evaluate(() => globalThis.__wasm);
  r.errors = errors.slice(0, 8); r.remote = remote.slice(0, 5);
  r.ok = r.i1.ok && r.i47.ok && r.app.ok && errors.length === 0 && remote.length === 0
    && r.wasm.instance + r.wasm.instantiate > 0;
}
// ------------------------------------------------------------ Delian
{
  const { page, errors, remote } = await open(delianUrl);
  const r = (res.delian = {});
  const m = page.locator("[data-playground=delian-means]").first();
  await m.scrollIntoViewIfNeeded();
  r.means = { start: await caseOf(m), badge: await badge(m) };
  await drag(page, m.locator("[data-drag=b]"), 80, 0);
  r.means.draggedB = { kase: await caseOf(m), state: await attr(m, "data-state") };
  const rat = nums(await m.locator("[data-readout=ratios]").innerText());
  for (const k of ["eight", "half", "equal", "theta"]) { await preset(m, k); r.means[k] = await caseOf(m); }
  r.means.thetaBadge = await badge(m); r.means.thetaOn = await attr(m, "data-on");
  await m.screenshot({ path: `${shots}/delian-means.png` });
  r.means.ok = r.means.start === "delian" && r.means.badge === "broken" && r.means.draggedB.kase !== "delian"
    && Math.abs(rat[0] - rat[1]) < 1e-3 && Math.abs(rat[1] - rat[2]) < 1e-3
    && r.means.eight === "cube" && r.means.half === "solid" && r.means.equal === "equal"
    && r.means.theta === "delian" && r.means.thetaBadge === "ok";

  const c = page.locator("[data-playground=delian-conchoid]").first();
  await c.scrollIntoViewIfNeeded();
  r.conchoid = { start: await caseOf(c) };
  await drag(page, c.locator("[data-drag=R]"), 40, 0);
  const rq = nums(await c.locator("[data-readout=RQ]").innerText());
  r.conchoid.rq = rq;
  for (const k of ["cusp", "no-loop", "neusis"]) { await preset(c, k); r.conchoid[k] = { kase: await caseOf(c), badge: await badge(c) }; }
  await c.screenshot({ path: `${shots}/delian-conchoid.png` });
  r.conchoid.ok = r.conchoid.start === "loop" && Math.abs(rq[0] - rq[2]) < 1e-3 && Math.abs(rq[1] - rq[2]) < 1e-3
    && r.conchoid.cusp.kase === "cusp" && r.conchoid["no-loop"].kase === "smooth" && r.conchoid.neusis.badge === "ok";

  const s = page.locator("[data-playground=delian-cissoid]").first();
  await s.scrollIntoViewIfNeeded();
  r.cissoid = { start: await caseOf(s) };
  await drag(page, s.locator("[data-drag=u]"), 50, 0);
  const sr = nums(await s.locator("[data-readout=ratios]").innerText());
  r.cissoid.dragged = { kase: await caseOf(s), ratios: sr, badge: await badge(s) };
  for (const k of ["centre", "delian", "edge"]) { await preset(s, k); r.cissoid[k] = await caseOf(s); }
  await s.screenshot({ path: `${shots}/delian-cissoid.png` });
  r.cissoid.ok = r.cissoid.start === "near" && Math.abs(sr[0] - sr[1]) < 1e-3 && Math.abs(sr[1] - sr[2]) < 1e-3
    && r.cissoid.dragged.badge === "ok" && r.cissoid.centre === "centre" && r.cissoid.delian === "delian"
    && r.cissoid.edge === "far";
  const a = page.locator("[data-playground=delian-archytas]").first();
  await a.scrollIntoViewIfNeeded();
  await wait(page, 1500);
  r.archytas = { start: await caseOf(a), p: await attr(a, "data-p"), badge: await badge(a) };
  const kNums = nums(await a.locator("[data-readout=k]").innerText());
  const kRat = nums(await a.locator("[data-readout=ratios]").innerText());
  r.archytas.k = kNums; r.archytas.ratiosK = kRat;
  const handles = a.locator(".mafs-movable-point");
  r.archytas.handles = await handles.count();
  const s0 = await attr(a, "data-state");
  // A real mouse drag of each plan handle: the point moves alone (its
  // centre follows the pointer), the Mafs view does not pan (its
  // --mafs-view-transform is unchanged), and the state changes. The labels
  // B and P sit on their handles: before the series' mafs.css made Mafs
  // text click-through they took the mousedown and the plan panned.
  const view = () => a.locator(".MafsView").first().evaluate((s) =>
    s.style.getPropertyValue("--mafs-view-transform") + " | " + (s.querySelector("svg") || s).getAttribute("viewBox"));
  const centre = async (h) => { const b = await h.boundingBox(); return [b.x + b.width / 2, b.y + b.height / 2]; };
  async function mouseDrag(h, dx, dy) {
    const v0 = await view(), c0 = await centre(h), st0 = await attr(a, "data-state");
    await drag(page, h, dx, dy);
    const c1 = await centre(h);
    return { pan: (await view()) !== v0, moved: Math.hypot(c1[0] - c0[0], c1[1] - c0[1]),
             changed: (await attr(a, "data-state")) !== st0 };
  }
  r.archytas.mouseB = await mouseDrag(handles.nth(0), -30, -25);   // B round the circle: AB changes
  r.archytas.draggedB = { kase: await caseOf(a), state: await attr(a, "data-state") };
  r.archytas.mouseP = await mouseDrag(handles.nth(1), 25, 20);     // P's foot: P runs on the curve
  r.archytas.draggedP = { p: await attr(a, "data-p"), state: await attr(a, "data-state") };
  const byMouse = (m) => !m.pan && m.moved > 3 && m.changed;
  await a.screenshot({ path: `${shots}/delian-archytas-dragged.png` });
  for (const k of ["on-k", "eight", "equal", "no-chord", "delian"]) {
    await preset(a, k);
    r.archytas[k] = { kase: await caseOf(a), p: await attr(a, "data-p"), badge: await badge(a) };
    await a.screenshot({ path: `${shots}/delian-archytas-${k}.png` });
  }
  r.archytas.ok = r.archytas.start === "delian" && r.archytas.badge === "ok" && r.archytas.p !== "on"
    && Math.abs(kNums[0] - kNums[3]) < 1e-3 && Math.abs(kRat[0] - kRat[1]) < 1e-3 && Math.abs(kRat[1] - kRat[2]) < 1e-3
    && byMouse(r.archytas.mouseB) && byMouse(r.archytas.mouseP)
    && r.archytas.draggedB.state !== s0 && r.archytas.draggedB.kase !== "delian"
    && r.archytas.draggedP.state !== r.archytas.draggedB.state
    && r.archytas["on-k"].p === "on" && r.archytas.eight.kase === "cube" && r.archytas.equal.kase === "equal"
    && r.archytas["no-chord"].kase === "no-chord" && r.archytas["no-chord"].badge === "broken"
    && r.archytas.delian.kase === "delian" && r.archytas.delian.badge === "ok";

  r.wasm = await page.evaluate(() => globalThis.__wasm);
  r.errors = errors.slice(0, 8); r.remote = remote.slice(0, 5);
  r.ok = r.means.ok && r.conchoid.ok && r.cissoid.ok && r.archytas.ok && errors.length === 0 && remote.length === 0
    && r.wasm.instance + r.wasm.instantiate > 0;
}
const ok = res.euclid.ok && (!res.delian || res.delian.ok);
console.log(JSON.stringify({ ok, ...res }));
await browser.close();
process.exit(ok ? 0 : 1);
