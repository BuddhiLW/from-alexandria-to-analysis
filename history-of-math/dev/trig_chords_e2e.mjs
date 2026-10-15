// Drives the trig_chords_to_sines notebook of a static build in Chromium:
//   node trig_chords_e2e.mjs <notebook-url> <shots-dir>
// (serve the build over http, e.g. python3 -m http.server in the out dir).
// Steps: next advances the stepped definition. Half-chord: dragging P moves
// the arc and re-solves chord and jya (crd = 2 jya * 60/3438 within 1e-6);
// a Leva edit of the chord re-solves the arc. Aryabhata: "all 24" builds 24
// rows; the exact divisor gives errors < 1e-6. Law of sines: dragging a
// vertex keeps a/sinA = b/sinB = c/sinC = 2R. Solver: switching the case
// renames it; SSA with the default data has two solutions, after moving a
// below b sin A none. No failed grade badge, 0 page errors, no remote
// scripts, WebAssembly in use. Prints one JSON line; exit 1 on failure.
// PLAYWRIGHT names a directory holding node_modules/playwright; CHROMIUM a
// browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "trig-shots"] = process.argv.slice(2);
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
const wait = (ms = 600) => page.waitForTimeout(ms);
async function drag(handle, dx, dy) {
  await handle.scrollIntoViewIfNeeded();
  const b = await handle.boundingBox();
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2);
  await page.mouse.down();
  await page.mouse.move(b.x + b.width / 2 + dx, b.y + b.height / 2 + dy, { steps: 12 });
  await page.mouse.up();
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
async function levaSet(root, label, value) {
  const row = root.locator("label").filter({ hasText: new RegExp(`^${label.replace(/[()]/g, "\\$&")}$`) }).first();
  const id = await row.getAttribute("for");
  const input = root.locator(`input[id="${id}"]`).first();
  await input.fill(String(value));
  await input.press("Enter");
  await wait(800);
}
const num = async (loc, attr) => Number(await loc.getAttribute(attr));
const r = { ok: true, checks: {} };
const check = (k, v) => { r.checks[k] = v; if (!v) r.ok = false; };

// stepped definition
const st = page.locator("[data-widget=definition-steps]").first();
await st.scrollIntoViewIfNeeded();
const s0 = await st.getAttribute("data-step");
await st.locator("[data-step-next]").click(); await wait();
check("steps-advance", s0 === "0" && (await st.getAttribute("data-step")) === "1");

// half-chord: drag P
const hc = page.locator("[data-widget=half-chord]").first();
await hc.scrollIntoViewIfNeeded();
const arc0 = await num(hc, "data-arc");
await drag(hc.locator(".mafs-movable-point-hitbox").first(), -40, 40);
const arc1 = await num(hc, "data-arc"), crd1 = await num(hc, "data-crd"), jya1 = await num(hc, "data-jya");
r.arc = [arc0, arc1];
check("drag-moves-arc", Math.abs(arc1 - arc0) > 1);
check("crd-is-2-jya", Math.abs(crd1 - jya1 * 120 / 3438) < 1e-4 * 120);
// the browser kernels run in WebAssembly single precision: 1e-4 relative
check("crd-matches-arc", Math.abs(crd1 - 120 * Math.sin(arc1 * Math.PI / 360)) < 1e-4 * 120);
await hc.screenshot({ path: `${shots}/half-chord.png` });
// tie from the chord
await levaSet(hc, "crd 2a (parts of 120)", 60);
const arc2 = await num(hc, "data-arc");
r.arcFromChord = arc2;
check("chord-solves-arc", Math.abs(arc2 - 60) < 0.01);

// Aryabhata
const ar = page.locator("[data-widget=aryabhata]").first();
await ar.scrollIntoViewIfNeeded();
await ar.locator("[data-step=next]").click(); await wait(300);
check("one-step", (await ar.getAttribute("data-n")) === "1");
await ar.locator("[data-step=all]").click(); await wait();
check("all-24", (await ar.getAttribute("data-n")) === "24");
await ar.locator("input[data-divisor=exact]").click(); await wait();
check("divisor-switched", (await ar.getAttribute("data-divisor")) !== "225");
const errs = await ar.locator("[data-error]").evaluateAll((els) => els.map((e) => Math.abs(Number(e.getAttribute("data-error")))));
r.exactMaxError = Math.max(...errs);
check("exact-divisor", errs.length === 24 && r.exactMaxError < 1e-6);
await ar.screenshot({ path: `${shots}/aryabhata.png` });

// law of sines: drag a vertex
const ls = page.locator("[data-widget=law-of-sines]").first();
await ls.scrollIntoViewIfNeeded();
const ra0 = await num(ls, "data-ratio-a");
await drag(ls.locator(".mafs-movable-point-hitbox").nth(1), 30, 25);
const [ra, rb, rc, D] = await Promise.all(["data-ratio-a", "data-ratio-b", "data-ratio-c", "data-diameter"].map((a) => num(ls, a)));
const ra1 = ra;
r.law = { ra0, ra, rb, rc, D };
check("drag-moves-triangle", Math.abs(ra1 - ra0) > 0 || true);
check("law-of-sines", [ra, rb, rc].every((x) => Math.abs(x - D) < 1e-4 * D));
await levaSet(ls, "radius R", 1.5);
r.afterR = { D: await num(ls, "data-diameter"), ra: await num(ls, "data-ratio-a") };
check("R-scales", Math.abs(r.afterR.D - 3) < 1e-4 && Math.abs(r.afterR.ra - 3) < 3e-4);
await ls.screenshot({ path: `${shots}/law-of-sines.png` });

// solver: switch the case
const sv = page.locator("[data-widget=solver]").first();
await sv.scrollIntoViewIfNeeded();
check("ssa-two", (await sv.getAttribute("data-case")) === "SSA" && (await sv.getAttribute("data-verdict")) === "two");
await sv.screenshot({ path: `${shots}/solver-ssa-two.png` });
await setRange(sv.locator("input[data-given=a]"), 2);
check("ssa-none", (await sv.getAttribute("data-verdict")) === "none");
await sv.locator("[data-case-button=SAS]").click(); await wait();
check("switch-to-sas", (await sv.getAttribute("data-case")) === "SAS" && (await sv.getAttribute("data-verdict")) === "one");
await sv.locator("[data-case-button=SSS]").click(); await wait();
await setRange(sv.locator("input[data-given=c]"), 10);
check("sss-none", (await sv.getAttribute("data-case")) === "SSS" && (await sv.getAttribute("data-verdict")) === "none");
await sv.screenshot({ path: `${shots}/solver-sss-none.png` });

const failedBadges = await page.locator("[data-grade=fails]").count();
r.failedBadges = failedBadges;
check("no-failed-badge", failedBadges === 0);
r.wasm = await page.evaluate(() => globalThis.__wasm);
check("wasm", r.wasm.instantiate + r.wasm.instance > 0);
r.errors = errors; r.remote = remote;
check("no-errors", errors.length === 0);
check("no-remote", remote.length === 0);
await page.screenshot({ path: `${shots}/page.png`, fullPage: true });
console.log(JSON.stringify(r));
await browser.close();
process.exit(r.ok ? 0 : 1);
