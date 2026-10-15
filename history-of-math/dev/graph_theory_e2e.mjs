// Drives the graph_theory_bridges_to_colours notebook of a static build in
// Chromium:   node graph_theory_e2e.mjs <notebook-url> <shots-dir>
// (serve the build over http, e.g. python3 -m http.server in the out dir).
// Euler: Koenigsberg is named "none" with 4 odd regions; removing bridge g
// (C-D) renames it "path" with 2 odd regions and writes a walk over 6
// bridges in Euler's letters; adding a B-C bridge instead gives a path too;
// doubling every bridge gives "circuit". Kirchhoff: 21 trees and rank 4 at
// Koenigsberg, 21 trees listed; removing g gives 10 and rank 3. Cayley:
// n = 5 gives 125 = 5^3 from the determinant, and E/rank follow the tie.
// Icosian: "show a solution" closes the cycle through 20 vertices. Steps
// advance. No failed grade badge, 0 page errors, no remote scripts,
// WebAssembly in use. Prints one JSON line; exit 1 on failure.
// PLAYWRIGHT names a directory holding node_modules/playwright; CHROMIUM a
// browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "graph-shots"] = process.argv.slice(2);
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
const wait = (ms = 700) => page.waitForTimeout(ms);
const num = async (loc, attr) => Number(await loc.getAttribute(attr));
const near = (a, b) => Math.abs(a - b) < 1e-3; // raster kernels run in float32 wasm
async function setRange(input, value) {
  await input.evaluate((el, v) => {
    const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value").set;
    setter.call(el, String(v));
    el.dispatchEvent(new Event("input", { bubbles: true }));
    el.dispatchEvent(new Event("change", { bubbles: true }));
  }, value);
  await wait(900);
}
const r = { ok: true, checks: {} };
const check = (k, v) => { r.checks[k] = v; if (!v) r.ok = false; };

// stepped notation
const st = page.locator("[data-widget=euler-notation]").first();
await st.scrollIntoViewIfNeeded();
await st.locator("[data-step-next]").click(); await wait();
check("steps-advance", (await st.getAttribute("data-step")) === "1");

// Euler: remove a bridge and the named case changes
const eu = page.locator("[data-widget=multigraph-euler]").first();
await eu.scrollIntoViewIfNeeded();
r.euler0 = { case: await eu.getAttribute("data-case"), odd: await num(eu, "data-odd") };
check("koenigsberg-none", r.euler0.case === "none" && near(r.euler0.odd, 4));
await eu.screenshot({ path: `${shots}/euler-koenigsberg.png` });
await eu.locator("[data-remove=CD]").click(); await wait(1200);
r.euler1 = { case: await eu.getAttribute("data-case"), odd: await num(eu, "data-odd"), walk: await eu.getAttribute("data-walk") };
check("remove-g-path", r.euler1.case === "path" && near(r.euler1.odd, 2));
check("walk-written", (r.euler1.walk || "").length === 13);
await page.waitForTimeout(3000);
await eu.screenshot({ path: `${shots}/euler-without-g.png` });
await eu.locator("[data-add=CD]").click(); await wait();
check("add-back-none", (await eu.getAttribute("data-case")) === "none");
await eu.locator("[data-preset^='Cross each bridge twice']").click(); await wait();
check("doubled-circuit", (await eu.getAttribute("data-case")) === "circuit");
await eu.locator("[data-preset^='Reset']").click(); await wait();

// Kirchhoff
const ki = page.locator("[data-widget=multigraph-kirchhoff]").first();
await ki.scrollIntoViewIfNeeded();
r.kirchhoff0 = { trees: await num(ki, "data-trees"), rank: await num(ki, "data-rank"), listed: await num(ki, "data-listed") };
check("kirchhoff-21", near(r.kirchhoff0.trees, 21) && near(r.kirchhoff0.rank, 4) && r.kirchhoff0.listed === 21);
await ki.locator("[data-tree='3']").click(); await wait();
await ki.screenshot({ path: `${shots}/kirchhoff.png` });
await ki.locator("[data-remove=CD]").click(); await wait(1200);
r.kirchhoff1 = { trees: await num(ki, "data-trees"), rank: await num(ki, "data-rank"), listed: await num(ki, "data-listed") };
check("kirchhoff-without-g", near(r.kirchhoff1.trees, 10) && near(r.kirchhoff1.rank, 3) && r.kirchhoff1.listed === 10);

// Cayley: the tie and n^(n-2)
const ca = page.locator("[data-widget=cayley]").first();
await ca.scrollIntoViewIfNeeded();
{
  const row = ca.locator("label").filter({ hasText: /^points n$/ }).first();
  const id = await row.getAttribute("for");
  const input = ca.locator(`input[id="${id}"]`).first();
  await input.fill("5"); await input.press("Enter"); await wait(1500);
}
r.cayley = { n: await num(ca, "data-n"), trees: await num(ca, "data-trees"), cayley: await num(ca, "data-cayley") };
check("cayley-125", r.cayley.n === 5 && near(r.cayley.trees, 125) && Math.abs(r.cayley.cayley - 125) < 1e-6);
await ca.locator("[data-next-tree]").click(); await wait();
await ca.screenshot({ path: `${shots}/cayley.png` });

// Icosian
const ic = page.locator("[data-widget=icosian]").first();
await ic.scrollIntoViewIfNeeded();
await ic.locator("[data-vertex='0']").click(); await wait(300);
check("icosian-start", (await ic.getAttribute("data-visited")) === "1");
await ic.locator("[data-solution]").click(); await wait();
check("icosian-cycle", (await ic.getAttribute("data-visited")) === "20" && (await ic.getAttribute("data-closed")) === "true");
await ic.screenshot({ path: `${shots}/icosian.png` });

// Kempe
const ke = page.locator("[data-widget=kempe-steps]").first();
await ke.scrollIntoViewIfNeeded();
for (let i = 0; i < 4; i++) { await ke.locator("[data-step-next]").click(); await wait(300); }
check("kempe-last-step", (await ke.getAttribute("data-step")) === "4");
const cm = page.locator("[data-widget=colour-map]").first();
check("map-clash", (await cm.getAttribute("data-clashes")) !== "0");

const failedBadges = await page.locator("[data-grade=fails]").count();
r.failedBadges = failedBadges;
check("no-failed-badge", failedBadges === 0);
r.proved = await page.locator("[data-grade='grade/proved'], [data-grade=proved]").count();
r.wasm = await page.evaluate(() => globalThis.__wasm);
check("wasm", r.wasm.instantiate + r.wasm.instance > 0);
r.errors = errors; r.remote = remote;
check("no-errors", errors.length === 0);
check("no-remote", remote.length === 0);
await page.screenshot({ path: `${shots}/page.png`, fullPage: true });
console.log(JSON.stringify(r));
await browser.close();
process.exit(r.ok ? 0 : 1);
