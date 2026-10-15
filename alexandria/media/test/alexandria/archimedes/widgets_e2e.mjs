// Drives the tied widgets of the Archimedes and Eudoxus notebooks in a
// static build, in Chromium:
//   node widgets_e2e.mjs <archimedes-url> <eudoxus-url> <shots-dir>
// Checks: no page errors, no remote scripts, raster's wasm instantiated,
// every derivation grade proved, and the ties: typing a value into one
// Leva control re-solves the others by the equations, exactly one solve per
// edit (no oscillation), and the round trip lands back. The sphere widget
// has a MathBox canvas; the Prop. 3 widget starts inside Archimedes'
// bracket at n = 96. Prints one JSON line; exit 1 on failure.
// PLAYWRIGHT names a directory holding node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [urlA, urlE, shots = "widgets-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const fails = [];
const check = (label, ok, got) => { if (!ok) fails.push({ label, got }); };
const near = (a, b, tol) => Math.abs(a - b) <= tol;
const report = {};

async function open(url) {
  const page = await browser.newPage({ viewport: { width: 1300, height: 1000 } });
  const errors = [], remote = [];
  page.on("pageerror", (e) => errors.push(String(e)));
  page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text().slice(0, 300)); });
  page.on("request", (r) => { const u = r.url(); if (r.resourceType() === "script" && !u.startsWith(new URL(url).origin)) remote.push(u); });
  await page.addInitScript(() => {
    const W = globalThis.WebAssembly;
    const counts = (globalThis.__wasm = { instance: 0 });
    W.Instance = new Proxy(W.Instance, { construct(t, a, nt) { counts.instance++; return Reflect.construct(t, a, nt); } });
  });
  await page.goto(url, { waitUntil: "networkidle" });
  return { page, errors, remote };
}

// read the Leva inputs of one widget, by label order
const readFig = (page, id) => page.evaluate((id) => {
  const fig = document.querySelector(`[data-figure=${id}]`);
  const out = {};
  for (const inp of fig.querySelectorAll("input[id]")) if (inp.type !== "checkbox") out[inp.id] = parseFloat(inp.value);
  out.text = fig.innerText;
  for (const k of ["data-bracket", "data-n-whole", "data-alike", "data-k"]) {
    const el = fig.querySelector(`[${k}]`); if (el) out[k] = el.getAttribute(k);
  }
  return out;
}, id);

async function set(page, id, key, v) {
  const counter = "__tie_" + id;
  const before = await page.evaluate((c) => globalThis[c] || 0, counter);
  const input = page.locator(`[data-figure=${id}] input[id="${key}"]`).first();
  await input.fill(String(v));
  await input.press("Enter");
  await page.waitForTimeout(1000);
  const after = await page.evaluate((c) => globalThis[c] || 0, counter);
  check(`${id}: set ${key}=${v}: exactly one solve`, after - before === 1, after - before);
  return readFig(page, id);
}

// ---------------- Archimedes
{
  const { page, errors, remote } = await open(urlA);
  for (const id of ["ngon", "quadrature", "sphere-cylinder"]) {
    const fig = page.locator(`[data-figure=${id}]`);
    await fig.waitFor({ timeout: 60000 });
  }
  await page.locator("[data-figure=ngon]").scrollIntoViewIfNeeded();
  await page.waitForTimeout(1500);
  let s = await readFig(page, "ngon");
  report.ngon0 = s;
  check("ngon starts at 96", s.n === 96, s.n);
  check("96-gon inside Archimedes' bracket", s["data-bracket"] === "inside", s["data-bracket"]);
  await page.locator("[data-figure=ngon]").screenshot({ path: shots + "/ngon-96.png" });
  s = await set(page, "ngon", "n", 6);
  check("n=6: p = 6 d sin(pi/6) = 3d", near(s.p, 3 * s.d, 2e-4), s);
  check("n=6: outside the bracket", s["data-bracket"] === "outside", s["data-bracket"]);
  await page.locator("[data-figure=ngon]").screenshot({ path: shots + "/ngon-6.png" });
  s = await set(page, "ngon", "eps", 0.001);
  check("eps=0.001: n solved, eps(n) = 0.001", near(s.n * (Math.tan(Math.PI / s.n) - Math.sin(Math.PI / s.n)), 0.001, 2e-4) || near(s.eps, 0.001, 1e-5), s);
  report.ngonEps = s;
  const d0 = s.d;
  s = await set(page, "ngon", "p", 2.5);
  check("p=2.5: n held, d = p/(n sin(pi/n))", near(s.d, 2.5 / (s.n * Math.sin(Math.PI / s.n)), 2e-3), s);
  s = await set(page, "ngon", "d", d0);
  check("round trip: d back, p back", near(s.d, d0, 2e-3), s);

  await page.locator("[data-figure=quadrature]").scrollIntoViewIfNeeded();
  s = await readFig(page, "quadrature");
  report.quad0 = s;
  s = await set(page, "quadrature", "k", 6);
  check("k=6: S + R = 4/3 T", near(s.S + s.R, (4 / 3) * s.T, 2e-4), s);
  const R6 = s.R;
  s = await set(page, "quadrature", "k", 1);
  s = await set(page, "quadrature", "R", R6);
  check("R back -> k = 6", near(s.k, 6, 0.01), s);
  await page.locator("[data-figure=quadrature]").screenshot({ path: shots + "/quadrature.png" });

  await page.locator("[data-figure=sphere-cylinder]").scrollIntoViewIfNeeded();
  await page.waitForTimeout(1500);
  s = await set(page, "sphere-cylinder", "V", 2);
  check("V=2: C = 3V/2, K = V/4", near(s.C, 3, 2e-3) && near(s.K, 0.5, 2e-3), s);
  check("V/C = 2/3 shown", /0\.6667/.test(s.text), s.text.slice(0, 300));
  const canv = await page.evaluate(() => document.querySelector("[data-figure=sphere-cylinder]").querySelectorAll("canvas").length);
  check("sphere: one MathBox canvas", canv >= 1, canv);
  await page.locator("[data-figure=sphere-cylinder]").screenshot({ path: shots + "/sphere.png" });

  const grades = await page.evaluate(() => [...document.querySelectorAll("[data-grade]")].map((x) => x.getAttribute("data-grade")));
  const wasm = await page.evaluate(() => globalThis.__wasm.instance);
  check("archimedes: every derivation grade proved", grades.length >= 20 && grades.every((g) => g === "proved"), grades);
  check("archimedes: wasm instantiated", wasm >= 10, wasm);
  check("archimedes: no page errors", errors.length === 0, errors);
  check("archimedes: no remote scripts", remote.length === 0, remote);
  report.archimedes = { grades: grades.length, wasm };
  await page.close();
}

// ---------------- Eudoxus
{
  const { page, errors, remote } = await open(urlE);
  for (const id of ["def-5", "xii-2"]) await page.locator(`[data-figure=${id}]`).waitFor({ timeout: 60000 });
  await page.locator("[data-figure=def-5]").scrollIntoViewIfNeeded();
  await page.waitForTimeout(1000);
  let s = await set(page, "def-5", "C", 2.5);
  check("C moved: C/D = A/B", near(s.C / s.D, s.A / s.B, 1e-3), s);
  check("alike", s["data-alike"] === "alike", s["data-alike"]);
  s = await set(page, "def-5", "m", 9);
  check("m=9: still alike", s["data-alike"] === "alike", s);
  s = await set(page, "def-5", "D", 3.3);
  check("D moved: C = A D / B", near(s.C, s.A * 3.3 / s.B, 2e-3), s);
  await page.locator("[data-figure=def-5]").screenshot({ path: shots + "/def5.png" });
  await page.locator("[data-figure=xii-2]").scrollIntoViewIfNeeded();
  s = await set(page, "xii-2", "k", 5);
  const eps5 = s.eps;
  check("k=5: eps = 1 - 16 sin(pi/16)/pi", near(eps5, 1 - (16 * Math.sin(Math.PI / 16)) / Math.PI, 2e-5), s);
  s = await set(page, "xii-2", "k", 2);
  s = await set(page, "xii-2", "eps", eps5);
  check("eps back -> k = 5", near(s.k, 5, 0.01), s);
  await page.locator("[data-figure=xii-2]").screenshot({ path: shots + "/xii2.png" });
  const grades = await page.evaluate(() => [...document.querySelectorAll("[data-grade]")].map((x) => x.getAttribute("data-grade")));
  const wasm = await page.evaluate(() => globalThis.__wasm.instance);
  check("eudoxus: grades proved", grades.length >= 3 && grades.every((g) => g === "proved"), grades);
  check("eudoxus: wasm instantiated", wasm >= 4, wasm);
  check("eudoxus: no page errors", errors.length === 0, errors);
  check("eudoxus: no remote scripts", remote.length === 0, remote);
  report.eudoxus = { grades: grades.length, wasm };
  await page.close();
}

console.log(JSON.stringify({ ok: fails.length === 0, fails, report }));
await browser.close();
process.exit(fails.length === 0 ? 0 : 1);
