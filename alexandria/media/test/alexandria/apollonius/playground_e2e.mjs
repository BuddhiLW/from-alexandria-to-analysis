// Drives the conics playground (polar coordinates as a projection) of a
// static build in Chromium:
//   node playground_e2e.mjs <notebook-url> <shots-dir>
// Checks: no page errors, one MathBox canvas, raster's wasm instantiated,
// every derivation grade proved, and the tie: typing a value into one Leva
// control re-solves the others by the equations (alpha fixed for tilt, h,
// e, l; the plane fixed for alpha), clamps at the sliders' edges, and the
// panel settles. Every edit runs the solver exactly once (the pushed values
// never re-fire it: no oscillation). X' from the cone and the plane (the 3D
// shadow) agrees with X' from rho = l/(1 - e cos theta) (the 2D curve)
// within 1e-4 at every step. The HTML letters X, X', F ride over the scene.
// The point X runs (theta changes between two reads). Prints one JSON line;
// exit 1 on failure. PLAYWRIGHT names a directory holding
// node_modules/playwright; CHROMIUM a browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "playground-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1300, height: 1000 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text().slice(0, 300)); });
await page.addInitScript(() => {
  const W = globalThis.WebAssembly;
  const counts = (globalThis.__wasm = { instance: 0 });
  W.Instance = new Proxy(W.Instance, {
    construct(t, a, nt) { counts.instance++; return Reflect.construct(t, a, nt); },
  });
});

await page.goto(url, { waitUntil: "networkidle" });
const fig = page.locator("[data-figure=conics-playground]");
await fig.waitFor({ timeout: 60000 });
await page.waitForTimeout(2500);

const keys = ["alpha", "tilt", "h", "l", "e"];
const read = () => page.evaluate((keys) => {
  const v = {};
  for (const k of keys) v[k] = parseFloat(document.getElementById(k).value);
  v.theta = parseFloat(document.querySelector("[data-x-theta]").getAttribute("data-x-theta"));
  v.name = document.querySelector("[data-conic-name]").textContent;
  v.formula = document.querySelector("[data-formula]").getAttribute("data-formula");
  return v;
}, keys);
const rad = (d) => (d * Math.PI) / 180;
const near = (a, b, tol) => Math.abs(a - b) <= tol;
const fails = [];
const check = (label, ok, got) => { if (!ok) fails.push({ label, got }); };

const solves = () => page.evaluate(() => globalThis.__playgroundSolves || 0);
const solveCounts = [];
const set = async (k, v) => {
  const before = await solves();
  const input = page.locator("#" + k);
  await input.fill(String(v));
  await input.press("Enter");
  await page.waitForTimeout(900);
  const after = await solves();
  solveCounts.push([k, v, after - before]);
  check(`set ${k}=${v}: exactly one solve (no oscillation)`, after - before === 1, after - before);
};
// X' from the cone and the plane against X' from l and e, at the same theta
const agreement = [];
const agree = async (label) => {
  const r = await page.evaluate(() => {
    const el = document.querySelector("[data-x3]");
    const a = el.getAttribute("data-x3").split(" ").map(Number);
    const b = el.getAttribute("data-x2").split(" ").map(Number);
    return { a, b, d: Math.hypot(a[0] - b[0], a[1] - b[1]) };
  });
  agreement.push([label, r.d]);
  check(label + ": 3D shadow = 2D curve at X within 1e-4", Number.isFinite(r.d) ? r.d <= 1e-4 : !Number.isFinite(r.a[0]) && !Number.isFinite(r.b[0]), r);
};

const s0 = await read();
await page.waitForTimeout(500);
const s0b = await read();
check("X runs (theta moves)", s0.theta !== s0b.theta, [s0.theta, s0b.theta]);
check("start: l = h tan alpha", near(s0.l, s0.h * Math.tan(rad(s0.alpha)), 0.006), s0);
check("start: e = tan tilt tan alpha", near(s0.e, Math.tan(rad(s0.tilt)) * Math.tan(rad(s0.alpha)), 0.006), s0);
await agree("start");

const steps = [];
// tilt moves: alpha, h fixed; l, e re-solved
await set("tilt", 60);
let s = await read(); steps.push(["tilt=60", s]);
check("tilt: alpha, h, l fixed", s.alpha === s0.alpha && s.h === s0.h && near(s.l, s0.l, 0.006), s);
check("tilt: tilt kept as typed", s.tilt === 60, s);
check("tilt: e = tan 60 tan alpha", near(s.e, Math.tan(rad(60)) * Math.tan(rad(s.alpha)), 0.006), s);
check("tilt: hyperbola", s.name === "hyperbola", s.name);
await agree("tilt=60");
await fig.screenshot({ path: shots + "/hyperbola.png" });
// e moves: alpha, l fixed; tilt, h re-solved (parabola)
const prev = s;
await set("e", 1);
s = await read(); steps.push(["e=1", s]);
check("e: e kept as typed", s.e === 1, s);
check("e: alpha, l fixed", s.alpha === prev.alpha && near(s.l, prev.l, 0.006), s);
check("e: tilt = atan(1/tan alpha)", near(s.tilt, (Math.atan(1 / Math.tan(rad(s.alpha))) * 180) / Math.PI, 0.26), s);
check("e: h = l / tan alpha", near(s.h, s.l / Math.tan(rad(s.alpha)), 0.006), s);
check("e: parabola", s.name === "parabola", s.name);
await agree("e=1");
await fig.screenshot({ path: shots + "/parabola.png" });
// l moves past what h allows: clamped at h = 2.5
await set("l", 2.9);
s = await read(); steps.push(["l=2.9", s]);
check("l: h = l / tan alpha, clamped at 2.5", near(s.h, 2.5, 0.006) && near(s.l, 2.5 * Math.tan(rad(s.alpha)), 0.006), s);
await agree("l=2.9");
// alpha moves: the plane fixed, l and e re-solved
const before = s;
await set("alpha", 25);
s = await read(); steps.push(["alpha=25", s]);
check("alpha: tilt, h fixed", near(s.tilt, before.tilt, 0.06) && near(s.h, before.h, 0.006), s);
check("alpha: l, e forward", near(s.l, s.h * Math.tan(rad(25)), 0.006) && near(s.e, Math.tan(rad(s.tilt)) * Math.tan(rad(25)), 0.006), s);
await agree("alpha=25");
// tilt then h: an ellipse
await set("tilt", 20);
await set("h", 1.5);
s = await read(); steps.push(["tilt=20,h=1.5", s]);
check("h: l = h tan alpha", near(s.l, 1.5 * Math.tan(rad(s.alpha)), 0.006), s);
check("ellipse again", s.name === "ellipse", s.name);
await agree("tilt=20,h=1.5");
await fig.screenshot({ path: shots + "/ellipse.png" });
// settled: nothing changes without input, and no solve runs
const n1 = await solves();
const t1 = await read(); await page.waitForTimeout(1500); const t2 = await read();
check("panel settles", keys.every((k) => t1[k] === t2[k]), [t1, t2]);
check("no solve without input", (await solves()) === n1, [n1, await solves()]);
for (let i = 0; i < 4; i++) { await page.waitForTimeout(250); await agree("running X " + i); }

const wasm = await page.evaluate(() => globalThis.__wasm.instance);
const grades = await page.evaluate(() => [...document.querySelectorAll("[data-grade]")].map((x) => x.getAttribute("data-grade")));
const letters = await page.evaluate(() => [...document.querySelectorAll("[data-figure=conics-playground] [data-label]")]
  .filter((x) => x.style.display !== "none").map((x) => x.getAttribute("data-label")));
const canvases = await page.evaluate(() => document.querySelectorAll("canvas").length);
check("wasm kernels instantiated", wasm >= 15, wasm);
check("every grade proved", grades.length > 0 && grades.every((g) => g === "proved"), grades);
check("HTML letters over the scene", ["X", "X\u2032", "F"].every((l) => letters.includes(l)), letters);
check("one MathBox canvas", canvases === 1, canvases);
check("no page errors", errors.length === 0, errors);

console.log(JSON.stringify({ ok: fails.length === 0, fails, wasm, grades: grades.length, solveCounts, agreement, steps }));
await browser.close();
process.exit(fails.length === 0 ? 0 : 1);
