// Drives the Kepler and Galileo widgets of a static build in Chromium:
//   node kepler_galileo_e2e.mjs <widget> <notebook-url> <shots-dir>
// widget: mysterium | orbit | shot. Checks: no page errors, no remote
// scripts, raster's wasm instantiated, every derivation grade proved, and
// the tie: typing a value into one Leva control re-solves the others by
// the widget's equations, exactly one solve per edit (no oscillation), and
// the panel settles. Mysterium: neighbouring radii keep the solids' r/R,
// one MathBox canvas, the shell picked is named. Orbit: T^2 = a^3,
// l = a(1 - e^2), the conic named live (ellipse, circle at e = 0), the
// focal formula substituted, equal areas. Shot: R and H follow v and
// theta, 45 degrees named. Prints one JSON line; exit 1 on failure.
// PLAYWRIGHT names a directory holding node_modules/playwright; CHROMIUM a
// browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [widget, url, shots = "kg-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1300, height: 1100 } });
const errors = [];
const remote = [];
const origin = new URL(url).origin;
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text().slice(0, 300)); });
page.on("request", (r) => { if (r.resourceType() === "script" && !r.url().startsWith(origin) && !r.url().startsWith("data:")) remote.push(r.url()); });
await page.addInitScript(() => {
  const W = globalThis.WebAssembly;
  const counts = (globalThis.__wasm = { instance: 0 });
  W.Instance = new Proxy(W.Instance, {
    construct(t, a, nt) { counts.instance++; return Reflect.construct(t, a, nt); },
  });
});

const spec = {
  mysterium: { figure: "kepler-mysterium", counter: "__mysteriumSolves", keys: ["saturn", "jupiter", "mars", "earth", "venus", "mercury"] },
  orbit: { figure: "kepler-orbit", counter: "__keplerSolves", keys: ["a", "T", "e", "l"] },
  shot: { figure: "galileo-shot", counter: "__galileoSolves", keys: ["v", "theta", "R", "H"] },
}[widget];

await page.goto(url, { waitUntil: "networkidle" });
const fig = page.locator(`[data-figure=${spec.figure}]`);
await fig.waitFor({ timeout: 90000 });
await fig.scrollIntoViewIfNeeded();
await page.waitForTimeout(3000);

const fails = [];
const check = (label, ok, got) => { if (!ok) fails.push({ label, got }); };
const near = (a, b, tol) => Math.abs(a - b) <= tol * Math.max(1, Math.abs(a), Math.abs(b));
const inFig = (sel) => `[data-figure=${spec.figure}] ${sel}`;
const read = () => page.evaluate(({ keys, figure }) => {
  const root = document.querySelector(`[data-figure=${figure}]`);
  const v = {};
  for (const k of keys) { const el = root.closest("div").parentElement.querySelector("#" + k) || document.querySelector("#" + k); v[k] = parseFloat(el.value); }
  const attr = (a) => { const el = root.querySelector(`[${a}]`) || (root.hasAttribute(a) ? root : null); return el && el.getAttribute(a); };
  v.name = (root.querySelector("[data-conic-name]") || root.querySelector("[data-solid-name]") || {}).textContent;
  v.formula = attr("data-formula");
  v.radii = attr("data-radii");
  v.state = attr("data-state");
  v.areas = attr("data-areas");
  v.expected = attr("data-expected");
  v.R = attr("data-R"); v.H = attr("data-H"); v.max = attr("data-max-name");
  return v;
}, spec);
const solves = () => page.evaluate((c) => globalThis[c] || 0, spec.counter);
const solveCounts = [];
const set = async (k, v) => {
  const before = await solves();
  const input = fig.locator("#" + k).first();
  const n = await input.count();
  const target = n ? input : page.locator("#" + k).last();
  await target.fill(String(v));
  await target.press("Enter");
  await page.waitForTimeout(1000);
  const after = await solves();
  solveCounts.push([k, v, after - before]);
  check(`set ${k}=${v}: exactly one solve`, after - before === 1, after - before);
};
const steps = [];

if (widget === "mysterium") {
  const ratios = [1 / Math.sqrt(3), 1 / 3, Math.sqrt((5 + 2 * Math.sqrt(5)) / 15), Math.sqrt((5 + 2 * Math.sqrt(5)) / 15), 1 / Math.sqrt(3)];
  const nested = (st, label) => {
    const s = JSON.parse(st);
    for (let j = 0; j < 5; j++) check(`${label}: r${j + 1}/r${j} = solid's r/R`, near(s[j + 1] / s[j], ratios[j], 1e-9), s);
  };
  let s = await read(); steps.push(["start", s.state]);
  nested(s.state, "start");
  check("shell named", s.name === "dodecahedron", s.name);
  await fig.screenshot({ path: shots + "/mysterium-start.png" });
  await set("earth", 0.1);
  s = await read(); steps.push(["earth=0.1", s.state]);
  nested(s.state, "earth=0.1");
  check("earth holds 0.1", near(JSON.parse(s.state)[3], 0.1, 1e-9), s.state);
  check("formula substituted", s.formula && s.formula.includes("0.1000"), s.formula);
  check("readout radii follow the state", JSON.parse(s.radii).every((r, i) => near(r, JSON.parse(s.state)[i], 1e-9)), [s.radii, s.state]);
  await set("saturn", 0.5);
  s = await read(); steps.push(["saturn=0.5", s.state]);
  nested(s.state, "saturn=0.5");
  await set("mercury", 1);   // past its range: clamped
  s = await read(); steps.push(["mercury=1 (clamped)", s.state]);
  nested(s.state, "mercury clamped");
  check("clamped at Saturn = 1", near(JSON.parse(s.state)[0], 1, 1e-9), s.state);
  await page.locator("#shell").last().fill("5"); await page.locator("#shell").last().press("Enter");
  await page.waitForTimeout(600);
  s = await read();
  check("shell 5 named octahedron", s.name === "octahedron", s.name);
  const canvases = await page.evaluate((f) => document.querySelector(`[data-figure=${f}]`).querySelectorAll("canvas").length, spec.figure);
  check("one MathBox canvas", canvases === 1, canvases);
  const labels = await page.evaluate((f) => [...document.querySelectorAll(`[data-figure=${f}] [data-label]`)].filter((x) => x.style.display !== "none").map((x) => x.getAttribute("data-label")), spec.figure);
  check("planet names over the scene", ["Saturn", "Earth", "Mercury"].every((l) => labels.includes(l)), labels);
  await fig.screenshot({ path: shots + "/mysterium-octahedron.png" });
}

if (widget === "orbit") {
  const tiedOk = (s, label) => {
    check(`${label}: T^2 = a^3`, near(s.T * s.T, s.a ** 3, 0.01), s);
    check(`${label}: l = a(1 - e^2)`, near(s.l, s.a * (1 - s.e * s.e), 0.01), s);
  };
  let s = await read(); steps.push(["start", s]);
  tiedOk(s, "start");
  check("named ellipse", s.name === "ellipse", s.name);
  await set("a", 2);
  s = await read(); steps.push(["a=2", s]); tiedOk(s, "a=2");
  check("formula substituted", s.formula && s.formula.includes(s.l.toFixed(2).slice(0, 3)), s.formula);
  await set("T", 8);
  s = await read(); steps.push(["T=8", s]); tiedOk(s, "T=8");
  check("a = 4 from T = 8", near(s.a, 4, 0.01), s);
  await set("e", 0);
  s = await read(); steps.push(["e=0", s]);
  check("named circle at e = 0", s.name === "circle", s.name);
  await set("e", 0.6);
  await set("l", 1);
  s = await read(); steps.push(["l=1", s]); tiedOk(s, "l=1");
  const areas = JSON.parse(s.areas);
  check("equal areas", areas.every((x) => near(x, parseFloat(s.expected), 1e-4)), [areas, s.expected]);
  await fig.screenshot({ path: shots + "/orbit.png" });
}

if (widget === "shot") {
  const g = 9.81, rad = (d) => (d * Math.PI) / 180;
  let s = await read(); steps.push(["start", s]);
  await set("theta", 45);
  s = await read(); steps.push(["theta=45", s]);
  check("R = v^2 sin 2theta / g", near(parseFloat(s.R), (s.v * s.v * Math.sin(rad(90))) / g, 1e-3), s);
  check("45 named max", s.max === "max", s.max);
  await set("R", 30);
  s = await read(); steps.push(["R=30", s]);
  check("R = 30 holds, v solved back", near(parseFloat(s.R), 30, 1e-3) && near(s.v, Math.sqrt(30 * g), 1e-3), s);
  await set("theta", 60);
  s = await read();
  check("H = v^2 sin^2 / 2g", near(parseFloat(s.H), (s.v * s.v * Math.sin(rad(60)) ** 2) / (2 * g), 1e-3), s);
  await fig.screenshot({ path: shots + "/shot.png" });
}

const n1 = await solves();
await page.waitForTimeout(1500);
check("no solve without input", (await solves()) === n1, [n1, await solves()]);
const wasm = await page.evaluate(() => globalThis.__wasm.instance);
const grades = await page.evaluate(() => [...document.querySelectorAll("[data-grade]")].map((x) => x.getAttribute("data-grade")));
check("wasm kernels instantiated", wasm >= 2, wasm);
check("every grade proved (or numeric where Kepler's numbers are checked)",
  grades.length > 0 && grades.includes("proved") && grades.every((g) => g === "proved" || g === "numeric"),
  grades.filter((g) => g !== "proved" && g !== "numeric"));
check("no remote scripts", remote.length === 0, remote);
check("no page errors", errors.length === 0, errors);

console.log(JSON.stringify({ widget, ok: fails.length === 0, fails, wasm, grades: grades.length, solveCounts, steps: steps.length }));
await browser.close();
process.exit(fails.length === 0 ? 0 : 1);
