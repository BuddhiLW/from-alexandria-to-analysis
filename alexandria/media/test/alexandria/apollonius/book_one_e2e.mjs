// Drives the Book I.11-14 walk (alexandria.apollonius.book-one-scene) of a
// static build in Chromium, served over http:
//   node book_one_e2e.mjs <notebook-url> <shots-dir>
// For each proposition I.11-I.14: the stepped figure plays (next/step
// count), the SVG letters are drawn, the tied controls x and y round-trip
// (move y, x is solved; put y back, x returns), QV² equals the applied
// rectangle (data-qv2 vs data-rect, both kernel outputs) within 1e-4, the
// two ratios Heath compares agree, and the live name is the proposition's
// case (parabola / hyperbola / ellipse; I.14 a hyperbola). Also: no page
// errors, no remote scripts, WebAssembly instantiated. Screenshots per
// proposition. Prints one JSON line; exit 1 on failure.
// PLAYWRIGHT names a directory holding node_modules/playwright; CHROMIUM a
// browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "book-one-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1400, height: 1000 } });
page.setDefaultTimeout(90000);
const errors = [];
const remote = [];
const origin = new URL(url).origin;
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text().slice(0, 300)); });
page.on("request", (r) => {
  const u = r.url();
  if (r.resourceType() === "script" && /^https?:/.test(u) && !u.startsWith(origin)) remote.push(u);
});
await page.addInitScript(() => {
  const W = globalThis.WebAssembly;
  const counts = (globalThis.__wasm = { instantiate: 0, instance: 0 });
  const inst = W.instantiate.bind(W);
  W.instantiate = (...a) => { counts.instantiate++; return inst(...a); };
  W.Instance = new Proxy(W.Instance, {
    construct(t, a, nt) { counts.instance++; return Reflect.construct(t, a, nt); },
  });
});

await page.goto(url, { waitUntil: "networkidle" });
await page.locator("[data-book-one]").first().waitFor({ timeout: 90000 });

const expected = { "I.11": "parabola", "I.12": "hyperbola", "I.13": "ellipse", "I.14": "hyperbola" };
const results = {};
const fails = [];
const close = (a, b, tol = 1e-4) => Math.abs(a - b) <= tol * Math.max(1, Math.abs(a), Math.abs(b));

for (const id of Object.keys(expected)) {
  const sel = `[data-book-one="${id}"]`;
  const root = page.locator(sel);
  if ((await root.count()) !== 1) { fails.push(`${id}: ${await root.count()} roots`); continue; }
  await page.evaluate((s) => document.querySelector(s)?.scrollIntoView({ block: "center" }), sel);
  await page.waitForTimeout(1200);

  const setRange = async (key, v) => {
    await root.locator(`input[data-control=${key}]`).evaluate((el, v) => {
      const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value").set;
      setter.call(el, String(v));
      el.dispatchEvent(new Event("input", { bubbles: true }));
      el.dispatchEvent(new Event("change", { bubbles: true }));
    }, v);
    await page.waitForTimeout(250);
  };
  const read = async () => root.evaluate((r) => {
    const w = r.querySelector("[data-book-widget]");
    return {
      step: +r.dataset.step, tilt: +r.dataset.tilt, x: +r.dataset.x, y: +r.dataset.y,
      origin: r.dataset.origin, kind: w.dataset.kind,
      qv2: +w.dataset.qv2, rect: +w.dataset.rect, hvvk: +w.dataset.hvvk,
      rl: +w.dataset.ratioL, rr: +w.dataset.ratioR,
      letters: r.querySelectorAll("svg[data-figure] text[data-label]").length,
      katex: r.querySelectorAll(".katex").length,
    };
  });
  const check = (s, tag) => {
    if (!close(s.qv2, s.rect)) fails.push(`${id} ${tag}: QV² ${s.qv2} vs rect ${s.rect}`);
    if (!close(s.qv2, s.hvvk)) fails.push(`${id} ${tag}: QV² ${s.qv2} vs HV·VK ${s.hvvk}`);
    if (!close(s.rl, s.rr)) fails.push(`${id} ${tag}: ratios ${s.rl} vs ${s.rr}`);
    if (s.kind !== expected[id]) fails.push(`${id} ${tag}: kind ${s.kind}`);
  };

  // the steps: play through to the last
  const s0 = await read();
  const n = await root.locator("li[data-step-id]").count();
  for (let i = 1; i < n; i++) { await root.locator("button[data-act=next]").click(); await page.waitForTimeout(150); }
  await page.waitForTimeout(6000);
  const sEnd = await read();
  if (sEnd.step !== n) fails.push(`${id}: step ${sEnd.step} of ${n}`);
  if (sEnd.letters < 8) fails.push(`${id}: ${sEnd.letters} letters`);
  check(sEnd, "start");
  await page.screenshot({ path: `${shots}/book-one-${id}.png`, clip: await root.boundingBox() });

  // the tie: x moves y; y moves x; y back returns x
  await setRange("x", 0.6);
  const a = await read();
  check(a, "x=0.6");
  if (!close(a.x, 0.6, 1e-3)) fails.push(`${id}: x ${a.x}`);
  await setRange("y", Math.max(0.05, a.y - 0.05));
  const b = await read();
  check(b, "y-");
  if (close(b.x, a.x, 1e-6)) fails.push(`${id}: x did not follow y`);
  await setRange("y", a.y);
  const c = await read();
  if (!close(c.x, a.x, 2e-3)) fails.push(`${id}: round-trip x ${a.x} -> ${c.x}`);

  // the tilt (not I.11: held)
  if (id !== "I.11") {
    const t0 = c.tilt;
    const lo = +(await root.locator("input[data-control=tilt]").getAttribute("min"));
    const hi = +(await root.locator("input[data-control=tilt]").getAttribute("max"));
    await setRange("tilt", (lo + hi) / 2 + 1.3);
    const d = await read();
    check(d, "tilt");
    if (close(d.tilt, t0, 1e-6)) fails.push(`${id}: tilt did not move`);
    if (!close(d.x, c.x, 1e-6)) fails.push(`${id}: tilt moved x`);
  }
  await page.screenshot({ path: `${shots}/book-one-${id}-tied.png`, clip: await root.boundingBox() });
  results[id] = { steps: n, letters: sEnd.letters, katex: sEnd.katex, kind: sEnd.kind, start: s0.step, x: a.x, y: a.y };
}

const wasm = await page.evaluate(() => globalThis.__wasm);
const grades = await page.locator("[data-grade]").evaluateAll((els) => els.map((e) => e.dataset.grade));
if (!(wasm.instantiate + wasm.instance > 0)) fails.push("no wasm");
if (errors.length) fails.push(...errors.slice(0, 5));
if (remote.length) fails.push("remote scripts: " + remote.slice(0, 3).join(" "));
const unproved = grades.filter((g) => g !== "proved");
if (unproved.length) fails.push(`grades not proved: ${unproved.length}`);
console.log(JSON.stringify({ ok: fails.length === 0, fails, wasm, grades: grades.length, results }));
await browser.close();
process.exit(fails.length ? 1 : 0);
