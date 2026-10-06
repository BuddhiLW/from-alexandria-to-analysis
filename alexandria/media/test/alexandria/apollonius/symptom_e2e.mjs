// Drives the live symptoma widget (Apollonius I.11-13) of a static build in
// Chromium:
//   node symptom_e2e.mjs <notebook-url> <shots-dir>
// Checks: no page errors, a MathBox canvas in the widget, letters as HTML
// (no canvas text: MathBox Label/Format/Text absent, letters in the
// [data-html-labels] overlay), WebAssembly instantiated. Sweeps the tilt
// slider 25 -> 80 and x, then sets y; at each state the two sides of the
// symptoma (both kernel outputs, data-lhs / data-rhs) agree within 1e-4,
// the name matches the sign of c, and the state lies on the section.
// Screenshots per tilt. Prints one JSON line; exit 1 on failure.
// PLAYWRIGHT names a directory holding node_modules/playwright; CHROMIUM a
// browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "symptom-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1400, height: 1000 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text().slice(0, 300)); });
await page.addInitScript(() => {
  const W = globalThis.WebAssembly;
  const counts = (globalThis.__wasm = { instantiate: 0, instance: 0 });
  const inst = W.instantiate.bind(W);
  W.instantiate = (...a) => { counts.instantiate++; return inst(...a); };
  W.Instance = new Proxy(W.Instance, {
    construct(t, a, nt) { counts.instance++; return Reflect.construct(t, a, nt); },
  });
  // canvas text: count fillText/strokeText calls on 2D contexts
  globalThis.__canvasText = 0;
  const P = CanvasRenderingContext2D.prototype;
  for (const k of ["fillText", "strokeText"]) {
    const f = P[k];
    P[k] = function (...a) { globalThis.__canvasText++; return f.apply(this, a); };
  }
});

await page.goto(url, { waitUntil: "networkidle" });
const root = page.locator("[data-symptom]").first();
await root.waitFor({ timeout: 90000 });
await page.locator("[data-figure=apollonius-symptom]").scrollIntoViewIfNeeded();
await page.waitForTimeout(2500);

const setRange = async (key, v) => {
  await page.locator(`[data-symptom] input[data-control=${key}]`).evaluate((el, v) => {
    const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value").set;
    setter.call(el, String(v));
    el.dispatchEvent(new Event("input", { bubbles: true }));
    el.dispatchEvent(new Event("change", { bubbles: true }));
  }, v);
  await page.waitForTimeout(250);
};

const read = async () => page.evaluate(() => {
  const r = document.querySelector("[data-symptom]");
  const t = r.querySelector("[data-symptom-tex]");
  const nm = r.querySelector("[data-name]");
  return {
    tilt: +r.dataset.tilt, x: +r.dataset.x, y: +r.dataset.y, origin: r.dataset.origin,
    lhs: +t.dataset.lhs, rhs: +t.dataset.rhs, c: +t.dataset.c,
    name: nm.dataset.name, nameText: nm.innerText,
    katex: !!t.querySelector(".katex"),
    areas: [...r.querySelectorAll("[data-area]")].map((g) => g.dataset.area),
  };
});

const greek = { parabola: "παραβολή", hyperbola: "ὑπερβολή", ellipse: "ἔλλειψις" };
const expect = (c) => (Math.abs(c) < 0.004 ? "parabola" : c > 0 ? "hyperbola" : "ellipse");
const states = [];
const check = (s, label) => {
  const ok = Math.abs(s.lhs - s.rhs) < 1e-4 && Math.abs(s.y * s.y - s.rhs) < 1e-4 &&
    s.name === expect(s.c) && s.nameText.includes(greek[s.name]) && s.katex &&
    s.areas.includes("y2") && s.areas.includes("px") && (s.name === "parabola" || s.areas.includes("cx2"));
  states.push({ label, ok, ...s, nameText: undefined });
  return ok;
};

for (const tilt of [25, 35, 50, 59, 59.1, 65, 80]) {
  await setRange("tilt", tilt);
  check(await read(), `tilt ${tilt}`);
  await page.locator("[data-figure=apollonius-symptom]").screenshot({ path: `${shots}/tilt-${tilt}.png` });
}
for (const tilt of [35, 80]) {
  await setRange("tilt", tilt);
  for (const x of [0.1, 0.5, 1.2, 1.6]) {
    await setRange("x", x);
    check(await read(), `tilt ${tilt} x ${x}`);
  }
  for (const y of [0.3, 0.6]) {
    await setRange("y", y);
    const s = await read();
    check(s, `tilt ${tilt} y ${y}`);
    if (Math.abs(s.y - y) > 1e-6 && s.origin !== "pushed") states.push({ label: `y not held ${y}`, ok: false });
  }
}
await page.screenshot({ path: `${shots}/page.png`, fullPage: false });
// the sweep button runs the tilt 25 -> 80
await setRange("tilt", 30);
await page.locator("[data-symptom] [data-act=sweep]").click();
await page.waitForTimeout(10000);
const swept = await read();
check(swept, "after sweep");

const wasm = await page.evaluate(() => globalThis.__wasm);
const canvasText = await page.evaluate(() => globalThis.__canvasText);
const canvases = await page.locator("[data-figure=apollonius-symptom] canvas").count();
const letters = await page.locator("[data-figure=apollonius-symptom] [data-label]").allInnerTexts();
const notation = await page.locator("[data-notation-step]").count();
const ok = errors.length === 0 && canvases > 0 && states.every((s) => s.ok) &&
  wasm.instantiate + wasm.instance > 0 && canvasText === 0 && notation === 1 &&
  ["P", "Q", "V", "L"].every((l) => letters.includes(l)) && swept.tilt > 79;
console.log(JSON.stringify({ ok, url, errors, canvases, canvasText, notation, letters, wasm, swept, states }));
await browser.close();
process.exit(ok ? 0 : 1);
