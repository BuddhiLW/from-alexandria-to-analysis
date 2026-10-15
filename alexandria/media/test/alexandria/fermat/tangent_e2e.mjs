// Drives the Descartes-Fermat widgets of a static build in Chromium:
//   node tangent_e2e.mjs <notebook-url> <shots-dir>
// Checks: no page errors, no remote scripts, raster's wasm instantiated,
// every derivation grade proved; the tie: typing CD, CE, DP or p into the
// Leva panel re-solves the others (CE = 2 CD, DP = CD + p/2) with exactly
// one solve per edit (pushed values never re-fire it), and the round trip
// CD -> CE -> CD comes back; Descartes' circle is a double root only at
// P; the Isagoge panel names the curve live as the equation and k move;
// the Mafs figure changes when a slider moves. The residual of I in its equation is held to 1e-4: the
// circle and ellipse go through the kernels' sin and cos. Prints one JSON line; exit 1
// on failure. PLAYWRIGHT names a directory holding node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "fermat-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1300, height: 1000 } });
const errors = [];
const remote = [];
const host = new URL(url).host;
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text().slice(0, 300)); });
page.on("request", (r) => {
  if (r.resourceType() === "script" && new URL(r.url()).host !== host) remote.push(r.url());
});
await page.addInitScript(() => {
  const W = globalThis.WebAssembly;
  const counts = (globalThis.__wasm = { instance: 0 });
  W.Instance = new Proxy(W.Instance, {
    construct(t, a, nt) { counts.instance++; return Reflect.construct(t, a, nt); },
  });
});

await page.goto(url, { waitUntil: "networkidle" });
const fig = page.locator("[data-figure=fermat-tangent-normal][data-focus=fermat]");
await fig.waitFor({ timeout: 60000 });
await fig.scrollIntoViewIfNeeded();
await page.waitForTimeout(2500);

const fails = [];
const check = (label, ok, got) => { if (!ok) fails.push({ label, got }); };
const near = (a, b, tol = 0.011) => Math.abs(a - b) <= tol;
const keys = ["p", "c", "d", "ce", "np"];
const read = () => page.evaluate((keys) => {
  const v = {};
  const root = document.querySelector("[data-figure=fermat-tangent-normal][data-focus=fermat]");
  for (const k of keys) v[k] = parseFloat(root.querySelector("#" + k).value);
  v.delta = parseFloat(root.querySelector("[data-delta]").getAttribute("data-delta"));
  v.formula = root.querySelector("[data-formula=fermat]").getAttribute("data-tex");
  v.name = root.querySelector("[data-conic-name]").getAttribute("data-conic-name");
  v.svg = document.querySelector("[data-figure=fermat-tangent-normal][data-focus=fermat] svg")?.innerHTML.length;
  v.path = [...document.querySelectorAll("[data-figure=fermat-tangent-normal] svg line, [data-figure=fermat-tangent-normal] svg polyline")].map((e) => e.outerHTML).join("").length;
  return v;
}, keys);
const solves = () => page.evaluate(() => globalThis.__fermatSolves || 0);
const set = async (k, v) => {
  const before = await solves();
  const input = fig.locator("#" + k);
  await input.fill(String(v));
  await input.press("Enter");
  await page.waitForTimeout(900);
  const n = (await solves()) - before;
  check(`set ${k}=${v}: exactly one solve`, n === 1, n);
};
const tie = (s, label) => {
  const q = s.p + 2 * s.c * s.d;
  check(label + ": CE = 2 BC^2 / (p + 2c CD)", near(s.ce, (2 * s.d * (s.p + s.c * s.d)) / q, 0.03), s);
  check(label + ": DP = CD (1 + c) + p/2", near(s.np, s.d * (1 + s.c) + s.p / 2, 0.03), s);
};

const s0 = await read();
tie(s0, "start");
await page.screenshot({ path: shots + "/tangent-start.png", clip: await fig.boundingBox() });
const figBefore = await page.evaluate(() => document.querySelector("[data-figure=fermat-tangent-normal][data-focus=fermat] svg").innerHTML);
await set("d", 1.6); const s1 = await read(); tie(s1, "CD moved"); check("CD = 1.6", near(s1.d, 1.6), s1);
const figAfter = await page.evaluate(() => document.querySelector("[data-figure=fermat-tangent-normal][data-focus=fermat] svg").innerHTML);
check("the figure moves with the slider", figBefore !== figAfter, null);
await set("ce", 2.4); const s2 = await read(); tie(s2, "CE moved"); check("CD = CE/2 = 1.2", near(s2.d, 1.2), s2);
await set("np", 2.5); const s3 = await read(); tie(s3, "DP moved"); check("CD = DP - p/2 = 2", near(s3.d, 2), s3);
await set("p", 2); const s4 = await read(); tie(s4, "p moved"); check("CD held at 2", near(s4.d, 2), s4);
await set("d", 1); await set("ce", 2.6); await set("d", 1); const s5 = await read();
check("round trip CD -> CE -> CD", near(s5.d, 1) && near(s5.ce, 2), s5);
// Descartes' circle: double root only with the centre on P
await fig.locator("#off").fill("0"); await fig.locator("#off").press("Enter"); await page.waitForTimeout(600);
const d0 = (await read()).delta;
check("centre on P: discriminant 0", Math.abs(d0) < 1e-9, d0);
await fig.locator("#off").fill("0.5"); await fig.locator("#off").press("Enter"); await page.waitForTimeout(600);
const d1 = (await read()).delta;
check("centre past P: discriminant positive", d1 > 1e-6, d1);
await page.screenshot({ path: shots + "/tangent-moved.png", clip: await fig.boundingBox() });

// the excess c names the section live (Apollonius' names)
check("starts as the parabola", (await read()).name === "parabola", (await read()).name);
await set("c", -0.4); const s6 = await read(); tie(s6, "c moved"); check("c < 0: ellipse", s6.name === "ellipse", s6.name);
check("formula substituted", s6.formula.includes("0.400") || s6.formula.includes("−0.400"), s6.formula);
await set("c", 0.6); const s7 = await read(); tie(s7, "c > 0"); check("c > 0: hyperbola", s7.name === "hyperbola", s7.name);
await set("ce", 3); const s8 = await read(); tie(s8, "CE moved on the hyperbola"); check("CE = 3", near(s8.ce, 3), s8);
await page.screenshot({ path: shots + "/tangent-hyperbola.png", clip: await fig.boundingBox() });
// Descartes' own framing: his ellipse, DP = 3/2, double root at P
const dfig = page.locator("[data-figure=fermat-tangent-normal][data-focus=descartes]");
await dfig.scrollIntoViewIfNeeded(); await page.waitForTimeout(800);
const dname = await dfig.locator("[data-conic-name]").getAttribute("data-conic-name");
check("Descartes opens on his ellipse", dname === "ellipse", dname);
const dnp = parseFloat(await dfig.locator("#np").inputValue());
check("Descartes: DP = 3/2", near(dnp, 1.5), dnp);
await dfig.locator("#off").fill("0"); await dfig.locator("#off").press("Enter"); await page.waitForTimeout(600);
const dd = parseFloat(await dfig.locator("[data-delta]").getAttribute("data-delta"));
check("Descartes: centre on P, double root", Math.abs(dd) < 1e-9, dd);
await page.screenshot({ path: shots + "/descartes-ellipse.png", clip: await dfig.boundingBox() });

const grades = await page.evaluate(() => [...document.querySelectorAll("[data-grade]")].map((e) => e.getAttribute("data-grade")));
check("derivation grades all proved", grades.length >= 15 && grades.every((g) => g === "proved"), grades);

// The Isagoge panel
const loci = page.locator("[data-figure=fermat-loci]");
await loci.scrollIntoViewIfNeeded();
await page.waitForTimeout(800);
const name = () => page.evaluate(() => document.querySelector("[data-locus-name]").getAttribute("data-locus-name"));
const residual = () => page.evaluate(() => parseFloat(document.querySelector("[data-residual]").getAttribute("data-residual")));
const names = { start: await name() };
check("starts as an ellipse", names.start === "ellipse", names.start);
check("I on the curve", Math.abs(await residual()) < 1e-4, await residual());
await page.locator("#k").fill("1"); await page.locator("#k").press("Enter"); await page.waitForTimeout(600);
names.k1 = await name();
check("k = 1: a circle", names.k1 === "circle", names.k1);
await page.screenshot({ path: shots + "/loci-circle.png", clip: await loci.boundingBox() });
// pick each equation from Leva's select
const select = loci.locator("select");
const picked = [];
for (const [label, want] of [["A in E = Z pl.", "hyperbola"], ["A q. = D in E", "parabola"], ["D in A = B in E", "line"], ["B q. − A q. = E q.", "circle"], ["(B q. + A q.) : E q. = k", "hyperbola"]]) {
  await select.selectOption({ label });
  await page.waitForTimeout(600);
  const n = await name();
  picked.push([label, n]);
  check(`${label}: ${want}`, n === want, n);
  check(`${label}: I on the curve`, Math.abs(await residual()) < 1e-4, await residual());
}
await page.screenshot({ path: shots + "/loci-hyperbola.png", clip: await loci.boundingBox() });

const wasm = await page.evaluate(() => globalThis.__wasm.instance);
check("raster wasm instantiated", wasm > 0, wasm);
check("no page errors", errors.length === 0, errors);
check("no remote scripts", remote.length === 0, remote);
await page.screenshot({ path: shots + "/page.png", fullPage: false });
console.log(JSON.stringify({ ok: fails.length === 0, fails, wasm, names, picked, states: [s0, s1, s2, s3, s4, s5, s6, s7, s8].map(({ formula, svg, path, ...s }) => s), errors: errors.slice(0, 5) }));
await browser.close();
process.exit(fails.length ? 1 : 0);
