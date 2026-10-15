// Drives the Euclid notebook of a static build in Chromium:
//   node euclid_e2e.mjs <notebook-url> <shots-dir>
// Checks: no page errors, no remote scripts, WebAssembly instantiated
// (emmy-viewers' raster glue); I.1's player steps twelve sentences; the
// I.47 widget: c typed to 4 at A = 90 gives a = 5.657 (I.47), A typed to
// 120 names II.12 (obtuse) and a^2 grows, A typed to 60 names II.13, a
// typed back re-solves c (b and A held, a^2 = b^2 + c^2 - 2bc cos A), B
// dragged changes the state; the application widget: k typed to 0, 0.5,
// -1 names parabole, hyperbole, elleipsis; S typed re-solves y; every Emmy
// grade proved. Prints one JSON line; exit 1 on failure. PLAYWRIGHT names a
// directory holding node_modules/playwright; CHROMIUM a browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "euclid-shots"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1300, height: 950 } });
const errors = [];
const remote = [];
const origin = new URL(url).origin;
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text().slice(0, 300)); });
page.on("request", (r) => {
  if (r.resourceType() === "script" && !r.url().startsWith(origin)) remote.push(r.url());
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
await page.waitForTimeout(2000);

const state = async (fig) => (await fig.getAttribute("data-state")).split(" ").map(Number);
const typeInto = async (fig, label, v) => {
  // Leva rows: the label text, then a number input
  const row = fig.locator("label", { hasText: label }).first();
  const input = row.locator("xpath=ancestor::div[1]/..").locator("input:not([type=range])").first();
  await input.fill(String(v)); await input.press("Enter"); await page.waitForTimeout(800);
};

// ---- I.47
const py = page.locator("[data-figure=euclid-pythagoras]").first();
await py.waitFor({ timeout: 60000 });
await py.scrollIntoViewIfNeeded();
await page.waitForTimeout(800);
const pyRes = { start: { state: await state(py), kind: await py.getAttribute("data-kind") } };
await py.screenshot({ path: `${shots}/i47-start.png` });
await typeInto(py, "c = AB", 4);
pyRes.c4 = { state: await state(py), kind: await py.getAttribute("data-kind") };
await typeInto(py, "angle A", 120);
pyRes.A120 = { state: await state(py), kind: await py.getAttribute("data-kind"),
               name: await py.locator("[data-case]").innerText() };
await py.screenshot({ path: `${shots}/i47-obtuse.png` });
await typeInto(py, "angle A", 60);
pyRes.A60 = { state: await state(py), kind: await py.getAttribute("data-kind"),
              name: await py.locator("[data-case]").innerText() };
await py.screenshot({ path: `${shots}/i47-acute.png` });
await typeInto(py, "a = BC", 5);
pyRes.a5 = { state: await state(py) };
// drag B
const handle = py.locator("[data-drag=B]");
const box = await handle.boundingBox();
await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
await page.mouse.down();
await page.mouse.move(box.x - 60, box.y - 30, { steps: 8 });
await page.mouse.up();
await page.waitForTimeout(600);
pyRes.drag = { state: await state(py), kind: await py.getAttribute("data-kind") };
await py.screenshot({ path: `${shots}/i47-dragged.png` });

const law = ([a, b, c, A]) => Math.abs(a * a - (b * b + c * c - 2 * b * c * Math.cos(A * Math.PI / 180)));
const pyOk = pyRes.start.kind === "right"
  && Math.abs(pyRes.c4.state[0] - Math.sqrt(32)) < 1e-6 && pyRes.c4.kind === "right"
  && pyRes.A120.kind === "obtuse" && pyRes.A120.name.startsWith("II.12")
  && pyRes.A60.kind === "acute" && pyRes.A60.name.startsWith("II.13")
  && Math.abs(pyRes.a5.state[0] - 5) < 1e-9 && pyRes.a5.state[3] === 60
  && [pyRes.c4, pyRes.A120, pyRes.A60, pyRes.a5, pyRes.drag].every((s) => law(s.state) < 1e-6)
  && pyRes.drag.state.join() !== pyRes.a5.state.join();

// ---- application of areas
const ap = page.locator("[data-figure=euclid-application]").first();
await ap.scrollIntoViewIfNeeded();
await page.waitForTimeout(800);
const apRes = { start: { state: await state(ap), name: await ap.locator("[data-conic-name]").getAttribute("data-conic-name") } };
await ap.screenshot({ path: `${shots}/areas-ellipsis.png` });
const names = {};
for (const k of [0, 0.5, -1]) {
  await typeInto(ap, "k (shape", k);
  names[k] = { state: await state(ap), name: await ap.locator("[data-conic-name]").getAttribute("data-conic-name") };
  await ap.screenshot({ path: `${shots}/areas-k${k}.png` });
}
await typeInto(ap, "S (area)", 16);
apRes.S16 = { state: await state(ap) };
apRes.names = names;
const area = ([a, S, y, k]) => Math.abs((a + k * y) * y - S);
const apOk = apRes.start.name === "ellipse"
  && names[0].name === "parabola" && names[0.5].name === "hyperbola" && names[-1].name === "ellipse"
  && Math.abs(apRes.S16.state[1] - 16) < 1e-9 && Math.abs(apRes.S16.state[2] - 2) < 1e-6
  && [names[0], names[0.5], names[-1], apRes.S16].every((s) => area(s.state) < 1e-6);

// ---- I.1 player and grades
const grades = await page.locator("[data-grade]").count();
const proved = await page.locator("[data-grade=proved]").count();
const wasm = await page.evaluate(() => globalThis.__wasm);
const solves = await page.evaluate(() => globalThis.__euclidSolves || 0);
await page.screenshot({ path: `${shots}/page.png`, fullPage: false });

const ok = errors.length === 0 && remote.length === 0 && wasm.instance + wasm.instantiate > 0
  && pyOk && apOk && grades >= 15 && grades === proved;
console.log(JSON.stringify({ ok, pyOk, apOk, grades, proved, wasm, solves, remote: remote.slice(0, 5),
                             py: pyRes, ap: apRes, errors: errors.slice(0, 8) }));
await browser.close();
process.exit(ok ? 0 : 1);
