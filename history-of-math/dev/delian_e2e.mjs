// Drives the doubling-of-the-cube notebook of a static build in Chromium:
//   node dev/delian_e2e.mjs <notebook-url> [shots-dir]
// Checks: no page errors and no script loaded from another origin; every
// widget's kernels are WebAssembly. Tied controls round-trip:
//   Menaechmus {a x V}: dragging the x slider re-solves a = x/∛2 and
//     V = x³ in the panel and moves Θ; typing V re-solves a and x; the
//     live formula follows; V/a³ stays 2.
//   Archytas {AB AI AK}: dragging AI re-solves AB and AK, K moves in the
//     MathBox scene (HTML letter and canvas pixels), AI³ = AD·AB²;
//     dragging on the canvas orbits it.
//   Mesolabe {s DΘ BZ GH}: typing DΘ slides the frames to s = ∛(DΘ/2).
// Screenshots go to shots-dir. Prints one JSON line; exit 1 on failure.
// PLAYWRIGHT names a directory holding node_modules/playwright; CHROMIUM a
// browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:8099/doubling-the-cube/", shots = "target/delian-shots"] = process.argv.slice(2);
mkdirSync(shots, { recursive: true });
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1280, height: 1000 } });
const errors = [];
const remote = [];
const origin = new URL(url).origin;
page.on("pageerror", (e) => errors.push(String(e)));
page.on("request", (r) => {
  if (r.resourceType() === "script" && !r.url().startsWith(origin) && !r.url().startsWith("data:")) remote.push(r.url());
});
await page.addInitScript(() => {
  const inst = WebAssembly.instantiate;
  globalThis.__wasm = 0;
  WebAssembly.instantiate = function (...a) { globalThis.__wasm++; return inst.apply(this, a); };
  const Inst = WebAssembly.Instance;
  WebAssembly.Instance = function (...a) { globalThis.__wasm++; return new Inst(...a); };
});

await page.goto(url, { waitUntil: "networkidle" });

const input = (fig, k) => fig.locator(`input[id='${k}'], input[id$='.${k}']`).first();
const val = async (fig, k) => parseFloat(await input(fig, k).inputValue());
const settle = () => page.waitForTimeout(900);

async function drag(fig, k, min, max, from, to) {
  const track = input(fig, k).locator("xpath=../preceding-sibling::*[1]");
  const box = await track.boundingBox();
  const at = (v) => box.x + ((v - min) / (max - min)) * box.width;
  const y = box.y + box.height / 2;
  await page.mouse.move(at(from), y);
  await page.mouse.down();
  const n = 10;
  for (let i = 1; i <= n; i++) await page.mouse.move(at(from + ((to - from) * i) / n), y);
  await page.mouse.up();
  await settle();
}

async function type(fig, k, v) {
  const i = input(fig, k);
  await i.click({ clickCount: 3 });
  await i.fill(String(v));
  await i.press("Enter");
  await settle();
}

const close = (x, y, tol) => Math.abs(x - y) < tol;
const c2 = Math.cbrt(2);

// ---- Menaechmus {a x V}
const men = page.locator("[data-figure=delian-menaechmus-tied]");
await men.waitFor({ timeout: 60000 });
await men.scrollIntoViewIfNeeded();
await men.locator("li[data-step='5']").click();
await settle();
const thetaPoint = () =>
  men.locator(".MafsView svg circle").evaluateAll((cs) => cs.map((c) => [c.getAttribute("cx"), c.getAttribute("cy")]));
const mread = async () => ({
  a: await val(men, "a"), x: await val(men, "x"), V: await val(men, "V"),
  dx: parseFloat(await men.locator("[data-menaechmus-x]").getAttribute("data-menaechmus-x")),
  da: parseFloat(await men.locator("[data-menaechmus-x]").getAttribute("data-menaechmus-a")),
  ratio: parseFloat(await men.locator("[data-vol-ratio]").getAttribute("data-vol-ratio")),
  formula: await men.locator("[data-formula]").first().getAttribute("data-formula"),
});
const m0 = await mread();
const p0 = await thetaPoint();
await men.screenshot({ path: `${shots}/menaechmus-start.png` });
const xr = [0.6 * c2, 2.2 * c2];
await drag(men, "x", xr[0], xr[1], m0.x, 1.2);
const m1 = await mread();
const p1 = await thetaPoint();
await men.screenshot({ path: `${shots}/menaechmus-x-dragged.png` });
await type(men, "V", 10);
const m2 = await mread();
await men.screenshot({ path: `${shots}/menaechmus-V-typed.png` });
const menaechmus = {
  m0, m1, m2,
  xDragged: !close(m1.x, m0.x, 0.05),
  aFollowsX: close(m1.a, m1.x / c2, 0.011) && close(m1.da, m1.dx / c2, 1e-9),
  vFollowsX: close(m1.V, m1.x ** 3, 0.02),
  thetaMoved: JSON.stringify(p0) !== JSON.stringify(p1),
  formulaLive: m0.formula !== m1.formula,
  fromV: close(m2.da, Math.cbrt(5), 1e-6) && close(m2.dx, Math.cbrt(10), 1e-6) && close(m2.a, Math.cbrt(5), 0.011),
  ratioTwo: [m0, m1, m2].every((m) => close(m.ratio, 2, 1e-9)),
};
menaechmus.ok = Object.entries(menaechmus).filter(([k]) => !/^m\d$/.test(k)).every(([, v]) => v === true);

// ---- Archytas {AB AI AK}
const arch = page.locator("[data-figure=delian-archytas-tied]");
await arch.scrollIntoViewIfNeeded();
const steps = [];
for (const s of [1, 2, 3, 4, 5, 6]) {
  await arch.locator(`li[data-step='${s}']`).click();
  await page.waitForTimeout(600);
  steps.push(await arch.getAttribute("data-step"));
  await arch.screenshot({ path: `${shots}/archytas-step-${s}.png` });
}
const canvas = arch.locator("canvas").first();
const canvases = await arch.locator("canvas").count();
const labels = await arch.locator("[data-label]").evaluateAll((ds) =>
  ds.filter((d) => d.style.display !== "none").map((d) => d.textContent));
const kAt = () => arch.locator("[data-label='K']").evaluate((d) => d.style.transform);
const aread = async () => ({ ab: await val(arch, "b"), ai: await val(arch, "ai"), ak: await val(arch, "ak"),
  dai: parseFloat(await arch.locator("[data-ai]").getAttribute("data-ai")) });
const a0 = await aread();
const k0 = await kAt();
const shot0 = await canvas.screenshot();
const air = [Math.cbrt(2 * 0.49), Math.cbrt(2 * 2.56)];
await drag(arch, "ai", air[0], air[1], a0.ai, 1.45);
const a1 = await aread();
const k1 = await kAt();
const shot1 = await canvas.screenshot();
await arch.screenshot({ path: `${shots}/archytas-AI-dragged.png` });
const cb = await canvas.boundingBox();
await page.mouse.move(cb.x + cb.width * 0.5, cb.y + cb.height * 0.5);
await page.mouse.down();
for (let i = 1; i <= 12; i++) await page.mouse.move(cb.x + cb.width * (0.5 + i * 0.02), cb.y + cb.height * (0.5 + i * 0.005));
await page.mouse.up();
await settle();
const k2 = await kAt();
const shot2 = await canvas.screenshot();
await arch.screenshot({ path: `${shots}/archytas-orbited.png` });
const archytas = {
  steps, canvases, labels, a0, a1,
  aiDragged: !close(a1.ai, a0.ai, 0.05),
  abFollows: close(a1.ab, Math.sqrt(a1.ai ** 3 / 2), 0.011),
  akFollows: close(a1.ak, Math.sqrt(2 * a1.ai), 0.002),
  kMoved: k0 !== k1 && !shot0.equals(shot1),
  cubeLaw: close(a1.dai ** 3, 2 * a1.ab ** 2, 0.05),
  orbited: k1 !== k2 && !shot1.equals(shot2),
  letters: ["A", "D", "B", "K", "I"].every((l) => labels.includes(l)),
};
archytas.ok = canvases > 0 && ["aiDragged", "abFollows", "akFollows", "kMoved", "cubeLaw", "orbited", "letters"].every((k) => archytas[k]);

// ---- Mesolabe {s DΘ BZ GH}
const meso = page.locator("[data-figure=delian-mesolabe-tied]");
await meso.scrollIntoViewIfNeeded();
await meso.locator("li[data-step='4']").click();
await settle();
const sread = async () => ({
  s: parseFloat(await meso.locator("[data-mesolabe-s]").getAttribute("data-mesolabe-s")),
  b: parseFloat(await meso.locator("[data-mesolabe-s]").getAttribute("data-mesolabe-b")),
  frames: await meso.locator(".MafsView svg polygon").evaluateAll((ps) => ps.map((p) => p.getAttribute("points"))),
});
const s0 = await sread();
await meso.screenshot({ path: `${shots}/mesolabe-start.png` });
await type(meso, "b", 1);
const s1 = await sread();
await meso.screenshot({ path: `${shots}/mesolabe-b-typed.png` });
await drag(meso, "s", 0.5, 1.0, s1.s, 0.95);
const s2 = await sread();
await meso.screenshot({ path: `${shots}/mesolabe-s-dragged.png` });
const mesolabe = {
  s0: s0.s, s1: s1.s, s2: s2.s, b2: s2.b,
  fromB: close(s1.s, Math.cbrt(0.5), 1e-6),
  framesSlid: JSON.stringify(s0.frames) !== JSON.stringify(s1.frames),
  bFollowsS: close(s2.b, 2 * s2.s ** 3, 1e-9) && !close(s2.s, s1.s, 0.05),
};
mesolabe.ok = mesolabe.fromB && mesolabe.framesSlid && mesolabe.bFollowsS;

const wasm = await page.evaluate(() => globalThis.__wasm);
const solves = await page.evaluate(() => globalThis.__delianSolves);
await page.screenshot({ path: `${shots}/page.png`, fullPage: true });
const ok = errors.length === 0 && remote.length === 0 && wasm > 0 && menaechmus.ok && archytas.ok && mesolabe.ok;
console.log(JSON.stringify({ ok, url, errors, remote, wasm, solves, menaechmus, archytas, mesolabe, shots }));
await browser.close();
process.exit(ok ? 0 : 1);
