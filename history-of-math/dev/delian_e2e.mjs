// Drives the doubling-of-the-cube notebook of a static build in Chromium:
//   node dev/delian_e2e.mjs <notebook-url> [shots-dir]
// Checks: no page errors and no script loaded from another origin; the
// Menaechmus widget stepped to its last sentence draws Θ, and dragging
// Leva's a slider moves Θ (its readout and its Mafs point); the Archytas
// widget stepped to K renders a MathBox canvas with the HTML letters A, D,
// B, K, I, and dragging on the canvas orbits the camera (the letter K moves
// on screen and the canvas pixels change). Screenshots go to shots-dir.
// Prints one JSON line; exit 1 on any failure. PLAYWRIGHT names a directory
// holding node_modules/playwright; CHROMIUM a browser binary.
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

await page.goto(url, { waitUntil: "networkidle" });

// ---- Menaechmus
const men = page.locator("[data-figure=delian-menaechmus]");
await men.waitFor({ timeout: 60000 });
await men.scrollIntoViewIfNeeded();
await men.locator("li[data-step='5']").click();
const readout = men.locator("[data-theta-x]");
await readout.waitFor({ timeout: 30000 });
await page.waitForTimeout(800);
const thetaPoint = async () =>
  men.locator(".MafsView svg circle").evaluateAll((cs) => cs.map((c) => [c.getAttribute("cx"), c.getAttribute("cy")]));
const x0 = await readout.getAttribute("data-theta-x");
const p0 = await thetaPoint();
await men.screenshot({ path: `${shots}/menaechmus-a-1.4.png` });

const aInput = men.locator("input[id='a'], input[id$='.a']").first();
let dragged = "none";
try {
  const track = aInput.locator("xpath=../preceding-sibling::*[1]");
  const box = await track.boundingBox();
  const v0 = await aInput.inputValue();
  // value = min + (x / width) * (max - min), min 0.6, max 2.2
  const at = (v) => box.x + ((v - 0.6) / 1.6) * box.width;
  await page.mouse.move(at(1.4), box.y + box.height / 2);
  await page.mouse.down();
  for (let v = 1.4; v >= 0.9; v -= 0.05) await page.mouse.move(at(v), box.y + box.height / 2);
  await page.mouse.up();
  dragged = `${v0} -> ${await aInput.inputValue()}`;
} catch (e) {
  dragged = "drag failed: " + e;
}
await page.waitForTimeout(800);
const x1 = await readout.getAttribute("data-theta-x");
const p1 = await thetaPoint();
await men.screenshot({ path: `${shots}/menaechmus-a-dragged.png` });
const aNow = parseFloat(await aInput.inputValue());
const menaechmus = {
  x0, x1, dragged, p0, p1,
  moved: x0 !== x1 && JSON.stringify(p0) !== JSON.stringify(p1),
  cubeRoot: Math.abs(parseFloat(x1) / aNow - Math.cbrt(2)) < 1e-3,
};

// ---- Archytas
const arch = page.locator("[data-figure=delian-archytas]");
await arch.scrollIntoViewIfNeeded();
const steps = [];
for (const s of [1, 2, 3, 4, 5, 6]) {
  await arch.locator(`li[data-step='${s}']`).click();
  await page.waitForTimeout(700);
  steps.push(await arch.getAttribute("data-step"));
  await arch.screenshot({ path: `${shots}/archytas-step-${s}.png` });
}
const canvas = arch.locator("canvas").first();
const canvases = await arch.locator("canvas").count();
const labels = await arch.locator("[data-label]").evaluateAll((ds) =>
  ds.filter((d) => d.style.display !== "none").map((d) => d.textContent));
const kAt = () => arch.locator("[data-label='K']").evaluate((d) => d.style.transform);
const k0 = await kAt();
const shot0 = await canvas.screenshot();
const cb = await canvas.boundingBox();
await page.mouse.move(cb.x + cb.width * 0.5, cb.y + cb.height * 0.5);
await page.mouse.down();
for (let i = 1; i <= 12; i++) await page.mouse.move(cb.x + cb.width * (0.5 + i * 0.02), cb.y + cb.height * (0.5 + i * 0.005));
await page.mouse.up();
await page.waitForTimeout(800);
const k1 = await kAt();
const shot1 = await canvas.screenshot();
await arch.screenshot({ path: `${shots}/archytas-orbited.png` });
const ai = await arch.locator("[data-ai]").getAttribute("data-ai");
const archytas = {
  steps, canvases, labels, k0, k1, ai,
  orbited: k0 !== k1 && !shot0.equals(shot1),
  aiCube: Math.abs(parseFloat(ai) ** 3 - 2) < 1e-6,
};

await page.screenshot({ path: `${shots}/page.png`, fullPage: true });
const ok = errors.length === 0 && remote.length === 0 && menaechmus.moved && menaechmus.cubeRoot &&
  canvases > 0 && archytas.orbited && archytas.aiCube && ["A", "D", "B", "K", "I"].every((l) => labels.includes(l));
console.log(JSON.stringify({ ok, url, errors, remote, menaechmus, archytas, shots }));
await browser.close();
process.exit(ok ? 0 : 1);
