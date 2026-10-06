// The letters of the MathBox scenes survive canvas fingerprinting defences.
//
//   node dev/labels_e2e.mjs <notebook-url>
//
// Mullvad Browser and Tor Browser answer a canvas read-back
// (CanvasRenderingContext2D.getImageData, HTMLCanvasElement.toDataURL) with
// noise. MathBox's Format/Label draws glyphs on a 2D canvas and reads them
// back into a texture, so under that defence every letter becomes a hatched
// block. Our scenes letter their points with alexandria.medium.html-labels
// instead: plain DOM text over the canvas.
//
// The page is loaded twice, clean and with the read-back noise injected
// (addInitScript, before any page script). Checks, for each figure
// ([data-figure] apollonius-notation, apollonius-cone, apollonius-3d):
//   - the noise is live in the noisy run (getImageData of a drawn canvas
//     differs between two reads);
//   - the letters are DOM text ([data-html-labels] [data-label]), visible,
//     inside the canvas, at plausible places (the apex above or below the
//     base as the figure stands, B and C apart on one level);
//   - the noisy run puts every letter where the clean run does (<= 2 px);
//   - a screenshot crop around each of a few letters is the same with and
//     without the noise (mean absolute channel difference <= 6 of 255):
//     MathBox text would turn hatched there.
// No page errors. Prints one JSON line; exit 1 on any failure. PLAYWRIGHT
// names a directory holding node_modules/playwright; CHROMIUM a browser.
import { createRequire } from "node:module";
import { writeFileSync } from "node:fs";

const [url = "http://127.0.0.1:8097/labels-scratch/", shots = ""] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});

function noise() {
  // Mullvad / Tor style: every read-back of a 2D canvas returns random data
  const rnd = (n) => { const a = new Uint8ClampedArray(n); for (let i = 0; i < n; i++) a[i] = (Math.random() * 256) | 0; return a; };
  const gid = CanvasRenderingContext2D.prototype.getImageData;
  CanvasRenderingContext2D.prototype.getImageData = function (...a) {
    const img = gid.apply(this, a);
    img.data.set(rnd(img.data.length));
    return img;
  };
  HTMLCanvasElement.prototype.toDataURL = function () {
    return "data:image/png;base64," + btoa(String.fromCharCode(...rnd(64)));
  };
  globalThis.__noise = true;
}

const figures = ["apollonius-notation", "apollonius-cone", "apollonius-3d"];

async function run(noisy) {
  const ctx = await browser.newContext({ viewport: { width: 1200, height: 900 } });
  if (noisy) await ctx.addInitScript(noise);
  const page = await ctx.newPage();
  const errors = [];
  page.on("pageerror", (e) => errors.push(String(e)));
  await page.goto(url, { waitUntil: "networkidle" });
  for (const f of figures) await page.locator(`[data-figure="${f}"] canvas`).first().waitFor({ timeout: 60000 });
  // the notation figure: jump to S8 (every letter drawn), let it finish
  await page.locator('[data-step="8"]').click();
  await page.waitForTimeout(10500);
  const noiseLive = await page.evaluate(() => {
    const c = document.createElement("canvas"); c.width = c.height = 8;
    const g = c.getContext("2d"); g.fillStyle = "#000"; g.fillRect(0, 0, 8, 8);
    const a = g.getImageData(0, 0, 8, 8).data, b = g.getImageData(0, 0, 8, 8).data;
    return a.some((x, i) => x !== b[i]);
  });
  const out = { errors, noiseLive, figures: {}, crops: {} };
  for (const f of figures) {
    const fig = page.locator(`[data-figure="${f}"]`).first();
    await fig.scrollIntoViewIfNeeded();
    await page.waitForTimeout(600);
    out.figures[f] = await fig.evaluate((el) => {
      const cv = el.querySelector("canvas").getBoundingClientRect();
      const labels = [...el.querySelectorAll("[data-html-labels] [data-label]")]
        .filter((d) => getComputedStyle(d).display !== "none" && +getComputedStyle(d).opacity > 0.5)
        .map((d) => { const r = d.getBoundingClientRect();
          return { text: d.textContent, x: r.x + r.width / 2 - cv.x, y: r.y + r.height / 2 - cv.y, w: r.width, h: r.height }; });
      return { canvas: { w: cv.width, h: cv.height }, labels };
    });
    // crops around B, P (or the first letters), page coordinates
    const crops = [];
    const picks = out.figures[f].labels.filter((l) => ["A", "B", "P", "Q"].includes(l.text));
    const cvBox = await fig.locator("canvas").first().boundingBox();
    for (const l of picks) {
      const clip = { x: cvBox.x + l.x - 22, y: cvBox.y + l.y - 22, width: 44, height: 44 };
      crops.push({ text: l.text, png: (await page.screenshot({ clip })).toString("base64") });
    }
    out.crops[f] = crops;
    if (shots) await fig.screenshot({ path: `${shots}/${f}-${noisy ? "noisy" : "clean"}.png` });
  }
  await ctx.close();
  return out;
}

const clean = await run(false);
const noisy = await run(true);

// pixel comparison of the crops, in a clean page (no injected noise)
const cmp = await browser.newPage();
const diffs = {};
for (const f of figures) {
  diffs[f] = [];
  for (const a of clean.crops[f]) {
    const b = noisy.crops[f].find((c) => c.text === a.text);
    if (!b) { diffs[f].push({ text: a.text, diff: null }); continue; }
    const d = await cmp.evaluate(async ([pa, pb]) => {
      const load = async (s) => { const r = await fetch("data:image/png;base64," + s); return createImageBitmap(await r.blob()); };
      const [ia, ib] = await Promise.all([load(pa), load(pb)]);
      const c = new OffscreenCanvas(ia.width, ia.height); const g = c.getContext("2d");
      g.drawImage(ia, 0, 0); const da = g.getImageData(0, 0, ia.width, ia.height).data;
      g.clearRect(0, 0, ia.width, ia.height); g.drawImage(ib, 0, 0); const db = g.getImageData(0, 0, ia.width, ia.height).data;
      let s = 0; for (let i = 0; i < da.length; i++) s += Math.abs(da[i] - db[i]);
      return s / da.length;
    }, [a.png, b.png]);
    diffs[f].push({ text: a.text, diff: +d.toFixed(2) });
  }
}

const at = (fig, t) => fig.labels.find((l) => l.text === t);
const problems = [];
if (clean.errors.length || noisy.errors.length) problems.push("page errors");
if (!noisy.noiseLive) problems.push("noise injection not live");
if (clean.noiseLive) problems.push("clean run is noisy");
const expect = {
  "apollonius-notation": ["A", "B", "C", "P", "M", "V", "L", "x", "p"],
  "apollonius-cone": ["A", "B", "C", "B′", "C′", "P", "M", "V", "L"],
  "apollonius-3d": ["P", "H", "K", "V"],
};
for (const f of figures) {
  for (const run of [clean, noisy]) {
    const fig = run.figures[f];
    for (const t of expect[f]) {
      const l = at(fig, t);
      if (!l) { problems.push(`${f}: ${t} missing`); continue; }
      if (l.x < 0 || l.y < 0 || l.x > fig.canvas.w || l.y > fig.canvas.h) problems.push(`${f}: ${t} outside the canvas`);
    }
  }
  // the noisy run places every letter as the clean run does
  for (const l of clean.figures[f].labels) {
    const m = noisy.figures[f].labels.find((n) => n.text === l.text);
    if (!m || Math.hypot(m.x - l.x, m.y - l.y) > 2) problems.push(`${f}: ${l.text} moved under noise`);
  }
  for (const d of diffs[f]) if (d.diff === null || d.diff > 6) problems.push(`${f}: crop at ${d.text} differs (${d.diff})`);
  if (!diffs[f].length) problems.push(`${f}: no crops`);
}
// plausible places: the notation apex on top (MathBox y up), the cone widget's
// apex below its base (z up, the base at +zb); B and C apart, one level
const n = clean.figures["apollonius-notation"], c = clean.figures["apollonius-cone"], s = clean.figures["apollonius-3d"];
const ok3 = (cond, msg) => { if (!cond) problems.push(msg); };
if (at(n, "A") && at(n, "B") && at(n, "C")) {
  ok3(at(n, "A").y < at(n, "B").y && at(n, "A").y < at(n, "C").y, "notation: A not above BC");
  ok3(Math.abs(at(n, "B").x - at(n, "C").x) > 40, "notation: B and C not apart");
}
if (at(c, "A") && at(c, "B") && at(c, "C") && at(c, "B′")) {
  ok3(at(c, "A").y > at(c, "B").y && at(c, "A").y > at(c, "C").y, "cone: A not below BC");
  ok3(at(c, "B′").y > at(c, "A").y, "cone: B′ not below A");
  ok3(Math.abs(at(c, "B").x - at(c, "C").x) > 40, "cone: B and C not apart");
}
if (at(s, "H") && at(s, "K") && at(s, "V")) {
  const [h, k, v] = ["H", "K", "V"].map((t) => at(s, t));
  ok3(Math.min(h.x, k.x) - 15 < v.x && v.x < Math.max(h.x, k.x) + 15, "3d: V not between H and K");
}

const summary = (r) => Object.fromEntries(figures.map((f) => [f, r.figures[f].labels.map((l) => `${l.text}@${l.x | 0},${l.y | 0}`)]));
const ok = problems.length === 0;
console.log(JSON.stringify({ ok, url, problems, noiseLive: noisy.noiseLive, diffs, clean: summary(clean), noisy: summary(noisy), errors: [...clean.errors, ...noisy.errors] }));
await browser.close();
process.exit(ok ? 0 : 1);
