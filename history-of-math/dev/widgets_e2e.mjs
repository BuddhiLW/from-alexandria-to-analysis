// Drives the conics playground of a static build in Chromium:
//   node dev/widgets_e2e.mjs <notebook-url>
// Checks: no page errors, the Leva panel, an SVG path for the Mafs curve, a
// canvas for MathBox, and that dragging Leva's e slider from 0.5 towards 1.5
// turns the live label from "ellipse" into "hyperbola". Counts the
// WebAssembly modules the page instantiates (WebAssembly.instantiate and
// new WebAssembly.Instance, the two paths emmy-viewers' raster glue uses).
// Prints one JSON line; exit 1 on any failure. PLAYWRIGHT names a directory
// holding node_modules/playwright; CHROMIUM a browser binary.
import { createRequire } from "node:module";

const [url = "http://127.0.0.1:8099/conics-playground/"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1200, height: 900 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
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
const label = page.locator("[data-conic-name]").first();
await label.waitFor({ timeout: 30000 });
await page.waitForTimeout(1500);

const before = (await label.innerText()).trim();
const levaRoot = page.locator("[class*=leva]").first();
const leva = await levaRoot.count();
const mafsPaths = await page.locator(".MafsView svg path").count();
const canvases = await page.locator("canvas").count();

// Leva's number row: the drag track sits beside the text input of key e.
const ids = await page.locator("input[id]").evaluateAll((xs) => xs.map((x) => x.id));
const eInput = page.locator(`input[id="${ids.find((i) => i === "e" || i.endsWith(".e"))}"]`);
let dragged = "none";
try {
  // the range track is the sibling before the input's wrapper (Leva 0.9)
  const track = eInput.locator("xpath=../preceding-sibling::*[1]");
  const box = await track.boundingBox();
  const v0 = await eInput.inputValue();
  // value = min + (x / width) * (max - min), min 0, max 2
  const at = (v) => box.x + (v / 2) * box.width;
  await page.mouse.move(at(0.5), box.y + box.height / 2);
  await page.mouse.down();
  for (let v = 0.5; v <= 1.5; v += 0.1) await page.mouse.move(at(v), box.y + box.height / 2);
  await page.mouse.up();
  dragged = `${v0} -> ${await eInput.inputValue()}`;
} catch (e) {
  dragged = "drag failed: " + e;
}
await page.waitForTimeout(800);
const after = (await label.innerText()).trim();
const pathD = await page.locator(".MafsView svg path").evaluateAll((ps) => ps.map((p) => (p.getAttribute("d") || "").length));
const wasm = await page.evaluate(() => globalThis.__wasm);

const result = { url, errors, leva, mafsPaths, pathD, canvases, before, after, dragged, wasm, ids };
const ok = errors.length === 0 && leva > 0 && mafsPaths > 0 && canvases > 0 &&
  before === "ellipse" && after === "hyperbola" && wasm.instantiate + wasm.instance > 0;
console.log(JSON.stringify({ ok, ...result }));
await browser.close();
process.exit(ok ? 0 : 1);
