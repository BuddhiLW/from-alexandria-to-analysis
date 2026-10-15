// Browser check of the Möbius notebook (1865):
//   node dev/moebius_band_e2e.mjs <url> <out-dir>
// Zero page errors. Band widget: at 0 half twists chi = 0, orientable, the
// arrow returns the same way and the cut gives 2 pieces; at 1 half twist chi
// is still 0, not orientable, the character is [1,-1], the arrow returns
// flipped and the cut gives 1 piece; at 2 it is orientable again. Content
// widget: the cube's check holds, the decahedron's and the strip's fail and
// their volume is undefined. Screenshots into <out-dir>. PLAYWRIGHT names a
// directory holding node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:7779/notebooks/topology_moebius_band_1865", out = "target/moebius-e2e"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(out, { recursive: true });

const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1300, height: 1000 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
await page.goto(url, { waitUntil: "networkidle", timeout: 180000 });
await page.waitForTimeout(8000);

const results = [];
const check = (name, cond, got) => results.push({ name, ok: !!cond, got });
const click = async (loc) => { await loc.first().click(); await page.waitForTimeout(500); };

const band = page.locator('[data-figure="moebius-band"]').first();
await band.scrollIntoViewIfNeeded();
const bst = async () => ({
  twists: await band.getAttribute("data-twists"), chi: await band.getAttribute("data-chi"),
  orientable: await band.getAttribute("data-orientable"), flipped: await band.getAttribute("data-flipped"),
  pieces: await band.getAttribute("data-pieces"), cut: await band.getAttribute("data-cut"),
  character: await band.locator("[data-character]").first().textContent(),
  boundary: await band.locator("[data-boundary]").first().textContent() });
const slide = async (v) => {
  await band.locator('[data-act="slider"]').evaluate((el, val) => {
    const set = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value").set;
    set.call(el, String(val)); el.dispatchEvent(new Event("input", { bubbles: true })); el.dispatchEvent(new Event("change", { bubbles: true }));
  }, v);
  await page.waitForTimeout(500);
};

let s = await bst();
check("starts with 0 half twists: chi 0, orientable", s.twists === "0" && s.chi === "0" && s.orientable === "true", s);
await slide(100);
s = await bst();
check("t=0: the arrow returns the same way", s.flipped === "false", s);
await click(band.locator('[data-act="cut"]'));
s = await bst();
check("t=0: the cut gives two bands", s.cut === "true" && s.pieces === "2", s);
await click(band.locator('[data-act="twist-1"]'));
s = await bst();
check("t=1: chi still 0, not orientable, character [1 -1], one boundary",
  s.chi === "0" && s.orientable === "false" && s.character === "[1 -1]" && s.boundary === "1", s);
await slide(100);
s = await bst();
check("t=1: the arrow returns flipped", s.flipped === "true", s);
await band.screenshot({ path: `${out}/band-t1.png` });
await click(band.locator('[data-act="cut"]'));
s = await bst();
check("t=1: the cut gives one band", s.cut === "true" && s.pieces === "1", s);
await band.screenshot({ path: `${out}/band-t1-cut.png` });
await click(band.locator('[data-act="twist-2"]'));
s = await bst();
check("t=2: orientable again, chi 0", s.orientable === "true" && s.chi === "0", s);
await click(band.locator('[data-act="twist-3"]'));
s = await bst();
check("t=3: not orientable", s.orientable === "false", s);

const c = page.locator('[data-figure="moebius-content"]').first();
await c.scrollIntoViewIfNeeded();
const cst = async () => ({ oriented: await c.getAttribute("data-oriented"),
  check: await c.locator("[data-check]").first().textContent(),
  undef: await c.locator("[data-undefined]").count() });
let k = await cst();
check("cube: the law of opposite edges holds, volume defined", k.oriented === "true" && k.check === "holds" && k.undef === 0, k);
const a = (await c.locator("[data-vol-a]").first().textContent()), b = (await c.locator("[data-vol-b]").first().textContent());
check("cube: volume 1.000 from both corners (up to sign)", Math.abs(parseFloat(a)) === 1 && Math.abs(parseFloat(b)) === 1, { a, b });
await click(c.locator('[data-act="case-1"]'));
const fa = await c.locator("[data-vol-a]").first().textContent(), fb = await c.locator("[data-vol-b]").first().textContent();
check("flipped cube: volume depends on the corner", fa !== fb, { fa, fb });
await click(c.locator('[data-act="case-2"]'));
k = await cst();
check("decahedron: the law fails, content undefined", k.oriented === "false" && k.check === "fails" && k.undef === 1, k);
await click(c.locator('[data-act="case-3"]'));
k = await cst();
check("twisted strip: the law fails, content undefined", k.oriented === "false" && k.undef === 1, k);
await c.screenshot({ path: `${out}/content-strip.png` });

console.log(JSON.stringify({ errors, results }, null, 1));
await browser.close();
process.exit(errors.length === 0 && results.every((r) => r.ok) ? 0 : 1);
