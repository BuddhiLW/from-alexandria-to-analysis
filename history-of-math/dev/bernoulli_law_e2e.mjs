// Browser check of the Bernoulli notebook (1713):
//   node dev/bernoulli_law_e2e.mjs <url> <out-dir>
// Zero page errors. Law widget: at ratio 3 : 2, scale 10 (r = 30, s = 20,
// t = 50) and odds 1000 : 1 the widget reports Bernoulli's n = 25550 and the
// exact odds at that n above 1000 : 1; the exact probability rises with n and
// the Chebyshev bound lies below it; at n = 3000 the verdict says the band
// holds, at n = 5 it says it does not yet; a seeded sample is drawn and marked.
// Screenshots into <out-dir>. PLAYWRIGHT names a directory holding
// node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:7779/notebooks/probability_bernoulli_1713", out = "target/bernoulli-e2e"] = process.argv.slice(2);
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
const click = async (loc) => { await loc.first().click(); await page.waitForTimeout(400); };

const fig = page.locator('[data-figure="bernoulli-law"]').first();
await fig.scrollIntoViewIfNeeded();
const st = async () => ({
  r: await fig.getAttribute("data-r"), s: await fig.getAttribute("data-s"), t: await fig.getAttribute("data-t"),
  c: await fig.getAttribute("data-c"), n: await fig.getAttribute("data-n"),
  inp: parseFloat(await fig.getAttribute("data-in")), miss: parseFloat(await fig.getAttribute("data-miss")),
  cheb: parseFloat(await fig.getAttribute("data-cheb")),
  bernN: await fig.getAttribute("data-bern-n"), bernIn: parseFloat(await fig.getAttribute("data-bern-in")),
  bernMiss: parseFloat(await fig.getAttribute("data-bern-miss")), certain: await fig.getAttribute("data-certain") });
const slide = async (i) => {
  await fig.locator('[data-act="n-slider"]').evaluate((el, val) => {
    const set = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value").set;
    set.call(el, String(val)); el.dispatchEvent(new Event("input", { bubbles: true })); el.dispatchEvent(new Event("change", { bubbles: true }));
  }, i);
  await page.waitForTimeout(400);
};

await click(fig.locator('[data-act="ratio-0"]'));
await click(fig.locator('[data-act="scale-2"]'));
await click(fig.locator('[data-act="odds-2"]'));
let s = await st();
check("r = 30, s = 20, t = 50, odds 1000", s.r === "30" && s.s === "20" && s.t === "50" && s.c === "1000", s);
check("Bernoulli's n is 25550", s.bernN === "25550", s);
check("exact odds at 25550 exceed 1000 : 1", s.bernIn / s.bernMiss > 1000, s);

await slide(17);
s = await st();
check("n = 3000: the exact probability is high and above Chebyshev", s.n === "3000" && s.inp > 0.9 && s.cheb <= s.inp + 1e-12, s);
await fig.screenshot({ path: `${out}/law-n3000.png` });
const hi = s.inp;
await slide(3);
s = await st();
check("n = 5: the odds are not yet 1000 : 1, and the probability is lower", s.n === "5" && s.certain === "false" && s.inp < hi, s);
check("the verdict sentence is shown", (await fig.locator("[data-verdict]").first().textContent()).length > 10, null);
check("the sample circle is marked", (await fig.locator("[data-sample]").count()) === 1, null);
check("the band is drawn", (await fig.locator("[data-band]").count()) === 1, null);

await click(fig.locator('[data-act="odds-0"]'));
await click(fig.locator('[data-act="scale-0"]'));
await slide(12);
s = await st();
check("t = 10, odds 10 : 1, n = 300: the band holds", s.t === "10" && s.c === "10" && s.n === "300" && s.certain === "true", s);
await fig.screenshot({ path: `${out}/law-t10.png` });

console.log(JSON.stringify({ errors, results }, null, 1));
await browser.close();
process.exit(errors.length === 0 && results.every((r) => r.ok) ? 0 : 1);
