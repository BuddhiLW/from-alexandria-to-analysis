// Browser check of the Cantor 1891 notebook:
//   node dev/set_theory_cantor_1891_e2e.mjs <url> <out-dir>
// Zero page errors. Table widget: starts with the diagonal in no row; the
// diagonal and flip strings follow a click (cell 0-0 flips from m to w and
// the flip changes at place 0 only); "every letter m" gives flip wwwwwwww;
// reset returns to the start. Theorem widget: n = 3 with the default map
// misses D; ticking a box changes D; the run button shows three rows, each
// "missed in every case" (2, 16, 512 maps). Screenshots into <out-dir>.
// PLAYWRIGHT names a directory holding node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:7779/notebooks/set-theory-cantor-1891", out = "target/cantor-1891-e2e"] = process.argv.slice(2);
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

const tab = page.locator('[data-figure="cantor-table"]').first();
await tab.scrollIntoViewIfNeeded();
const ts = async () => ({ diag: await tab.getAttribute("data-diagonal"), flip: await tab.getAttribute("data-flip"),
  inNoRow: await tab.getAttribute("data-in-no-row") });
let s = await ts();
check("table starts with the diagonal in no row", s.inNoRow === "true" && s.diag.length === 8 && s.flip.length === 8, s);
check("flip is the letterwise opposite of the diagonal",
  [...s.diag].every((c, i) => s.flip[i] === (c === "m" ? "w" : "m")), s);
check("8 + 1 rows of cells", await tab.locator("button[data-cell]").count() === 64, null);
const before = s;
await click(tab.locator('[data-cell="0-0"]'));
s = await ts();
check("clicking cell 0-0 flips diagonal place 0 and the flip place 0",
  s.diag[0] !== before.diag[0] && s.flip[0] !== before.flip[0] && s.diag.slice(1) === before.diag.slice(1) && s.inNoRow === "true", { before, s });
await click(tab.locator('[data-cell="3-5"]'));
s = await ts();
check("an off-diagonal click leaves the diagonal alone", s.inNoRow === "true", s);
await click(tab.locator('[data-act="all-m"]'));
s = await ts();
check("every letter m gives the flip wwwwwwww, still in no row", s.diag === "mmmmmmmm" && s.flip === "wwwwwwww" && s.inNoRow === "true", s);
await tab.screenshot({ path: `${out}/table-all-m.png` });
await click(tab.locator('[data-act="reset"]'));
s = await ts();
check("reset returns to the start", s.diag === before.diag && s.inNoRow === "true", s);

const th = page.locator('[data-figure="cantor-theorem"]').first();
await th.scrollIntoViewIfNeeded();
const hs = async () => ({ n: await th.getAttribute("data-n"), d: await th.getAttribute("data-d"), missed: await th.getAttribute("data-missed") });
let h = await hs();
check("theorem widget starts at n = 3, D missed", h.n === "3" && h.missed === "true", h);
const d0 = h.d;
await click(th.locator('[data-cell="1-1"]'));
h = await hs();
check("ticking x = 1 in f(1) changes D, still missed", h.d !== d0 && h.missed === "true", { d0, h });
await click(th.locator('[data-n-btn="2"]'));
h = await hs();
check("n = 2 still missed", h.n === "2" && h.missed === "true", h);
await click(th.locator('[data-n-btn="1"]'));
h = await hs();
check("n = 1 still missed", h.n === "1" && h.missed === "true", h);
await click(th.locator('[data-act="run-all"]'));
const rows = await th.locator("[data-report] tr[data-row]").allTextContents();
check("the run reports three cases, each missed in every case",
  rows.length === 3 && rows.every((r) => r.includes("missed in every case")), rows);
check("map counts 2, 16, 512", ["2", "16", "512"].every((c, i) => rows[i] && rows[i].includes(c)), rows);
await th.screenshot({ path: `${out}/theorem-run.png` });

console.log(JSON.stringify({ errors, results }, null, 1));
await browser.close();
process.exit(errors.length === 0 && results.every((r) => r.ok) ? 0 : 1);
