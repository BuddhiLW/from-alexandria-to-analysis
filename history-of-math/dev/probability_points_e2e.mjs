// Browser check of the problem-of-points notebook (Pascal and Fermat 1654,
// Huygens 1657) on a static build served over http:
//   node dev/probability_points_e2e.mjs <url> <out-dir>
// Zero page errors, no remote <script>, wasm modules instantiated (raster
// kernels). For several games (a, b), set from the shared a/b buttons: the
// stake bar, Fermat's count, Pascal's grid and the triangle give the same
// fraction, and the triangle widget's agree badge passes; 2 against 3 gives
// 11 of 16 and 44 of 64. Pascal's grid steps and a click shows the two
// dependencies. Huygens: the Prop. I preset prices 5; the price tie solves q
// back. Screenshots into <out-dir>. PLAYWRIGHT names a directory holding
// node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:8097/probability-points-1654/", out = "target/probability-points-e2e"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(out, { recursive: true });

const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1300, height: 1000 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
await page.addInitScript(() => {
  window.__wasm = { modules: 0 };
  const M = WebAssembly.Module;
  WebAssembly.Module = function (b) { window.__wasm.modules++; return new M(b); };
  const ci = WebAssembly.instantiate;
  WebAssembly.instantiate = function (...a) { window.__wasm.modules++; return ci.apply(this, a); };
});
await page.goto(url, { waitUntil: "networkidle", timeout: 120000 });
await page.waitForTimeout(8000);

const remote = await page.evaluate(() =>
  [...document.querySelectorAll("script[src]")].map((s) => s.src).filter((s) => !s.startsWith(location.origin)));
const fig = (id) => page.locator(`[data-figure="${id}"]`).first();
const attr = (id, a) => fig(id).getAttribute(a);
const r = { url, remote, games: [], checks: [] };
const check = (name, ok, detail) => r.checks.push({ name, ok: !!ok, detail });

async function setGame(a, b) {
  const el = fig("points-fermat");
  await el.locator(`[data-set-a="${a}"]`).first().click();
  await page.waitForTimeout(300);
  await el.locator(`[data-set-b="${b}"]`).first().click();
  await page.waitForTimeout(1200);
}

for (const [a, b] of [[2, 3], [1, 2], [3, 1], [4, 5], [5, 5], [2, 4]]) {
  await setGame(a, b);
  const g = {
    a, b,
    stakeA: await attr("points-stake", "data-a"), stakeB: await attr("points-stake", "data-b"),
    stake: Number(await attr("points-stake", "data-frac")),
    fermat: Number(await attr("points-fermat", "data-frac")),
    pascal: Number(await attr("points-pascal", "data-frac")),
    tri: Number(await attr("points-triangle", "data-triangle")),
    agree: await attr("points-triangle", "data-agree"),
    fav: await attr("points-fermat", "data-favourable"), total: await attr("points-fermat", "data-total"),
  };
  r.games.push(g);
  const same = [g.fermat, g.pascal, g.tri].every((x) => Math.abs(x - g.stake) < 1e-9);
  check(`(${a},${b}) shared state`, g.stakeA === String(a) && g.stakeB === String(b), g);
  check(`(${a},${b}) three methods and the stake bar agree`, same && g.agree === "yes", g);
}
const g23 = r.games[0];
check("2 against 3: 11 of 16", g23.fav === "11" && g23.total === "16", g23);
check("2 against 3: E = 11/16", Math.abs(g23.stake - 11 / 16) < 1e-12, g23.stake);
await setGame(2, 3);
await fig("points-stake").screenshot({ path: `${out}/stake.png` });
await fig("points-fermat").screenshot({ path: `${out}/fermat.png` });
const share = Number(await attr("points-stake", "data-share"));
check("2 against 3 at 32 each: 44 pistoles", Math.abs(share - 44) < 1e-9, share);

// Pascal's grid: step, then click a cell
const pg = fig("points-pascal");
await pg.scrollIntoViewIfNeeded();
for (let i = 0; i < 3; i++) { await pg.locator('[data-act="next"]').first().click(); await page.waitForTimeout(300); }
check("grid steps", (await pg.getAttribute("data-step")) === "3", await pg.getAttribute("data-step"));
await pg.locator('[data-cell="3,2"]').first().click();
await page.waitForTimeout(500);
check("click selects a cell", (await pg.getAttribute("data-sel")) === "3,2", await pg.getAttribute("data-sel"));
await pg.locator('[data-act="all"]').first().click();
await page.waitForTimeout(600);
const v12 = await pg.locator('[data-cell="1,2"]').first().getAttribute("data-cell-value");
check("E(1,2) = 3/4 (Pascal's 48 of 64)", Math.abs(Number(v12) - 0.75) < 1e-12, v12);
await pg.screenshot({ path: `${out}/pascal.png` });
await fig("points-triangle").screenshot({ path: `${out}/triangle.png` });

// Stepped definition
const st = fig("triangle-steps");
await st.scrollIntoViewIfNeeded();
await st.locator('[data-act="next"]').first().click();
await page.waitForTimeout(300);
check("triangle steps", (await st.getAttribute("data-step")) === "1", await st.getAttribute("data-step"));

// Huygens
const hu = fig("huygens");
await hu.scrollIntoViewIfNeeded();
await hu.locator('[data-preset="prop1"]').first().click();
await page.waitForTimeout(1000);
const x1 = Number(await hu.getAttribute("data-x"));
check("Prop. I: 3 or 7 is worth 5", Math.abs(x1 - 5) < 1e-9, x1);
await hu.locator('[data-preset="points"]').first().click();
await page.waitForTimeout(1000);
const x2 = Number(await hu.getAttribute("data-x"));
check("points as an expectation: 44", Math.abs(x2 - 44) < 1e-9, x2);
await hu.screenshot({ path: `${out}/huygens.png` });

r.wasm = await page.evaluate(() => window.__wasm.modules);
check("wasm in use", r.wasm > 0, r.wasm);
check("no remote scripts", remote.length === 0, remote);
check("zero page errors", errors.length === 0, errors);
r.errors = errors;
const failed = r.checks.filter((c) => !c.ok);
console.log(JSON.stringify(r, null, 1));
console.log(`${r.checks.length - failed.length}/${r.checks.length} checks passed`);
await browser.close();
process.exit(failed.length ? 1 : 0);
