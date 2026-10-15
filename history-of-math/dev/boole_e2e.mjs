// Browser check of the Boole notebook (1854):
//   node dev/boole_e2e.mjs <url> <out-dir>
// Zero page errors. Syllogism widget: Barbara (figure 1, A A) eliminates the
// middle term to "All S are P", the region s(1-p) is empty, and all 8 rows of
// the truth table are ok; Celarent (E A) gives "No S are P"; a particular
// premise (A I) is not eliminated, and the widget still lists Darii; AEE has
// no valid conclusion; the all-moods button shows 15 valid and 9 with
// existential import. Screenshots into <out-dir>. PLAYWRIGHT names a
// directory holding node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:7779/notebooks/logic_boole_1854", out = "target/boole-e2e"] = process.argv.slice(2);
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

const w = page.locator('[data-figure="boole-syllogism"]').first();
await w.scrollIntoViewIfNeeded();
const st = async () => ({
  fig: await w.getAttribute("data-fig"), mood: await w.getAttribute("data-mood"),
  eliminated: await w.getAttribute("data-eliminated"),
  conclusion: await w.locator("[data-conclusion]").first().textContent(),
  valid: await w.locator("[data-valid]").first().textContent(),
  imports: await w.locator("[data-import]").count(),
  empty: await w.locator('[data-empty="true"]').count(),
  rows: await w.locator("[data-row-ok]").count(),
  bad: await w.locator('[data-row-ok="false"]').count(),
  premises: await w.locator("[data-premise]").allTextContents() });

let s = await st();
check("starts at Barbara: figure 1, A A, eliminated", s.fig === "1" && s.mood === "AA" && s.eliminated === "true", s);
check("Barbara: the premises are m(1 - p) = 0 and s(1 - m) = 0", s.premises.join("|") === "m(1 - p) = 0|s(1 - m) = 0", s);
check("Barbara: the middle term gives All S are P", /All S are P/.test(s.conclusion), s);
check("Barbara: exactly one region emptied (s(1-p))", s.empty === 1, s);
check("Barbara: truth table has 8 rows, none failing", s.rows === 8 && s.bad === 0, s);
check("Barbara: Barbari is flagged as needing existential import", s.imports === 1, s);
await w.screenshot({ path: `${out}/barbara.png` });

await click(w.locator('[data-act="f1-E"]'));
s = await st();
check("Celarent (E A): No S are P", s.mood === "EA" && /No S are P/.test(s.conclusion) && s.rows === 8 && s.bad === 0, s);

await click(w.locator('[data-act="f2-I"]'));
await click(w.locator('[data-act="f1-A"]'));
s = await st();
check("Darii (A I): a particular premise is not eliminated", s.mood === "AI" && s.eliminated === "false" && s.rows === 0, s);
check("Darii: Some S are P is valid", /Some S are P/.test(s.valid), s);

await click(w.locator('[data-act="f2-E"]'));
s = await st();
check("A E in figure 1 (AEE): nothing about s and p is eliminated to a universal", s.mood === "AE" && /nothing follows|No S are P|All S are P/.test(s.conclusion), s);

await click(w.locator('[data-act="fig-2"]'));
await click(w.locator('[data-act="f1-A"]'));
await click(w.locator('[data-act="f2-E"]'));
s = await st();
check("Camestres-like pair in figure 2 (A E) concludes No S are P", s.fig === "2" && /No S are P/.test(s.conclusion), s);

await click(w.locator('[data-act="all-moods"]'));
const all = w.locator("[data-all-moods]").first();
check("all moods: 15 valid outright, 9 with existential import",
  (await all.getAttribute("data-valid-count")) === "15" && (await all.getAttribute("data-import-count")) === "9",
  { valid: await all.getAttribute("data-valid-count"), imp: await all.getAttribute("data-import-count") });
await w.screenshot({ path: `${out}/all-moods.png` });

console.log(JSON.stringify({ errors, results }, null, 1));
await browser.close();
process.exit(errors.length === 0 && results.every((r) => r.ok) ? 0 : 1);
