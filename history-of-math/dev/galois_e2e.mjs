// Browser check of the Galois notebook (1831, printed 1846):
//   node dev/galois_e2e.mjs <url> <out-dir>
// Zero page errors. Roots widget: starts at the identity, in the group;
// sigma gives (0 1 2 3), in the group; sigma then tau stays in the group;
// the transposition (0 1) from the list is not in the group; reset returns
// to the identity. The chain table and the grade badges are present.
// Screenshots into <out-dir>. PLAYWRIGHT names a directory holding
// node_modules/playwright.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url = "http://127.0.0.1:7779/notebooks/galois", out = "target/galois-e2e"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(out, { recursive: true });

const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1300, height: 1000 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
await page.goto(url, { waitUntil: "networkidle", timeout: 180000 });
await page.waitForTimeout(8000);

const fig = page.locator('[data-figure="galois-roots"]').first();
const st = async () => ({ perm: await fig.getAttribute("data-perm"), inGroup: await fig.getAttribute("data-in-group"),
  cycles: await fig.locator("[data-cycles]").first().textContent() });
const click = async (loc) => { await loc.first().click(); await page.waitForTimeout(600); };
const results = [];
const check = (name, cond, got) => results.push({ name, ok: !!cond, got });

await fig.scrollIntoViewIfNeeded();
let s = await st();
check("identity at start, in the group", s.perm === "[0 1 2 3]" && s.inGroup === "true", s);
await click(fig.locator('[data-act="sigma"]'));
s = await st();
check("sigma is the 4-cycle (0 1 2 3), in the group", s.cycles === "(0 1 2 3)" && s.inGroup === "true", s);
await fig.screenshot({ path: `${out}/roots-sigma.png` });
await click(fig.locator('[data-act="tau"]'));
s = await st();
check("sigma then tau stays in the group", s.inGroup === "true", s);
await click(fig.locator('[data-perm="[1 0 2 3]"]'));
s = await st();
check("the transposition (0 1) is not in the group", s.inGroup === "false" && s.cycles === "(0 1)", s);
await fig.screenshot({ path: `${out}/roots-out.png` });
await click(fig.locator('[data-act="reset"]'));
s = await st();
check("reset returns to the identity", s.perm === "[0 1 2 3]", s);
check("chain table present", await page.locator('[data-figure="galois-chains"]').count() === 1, null);

console.log(JSON.stringify({ errors, results }, null, 1));
await browser.close();
process.exit(errors.length === 0 && results.every((r) => r.ok) ? 0 : 1);
