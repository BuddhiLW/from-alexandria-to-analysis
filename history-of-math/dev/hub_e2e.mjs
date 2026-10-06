// Drives the built series in Chromium:
//   node dev/hub_e2e.mjs <hub-url>
// From the hub, opens each notebook named in NOTEBOOKS (default archimedes,
// kepler-laws) by clicking its link in the timeline table, checks the page
// renders (its title as an h1, no Clerk error text, no page errors), then
// follows the notebook's "next" link once. Prints one JSON line; exit 1 on
// any failure. PLAYWRIGHT names a directory holding node_modules/playwright.
import { createRequire } from "node:module";

const [hub = "http://127.0.0.1:8099/"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
const notebooks = (process.env.NOTEBOOKS || "archimedes,kepler-laws").split(",");

const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1200, height: 900 } });
const errors = [];
page.on("pageerror", (e) => errors.push(`${page.url()}: ${e}`));

const clerkError = /b is null|Fetch failed|Unhandled error|Cannot read properties/;
async function check(label) {
  await page.waitForLoadState("networkidle");
  await page.locator("h1").first().waitFor({ timeout: 20000 });
  const text = await page.locator("body").innerText();
  return { label, url: page.url(), h1: (await page.locator("h1").first().innerText()).trim(),
           clerkError: clerkError.test(text), hasNav: await page.locator("a[rel=next], a[rel=prev]").count() };
}

const visits = [];
for (const id of notebooks) {
  await page.goto(hub, { waitUntil: "networkidle" });
  await page.locator(`table a[href="${id}/"]`).first().click();
  await page.waitForURL(new RegExp(`/${id}/$`));
  visits.push(await check(id));
}
await page.locator("a[rel=next]").first().click();
await page.waitForLoadState("load");
visits.push(await check("next"));

const ok = errors.length === 0 && visits.every((v) => !v.clerkError && v.h1.length > 0) &&
           !visits.some((v) => /index\.html\/$/.test(v.url));
console.log(JSON.stringify({ ok, visits, errors }));
await browser.close();
process.exit(ok ? 0 : 1);
