// node dev/lens_e2e.mjs http://127.0.0.1:8099/ target/lens
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";
const [hub = "http://127.0.0.1:8099/", output = "target/lens"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(output, { recursive: true });
const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1400, height: 1000 }, deviceScaleFactor: 1 });
const errors = [];
page.on("pageerror", e => errors.push(String(e)));
await page.goto(hub, { waitUntil: "networkidle" });
await page.locator(".history-timeline .timeline-entry").first().waitFor({ timeout: 30000 });
const arcs = await page.locator(".influence-arc").count();
const before = await page.locator('.timeline-entry[data-notebook="apollonius-conics"]').first().getAttribute("data-active");
await page.locator('button[data-field="mechanics-physics"]').click();
await page.locator('.history-timeline[data-strand="mechanics-physics"]').waitFor({ timeout: 10000 });
const lit = await page.locator('.timeline-entry[data-notebook="kepler-laws"]').first().getAttribute("data-active");
// a strand shows only its own timeline: an entry off the strand is not drawn
const faded = String((await page.locator('.timeline-entry[data-notebook="cauchy"]').count()) > 0);
await page.screenshot({ path: `${output}/hub-physics.png`, fullPage: true });
const visits = [];
for (const id of ["apollonius-conics", "euler"]) {
  await page.goto(new URL(`${id}/`, hub).href, { waitUntil: "networkidle" });
  await page.getByText("Through today's lens").first().waitFor({ timeout: 30000 });
  const html = await page.locator("body").innerText();
  visits.push({ id, lens: html.includes("Through today's lens"), failedBadges: (html.match(/FAILS:/g) || []).length });
  await page.screenshot({ path: `${output}/${id}-lens.png`, fullPage: true });
}
const ok = arcs >= 8 && before === "true" && lit === "true" && faded === "false" &&
  visits.every(v => v.lens && v.failedBadges === 0) && errors.length === 0;
console.log(JSON.stringify({ ok, arcs, before, lit, faded, visits, errors }));
await browser.close();
process.exit(ok ? 0 : 1);
