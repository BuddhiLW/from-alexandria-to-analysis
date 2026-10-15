// node dev/strands_e2e.mjs http://127.0.0.1:8099/all/ target/strands
// The page of every work (<out>/all/, never the apex), its strand selector: picking a strand shows only its timeline (every
// entry on it, none off it), entries on several strands carry chips for the
// others, bridges between strands are drawn, every one of the fourteen
// strands has entries, the hub reads at phone width (390 px) without
// horizontal scroll, and the page logs no errors.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";
const [hub = "http://127.0.0.1:8099/", output = "target/strands"] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(output, { recursive: true });
const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1400, height: 1000 }, deviceScaleFactor: 1 });
const errors = [];
page.on("pageerror", e => errors.push(String(e)));
await page.goto(hub, { waitUntil: "networkidle" });
await page.locator(".history-timeline .timeline-entry").first().waitFor({ timeout: 30000 });
const entries = () => page.locator(".history-timeline .timeline-entry");
const fieldsOf = () => entries().evaluateAll(es => es.map(e => e.getAttribute("data-fields").split(" ")));
const union = await entries().count();
const strands = await page.locator(".strand-map .strand-node").count();
const bridges = await page.locator(".strand-map .strand-bridge").count();
// every strand button names a non-empty timeline
const counts = await page.locator("button[data-field]:not([data-field=all])").evaluateAll(bs =>
  bs.map(b => ({ s: b.getAttribute("data-field"), n: +((b.textContent.match(/\((\d+)\)$/) || [0, 0])[1]) })));
await page.screenshot({ path: `${output}/hub-all.png`, fullPage: true });
const results = [];
for (const s of ["trigonometry", "probability", "geometry", "differential-geometry", "measure-theory",
                 "abstract-algebra", "graph-theory", "set-theory", "topology", "logic-computing"]) {
  await page.locator(`button[data-field="${s}"]`).click();
  await page.locator(`.history-timeline[data-strand="${s}"]`).waitFor({ timeout: 10000 });
  const fs = await fieldsOf();
  const chips = await page.locator(".history-timeline .strand-chip").count();
  const selfChips = await page.locator(`.history-timeline .strand-chip[data-strand="${s}"]`).count();
  const scope = await page.locator(".strand-scope").innerText();
  results.push({ s, count: fs.length, allOn: fs.every(f => f.includes(s)), chips, selfChips, scope: scope.length > 20 });
  await page.screenshot({ path: `${output}/hub-${s}.png`, fullPage: true });
}
// clicking a strand on the bridge map selects it too; "All" restores the union
await page.locator('.strand-map .strand-node[data-strand="algebra"] circle').click();
const viaMap = await page.locator('.history-timeline[data-strand="algebra"]')
  .waitFor({ timeout: 5000 }).then(() => 1, () => 0);
await page.locator('button[data-field="all"]').click();
await page.locator('.history-timeline[data-strand="all"]').waitFor({ timeout: 5000 });
const back = await entries().count();
// phone width: the selector wraps, the strand map scales, nothing scrolls sideways
await page.setViewportSize({ width: 390, height: 844 });
await page.waitForTimeout(500);
const phone = await page.evaluate(() => {
  const doc = document.documentElement;
  const sel = document.querySelector(".timeline-fields").getBoundingClientRect();
  const map = document.querySelector(".strand-map").getBoundingClientRect();
  const labels = [...document.querySelectorAll(".strand-map .strand-node text")].map(t => t.getBoundingClientRect());
  return { scrollWidth: doc.scrollWidth, clientWidth: doc.clientWidth,
           selRight: sel.right, selHeight: sel.height, mapWidth: map.width, mapRight: map.right,
           labelsIn: labels.every(r => r.left >= map.left - 1 && r.right <= map.right + 1),
           minLabelHeight: Math.min(...labels.map(r => r.height)) };
});
await page.locator(".timeline-fields").scrollIntoViewIfNeeded();
await page.screenshot({ path: `${output}/hub-phone.png`, fullPage: false });
await page.locator(".strand-map").screenshot({ path: `${output}/strand-map-phone.png` });
const phoneOk = phone.scrollWidth <= phone.clientWidth && phone.selRight <= 390 && phone.mapRight <= 390 &&
  phone.labelsIn && phone.minLabelHeight >= 8;
const ok = union >= 120 && strands === 14 && counts.length === 14 && counts.every(c => c.n > 0) &&
  bridges >= 3 && viaMap === 1 && back === union && phoneOk &&
  results.every(r => r.count >= 3 && r.count < union && r.allOn && r.chips > 0 && r.selfChips === 0 && r.scope) &&
  errors.length === 0;
console.log(JSON.stringify({ ok, union, strands, bridges, counts, results, phone, viaMap, back, errors }));
await browser.close();
process.exit(ok ? 0 : 1);
