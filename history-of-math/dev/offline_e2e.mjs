// Loads pages of a static build in Chromium and records every request:
//   node dev/offline_e2e.mjs <base-url> [page ...]
// (pages relative to base, default: the hub, conics-playground/,
// apollonius-conics/, euclid-elements/). A page fails when it requests
// anything but a stylesheet or a font from an origin other than base's
// (scripts, fetch/xhr, images), when a request fails or answers >= 400, or
// when it logs a console error or throws. Prints one JSON line; exit 1 on
// any failure. PLAYWRIGHT names a directory holding node_modules/playwright;
// CHROMIUM a browser binary.
import { createRequire } from "node:module";

const [base = "http://127.0.0.1:8099/", ...rest] = process.argv.slice(2);
const pages = rest.length ? rest : ["", "conics-playground/", "apollonius-conics/", "euclid-elements/"];
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
const origin = new URL(base).origin;
const allowedRemote = new Set(["stylesheet", "font"]);

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const results = {};
let ok = true;
for (const p of pages) {
  const url = new URL(p, base).href;
  const page = await browser.newPage({ viewport: { width: 1200, height: 900 } });
  const r = { remote: [], forbidden: [], failed: [], consoleErrors: [], pageErrors: [], local: {} };
  page.on("request", (q) => {
    const u = new URL(q.url());
    if (u.protocol === "data:" || u.protocol === "blob:") return;
    const t = q.resourceType();
    if (u.origin === origin) {
      r.local[t] = (r.local[t] || 0) + 1;
    } else {
      r.remote.push(`${t} ${q.url()}`);
      if (!allowedRemote.has(t)) r.forbidden.push(`${t} ${q.url()}`);
    }
  });
  page.on("requestfailed", (q) => r.failed.push(`${q.url()} ${q.failure()?.errorText}`));
  page.on("response", (s) => { if (s.status() >= 400) r.failed.push(`${s.status()} ${s.url()}`); });
  page.on("console", (m) => { if (m.type() === "error") r.consoleErrors.push(m.text()); });
  page.on("pageerror", (e) => r.pageErrors.push(String(e)));
  await page.goto(url, { waitUntil: "networkidle" });
  await page.waitForTimeout(3000);
  r.katex = await page.locator(".katex").count();
  r.plotly = await page.locator(".plotly .main-svg").count();
  r.icon = await page.evaluate(() => document.querySelector('link[rel="icon"]')?.href || null);
  r.viewerCss = await page.evaluate(() =>
    [...document.querySelectorAll('link[rel="stylesheet"]')].map((l) => l.href).filter((h) => h.includes("viewer.css")));
  r.tailwindApplied = await page.evaluate(() => getComputedStyle(document.querySelector("#clerk > div") || document.body).display);
  const pageOk = !r.forbidden.length && !r.failed.length && !r.consoleErrors.length && !r.pageErrors.length;
  ok = ok && pageOk;
  results[p || "/"] = { ok: pageOk, ...r };
  await page.close();
}
console.log(JSON.stringify({ ok, results }));
await browser.close();
process.exit(ok ? 0 : 1);
