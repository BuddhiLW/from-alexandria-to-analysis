// The site header and favicons that history-of-math.build puts on every page
// (Pedro's compass, protractor and ruler mark), in Clerk's light and dark
// themes:
//   node dev/logo_e2e.mjs <base-url> <shot-dir> [page ...]
// (pages relative to base, default: the apex, one notebook, one series page).
// For each page and theme it checks that the header logo loaded (naturalWidth
// > 0), that the SVG icon is linked and answers 200, that the site name is
// legible (contrast >= 4.5 against the page background) and that the logo is
// dark on a light page and light on a dark one; writes a screenshot per page
// and theme. PLAYWRIGHT names a directory holding node_modules/playwright;
// CHROMIUM a browser binary.
// Prints one JSON line; exit 1 on any failure.
import { createRequire } from "node:module";

const [base = "http://127.0.0.1:8099/", shots = "/tmp", ...rest] = process.argv.slice(2);
const pages = rest.length ? rest : ["", "euclid-elements/", "series/alexandria-to-analysis/"];
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");

const lum = ([r, g, b]) => {
  const f = (c) => { c /= 255; return c <= 0.03928 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4; };
  return 0.2126 * f(r) + 0.7152 * f(g) + 0.0722 * f(b);
};
const contrast = (a, b) => { const [x, y] = [lum(a), lum(b)].sort((p, q) => q - p); return (x + 0.05) / (y + 0.05); };
const rgb = (s) => (s.match(/\d+(\.\d+)?/g) || []).slice(0, 3).map(Number);

const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
let ok = true;
const results = {};
for (const theme of ["light", "dark"]) {
  const ctx = await browser.newContext({ viewport: { width: 1200, height: 800 } });
  if (theme === "dark") await ctx.addInitScript(() => localStorage.setItem("clerk-darkmode", "true"));
  for (const p of pages) {
    const page = await ctx.newPage();
    const errors = [];
    page.on("pageerror", (e) => errors.push(String(e)));
    await page.goto(base + p, { waitUntil: "networkidle" });
    await page.waitForTimeout(1500);
    const r = await page.evaluate(() => {
      const img = document.querySelector(".hom-site-header img.hom-site-logo");
      const name = document.querySelector(".hom-site-header .hom-site-name");
      const icons = [...document.querySelectorAll('link[rel="icon"]')].map((l) => ({ type: l.type, href: l.href }));
      return {
        header: !!document.querySelector("body > header.hom-site-header"),
        logoLoaded: !!img && img.complete && img.naturalWidth > 0,
        logoFilter: img ? getComputedStyle(img).filter : null,
        nameColor: name ? getComputedStyle(name).color : null,
        bg: getComputedStyle(document.body).backgroundColor,
        dark: document.documentElement.classList.contains("dark"),
        icons,
      };
    });
    const svgIcon = r.icons.find((i) => i.type === "image/svg+xml");
    r.svgIconStatus = svgIcon ? (await page.request.get(svgIcon.href)).status() : null;
    let bg = rgb(r.bg);
    if (r.bg === "rgba(0, 0, 0, 0)") bg = [255, 255, 255];
    r.nameContrast = r.nameColor ? +contrast(rgb(r.nameColor), bg).toFixed(2) : 0;
    // the mark is black: on dark pages it must be inverted
    r.logoReadable = r.dark ? /invert\(/.test(r.logoFilter || "") : !/invert\(/.test(r.logoFilter || "");
    r.errors = errors;
    r.pass = r.header && r.logoLoaded && r.svgIconStatus === 200 && r.nameContrast >= 4.5 &&
      r.logoReadable && (theme === "dark") === r.dark && !errors.length;
    const shot = `${shots}/hom-logo-${theme}-${(p || "apex").replace(/\//g, "_")}.png`;
    await page.screenshot({ path: shot, clip: { x: 0, y: 0, width: 1200, height: 360 } });
    r.shot = shot;
    ok = ok && r.pass;
    results[`${theme} /${p}`] = r;
    await page.close();
  }
  await ctx.close();
}
console.log(JSON.stringify({ ok, results }));
await browser.close();
process.exit(ok ? 0 : 1);
