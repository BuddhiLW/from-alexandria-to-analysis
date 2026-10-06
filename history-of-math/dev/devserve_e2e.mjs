// Drives the dev server (history-of-math.dev/serve!) in headless Chromium:
//   node dev/devserve_e2e.mjs [base-url] [notebook-path ...]
// For the hub (/) and each notebook path (default /notebooks/apollonius_conics):
// page errors, WebAssembly modules instantiated, Leva panels, canvases, SVGs,
// the links of the hub (timeline) and the prev/next links. With LINKS=1 it
// also opens every notebook link of the hub and checks it loads without page
// errors. Prints one JSON line per page; exit 1 on any page error.
// PLAYWRIGHT names a directory holding node_modules/playwright; CHROMIUM a
// browser binary.
import { createRequire } from "node:module";

const [base = "http://127.0.0.1:7777", ...paths] = process.argv.slice(2);
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");

const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});

async function visit(url, settleMs = 8000) {
  const page = await browser.newPage({ viewport: { width: 1200, height: 900 } });
  const errors = [];
  page.on("pageerror", (e) => errors.push(String(e)));
  await page.addInitScript(() => {
    const W = globalThis.WebAssembly;
    const counts = (globalThis.__wasm = { instantiate: 0, instance: 0 });
    const inst = W.instantiate.bind(W);
    W.instantiate = (...a) => { counts.instantiate++; return inst(...a); };
    W.Instance = new Proxy(W.Instance, {
      construct(t, a, nt) { counts.instance++; return Reflect.construct(t, a, nt); },
    });
  });
  await page.goto(url, { waitUntil: "load", timeout: 180000 });
  await page.waitForTimeout(settleMs);
  const info = await page.evaluate(() => ({
    title: document.title,
    wasm: globalThis.__wasm,
    leva: document.querySelectorAll("[class*=leva]").length,
    canvases: document.querySelectorAll("canvas").length,
    svgs: document.querySelectorAll("svg").length,
    bundle: [...document.querySelectorAll("script[type=module][src]")].map((s) => s.getAttribute("src")),
    links: [...document.querySelectorAll("a[data-ignore-anchor-click]")].map((a) => a.getAttribute("href")),
    h1: document.querySelector("h1")?.textContent,
  }));
  return { page, url, errors, ...info };
}

let failed = false;
const report = (r) => {
  const { page, ...rest } = r;
  if (r.errors.length) failed = true;
  console.log(JSON.stringify(rest));
};

const hub = await visit(base + "/", 5000);
report(hub);
if (process.env.LINKS) {
  const hrefs = [...new Set(hub.links)];
  for (const href of hrefs) {
    const r = await visit(new URL(href, base + "/").href, 4000);
    report({ ...r, links: r.links.length });
    await r.page.close();
  }
}
await hub.page.close();
for (const p of paths.length ? paths : ["/notebooks/apollonius_conics"]) {
  const r = await visit(base + p);
  report(r);
  await r.page.close();
}
await browser.close();
process.exit(failed ? 1 : 0);
