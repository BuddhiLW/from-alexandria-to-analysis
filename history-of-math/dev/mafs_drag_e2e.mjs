// Mouse-drags every Mafs MovablePoint of a served notebook page in Chromium:
//   node mafs_drag_e2e.mjs <notebook-url> <shots-dir> [min-handles]
// For each handle (.mafs-movable-point) of each figure: a real mouse drag
// (down on the handle's centre, ten moves, up; the opposite direction when a
// constraint holds the point still), then three checks: the point moved
// (its centre followed the pointer), the view did not pan (the MafsView's
// --mafs-view-transform is unchanged) and the figure's readout (the text of
// its [data-playground] / [data-figure] root) changed. A label drawn over a
// handle used to take the mousedown and pan the view: this is the gate.
// Every page: 0 page errors, no remote scripts. Prints one JSON line; exit 1
// on failure. PLAYWRIGHT names a directory holding node_modules/playwright;
// CHROMIUM a browser binary.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";

const [url, shots = "drag-shots", minArg = "1"] = process.argv.slice(2);
if (!url || !url.startsWith("http")) throw new Error("pass a served notebook URL (http)");
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(shots, { recursive: true });
const browser = await chromium.launch({
  args: ["--use-gl=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1300, height: 950 } });
const errors = [], remote = [];
const origin = new URL(url).origin;
page.on("pageerror", (e) => errors.push(String(e).slice(0, 300)));
page.on("request", (r) => {
  if (r.resourceType() === "script" && !r.url().startsWith(origin) && !r.url().startsWith("data:")) remote.push(r.url());
});
await page.goto(url, { waitUntil: "networkidle", timeout: 180000 });
await page.waitForTimeout(3000);

const wait = (ms = 500) => page.waitForTimeout(ms);
const views = page.locator(".MafsView:has(.mafs-movable-point)");
const nViews = await views.count();
const results = [];
for (let v = 0; v < nViews; v++) {
  const view = views.nth(v);
  await view.scrollIntoViewIfNeeded(); await wait(800);
  const root = view.locator("xpath=ancestor::*[@data-playground or @data-figure][1]");
  const name = (await root.count())
    ? (await root.getAttribute("data-playground")) || (await root.getAttribute("data-figure")) : `view-${v}`;
  // the readout: the root's text, its data-* attributes and its inputs'
  // values (a Leva field is the only readout of some parameters)
  const readout = async () => ((await root.count()) ? root : view.locator("xpath=..")).evaluate((el) =>
    el.textContent + [...el.attributes].map((a) => a.name + "=" + a.value).join(" ")
      + [...el.querySelectorAll("input")].map((i) => i.value).join(","));
  const transform = () => view.evaluate((s) =>
    s.style.getPropertyValue("--mafs-view-transform") + " | " + (s.querySelector("svg") || s).getAttribute("viewBox"));
  // the figure: the view's SVG without the handles themselves. A layout
  // handle (a region of Koenigsberg, a vertex of a drawing) changes no
  // number, by design, but redraws what hangs on it; a handle on an atom
  // nothing reads moves only its own dot.
  const figure = () => view.evaluate((s) => {
    const c = s.cloneNode(true);
    c.querySelectorAll(".mafs-movable-point").forEach((e) => e.remove());
    return c.innerHTML;
  });
  const handles = view.locator(".mafs-movable-point");
  const n = await handles.count();
  for (let i = 0; i < n; i++) {
    const h = handles.nth(i);
    const centre = async () => { const b = await h.boundingBox(); return [b.x + b.width / 2, b.y + b.height / 2]; };
    const r = { figure: name, handle: i };
    let moved = 0, pan = false, changed = false, redrawn = false;
    for (const [dx, dy] of [[40, -25], [-40, 25], [0, -40]]) {
      const t0 = await transform(), c0 = await centre(), s0 = await readout(), f0 = await figure();
      await page.mouse.move(c0[0], c0[1]); await page.mouse.down();
      // paced moves: a handle whose on-move searches (a crossing time)
      // drops pointer moves that arrive faster than it answers
      for (let k = 1; k <= 10; k++) { await page.mouse.move(c0[0] + dx * k / 10, c0[1] + dy * k / 10); await wait(30); }
      await page.mouse.up();
      await wait(700);
      const c1 = await centre();
      pan = pan || (await transform()) !== t0;
      moved = Math.hypot(c1[0] - c0[0], c1[1] - c0[1]);
      changed = (await readout()) !== s0;
      redrawn = (await figure()) !== f0;
      (r.tries ||= []).push([dx, dy, Math.round(moved), changed, redrawn]);
      if (moved > 3 && changed) break;
    }
    // a readout that changed passes; a figure that followed the handle
    // passes as layout (reported as such, so a reader can tell them apart)
    const layout = !changed && redrawn;
    // a view whose view-box is computed from the state (Oresme's mean and
    // series) rescales when the handle changes the state: its transform
    // moves together with a changed readout. A pan (the press missed the
    // handle) changes no readout.
    const rescaled = pan && changed;
    Object.assign(r, { moved: Math.round(moved), pan, rescaled, changed, layout,
                       ok: moved > 3 && (!pan || rescaled) && (changed || redrawn) });
    results.push(r);
  }
  await view.screenshot({ path: `${shots}/${String(name).replace(/\W+/g, "-")}-${v}.png` });
}
const bad = results.filter((r) => !r.ok);
const ok = results.length >= Number(minArg) && bad.length === 0 && errors.length === 0 && remote.length === 0;
console.log(JSON.stringify({ url, ok, views: nViews, handles: results.length, layout: results.filter((r) => r.ok && r.layout).length, bad, errors: errors.slice(0, 5), remote: remote.slice(0, 3) }));
await browser.close();
process.exit(ok ? 0 : 1);
