// Browser gate for the four Cauchy part-A playgrounds (alexandria.cauchy.playground-limits):
// the limit (eps band, N(eps)), continuity ch. II (the increment), the
// difference quotient (secant onto tangent), the root between two signs
// (brackets). Run on a served static build, never file://:
//   node dev/cauchy_limits_playgrounds_e2e.mjs http://localhost:8093/cauchy.html target/cauchy-limits-e2e
// PLAYWRIGHT names a directory holding node_modules/playwright; CHROMIUM a browser binary.
// For each playground: drags every MovablePoint on its figure and checks the
// raster readout (data-value) moves; presses every Try-this preset, and the
// "Break it" preset must turn the verdict red (data-holds=false) with a new
// case name; Reset restores the starting case. Then: 0 page errors, no
// remote scripts, WebAssembly instantiated. Prints one JSON line; exit 1 on failure.
import { createRequire } from 'node:module';
import { mkdirSync } from 'node:fs';
import assert from 'node:assert/strict';

const [url, out = 'target/cauchy-limits-e2e'] = process.argv.slice(2);
if (!url || !url.startsWith('http')) throw new Error('pass a served notebook URL (http), not file://');
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + '/');
const { chromium } = require('playwright');
mkdirSync(out, { recursive: true });

const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1400, height: 1000 } });
const errors = [];
const remote = [];
const origin = new URL(url).origin;
page.on('pageerror', e => errors.push(String(e)));
page.on('request', r => {
  if (r.resourceType() === 'script' && !r.url().startsWith(origin) && !r.url().startsWith('data:')) remote.push(r.url());
});
await page.addInitScript(() => {
  const W = globalThis.WebAssembly;
  globalThis.__wasm = 0;
  const inst = W.instantiate.bind(W);
  W.instantiate = (...a) => { globalThis.__wasm++; return inst(...a); };
  W.Instance = new Proxy(W.Instance, { construct(t, a, nt) { globalThis.__wasm++; return Reflect.construct(t, a, nt); } });
});

const ids = ['limit', 'continuity-ch2', 'ivt', 'derivative'];
const state = async fig => ({
  value: await fig.getAttribute('data-value'),
  name: await fig.getAttribute('data-case'),
  holds: await fig.getAttribute('data-holds'),
});
const results = {};
try {
  await page.goto(url, { waitUntil: 'networkidle', timeout: 120000 });
  for (const id of ids) {
    const fig = page.locator(`[data-playground="${id}"]`).first();
    await fig.waitFor({ timeout: 120000 });
    await fig.scrollIntoViewIfNeeded();
    await page.waitForTimeout(800);
    const start = await state(fig);
    assert.equal(start.holds, 'true', `${id}: Cauchy's setup should satisfy the hypothesis`);
    await fig.screenshot({ path: `${out}/${id}-start.png` });

    // drag every movable point (Mafs renders them as focusable <g> handles)
    const handles = fig.locator('.mafs-movable-point');
    const count = await handles.count();
    assert(count >= 2, `${id}: expected at least two draggable points, found ${count}`);
    const drags = [];
    for (let k = 0; k < count; k++) {
      const before = await state(fig);
      const box = await handles.nth(k).boundingBox();
      assert(box, `${id}: handle ${k} not rendered`);
      const cx = box.x + box.width / 2, cy = box.y + box.height / 2;
      await page.mouse.move(cx, cy);
      await page.mouse.down();
      // the eps handle moves vertically, the others along x
      const [dx, dy] = id === 'limit' && k === 0 ? [0, 40] : [-45, 0];
      await page.mouse.move(cx + dx / 2, cy + dy / 2, { steps: 4 });
      await page.mouse.move(cx + dx, cy + dy, { steps: 4 });
      await page.mouse.up();
      await page.waitForTimeout(400);
      const after = await state(fig);
      drags.push({ k, before: before.value, after: after.value, name: after.name });
    }
    assert(drags.some(d => d.before !== d.after), `${id}: no drag moved the raster readout`);
    await fig.screenshot({ path: `${out}/${id}-dragged.png` });

    // Reset, then each preset; the Break-it preset must break the hypothesis
    await fig.getByRole('button', { name: 'Reset to Cauchy’s setup' }).click();
    await page.waitForTimeout(300);
    assert.equal(await fig.getAttribute('data-case'), start.name, `${id}: reset case`);
    const labels = (await fig.locator('button').allTextContents()).filter(t => t && !t.startsWith('Reset'));
    const presets = [];
    for (const label of labels) {
      await fig.getByRole('button', { name: label, exact: true }).click();
      await page.waitForTimeout(400);
      const s = await state(fig);
      presets.push({ label, ...s });
      if (label.startsWith('Break it')) {
        assert.equal(s.holds, 'false', `${id}: "${label}" should break the hypothesis`);
        assert.notEqual(s.name, start.name, `${id}: case name should change on "${label}"`);
        await fig.screenshot({ path: `${out}/${id}-broken.png` });
      }
      await fig.getByRole('button', { name: 'Reset to Cauchy’s setup' }).click();
      await page.waitForTimeout(250);
    }
    assert(presets.some(p => p.label.startsWith('Break it')), `${id}: no Break-it preset`);
    assert.equal(await fig.getAttribute('data-case'), start.name, `${id}: final reset`);
    results[id] = { start, drags, presets };
  }
  const wasm = await page.evaluate(() => globalThis.__wasm);
  assert.deepEqual(errors, [], 'page errors');
  assert.deepEqual(remote, [], 'remote scripts');
  assert(wasm > 0, 'raster WebAssembly was not instantiated');
  console.log(JSON.stringify({ ok: true, wasm, errors, remote, results }));
} catch (e) {
  console.log(JSON.stringify({ ok: false, error: String(e), errors, remote, results }));
  process.exitCode = 1;
} finally { await browser.close(); }
