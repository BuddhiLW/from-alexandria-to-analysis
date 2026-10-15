// Browser gate for the ten Cauchy playgrounds, run on a served static notebook:
// node dev/cauchy_playgrounds_e2e.mjs http://localhost:8093/cauchy.html target/cauchy-e2e
import { createRequire } from 'node:module';
import { mkdirSync } from 'node:fs';
import assert from 'node:assert/strict';
const [url, out = 'target/cauchy-e2e'] = process.argv.slice(2);
if (!url) throw new Error('pass a served notebook URL, not file://');
if (!url.startsWith('http')) throw new Error('use HTTP');
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + '/');
const { chromium } = require('playwright');
mkdirSync(out, { recursive: true });
const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1400, height: 960 } });
const errors = [];
page.on('pageerror', e => errors.push(String(e)));
await page.addInitScript(() => {
  window.__cauchyWasm = 0;
  const original = WebAssembly.instantiate;
  WebAssembly.instantiate = function (...args) {
    window.__cauchyWasm++;
    return original.apply(this, args);
  };
});
const ids = ['limit', 'continuity-ch2', 'ivt', 'series', 'root-test', 'derivative',
  'integral', 'refinement', 'fundamental', 'flat'];
const results = {};
try {
  await page.goto(url, { waitUntil: 'networkidle', timeout: 120000 });
  for (const id of ids) {
    const fig = page.locator(`[data-playground="${id}"]`).first();
    await fig.waitFor({ timeout: 120000 });
    await fig.scrollIntoViewIfNeeded();
    const before = { value: await fig.getAttribute('data-value'), name: await fig.getAttribute('data-case') };
    const slider = fig.locator('input[id="n"], input[id$=".n"], input[id="i"], input[id$=".i"]').first();
    assert(await slider.count(), `${id}: no n or increment control`);
    await slider.fill(id === 'series' || id === 'root-test' || id === 'ivt' || id === 'integral' || id === 'refinement' || id === 'fundamental' || id === 'flat' ? '25' : '0.05');
    await slider.press('Enter');
    await page.waitForTimeout(400);
    const changed = await fig.getAttribute('data-value');
    assert.notEqual(changed, before.value, `${id}: changing input did not update raster readout`);
    const buttons = fig.locator('button', { hasNotText: 'Reset to Cauchy' });
    const presets = await buttons.allTextContents();
    for (const text of presets.filter(t => t && t !== 'Reset to Cauchy’s setup')) {
      await fig.getByRole('button', { name: text, exact: true }).click();
      await page.waitForTimeout(150);
      assert(await fig.getAttribute('data-case'), `${id}: missing case after ${text}`);
    }
    await fig.getByRole('button', { name: 'Reset to Cauchy’s setup' }).click();
    await page.waitForTimeout(300);
    assert.equal(await fig.getAttribute('data-case'), before.name, `${id}: reset case`);
    await fig.screenshot({ path: `${out}/${id}.png` });
    results[id] = { before, changed, presets };
  }
  const remote = await page.evaluate(() => [...document.querySelectorAll('script[src]')]
    .map(s => s.src).filter(src => !src.startsWith(location.origin)));
  const wasm = await page.evaluate(() => window.__cauchyWasm);
  assert.deepEqual(errors, [], 'page errors');
  assert.deepEqual(remote, [], 'remote scripts');
  assert(wasm > 0, 'raster WebAssembly was not instantiated');
  console.log(JSON.stringify({ ok: true, wasm, errors, remote, results }));
} finally { await browser.close(); }
