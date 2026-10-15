// Browser gate for the three Euler playgrounds (Basel, Koenigsberg, V - E + F),
// run on the served static notebook:
//   node dev/euler_playgrounds_e2e.mjs http://localhost:8094/euler/ target/euler-playgrounds-e2e
// Checks per playground: a drag of a movable point changes a raster readout,
// a control changes the live case name, every "Try this" preset sets the
// state (case name read after each), the hypothesis badge turns red where
// the preset breaks the hypothesis, reset restores the author's case.
// Page-wide: 0 page errors, no remote scripts, WebAssembly instantiated.
import { createRequire } from 'node:module';
import { mkdirSync } from 'node:fs';
import assert from 'node:assert/strict';
const [url, out = 'target/euler-playgrounds-e2e'] = process.argv.slice(2);
if (!url || !url.startsWith('http')) throw new Error('pass a served notebook URL (http), not file://');
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + '/');
const { chromium } = require('playwright');
mkdirSync(out, { recursive: true });
const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1400, height: 1000 } });
const errors = [];
page.on('pageerror', e => errors.push(String(e)));
await page.addInitScript(() => {
  window.__eulerWasm = 0;
  for (const k of ['instantiate', 'compile']) {
    const f = WebAssembly[k];
    WebAssembly[k] = function (...a) { window.__eulerWasm++; return f.apply(this, a); };
  }
  const M = WebAssembly.Module;
  WebAssembly.Module = function (...a) { window.__eulerWasm++; return new M(...a); };
  WebAssembly.Module.prototype = M.prototype;
});

const settle = (ms = 350) => page.waitForTimeout(ms);
const attr = (loc, a) => loc.getAttribute(a);

async function drag(fig, index, dx, dy) {
  const h = fig.locator('.mafs-movable-point').nth(index);
  await h.scrollIntoViewIfNeeded();
  const b = await h.boundingBox();
  assert(b, 'movable point has no box');
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2);
  await page.mouse.down();
  await page.mouse.move(b.x + b.width / 2 + dx, b.y + b.height / 2 + dy, { steps: 10 });
  await page.mouse.up();
  await settle();
}

async function presets(fig, root, resetPrefix, expect) {
  const names = (await fig.locator('[data-preset]').allTextContents()).filter(Boolean);
  assert(names.length >= 3, 'at least two presets and a reset');
  const seen = {};
  for (const n of names.filter(n => !n.startsWith(resetPrefix))) {
    await fig.locator(`[data-preset="${n}"]`).click();
    await settle();
    seen[n] = { case: await attr(root, 'data-case'),
                hypothesis: await fig.locator('[data-hypothesis]').evaluate(el => getComputedStyle(el).backgroundColor) };
    assert(seen[n].case, `case after ${n}`);
  }
  for (const [n, check] of Object.entries(expect)) check(seen[Object.keys(seen).find(k => k.startsWith(n))], n);
  await fig.locator(`[data-preset^="${resetPrefix}"]`).click();
  await settle();
  return seen;
}

const RED = 'rgb(192, 57, 43)';
const results = {};
try {
  await page.goto(url, { waitUntil: 'networkidle', timeout: 180000 });

  // 1. Basel
  {
    const root = page.locator('[data-figure="euler-basel-playground"]');
    const fig = root.locator('[data-playground="euler-basel"]');
    await fig.waitFor({ timeout: 120000 });
    await fig.scrollIntoViewIfNeeded();
    const c0 = await attr(root, 'data-case'), sn0 = await attr(root, 'data-sn'), r0 = await attr(root, 'data-resid');
    assert.match(c0, /π²\/6/, 'Basel starts at zeta(2)');
    await drag(fig, 0, 120, 0);          // N along the terms
    assert.notEqual(await attr(root, 'data-sn'), sn0, 'dragging N moves S_N');
    await drag(fig, 1, 80, 0);           // x along sin x / x
    assert.notEqual(await attr(root, 'data-resid'), r0, 'dragging x moves the residual');
    const s = fig.locator('input[id="s"], input[id$=".s"]').first();
    await s.fill('0.8'); await s.press('Enter'); await settle();
    assert.match(await attr(root, 'data-case'), /diverges/, 's < 1 breaks the hypothesis');
    assert.equal(await fig.locator('[data-hypothesis]').evaluate(el => getComputedStyle(el).backgroundColor), RED);
    results.basel = await presets(fig, root, 'Reset', {
      'Set s = 1': (v, n) => { assert.match(v.case, /harmonic/, n); assert.equal(v.hypothesis, RED, n); },
      'Set s = 4': (v, n) => assert.match(v.case, /π⁴\/90/, n),
    });
    assert.equal(await attr(root, 'data-case'), c0, 'Basel reset');
    await fig.screenshot({ path: `${out}/basel.png` });
  }

  // 2. Koenigsberg
  {
    const root = page.locator('[data-figure="euler-koenigsberg-playground"]');
    const fig = root.locator('[data-playground="euler-koenigsberg"]');
    await fig.waitFor({ timeout: 120000 });
    await fig.scrollIntoViewIfNeeded();
    const c0 = await attr(root, 'data-case');
    assert.match(c0, /4 odd regions/, 'Koenigsberg: four odd regions');
    assert.equal(Number(await attr(root, 'data-letters')), 9, 'Euler: 9 letters');
    const shot0 = await fig.locator('.MafsView').first().screenshot();
    await drag(fig, 3, -60, 40);        // region D
    assert.notDeepEqual(await fig.locator('.MafsView').first().screenshot(), shot0, 'dragging D redraws the bridges');
    const bc = fig.locator('input[id="BC"], input[id$=".BC"]').first();
    await bc.fill('1'); await bc.press('Enter'); await settle();
    assert.match(await attr(root, 'data-case'), /two odd regions/, 'a bridge B-C allows the walk');
    assert.equal((await attr(root, 'data-walk')).length, 2 * 8 + 1, 'the walk spells 8 bridges and 9 regions');
    results.koenigsberg = await presets(fig, root, 'Reset', {
      'Cross each bridge twice': (v, n) => assert.match(v.case, /every region even/, n),
      'Split the city': (v, n) => { assert.match(v.case, /separate pieces/, n); assert.equal(v.hypothesis, RED, n); },
    });
    assert.equal(await attr(root, 'data-case'), c0, 'Koenigsberg reset');
    await fig.screenshot({ path: `${out}/koenigsberg.png` });
  }

  // 3. V - E + F
  {
    const root = page.locator('[data-figure="euler-vef-playground"]');
    const fig = root.locator('[data-playground="euler-vef"]');
    await fig.waitFor({ timeout: 120000 });
    await fig.scrollIntoViewIfNeeded();
    const c0 = await attr(root, 'data-case');
    assert.equal(await attr(root, 'data-chi'), '2', 'cube: V - E + F = 2');
    assert.equal(await attr(root, 'data-vef'), '8 12 6');
    await drag(fig, 4, 25, -15);         // inner vertex E, a small move keeps the map
    assert.equal(await attr(root, 'data-chi'), '2', 'a small drag keeps chi');
    await drag(fig, 4, 260, 60);         // across the outer square
    assert.match(await attr(root, 'data-case'), /edges cross/, 'pulled across: crossings');
    await fig.locator('[data-edge="12"]').click(); await settle();   // a diagonal
    await fig.locator('[data-preset^="Reset"]').click(); await settle();
    await fig.locator('[data-edge="12"]').click(); await settle();
    assert.equal(await attr(root, 'data-vef'), '8 13 7', 'a diagonal adds an edge and a face');
    await fig.locator('[data-outer]').click(); await settle();
    assert.equal(await attr(root, 'data-chi'), '1', 'outer face away: 1');
    results.vef = await presets(fig, root, 'Reset', {
      "Draw Cauchy": (v, n) => assert.match(v.case, /outer face taken away/, n),
      'Rub out': (v, n) => { assert.match(v.case, /2 pieces/, n); assert.equal(v.hypothesis, RED, n); },
      'Pull corner': (v, n) => { assert.match(v.case, /edges cross/, n); assert.equal(v.hypothesis, RED, n); },
    });
    assert.equal(await attr(root, 'data-case'), c0, 'V - E + F reset');
    for (const i of [0, 2]) {
      await fig.locator(`[data-solid="${i}"]`).click(); await settle();
      assert.equal(await attr(root, 'data-chi'), '2', `solid ${i}: chi 2`);
    }
    await fig.screenshot({ path: `${out}/vef.png` });
  }

  const remote = await page.evaluate(() => [...document.querySelectorAll('script[src]')]
    .map(s => s.src).filter(src => !src.startsWith(location.origin)));
  const wasm = await page.evaluate(() => window.__eulerWasm);
  assert.deepEqual(errors, [], 'page errors');
  assert.deepEqual(remote, [], 'remote scripts');
  assert(wasm > 0, 'raster WebAssembly was not instantiated');
  console.log(JSON.stringify({ ok: true, wasm, errors, remote, results }));
} finally { await browser.close(); }
