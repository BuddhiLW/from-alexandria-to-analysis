// node dev/rigor_playground_e2e.mjs http://127.0.0.1:8099/riemann_weierstrass/ http://127.0.0.1:8099/dedekind_cantor/
// Serve each static build over HTTP. Screenshots live under target/.
import { createRequire } from 'node:module';
import { mkdirSync } from 'node:fs';
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + '/');
const { chromium } = require('playwright');
const [riemann, dedekind] = process.argv.slice(2);
if (!riemann || !dedekind) throw Error('Supply both notebook URLs');
const browser = await chromium.launch({
  args: ['--use-gl=swiftshader', '--enable-unsafe-swiftshader'],
  ...(process.env.CHROMIUM ? {executablePath: process.env.CHROMIUM} : {})
});
mkdirSync('target', {recursive:true});
const results = [];
for (const [name, url, ids] of [
  ['riemann', riemann, ['integral','jumps','uniform','monster']],
  ['dedekind', dedekind, ['cut','zigzag','heights','diagonal','measure','dirichlet']]
]) {
  const page = await browser.newPage({viewport:{width:1280,height:900}});
  const errors = [], remote = [];
  page.on('pageerror', e => errors.push(String(e)));
  page.on('request', request => {
    const u = new URL(request.url());
    if (u.protocol.startsWith('http') && u.origin !== new URL(url).origin) remote.push(request.url());
  });
  await page.addInitScript(() => {
    const W = globalThis.WebAssembly;
    globalThis.__wasm = {instance:0, instantiate:0};
    const old = W.instantiate.bind(W);
    W.instantiate = (...args) => {globalThis.__wasm.instantiate++; return old(...args);};
    W.Instance = new Proxy(W.Instance, {construct(t,args,n) {
      globalThis.__wasm.instance++; return Reflect.construct(t,args,n);
    }});
  });
  await page.goto(url, {waitUntil:'networkidle'});
  for (const id of ids) {
    const widget = page.locator(`[data-playground="${id}"]`);
    await widget.waitFor({timeout:30000});
    const readout = widget.locator('[data-readout]');
    const named = widget.locator('[data-case]');
    const before = await readout.innerText();
    const caseBefore = await named.innerText();
    const inputs = widget.locator('input[type=range]');
    for (let j = 0; j < await inputs.count(); j++) {
      const input = inputs.nth(j);
      await input.evaluate(el => {
        el.value = String(Number(el.max) * 0.8 + Number(el.min) * 0.2);
        el.dispatchEvent(new Event('change', {bubbles:true}));
      });
    }
    const handle = widget.locator('[data-drag]').first();
    const box = await handle.boundingBox();
    if (box) {
      await page.mouse.move(box.x + box.width/2, box.y + box.height/2);
      await page.mouse.down(); await page.mouse.move(box.x + 90, box.y + box.height/2, {steps:8}); await page.mouse.up();
    }
    const after = await readout.innerText();
    const buttons = widget.locator('button');
    for (let j = 0; j < await buttons.count(); j++) await buttons.nth(j).click();
    await widget.screenshot({path:`target/rigor-${name}-${id}.png`});
    results.push({id, before, after, caseBefore, caseAfter:await named.innerText(),
      controls:await inputs.count(), buttons:await buttons.count(), dragged:!!box});
  }
  const wasm = await page.evaluate(() => globalThis.__wasm);
  results.push({name, errors, remote, wasm});
  await page.close();
}
await browser.close();
const ok = results.every(r => r.id ? r.controls > 0 && r.buttons >= 3 && r.dragged && r.before !== r.after
  : !r.errors.length && !r.remote.length && r.wasm.instantiate + r.wasm.instance > 0);
console.log(JSON.stringify({ok, results}));
process.exit(ok ? 0 : 1);
