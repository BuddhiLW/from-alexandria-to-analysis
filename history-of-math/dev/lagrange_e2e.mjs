// Run: PLAYWRIGHT=/path/to/project-with-playwright node dev/lagrange_e2e.mjs http://127.0.0.1:8099/lagrange/
// A served static build is required; never use file://.
import {createRequire} from 'node:module';
import {mkdir} from 'node:fs/promises';
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + '/');
const {chromium} = require('playwright');
const url = process.argv[2] || 'http://127.0.0.1:8099/lagrange/';
if (!url.startsWith('http://')) throw Error('Serve the static build over HTTP');
const browser = await chromium.launch({args:['--use-gl=swiftshader','--enable-unsafe-swiftshader'], ...(process.env.CHROMIUM ? {executablePath:process.env.CHROMIUM} : {})});
const page = await browser.newPage({viewport:{width:1280,height:950}});
const errors=[], remote=[];
page.on('pageerror', e => errors.push(String(e)));
page.on('request', q => {if(q.resourceType()==='script' && new URL(q.url()).origin!==new URL(url).origin) remote.push(q.url());});
await page.addInitScript(() => {
  const W=globalThis.WebAssembly;
  globalThis.__lagrangeWasm=0;
  W.Instance = new Proxy(W.Instance, {construct(t,args,nt) { globalThis.__lagrangeWasm++; return Reflect.construct(t,args,nt); }});
});
await page.goto(url,{waitUntil:'networkidle'});
await page.locator('[data-figure="lagrange-mean"]').waitFor({timeout:60000});
await mkdir('target',{recursive:true});
const ids=['brach','delta','virtual','pendulum','double','kepler','mean'];
const results=[];
for (const id of ids) {
  const figure=page.locator(`[data-figure="lagrange-${id}"]`);
  await figure.scrollIntoViewIfNeeded();
  const title=figure.locator(`[data-case="${id}"]`);
  const before=await title.innerText();
  const paths=await figure.locator('svg path').count();
  const controls=await figure.locator('input').count();
  const presets=figure.getByRole('button');
  const n=await presets.count();
  const cases=[];
  for(let i=0;i<n;i++) {await presets.nth(i).click(); await page.waitForTimeout(100); cases.push(await title.innerText());}
  const inputs=figure.locator('input[type="number"]');
  if (await inputs.count()) {
    const input=inputs.first(); const value=Number(await input.inputValue());
    await input.fill(String(value===0 ? 0.2 : value*0.9)); await input.press('Tab');
  }
  const handle=figure.locator('.MafsView svg [role="button"], .MafsView svg circle').first();
  if(await handle.count()) {const box=await handle.boundingBox(); if(box) await page.mouse.move(box.x+box.width/2,box.y+box.height/2);}
  await figure.screenshot({path:`target/lagrange-${id}.png`});
  results.push({id,before,cases,paths,controls,presets:n,after:await title.innerText(),readout:await figure.innerText()});
}
const wasm=await page.evaluate(()=>globalThis.__lagrangeWasm);
const ok=!errors.length && !remote.length && wasm>0 && results.every(r=>r.paths>0 && r.controls>0 && r.presets>=3 && r.cases.some(c=>c!==r.before));
console.log(JSON.stringify({ok,errors,remote,wasm,results:results.map(({readout,...r})=>({...r,readout:readout.slice(0,170)}))}));
await browser.close();
process.exit(ok?0:1);
