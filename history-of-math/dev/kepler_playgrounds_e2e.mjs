// Browser gate for the Kepler playgrounds, on a served static build (never file://):
//   node dev/kepler_playgrounds_e2e.mjs mysterium http://localhost:8094/kepler_mysterium/ target/kepler-e2e
//   node dev/kepler_playgrounds_e2e.mjs laws      http://localhost:8094/kepler_laws/      target/kepler-e2e
// PLAYWRIGHT names a directory holding node_modules/playwright; CHROMIUM a browser binary.
// mysterium (alexandria.kepler.mysterium-playground): reorders a solid with the
//   gap select (the order stays a permutation, the radii re-nest, the verdict
//   says Kepler's order fits better), drags a planet's point on the equatorial
//   section (the radii scale), toggles Mercury's midsphere (the rule badge turns
//   red), orbits the MathBox canvas, presses every Try-this preset and Reset.
// laws (alexandria.kepler.playground): sector playground: drags the perihelion
//   (eccentricity and areas change), presses every preset; the force 1/r^2.5
//   preset keeps equal areas but breaks the ellipse badge; drag makes the
//   case "resisted". Harmonic playground: drags the planet along the law,
//   presses the planet presets and the 1/r preset (case changes, badge red).
// Then: 0 page errors, no remote scripts, WebAssembly instantiated. Prints one
// JSON line; exit 1 on failure.
import { createRequire } from 'node:module';
import { mkdirSync } from 'node:fs';

const [which, url, out = 'target/kepler-e2e'] = process.argv.slice(2);
if (!url || !url.startsWith('http')) throw new Error('pass a served notebook URL (http), not file://');
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + '/');
const { chromium } = require('playwright');
mkdirSync(out, { recursive: true });

const browser = await chromium.launch({
  args: ['--use-gl=swiftshader', '--enable-unsafe-swiftshader', '--ignore-gpu-blocklist'],
  ...(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {}),
});
const page = await browser.newPage({ viewport: { width: 1400, height: 1100 } });
const errors = [], remote = [], fails = [], steps = [];
const origin = new URL(url).origin;
page.on('pageerror', (e) => errors.push(String(e) + ' @ ' + String(e.stack || '').slice(0, 400) + ' after ' + (steps.at(-1) || ['start'])[0]));
page.on('request', (r) => {
  if (r.resourceType() === 'script' && !r.url().startsWith(origin) && !r.url().startsWith('data:')) remote.push(r.url());
});
await page.addInitScript(() => {
  const W = globalThis.WebAssembly;
  globalThis.__wasm = 0;
  const inst = W.instantiate.bind(W);
  W.instantiate = (...a) => { globalThis.__wasm++; return inst(...a); };
  W.Instance = new Proxy(W.Instance, { construct(t, a, nt) { globalThis.__wasm++; return Reflect.construct(t, a, nt); } });
});
const check = (label, ok, got) => { steps.push([label, !!ok]); if (!ok) fails.push({ label, got }); };
const settle = (ms = 900) => page.waitForTimeout(ms);
const drag = async (handle, dx, dy) => {
  const b = await handle.boundingBox();
  const x = b.x + b.width / 2, y = b.y + b.height / 2;
  await page.mouse.move(x, y); await page.mouse.down();
  await page.mouse.move(x + dx / 2, y + dy / 2, { steps: 5 });
  await page.mouse.move(x + dx, y + dy, { steps: 5 });
  await page.mouse.up(); await settle();
};
const press = async (fig, label) => { await fig.locator(`button[data-preset="${label}"]`).click(); await settle(1500); };
const presets = (fig) => fig.locator('button[data-preset]').evaluateAll((bs) => bs.map((b) => b.getAttribute('data-preset')));

await page.goto(url, { waitUntil: 'networkidle', timeout: 120000 });

if (which === 'mysterium') {
  const fig = page.locator('[data-playground="kepler-mysterium-nest"]').first();
  await fig.waitFor({ timeout: 120000 });
  await fig.scrollIntoViewIfNeeded();
  await settle(4000);
  const st = async () => fig.evaluate((el) => ({
    order: JSON.parse(el.getAttribute('data-order')), radii: JSON.parse(el.getAttribute('data-radii')),
    misses: JSON.parse(el.getAttribute('data-misses')), rank: el.getAttribute('data-rank'), mid: el.getAttribute('data-mid'),
    verdict: el.querySelector('[data-verdict]').textContent, solid: el.querySelector('[data-solid-name]').textContent,
    badges: [...el.querySelectorAll('[data-invariant]')].map((b) => b.getAttribute('data-invariant')),
  }));
  let s = await st();
  check('start: Kepler order', s.order.join() === 'cube,tetrahedron,dodecahedron,icosahedron,octahedron', s.order);
  check('start: rank 1, verdict names Kepler', s.rank === '1' && s.verdict.startsWith("Kepler's order fits best"), s.verdict);
  check('start: all badges pass', s.badges.every((b) => b === 'pass'), s.badges);
  check('start: radii finite and nested', s.radii.every((r, i) => isFinite(r) && (i === 0 || r < s.radii[i - 1])), s.radii);
  check('start: tetrahedron miss under 1%', Math.abs(s.misses[1]) < 0.01, s.misses);
  check('one MathBox canvas', (await fig.locator('canvas').count()) >= 1, null);
  await page.screenshot({ animations: "disabled", path: `${out}/mysterium-start.png` });
  // reorder: put the octahedron in the first gap
  await fig.locator('select[data-gap="0"]').selectOption('octahedron'); await settle(1500);
  s = await st();
  check('reorder: permutation kept', [...s.order].sort().join() === 'cube,dodecahedron,icosahedron,octahedron,tetrahedron' && s.order[0] === 'octahedron' && s.order[4] === 'cube', s.order);
  check('reorder: octahedron/cube dual swap ties Kepler', s.rank === '1' && s.verdict.startsWith('this order ties'), s.verdict);
  await fig.locator('select[data-gap="1"]').selectOption('dodecahedron'); await settle(1500);
  s = await st();
  check('reorder: worse order ranked, verdict names Kepler better', Number(s.rank) > 1 && s.verdict.includes("Kepler's order fits better"), s.verdict);
  check('reorder: best-order badge red', s.badges[0] === 'fail', s.badges);
  await page.screenshot({ animations: "disabled", path: `${out}/mysterium-reordered.png` });
  // drag a planet point on the section: the nest scales
  const before = (await st()).radii[0];
  const handles = fig.locator('[data-section] .mafs-movable-point');
  check('six draggable planets', (await handles.count()) === 6, await handles.count());
  await drag(handles.first(), -40, -40);
  const after = (await st()).radii;
  check('drag Saturn: radii scale', Math.abs(after[0] - before) > 1e-3 && Math.abs(after[1] / after[0] - (await st()).radii[1] / (await st()).radii[0]) < 1e-9, [before, after[0]]);
  // orbit the 3D scene
  const canvas = fig.locator('canvas').first();
  const cb = await canvas.boundingBox();
  await page.mouse.move(cb.x + cb.width / 2, cb.y + cb.height / 2); await page.mouse.down();
  await page.mouse.move(cb.x + cb.width * 0.7, cb.y + cb.height * 0.55, { steps: 10 }); await page.mouse.up(); await settle();
  // presets
  for (const p of await presets(fig)) {
    await press(fig, p);
    s = await st();
    steps.push([`preset ${p}`, s.order.join(), s.rank, s.verdict]);
    if (p === 'Swap cube and tetrahedron') {
      check('swap cube/tetrahedron: worse than Kepler', Number(s.rank) > 1 && s.verdict.includes("Kepler's order fits better"), s.verdict);
      check('swap: shell 1 is the tetrahedron', s.solid === 'tetrahedron', s.solid);
    }
    if (p.startsWith('Mercury on the midsphere')) {
      check('midsphere: rule badge red', s.mid === 'true' && s.badges[1] === 'fail', s.badges);
      check('midsphere: Mercury miss under 3%', Math.abs(s.misses[4]) < 0.03, s.misses);
    }
    await page.screenshot({ animations: "disabled", path: `${out}/mysterium-${p.replace(/\W+/g, '-')}.png` });
  }
  s = await st();
  check('reset ends on Kepler with insphere', s.order[0] === 'cube' && s.mid === 'false' && s.rank === '1', s);
  await fig.locator('input[data-mid]').click(); await settle();
  s = await st();
  check('midsphere checkbox: rule badge red', s.mid === 'true' && s.badges[1] === 'fail', s.badges);
  await fig.locator('input[data-mid]').click(); await settle();
}

if (which === 'laws') {
  const sec = page.locator('[data-playground="kepler-sectors"]').first();
  await sec.waitFor({ timeout: 120000 }); await sec.scrollIntoViewIfNeeded(); await settle(4000);
  const sst = () => sec.evaluate((el) => ({
    areas: JSON.parse(el.getAttribute('data-areas')), spread: +el.getAttribute('data-spread'), gap: +el.getAttribute('data-gap'),
    name: el.querySelector('[data-case]').textContent, badges: [...el.querySelectorAll('[data-invariant]')].map((b) => b.getAttribute('data-invariant')),
  }));
  let s = await sst();
  check('sectors start: ellipse named, badges pass', s.name.startsWith('ellipse') && s.badges.every((b) => b === 'pass'), s);
  check('sectors start: 8 areas from RK4', s.areas.length === 8, s.areas);
  // a real mouse drag of the perihelion (its label sits on it: before the
  // series' mafs.css made Mafs text click-through, the label took the
  // mousedown and the view panned): the point moves alone, no pan
  const peri = sec.locator('.mafs-movable-point').first();
  const view = () => sec.locator('.MafsView').first().evaluate((v) =>
    v.style.getPropertyValue('--mafs-view-transform') + ' | ' + (v.querySelector('svg') || v).getAttribute('viewBox'));
  const centre = async (h) => { const b = await h.boundingBox(); return [b.x + b.width / 2, b.y + b.height / 2]; };
  const v0 = await view(), c0 = await centre(peri);
  await drag(peri, 60, 0); await settle(1500);
  const c1 = await centre(peri);
  check('mouse drag perihelion: no pan', (await view()) === v0, [v0, await view()]);
  check('mouse drag perihelion: the point moved', Math.abs(c1[0] - c0[0]) > 3, [c0, c1]);
  const s2 = await sst();
  check('drag perihelion: areas change, still equal', s2.areas[0] !== s.areas[0] && s2.badges[0] === 'pass', [s.areas[0], s2.areas[0]]);
  for (const p of await presets(sec)) {
    await press(sec, p); s = await sst(); steps.push([`sector preset ${p}`, s.name]);
    if (p.startsWith('Force 1/r^2.5')) check('p=2.5: equal areas hold, ellipse breaks', s.badges[0] === 'pass' && s.badges[1] === 'fail' && s.name.startsWith('central but not'), s);
    if (p === 'A resisting medium') check('drag: resisted, areas unequal', s.name.startsWith('resisted') && s.badges[0] === 'fail', s);
    if (p === 'Circular orbit') check('circle named', s.name.startsWith('circle'), s.name);
  }
  await page.screenshot({ animations: "disabled", path: `${out}/laws-sectors.png` });

  const har = page.locator('[data-playground="kepler-harmonic"]').first();
  await har.scrollIntoViewIfNeeded(); await settle(2000);
  const hst = () => har.evaluate((el) => ({
    T: +el.getAttribute('data-period'), ratio: +el.getAttribute('data-ratio'), spread: +el.getAttribute('data-spread'),
    name: el.querySelector('[data-case]').textContent, badge: el.querySelector('[data-invariant]').getAttribute('data-invariant'),
  }));
  let h = await hst();
  check('harmonic start: T^2/a^3 = 1 by RK4', Math.abs(h.ratio - 1) < 1e-3 && h.badge === 'pass', h);
  await drag(har.locator('.mafs-movable-point').first(), 80, 0);
  const h2 = await hst();
  check('drag planet: T changes, ratio stays', Math.abs(h2.T - h.T) > 1e-3 && Math.abs(h2.ratio - 1) < 1e-3, [h.T, h2.T]);
  for (const p of await presets(har)) {
    await press(har, p); h = await hst(); steps.push([`harmonic preset ${p}`, h.name]);
    if (p === 'Force 1/r: what slope?') check('1/r: no harmonic law, badge red', h.name.startsWith('force 1/r^1.00') && h.badge === 'fail', h);
    if (p === 'Beyond Saturn') check('beyond Saturn named', h.name.startsWith('beyond Saturn'), h.name);
  }
  await page.screenshot({ animations: "disabled", path: `${out}/laws-harmonic.png` });
}

const wasm = await page.evaluate(() => globalThis.__wasm);
check('wasm in use', wasm >= 1, wasm);
check('no remote scripts', remote.length === 0, remote);
check('no page errors', errors.length === 0, errors);
console.log(JSON.stringify({ which, ok: fails.length === 0, fails, wasm, checks: steps.length }));
await browser.close();
process.exit(fails.length === 0 ? 0 : 1);
