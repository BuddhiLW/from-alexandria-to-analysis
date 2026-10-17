// Headless check of the topics page (<site>/series/) of a static build:
//   node e2e/topics_page.mjs <site-dir>
// Playwright comes from $PLAYWRIGHT_DIR (default ~/PP/play/plato); the
// browser from $CHROMIUM (default the cached headless shell).
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';
import os from 'node:os';
import { createRequire } from 'node:module';

const site = path.resolve(process.argv[2] || 'target/history-of-math');
const req = createRequire(path.join(process.env.PLAYWRIGHT_DIR || `${os.homedir()}/PP/play/plato`, 'x.js'));
const { chromium } = req('playwright');
const exe = process.env.CHROMIUM ||
  `${os.homedir()}/.cache/ms-playwright/chromium_headless_shell-1243/chrome-headless-shell-linux64/chrome-headless-shell`;

const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.svg': 'image/svg+xml',
                '.edn': 'application/edn', '.json': 'application/json', '.ico': 'image/x-icon' };
const server = http.createServer((q, r) => {
  let p = path.join(site, decodeURIComponent(new URL(q.url, 'http://x').pathname));
  if (fs.existsSync(p) && fs.statSync(p).isDirectory()) p = path.join(p, 'index.html');
  if (!p.startsWith(site) || !fs.existsSync(p)) { r.writeHead(404); return r.end(); }
  r.writeHead(200, { 'content-type': types[path.extname(p)] || 'application/octet-stream' });
  fs.createReadStream(p).pipe(r);
});
await new Promise(ok => server.listen(0, '127.0.0.1', ok));
const base = `http://127.0.0.1:${server.address().port}`;

let failures = 0, passed = 0;
const check = (ok, what) => { ok ? passed++ : failures++; console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}`); };

const browser = await chromium.launch({ executablePath: exe });
const errors = [];
async function open(url, opts = {}) {
  const ctx = await browser.newContext(opts);
  const page = await ctx.newPage();
  page.on('pageerror', e => errors.push(`${url}: ${e.message}`));
  await page.goto(base + url, { waitUntil: 'load' });
  return page;
}

try {
  const raw = fs.readFileSync(path.join(site, 'series/index.html'), 'utf8');
  check(/<noscript><div class="hom-noscript">.*topics-list/s.test(raw), 'series/index.html carries the noscript list');
  check((raw.match(/href="\.\.\/series\/[a-z-]+\/"/g) || []).length >= 12, 'noscript list links every topic timeline');

  const page = await open('/series/');
  await page.waitForSelector('svg.topics-graph g.topics-node', { timeout: 30000 });
  check(await page.locator('g.topics-node').count() === 12, 'the graph has 12 topic nodes');
  check(await page.locator('g.topics-edge').count() >= 10, 'the graph has the shared-work edges');

  const slug = 'chords-to-fourier';
  await page.locator(`g.topics-node[data-series="${slug}"]`).click();
  await page.waitForSelector(`section.topics-panel[data-kind="topic"][data-series="${slug}"]`);
  const lit = await page.$$eval('g.topics-node', ns => ns.map(n => [n.dataset.series, n.dataset.lit]));
  const on = lit.filter(([, l]) => l === 'true').map(([s]) => s);
  check(on.includes(slug) && on.length > 1 && on.length < 12, `selecting ${slug} lights it and its neighbours (${on.length} lit)`);
  const hrefs = await page.$$eval('section.topics-panel a.topics-notebook', as => as.map(a => a.getAttribute('href')));
  check(hrefs.length > 0 && hrefs.every(h => h.endsWith(`/?series=${slug}`)), `panel lists its notebooks (${hrefs.length} built)`);
  const flags = await page.$$eval('section.topics-panel li.topics-work', ls => ls.map(l => l.dataset.built));
  check(flags.join(',') === [...flags].sort().reverse().join(','), 'built works first, planned after');
  check(await page.locator('section.topics-panel a.topics-open-series').getAttribute('href') === `../series/${slug}/`,
        'panel links the topic timeline');
  check(page.url().includes(`?series=${slug}`), 'the URL remembers the topic');

  await page.locator('g.topics-edge .topics-edge-hit').first().dispatchEvent('click');
  check(await page.locator('section.topics-panel[data-kind="edge"]').count() === 1, 'selecting an edge shows the topics it bridges');
  await page.locator(`g.topics-node[data-series="${slug}"]`).click();
  await page.locator('button.topics-bridges').first().click();
  check(await page.locator('section.topics-panel[data-kind="work"] .topics-work-topics li').count() >= 2,
        'selecting a work names the topics it bridges');

  const pre = await open(`/series/?series=whole-numbers`);
  await pre.waitForSelector('section.topics-panel[data-series="whole-numbers"]', { timeout: 30000 });
  check(true, '?series= preselects a topic');

  const mobile = await open('/series/', { viewport: { width: 390, height: 844 }, deviceScaleFactor: 2 });
  await mobile.waitForSelector('svg.topics-graph');
  const [sw, cw] = await mobile.evaluate(() => [document.documentElement.scrollWidth, document.documentElement.clientWidth]);
  check(sw <= cw, `390px wide: no horizontal scroll (${sw} <= ${cw})`);

  const lum = c => { const [r, g, b] = c.match(/\d+(\.\d+)?/g).map(Number); return (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255; };
  const bgOf = p => p.evaluate(() => {
    for (let e = document.querySelector('.topics-panel'); e; e = e.parentElement) {
      const c = getComputedStyle(e).backgroundColor;
      if (c && !/rgba\(0, 0, 0, 0\)|transparent/.test(c)) return c;
    }
    return 'rgb(255, 255, 255)'; // nothing painted: the browser's white
  });
  // the pages follow Clerk's dark mode, html.dark (the toggle in its menu)
  const dark = await open('/series/', { colorScheme: 'dark' });
  await dark.waitForSelector('section.topics-panel');
  await dark.evaluate(() => document.documentElement.classList.add('dark'));
  await dark.waitForTimeout(300);
  const darkBg = await bgOf(dark);
  check(lum(darkBg) < 0.3, `dark mode: the page is dark (${darkBg})`);
  const ink = await dark.evaluate(() => getComputedStyle(document.querySelector('.topics-panel')).color);
  check(lum(ink) > 0.6, `dark mode: the text is light (${ink})`);

  const apex = await open("/");
  await apex.waitForFunction(() => document.querySelector('a.topics-hub-link[href="series/"]'), null, { timeout: 30000 }).catch(() => {});
  check(await apex.locator('a.topics-hub-link[href="series/"]').count() >= 1, 'the landing links series/');
  const sp = await open('/series/alexandria-to-analysis/');
  await sp.waitForLoadState('networkidle');
  check(await sp.locator('a[href^="../../series/"]').count() >= 1, 'a series page links the topics page');

  check(errors.length === 0, `no page errors${errors.length ? ': ' + errors.join(' | ') : ''}`);
} catch (e) {
  failures++; console.log('FAIL', e.message);
} finally {
  await browser.close(); server.close();
}
console.log(`${passed} passed, ${failures} failed`);
process.exit(failures ? 1 : 0);
