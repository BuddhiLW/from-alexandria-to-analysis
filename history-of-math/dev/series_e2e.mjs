// node dev/series_e2e.mjs http://127.0.0.1:8099/ target/series
// The apex lists every topic as a full-width card and nothing else: no svg,
// no strand selector, no ring map, no arcs, one line to the page of every
// work. Each card opens its topic page (<out>/series/<slug>/): that topic's
// timeline alone, exactly its own entries in year order, built entries
// link to notebooks with ?series, planned ones are not links, no chips, no
// arcs, no selector; under it the connected topics, each a link. A
// notebook's previous / next walk the topic in its order and keep ?series;
// the breadcrumb names every topic it is on. Nothing scrolls sideways at
// 390 px; no page errors.
// SERIES names the topic to walk (default chords-to-fourier, whose
// notebooks are all in the build of :only). SERVE=1: the apex is a Clerk
// serve (history-of-math.dev), topic pages are
// /notebooks/series/series_page?series=<slug>.
import { createRequire } from "node:module";
import { mkdirSync } from "node:fs";
const [apex = "http://127.0.0.1:8099/", output = "target/series"] = process.argv.slice(2);
const walk = process.env.SERIES || "chords-to-fourier";
const serve = !!process.env.SERVE;
const require = createRequire((process.env.PLAYWRIGHT || process.cwd()) + "/");
const { chromium } = require("playwright");
mkdirSync(output, { recursive: true });
const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
const page = await browser.newPage({ viewport: { width: 1200, height: 900 }, deviceScaleFactor: 1 });
const errors = [];
page.on("pageerror", e => errors.push(`${page.url()}: ${e}`));
const failed = [];
page.on("response", r => { if (r.status() >= 400) failed.push(`${r.status()} ${r.url()}`); });
const overflow = () => page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth);
const topicUrl = slug => serve ? new URL(`notebooks/series/series_page?series=${slug}`, apex).href
                               : new URL(`series/${slug}/`, apex).href;
const topicRe = slug => serve ? new RegExp(`series_page\\?series=${slug}$`) : new RegExp(`/series/${slug}/$`);

await page.goto(apex, { waitUntil: "networkidle" });
await page.locator(".series-card").first().waitFor({ timeout: 30000 });
const cards = await page.locator(".series-card").evaluateAll(cs => cs.map(c => ({
  slug: c.getAttribute("data-series"),
  title: c.querySelector(".series-link").textContent,
  href: c.querySelector(".series-link").getAttribute("href"),
  scope: c.querySelector(".series-scope").textContent.length,
  width: c.getBoundingClientRect().width })));
const container = await page.locator(".series-index").evaluate(e => e.getBoundingClientRect().width);
const decks = await page.locator(".series-decks li").count();
const apexClean = await page.evaluate(() => ({
  svgs: document.querySelectorAll(".series-index svg, .history-timeline, .strand-map, .influence-arc, .strand-bridge").length,
  selector: document.querySelectorAll("button[data-field], .timeline-fields").length,
  allLinks: [...document.querySelectorAll(".all-works-link a")].map(a => a.getAttribute("href")) }));
await page.screenshot({ path: `${output}/apex.png`, fullPage: true });

// the index: a plain-link h3 per series, the whole timeline under it. Every
// series page resolves, and so does every built notebook link of the walked
// series (ALL=1: every index link, for a build of the whole shelf).
const idx = await page.locator(".index-series").evaluateAll(ss => ss.map(s => ({
  slug: s.getAttribute("data-series"),
  head: s.querySelector("h3 > a.index-series-link")?.getAttribute("href") || null,
  notebooks: [...s.querySelectorAll("li[data-built=true] a.index-notebook")].map(a => a.getAttribute("href")),
  planned: [...s.querySelectorAll("li[data-built=false]")].map(l => l.querySelectorAll("a").length) })));
const firsts = await page.locator(".series-card a.series-first").evaluateAll(as => as.map(a => a.getAttribute("data-first")));
const status = async href => (await page.request.get(new URL(href, page.url()).href)).status();
const indexBad = [];
for (const s of idx) {
  if (s.head) { const c = await status(s.head); if (c >= 400) indexBad.push(`${c} ${s.head}`); }
  for (const h of (process.env.ALL || s.slug === walk) ? s.notebooks : []) {
    const c = await status(h); if (c >= 400) indexBad.push(`${c} ${h}`);
  }
}
const indexOk = idx.length === cards.length && idx.every(s => s.head && s.planned.every(n => n === 0)) &&
  idx.some(s => s.notebooks.length > 0) && indexBad.length === 0 && firsts.length > 0;

// the year of each entry, as the JVM wrote it, and every topic's own count
const pages = [];
for (const c of cards) {
  await page.goto(apex, { waitUntil: "networkidle" });
  await page.locator(`.series-card[data-series="${c.slug}"] .series-link`).click();
  await page.waitForURL(topicRe(c.slug), { timeout: 20000 });
  await page.locator(`.topic-page[data-series="${c.slug}"] .timeline-entry`).first().waitFor({ timeout: 30000 });
  const h1 = (await page.locator(".topic-page h1").first().innerText()).trim();
  const entries = await page.locator(".topic-timeline .timeline-entry").evaluateAll(es => es.map(e => ({
    year: +e.getAttribute("data-year"), built: e.getAttribute("data-built") === "true",
    href: e.querySelector("a.entry-link")?.getAttribute("href") || null })));
  const extra = await page.evaluate(() => document.querySelectorAll(
    ".strand-chip, .influence-arc, .strand-map, .timeline-fields, button[data-field]").length);
  const timelines = await page.locator(".topic-timeline").count();
  const conns = await page.locator(".series-connections li a").evaluateAll(as => as.map(a => a.getAttribute("href")));
  pages.push({ slug: c.slug, h1ok: h1 === c.title, n: entries.length, timelines, extra,
               ordered: entries.every((e, i) => i === 0 || entries[i - 1].year <= e.year),
               linksOk: entries.every(e => e.built ? (e.href || "").endsWith(`series=${c.slug}`) : e.href === null),
               built: entries.filter(e => e.built).length, conns: conns.length });
  if (c.slug === walk) await page.screenshot({ path: `${output}/topic-${walk}.png`, fullPage: true });
}

// walk one topic: its first built entry, then next to the end, then back
await page.goto(topicUrl(walk), { waitUntil: "networkidle" });
await page.locator(".topic-timeline a.entry-link").first().waitFor({ timeout: 30000 });
const expected = await page.locator(".topic-timeline .timeline-entry[data-built=true]").evaluateAll(es =>
  [...new Set(es.map(e => e.getAttribute("data-notebook")))]);
await page.locator(".topic-timeline a.entry-link").first().click();
const seen = [];
const crumbs = [];
const idOf = u => serve ? u.pathname.split("/").pop().replaceAll("_", "-") : u.pathname.split("/").filter(Boolean).pop();
for (let i = 0; i < expected.length; i++) {
  await page.waitForLoadState("networkidle");
  await page.locator(".series-nav").waitFor({ timeout: 60000 });
  const u = new URL(page.url());
  seen.push({ id: idOf(u), series: u.searchParams.get("series") });
  crumbs.push(await page.locator(".series-breadcrumb a").evaluateAll(as => as.map(a => ({
    slug: a.getAttribute("data-series"), current: a.getAttribute("aria-current") === "true" }))));
  if (i < expected.length - 1) await page.locator(".series-nav a[rel=next]").click();
}
const lastHasNext = await page.locator(".series-nav a[rel=next]").count();
await page.locator(`.series-breadcrumb a[data-series="${walk}"]`).click();
const crumbBack = await page.waitForURL(topicRe(walk), { timeout: 20000 }).then(() => true, () => false);

// phone width
await page.setViewportSize({ width: 390, height: 844 });
const phone = {};
for (const [k, url] of [["apex", apex], ["topic", topicUrl(walk)]]) {
  await page.goto(url, { waitUntil: "networkidle" });
  await page.waitForTimeout(800);
  phone[k] = await overflow();
  await page.screenshot({ path: `${output}/phone-${k}.png`, fullPage: false });
}

const ok = cards.length >= 12 && new Set(cards.map(c => c.slug)).size === cards.length &&
  cards.every(c => c.scope > 20 && c.width >= container - 2) &&
  decks === 6 && apexClean.svgs === 0 && apexClean.selector === 0 && apexClean.allLinks.length === 1 &&
  pages.length === cards.length && indexOk &&
  pages.every(p => p.h1ok && p.n > 0 && p.timelines === 1 && p.extra === 0 && p.ordered && p.linksOk) &&
  expected.length >= 3 && seen.length === expected.length &&
  // previous / next follow the series' reading order (notebook years), which may
  // differ from the order of the timeline's entries: every built one, once
  new Set(seen.map(s => s.id)).size === expected.length && seen.every(s => expected.includes(s.id) && s.series === walk) &&
  crumbs.every(cs => cs.filter(c => c.current).length === 1 && cs.find(c => c.current).slug === walk) &&
  lastHasNext === 0 && crumbBack &&
  phone.apex <= 0 && phone.topic <= 0 && errors.length === 0;
console.log(JSON.stringify({ ok, indexOk, index: idx.length, indexBad, firsts: firsts.length, cards: cards.length, decks, apexClean, pages, walk, expected, seen,
                             crumbs: crumbs.map(c => c.length), lastHasNext, crumbBack, phone, errors, failed }));
await browser.close();
process.exit(ok ? 0 : 1);
