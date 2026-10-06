# From Alexandria to Analysis

Clerk notebooks on the history of mathematics, from Euclid's *Elements*
(c. 300 BC) to Lebesgue's integral (1902). This project holds no
mathematics: figures, proofs, graded checks and proof players live in
`../alexandria` (core) and `../alexandria/media` (players). A notebook here
narrates and mounts them.

## Build

```sh
clojure -J-Xmx2g -X:notebooks :out-path '"target/hom"'
```

writes `target/hom/index.html` (the hub) and `target/hom/<id>/index.html` per
notebook. Each notebook is its own Clerk static app, so links between
pages (the hub's timeline and table, each notebook's previous/next links)
carry `data-ignore-anchor-click`. Clerk's in-app router then leaves them
alone and the browser loads the page. The hub (`notebooks/index.clj`) is
never cached, so edits to `timeline.edn` always show. The id is the file stem with `_` as `-`
(`notebooks/euclid_elements.clj` becomes `euclid-elements`).

The pages do not load Clerk's CDN viewer. The build release-compiles
`history-of-math.sci-extensions` (Clerk's viewer, emmy-viewers from the
local PR #83 worktree, alexandria's players) once with shadow-cljs, through
`mentat.clerk-utils`, into `<out>/_data/<hash>.js`, and copies the widget
CSS into `<out>/css/`. Every page loads both by a relative path. Run
`npm install` here first. `:only '["conics-playground"]'` builds just the
named notebooks and the hub; `:cljs-namespaces '[]'` goes back to the CDN
viewer.

No page loads a script from another origin. Clerk's render fetches KaTeX
(`katex@0.16.4`) and Plotly (`plotly.js-dist@2.15.1`) at runtime with
d3-require, which by default goes to cdn.jsdelivr.net; the bundle resolves
`d3-require` to `js/d3_require_local.js`, which sends those two names to
`<out>/vendor/<name>.js`, copied from `node_modules` by the build. Clerk's
Tailwind CDN script is replaced by `<out>/css/viewer.css`, compiled once
after all pages with `node_modules/.bin/tailwindcss` (Clerk's own
`viewer.css` and `tailwind.config.js`, content = the built pages and the
bundle). The build also writes `<out>/favicon.ico` and links it from every
page. Only stylesheets and fonts still come from CDNs (KaTeX CSS and fonts,
Bunny Fonts). `node dev/offline_e2e.mjs <base-url>` loads the hub and
three notebooks and fails on any other remote request, any failed request
or any console error.

Widgets (Leva, Mafs, MathBox) compile on the raster backend: a notebook
calls `(history-of-math.widgets/install!)` in a hidden form at its top.
`notebooks/conics_playground.clj` is the example, and
`node dev/widgets_e2e.mjs <url>` checks it in Chromium.

Tests (discovery, notebook metadata, the timeline):

```sh
clojure -J-Xmx2g -M:test
```

## How a notebook declares itself

Put the notebook at `notebooks/<stem>.clj` and give its `ns` form three keys:

```clojure
(ns euclid-elements
  {:history/year -300                 ; int, the work's date; negative = BC
   :history/title "Euclid, Elements"
   :history/era "Greek geometry"
   :nextjournal.clerk/toc true}
  (:require ...))
```

`history-of-math.build` reads only that first form, with edamame, and
evaluates nothing. It validates the keys with malli and sorts the notebooks
by year. A notebook missing a key fails the build with a message naming the
file and the missing keys.

`notebooks/index.clj` is the hub and is never discovered. It draws the
series timeline from `resources/history_of_math/timeline.edn`. Each entry
there has a year, a figure, a work, a one-line significance and an era. An
entry links to its notebook once one exists. It finds the notebook by
`:notebook` id, or else by the same year and a title that names the figure.
Entries with no notebook show as planned. Era colours come from the
`:series` palette in `alexandria.palette`.

## Adding a notebook

1. Put the mathematics in alexandria: shelf data, an `alexandria.<author>.<topic>`
   namespace, and a viewer in `media`.
2. Write `notebooks/<stem>.clj` with the three keys. It requires the
   alexandria viewers and `history-of-math.page` (step tables, grade badges).
3. If the work is not on the timeline yet, add it to `timeline.edn`, with
   `:notebook "<id>"`.

## Dev server

A live Clerk server for every notebook so far, on **http://127.0.0.1:7777/**
(the hub; a notebook is at `/notebooks/<file stem>`, e.g.
`/notebooks/apollonius_conics`). It runs from the integration worktree
`.claude/worktrees/alex-dev-live` (branch `alex/dev-live`, never pushed),
where `dev/merge_loop.sh` merges every `alex/illus-*` branch and `alexandria`
each minute (log: `history-of-math/.clerk/dev-logs/merge.log`; a conflict
outside `catalogue.edn` / `timeline.edn` aborts that merge and skips the
branch until it gets a new commit).

`history-of-math.dev/serve!` serves the pages with the series' own bundle
(`history-of-math.sci-extensions`, built from `history-of-math.bundle/config`)
compiled by shadow-cljs in watch mode, the raster backend installed, and links
that follow the mode (`history-of-math.links`: `/notebooks/<stem>` here,
`<id>/` in the static build).

What reloads without a restart:

| change | effect |
|---|---|
| `notebooks/*.clj` saved (or merged) | Clerk re-evaluates it and every open tab shows it (about 3 s) |
| alexandria `.clj`/`.cljc` (`../alexandria/src`, `../alexandria/media/src`) | loaded into the JVM, the last notebook re-evaluated with no cache |
| a scene `.cljs`/`.cljc` that Clerk ships as source | the last notebook is re-shown with it |
| a bundled namespace (`widgets/bundled-cljs`, emmy-viewers) | shadow-cljs recompiles it and open pages reload |

A restart is needed for `deps.edn`, `package.json`/`node_modules` and
`history-of-math.dev` itself. Clerk shows one document at a time: every open
tab follows the notebook evaluated last.

Start (from `alex-dev-live/history-of-math`; plain nREPL on 7931, survives
hive restarts, lives 12 h):

    setsid nohup timeout 43200 clojure -J-Xmx3g \
      -Sdeps '{:deps {nrepl/nrepl {:mvn/version "1.3.0"}}}' \
      -M:notebooks -m nrepl.cmdline --port 7931 \
      > .clerk/dev-logs/nrepl.log 2>&1 < /dev/null &
    # then, on nREPL 7931 (the first bundle compile takes a few minutes):
    (require 'history-of-math.dev) (history-of-math.dev/serve! {})

    # the merge loop, from the worktree root:
    setsid nohup timeout 43200 history-of-math/dev/merge_loop.sh > /dev/null 2>&1 < /dev/null &

Stop: `(history-of-math.dev/halt!)` on 7931, then
`pkill -f 'nrepl.cmdline --port 7931'`; `pkill -f merge_loop.sh`.
Restart: stop, then start. Check it in Chromium with
`node dev/devserve_e2e.mjs http://127.0.0.1:7777 /notebooks/apollonius_conics`
(`LINKS=1` also opens every hub link; `PLAYWRIGHT=<dir with node_modules/playwright>`).
