# From Alexandria to Analysis

An open wiki for learning mathematics from its sources: Clerk notebooks on the
history of mathematics, from Euclid's *Elements* (c. 300 BC) to Lebesgue's
integral (1902). Each notebook takes one work, quotes the original, follows
the author's argument step by step, moves the figure, and checks the claims
with [Emmy](https://github.com/mentat-collective/emmy).

Read it at **https://notebooks.buddhilw.com/**.

## Layout

- `alexandria/`: the mathematics. Shelves (EDN), figures and proofs as Emmy
  expressions, graded checks, and, under `alexandria/media/`, the proof
  players, raster (WebAssembly) kernels and widgets.
- `history-of-math/`: the notebooks. They narrate and mount what Alexandria
  hosts; no mathematics lives here. `history-of-math/README.md` explains how
  a notebook declares itself and how the site is built.

## Build

Needs a JDK (24 or later), the Clojure CLI and Node.js.

```sh
cd history-of-math
npm install
clojure -J-Xmx3g -X:notebooks :out-path '"target/hom"'
```

The site is written to `history-of-math/target/hom/`; serve that directory
with any static file server. Tests:

```sh
cd alexandria && clojure -M:test
cd history-of-math && clojure -M:test
```

## Contributing

To fix a reading, add a proposition or write a new era, open an issue or a
pull request. A notebook is one file in `history-of-math/notebooks/`; its
figures, proofs and graded checks go into `alexandria/`. Quote public-domain
editions of the originals; when you quote a modern translation, name its
translator and edition beside the quote.

`main` takes changes only through pull requests. Every pull request runs the
`build` check (`.github/workflows/site.yml`): all notebooks are built, which
runs every Emmy check, and a headless browser opens each page and fails on
any error or remote script. A merge to `main` publishes the new version to
https://notebooks.buddhilw.com/ within a few minutes.

## Licence

- Code (Alexandria, its players and kernels, the build): GNU General Public
  License, version 3 or later (`LICENSE`), the licence of Emmy, which every
  page bundles.
- Prose, figures and notebooks: Creative Commons Attribution-ShareAlike 4.0
  International (`LICENSE-CONTENT`).
- Quoted texts keep their own status. Ancient and early-modern works are in
  the public domain; modern translations are quoted with attribution.
