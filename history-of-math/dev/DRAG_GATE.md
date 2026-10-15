# Mafs drag gate: results

`node dev/mafs_drag_e2e.mjs <url> <shots-dir> <min-handles>` mouse-drags every
Mafs handle (`.mafs-movable-point`) of a served page and checks that the point
moved, the view did not pan (unless the readout changed with it: a view-box
computed from the state) and either a readout changed or the figure was
redrawn (a layout handle). Run with `PLAYWRIGHT=<dir with
node_modules/playwright>` and, if the bundled browser is missing,
`CHROMIUM=<chrome-headless-shell>`.

"Bad before" is the first run on the branch head (6669b1c); "bad after" is the
run after the fixes on alex/drag-gate-rest. Every page: 0 page errors and no
remote scripts. N/A: the page has no draggable Mafs handle (Leva sliders,
MathBox or plain points only), so there is nothing for the gate to drag.

| notebook | handles | bad before | bad after | result | fix | date |
|---|---|---|---|---|---|---|
| apollonius-conics | 1 | 0 | 0 | pass | | 2026-10-12 |
| conics-playground | 0 | – | – | N/A (Leva only) | | 2026-10-12 |
| descartes-fermat | 3 | 0 | 0 | pass | | 2026-10-12 |
| leibniz-nova-methodus | 0 → 4 | 3 playgrounds not mounted | 0 | pass | 9408344 | 2026-10-12 |
| newton-principia | 0 | – | – | N/A (playgrounds have no handles) | shares 9408344 | 2026-10-12 |
| lagrange | build failed, then 5 | 1 (lever l₂) | 0 | pass | 3312c4f | 2026-10-12 |
| cauchy | 2 | 0 | 0 | pass | | 2026-10-12 |
| riemann-weierstrass | 0 | – | – | N/A (no Mafs) | | 2026-10-12 |
| dedekind-cantor | 0 | – | – | N/A (no Mafs) | | 2026-10-12 |
| kepler-mysterium | 6 → 3 | 3 (inner planets) | 0 | pass | b24eda3 | 2026-10-12 |
| eudoxus | 2 | 0 | 0 | pass | | 2026-10-12 |
| bernoulli-brachistochrone | 0 | – | – | N/A (race points not draggable) | | 2026-10-12 |
| euler | 14 | 12 by the first gate | 0 | pass (12 layout handles) | gate 6a6f6d2 | 2026-10-12 |
| gauss | 2 | 0 | 0 | pass | | 2026-10-12 |
| oresme | 10 | 2 by the first gate | 0 | pass (views rescale) | gate 6a6f6d2 | 2026-10-12 |
| copernicus | 9 | 1 (Mercury) | 0 | pass | 087c177 | 2026-10-12 |
| ptolemy-almagest | 7 | 0 | 0 | pass | | 2026-10-12 |
| riemann-hypothesen | 3 | 0 | 0 | pass | | 2026-10-12 |
| non-euclidean | 4 | 0 | 0 | pass | | 2026-10-12 |

Euler's Königsberg regions and V−E+F vertices move the drawing but change no
number by design. Oresme's mean and series views compute their view-box from
the state. Neither was a bug: the gate (6a6f6d2) now accepts both and reports
them.

Root causes fixed:

- **Leibniz**: a bare lazy seq of buttons in hiccup (gotcha 2), `ev/get` quoted
  into the browser, and handles on a scalar cursor.
- **Lagrange**: `draggable` was used before it was defined, and a weight handle
  lay inside the l₂ hitbox.
- **Mysterium**: overlapping hitboxes on one diagonal.
- **Copernicus**: a handle that went past its clamp, leaving its orbit.
