# Alexandria: the curated library of historical mathematics

Alexandria collects the propositions, constructions, and equations of the historical texts. Euclid, Archimedes, Eudoxus, Apollonius, Euler, Gauss, Riemann, and others are each a shelf. The decks and the Clerk notebooks import these objects instead of keeping their own copies.

It is a CURATION layer. The computing, the drawing, and the numerics already exist, and Alexandria uses them:

| Need | Already in | Where |
|---|---|---|
| Symbolic algebra, simplification, proof by `simplify` to 0 | Emmy | `~/PP/mentat-collective/emmy` (`emmy.env`, `emmy.generic`, `emmy.simplify`) |
| Vectors, cross/dot, matrices, quaternions | Emmy | `emmy.structure`, `emmy.matrix`, `emmy.quaternion` |
| Manifolds, coordinate systems (R2, S2...), metrics, connections, curvature, geodesic equations | Emmy (FDG) | `emmy.calculus.*`; the book: `~/PP/mentat-collective/fdg-book` |
| ODEs, roots, quadrature, minimization | Emmy | `emmy.numerical.*` |
| Constructions as data: free, on, meet, mid, map, expr; checks; draw layers; draggable boards | desargues | `desargues.board.construction`, `.euclid`, `.projective`, `.symmetry`, `.groups`, `.shapes`, `.view` |
| Projective geometry: join, meet, cross-ratio, homographies, conics, polar, proofs | desargues | `desargues.geometry.projective` |
| TeX | Emmy and desargues | `emmy.expression.render` (`->TeX`), `desargues.tex` |
| Numerics: compiled kernels, the number backend | raster | `~/PP/replikativ/raster`; desargues boards already compile to it (`desargues.board.kernel`) |
| Media: Mafs, MathBox, JSXGraph, Leva in Clerk | emmy-viewers and wrappers | `~/PP/mentat-collective/emmy-viewers`, `Mafs.cljs`, `MathBox.cljs`, `JSXGraph.cljs`, `Leva.cljs`, `Clerk-Utils` |
| Slides | plato | the decks' `deps.edn` |

## The rule: never reinvent (no NIH)

Before writing a function, search the stack with carto. Every repository listed above is indexed. Use:
- `mcp__hive__code command="carto search"` / `carto_multi_search` with `scopes` set to the mentat-collective paths;
- `carto ns` and `carto read-form`.

If the function exists, use it. If it nearly exists, extend it by registration: a `defmethod` on desargues' open multimethods (`point`, `check`, `draw-layer`, `expand-point`, `plan`), or `extend-protocol`. Never fork the code and never edit it in place. Write new code only when the search comes back empty, and say so in the namespace docstring: name what you searched for and what you found.

## What Alexandria adds

1. **Shelves, as data.** Each tradition is an EDN resource of propositions written as desargues `:construction` boards, with metadata: author, work, proposition number, Heath's or the critical edition's statement, and source page. `resources/alexandria/catalogue.edn` lists the shelves. Adding a proposition means adding a map; adding a tradition means adding a file and one catalogue entry. No code changes either way.
2. **The operations Euclid needs that desargues lacks**, registered as `defmethod`s on `desargues.board.construction/point`. Candidates: circle meets circle (Post. 3, I.1), line meets circle, producing a line (Post. 2). Each is an Emmy formula so the board compiles through raster.
3. **Contexts (surfaces).** The same construction data runs on other surfaces:
   - the sphere and elliptic geometry, on Emmy's S2 manifold with geodesics as great circles;
   - the hyperbolic plane, through the Klein model as a projective Cayley–Klein geometry (desargues' projective layer), plus the Poincaré disk and upper half-plane;
   - flat surfaces with global wrap (cylinder, cone);
   - general parametric surfaces, via Emmy's metric, Christoffel symbols and geodesic ODE.
   A context only reinterprets the construction ops (line = geodesic, circle = geodesic-distance locus). Gauss–Bonnet is the check valid in every context; take the curvature from Emmy.
4. **Proof grades.** For each proposition's checks:
   - `simplify` reduces a symbolic difference to 0: proved;
   - the closed-form numeric value matches;
   - only an ODE or numeric solution matches;
   - the check fails.
   The grade is a closed set, so it is written as a hive-dsl `defadt`.
5. **A vocabulary, not coordinate algebra.** Shelves speak in abstract operations: `(area A V B)`, `(length A B)`, `(angle B A C)`, `(segment-area P A B)`, `(hyperbola a b)`, `(series-sum r n)`. Each name is an Emmy literal function inside an Emmy symbolic expression; it is never expanded by hand.
   - **Meaning** (`alexandria.vocab`): `realize` is open on the operation and turns a vocabulary expression into an Emmy value from a board's points. It is registered as the desargues check `:claim`, so boards still compile to raster.
   - **Notation** (`alexandria.notation`): `render` dispatches on `[notation op]`. Modern TeX is one notation; Euclid's verbal style ("the square on AB"), Archimedes' ratios, Descartes and Leibniz are others. It extends the TeX renderer the Emmy fork already exposes (the one `desargues.logic.tex` builds on) instead of writing a new one.
   - **Frozen formulas:** `H1 := (hyperbola a b)` with bindings `{a 1, b 0}` and a passage (author, work, locus) is a value. It is frozen with Emmy, quoted as a historical object, and rendered in its author's notation or any other.
   - **The gate:** a test fails the build when a shelf check reaches into coordinates (`(x P)`, `(y P)`, a hand-expanded shoelace or distance formula). A check may only compose vocabulary operations. An operation the vocabulary lacks is added as a `realize` method plus a `render` method, never inlined.

## Principles (house canon, applied)

- **Stratified design / CPPB:**
  - Collect: the shelves (EDN, validated at entry with malli).
  - Promote: the desargues construction in a context.
  - Pipeline: checks and grades.
  - Boundary: the medium (plato board, Clerk viewer via emmy-viewers, TeX, SVG).
  Effects happen only at the boundary.
- **Open sets extend by registration** (multimethod or protocol); closed sets are ADTs. Cardinality decides which.
- **Railway:** an operation that can fail returns a hive-dsl Result. Never nil-punning, and never drop a step silently.
- **Tests:**
  - Every context passes one shared contract suite: hive-test properties with test.check generators.
  - Every shelf validates.
  - Every check in every proposition grades as expected.
  - Tests depend on ports, not concretions; no `with-redefs`.
- **Tools:** use hive tools only: carto for reading and editing Clojure, hive memory for recall. Never shell reads or writes of source files.

## Layout

```
alexandria/
  deps.edn                      desargues, emmy, raster by git sha (local.deps.edn may point at ~/PP worktrees)
  resources/alexandria/         catalogue.edn, one EDN per tradition
  src/alexandria/ops/           defmethods registered into desargues (euclid circle ops, ...)
  src/alexandria/ctx/           contexts: sphere, hyperbolic, flat-wrapped, parametric
  src/alexandria/library.cljc   Collect: catalogue -> validated propositions
  src/alexandria/grade.cljc     Pipeline: the Grade ADT and grading
  notebooks/                    Clerk pages built from the shelves (emmy-viewers)
  test/alexandria/              contract suite, shelf validation, per-proposition grades
```
