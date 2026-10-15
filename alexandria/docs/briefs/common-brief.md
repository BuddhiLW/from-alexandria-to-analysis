# Common brief: history-of-mathematics series (alexandria + history-of-math notebooks)

This is the design spec for the series of Clerk notebooks (and later decks) that run from the ancient
Greeks to modern analysis. Every era is built to the same shape. Each era is developed on its own branch
and reviewed before it is merged.
Paths below are relative to the repository root.

## Step 0: tooling

Read and edit Clojure with structure-aware tools (a code index with form-level read and write), not with
sed or ad-hoc text rewriting. Develop interactively in a REPL started in the era's checkout (the
alexandria dir, alias :test); run a cold `clojure -J-Xmx2g -M:test` only as the final sign-off, in the
background with a leading `timeout 900`. Never give a JVM more than -Xmx2g (shared machine).

## Step 1 — read before writing

- `alexandria/ARCHITECTURE.md` (the rules: curation over desargues/Emmy/raster, no NIH, shelves as data,
  vocabulary not coordinates, Grade ADT, CPPB strata, tests on ports, no with-redefs).
- The design decisions behind the series: alexandria hosts the math and consumers only mount it; proof
  players take proofs as shelf data, draw figures as raster kernels and dispatch scenes through an open
  multimethod (mind the Clerk/edamame version pin); motion is Manim-like (see The Method); notebooks are
  authored with hiccup, Emmy and real TeX; teach the pattern, not repeats; state what the sources show,
  without hedges.
- The worked model to copy the SHAPE of: `alexandria.archimedes.circle`, `alexandria.archimedes.method`,
  `alexandria.proofs`, `resources/alexandria/archimedes/*.edn`, `media/src/alexandria/archimedes/*_scenes.cljc`
  and `*_view.clj`, `media/src/alexandria/medium/*` (timeline, anim, figure, scene, player, clerk),
  and tests under `test/alexandria/archimedes/`. The Archimedes notebook
  `history-of-math/notebooks/archimedes.clj`
  shows how a notebook narrates and mounts a viewer; the `deps.edn` `:notebooks` alias of `history-of-math` shows the
  classpath (Clerk 0.18.1158, alexandria-media local root, edamame 1.4.28 pin, hive-dsl 0.5.34).

Before writing any function: search desargues, Emmy (mentat-collective/emmy), raster and
alexandria for it. Extend by registration (defmethod, extend-protocol), never fork.

## Step 2 — deliverables for your era (each one required)

1. **Shelf data**: `alexandria/resources/alexandria/<author>/<work>.edn` (propositions with author, work,
   number, statement in a public-domain English translation, source and page) and ONE appended entry
   in `resources/alexandria/catalogue.edn` (the only shared file you may edit; append, never reorder).
2. **Proof steps**: `<work>.proofs.edn` — `{:claim :why :stage}` steps; quotations verbatim from the
   public-domain translation you cite (archive.org / Gutenberg / Wikisource), with the source named.
3. **Core namespace(s)** `alexandria.<author>.<topic>` in `alexandria/src`: figures as Emmy functions,
   claims proved by Emmy (`simplify` to 0, graded `:grade/proved` via `alexandria.grade`), exact
   rationals where the author computed numbers, `:grade/numeric` only where no closed form exists.
   Shelf checks compose vocabulary operations (`alexandria.vocab` realize + `alexandria.notation`
   render); add missing operations there by registration. The shelf gate test must stay green.
4. **Media**: scenes `media/src/alexandria/<author>/<topic>_scenes.cljc` (defmethods on the scene
   multimethod, Manim-like motion via `alexandria.medium.anim`) and a viewer ns `<topic>_view.clj`
   mounting a player, like the Archimedes ones. Figures driven by raster kernels where they move.
5. **Tests** in `alexandria/test/alexandria/<author>/` (and media tests where the Archimedes ones have
   them): every proposition grades as expected; figure properties; no with-redefs; depend on ports.
6. **Notebook**: `history-of-math/notebooks/<id>.clj` (`history-of-math/deps.edn` and its build are
   maintained separately and are not part of an era's deliverables).
   The ns form carries metadata `{:history/year <int, the work's date> :history/title "<title>"
   :history/era "<era>" :nextjournal.clerk/toc true}`. The notebook only narrates and mounts alexandria
   viewers: plain English, short sentences, every notation defined at first use, period context (who,
   where, what problem, what came before and after — connect to the neighbouring eras of the series),
   state what the sources show without defensive hedges. Each proposition: statement, the author's
   argument as steps, the moving figure, the graded checks as badges.
   Verify the notebook builds: `nextjournal.clerk/build!` it to a scratch dir under the checkout's
   `target/` with a classpath like the `:notebooks` alias of `history-of-math/deps.edn` (a temporary -Sdeps is fine).

## Step 3 — finish

- Commit on the era's branch in small logical commits, explicit paths staged (never `git commit -a`).
  Messages: conventional style, technical content only.
- Record non-obvious findings (gotchas, design decisions) in the project notes.
- Report: branch, commit list, what is graded proved vs numeric, test counts from the cold run,
  anything left undone and why.
- If blocked on a decision that is not the era's to make, raise it with the maintainers; otherwise make
  the reasonable choice, note it, and continue.
