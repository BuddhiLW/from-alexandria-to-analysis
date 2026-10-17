# Common brief: history-of-mathematics series (alexandria + history-of-math notebooks)

You are one ling in a swarm building a series of Clerk notebooks (and later plato decks) that run from
the ancient Greeks to modern analysis. The coordinator reviews and merges; you write. Work ONLY in your
own worktree (given in your task). Do not merge, do not push, do not touch other worktrees.
Paths below: $REPO = $HOME/PP/presentations.

## Step 0 — bootstrap (do this first, exactly)

1. ToolSearch query: "select:mcp__hive__project,mcp__hive__code,mcp__hive__memory,mcp__hive__bash,mcp__hive__git,mcp__hive__clojure_eval,mcp__hive__swarm"
2. mcp__hive__project command="workflow catchup" directory="<your worktree>"
3. Scan carto for your scope: mcp__hive__code command="carto auto" scope="<your worktree>/alexandria" and use carto
   (search / multi-search / definition / callers / read-form / read-file mode raw) to read code.

Hive tools are obligatory: carto for reading and editing Clojure (create-file, write-form, insert-form,
raw-splice, ns-edit), hive memory for recall, mcp__hive__bash for shell. Never use sed/python/cat
heredocs on .clj files. Develop interactively: spawn your own nREPL in your worktree
(mcp__hive__code command="cider spawn" with your worktree's alexandria dir, alias :test) and verify with
mcp__hive__clojure_eval; a cold `clojure -J-Xmx2g -M:test` only as the final sign-off, run in the
background with a leading `timeout 900`. Never give a JVM more than -Xmx2g (shared machine).

## Step 1 — read before writing

- `alexandria/ARCHITECTURE.md` (the rules: curation over desargues/Emmy/raster, no NIH, shelves as data,
  vocabulary not coordinates, Grade ADT, CPPB strata, tests on ports, no with-redefs).
- hive memory: `memory batch-get` ids 20261004053849-798662ed (alexandria hosts the math; consumers only
  mount), 20261004060950-20c508f1 (proof players: proofs as shelf data, figures as raster kernels,
  scenes as open multimethod; Clerk/edamame gotchas), 20261004071106-5799f6e4 (Manim-like motion,
  The Method), 20260924234250-2b7b2676 (authoring: hiccup, Emmy, real TeX), 20261003211311-391deee3
  (teach the pattern, not repeats), 20261005010805-62f8b432 (state what sources show; no hedges).
- The worked model to copy the SHAPE of: `alexandria.archimedes.circle`, `alexandria.archimedes.method`,
  `alexandria.proofs`, `resources/alexandria/archimedes/*.edn`, `media/src/alexandria/archimedes/*_scenes.cljc`
  and `*_view.clj`, `media/src/alexandria/medium/*` (timeline, anim, figure, scene, player, clerk),
  and tests under `test/alexandria/archimedes/`. The set-theory notebook
  `$REPO/.claude/worktrees/set-theory-widgets/set-theory/notebooks/archimedes_parabola.clj`
  shows how a notebook narrates and mounts a viewer; its `deps.edn` `:notebooks` alias shows the
  classpath (Clerk 0.18.1158, alexandria-media local root, edamame 1.4.28 pin, hive-dsl 0.5.34).

Before writing any function: carto-search desargues, Emmy ($HOME/PP/mentat-collective/emmy), raster and
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
6. **Notebook**: `history-of-math/notebooks/<id>.clj` (create the directory if absent; another ling is
   scaffolding `history-of-math/deps.edn` and its build in parallel — do NOT create those files).
   The ns form carries metadata `{:history/year <int, the work's date> :history/title "<title>"
   :history/era "<era>" :nextjournal.clerk/toc true}`. The notebook only narrates and mounts alexandria
   viewers: plain English, short sentences, every notation defined at first use, period context (who,
   where, what problem, what came before and after — connect to the neighbouring eras of the series),
   state what the sources show without defensive hedges. Each proposition: statement, the author's
   argument as steps, the moving figure, the graded checks as badges.
   Verify the notebook builds: `nextjournal.clerk/build!` it to a scratch dir under your worktree's
   `target/` with a classpath like set-theory's `:notebooks` alias (a temporary -Sdeps is fine).

## Step 3 — finish

- Commit on your branch in small logical commits, explicit paths staged (never `git commit -a`).
  Messages: conventional style, technical content only. NO Co-Authored-By, NO session URLs, NO AI
  attribution of any kind.
- Persist non-obvious findings (gotchas, design decisions) to hive memory with tags
  `alexandria`, `history-of-math`, `scope:project:presentations`.
- Final reply: branch, commit list, what is graded proved vs numeric, test counts from the cold run,
  anything left undone and why.
- If blocked on a decision only the coordinator can make, ask via hivemind (swarm `hivemind ask`);
  otherwise make the reasonable choice, note it, and continue.
