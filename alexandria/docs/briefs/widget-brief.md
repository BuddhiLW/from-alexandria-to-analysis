# Widget brief: notebooks in the Road to Reality style, on the raster backend

Pedro, 2026-10-05: "We should write using widgets like in fdg-book, roadtoreality.substack.com,
sicm-book, emmy-viewers ... We can control equation of hyperbola, ellipse, and parabola etc." Then:
"use our local version [of emmy-viewers, PR #83], raster backend".

Model: mentat-collective/road-to-reality (a Clerk book with Leva panels driving Mafs and MathBox).
fdg-book is org-mode prose with no widgets; use it only for its prose and TeX style.

## 1. Dependency: our local emmy-viewers, raster backend

- emmy-viewers comes from the local worktree of PR #83 (branch feat/raster-backend, sha 3f5b6b1 or
  later), not the GitHub fork pin. Every alexandria branch lives at the same depth, so the path is
  relative from `<worktree>/alexandria/media`:
  `{:local/root "../../../../../../mentat-collective/emmy-viewers/.claude/worktrees/raster-backend"
    :exclusions [org.mentat/emmy]}`.
- Every compiled plot goes through `emmy.viewer.compile/*backend*` bound to `:raster` (the Kernel
  contract: `compiled-fn*`, `emmy.viewer.kernel`). Mafs `of-x` / `parametric` and MathBox
  `of-xy` / `parametric-surface` then batch through wasm kernels. The `:js` fallback is for no
  WebAssembly only.

## 2. The widget pattern (from emmy-viewers/dev/examples)

```clojure
(:require [emmy.clerk :as ec] [emmy.env :as e :refer :all] [emmy.leva :as leva]
          [emmy.mafs :as mafs] [emmy.mathbox.plot :as plot] [emmy.viewer :as ev]
          [nextjournal.clerk :as clerk])
^{::clerk/visibility {:code :hide :result :hide}} (ec/install!)

(ev/with-let [!p {:l 2 :e 0.6}]                       ; state lives in the browser
  [:<>
   (leva/controls {:atom !p :schema {:l {:min 0.5 :max 4 :step 0.01}
                                     :e {:min 0 :max 2 :step 0.01}}})
   (mafs/mafs {:view-box {:x [-8 8] :y [-6 6]}}
     (mafs/cartesian)
     (mafs/parametric
       {:xy (ev/with-params {:atom !p :params [:l :e]}
              (fn [l e] (fn [t] (let [r (/ l (+ 1 (* e (cos t))))]
                                  [(* r (cos t)) (* r (sin t))]))))
        :t [(- 0.01 Math/PI) (- Math/PI 0.01)]}))])
```

- `ev/with-params` re-closes the parameters on each atom change. It never recompiles.
- `:params` holds map keys (a Leva atom) or vector indices (a movable-point atom).
- `ev/get` reads one key in a plain option.
- 3D: `plot/scene` + `leva/controls` + `plot/parametric-surface {:f (ev/with-params ...)}`. The orbit
  camera is on by default (drag rotates, scroll zooms, right-drag pans); z is up.
- Focus-directrix constructions with draggable points: `emmy.jsxgraph` (`ellipse`, `hyperbola`,
  `parabola`). Mafs `movable-point` with `:constrain` handles a dragged focus.
- Prose stays `;;` Markdown with `$...$` TeX. `ec/->TeX` renders a derived formula. Use
  `{::clerk/visibility {:code :fold}}` for a show-code toggle.

## 3. Static build: ship the bundle ourselves

`emmy.clerk/build!` defaults to a prebuilt bundle on cas.clerk.garden, frozen at an old
emmy-viewers revision. That is wrong for our local raster backend. Follow road-to-reality instead:
- `mentat.clerk-utils.build/build!` with `:cljs-namespaces '[history-of-math.sci-extensions]`
  (a cljs ns that calls `(emmy-viewers.sci/install!)`, plus alexandria.medium.* via `sci/copy-ns`).
  shadow-cljs release-compiles it, and the bundle lands in the build output.
- A `package.json` with the JS deps of road-to-reality (leva 0.9.34, mafs 0.15.2, mathbox 2.3.1,
  three 0.151.3, react 18.2.0 ...).
- CSS via `css/set-css!` (mafs core.css and font.css, mathbox.css, jsxgraph.css); without it the
  widgets look broken.
- This replaces the per-notebook `emmy.clerk/install!` bundle swap of decision 20261005201447. One
  bundle serves every notebook, and our own player and scenes ride in it.

## 4. Definitions and notation are drawn step by step

Pedro, 2026-10-05, on the Apollonius "Notation, fixed once" passage (cone, double cone, axial
triangle ABC, diameter PM, ordinate QV = y, abscissa PV = x, parameter p, transverse diameter PP'):
"We should improve illustrating passages like [this] step by step. The drawings should show that."
Rule for every notebook: a passage that introduces objects or notation gets ONE figure stepped
sentence by sentence. The current sentence is highlighted beside the drawing, and each step adds
exactly what its sentence introduces, with the author's letters, Manim-style (Create, FadeIn), points
from raster kernels, in 3D the reader can orbit when the object is solid. A static paragraph followed
by one finished figure is not enough. Model: alexandria.apollonius.notation-steps.

## 5. Tied controls: equations, editable from every side

Pedro, 2026-10-05: "The widget values can be equationally tied. But we should be able to change both
(it just will also change the other, necessarily. it's tied equations".
Rule: when quantities are linked by equations, every one of them is a live control. Moving any
control re-solves the others through the equations and inverses, and pushes them back into the
panel. Each tie names its policy: which quantity is held fixed when another moves. Only the
user-touched key is a source. A pushed value never re-fires the solver (guard on origin and an
epsilon). Forward and inverse maps are raster kernels, and Emmy proves they compose to the identity.

Model, the conics playground (polar coordinates as a projection): the section of the cone of
half-angle alpha by the plane z = h + x tan(tilt), projected straight down onto the base, is
rho = l / (1 - e cos theta) with focus on the axis, where l = h tan(alpha) and
e = tan(tilt) tan(alpha); inverses h = l / tan(alpha) and tilt = atan(e / tan(alpha)).
Policy: alpha is held fixed when e or l moves, and the plane (tilt, h) is held fixed when alpha moves.

Show and solve the equations symbolically with Emmy (Pedro, 2026-10-05). The notebook derives the
tie on the page, as TeX via emmy.clerk/->TeX:
- cone z = rho cot(alpha), plane z = h + rho cos(theta) tan(tilt);
- eliminate z, solve the linear equation for rho with Emmy (its linear solver or a polynomial
  division, not a hand-written formula), simplify to l / (1 - e cos theta), and read off l and e;
- solve back for h and tilt;
- grade forward-then-inverse = identity as :grade/proved.
Beside the sliders, the same formula appears with the current values substituted. It updates
live, and the values come from the raster kernels.

## 6. A live REPL even while hive-mcp is down

Measured 2026-10-05 23:40, with hive-mcp's nREPL (7910) down: `mcp__hive__clojure_eval` works
against an agent's OWN nREPL when given an explicit `port`. It needs only the hive MCP connection,
not the hive JVM. `cider spawn` does need the hive JVM, so do not use it while hive is down.
1. Start the nREPL from Bash in your worktree, with the aliases you need, backgrounded under a
   timeout:
   `timeout 3600 clojure -J-Xmx2g -Sdeps '{:deps {nrepl/nrepl {:mvn/version "1.3.0"}}}' -M:<aliases> -m nrepl.cmdline --port <free port 7960-7999>`.
2. Load the tool with ToolSearch "select:mcp__hive__clojure_eval" and call it with that `port`.
   Check `(System/getProperty "user.dir")` first.
3. Use it for the inner loop: require, reload, run one test ns, inspect a compiled widget form.
   Keep cold `clojure -M:test` runs for the final sign-off.
4. Use no other hive tool (memory, swarm, git, code) unless the user allows it. Stop your nREPL
   when done.

## 7. What every era notebook gains

Our proof player (steps of the author's argument, Manim-style motion) stays for the argument. Next
to it, an exploration widget lets the reader drive the author's object:
- Apollonius: a conics playground. l and e sliders on r = l/(1 + e cos t), with ellipse (e < 1),
  parabola (e = 1) and hyperbola (e > 1) named live. The application-of-areas panel reads the same
  atom. The 3D cone + plane runs in MathBox with tilt and height sliders.
- Doubling the cube: Menaechmus' two parabolas with an `a` slider, and their meeting point; Archytas'
  three surfaces in MathBox.
- Descartes-Fermat: a movable point on a curve with its normal by the double root, and the
  adequality tangent.
- Kepler and Newton: e and a sliders on the orbit, with the area swept per time step.
- Euler: the partial sums of sum 1/n^2 against pi^2/6 with an n slider; polyhedra in MathBox.
- Cauchy: an epsilon slider for the delta search; the Abel n slider.
Every number on screen still comes from raster: plots through the raster backend, values through
alexandria.raster kernels.
