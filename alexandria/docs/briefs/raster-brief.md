# Raster brief: the calculus and the animations run on raster

Pedro, 2026-10-05: "Make sure the calculus is done using raster backend!" and "Animations also using this
backend. as we did latest emmy updates PRs on this direction". Hive memory: the feedback entry tagged
`raster` `feedback-derived` of 2026-10-05, and decisions 20261004002657-3dce3df7 (emmy-viewers
computational-backend seam: `emmy.viewer.compile/compiled-fn*` on `*backend* :raster`, PR
mentat-collective/emmy-viewers#83, fork sha 3f5b6b1 which alexandria/media already pins),
20261004060950-20c508f1 (alexandria figures as raster kernels: KernelFigure vs FnFigure) and principle
20261004085255-241845ea (raster wasm libm error; keep big factors out of trig). Read them first.

## The rule

- Emmy is the SYMBOLIC layer only: identities, simplify to 0, derivatives as expressions, Lagrangians.
  Those stay as they are and keep grading :grade/proved.
- Every NUMBER the notebooks show or grade comes from raster: numeric integration and ODEs, root
  finding, quadrature and Riemann/Cauchy sums, partial sums of series, difference quotients,
  epsilon-delta searches, any sampled curve.
  Forbidden for numbers: emmy.numerical.* (bisection, quadrature, ODE solvers), hand loops over
  Math/sin etc., `double` arithmetic in Clojure loops standing in for a computation.
  Find raster's own facilities first with carto (raster at $HOME/PP/replikativ/raster and its gitlib
  copy; desargues' TrajectorySolver port / desargues.board.kernel is the existing ODE route; alexandria
  media's medium.kernel and medium.figure compile Emmy functions to raster kernels). Extend by
  registration; never NIH a solver that raster or desargues has.
- Every ANIMATION is a raster kernel: each moving figure is a KernelFigure (wasm batch per frame), or
  goes through emmy-viewers' compiled-fn* with *backend* :raster. A figure drawn by a cljs function
  doing its own arithmetic per frame, or by Emmy's :js compile, is converted. FnFigure stays only as the
  JVM test oracle: add a test per converted figure that the kernel matches the Emmy oracle within
  raster's libm tolerance (1e-4, or tighter where no trig).
- Numeric grades (:grade/numeric) must record that the number came from raster.

## Tools (obligatory)
Bootstrap: ToolSearch "select:mcp__hive__project,mcp__hive__code,mcp__hive__memory,mcp__hive__bash,mcp__hive__git,mcp__hive__clojure_eval,mcp__hive__swarm",
then `mcp__hive__project command="workflow catchup" directory="<your worktree>"`.
carto for every read, search and edit of Clojure (carto search / multi-search / callers / refs /
read-form / read-file mode raw / write-form / insert-form / raw-splice / ns-edit); mcp__hive__git for
status, diff, stage and commit (explicit files; no AI attribution in messages); mcp__hive__bash only for
running builds and tests (backgrounded with a leading timeout, -J-Xmx2g, never more);
mcp__hive__clojure_eval against your own nREPL for the loop. Never sed/python/cat on sources. Do not
reproduce long passages in one output (an earlier ling was killed by the content filter).

## Audit first
Start with a carto-driven inventory of every numeric computation and every moving figure in your
namespaces: list each as {ns/var, what it computes, current engine (raster | emmy.numerical | Math loop |
js), target}. Put the inventory in your final reply. Then convert.

## Done means
- alexandria core and media suites green cold (media deps pins may be missing until hom-polish lands;
  add -Sdeps pins for hive-dsl 0.5.34 and edamame 1.4.28 on the command line if needed).
- `history-of-math` build (clojure -J-Xmx2g -X:notebooks :out-path '"target/hom"') green for your
  notebooks with no FAILS badge, and the numbers shown unchanged within tolerance.
- Small commits per namespace. Final reply: the inventory table, what changed, test counts.
