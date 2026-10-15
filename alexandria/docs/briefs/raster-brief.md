# Raster brief: the calculus and the animations run on raster

Maintainer direction, 2026-10-05: "Make sure the calculus is done using raster backend!" and "Animations
also using this backend. as we did latest emmy updates PRs on this direction". Background: the
emmy-viewers computational-backend seam (`emmy.viewer.compile/compiled-fn*` on `*backend* :raster`,
mentat-collective/emmy-viewers#83; alexandria/media pins the fork sha, see `media/deps.edn`);
alexandria figures as raster kernels (KernelFigure vs FnFigure); and the raster wasm libm error (keep
big factors out of trig).

## The rule

- Emmy is the SYMBOLIC layer only: identities, simplify to 0, derivatives as expressions, Lagrangians.
  Those stay as they are and keep grading :grade/proved.
- Every NUMBER the notebooks show or grade comes from raster: numeric integration and ODEs, root
  finding, quadrature and Riemann/Cauchy sums, partial sums of series, difference quotients,
  epsilon-delta searches, any sampled curve.
  Forbidden for numbers: emmy.numerical.* (bisection, quadrature, ODE solvers), hand loops over
  Math/sin etc., `double` arithmetic in Clojure loops standing in for a computation.
  Find raster's own facilities first with a code index (the replikativ/raster sources; desargues' TrajectorySolver port / desargues.board.kernel is the existing ODE route; alexandria
  media's medium.kernel and medium.figure compile Emmy functions to raster kernels). Extend by
  registration; never NIH a solver that raster or desargues has.
- Every ANIMATION is a raster kernel: each moving figure is a KernelFigure (wasm batch per frame), or
  goes through emmy-viewers' compiled-fn* with *backend* :raster. A figure drawn by a cljs function
  doing its own arithmetic per frame, or by Emmy's :js compile, is converted. FnFigure stays only as the
  JVM test oracle: add a test per converted figure that the kernel matches the Emmy oracle within
  raster's libm tolerance (1e-4, or tighter where no trig).
- Numeric grades (:grade/numeric) must record that the number came from raster.

## Tools
Read, search and edit Clojure with a structure-aware code index (search, callers, refs, form-level read
and write), not with sed or ad-hoc scripts. Run builds and tests in the background with a leading
timeout and -J-Xmx2g, never more. Use a REPL started in the checkout for the inner loop. Do not
reproduce long passages of a book in one output.

## Audit first
Start with an inventory of every numeric computation and every moving figure in the era's namespaces:
list each as {ns/var, what it computes, current engine (raster | emmy.numerical | Math loop | js),
target}. Put the inventory in the final report. Then convert.

## Done means
- alexandria core and media suites green cold (add -Sdeps pins for hive-dsl 0.5.34 and edamame 1.4.28
  on the command line if the media deps lack them).
- `history-of-math` build (clojure -J-Xmx2g -X:notebooks :out-path '"target/hom"') green for the era's
  notebooks with no FAILS badge, and the numbers shown unchanged within tolerance.
- Small commits per namespace. Final report: the inventory table, what changed, test counts.
