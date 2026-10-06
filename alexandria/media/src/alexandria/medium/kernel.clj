(ns alexandria.medium.kernel
  "Figures as browser kernels. A figure is {:f :params :state}: f an Emmy
   function (fn [& params] (fn [state] [x y])), params and state initial
   values. `kernel` compiles it through emmy-viewers' Kernel contract with
   the :raster backend (WebAssembly) and returns plain data a player turns
   back into a function (alexandria.medium.player/revive):

     {:glue     JS source of a function of fb that returns the kernel
      :fallback [arg-names... body], Emmy's :js compile of f, fb}

   The revived kernel f has f(state, ps) -> [x y] and
   f.batch(xs, n, ps, out) -> Float64Array of n points.

   Solutions of equations in a scene (an ODE run, a root of Kepler's
   equation, a quadrature) are NOT browser kernels: alexandria.raster
   computes them once on the JVM (ode / root / integral / sample, raster's
   solvers on Emmy-lowered kernels) and they ship as ctx :data; the scene
   draws them through a figure kernel. Examples:
   alexandria.lagrange.mechanics-scenes (:double, :swing, :phis through
   :bobs and :conic) and alexandria.newton.principia-scenes (:thetas
   through :orbit). A scene does no arithmetic of its own on coordinates.

   Writing a figure for raster's polynomial libm (sin/cos ~5e-6, inverse
   trig ~3e-5): keep big factors out of trig, and keep acos/asin/sqrt
   arguments off their domain edges. acos(1) is NaN in raster's wasm, so
   use atan2 of a sum of squares (alexandria.lagrange.variations/bowl-figure).
   Test every figure with alexandria.medium.kernel-oracle/deviation (media
   test tree): it runs the wasm in node against the FnFigure oracle and
   reports NaN states."
  (:require [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(defn kernel
  "The figure compiled by `backend` (default :raster), as {:glue :fallback}.
   The figure's :opts go to the compiler (e.g. {:simplify? false} for an
   expression Emmy's simplifier does not help)."
  ([figure] (kernel figure :raster))
  ([{:keys [f params state opts]} backend]
   (let [[[_ _ glue] [_ & fallback]] (binding [vc/*backend* backend]
                                       (vc/compiled-fn f params state (or opts {})))]
     {:glue glue :fallback (vec fallback)})))

(def kernels
  "{name figure} -> {name kernel}, memoized on the figures map."
  (memoize (fn [figures] (update-vals figures kernel))))
