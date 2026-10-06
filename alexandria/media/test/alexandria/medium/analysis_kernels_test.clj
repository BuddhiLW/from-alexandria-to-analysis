(ns alexandria.medium.analysis-kernels-test
  "Every moving figure of the analysis eras (Leibniz, Euler, Cauchy,
   Riemann, Weierstrass, Lebesgue) as its raster wasm kernel, run in node,
   against its Emmy oracle (FnFigure) on the JVM, within raster's libm
   tolerance 1e-4. Skipped without node.

   Modules over 4 KB (emmy.viewer.raster.glue/sync-limit: the 48-term Abel
   sum, Riemann's graph, the Weierstrass figures) compile asynchronously;
   alexandria.medium.kernel-oracle/deviation waits for f.ready()."
  (:require [alexandria.cauchy.analysis :as cauchy]
            [alexandria.euler.series :as euler]
            [alexandria.lebesgue.measure :as lebesgue]
            [alexandria.leibniz.calculus :as leibniz]
            [alexandria.medium.kernel-oracle :as ko]
            [alexandria.riemann.integral :as riemann]
            [alexandria.weierstrass.monster :as weierstrass]
            [clojure.test :refer [deftest is testing]]))

(def ^:private cases
  "[label figure params states]. Riemann's (x) = atan(tan(pi k x))/pi is
   probed off its jumps."
  [["leibniz triangle" (:triangle leibniz/figures) [0.4 0.3] [[0 0] [1 0] [1 1] [0.5 0.25]]]
   ["leibniz tangent" (:tangent leibniz/figures) [0.4] [[0] [0.4] [1.2]]]
   ["leibniz transmute" (:transmute leibniz/figures) [8 0.6] [[0 0] [3 1] [5 2] [7 3]]]
   ["euler spiral" (:spiral euler/figures) [Math/PI] [[0] [1.5] [4] [9] [16]]]
   ["euler product" (:product euler/figures) [6.5] [[0.5] [2.0] [5.0] [9.0]]]
   ["cauchy abel" (:abel cauchy/figures) [12.5] [[0.3] [1.7] [3.0] [-2.2]]]
   ["riemann graph" (:graph riemann/figures) [7.5] [[0.13] [0.31] [0.62] [0.87]]]
   ["weierstrass geometric" (:geometric weierstrass/figures) [9.3] [[-0.45] [0.0] [0.3]]]
   ["weierstrass abel" (:abel weierstrass/figures) [20.5] [[0.4] [1.9] [2.9]]]
   ["weierstrass monster zoom" (:monster weierstrass/figures) [0.3 1 1.2 9] [[-0.9] [-0.2] [0.35] [0.8]]]
   ["lebesgue cover" (:cover lebesgue/figures) [0.5] [[0.5 1 0] [1/3 3 1] [0.25 6 0.5]]]])

(deftest every-analysis-figure-kernel-matches-its-emmy-oracle
  (doseq [[label fig params states] cases]
    (testing label
      (when-let [{:keys [wasm max-error nan-states]} (ko/deviation fig params states)]
        (is wasm (str label ": the wasm module loaded"))
        (is (empty? nan-states) (str label ": NaN at " nan-states))
        (is (< max-error 1e-4) (str label ": max error " max-error))))))
