(ns alexandria.riemann.integral-scenes-test
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel :as kernel]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [alexandria.riemann.integral :as integral]
            [alexandria.riemann.integral-scenes]
            [alexandria.riemann.integral-view :as view]
            [clojure.test :refer [deftest is testing]]))

(def ctx
  {:figures (update-vals integral/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data (view/pathological-data)})

(deftest every-stage-is-drawn
  (doseq [id [:riemann/integral :riemann/pathological]
          {:keys [stage]} (proofs/steps integral/proofs-resource id)
          p [0 0.37 1]]
    (let [h (scene/draw id stage p ctx)]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))

(deftest the-graph-figure-is-riemanns-partial-sum
  (testing "the Emmy figure agrees with the exact partial sums off the jumps (odd denominators)"
    (doseq [x [1/7 2/9 3/11 1/3 2/5 13/15] n [1 3 8]]
      (let [[_ y] (first (figure/points (figure/->FnFigure integral/graph) [n] [[x]]))]
        (is (< (Math/abs (- y (double (integral/partial-sum n x)))) 1e-9) (str "N = " n ", x = " x))))))

(deftest the-graph-compiles-to-a-kernel
  (is (string? (:glue (kernel/kernel (:graph integral/figures))))))

(deftest the-exact-cells-bound-the-graph
  (let [{:keys [cells]} (first (filter #(= 16 (:n %)) (:cells (view/pathological-data))))]
    (doseq [[x0 x1 hi lo] cells
            :let [xm (/ (+ x0 x1) 2) y (double (integral/partial-sum 8 (rationalize xm)))]]
      (is (<= (- lo 1e-12) y (+ hi 1e-12))))))
