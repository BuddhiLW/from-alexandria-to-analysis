(ns alexandria.archimedes.quadrature-scenes-test
  (:require [alexandria.archimedes.quadrature :as q]
            [alexandria.archimedes.quadrature-scenes]
            [alexandria.archimedes.quadrature-view :as view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel :as kernel]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is]]))

(def ctx
  {:figures (update-vals q/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data (view/data)})

(deftest every-stage-is-drawn
  (doseq [id [:archimedes/parabola-21 :archimedes/parabola-22 :archimedes/parabola-23 :archimedes/parabola-24]
          {:keys [stage]} (proofs/steps q/proofs-resource id)
          p [0 0.37 1]
          controls [{} {:stages 5}]]
    (let [h (scene/draw :archimedes/parabola stage p (assoc ctx :controls controls))]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))

(deftest the-stage-figure-compiles-to-a-raster-kernel
  (let [{:keys [glue fallback]} (kernel/kernel (:stage q/figures))]
    (is (string? glue))
    (is (seq fallback))))

(deftest rows-are-exact
  (is (= {:stage 3 :polygon "85/64" :left "1/192"} (nth (view/rows) 3))))
