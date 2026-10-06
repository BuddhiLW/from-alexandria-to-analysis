(ns alexandria.archimedes.rings-scenes-test
  (:require [alexandria.archimedes.circle :as circle]
            [alexandria.archimedes.rings-scenes]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]
            [alexandria.medium.kernel-oracle :as ko]))

(defn- ctx [n]
  {:figures {:rings (figure/->FnFigure circle/ring-open)}
   :palette (palette/palette :deck)
   :controls {:n n}})

(deftest every-stage-is-drawn-for-any-number-of-rings
  (doseq [{:keys [stage]} (proofs/steps circle/proofs-resource :archimedes/circle-rings)
          n [1 4 16]
          p [0 0.4 1]]
    (let [h (scene/draw :archimedes/circle-rings stage p (ctx n))]
      (is (and (vector? h) (not= :text (first h))) (str stage " n=" n " p=" p)))))

(deftest every-circle-figure-kernel-matches-its-emmy-oracle
  (testing "the first raster kernels (Measurement of a Circle), audited: wasm in node against Emmy on the JVM"
    (doseq [[id params states] [[:inscribed [6 0.4 1] (mapv vector (range 12))]
                                [:circumscribed [6 0.4 1] (mapv vector (range 24))]
                                [:unroll [0.6 1] [[0] [0.3] [0.8] [1]]]
                                [:sectors [6 0.5 0.3 1 1 0.5 -1.2] [[0 0] [2 1] [5 2]]]
                                [:polygon [32 1 0] [[0]]]
                                [:polygon [16 1 1] [[0]]]
                                [:rings [4 1 0.6 1 0.3] [[0 -0.5 0] [2 0.2 1] [3 0.5 0.5]]]]]
      (when-let [{:keys [wasm max-error]} (ko/deviation (circle/figures id) params states)]
        (is wasm (str id))
        (is (< max-error 1e-4) (str id " " max-error))))))
