(ns alexandria.leibniz.calculus-scenes-test
  (:require [alexandria.leibniz.calculus :as calc]
            [alexandria.leibniz.calculus-scenes]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is]]))

(def ctx
  {:figures (update-vals calc/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data {}})

(deftest every-stage-is-drawn
  (doseq [id [:leibniz/transmutation :leibniz/arithmetical-quadrature :leibniz/nova-methodus]
          {:keys [stage]} (proofs/steps calc/proofs-resource id)
          p [0 0.37 1]]
    (let [h (scene/draw id stage p ctx)]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))
