(ns alexandria.archimedes.circle-scenes-test
  (:require [alexandria.archimedes.circle :as circle]
            [alexandria.archimedes.circle-scenes]
            [alexandria.archimedes.circle-view :as view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [clojure.test :refer [deftest is]]))

(def ctx
  {:figures (update-vals circle/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data (merge {:excess 0.05} (view/prop-3-data))})

(defn- drawn? [h] (and (vector? h) (not= :text (first h))))

(deftest every-stage-of-every-proof-is-drawn
  (doseq [[id {:keys [steps]}] (:ok (circle/proofs))
          {:keys [stage]} steps
          p [0 0.37 1]]
    (is (drawn? (scene/draw id stage p ctx)) (str id " " stage " at " p))))

(deftest archimedes-writes-mixed-numbers
  (is (= "4673 1/2" (view/mixed 9347/2)))
  (is (= "1838 9/11" (view/mixed 20227/11)))
  (is (= "265" (view/mixed 265))))
