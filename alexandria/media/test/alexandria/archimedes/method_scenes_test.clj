(ns alexandria.archimedes.method-scenes-test
  (:require [alexandria.archimedes.method :as method]
            [alexandria.archimedes.method-scenes]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is]]))

(def ctx
  {:figures (update-vals method/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data {:points (update-vals method/points #(mapv double %))}})

(deftest every-stage-is-drawn
  (doseq [{:keys [stage]} (proofs/steps method/proofs-resource :archimedes/method-1)
          p [0 0.37 1]]
    (let [h (scene/draw :archimedes/method-1 stage p ctx)]
      (is (and (vector? h) (not= :text (first h))) (str stage " at " p)))))
