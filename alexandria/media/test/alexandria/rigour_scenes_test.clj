(ns alexandria.rigour-scenes-test
  "Every stage of the Dedekind, Cantor and Lebesgue players is drawn, at the
   start, the middle and the end of its step, through the FnFigure port."
  (:require [alexandria.cantor.sets :as sets]
            [alexandria.cantor.sets-scenes]
            [alexandria.cantor.sets-view :as sets-view]
            [alexandria.dedekind.cuts :as cuts]
            [alexandria.dedekind.cuts-scenes]
            [alexandria.dedekind.cuts-view :as cuts-view]
            [alexandria.lebesgue.measure :as measure]
            [alexandria.lebesgue.measure-scenes]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]))

(defn- ctx [figures data]
  {:figures (update-vals figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data data})

(defn- every-stage-drawn [scene-id resource proof-id c]
  (let [steps (proofs/steps resource proof-id)]
    (is (< 1 (count steps)) (str proof-id " has steps"))
    (doseq [{:keys [stage]} steps p [0 0.37 1]]
      (let [h (scene/draw scene-id stage p c)]
        (is (and (vector? h) (not= :text (first h))) (str scene-id " " stage " at " p))))))

(deftest dedekind
  (every-stage-drawn :dedekind/cut-sqrt-2 cuts/proofs-resource :dedekind/cut-sqrt-2
                     (ctx cuts/figures (cuts-view/scene-data))))

(deftest cantor
  (every-stage-drawn :cantor/zigzag sets/proofs-resource :cantor/zigzag
                     (ctx sets/figures {:walk sets/walk-cells :list (mapv str (take 24 (sets/zig-zag)))}))
  (every-stage-drawn :cantor/heights sets/proofs-resource :cantor/heights
                     (ctx {} {:heights (sets-view/heights-data)}))
  (every-stage-drawn :cantor/diagonal sets/proofs-resource :cantor/diagonal
                     (ctx {} (sets-view/diagonal-data))))

(deftest lebesgue
  (let [data {:rationals (mapv double (take 40 (measure/rationals-01)))}]
    (every-stage-drawn :lebesgue/measure-zero measure/proofs-resource :lebesgue/measure-zero
                       (ctx measure/figures data))
    (every-stage-drawn :lebesgue/dirichlet measure/proofs-resource :lebesgue/dirichlet
                       (ctx {} data))))

(deftest heights-data-reads-cantors-phi
  (is (= [1 2 4 12] (map count (sets-view/heights-data)))))
