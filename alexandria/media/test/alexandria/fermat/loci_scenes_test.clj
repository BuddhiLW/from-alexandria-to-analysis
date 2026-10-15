(ns alexandria.fermat.loci-scenes-test
  (:require [alexandria.fermat.loci :as loci]
            [alexandria.fermat.loci-scenes]
            [alexandria.fermat.loci-view :as view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]))

(defn- ctx []
  {:figures (update-vals loci/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck) :data {} :controls {}})

(deftest every-loci-stage-is-drawn
  (let [steps (proofs/steps loci/proofs-resource :fermat/loci)]
    (is (= 7 (count steps)))
    (doseq [{:keys [stage]} steps p [0 0.37 1]]
      (let [h (scene/draw :fermat/loci stage p (ctx))]
        (is (and (vector? h) (not= :text (first h))) (str stage " at " p))))))

(deftest the-moving-i-is-on-fermat's-curve
  (testing "the scene's I at u, through the figure kernel, satisfies the equation"
    (let [eqs (loci/equations {:D 1 :B 1.5 :Z 1 :m 0.6})
          fig (fn [k params] (figure/->FnFigure (:f (loci/figures k))))]
      (doseq [[k params] {:line [1 2] :hyperbola [1] :parabola [1] :circle [1.5] :ellipse [1.5 0.6]}
              :when (not= k :line)
              u [0.5 1.3]]
        (let [[pt] (figure/points (fig k params) params [[u]])]
          (is (< (Math/abs (double ((eqs k) pt))) 1e-9) (str k " " u)))))))

(deftest the-view-mounts
  (is (:steps (view/section)))
  (is (some? (view/loci))))
