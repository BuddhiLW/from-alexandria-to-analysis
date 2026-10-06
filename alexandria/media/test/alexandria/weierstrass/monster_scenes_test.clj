(ns alexandria.weierstrass.monster-scenes-test
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel :as kernel]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [alexandria.weierstrass.monster :as monster]
            [alexandria.weierstrass.monster-scenes]
            [alexandria.weierstrass.monster-view :as view]
            [clojure.test :refer [deftest is testing]]))

(def ctx
  {:figures (update-vals monster/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data {:x0 (double view/x0) :a monster/a :roughness monster/roughness :rows (view/rows)}})

(deftest every-stage-is-drawn
  (doseq [id [:weierstrass/uniform :weierstrass/monster]
          {:keys [stage]} (proofs/steps monster/proofs-resource id)
          p [0 0.37 1]]
    (let [h (scene/draw id stage p ctx)]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))

(deftest figures-compile-to-kernels
  (doseq [[k fig] monster/figures]
    (is (string? (:glue (kernel/kernel fig))) (str k))))

(deftest the-window-figure-is-the-monster
  (testing "nine terms, window of half-width w: the rise matches the exact series to 2^-9"
    (let [x0 3/10 w 1/13
          [[_ y]] (figure/points (figure/->FnFigure monster/monster-window) [x0 w 1 9] [[1/2]])
          exact (- (monster/monster (+ x0 (* 1/2 w))) (monster/monster x0))]
      (is (< (Math/abs (- y exact)) (* 2 (Math/pow 0.5 9)))))))

(deftest the-zoom-never-straightens
  (testing "at every magnification the scaled window keeps its roughness: total variation stays large"
    (let [fig (figure/->FnFigure monster/monster-window)
          tv (fn [w] (let [h (Math/pow w monster/roughness)
                           ys (map second (figure/points fig [0.3 w h 9] (mapv (fn [i] [(- (/ i 200.0) 1)]) (range 401))))]
                       (reduce + (map #(Math/abs (- %2 %1)) ys (rest ys)))))]
      (is (every? #(> (tv %) 2.0) [1 (/ 1 13.0) (/ 1 169.0)])))))
