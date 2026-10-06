(ns alexandria.galileo.motion-scenes-test
  (:require [alexandria.galileo.motion :as motion]
            [alexandria.galileo.motion-scenes]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel :as kernel]
            [alexandria.medium.kernel-oracle :as ko]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]))

(def ctx
  {:figures (update-vals motion/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data {}})

(def scenes
  [:galileo/third-day-1 :galileo/third-day-2 :galileo/corollary-1
   :galileo/inclined-plane :galileo/fourth-day-1 :galileo/fourth-day-7])

(deftest every-stage-is-drawn
  (doseq [id scenes
          {:keys [stage]} (proofs/steps motion/proofs-resource id)
          p [0 0.37 1]]
    (let [h (scene/draw id stage p ctx)]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))

(deftest the-projectile-figure-lies-on-the-parabola
  (let [[u k] [1 0.25]
        p (motion/latus-rectum u k)]
    (doseq [[x y] (figure/points (:throw (:figures ctx)) [u k] (map vector (range 0 4 0.25)))]
      (is (< (Math/abs (- (* x x) (* p (- y)))) 1e-9)))))

(deftest the-ball-reaches-the-foot-in-four-times
  (let [[[x _]] (figure/points (:roll (:figures ctx)) [1.5 1 0] [[4]])]
    (is (< (Math/abs (- x 12.0)) 1e-9))))

(deftest the-figures-compile-to-kernels
  (doseq [[k v] (kernel/kernels motion/figures)]
    (is (string? (:glue v)) (str k)))
  (testing "each kernel, run as wasm in node, matches its Emmy oracle"
    (doseq [[k fig params] [[:throw (:throw motion/figures) [1 0.25]]
                            [:roll (:roll motion/figures) [0.75 0.995 0.1]]
                            [:shot (:shot motion/figures) [15 4]]
                            [:shot (:shot motion/figures) [60 4]]
                            [:speed (:speed motion/figures) [10]]
                            [:speed (:speed motion/figures) [70]]]
            :let [res (ko/deviation fig params (mapv vector (range 0 1.01 0.1)))]
            :when res]
      (is (:wasm res) (str k))
      (is (< (:max-error res) 1e-4) (str k " " (:max-error res))))))

(deftest shots-at-45-plus-and-minus-d-land-together
  (let [land (fn [deg] (peek (figure/points (:shot (:figures ctx)) [deg 4] [[0] [1]])))]
    (is (< (Math/abs (- (first (land 30)) (first (land 60)))) 1e-9))
    (is (< (Math/abs (- (first (land 45)) 4.0)) 1e-9))))
