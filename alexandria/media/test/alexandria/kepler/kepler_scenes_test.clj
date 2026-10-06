(ns alexandria.kepler.kepler-scenes-test
  (:require [alexandria.kepler.mysterium :as my]
            [alexandria.kepler.mysterium-scenes]
            [alexandria.kepler.mysterium-view :as my-view]
            [alexandria.kepler.orbit :as orbit]
            [alexandria.kepler.orbit-scenes]
            [alexandria.kepler.orbit-view :as orbit-view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel :as kernel]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]
            [alexandria.medium.kernel-oracle :as ko]))

(defn- ctx [figures data]
  {:figures (update-vals figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data data})

(defn- drawn? [scene-id resource ctx]
  (doseq [{:keys [stage]} (proofs/steps resource scene-id)
          p [0 0.37 1]]
    (let [h (scene/draw scene-id stage p ctx)]
      (is (and (vector? h) (not= :text (first h))) (str scene-id " " stage " at " p)))))

(deftest every-stage-is-drawn
  (drawn? :kepler/mysterium my/proofs-resource (ctx my-view/figures (my-view/data)))
  (let [c (ctx orbit-view/figures (orbit-view/data))]
    (drawn? :kepler/war-with-mars orbit/proofs-resource c)
    (drawn? :kepler/harmonice orbit/proofs-resource c)))

(deftest the-projection-is-orthographic
  (let [project (:f (:project my-view/figures))
        cube (:vertices (:cube (:solids (my-view/data))))]
    (testing "the cube's vertices project inside the circle of its circumradius"
      (doseq [v cube :let [[x y] (mapv double ((project 0.7 0.42 1.0) v))]]
        (is (<= (+ (* x x) (* y y)) (+ 3 1e-9)))))
    (testing "with no turn and no tilt, a vertex drops its depth"
      (is (= [1.0 -1.0] (mapv double ((project 0 0 1.0) [1 -1 1])))))))

(deftest the-planet-kernels-compile
  (testing "every figure compiles to a raster kernel"
    (is (every? :glue (vals (kernel/kernels orbit-view/figures))))
    (is (every? :glue (vals (kernel/kernels my-view/figures)))))
  (testing "each orbit kernel, run as wasm in node, matches its Emmy oracle"
    (doseq [[k fig] orbit-view/figures
            :let [res (ko/deviation fig (mapv vector (range 0 6.3 0.45)))]
            :when res]
      (is (:wasm res) (str k))
      (is (< (:max-error res) 1e-4) (str k " " (:max-error res)))))
  (testing "the kernel's fixed-point steps put Mars where raster's root does"
    (let [{ecc :e a :a} orbit/mars
          f (orbit/planet ecc a)]
      (doseq [m [0.3 1.0 2.5 4.0]
              :let [[x _] (mapv double (f [m]))
                    E (orbit/eccentric-anomaly ecc m)]]
        (is (< (Math/abs (- x (* a (Math/cos E)))) 1e-5))))))

(deftest the-projection-kernel-matches-emmy
  (testing "every vertex of every solid, projected by the wasm kernel, lands where Emmy puts it"
    (let [solids (:solids (my-view/data))
          states (vec (mapcat :vertices (vals solids)))]
      (doseq [params [[0 0.42 1] [0.9 0.42 0.5] [2.3 -0.3 1.7]]]
        (when-let [{:keys [wasm max-error]} (ko/deviation (:project my-view/figures) params states)]
          (is wasm)
          (is (< max-error 1e-4) (str params " " max-error)))))))
