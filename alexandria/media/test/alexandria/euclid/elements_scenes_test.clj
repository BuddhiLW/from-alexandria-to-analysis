(ns alexandria.euclid.elements-scenes-test
  (:require [alexandria.archimedes.quadrature :as q]
            [alexandria.euclid.elements :as el]
            [alexandria.euclid.elements-scenes]
            [alexandria.euclid.elements-view :as view]
            [alexandria.eudoxus.proportion :as ep]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel-oracle :as ko]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]))

(defn- ctx [id]
  {:figures (update-vals el/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data (view/data id)})

(deftest every-stage-of-i-1-and-i-47-is-drawn
  (doseq [id [:euclid/I.1 :euclid/I.47]
          {:keys [stage]} (proofs/steps el/proofs-resource id)
          p [0 0.37 1]]
    (let [h (scene/draw id stage p (ctx id))]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))

(defn- close? [[a b] [c d]] (< (max (Math/abs (- a c)) (Math/abs (- b d))) 1e-9))

(deftest the-windmill-ends-on-the-rectangles
  (let [{:keys [left right points]} (view/data :euclid/I.47)
        g (fn [{:keys [centre theta states]}]
            (figure/points (figure/->FnFigure el/motion) [(first centre) (second centre) theta 1 1]
                           (mapv #(assoc % 10 1) states)))]
    (testing "square GB (A B F G) sheared, turned about B, sheared: rectangle D B M L (I.41, I.4)"
      (is (every? true? (map close? (g left) (map points [:D :B :M :L])))))
    (testing "square HC (A C K H) likewise about C: rectangle E C M L"
      (is (every? true? (map close? (g right) (map points [:E :C :M :L])))))))

(deftest the-greek-figures-are-raster-kernels-matching-emmy
  (testing "I.1's circles and I.47's windmill (trig: raster libm tolerance)"
    (let [{:keys [left right]} (view/data :euclid/I.47)
          {{:keys [A B]} :points} (view/data :euclid/I.1)
          circle (mapv (fn [i] (into (vec B) (concat (repeat 8 0) [(/ i 12)]))) (range 13))]
      (when-let [{:keys [wasm max-error]} (ko/deviation (:motion el/figures)
                                                        [(first A) (second A) (* 2 Math/PI) 0 0] circle)]
        (is wasm) (is (< max-error 1e-4)))
      (doseq [{:keys [centre theta states]} [left right] [wa s wb] [[0 0 0] [0.5 0 0] [1 0.5 0] [1 1 0.5] [1 1 1]]]
        (when-let [{:keys [wasm max-error]} (ko/deviation (:motion el/figures)
                                                          [(first centre) (second centre) theta wa wb]
                                                          (mapv #(assoc % 10 s) states))]
          (is wasm) (is (< max-error 1e-4))))))
  (testing "Quadrature: the stage figure has no trig"
    (doseq [s (range 4) t [0 0.5 1]]
      (when-let [{:keys [wasm max-error]} (ko/deviation (:stage q/figures) [s t]
                                                        (vec (for [k (range (bit-shift-left 1 s)) j (range 3)] [k j])))]
        (is wasm) (is (< max-error 1e-10)))))
  (testing "XII.2: the doubling polygon"
    (doseq [k (range 2 6) t [0 0.3 1]]
      (when-let [{:keys [wasm max-error]} (ko/deviation (:doubling ep/figures) [k t 1]
                                                        (mapv vector (range (bit-shift-left 1 (inc k)))))]
        (is wasm) (is (< max-error 1e-4))))))
