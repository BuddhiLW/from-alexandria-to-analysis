(ns alexandria.archimedes.quadrature-test
  (:require [alexandria.archimedes.quadrature :as q]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(deftest every-check-is-proved
  (is (= 7 (count (q/graded))))
  (is (every? #{:grade/proved} (map :grade (q/graded)))))

(deftest the-ported-functions
  (testing "the first triangle and its children"
    (is (= 1 (q/area q/first-triangle)))
    (is (= [0 1] (:v q/first-triangle)))
    (is (= 2 (count (q/stage-triangles 1))))
    (is (= 8 (count (q/stage-triangles 3)))))
  (testing "each stage is a quarter of the one before, exactly"
    (is (= [1 1/4 1/16 1/64] (map q/stage-area (range 4)))))
  (testing "partial areas and the geometric sum"
    (is (= 85/64 (q/partial-area 3) (q/geometric-partial 3)))
    (is (= 1/192 (q/uncovered 3))))
  (testing "Prop. 23, and a false variant fails"
    (is (every? q/prop-23-holds? (range 12)))
    (is (not= 4/3 (q/geometric-partial 5))))
  (is (= 4/3 q/segment-area)))

(deftest the-stage-figure
  (testing "grown in place, every corner lies on the parabola"
    (doseq [s (range 4) k (range (bit-shift-left 1 s)) j (range 3)]
      (let [[x y] ((q/stage-figure s 1) [k j])]
        (is (< (Math/abs (- (double y) (- 1 (* x x)))) 1e-12)))))
  (testing "flat, the vertex of the right triangle of stage 1 lies on its chord from (0, 1) to (1, 0)"
    (is (= [1/2 1/2] ((q/stage-figure 1 0) [1 1])))))

(deftest proofs-are-shelf-data
  (is (r/ok? (q/proof)))
  (doseq [id [:archimedes/parabola-21 :archimedes/parabola-22 :archimedes/parabola-23 :archimedes/parabola-24]]
    (let [p (get-in (q/proof) [:ok id])]
      (is (string? (:statement p)) (str id))
      (is (seq (:steps p)) (str id)))))

(deftest the-arc-is-sampled-by-raster-on-the-parabola
  (let [arc (q/arc 40)]
    (is (= 41 (count arc)))
    (is (every? (fn [[x y]] (< (Math/abs (- y (- 1 (* x x)))) 1e-12)) arc))
    (is (= [[-1.0 0.0] [1.0 0.0]] [(first arc) (last arc)]))))
