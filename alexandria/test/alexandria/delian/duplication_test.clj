(ns alexandria.delian.duplication-test
  (:require [alexandria.delian.duplication :as d]
            [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(deftest every-identity-is-proved
  (doseq [[section gs] (d/graded) {:keys [label grade]} gs]
    (is (= :grade/proved grade) (str section ": " label))))

(deftest the-numbers-come-from-raster
  (let [c (d/cube-root-2)]
    (testing "Menaechmus' meeting point"
      (let [{:keys [x y root]} (d/menaechmus-point 1 2)]
        (is (= :raster (:source root)))
        (is (< (abs (- (* x x x) 2)) 1e-12))
        (is (< (abs (- y (* c c))) 1e-12))))
    (testing "Archytas: AC = 2, AB = 1 gives AM^3 = 2 and AP^3 = 4"
      (let [{:keys [m r x y z root]} (d/archytas-numbers 2 1)]
        (is (= :raster (:source root)))
        (is (< (abs (- (* m m m) 2)) 1e-12))
        (is (< (abs (- (* r r r) 4)) 1e-12))
        (testing "P lies on the three surfaces"
          (is (< (abs (- (+ (* x x) (* y y)) (* 2 x))) 1e-12))
          (is (< (abs (- (+ (* x x) (* y y) (* z z)) (* 2 m))) 1e-12))
          (is (< (abs (- (+ (* x x) (* y y) (* z z)) (* 4 x x))) 1e-12)))))
    (testing "the mesolabe slide: 2 s^3 = 1"
      (let [s (:value (d/mesolabe-slide 2 1))]
        (is (< (abs (- (* 2 s s s) 1)) 1e-12))))
    (is (= :grade/numeric (:grade (d/numeric "cube root of 2" c 1.2599210498948732 1e-12))))))

(deftest wantzel-facts
  (let [w (d/wantzel)]
    (is (= [1 -1 2 -2] (:candidates w)))
    (is (:irreducible w))
    (is (not (:power-of-two? w)))))

(deftest the-shelf-is-catalogued
  (let [res (library/shelves)]
    (is (r/ok? res))
    (is (contains? (:ok res) :delian/hippocrates))
    (is (contains? (:ok res) :delian/wantzel))))
