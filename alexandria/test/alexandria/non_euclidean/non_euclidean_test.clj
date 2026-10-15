(ns alexandria.non-euclidean.non-euclidean-test
  (:require [alexandria.non-euclidean.disk :as d]
            [alexandria.non-euclidean.parallels :as p]
            [alexandria.non-euclidean.pseudosphere :as ps]
            [alexandria.raster :as raster]
            [clojure.test :refer [deftest is testing]]))

(def ok #{:grade/proved :grade/numeric})

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest parallels-tie
  (testing "every grade passes"
    (is (every? ok (map :grade (p/graded)))))
  (testing "the tie solves from each source"
    (let [[x P t] ((p/tie 1.0 45 0.5) nil)]
      (is (close? t (Math/exp -1) 1e-12))
      (is (close? P (Math/toDegrees (* 2 (Math/atan (Math/exp -1)))) 1e-9)))
    (let [[x P t] (subvec ((p/tie 0 90 0.5) nil) 3 6)]
      (is (close? x 0 1e-9) "Pi = 90 degrees is the Euclidean distance 0")
      (is (close? t 1 1e-12))))
  (testing "the readout kernel compiles with raster"
    (let [o (raster/value (fn [_] ((p/readout 2.0) nil)) 0)]
      (is (close? (nth o 1) (Math/toDegrees (* 2 (Math/atan (Math/exp -2)))) 1e-9)))))

(deftest disk
  (is (every? ok (map :grade (d/graded))))
  (testing "a tiny triangle at the centre is almost Euclidean"
    (let [o ((d/triangle 0 0.01 0 -0.005 0.008 -0.005 -0.008) nil)]
      (is (< (nth o 4) 1e-3))))
  (testing "a nearly ideal triangle has angle sum near 0 and area near pi"
    (let [o ((d/triangle 0 0.995 0 -0.4975 0.8617 -0.4975 -0.8617) nil)]
      (is (< (nth o 3) 0.05))
      (is (close? (nth o 4) Math/PI 0.05))))
  (testing "Klein display of a point on the real axis"
    (let [[x y] (d/display 1 [0.5 0])]
      (is (close? x 0.8 1e-12)) (is (close? y 0 1e-12))))
  (testing "parallels: the line at angle Pi(d) is the limiting parallel"
    (let [o ((d/parallels 0 0.1 0.4 0.3) nil)]
      (is (close? (nth o 1) (nth o 2) 1e-9)))))

(deftest pseudosphere
  (is (every? ok (map :grade (ps/graded))))
  (let [o ((ps/probe 1 0.7 1.4) nil)]
    (is (close? (nth o 6) -1 1e-8))))
