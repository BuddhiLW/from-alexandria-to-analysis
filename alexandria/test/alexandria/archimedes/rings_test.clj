(ns alexandria.archimedes.rings-test
  (:require [alexandria.archimedes.circle :as circle]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b eps] (< (Math/abs (- (double a) (double b))) eps))
(defn- pt [params state] (mapv double ((apply circle/ring-open params) state)))
(defn- arc-length [params k b]
  (let [ps (map #(pt params [k (- (/ % 400) 1/2) b]) (range 401))]
    (reduce + (map (fn [[x1 y1] [x2 y2]] (Math/hypot (- x2 x1) (- y2 y1))) ps (rest ps)))))

(deftest the-rings-argument-is-proved
  (is (every? #{:grade/proved} (map :grade (circle/graded-ring-identities)))))

(deftest a-ring-opens-into-its-trapezoid
  (testing "closed: the disk, cut at the top"
    (is (close? 1 (second (pt [4 0 0 1 0] [0 -1/2 1])) 1e-9))
    (is (close? -1 (second (pt [4 0 0 1 0] [0 0 1])) 1e-9)))
  (testing "open: the outer edge lies on y = -r with ends at x = -pi r and pi r"
    (is (close? (- Math/PI) (first (pt [4 1 0 1 0] [0 -1/2 1])) 1e-6))
    (is (close? -1 (second (pt [4 1 0 1 0] [0 -1/2 1])) 1e-3)))
  (testing "every concentric line keeps its length while it opens"
    (doseq [s0 [0 0.3 0.7 1]]
      (is (close? (* 2 Math/PI 0.875) (arc-length [4 s0 0 1 0] 0 1/2) 1e-3)))))

(deftest the-trapezoids-stack-into-k
  (testing "the innermost ring's inner edge closes to the apex at the centre"
    (is (close? 0 (second (pt [4 1 1 1 0] [3 0 0])) 1e-9)))
  (testing "leaning puts the apex above the left end of the base"
    (is (close? (- Math/PI) (first (pt [4 1 1 1 1] [3 0 0])) 1e-9))))
