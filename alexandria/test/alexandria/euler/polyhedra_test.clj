(ns alexandria.euler.polyhedra-test
  (:require [alexandria.euler.polyhedra :as p]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(deftest platonic-counts-from-coordinates
  (is (= {:tetrahedron {:V 4 :E 6 :F 4}
          :cube {:V 8 :E 12 :F 6}
          :octahedron {:V 6 :E 12 :F 8}
          :dodecahedron {:V 20 :E 30 :F 12}
          :icosahedron {:V 12 :E 30 :F 20}}
         (into {} (map (juxt :id p/counts)) p/platonic))))

(deftest prisms-and-pyramids
  (doseq [n (range 3 9)]
    (is (= {:V (* 2 n) :E (* 3 n) :F (+ n 2)} (p/counts (p/prism n))))
    (is (= {:V (inc n) :E (* 2 n) :F (inc n)} (p/counts (p/pyramid n))))))

(deftest every-solid-is-proved
  (is (every? #{:grade/proved} (map :grade (p/graded)))))

(deftest cauchy-keeps-the-invariant
  (let [steps (p/cauchy-steps)]
    (is (= 2 (p/characteristic (:counts (first steps)))))
    (is (every? #(= 1 (p/characteristic (:counts %))) (rest steps)))
    (is (= {:V 3 :E 3 :F 1} (:counts (peek steps))))))

(deftest the-cube-flattens
  (testing "at t = 1 the top face is the outer square, the bottom the inner one"
    (let [f (p/flatten-cube 1)
          r (fn [v] (let [[x y] (f v)] (Math/hypot x y)))]
      (is (> (r [1 1 1]) (r [1 1 -1])))
      (is (< (Math/abs (- (r [1 1 1]) (r [-1 1 1]))) 1e-12)))))

(deftest the-proof-is-shelf-data
  (is (r/ok? (p/proof)))
  (is (= 6 (count (get-in (p/proof) [:ok :euler/polyhedra :steps])))))
