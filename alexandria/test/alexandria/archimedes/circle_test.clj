(ns alexandria.archimedes.circle-test
  (:require [alexandria.archimedes.circle :as circle]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))
(defn- point [figure params state] (mapv double ((apply figure params) state)))
(defn- norm [[x y]] (Math/hypot x y))

(deftest identities-are-proved-by-emmy
  (is (= #{:grade/proved} (set (map :grade (circle/graded-identities))))))

(deftest archimedes-numbers-hold-exactly
  (is (every? :holds? (circle/table-checks)))
  (is (< circle/lower-bound (circle/lower-ratio (last circle/lower-table))
         Math/PI
         (circle/upper-ratio (last circle/upper-table)) circle/upper-bound)))

(deftest bisection-figures
  (testing "inscribed: odd vertices rise from the side's midpoint to the arc"
    (is (close? (Math/cos (/ Math/PI 6)) (norm (point circle/inscribed-bisection [6 0 1] [1]))))
    (is (close? 1 (norm (point circle/inscribed-bisection [6 1 1] [1])))))
  (testing "circumscribed: corners are cut back to the new points of contact"
    (is (close? (/ 1 (Math/cos (/ Math/PI 6))) (norm (point circle/circumscribed-bisection [6 0 1] [2]))))
    (is (close? 1 (norm (point circle/circumscribed-bisection [6 1 1] [2]))))
    (is (close? (/ 1 (Math/cos (/ Math/PI 12))) (norm (point circle/circumscribed-bisection [6 0.4 1] [1]))))))

(deftest unrolled-circumference-is-the-leg-c
  (is (close? (* 2 Math/PI) (first (point circle/unroll [1 1] [1]))))
  (is (close? 0 (second (point circle/unroll [1 1] [0.5])))))

(deftest a-row-of-triangles-leans-into-one
  (let [n 6 x0 -3 y0 -2
        pts (for [k (range n) j (range 3)] (point circle/sector-row [n 1 1 1 0 x0 y0] [k j]))]
    (is (every? #(close? x0 (first %)) (take-nth 3 pts)) "every apex above x0")
    (is (< (Math/abs (- (+ x0 (* n 2 (Math/sin (/ Math/PI n)))) (first (last pts)))) 1e-6)
        "the base is the perimeter")))

(deftest proofs-are-shelf-data
  (let [res (circle/proofs)]
    (is (r/ok? res))
    (is (every? (set (keys (:ok res))) [:archimedes/circle-1 :archimedes/circle-2 :archimedes/circle-3 :archimedes/circle-rings]))
    (is (every? (fn [{:keys [claim why stage]}] (and (string? claim) (string? why) stage))
                (mapcat :steps (vals (:ok res)))))))
