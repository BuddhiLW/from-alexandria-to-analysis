(ns alexandria.eudoxus.proportion-test
  (:require [alexandria.euclid.elements :as el]
            [alexandria.eudoxus.proportion :as ep]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(deftest every-identity-and-check-is-proved
  (is (= 8 (count (ep/graded))))
  (is (every? #{:grade/proved} (map :grade (ep/graded)))))

(deftest the-shares-come-from-raster-and-exhaust-the-circle
  (let [s (ep/circle-shares 10)]
    (testing "the square is 2/pi of the circle"
      (is (< (Math/abs (- (s 2) (/ 2 Math/PI))) 1e-12)))
    (testing "each doubling takes more, and the 1024-gon leaves less than 1e-4"
      (is (apply < (map s (range 2 11))))
      (is (< (- 1 (s 10)) 1e-4)))))

(deftest the-ported-functions
  (testing "equimultiples"
    (is (= :greater (ep/equimultiple-order 3 2 2 2)))
    (is (= :equal (ep/equimultiple-order 2 3 3 2))))
  (testing "Def. 5 holds for equal ratios and fails for unequal ones"
    (is (ep/same-ratio? 2 3 4 6 (ep/pairs 10)))
    (is (not (ep/same-ratio? 2 3 3 4 (ep/pairs 10)))))
  (testing "Def. 7: 3 x 3 > 4 x 2 while 3 x 4 = 4 x 3 is not greater; none the other way"
    (is (= {:m 3 :n 4} (ep/greater-ratio? 3 2 4 3 (ep/pairs 5))))
    (is (nil? (ep/greater-ratio? 4 3 3 2 (ep/pairs 5)))))
  (testing "the cut of diagonal : side"
    (is (= :lower (ep/diagonal-cut-side 7 5)))
    (is (= :upper (ep/diagonal-cut-side 17 12)))
    (is (not-any? #(= :equal (:side %)) (ep/cut-sample 40)))
    (is (= (count (ep/cut-sample 3)) (count (distinct (map (juxt :m :n) (ep/cut-sample 3))))))))

(deftest the-doubling-figure
  (testing "even vertices lie on the circle; odd ones reach it at t = 1"
    (doseq [m (range 8) t [0 1]]
      (let [[x y] (mapv double ((ep/doubling 2 t 1) [m]))
            rho (Math/hypot x y)]
        (if (or (even? m) (= 1 t))
          (is (< (Math/abs (- rho 1)) 1e-9))
          (is (< (Math/abs (- rho (Math/cos (/ Math/PI 4)))) 1e-9)))))))

(deftest the-shelf
  (is (r/ok? (ep/proof)))
  (is (= [:grade/proved] (map :grade (el/graded :eudoxus/V.def-5))))
  (is (= [:grade/proved] (map :grade (el/graded :eudoxus/XII.2)))))
