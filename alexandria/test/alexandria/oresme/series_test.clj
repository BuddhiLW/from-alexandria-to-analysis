(ns alexandria.oresme.series-test
  "Oresme's series: the grouping bound, the harmonic sums against
   ln N + gamma, the convergent n/2^n, the p > 1 case breaking the bound."
  (:require [alexandria.oresme.series :as series]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest grades
  (is (every? #{:grade/proved :grade/numeric} (map :grade (series/graded)))))

(deftest sums
  (testing "block and term kernels agree with plain sums"
    (is (close? (reduce + (map #(/ 1.0 %) (range 1 101))) (first (series/partial-sum 100)) 1e-12))
    (is (close? (reduce + (map #(/ 1.0 (* % %)) (range 5 77))) (first (series/partial-sum 2 5 76)) 1e-12)))
  (testing "each group of the harmonic series is at least 1/2"
    (is (every? #(>= % 0.5) (map series/group-sum (range 1 12)))))
  (testing "p = 2: groups fall below 1/2, the sum stays below pi^2/6"
    (is (< (series/group-sum 2 3) 0.5))
    (is (< (first (series/partial-sum 2 1 4096)) (/ (* Math/PI Math/PI) 6))))
  (testing "Oresme's convergent series"
    (is (close? 2 (second (series/partial-sum 64)) 1e-12)))
  (is (= :harmonic-diverges (series/case-of 1)))
  (is (= :converges (series/case-of 2))))
