(ns alexandria.oresme.mean-speed-test
  "The mean degree theorem: grades, the tie from every source, the strip
   sum against the rectangle, and the bend breaking it."
  (:require [alexandria.oresme.mean-speed :as ms]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest grades
  (is (every? #{:grade/proved :grade/numeric} (map :grade (ms/graded)))))

(deftest tie
  (testing "forward then inverse = identity from every source"
    (is (every? #(close? 0 % 1e-9) (ms/round-trip 1 5 3)))
    (is (every? #(close? 0 % 1e-9) (ms/round-trip 0 4 4))))
  (testing "d sets t with the speeds held"
    (let [out (vec (ms/tie-values 1 5 3 3 30))]
      (is (close? 10 (nth out (+ 20 2)) 1e-9))))
  (testing "vm shifts both speeds, the difference held"
    (let [out (vec (ms/tie-values 1 5 3 7 0))]
      (is (close? 5 (nth out 15) 1e-9))
      (is (close? 9 (nth out 16) 1e-9))))
  (testing "the origin guard: vm = 0 and d set keeps t finite"
    (let [out (vec (ms/tie-values 0 0 3 0 5))]
      (is (Double/isFinite (nth out 22))))))

(deftest theorem
  (is (close? 8 (ms/raster-distance 0 4 4 0) 1e-9))
  (is (close? 12 (ms/raster-distance 2 4 4 0) 1e-9))
  (testing "a bend breaks the rule by 2bt/3"
    (is (close? (+ 8 4) (ms/raster-distance 0 4 4 1.5) 1e-3)))
  (is (= :difformly-difform (ms/case-of 0 4 1.5)))
  (is (= :uniformly-difform-from-none (ms/case-of 0 4 0)))
  (is (= :uniform (ms/case-of 3 3 0))))
