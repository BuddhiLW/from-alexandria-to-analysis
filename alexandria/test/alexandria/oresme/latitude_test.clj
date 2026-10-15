(ns alexandria.oresme.latitude-test
  "Oresme's latitude of forms: the figure's kernels, the case names, the
   area by raster against Boole's rule."
  (:require [alexandria.oresme.latitude :as lat]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest grades
  (is (every? #{:grade/proved :grade/numeric} (map :grade (lat/graded)))))

(deftest cases
  (is (= :uniform (lat/case-of [2.5 2.5 2.5 2.5 2.5])))
  (is (= :uniformly-difform-from-none (lat/case-of [0 1 2 3 4])))
  (is (= :uniformly-difform (lat/case-of [1 1.75 2.5 3.25 4])))
  (is (= :difformly-difform (lat/case-of [0.5 2.6 3.4 2.6 0.5]))))

(deftest areas
  (testing "Oresme's triangle from no degree: area 8 = middle degree 2 times AB = 4"
    (let [out (lat/summary-values [0 1 2 3 4])]
      (is (close? 8 (nth out (lat/summary-index :area)) 1e-12))
      (is (close? 8 (nth out (lat/summary-index :mid-rect)) 1e-12))
      (is (close? 8 (lat/raster-area [0 1 2 3 4] 100) 1e-9))))
  (testing "bent: the middle-degree rectangle no longer equals the figure"
    (let [out (lat/summary-values [0.5 2.6 3.4 2.6 0.5])]
      (is (> (Math/abs (- (nth out 0) (nth out 11))) 0.5))))
  (testing "a negative degree is clipped: the strips exceed the raw polynomial's area"
    (let [hs [3 0.5 -1 0.5 3]]
      (is (> (lat/raster-area hs 400) (first (lat/summary-values hs)))))))
