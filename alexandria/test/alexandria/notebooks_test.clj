(ns alexandria.notebooks-test
  (:require [alexandria.measure :as measure]
            [alexandria.notebooks.euclid-i1-surfaces :as i1]
            [clojure.test :refer [deftest is testing]]))

(defn close? [a b tol] (<= (Math/abs (- (double a) (double b))) tol))

(deftest euclid-i1-constructions
  (doseq [{:keys [title lengths context A B C excess curvature-integral]} i1/constructions]
    (testing title
      (is (close? (:AB lengths) (:BC lengths) 1.0e-5))
      (is (close? (:AB lengths) (:CA lengths) 1.0e-5))
      (is (close? excess (measure/geodesic-triangle-integral context A B C) 1.0e-3))
      (is (close? excess curvature-integral 1.0e-3))))
  (let [by-id (into {} (map (juxt :id identity) i1/constructions))]
    (is (> (:angle-sum (:sphere by-id)) Math/PI))
    (is (close? (:angle-sum (:plane by-id)) Math/PI 1.0e-6))
    (is (< (:angle-sum (:hyperbolic by-id)) Math/PI))))
