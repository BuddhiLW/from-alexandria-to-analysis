(ns alexandria.riemann-geometry.metric-test
  (:require [alexandria.riemann-geometry.metric :as m]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest normal-form-has-constant-curvature
  (testing "h = 0: K = alpha whatever A, from the metric alone"
    (doseq [[a b c] [[1 0 1] [2 0.5 1] [0.7 -0.3 1.6]]
            al [-1.0 0.0 0.8]]
      (is (close? al (nth (m/probe-at a b c al 0 0.3 -0.2) (m/probe-index :K)) 1e-8)))))

(deftest hill-makes-curvature-vary
  (let [k (fn [x y] (nth (m/probe-at 1 0 1 0 1.5 x y) (m/probe-index :K)))]
    (is (not (close? (k 0.0 0.0) (k 1.0 0.3) 1e-3)))))

(deftest det-and-tie
  (is (close? 1.75 (m/det 2 0.5 1) 1e-12))
  (let [[g11 g12 g22 d] (m/from-d 2 0.5 0 3)]
    (is (= [2 0.5] [g11 g12]))
    (is (close? 3 (m/det g11 g12 g22) 1e-9))
    (is (= 3 d))))

(deftest grades-pass
  (is (every? #{:grade/proved :grade/numeric} (map :grade (m/graded)))))
