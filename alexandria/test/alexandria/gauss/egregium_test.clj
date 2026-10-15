(ns alexandria.gauss.egregium-test
  (:require [alexandria.gauss.egregium :as g]
            [alexandria.raster :as raster]
            [clojure.test :refer [deftest is testing]]))

(defn- near [a b] (< (Math/abs (- a b)) 1e-9))

(deftest catenoid-family
  (let [p (g/probe :catenoid)]
    (testing "every bend t keeps K = -1/cosh^4 v at s = 1"
      (doseq [t [0.0 0.4 1.0 (/ Math/PI 2)] [u v] (g/sample-points :catenoid)
              :let [[_ _ _ E F G K Ks] (raster/value p t 1.0 u v)]]
        (is (near K (/ -1 (Math/pow (Math/cosh v) 4))))
        (is (near K Ks))
        (is (near E (Math/pow (Math/cosh v) 2)))
        (is (near F 0.0))))
    (testing "a stretch s changes K"
      (let [[_ _ _ _ _ _ K1] (raster/value p 0.5 1.0 0.0 0.3)
            [_ _ _ _ _ _ K2] (raster/value p 0.5 1.6 0.0 0.3)]
        (is (> (Math/abs (- K1 K2)) 0.05))))))

(deftest sheet
  (let [p (g/probe :sheet)]
    (doseq [t [0.0 0.5 1.0] [u v] (g/sample-points :sheet)
            :let [[_ _ _ E F G K] (raster/value p t 1.0 u v)]]
      (is (near K 0.0)) (is (near E 1.0)) (is (near F 0.0)) (is (near G 1.0)))
    (is (> (Math/abs (nth (raster/value p 0.5 1.5 0.2 0.3) 6)) 0.1))))

(deftest grades
  (is (every? #{:grade/proved :grade/numeric} (map :grade (g/graded))))
  (is (= :grade/proved (:grade (second (g/graded))))))
