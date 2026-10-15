(ns alexandria.noether.orbit-test
  (:require [alexandria.noether.orbit :as o]
            [clojure.test :refer [deftest is testing]]))

(deftest the-orbit-claims-hold-on-raster
  (testing "symmetric: E and J constant; time broken: J kept, E moves; rotation broken: E kept, J moves"
    (is (= [:grade/numeric :grade/numeric :grade/numeric] (mapv :grade (o/graded))))))

(deftest every-widget-strength-keeps-its-charge
  (testing "at each strength of the widget the unbroken charge stays within 1e-6"
    (doseq [eps [0.005 0.01 0.02]]
      (is (< (o/spread (o/run :time eps) :J) 1e-6) (str "time " eps))
      (is (< (o/spread (o/run :rotation eps) :E) 1e-6) (str "rotation " eps)))))

(deftest the-start-is-the-eccentricity-half-ellipse
  (testing "at the start E = v^2/2 - 1/r = 9/8 - 3/2 = -3/8 (semi-major axis 4/3) and J = r v = 1"
    (let [{:keys [E J]} (first (:samples (o/run :symmetric 0.0 0.6 0.06 8)))]
      (is (< (Math/abs (- E -0.375)) 1e-9) (str "E = " E))
      (is (< (Math/abs (- J 1.0)) 1e-9) (str "J = " J)))))
