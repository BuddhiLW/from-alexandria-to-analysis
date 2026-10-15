(ns alexandria.riemann-geometry.geodesic-test
  (:require [alexandria.riemann-geometry.geodesic :as g]
            [clojure.test :refer [deftest is testing]]))

(deftest torus-curvature-changes-sign
  (let [K (:K (g/compact :torus))]
    (is (pos? (K 0.0 0.0)) "outside of the torus: K > 0")
    (is (neg? (K 0.0 Math/PI)) "inside: K < 0")))

(deftest geodesics-keep-unit-speed
  (doseq [kind g/kinds]
    (testing (name kind)
      (doseq [s (g/run kind 0.2 0.1 1.1 150 0.01)]
        (is (< (Math/abs (- (g/speed2 kind s) 1.0)) 1e-6))))))

(deftest flat-geodesics-are-lines
  (let [[u v] (last (g/run :flat 0.0 0.0 0.5 100 0.01))]
    (is (< (Math/abs (- u (Math/cos 0.5))) 1e-9))
    (is (< (Math/abs (- v (Math/sin 0.5))) 1e-9))))

(deftest converge-and-diverge
  (is (= :converge (apply g/case-name (g/spread :sphere 0.0 0.0 0.3 0.3 1.5 150))))
  (is (= :diverge (apply g/case-name (g/spread :saddle 0.0 0.0 0.3 0.2 1.2 150))))
  (is (= :parallel (apply g/case-name (g/spread :flat 0.0 0.0 0.3 0.2 1.2 150))))
  (is (= :converge (apply g/case-name (g/spread :torus 0.0 0.0 0.3 0.2 1.2 150))))
  (is (= :diverge (apply g/case-name (g/spread :torus 0.0 3.0 0.3 0.2 1.2 150)))))

(deftest grades-pass
  (is (every? #{:grade/proved :grade/numeric} (map :grade (g/graded)))))
