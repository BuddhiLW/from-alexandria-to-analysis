(ns alexandria.apollonius.symptom-test
  (:require [alexandria.apollonius.conics :as conics]
            [alexandria.apollonius.symptom :as symptom]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest every-identity-is-proved-by-emmy
  (let [graded (symptom/graded)]
    (is (= 7 (count graded)))
    (doseq [{:keys [label grade]} graded]
      (is (= :grade/proved grade) label))))

(deftest the-forms-in-tex
  (let [{:keys [parabola hyperbola ellipse general c]} (symptom/tex)]
    (is (= "{y}^{2} = p\\,x" parabola))
    (is (str/includes? hyperbola "+ \\left(\\frac{p}{d}\\right)\\,{x}^{2}"))
    (is (str/includes? ellipse "- \\left(\\frac{p}{d}\\right)\\,{x}^{2}"))
    (is (= "{y}^{2} = p\\,x + c\\,{x}^{2}" general))
    (is (str/starts-with? c "c = "))))

(def ^:private view {:k 0.6 :h 0.7 :zb 2.2})

(deftest both-sides-from-raster
  (doseq [deg [25 35 50 (* (/ 180 Math/PI) (Math/atan2 1 0.6)) 70 80]
          x [0.1 0.5 0.85]]
    (let [{:keys [y2 rhs gap c p d]} (symptom/numbers (assoc view :deg deg :x x))]
      (is (< (Math/abs (- y2 rhs)) 1e-4) [deg x])
      (is (< (Math/abs gap) 1e-4))
      (when (> (Math/abs c) 1e-6)
        (is (< (Math/abs (+ c (/ p d))) 1e-4) "c = -p/d")))))

(deftest the-sign-of-c-names-the-section
  (is (= :ellipse (conics/kind (:c (symptom/numbers (assoc view :deg 35 :x 0.5))))))
  (is (= :hyperbola (conics/kind (:c (symptom/numbers (assoc view :deg 80 :x 0.5))))))
  (is (< (Math/abs (:c (symptom/numbers (assoc view :deg (* (/ 180 Math/PI) (Math/atan2 1 0.6)) :x 0.5))))
         1e-4)))

(deftest the-inverse-roots
  (testing "the near root gives back x on every section; the far root the ellipse's other x"
    (doseq [deg [35 70 80] x [0.2 0.6]]
      (let [{:keys [y]} (symptom/numbers (assoc view :deg deg :x x))
            {:keys [xnear]} (symptom/numbers (assoc view :deg deg :x x :yin y))]
        (is (< (Math/abs (- xnear x)) 1e-4) [deg x])))
    (let [x 1.4
          {:keys [y half]} (symptom/numbers (assoc view :deg 35 :x x))
          {:keys [xfar]} (symptom/numbers (assoc view :deg 35 :x x :yin y))]
      (is (> x half))
      (is (< (Math/abs (- xfar x)) 1e-4)))))
