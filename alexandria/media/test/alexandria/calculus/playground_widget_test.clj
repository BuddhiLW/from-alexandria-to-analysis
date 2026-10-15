(ns alexandria.calculus.playground-widget-test
  (:require [alexandria.calculus.playground :as math]
            [alexandria.calculus.playground-widget :as widget]
            [clojure.test :refer [deftest is testing]]))

(deftest playground-setups
  (is (= #{:transmutation :quadrature :product :lemma :impulses :ellipse}
         (set (keys widget/defaults))))
  (doseq [k (keys widget/defaults)]
    (is (seq (widget/prompts k)))
    (is (map? (widget/schemas k)))))

(deftest quadrature-and-limits
  (let [[s target err bound] (math/value math/series 8)]
    (is (< (Math/abs err) bound))
    (is (< (Math/abs (- target (/ Math/PI 4))) 1e-8))
    (is (< (Math/abs s) 2)))
  (is (< (nth (math/value math/rectangles 0 100) 2)
         (nth (math/value math/rectangles 0 8) 2))))

(deftest product-and-centre
  (let [[_ a b corner delta] (math/value math/product 1.2 1.0 0.25 0.3)]
    (is (< (Math/abs (- delta (+ a b corner))) 1e-10)))
  (is (< (nth (math/areas 8 0.2 0 0) 3) 1e-10))
  ;; areas are measured about S: a central impulse toward a displaced S
  ;; still sweeps equal areas (Principia I, Prop. 1)
  (is (< (nth (math/areas 8 0.2 0.7 0.2) 3) 1e-10))
  ;; an impulse displaced from S breaks them (the widget's prompt)
  (is (> (nth (math/areas 8 0.2 0 0 0.7) 3) 1e-5)))

(deftest ellipse-and-triangle
  (let [[x y r _ f torque] (math/value math/ellipse 0.55 1.5 1 0 0)]
    (is (< (Math/abs torque) 1e-10))
    (is (< (Math/abs (- f (/ 1 (* 1.5 r r)))) 1e-10))
    (is (number? x)) (is (number? y)))
  (is (not= 0 (nth (math/value math/ellipse 0.55 1.5 1 0.6 0.2) 5)))
  (is (every? number? (math/value math/triangle 2 0.4 0.15))))
