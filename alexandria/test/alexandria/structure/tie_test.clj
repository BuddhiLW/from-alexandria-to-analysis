(ns alexandria.structure.tie-test
  "Era ties are instances of the tie contract; readout and named case."
  (:require [alexandria.non-euclidean.parallels :as p]
            [alexandria.structure.tie :as st]
            [alexandria.structure.tie-contract :refer [deftie-contract]]
            [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]))

;; Lobachevsky's angle of parallelism (non-euclidean era)
(deftie-contract parallelism p/parallelism {:tol 1e-8})

;; Ptolemy-style chord of an arc on the circle of radius 60: c = 120 sin(d/2)
(def chord
  (st/tie {:tie/kind :pivot :keys [:d :c]
           :ranges {:d [1 179 0.1] :c [(* 120 (Math/sin (/ (Math/toRadians 1) 2))) (* 120 (Math/sin (/ (Math/toRadians 179) 2))) 0.01]}
           :to-pivot {:d identity :c #(e/* 2 (e/asin (e/divide % 120)) (/ 180 Math/PI))}
           :state (fn [d] [d (e/* 120 (e/sin (e/divide (e/* d (/ Math/PI 180)) 2)))])}))
(deftie-contract chord chord {:tol 1e-8})

;; A :sources tie (any policy): the rectangle a b with area A = a b, a held
(def rectangle
  (st/tie {:tie/kind :sources :keys [:a :b :A]
           :ranges {:a [0.5 4 0.01] :b [0.5 4 0.01] :A [0.25 16 0.01]}
           :sources {:a (fn [a b _] [a b (e/* a b)]) :b (fn [a b _] [a b (e/* a b)])
                     :A (fn [a _ A] [a (e/divide A a) A])}}))

(deftest sources-tie
  (is (= [2 3 6] (st/solve rectangle :a [2 3 99])))
  (is (= [2 4 8] (st/solve rectangle :A [2 3 8]))))

(deftest round-trip-grade-numbers
  (is (every? #(< (Math/abs (double %)) 1e-9) (st/round-trip-gaps p/parallelism :x [0.1 1.0 3.0]))))

(def lobachevsky-cases
  {:cases [[:euclid [:< :x 0.02]] [:far [:< :Pi 10]] [:between :else]]})

(deftest named-case
  (is (= :euclid (st/case-of lobachevsky-cases {:x 0.01 :Pi 89.4})))
  (is (= :far (st/case-of lobachevsky-cases {:x 4 :Pi 4.2})))
  (is (= :between (st/case-of lobachevsky-cases {:x 1 :Pi 40})))
  (testing "the browser form decides the same, read from a readout array"
    (is (= '(cond (< (aget r 0) 0.02) "euclid" (< (aget r 1) 10) "far" :else "between")
           (st/case-form lobachevsky-cases (fn [k] (list 'aget 'r (p/readout-index k))))))))

(deftest readout
  (is (= {:x 0 :Pi 1 :t 2 :defect 3 :linear 4 :r 5} p/readout-index))
  (is (< (Math/abs (- (nth ((p/readout 0) nil) 1) 90.0)) 1e-12)))
