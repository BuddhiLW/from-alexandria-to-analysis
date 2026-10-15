(ns alexandria.bernoulli.brachistochrone-test
  (:require [alexandria.bernoulli.brachistochrone :as b]
            [clojure.test :refer [deftest is testing]]))

(deftest graded-holds
  (let [gs (b/graded)]
    (is (= 10 (count gs)))
    (is (every? #(not= :grade/fails (:grade %)) gs))
    (is (= 6 (count (filter #(= :grade/proved (:grade %)) gs))))))

(deftest cycloid-through-two-points
  (testing "Bernoulli's own B = (pi a, 2a) ends at theta = pi, r = a"
    (let [{:keys [theta r T]} (b/through Math/PI 2.0)]
      (is (< (Math/abs (- theta Math/PI)) 1e-9))
      (is (< (Math/abs (- r 1.0)) 1e-9))
      (is (< (Math/abs (- T (* Math/PI (Math/sqrt (/ 1.0 b/g))))) 1e-9))))
  (testing "a shallow B: the cycloid dips below B (theta > pi)"
    (is (> (:theta (b/through 2.0 1.0)) Math/PI))))

(deftest race-order
  (doseq [[X Y] [[2.0 1.0] [1.0 1.0] [Math/PI 2.0]]]
    (is (= [:cycloid :arc :line] (b/finishing-order (b/times X Y))) [X Y])))
