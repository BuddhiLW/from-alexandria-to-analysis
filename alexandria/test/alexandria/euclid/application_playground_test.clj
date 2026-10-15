(ns alexandria.euclid.application-playground-test
  (:require [alexandria.euclid.application-playground :as pg]
            [clojure.test :refer [deftest is testing]]))

(defn- row [m] (pg/named (mapv double ((apply pg/figure (map m pg/order)) [0]))))
(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-7))

(deftest euclid-start-is-solved
  (let [{:keys [S y0 residual S-max y-max]} (row pg/start)]
    (is (close? S 21)) (is (close? y0 3)) (is (close? residual 0))
    (testing "VI.27: k = -1, the greatest is the square on half the line"
      (is (close? S-max 25)) (is (close? y-max 5)))))

(deftest the-solved-height-applies-the-given-area
  (doseq [m [{:a 10 :y 1 :k 0 :theta 60 :S0 21}
             {:a 7 :y 1 :k 0.5 :theta 120 :S0 13}
             {:a 9 :y 1 :k -0.6 :theta 75 :S0 12}]]
    (let [{:keys [y0]} (row m)
          {:keys [S]} (row (assoc m :y y0))]
      (is (close? S (:S0 m)) (str m)))))

(deftest the-diorismos-holds-the-given-area
  (testing "past VI.27's greatest the solved height stops at y-max (a double
            root there: the square root turns rounding of 1e-10 into 1e-4)"
    (let [{:keys [y0 y-max S-max]} (row (:impossible pg/presets))]
      (is (< S-max 32))
      (is (< (Math/abs (- y0 y-max)) 1e-3)))))

(deftest cases-and-parallelograms
  (is (= :ellipse (pg/kind -1)) ) (is (= :parabola (pg/kind 0))) (is (= :hyperbola (pg/kind 0.5)))
  (testing "applied + leftover = the strip on AB, in every case and angle"
    (doseq [k [-1 -0.3 0 0.4 1] th [30 90 140]]
      (let [{:keys [S left sin]} (row {:a 8 :y 2 :k k :theta th :S0 1})]
        (is (close? (- S left) (* 8 2 sin)))))))
