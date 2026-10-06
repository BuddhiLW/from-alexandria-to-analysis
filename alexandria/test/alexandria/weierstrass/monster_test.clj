(ns alexandria.weierstrass.monster-test
  (:require [alexandria.library :as library]
            [alexandria.weierstrass.monster :as monster]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(deftest every-check-grades-as-expected
  (let [graded (monster/graded)
        numeric (filter #(= :grade/numeric (:grade %)) graded)]
    (is (= 3 (count numeric)) "the estimate and the two sup-norm measurements are numeric")
    (is (every? #{:grade/proved} (map :grade (remove #(= :grade/numeric (:grade %)) graded))))))

(deftest the-numbers-come-from-raster-within-an-honest-libm-bound
  (let [checks (monster/numeric-checks)]
    (is (every? #(= :raster (:source %)) checks))
    (testing "the angle a^n x pi is reduced exactly, so f's error is libm's, not a^n times it"
      (doseq [x [3/10 1/7 5/11] n [10 40]]
        (let [q (* (.pow (biginteger monster/a) n) x)
              m (mod q 2)]
          (is (< (Math/abs (- (monster/cos-pi q) (Math/cos (* Math/PI (double m)))))
                 monster/libm-error)))))
    (testing "every quotient clears Weierstrass' bound by more than the libm margin"
      (doseq [{:keys [left right bound libm-margin]} (:rows (first checks))]
        (is (> (- (min (Math/abs left) (Math/abs right)) libm-margin) bound))))))

(deftest exact-angles
  (is (== 1 (monster/cos-pi 0)) "cos 0")
  (is (== -1 (monster/cos-pi (* 13 13 13 13 1)) ) "a^n odd: cos(a^n pi) = -1")
  (is (< (Math/abs (- (monster/cos-pi 1/3) 0.5)) 1e-12)))

(deftest the-difference-quotients-grow-without-bound
  (testing "at x0 = 3/10, m = 1..6: opposite signs, beyond the bound, growing like (ab)^m"
    (let [rows (map #(monster/quotients 3/10 %) (range 1 7))]
      (doseq [{:keys [left right bound]} rows]
        (is (neg? (* left right)))
        (is (>= (min (Math/abs left) (Math/abs right)) bound)))
      (is (apply < (map #(Math/abs (:left %)) rows))))))

(deftest uniform-and-not
  (testing "geometric series: the sup error halves with each term"
    (let [errs (map #(monster/sup-error monster/geometric-partial (fn [x] (/ 1 (- 1 x))) % [-0.5 0 0.5]) [5 6])]
      (is (< (Math/abs (- (/ (first errs) (second errs)) 2)) 1e-9))))
  (testing "Abel's series: near pi the error stays near pi/2"
    (is (> (monster/sup-error monster/abel-partial (fn [x] (/ x 2)) 200 monster/abel-grid) 1.4))))

(deftest the-shelf-and-the-proofs-are-data
  (is (r/ok? (library/proposition :weierstrass/nowhere-differentiable)))
  (let [res (monster/proof)]
    (is (r/ok? res))
    (is (string? (get-in res [:ok :weierstrass/monster :riemann-de])))
    (is (every? (fn [{:keys [claim why stage]}] (and (string? claim) (string? why) stage))
                (mapcat :steps (vals (:ok res)))))))
