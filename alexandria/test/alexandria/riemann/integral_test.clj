(ns alexandria.riemann.integral-test
  (:require [alexandria.library :as library]
            [alexandria.riemann.integral :as integral]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(deftest every-check-grades-as-expected
  (let [graded (integral/graded)]
    (is (= [:grade/numeric] (mapv :grade (filter #(= :grade/numeric (:grade %)) graded))))
    (is (every? #{:grade/proved} (map :grade (remove #(= :grade/numeric (:grade %)) graded))))))

(deftest riemanns-bracket
  (is (= [0 1/4 -1/4 0 3/10 -3/10] (map integral/bracket [0 1/4 3/4 1/2 13/10 17/10])))
  (is (= 0 (integral/bracket -1/2)))
  (is (= [1/2 -1/2] [(integral/bracket-left 1/2) (integral/bracket-right 1/2)])))

(deftest tagged-sums-lie-between-the-darboux-sums
  (let [f #(* % %)
        xs (integral/partition-points 0 1 12)
        {:keys [upper lower]} (integral/darboux-sums (integral/monotone-sup-inf f) xs)]
    (doseq [eps [(repeat 0) (repeat 1) (repeat 1/2) (cycle [1/7 5/6 2/3])]]
      (is (<= lower (integral/riemann-sum f xs eps) upper)))
    (is (= 1/12 (- upper lower)))))

(deftest the-partial-sums-jump-where-riemann-says
  (testing "a jump at p/(2k) for k <= N, none at odd denominators"
    (is (every? #(neg? (integral/jump 6 %)) [1/2 1/4 3/4 1/6 5/6 1/10 1/12]))
    (is (every? #(zero? (integral/jump 6 %)) [1/3 2/5 1/7 3/7])))
  (testing "between jumps S_N rises with slope H_N (no jump of S_5 in [31/100, 32/100])"
    (is (= [] (integral/breakpoints 5 31/100 32/100)))
    (is (= (* 1/100 (integral/harmonic 5))
           (- (integral/partial-sum 5 32/100) (integral/partial-sum 5 31/100))))))

(deftest upper-and-lower-close-on-each-other
  (let [rows (map #(integral/sums-of-partial 8 %) [4 8 16 32 64])]
    (is (apply >= (map :oscillation rows)))
    (is (< (:oscillation (last rows)) 1/10))
    (is (apply <= (map :lower rows)))
    (is (apply >= (map :upper rows)))))

(deftest the-shelf-and-the-proofs-are-data
  (is (r/ok? (library/proposition :riemann/integral-6)))
  (let [res (integral/proof)]
    (is (r/ok? res))
    (is (every? (fn [{:keys [claim why stage]}] (and (string? claim) (string? why) stage))
                (mapcat :steps (vals (:ok res)))))))
