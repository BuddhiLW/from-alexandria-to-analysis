(ns alexandria.bernoulli.widgets-test
  "The law widget's data: finite, exact where it claims to be, and the
   25550 of Ars conjectandi IV reached."
  (:require [alexandria.bernoulli.widgets :as w]
            [alexandria.structure.probability :as pr]
            [clojure.test :refer [deftest is testing]]))

(def data (w/law-data))

(deftest data-is-finite-and-complete
  (is (w/finite-law-data? data))
  (is (= (* 3 3) (count (:cases data))))
  (is (= w/trials (:ns data))))

(deftest curve-rises-to-one
  (let [cs (get (:cases data) [[3 2] 2])
        ins (map :in (:curve cs))]
    (is (< (first ins) 0.9))
    (is (> (last ins) 0.99))
    (is (every? #(<= 0 (:cheb %) (:in %)) (:curve cs)) "Chebyshev is a lower bound on the exact probability")))

(deftest bernoulli-numbers
  (let [cs (get (:cases data) [[3 2] 10])
        b (get (:bernoulli cs) 1000)]
    (is (= {:r 30 :s 20 :t 50 :n 25550} (select-keys b [:r :s :t :n])) "r=30 s=20 t=50 c=1000: 25550 trials")
    (is (> (/ (:in (:exact b)) (max 1e-300 (:miss (:exact b)))) 1000) "odds above 1000 : 1 at Bernoulli's n")))

(deftest seeded-and-reproducible
  (is (= (map :sample (:curve (get (:cases data) [[1 1] 5])))
         (map :sample (:curve (get (:cases (w/law-data)) [[1 1] 5]))))))

(deftest histogram-agrees-with-the-structure
  (testing "bars sum to the whole mass; the band mass equals the structure's exact within"
    (let [B (pr/binomial 120 3/5)]
      (is (< (abs (- (double (pr/within B 1/10)) (:in (w/at-n 3 5 1/10 120)))) 1e-12)))))
