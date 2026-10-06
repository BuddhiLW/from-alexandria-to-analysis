(ns alexandria.lebesgue.measure-test
  (:require [alexandria.lebesgue.measure :as measure]
            [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as r]))

(deftest every-check-is-proved
  (is (every? #{:grade/proved} (map :grade (measure/graded)))))

(defspec the-cover-is-shorter-than-eps 50
  (prop/for-all [k (gen/choose 1 1000) N (gen/choose 1 60)]
    (let [eps (/ 1 k) cv (measure/cover eps N)]
      (and (= (measure/total-length cv) (* eps (- 1 (/ 1 (bigint (.pow (biginteger 2) N))))))
           (< (measure/union-length cv) eps)
           (every? (fn [{:keys [r] [lo hi] :interval}] (< lo r hi)) cv)))))

(deftest the-rationals-of-the-unit-interval
  (is (= [0 1 1/2 1/3 2/3 1/4 1/5 3/4] (take 8 (measure/rationals-01))))
  (is (apply distinct? (take 300 (measure/rationals-01)))))

(defspec riemann-sums-never-meet 50
  (prop/for-all [cuts (gen/vector (gen/let [m (gen/choose 1 99)] (/ m 100)) 0 20)]
    (= {:S 1 :s 0} (measure/darboux-sums (sort (distinct (concat [0 1] cuts)))))))

(deftest lebesgue-integral-of-chi
  (is (zero? (measure/lebesgue-sum))))

(deftest the-cover-figure
  (let [f (measure/covering 0.5)]
    (is (= [0.375 -0.05] (mapv double (f [0.5 1 0]))))
    (is (= [0.625 -0.05] (mapv double (f [0.5 1 1]))))))

(deftest the-shelf-and-proofs-are-data
  (is (= "Henri Lebesgue" (get-in (:ok (library/shelves)) [:lebesgue/rationals-null :author])))
  (let [p (:ok (measure/proof))]
    (is (r/ok? (measure/proof)))
    (is (= [4 6] (map #(count (get-in p [% :steps])) [:lebesgue/measure-zero :lebesgue/dirichlet])))
    (is (every? :original (get-in p [:lebesgue/introduction :passages])))))
