(ns alexandria.structure.probability-test
  "FiniteSpace and Binomial pass the probability contract; the n-fold
   product of a coin is the binomial; Bernoulli's 25550; the exact tail is
   monotone in n."
  (:require [alexandria.structure.probability :as pr]
            [alexandria.structure.probability-contract :refer [defprob-contract defbinomial-contract]]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defprob-contract fair-die (pr/uniform [1 2 3 4 5 6]))
(defprob-contract loaded-coin (pr/bernoulli 3/10))
(defprob-contract two-dice (pr/pushforward (pr/product (pr/uniform [1 2 3 4 5 6]) (pr/uniform [1 2 3 4 5 6])) #(apply + %)))
(defprob-contract binomial-12 (pr/binomial 12 3/5))
(defbinomial-contract b12 12 3/5)
(defbinomial-contract b200 200 1/3)

(defspec binomial-tail-monotone-in-n 40
  (prop/for-all [n (gen/choose 1 40) k (gen/choose 0 10) a (gen/choose 1 6)]
    (let [tail (fn [m] (pr/measure (pr/binomial m (/ a 7)) #(>= % k)))]
      (<= (tail n) (tail (inc n))))))

(deftest product-is-binomial
  (let [F (pr/pushforward (pr/n-fold (pr/bernoulli 3/5) 6) #(reduce + %))
        B (pr/binomial 6 3/5)]
    (is (every? #(= (pr/measure F #{%}) (pr/measure B #{%})) (range 7)))))

(deftest fermat-eleven-sixteenths
  (testing "Pascal's 2 against 3: the first wins in 11 of 16 four-round outcomes"
    (is (= 11/16 (pr/measure (pr/binomial 4 1/2) #(>= % 2))))))

(deftest large-n-is-exact
  (let [B (pr/binomial 3000 3/5)
        w (pr/within B 1/10)]
    (is (= 1 (pr/mass B)))
    (is (= 1800 (pr/expectation B identity)))
    (is (and (ratio? w) (< 99/100 w 1)) "an exact rational strictly below 1")))

(deftest bernoulli-25550
  (let [r (pr/bernoulli-n 30 20 1000)]
    (is (= 50 (:t r)))
    (is (= [301 24728] [(:m1 r) (:n1 r)]))
    (is (= [211 25550] [(:m2 r) (:n2 r)]))
    (is (= 25550 (:n r))))
  (testing "at 25550 trials the band 3/5 +- 1/50 holds with odds above 1000 : 1, exactly"
    (let [p (pr/within (pr/binomial 25550 3/5) 1/50)]
      (is (> p (/ 1000 1001))))))
