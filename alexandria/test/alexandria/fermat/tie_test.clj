(ns alexandria.fermat.tie-test
  (:require [alexandria.fermat.tie :as tie]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [emmy.env :as e]))

(deftest every-claim-of-the-tie-is-proved
  (let [g (tie/graded)]
    (is (= 11 (count g)))
    (is (every? #{:grade/proved} (map :grade g)) (pr-str (remove (comp #{:grade/proved} :grade) g)))))

(deftest every-locus-is-named-by-its-invariants
  (let [g (tie/loci-graded)]
    (is (every? #{:grade/proved} (map :grade g)) (pr-str (remove (comp #{:grade/proved} :grade) g))))
  (testing "D = 1, B = 1.5, Z = 1, k = 2.5"
    (is (= {:line :line :hyperbola :hyperbola :parabola :parabola :circle :circle
            :ellipse :ellipse :hyperbola-ratio :hyperbola}
           (into {} (for [k tie/loci-types]
                      [k (tie/kind (mapv double (tie/invariants (tie/general-form k 1 1.5 1 2.5))))]))))
    (is (= :circle (tie/kind (tie/invariants (tie/general-form :ellipse 1 1.5 1 1)))) "ratio 1: the circle")))

(def positive (gen/fmap (fn [[n d]] (/ n d)) (gen/tuple (gen/choose 1 30) (gen/choose 1 7))))

(deftest the-tie-round-trips-on-exact-numbers
  (let [result (tc/quick-check 80
                 (prop/for-all [p positive d positive]
                   (let [[ce np] (tie/forward p d)]
                     (and (= d (tie/d-from-ce ce)) (= d (tie/d-from-np p np))
                          (= 0 (e/simplify (tie/discriminant p d np)))))))]
    (is (:pass? result) (pr-str (dissoc result :result-data)))))

(deftest the-derivation-has-every-step
  (let [steps (tie/derivation)]
    (is (= 7 (count steps)))
    (is (e/zero? (e/simplify (e/- (second (first (:eqs (nth steps 5)))) (e/+ 'd (e// 'p 2)))))
        "Emmy's root of the discriminant is the foot d + p/2")))

(deftest every-claim-of-the-conic-tie-is-proved
  (let [g (tie/conic-graded)]
    (is (= 15 (count g)))
    (is (every? #{:grade/proved} (map :grade g)) (pr-str (remove (comp #{:grade/proved} :grade) g))))
  (let [steps (tie/conic-derivation)]
    (is (= 7 (count steps)))
    (is (e/zero? (e/simplify (e/- (second (first (:eqs (nth steps 5)))) (e/+ (e/* 'd (e/+ 1 'k)) (e// 'p 2)))))
        "the double root's centre is d (1 + k) + p/2")))

(deftest the-conic-tie-round-trips-on-exact-numbers
  (let [result (tc/quick-check 80
                 (prop/for-all [p positive d positive c (gen/elements [-1/2 -1/3 1/4 1/2 1 3/2])]
                   ;; At p + 2 c d = 0 the conic has a horizontal tangent:
                   ;; the subtangent CE is at infinity, outside this inverse map.
                   (if (zero? (+ p (* 2 c d)))
                     true
                     (let [[_ np] (tie/conic-forward p c d)]
                       (and (= d (tie/conic-d-from-np p c np))
                            (= 0 (e/simplify (tie/conic-discriminant p c d np))))))))]
    (is (:pass? result) (pr-str (dissoc result :result-data)))))
