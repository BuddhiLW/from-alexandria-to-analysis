(ns alexandria.structure.probability-contract
  "The conformance suite every alexandria.structure.probability instance
   passes: (defprob-contract name S) defines, as test.check properties,
   total mass 1, masses in [0,1], additivity on disjoint events, the
   complement, linearity of expectation and expectation of an indicator.
   (defbinomial-contract name n p) adds: the pmf sums to 1 exactly and the
   mean is n p. The exact upper tail P(S_n >= k) is monotone in n
   (probability-test, binomial-tail-monotone-in-n)."
  (:require [alexandria.structure.probability :as pr]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn event-gen
  "An event of S: the set of a few of its outcomes."
  [S]
  (gen/fmap set (gen/vector (gen/elements (pr/outcomes S)))))

(defn numeric-key
  "A fixed numeric function of an outcome, for the linearity law: the
   outcome itself when a number, else its hash mod 7."
  [o]
  (if (number? o) o (mod (hash o) 7)))

(defmacro defprob-contract
  "Properties for the space S; events are subsets of its outcomes."
  [name S & [{:keys [num-tests] :or {num-tests 60}}]]
  (let [n #(symbol (str name "-" %))]
    `(do
       (defspec ~(n "total-mass-one") 1
         (prop/for-all [_# (gen/return nil)] (= 1 (pr/mass ~S))))
       (defspec ~(n "mass-in-unit-interval") ~num-tests
         (prop/for-all [e# (event-gen ~S)]
           (<= 0 (pr/measure ~S e#) 1)))
       (defspec ~(n "additive-on-disjoint") ~num-tests
         (prop/for-all [a# (event-gen ~S) b# (event-gen ~S)]
           (let [b'# (fn [o#] (and (b# o#) (not (a# o#))))]
             (= (pr/measure ~S (fn [o#] (or (a# o#) (b'# o#))))
                (+ (pr/measure ~S a#) (pr/measure ~S b'#))))))
       (defspec ~(n "complement") ~num-tests
         (prop/for-all [e# (event-gen ~S)]
           (= 1 (+ (pr/measure ~S e#) (pr/complement-mass ~S e#)))))
       (defspec ~(n "expectation-linear") ~num-tests
         (prop/for-all [x# (gen/choose -9 9) y# (gen/choose -9 9)]
           (= (+ (* x# (pr/expectation ~S numeric-key)) y#)
              (pr/expectation ~S (fn [o#] (+ (* x# (numeric-key o#)) y#))))))
       (defspec ~(n "expectation-of-indicator") ~num-tests
         (prop/for-all [e# (event-gen ~S)]
           (= (pr/measure ~S e#) (pr/expectation ~S (fn [o#] (if (e# o#) 1 0)))))))))

(defmacro defbinomial-contract
  "Binomial(n, p): pmf sums to 1, mean n p."
  [name n p]
  (let [nm #(symbol (str name "-" %))]
    `(do
       (defspec ~(nm "pmf-sums-to-one") 1
         (prop/for-all [_# (gen/return nil)]
           (let [B# (pr/binomial ~n ~p)]
             (= 1 (reduce + 0N (map #(pr/measure B# #{%}) (pr/outcomes B#)))))))
       (defspec ~(nm "mean-is-np") 1
         (prop/for-all [_# (gen/return nil)]
           (= (* ~n (rationalize ~p)) (pr/expectation (pr/binomial ~n ~p) identity)))))))
