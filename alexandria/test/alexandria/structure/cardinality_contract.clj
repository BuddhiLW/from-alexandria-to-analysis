(ns alexandria.structure.cardinality-contract
  "The conformance suite for alexandria.structure.cardinality:
   (defenumeration-contract name elements-gen) checks the Enumeration round
   trip on generated FiniteSets; (defdiagonal-contract name flip char-gen)
   checks that the diagonal of any generated list of sequences differs
   from the element at place n at place n, for every n, and is found at
   no place; (defpower-set-contract name n-gen) checks |P(X)| = 2^|X| > |X|
   and that D = {x : x not in f(x)} is missed by every generated map."
  (:require [alexandria.structure.cardinality :as c]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defmacro defenumeration-contract
  "index-of inverts nth-element on a FiniteSet from elements-gen, and an
   element not listed has no place."
  [name elements-gen & [{:keys [num-tests] :or {num-tests 60}}]]
  `(defspec ~(symbol (str name "-round-trip")) ~num-tests
     (prop/for-all [xs# (gen/vector ~elements-gen 0 12)]
       (let [E# (c/finite-set xs#)]
         (and (every? (fn [i#] (= i# (c/index-of E# (c/nth-element E# i#))))
                      (range (c/cardinal E#)))
              (nil? (c/index-of E# ::absent)))))))

(defmacro defdiagonal-contract
  "For a square list of rows (vectors of characters from char-gen) and the
   character flip: the diagonal differs from row i at place i, so it is in
   no row."
  [name flip char-gen & [{:keys [num-tests] :or {num-tests 80}}]]
  `(defspec ~(symbol (str name "-diagonal-in-no-row")) ~num-tests
     (prop/for-all [rows# (gen/bind (gen/choose 1 10)
                                    (fn [k#] (gen/vector (gen/vector ~char-gen k#) k#)))]
       (let [n# (count rows#)
             E# (c/indexed-list (fn [i#] (nth rows# i#)) n#)]
         (and (every? #(c/differs-at-place? E# ~flip %) (range n#))
              (c/in-no-place? E# ~flip n#)
              (nil? (c/index-of E# (c/diagonal-prefix E# ~flip n#))))))))

(defmacro defpower-set-contract
  "|P(X)| = 2^|X| > |X| for X = {0 .. n-1}, n from n-gen, and for every map
   f : X -> P(X) drawn at random the diagonal set D is not in the image."
  [name n-gen & [{:keys [num-tests] :or {num-tests 60}}]]
  `(defspec ~(symbol (str name "-cantor")) ~num-tests
     (prop/for-all [n# ~n-gen
                    picks# (gen/vector (gen/choose 0 1000000) 8)]
       (let [X# (c/finite-set (range n#))
             P# (c/power-set X#)
             subsets# (:xs P#)
             f# (fn [x#] (nth subsets# (mod (nth picks# x#) (count subsets#))))]
         (and (= (c/cardinal P#) (bit-shift-left 1 n#))
              (< n# (c/cardinal P#))
              (= (count subsets#) (count (distinct subsets#)))
              (c/missed? f# X#))))))
