(ns alexandria.structure.cardinality-test
  "The instances pass the cardinality contract; Cantor's 1891 table agrees
   with the structure; Cantor's theorem for every map on sets of up to
   three elements."
  (:require [alexandria.cantor.sets :as sets]
            [alexandria.structure.cardinality :as c]
            [alexandria.structure.cardinality-contract :refer [defenumeration-contract defdiagonal-contract
                                                              defpower-set-contract]]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]))

(defenumeration-contract ints (gen/choose 0 20))
(defenumeration-contract words gen/string-alphanumeric)
(defdiagonal-contract mw c/mw-flip (gen/elements [:m :w]))
(defdiagonal-contract bits c/bit-flip (gen/elements [0 1]))
(defpower-set-contract small (gen/choose 0 6))

(deftest cantor-1891-theorem-for-every-small-map
  (testing "every map X -> P(X) misses its diagonal set, |X| <= 3"
    (doseq [n (range 0 4)
            :let [r (c/cantor-report n)]]
      (is (= (:maps r) (:missed r)) (str "n = " n))
      (is (= (:power-size r) (long (Math/pow 2 n))))
      (is (< (:size r) (:power-size r))))
    (is (= [1 2 16 512] (mapv #(:maps (c/cantor-report %)) [0 1 2 3]))
        "(2^n)^n maps for n = 0, 1, 2, 3")))

(deftest the-table-of-1891-agrees-with-the-structure
  (let [rows (mapv :row (sets/binary-rows 8))
        E (c/indexed-list #(nth rows %) 8)]
    (is (= (sets/diagonal-element rows) (c/diagonal-prefix E c/mw-flip 8)))
    (is (sets/in-no-row? rows))
    (is (c/in-no-place? E c/mw-flip 8))))

(deftest the-two-proofs-are-one
  (testing "D = {n : n not in f(n)} with f(n) = {k : E_n(k) = w} is the w-places of the diagonal"
    (let [rows (mapv :row (sets/binary-rows 8))
          E (c/indexed-list #(nth rows %) 8)
          f (c/sequences->subsets E 8 8)
          X (c/finite-set (range 8))
          D (c/power-set-diagonal f X)]
      (is (= D (c/characteristic-set (c/diagonal E c/mw-flip) 8)))
      (is (c/missed? f X)))))
