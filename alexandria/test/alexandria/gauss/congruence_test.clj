(ns alexandria.gauss.congruence-test
  "The Disquisitiones shelf: the clock tie passes the tie contract, the
   named cases, and every grade passes."
  (:require [alexandria.gauss.congruence :as c]
            [alexandria.structure.tie :as st]
            [alexandria.structure.tie-contract :refer [deftie-contract]]
            [clojure.test :refer [deftest is testing]]))

(deftie-contract clock c/clock {:tol 1e-9})

(defn- readout [& state]
  (zipmap (:outputs c/clock-readout) ((apply (st/readout-kernel c/clock-readout) state) 0)))

(deftest clock
  (is (= [12 5 29 2] (st/solve c/clock :k [12 5 0 2])) "b = a + k n")
  (is (= [12 5 29 2] (st/solve c/clock :b [12 5 29 0])))
  (testing "named live: Gauss' examples of art. 1"
    (is (= :congruent (st/case-of c/clock-cases (readout 5 -9 16 0))))
    (is (= :congruent (st/case-of c/clock-cases (readout 11 -7 15 0))))
    (is (= :incongruent (st/case-of c/clock-cases (readout 3 -7 15 0)))))
  (testing "residues on the clock (art. 4: -13 mod 5 has least residue 2)"
    (is (< (Math/abs (- 2.0 (double (:ra (readout 5 -13 0 0))))) 1e-9))))

(deftest powers
  (is (= [1 3 2 6 4 5] (c/walk 7 3)) "3 is a primitive root of 7")
  (is (= [1 2 4] (c/walk 7 2)))
  (is (= [1.0 3.0 2.0 6.0 4.0 5.0 1.0] (c/raster-walk 3 7 6)))
  (is (= {:m 7 :phi 6 :units [1 2 3 4 5 6] :orders {1 1 2 3 3 6 4 3 5 6 6 2} :roots [3 5] :name "Z/6" :cyclic? true}
         (c/modulus-facts 7)))
  (is (empty? (:roots (c/modulus-facts 8))) "no primitive root mod 8"))

(deftest reciprocity
  (let [grid (c/reciprocity-grid)]
    (is (every? :law? (apply concat grid)))
    (is (= {:p 3 :q 7 :pq -1 :qp 1 :both3 true :law? true}
           (get-in grid [0 2])))))

(deftest grades
  (doseq [{:keys [label grade]} (c/graded)]
    (is (#{:grade/proved :grade/numeric} grade) label)))
