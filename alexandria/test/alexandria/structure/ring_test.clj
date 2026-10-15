(ns alexandria.structure.ring-test
  "Z/n passes the ring contract; the Legendre symbol the character
   contract; units, rows and structure names on known cases."
  (:require [alexandria.structure.groups :as gs]
            [alexandria.structure.ring :as ring]
            [alexandria.structure.ring-contract :refer [defring-contract defcharacter-contract]]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]))

(defring-contract z12 (ring/zmod 12) (gen/choose -40 40))
(defring-contract z17 (ring/zmod 17) (gen/choose 0 16))
(defcharacter-contract legendre-23 (gs/units-mod 23) #(ring/legendre % 23) (gen/choose 1 22))
(defcharacter-contract legendre-31 (gs/units-mod 31) #(ring/legendre % 31) (gen/choose 1 30))

(deftest congruence
  (is (ring/congruent? 5 -9 16) "Gauss art. 1: -9 and +16 are congruent mod 5")
  (is (ring/congruent? 11 -7 15) "-7 is a residue of 15 mod 11")
  (is (not (ring/congruent? 3 -7 15)) "but a nonresidue mod 3"))

(deftest units-and-rows
  (is (= [1 5 7 11] (ring/units (ring/zmod 12))))
  (is (= [0 2 4 6 8 10 0 2 4 6 8 10] (ring/row (ring/zmod 12) 2)))
  (is (ring/permutation-row? (ring/zmod 12) 5))
  (is (not (ring/permutation-row? (ring/zmod 12) 2)))
  (is (= (gs/cyclic 12) (ring/additive (ring/zmod 12)))))

(deftest structure-named
  (testing "the units mod n as a product of cyclic groups"
    (let [nm (fn [n] (let [{G :group els :elements} (ring/unit-group (ring/zmod n))] (ring/structure G els)))]
      (is (= {:order 16 :cyclic? true :factors [16] :name "Z/16"} (nm 17)))
      (is (= "Z/2 × Z/2" (:name (nm 8))) "(Z/8)^* is Klein's four-group")
      (is (= "Z/2 × Z/4" (:name (nm 15))))
      (is (= "Z/2 × Z/2 × Z/2" (:name (nm 24))))
      (is (= "Z/20" (:name (nm 25))))
      (is (:cyclic? (nm 18)))
      (is (= [3 5] (let [{G :group els :elements} (ring/unit-group (ring/zmod 7))] (ring/generators G els)))))))

(deftest legendre
  (is (= [1 2 4] (ring/residues 7)))
  (is (= [1 1 -1 1 -1 -1] (map #(ring/legendre % 7) (range 1 7))))
  (is (= 0 (ring/legendre 14 7)))
  (testing "Gauss' art. 131, a few cells"
    (is (= -1 (* (ring/legendre 3 7) (ring/legendre 7 3))) "3 and 7 are both 3 mod 4")
    (is (= 1 (* (ring/legendre 5 7) (ring/legendre 7 5))))))
