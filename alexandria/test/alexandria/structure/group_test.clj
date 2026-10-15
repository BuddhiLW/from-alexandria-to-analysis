(ns alexandria.structure.group-test
  "Every group instance of alexandria.structure.groups passes the group
   contract; the derived operations on known cases."
  (:require [alexandria.structure.arith :as a]
            [alexandria.structure.group :as g]
            [alexandria.structure.group-contract :as c :refer [defgroup-contract]]
            [alexandria.structure.groups :as gs]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; SO(2)
(defgroup-contract so2 gs/rotations (c/scaled -7 7) c/plane-point gs/same-point?)

;; Z/n and (Z/p)^*
(def z7 (gs/cyclic 7))
(defgroup-contract z7 z7 (gen/choose 0 6) (gen/choose 0 6) =)
(def u17 (gs/units-mod 17))
(defgroup-contract u17 u17 (gen/choose 1 16) (gen/choose 0 16) =)

;; S_5
(def s5 (gs/symmetric 5))
(defgroup-contract s5 s5 (gen/shuffle (range 5)) (gen/choose 0 4) =)

;; PGL(3): products of desargues map families
(def projective-element
  (gen/fmap (fn [[t s k dx dy p q]]
              (reduce #(g/compose gs/projective %1 %2)
                      [(gs/family [:rotation t]) (gs/family [:scale s]) (gs/family [:shear k])
                       (gs/family [:translation dx dy]) [[1 0 0] [0 1 0] [p q 1]]]))
            (gen/tuple (c/scaled -3 3) (c/scaled 0.5 2) (c/scaled -1 1) (c/scaled -1 1) (c/scaled -1 1)
                       (c/scaled -0.2 0.2) (c/scaled -0.2 0.2))))
(def small-point (gen/tuple (c/scaled -1 1) (c/scaled -1 1)))
(defgroup-contract pgl3 gs/projective projective-element small-point
  (fn [p q] (every? #(< (Math/abs (double %)) 1e-6) (map - p q))))

;; Moebius motions of the disk of curvature K, K = -1, 0, +1
(def disk-point (gen/tuple (c/scaled -0.6 0.6) (c/scaled -0.6 0.6)))
(defn moebius-element [K]
  (gen/fmap (fn [[t px py]]
              (g/compose (gs/moebius-disk K) (gs/moebius-translation [px py]) (gs/moebius-rotation t)))
            (gen/tuple (c/scaled -7 7) (c/scaled -0.6 0.6) (c/scaled -0.6 0.6))))
(defn- near? [p q] (every? #(< (Math/abs (double %)) 1e-7) (map - p q)))
(defgroup-contract hyperbolic (gs/moebius-disk -1) (moebius-element -1) disk-point near?)
(defgroup-contract euclidean (gs/moebius-disk 0) (moebius-element 0) disk-point near?)
(defgroup-contract elliptic (gs/moebius-disk 1) (moebius-element 1) disk-point near?)

(deftest so2-action-proved-symbolically
  (testing "a rotation by t is multiplication by e^{i t} (Euler), exactly"
    (let [p '[x y]
          diff (map e/- (g/act gs/rotations 't p) (a/c* (a/cis 't) p))]
      (is (every? #(e/zero? (e/simplify %)) diff))))
  (testing "at K = 0 the Moebius motions are the Euclidean motions: rotate by t, then translate"
    (let [M (gs/moebius-disk 0)]
      (doseq [[t ax ay x y] [[0.7 0.2 -0.3 0.5 0.1] [-2.1 0.4 0.4 -0.3 0.6] [3.0 -0.5 0.1 0.0 -0.2]]]
        (is (near? (g/act M (g/compose M (gs/moebius-translation [ax ay]) (gs/moebius-rotation t)) [x y])
                   (a/c+ (gs/rotate [x y] t) [ax ay])))))))

(deftest derived
  (testing "orders"
    (is (= (r/ok 16) (g/order u17 3)) "3 is a primitive root mod 17 (Gauss art. 57)")
    (is (= (r/ok 8) (g/order u17 2)))
    (is (= (r/ok 7) (g/order z7 3))))
  (testing "closure"
    (is (= 16 (count (:ok (g/closure u17 [3])))))
    (is (= 120 (count (:ok (g/closure s5 [[1 2 3 4 0] [1 0 2 3 4]])))))
    (is (= 6 (count (:ok (g/closure gs/projective (take 2 (rest (gs/finite-elements [:dihedral 3])))))))
        "desargues' dihedral generators close to D3")
    (is (r/err? (g/closure gs/rotations [1] 50)) "an irrational rotation generates no finite group"))
  (testing "orbits and stabilizers"
    (let [els (:ok (g/closure u17 [9]))]
      (is (= [1 9 13 15 16 8 4 2] (g/orbit u17 els 1 =)) "the period of 8 terms through 1 (Gauss)")
      (is (= 1 (count (g/stabilizer u17 els 1 =)))))
    (let [els (gs/finite-elements [:dihedral 4])
          orb (g/orbit gs/projective els [1 0] gs/same-point?)]
      (is (= 4 (count orb)) "a vertex of the square has 4 images under D4")
      (is (= 2 (count (g/stabilizer gs/projective els [1 0] gs/same-point?))))))
  (testing "the Moebius translation moves 0 to p, its inverse moves p back"
    (let [H (gs/moebius-disk -1) T (gs/moebius-translation [0.3 0.4])]
      (is (near? (g/act H T [0 0]) [0.3 0.4]))
      (is (near? (g/act H (g/inverse H T) [0.3 0.4]) [0 0]))))
  (testing "power and commutator"
    (is (= 0 (g/power z7 3 7)))
    (is (g/same? gs/rotations (g/commutator gs/rotations 0.4 1.1) 0))
    (is (not (g/same? s5 (g/commutator s5 [1 0 2 3 4] [0 2 1 3 4]) (g/unit s5))) "S_5 is not abelian")))
