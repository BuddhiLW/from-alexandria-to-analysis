(ns alexandria.structure.boolring-test
  "BoolRing passes the ring contract and the idempotence contract; Z/12 does
   not pass idempotence (that is what separates Boole's algebra from
   ordinary algebra); elimination, development and the truth table on
   Boole's own cases."
  (:require [alexandria.structure.boolring :as b]
            [alexandria.structure.boolring-contract :refer [defboolring-contract]]
            [alexandria.structure.ring :as ring]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]))

(def U #{:a :b :c :d})
(def R (b/boolring U))
(def classes (gen/fmap set (gen/vector (gen/elements (vec U)) 0 4)))
(def two (b/boolring #{:t}))

(defboolring-contract bool4 R classes)

(deftest idempotence-separates-the-algebras
  (is (b/boolean-ring? R))
  (is (b/boolean-ring? (b/boolring #{1 2 3})))
  (is (not (b/idempotent? (ring/zmod 12) 2)) "2 * 2 = 4 in Z/12")
  (is (not (b/boolean-ring? (ring/zmod 12))))
  (testing "in Z/n the idempotents are the roots of x^2 = x: only 0 and 1 for n prime"
    (is (= [0 1] (filter #(b/idempotent? (ring/zmod 7) %) (ring/elements (ring/zmod 7)))))
    (is (= [0 1 4 9] (filter #(b/idempotent? (ring/zmod 12) %) (ring/elements (ring/zmod 12)))))))

(deftest the-ring-operations
  (is (= #{:a} (ring/mul R #{:a :b} #{:a :c})) "xy: the objects both names apply to")
  (is (= #{:b :c} (ring/add R #{:a :b} #{:a :c})) "sum: symmetric difference")
  (is (= #{:c :d} (b/complement-of R #{:a :b})) "1 - x")
  (is (= #{:a :b :c} (ring/add R (ring/add R #{:a :b} #{:b :c}) (ring/mul R #{:a :b} #{:b :c})))
      "x + y + xy is the union (x + y - xy, and - is + here)")
  (is (= 16 (count (ring/elements R)))))

(deftest elimination
  (testing "Boole VII Prop. I: f(x) = 0 gives f(1) f(0) = 0"
    (let [g (b/eliminate two (fn [[x y]] (ring/mul two x (b/complement-of two y))) 0)]
      (is (b/holds? two g [#{}]) "x(1-y) = 0: (1-y)*0 = 0, nothing is learned about y")
      (is (b/holds? two g [#{:t}])))
    (let [g (b/eliminate two (fn [[x y]] (ring/add two x y)) 0)]
      (is (b/holds? two g [#{}]) "x + y = 0: (1+y) y = 0 for both y")
      (is (b/holds? two g [#{:t}])))
    (let [g (b/eliminate two (fn [[x]] x) 0)]
      (is (b/holds? two g []) "f = x: f(1) f(0) = 1 * 0 = 0"))
    (let [g (b/eliminate two (fn [[x]] (ring/one two)) 0)]
      (is (not (b/holds? two g [])) "f = 1: 1 = 0 is false, the residue is the absurd equation"))))

(deftest development
  (let [f (fn [[x y]] (ring/add R (ring/mul R x y) x))
        dev (b/develop R f 2)]
    (is (= [[0 0] [0 1] [1 0] [1 1]] (map :bits dev)))
    (doseq [[xs ys] [[#{:a :b} #{:b :c}] [#{} #{:a}] [U #{:d}]]]
      (is (ring/same? R (f [xs ys]) (b/developed-value R dev [xs ys]))
          "f equals the sum of f(bits) times the constituents"))))

(deftest truth-table
  (testing "all Y are X is y(1-x) = 0, over the 2^2 assignments of [y x]"
    (is (= [[0 0] [0 1] [1 1]]
           (b/eq-zero-points [(fn [[y x]] (ring/mul two y (b/complement-of two x)))] 2)))))
