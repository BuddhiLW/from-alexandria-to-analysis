(ns alexandria.structure.boolring
  "Domain stratum: Boole's algebra of classes (An Investigation of the Laws
   of Thought, 1854) as a special case of alexandria.structure.ring.

   BoolRing   the powerset ring of a finite universe: a class is a subset,
              the product x y the intersection (Boole II. 6: the objects to
              which both names apply), the sum the symmetric difference, 1
              the universe, 1 - x the complement. It is a Ring, and its
              additive group is the symmetric-difference group.

   Extra law  idempotence, x x = x (II. 9), which no ring of numbers obeys
              beyond {0, 1}. It gives x (1 - x) = 0 (III. 1), x + x = 0, and
              makes every polynomial reducible to multilinear form.
              (test/alexandria/structure/ring_contract.clj
              defidempotent-contract checks it.)

   Pure operations
     develop     f = sum over constituents of f(bits) * constituent (V. 10);
                 for one variable f(x) = f(1) x + f(0) (1 - x).
     eliminate   the complete result of eliminating x from f(x) = 0 is
                 f(1) f(0) = 0 (VII, Prop. I).
     syllogism   from the equations of two premises, eliminate the middle
                 term and read the residual equation."
  (:require [alexandria.structure.group :as g]
            [alexandria.structure.ring :as ring]))

(defn- sym-diff [a b] (into (set (remove (set b) a)) (remove (set a) b)))

(defn- powerset [u]
  (reduce (fn [acc x] (into acc (map #(conj % x) acc))) [#{}] (sort u)))

(defrecord SymDiff []
  g/Group
  (unit [_] #{})
  (compose [_ a b] (sym-diff a b))
  (inverse [_ a] (set a))
  (same? [_ a b] (= (set a) (set b)))
  g/Action
  (act [_ g x] (sym-diff g x)))

(defrecord BoolRing [universe]
  ring/Ring
  (zero [_] #{})
  (one [_] (set universe))
  (add [_ a b] (sym-diff a b))
  (neg [_ a] (set a))
  (mul [_ a b] (into #{} (filter (set b)) a))
  (same? [_ a b] (= (set a) (set b)))
  (elements [_] (powerset universe))
  (additive [_] (->SymDiff)))

(defn boolring
  "The powerset ring of the finite set universe."
  [universe]
  (->BoolRing (set universe)))

(defn idempotent?
  "x x = x in R, the law of Boole II. 9. Z/12 fails it (2 * 2 = 4); a
   Boolean ring obeys it."
  [R a]
  (ring/same? R (ring/mul R a a) a))

(defn boolean-ring?
  "True when every element of the finite ring R is idempotent."
  [R]
  (every? #(idempotent? R %) (ring/elements R)))

(defn complement-of "1 - a in R." [R a] (ring/add R (ring/one R) (ring/neg R a)))

(defn join-in
  "a or b = a + b - ab in R (in a Boolean ring - is +): the union of two
   classes. a = 0 and b = 0 together hold exactly when (join-in a b) = 0."
  [R a b]
  (ring/add R (ring/add R a b) (ring/mul R a b)))

(defn bit-vectors "All 2^n vectors of 0/1, in counting order." [n]
  (if (zero? n) [[]] (vec (for [b (bit-vectors (dec n)) x [0 1]] (conj b x)))))

(defn constituent
  "Product of x_i (bit 1) or 1 - x_i (bit 0) over xs: one of the 2^n
   constituents of Boole V. 10."
  [R xs bits]
  (reduce (fn [acc [x b]] (ring/mul R acc (if (= 1 b) x (complement-of R x))))
          (ring/one R) (map vector xs bits)))

(defn develop
  "Boole's development of f (a function of a vector of n elements of R):
   [{:bits [..] :coef f(bits read as 1 and 0)} ...], one per constituent.
   For n = 1: f = f(1) x + f(0) (1 - x). Pure."
  [R f n]
  (mapv (fn [bits] {:bits bits :coef (f (mapv #(if (= 1 %) (ring/one R) (ring/zero R)) bits))})
        (bit-vectors n)))

(defn developed-value
  "The sum of coef * constituent over a development at the elements xs. In a
   Boolean ring it equals (f xs) for any polynomial f."
  [R dev xs]
  (reduce (fn [acc {:keys [bits coef]}]
            (ring/add R acc (ring/mul R coef (constituent R xs bits))))
          (ring/zero R) dev))

(defn eliminate
  "Eliminate the i-th variable from f(x_0 .. x_n-1) = 0 (Boole VII, Prop.
   I): the complete result is f(..1..) f(..0..) = 0. Returns the function of
   the remaining n - 1 elements. Pure."
  [R f i]
  (fn [ys]
    (let [at (fn [v] (f (vec (concat (take i ys) [v] (drop i ys)))))]
      (ring/mul R (at (ring/one R)) (at (ring/zero R))))))

;; ---------------------------------------------------------------------------
;; Equations as functions on the two-valued algebra

(defn holds?
  "The equation f(x..) = 0 holds in R at the elements xs."
  [R f xs]
  (ring/same? R (f xs) (ring/zero R)))

(defn eq-zero-points
  "The 0/1 assignments (bit vectors over n variables) satisfying every
   equation in fs: Boole's truth table, in the 2-element ring."
  [fs n]
  (let [R (boolring #{:t})]
    (filterv (fn [bits] (every? #(holds? R % (mapv (fn [b] (if (= 1 b) #{:t} #{})) bits)) fs))
             (bit-vectors n))))
