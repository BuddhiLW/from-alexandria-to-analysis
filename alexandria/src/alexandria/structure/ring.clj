(ns alexandria.structure.ring
  "Domain stratum: a commutative ring with unit, its group of units, and a
   character of that group.

   Ring       (zero R) (one R) (add R a b) (neg R a) (mul R a b),
              (same? R a b), (elements R) for a finite ring, and
              (additive R), the additive group as an
              alexandria.structure.group/Group.
   Character  a homomorphism chi from a group to {+1, -1} under *:
              (chi (compose a b)) = (chi a) (chi b).

   Laws (test/alexandria/structure/ring_contract.clj checks them on
   generated elements for every instance): + associative and commutative
   with 0 and negatives, * associative and commutative with 1, *
   distributes over +; a character is multiplicative.

   Derived for every finite instance: unit? and units, the row of a times
   every element (a permutation exactly when a is a unit), and the
   structure of a finite abelian group named as a product of cyclic
   groups (cyclic or not), from element orders alone.

   Instance: zmod, the residues modulo n (Gauss, Disquisitiones
   arithmeticae art. 1-4; the clock of n hours); its additive group is
   structure.groups/cyclic, its units structure.groups/units-mod. The
   Legendre symbol (a/p) is the character of (Z/p)^* onto {+1, -1}
   (Euler's criterion a^((p-1)/2), DA art. 106), 0 on multiples of p."
  (:require [alexandria.structure.group :as g]
            [alexandria.structure.groups :as gs]
            [clojure.string :as str]))

(defprotocol Ring
  (zero [R] "The additive identity.")
  (one [R] "The multiplicative identity.")
  (add [R a b] "a + b.")
  (neg [R a] "-a.")
  (mul [R a b] "a b.")
  (same? [R a b] "True when a and b are the same element of R.")
  (elements [R] "The elements of a finite ring, in order (nil otherwise).")
  (additive [R] "The additive group, an alexandria.structure.group/Group."))

;; ---------------------------------------------------------------------------
;; Z/n

(defn gcd [a b] (if (zero? b) (Math/abs (long a)) (recur b (mod a b))))

(defrecord ZMod [n]
  Ring
  (zero [_] 0)
  (one [_] (mod 1 n))
  (add [_ a b] (mod (+ a b) n))
  (neg [_ a] (mod (- a) n))
  (mul [_ a b] (mod (* a b) n))
  (same? [_ a b] (= (mod a n) (mod b n)))
  (elements [_] (vec (range n)))
  (additive [_] (gs/cyclic n)))

(defn zmod "Z/n: the residues 0..n-1 modulo n (Gauss' residua minima)." [n] (->ZMod n))

(defn congruent?
  "a = b (mod n): n measures their difference (DA art. 1)."
  [n a b]
  (same? (zmod n) a b))

;; ---------------------------------------------------------------------------
;; Units and the multiplication rows

(defn unit?
  "a has an inverse under * in R (searched among the elements)."
  [R a]
  (boolean (some #(same? R (mul R a %) (one R)) (elements R))))

(defn units "The units of a finite ring, in order." [R] (filterv #(unit? R %) (elements R)))

(defn unit-group
  "The multiplicative group of units of Z/n: {:group (structure.groups/
   units-mod n) :elements [unit ...]}."
  [R]
  {:group (gs/units-mod (:n R)) :elements (units R)})

(defn row
  "a times every element, in order: a row of the multiplication table."
  [R a]
  (mapv #(mul R a %) (elements R)))

(defn permutation-row?
  "True when the row of a is a permutation of the elements."
  [R a]
  (= (count (elements R)) (count (distinct (row R a)))))

;; ---------------------------------------------------------------------------
;; A finite abelian group named: cyclic or a product

(defn element-orders
  "{element order} in the group G over its listed elements."
  [G els]
  (into (sorted-map) (map (fn [x] [x (:ok (g/order G x))]) els)))

(defn generators "The elements whose powers fill G." [G els]
  (let [n (count els)] (filterv #(= n (:ok (g/order G %))) els)))

(defn- prime? [p] (and (> p 1) (not-any? #(zero? (mod p %)) (range 2 p))))

(defn- elementary-divisors
  "{p [k ...]}: the exponents of the cyclic p-power factors of the abelian
   group whose element orders are ords. #{x : x^(p^j) = 1} has
   p^(sum over factors of min(k, j)) elements, so its log_p rises by
   d_j = #{factors with k >= j} from j - 1 to j."
  [ords n]
  (into (sorted-map)
        (for [p (filter #(and (prime? %) (zero? (mod n %))) (range 2 (inc n)))]
          (let [pj (fn [j] (long (Math/pow p j)))
                cnt (fn [j] (count (filter #(zero? (mod (pj j) %)) ords)))
                lg (fn [m] (long (Math/round (/ (Math/log m) (Math/log p)))))
                ds (loop [j 1 acc []]
                     (let [d (- (lg (cnt j)) (lg (cnt (dec j))))]
                       (if (zero? d) acc (recur (inc j) (conj acc d)))))
                d (fn [k] (get ds (dec k) 0))]
            [p (vec (for [k (range (count ds) 0 -1) _ (range (- (d k) (d (inc k))))] k))]))))

(defn structure
  "The finite abelian group G (elements els) as a product of cyclic groups:
   {:order n :cyclic? bool :factors [m ...] :name \"Z/2 × Z/4\"}, the
   factors invariant factors (each divides the next)."
  [G els]
  (let [n (count els)
        pp (elementary-divisors (vals (element-orders G els)) n)
        width (reduce max 0 (map count (vals pp)))
        factors (vec (sort (for [i (range width)]
                             (reduce * (for [[p ks] pp :let [k (get ks i)] :when k] (long (Math/pow p k)))))))]
    {:order n :cyclic? (<= (count factors) 1) :factors factors
     :name (if (empty? factors) "the trivial group" (str/join " × " (map #(str "Z/" %) factors)))}))

;; ---------------------------------------------------------------------------
;; Characters: the Legendre symbol

(defn power-mod [b x m]
  (loop [r (mod 1 m) b (mod b m) x x]
    (if (zero? x) r (recur (if (odd? x) (mod (* r b) m) r) (mod (* b b) m) (quot x 2)))))

(defn legendre
  "(a/p) for an odd prime p: 0 if p divides a, +1 if a is a square mod p
   (a quadratic residue, DA art. 95), -1 if not, by Euler's criterion
   a^((p-1)/2) = (a/p) (mod p)."
  [a p]
  (let [r (power-mod a (quot (dec p) 2) p)]
    (cond (zero? (mod a p)) 0 (= r 1) 1 :else -1)))

(defn character
  "The Legendre symbol of p as a character of (Z/p)^*: {:group :elements
   :chi}."
  [p]
  {:group (gs/units-mod p) :elements (vec (range 1 p)) :chi #(legendre % p)})

(defn residues "The quadratic residues mod p among 1..p-1: the squares." [p]
  (vec (sort (distinct (map #(mod (* % %) p) (range 1 p))))))
