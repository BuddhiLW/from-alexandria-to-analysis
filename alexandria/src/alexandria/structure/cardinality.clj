(ns alexandria.structure.cardinality
  "Domain stratum: counting by matching, and the diagonal that defeats
   a claimed matching (Cantor, Jahresbericht der DMV 1, 1891, pp. 75-78).

   Enumeration  a list of elements, by place from 0:
                (nth-element E n), the element at place n;
                (index-of E x), the place of x in the list, nil when x is
                not among the elements listed.
   Instances    IndexedList  an N-indexed list (nth n -> f n) of elements
                             that may themselves be sequences; the list is
                             infinite, so index-of looks at the first
                             `horizon` places and compares sequence
                             elements on their first `horizon` terms (the
                             finite shadow of a claim about infinite
                             sequences).
                FiniteSet    a finite set listed without repeats, with an
                             exact index-of.

   Pure functions:
     diagonal            from an Enumeration of sequences and a flip (a
                         character different from its argument), the
                         sequence whose place n is flip of place n of the
                         element at place n: Cantor's E0, in no place of
                         the list.
     power-set           all subsets of a FiniteSet, 2^|X| of them.
     power-set-diagonal  from f : X -> P(X), D = {x in X : x not in f(x)}.
     missed?             D is not f(x) for any x.
     all-maps, cantor-report  every map X -> P(X) for a small X, and the
                         count of those whose D is missed.

   Laws (test/alexandria/structure/cardinality_contract.clj checks them on
   generated lists, sequences and maps):
     round trip     (index-of E (nth-element E i)) = i on a list without
                    repeats
     diagonal       for every place n, (diagonal E flip) differs from
                    (nth-element E n) at place n; so it is found at no
                    place
     power set      |P(X)| = 2^|X| > |X|, and D = power-set-diagonal f is
                    not in the image of f.

   The same proof twice: a Boolean sequence is a function N -> {m, w}, that
   is a subset of N (w = member). Given a list E_0, E_1, ... put
   f(n) = {k : E_n(k) = w}; then D = {n : n not in f(n)} is the set whose
   sequence is the diagonal (characteristic-set checks this on a prefix).

   Reuse searched (carto, alexandria): alexandria.cantor.sets holds the one
   other instance, the m/w table of 1891 (flip, diagonal-element,
   in-no-row?); the contract test checks that it agrees with `diagonal`
   here. alexandria.structure.ring has `elements` of a finite ring and
   nothing that counts or lists; graph lists vertices, not maps between
   sets. So there is one prior instance (the table) and one new (finite
   sets); this namespace is kept thin.")


(defprotocol Enumeration
  (nth-element [E n] "The element at place n (from 0).")
  (index-of [E x] "The place of x in the list, nil when x is not among the elements listed."))

;; ---------------------------------------------------------------------------
;; Sequences

(defn term
  "Place k (from 0) of a sequence s: a function of k or a vector."
  [s k]
  (if (ifn? s) (s k) (nth s k)))

(defn prefix
  "The first n places of the sequence s as a vector; anything that is not
   a sequence (a function or a sequential) is returned as it is."
  [s n]
  (if (or (fn? s) (sequential? s)) (mapv #(term s %) (range n)) s))

(def mw-flip
  "Cantor's rule: b is m where a is w, and w where a is m."
  {:m :w :w :m})

(def bit-flip {0 1 1 0})

;; ---------------------------------------------------------------------------
;; Instances

(defrecord IndexedList [f horizon]
  Enumeration
  (nth-element [_ n] (f n))
  (index-of [_ x]
    (let [px (prefix x horizon)]
      (first (filter #(= px (prefix (f %) horizon)) (range horizon))))))

(defn indexed-list
  "The N-indexed list n -> (f n), searched over its first horizon places."
  [f horizon]
  (->IndexedList f horizon))

(defrecord FiniteSet [xs]
  Enumeration
  (nth-element [_ n] (nth xs n))
  (index-of [_ x] (let [i (.indexOf ^java.util.List xs x)] (when-not (neg? i) i))))

(defn finite-set
  "The distinct members of coll, in first-seen order."
  [coll]
  (->FiniteSet (vec (distinct coll))))

(defn cardinal "The number of members of a FiniteSet." [X] (count (:xs X)))

;; ---------------------------------------------------------------------------
;; The diagonal of a list of sequences

(defn diagonal
  "Cantor's E0 for the list E of sequences: the sequence whose place n is
   (flip (term (nth-element E n) n)). flip must send every character to a
   different one."
  [E flip]
  (fn [n] (flip (term (nth-element E n) n))))

(defn diagonal-prefix
  "The first n places of (diagonal E flip) as a vector."
  [E flip n]
  (prefix (diagonal E flip) n))

(defn differs-at-place?
  "True when (diagonal E flip) differs from the element at place n at place n."
  [E flip n]
  (not= (term (diagonal E flip) n) (term (nth-element E n) n)))

(defn in-no-place?
  "True when the diagonal is found at none of the first n places of E,
   comparing the first n terms: the finite check of Cantor's proof."
  [E flip n]
  (let [d (diagonal-prefix E flip n)]
    (every? #(not= d (prefix (nth-element E %) n)) (range n))))

;; ---------------------------------------------------------------------------
;; The power set of a finite set

(defn power-set
  "All subsets of the FiniteSet X, as sets, in the order of the bit masks:
   the subset at place m holds the members at the places of the 1 bits of m."
  [X]
  (let [xs (:xs X) n (count xs)]
    (->FiniteSet (mapv (fn [m] (into #{} (keep-indexed (fn [i x] (when (bit-test m i) x))) xs))
                       (range (bit-shift-left 1 n))))))

(defn power-set-diagonal
  "D = {x in X : x not in f(x)} for f : X -> P(X) (a function or a map)."
  [f X]
  (into #{} (remove (fn [x] (contains? (f x) x))) (:xs X)))

(defn image "The set of f(x) for x in X." [f X] (into #{} (map f) (:xs X)))

(defn missed?
  "True when D = power-set-diagonal f is not f(x) for any x in X."
  [f X]
  (not (contains? (image f X) (power-set-diagonal f X))))

(defn all-maps
  "Every map X -> P(X) as a Clojure map x -> subset: (2^n)^n of them."
  [X]
  (let [xs (:xs X) subsets (:xs (power-set X))]
    (map #(zipmap xs %)
         (reduce (fn [acc _] (for [a acc s subsets] (conj a s))) [[]] xs))))

(defn cantor-report
  "For the n-element set {0 .. n-1}: the number of maps to P(X), how many
   of them miss their D, the sizes |X| and |P(X)|."
  [n]
  (let [X (finite-set (range n)) ms (all-maps X)]
    {:n n :size n :power-size (cardinal (power-set X))
     :maps (count ms) :missed (count (filter #(missed? % X) ms))}))

;; ---------------------------------------------------------------------------
;; The two proofs are one

(defn characteristic-set
  "The members k < n of the sequence s where it is :w."
  [s n]
  (into #{} (filter #(= :w (term s %))) (range n)))

(defn sequences->subsets
  "The map n -> {k < width : E_n(k) = w} for the first n places of the list
   E of m/w sequences, as a Clojure map."
  [E n width]
  (into {} (map (fn [i] [i (characteristic-set (nth-element E i) width)])) (range n)))
