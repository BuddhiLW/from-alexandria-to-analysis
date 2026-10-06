(ns alexandria.cantor.sets
  "Cantor on countability (Crelle 77, 1874; Jahresbericht DMV 1, 1891), in
   exact arithmetic.

     zig-zag     the positive rationals counted along the diagonals of the
                 table of fractions, lowest terms only, and the inverse
     heights     Cantor's height N = n - 1 + |a0| + ... + |an| of a real
                 algebraic number; the irreducible primitive equations of
                 each height, their real roots counted exactly by Sturm
                 sequences over the rationals, phi(N)
     nested      1874 §2: from any sequence and interval, the nested
                 intervals that close on a number the sequence misses
     diagonal    1891: Cantor's b, the flipped diagonal of any table of
                 m/w rows, and the proof that it is in no row

   Reuse searched: Emmy (emmy.polynomial: make, evaluate, divide,
   pseudo-remainder; no Sturm sequence, no real-root count, no
   irreducibility test over Q), desargues and alexandria (nothing on
   countability). Polynomials here are small coefficient vectors of exact
   rationals, highest degree first as Cantor writes them; Emmy grades the
   identities."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The zig-zag

(defn gcd [a b] (if (zero? b) (abs a) (recur b (mod a b))))

(defn walk
  "The cells [p q] (column p, row q) of the table of fractions in the order
   of the zig-zag: diagonal p + q = s for s = 2, 3, ..., down the diagonal
   (row 1 first) when s is odd, up it when s is even."
  []
  (mapcat (fn [s]
            (let [cells (for [q (range 1 s)] [(- s q) q])]
              (if (odd? s) cells (reverse cells))))
          (iterate inc 2)))

(defn zig-zag
  "The positive rationals in the order of the walk, cells not in lowest
   terms skipped: 1, 2, 1/2, 1/3, 3, 4, 3/2, 2/3, 1/4, ..."
  []
  (->> (walk) (filter (fn [[p q]] (= 1 (gcd p q)))) (map (fn [[p q]] (/ p q)))))

(defn walk-index
  "The place (from 1) of cell [p q] on the walk: the inverse of the walk."
  [[p q]]
  (let [s (+ p q)
        before (quot (* (- s 2) (- s 1)) 2)
        along (if (odd? s) q (- s q))]
    (+ before along)))

(defn rational-index
  "The place (from 1) of the positive rational x in zig-zag: its cell's
   place on the walk less the cells skipped before it."
  [x]
  (let [x (rationalize x)
        [p q] (if (ratio? x) [(numerator x) (denominator x)] [x 1])
        k (walk-index [p q])
        skipped (count (remove (fn [[a b]] (= 1 (gcd a b))) (take (dec k) (walk))))]
    (- k skipped)))

(defn- clamp01 [x] (e// (e/+ 1 (e/- (e/abs x) (e/abs (e/- x 1)))) 2))

(def walk-cells
  "The cells of the first 28 steps of the walk (the diagonals p + q <= 8),
   which the walk figure follows."
  (vec (take 28 (walk))))

(defn walker
  "The pointer on the walk at time t (0 at cell 1, k at cell k + 1): the
   polyline through the cell centres of walk-cells, with the cell [p q] at
   (cell (p - 1), -cell (q - 1)). One Emmy expression: a sum of clamped
   segments, so it compiles to a kernel. State is unused ([_])."
  [cell]
  (fn [[t]]
    (let [pts (mapv (fn [[p q]] [(* cell (dec p)) (- (* cell (dec q)))]) walk-cells)
          steps (map vector pts (rest pts) (range))]
      (reduce (fn [[x y] [[x0 y0] [x1 y1] k]]
                (let [s (clamp01 (e/- t k))]
                  [(e/+ x (e/* s (- x1 x0))) (e/+ y (e/* s (- y1 y0)))]))
              (first pts)
              steps))))

(defn walk-pointer
  "The figure of the pointer: param t, the state unused."
  [t]
  (fn [_] ((walker 0.42) [t])))

(def figures
  "Every moving figure, by name: {:f :params :state}. :walk's param is the
   time t along the walk; the cell size is fixed at 0.42."
  {:walk {:f walk-pointer :params [0] :state [0]}})

;; ---------------------------------------------------------------------------
;; Polynomials: coefficient vectors [a0 a1 ... an], highest degree first

(defn- trim [v] (vec (drop-while zero? v)))

(defn degree [v] (dec (count (trim v))))

(defn value-at [v x] (reduce (fn [acc a] (+ (* acc x) a)) 0 v))

(defn derivative [v]
  (let [n (degree v)]
    (vec (map-indexed (fn [i a] (* a (- n i))) (butlast v)))))

(defn- poly-rem
  "Remainder of a by b over the rationals."
  [a b]
  (loop [a (trim a)]
    (if (< (count a) (count b))
      a
      (let [k (/ (first a) (first b))
            shifted (concat (map #(* k %) b) (repeat (- (count a) (count b)) 0))]
        (recur (trim (mapv - a shifted)))))))

(defn sturm-sequence [v]
  (loop [acc [(trim v) (trim (derivative v))]]
    (let [[a b] (take-last 2 acc)]
      (if (empty? b)
        (vec (butlast acc))
        (let [rm (mapv - (poly-rem a b))]
          (if (empty? rm) acc (recur (conj acc rm))))))))

(defn- sign-changes [xs]
  (let [s (remove zero? xs)]
    (count (filter (fn [[a b]] (neg? (* a b))) (partition 2 1 s)))))

(defn real-root-count
  "The number of distinct real roots of v in (lo, hi], by Sturm's theorem,
   exact."
  [v lo hi]
  (let [sq (sturm-sequence v)]
    (- (sign-changes (map #(value-at % lo) sq))
       (sign-changes (map #(value-at % hi) sq)))))

(defn root-bound
  "Cauchy's bound: every root lies in (-B, B), B = 1 + max |ai / a0|."
  [v]
  (+ 1 (reduce max 0 (map #(abs (/ % (first v))) (rest v)))))

(defn real-roots
  "The real roots of irreducible v: for degree 1 the one exact rational
   root; for higher degree (no rational roots) each root isolated by Sturm
   counts and bisected to a rational within tol."
  [v tol]
  (if (= 1 (degree v))
    [(- (/ (second v) (first v)))]
    (let [b (root-bound v)]
      (letfn [(isolate [lo hi]
                (let [k (real-root-count v lo hi)]
                  (cond (zero? k) []
                        (and (= 1 k) (< (- hi lo) tol)) [(/ (+ lo hi) 2)]
                        :else (let [mid (/ (+ lo hi) 2)]
                                (into (isolate lo mid) (isolate mid hi))))))]
        (isolate (- b) b)))))

(defn- divisors [n] (let [n (abs n)] (filter #(zero? (mod n %)) (range 1 (inc n)))))

(defn rational-roots
  "The rational roots of v (integer coefficients), by the rational root test."
  [v]
  (let [v (trim v)]
    (if (zero? (peek v))
      (cons 0 (rational-roots (pop v)))
      (distinct (for [p (divisors (peek v)) q (divisors (first v)) s [1 -1]
                      :let [x (* s (/ p q))] :when (zero? (value-at v x))]
                  x)))))

(defn- quadratic-factor?
  "v of degree 4 is a product of two integer quadratics (Gauss' lemma
   bounds the search by the coefficients' size)."
  [v]
  (let [bound (reduce + (map abs v))
        rng (range (- bound) (inc bound))]
    (boolean (some (fn [[a b c]]
                     (when (and (not (zero? a)) (zero? (mod (first v) a)))
                       (let [q [a b c]
                             rm (poly-rem v q)]
                         (and (empty? rm) true))))
                   (for [a (divisors (first v)) b rng c rng :when (not (zero? c))] [a b c])))))

(defn irreducible?
  "Integer v of degree <= 4 is irreducible over Q: degree 1, or no rational
   root (degrees 2, 3), and for degree 4 no quadratic factor either."
  [v]
  (let [n (degree v)]
    (cond (= 1 n) true
          (seq (rational-roots v)) false
          (<= n 3) true
          (= 4 n) (not (quadratic-factor? v))
          :else (throw (ex-info "irreducible? stops at degree 4" {:v v})))))

(defn height
  "Cantor's height (3.): N = n - 1 + |a0| + ... + |an|."
  [v]
  (+ (degree v) -1 (reduce + (map abs v))))

(defn- compositions
  "Integer vectors of length k whose absolute values sum to s."
  [k s]
  (if (= 1 k)
    (if (zero? s) [[0]] [[s] [(- s)]])
    (for [a (range (- s) (inc s)) rest (compositions (dec k) (- s (abs a)))]
      (into [a] rest))))

(defn equations-of-height
  "Cantor's normalized equations (1.) of height N: degree n >= 1, a0 > 0,
   coefficients without common divisor, irreducible."
  [N]
  (for [n (range 1 (inc N))
        :let [s (- N (dec n))]
        :when (pos? s)
        a0 (range 1 (inc s))
        tail (compositions n (- s a0))
        :let [v (into [a0] tail)]
        :when (= 1 (reduce gcd v))
        :when (irreducible? v)]
    v))

(defn numbers-of-height
  "[{:equation v :root x}] for the real algebraic numbers of height N, in
   increasing order (Cantor's order within a height). Rational roots are
   exact; irrational ones are rationals within 1e-9."
  [N]
  (->> (equations-of-height N)
       (mapcat (fn [v] (map (fn [x] {:equation v :root x}) (real-roots v 1/1000000000))))
       (sort-by (comp double :root))
       vec))

(defn phi
  "Cantor's phi(N): how many real algebraic numbers have height N."
  [N]
  (count (numbers-of-height N)))

;; ---------------------------------------------------------------------------
;; 1874 §2: nested intervals

(defn nested-intervals
  "Cantor's construction (1874 §2): from the interval (a, b), the first two
   members of the sequence xs strictly inside it, in increasing order, are
   the next interval; the search for the next pair starts after the later
   of the two. Up to k steps, or until fewer than two members remain inside.
   Returns [{:interval [lo hi] :used [i j]}]; every member of xs before the
   last index used lies outside the last interval."
  [xs [a b] k]
  (let [xs (vec xs)]
    (loop [lo a hi b start 0 acc [{:interval [a b] :used nil}]]
      (let [inside (filter #(< lo (nth xs %) hi) (range start (count xs)))
            [i j] (take 2 inside)]
        (if (or (nil? j) (= k (dec (count acc))))
          acc
          (let [p (nth xs i) q (nth xs j)]
            (recur (min p q) (max p q) (inc j)
                   (conj acc {:interval [(min p q) (max p q)] :used [i j]}))))))))

;; ---------------------------------------------------------------------------
;; 1891: the diagonal

(defn flip [c] (if (= c :m) :w :m))

(defn diagonal-element
  "Cantor's E0 for the rows: b_nu = flip of a_nu,nu."
  [rows]
  (vec (map-indexed (fn [i row] (flip (nth row i))) rows)))

(defn in-no-row?
  "E0 differs from row mu at place mu, for every mu of the table."
  [rows]
  (let [b (diagonal-element rows)]
    (every? (fn [mu] (not= (nth b mu) (nth (nth rows mu) mu))) (range (count rows)))))

(defn binary-rows
  "The first n binary digits (m = 0, w = 1) of the first n zig-zag
   rationals below 1, as a table of n rows."
  [n]
  (let [xs (take n (filter #(< % 1) (zig-zag)))
        digits (fn [x] (take n (map (fn [k] (if (even? (long (Math/floor (* x (Math/pow 2 k))))) :m :w))
                                    (range 1 (inc n)))))]
    (mapv (fn [x] {:x x :row (vec (digits x))}) xs)))

;; ---------------------------------------------------------------------------
;; Grades

(def identities
  "[label difference] pairs, each zero: the walk's place formula."
  (let [s 's]
    [["the diagonals before p + q = s hold 1 + 2 + ... + (s - 2) = (s - 2)(s - 1)/2 cells"
      (e/- (e/* 1/2 (e/- s 2) (e/- s 1)) (e// (e/* (e/- s 2) (e/+ (e/- s 2) 1)) 2))]]))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded
  "[{:label :grade}]: Emmy's identities, then the exact finite checks."
  []
  (let [first-100 (take 100 (zig-zag))]
    (into (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))})
                identities)
          (map (fn [[label ok?]] {:label label :grade (if ok? :grade/proved :grade/fails)}))
          [["the walk's place formula inverts the walk on its first 500 cells"
            (= (range 1 501) (map walk-index (take 500 (walk))))]
           ["the first 100 zig-zag rationals are distinct and each is found again at its place"
            (and (apply distinct? first-100)
                 (= (range 1 101) (map rational-index first-100)))]
           ["every p/q in lowest terms with p, q <= 12 is reached by the zig-zag"
            (let [seen (set (take 200 (zig-zag)))]
              (every? seen (for [p (range 1 13) q (range 1 13)] (/ p q))))]
           ["Cantor's phi(1) = 1, phi(2) = 2, phi(3) = 4"
            (= [1 2 4] (map phi [1 2 3]))]
           ["height 3 is exactly -2, -1/2, 1/2, 2"
            (= [-2 -1/2 1/2 2] (map :root (numbers-of-height 3)))]
           ["the diagonal element is in no row of the 12-row binary table"
            (in-no-row? (mapv :row (binary-rows 12)))]])))

;; ---------------------------------------------------------------------------
;; The proofs as data

(def proofs-resource "alexandria/cantor/mengenlehre.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
