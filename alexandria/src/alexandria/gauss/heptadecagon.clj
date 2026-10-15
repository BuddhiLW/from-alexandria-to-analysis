(ns alexandria.gauss.heptadecagon
  "Gauss, Disquisitiones arithmeticae (1801), Section VII, art. 354 and 365:
   the regular 17-gon by square roots alone.

   The sixteen roots r^k = cos(2 pi k/17) + i sin(2 pi k/17), k = 1..16, are
   ordered by the powers of the primitive root 3 (3^0, 3^1, ... mod 17).
   Gauss' periods (f, lambda) sum every e-th of them: two periods of eight,
   four of four, eight of two. Each period is the root of a quadratic whose
   coefficients are the periods one level up, so cos(2 pi/17) is reached by
   four square roots, nested.

   Real form used here: a period of f terms closed under k -> -k is
   sum 2 cos(2 pi k/17) over half its exponents.

     periods        [{:e :f :exps [[k ..] ..]}]: the tower for any prime p,
                    along the prime factors of p - 1 (smallest first)
     tower-values   the periods' numbers, each a raster kernel sum of cosines
     quadratic      the roots (s +- sqrt(s^2 - 4p))/2 of x^2 - s x + p
     nested         Emmy's nested radicals for 2 cos(2 pi/17), by the tower
     gauss-365      Gauss' printed expression of cos(2 pi/17) (art. 365)
     classify       n -> constructible?, the Fermat primes, the obstruction
                    (Gauss art. 365-366; Wantzel 1837 proved the necessity)
     graded         the checks, graded

   The algebra is Emmy's; every number comes from alexandria.raster."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Integers: primes, primitive roots, Fermat primes

(defn prime? [n]
  (and (> n 1) (not-any? #(zero? (mod n %)) (range 2 (inc (long (Math/sqrt n)))))))

(defn factorize
  "{prime exponent} of n > 1."
  [n]
  (loop [n n d 2 acc (sorted-map)]
    (cond (= n 1) acc
          (> (* d d) n) (update acc n (fnil inc 0))
          (zero? (mod n d)) (recur (quot n d) d (update acc d (fnil inc 0)))
          :else (recur n (inc d) acc))))

(defn prime-factors-list
  "The prime factors of n with multiplicity, smallest first."
  [n]
  (if (< n 2) [] (vec (mapcat (fn [[p a]] (repeat a p)) (factorize n)))))

(defn- pow-mod [b x m]
  (loop [r 1 b (mod b m) x x]
    (if (zero? x) r (recur (if (odd? x) (mod (* r b) m) r) (mod (* b b) m) (quot x 2)))))

(defn primitive-root
  "The least primitive root of the prime p (Gauss art. 57: 3 for 17)."
  [p]
  (if (= p 2) 1
      (let [qs (keys (factorize (dec p)))]
        (first (filter (fn [g] (every? #(not= 1 (pow-mod g (quot (dec p) %) p)) qs))
                       (range 2 p))))))

(def fermat-primes
  "The primes 2^(2^nu) + 1 known: nu = 0..4 (Gauss art. 365; Euler found
   641 | 2^32 + 1)."
  [3 5 17 257 65537])

(defn fermat-prime? [p] (boolean (some #{p} fermat-primes)))

(defn totient [n]
  (reduce (fn [acc [p a]] (* acc (dec p) (long (Math/pow p (dec a))))) 1 (factorize n)))

(defn power-of-two? [n] (and (pos? n) (zero? (bit-and n (dec n)))))

(defn classify
  "The regular n-gon (n >= 3): {:n :constructible? :factors {p a}
   :fermat [p ..] :obstruction kw :culprit p :phi :name}.

   :obstruction  nil              constructible: n = 2^a times distinct
                                  Fermat primes (art. 365-366)
                 :odd-prime-power an odd prime p divides n twice: the
                                  division into p^2 parts needs an equation
                                  of degree p (art. 366)
                 :not-fermat      p - 1 has an odd prime factor q: an
                                  equation of degree q that cannot be
                                  avoided (art. 365)"
  [n]
  (let [fs (factorize n)
        odd (dissoc fs 2)
        twice (first (filter #(> (odd %) 1) (keys odd)))
        bad (first (remove fermat-prime? (keys odd)))
        q (when bad (first (filter odd? (prime-factors-list (dec bad)))))
        ok? (and (nil? twice) (nil? bad))]
    {:n n :factors fs :constructible? ok? :phi (totient n)
     :fermat (vec (filter fermat-prime? (keys odd)))
     :obstruction (cond ok? nil twice :odd-prime-power :else :not-fermat)
     :culprit (or twice bad) :degree (or (when twice twice) q)}))

;; ---------------------------------------------------------------------------
;; Gauss' periods

(defn chain
  "The sizes e (number of periods) of the tower for the prime p: the partial
   products of the prime factors of (p - 1)/2, smallest first, after 1.
   The last level has (p-1)/2 periods of two terms: 2 cos(2 pi k/p)."
  [p]
  (vec (reductions * 1 (prime-factors-list (quot (dec p) 2)))))

(defn periods
  "The tower of p as data: for each e in (chain p), the e periods, each the
   list of exponents k in 1..(p-1)/2 whose 2 cos(2 pi k/p) it sums.
   Period i of e collects g^(i + e j) mod p, folded to min(k, p - k)."
  [p]
  (let [g (primitive-root p)
        pw (vec (map #(pow-mod g % p) (range (dec p))))
        fold (fn [k] (min k (- p k)))]
    (vec (for [e (chain p)]
           {:e e :f (quot (dec p) e)
            :exps (vec (for [i (range e)]
                         (vec (sort (distinct (map fold (take-nth e (drop i pw))))))))}))))

(defn two-cos
  "2 cos(2 pi k/p), the kernel every period sums."
  [k p]
  (e/* 2 (e/cos (e/divide (e/* 2 Math/PI k) p))))

(defn period-value
  "The number of one period (exponents ks of p): sum of 2 cos(2 pi k/p),
   each term from the raster kernel of two-cos."
  [ks p]
  (reduce + (raster/sample two-cos (mapv (fn [k] [k p]) ks))))

(defn tower-values
  "(periods p) with :values [number ..] per level, by raster."
  [p]
  (mapv (fn [{:keys [exps] :as lvl}] (assoc lvl :values (mapv #(period-value % p) exps)))
        (periods p)))

;; ---------------------------------------------------------------------------
;; The 17-gon by square roots

(defn quadratic
  "The two roots (s + sqrt(s^2 - 4q))/2, (s - sqrt(s^2 - 4q))/2 of
   x^2 - s x + q = 0."
  [s q]
  (let [d (e/sqrt (e/- (e/square s) (e/* 4 q)))]
    [(e/divide (e/+ s d) 2) (e/divide (e/- s d) 2)]))

(defn nested
  "Emmy's tower for p = 17, in Gauss' order (art. 354):
     y1, y2  periods of 8:  y^2 + y - 4 = 0   (sum -1, product -4)
     z1, z2  periods of 4 in y1: z^2 - y1 z - 1 = 0
     z3, z4  periods of 4 in y2: z^2 - y2 z - 1 = 0
     w1, w2  periods of 2 in z1: w^2 - z1 w + z3 = 0
   w1 = 2 cos(2 pi/17). Returns the symbolic radicals {:y1 .. :w1 :cos}."
  []
  (let [[y1 y2] (quadratic -1 -4)
        [z1 z2] (quadratic y1 -1)
        [z3 z4] (quadratic y2 -1)
        [w1 w2] (quadratic z1 z3)]
    {:y1 y1 :y2 y2 :z1 z1 :z2 z2 :z3 z3 :z4 z4 :w1 w1 :w2 w2 :cos (e/divide w1 2)}))

(def steps
  "The tower's quadratics, one per step: [label sum product roots exps]."
  [{:id :y :label "two periods of eight" :eq "y^2 + y - 4 = 0" :roots [:y1 :y2]}
   {:id :z1 :label "four of four, first pair" :eq "z^2 - y_1 z - 1 = 0" :roots [:z1 :z2]}
   {:id :z3 :label "four of four, second pair" :eq "z^2 - y_2 z - 1 = 0" :roots [:z3 :z4]}
   {:id :w :label "eight of two, the one we need" :eq "w^2 - z_1 w + z_3 = 0" :roots [:w1 :w2]}])

(defn gauss-365
  "cos(2 pi/17) as Gauss prints it in art. 365:
     -1/16 + sqrt17/16 + sqrt(34 - 2 sqrt17)/16
     + 1/8 sqrt(17 + 3 sqrt17 - sqrt(34 - 2 sqrt17) - 2 sqrt(34 + 2 sqrt17))"
  []
  (let [r17 (e/sqrt 17)
        a (e/sqrt (e/- 34 (e/* 2 r17)))
        b (e/sqrt (e/+ 34 (e/* 2 r17)))]
    (e/+ (e/divide -1 16) (e/divide r17 16) (e/divide a 16)
         (e/* (e/divide 1 8) (e/sqrt (e/- (e/+ 17 (e/* 3 r17)) a (e/* 2 b)))))))

(defn nested-values
  "Each radical of `nested` as a number, by its raster kernel (no
   arguments: the kernel is a constant)."
  []
  (into {} (for [[k v] (nested)] [k (raster/value (fn [_] v) 0)])))

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [kind diffs tol]
  (let [res (grade/grade kind diffs tol)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn vieta-diffs
  "For each step: (x - r1)(x - r2) - (x^2 - s x + q) as a polynomial in a
   symbolic x with symbolic s, q: Emmy simplifies it to 0, so the two roots
   of `quadratic` are the roots."
  []
  (let [[r1 r2] (quadratic 's 'q)]
    [(e/- (e/* (e/- 'x r1) (e/- 'x r2)) (e/+ (e/square 'x) (e/* -1 's 'x) 'q))]))

(defn graded
  "[{:label :grade}] for the 17-gon."
  []
  (let [nv (nested-values)
        c (raster/value (fn [_] (e/cos (e/divide (* 2 Math/PI) 17))) 0)
        g365 (raster/value (fn [_] (gauss-365)) 0)
        tv (tower-values 17)
        lvl (fn [e] (:values (first (filter #(= e (:e %)) tv))))]
    [{:label "the roots (s ± √(s² − 4q))/2 solve x² − s x + q = 0 (Vieta)"
      :grade (variant :symbolic (vieta-diffs) grade/default-tolerance)}
     {:label "the nested radical w₁/2 = cos(2π/17)"
      :grade (variant :numeric [(- (:cos nv) c)] 1e-12)}
     {:label "Gauss' printed formula of art. 365 = cos(2π/17)"
      :grade (variant :numeric [(- g365 c)] 1e-12)}
     {:label "the radicals y₁, y₂ = the periods of eight (sums of cosines)"
      :grade (variant :numeric [(- (:y1 nv) (first (lvl 2))) (- (:y2 nv) (second (lvl 2)))] 1e-12)}
     {:label "every pair of periods sums to the period it splits"
      :grade (variant :numeric
                      (for [[a b] (partition 2 1 tv)
                            i (range (:e a))]
                        (- (nth (:values a) i) (+ (nth (:values b) i) (nth (:values b) (+ i (:e a))))))
                      1e-12)}]))
