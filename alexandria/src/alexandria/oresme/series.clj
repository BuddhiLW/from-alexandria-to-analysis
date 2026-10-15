(ns alexandria.oresme.series
  "Oresme's two infinite series (Quaestiones super geometriam Euclidis,
   q. 2; Tractatus de configurationibus III.8).

   The harmonic series diverges. Oresme groups its terms after the first:
   1/2; 1/3 + 1/4 >= 1/4 + 1/4 = 1/2; 1/5 + ... + 1/8 >= 4 x 1/8 = 1/2; ...
   group k (the terms 1/(2^(k-1) + 1) .. 1/2^k) has 2^(k-1) terms each at
   least 1/2^k, so it is at least 1/2, and

     H(2^k) >= 1 + k/2,

   which passes any bound. Beside it, the modern asymptotics
   H(N) ~ ln N + gamma (Euler, 1734).

   The convergent series: a quality whose degree is 1 in the first half of
   an hour, 2 in the next quarter, 3 in the next eighth, ... has the finite
   total sum n/2^n = 2 (De configurationibus III.8).

   For the page, a BLOCK kernel sums `block` consecutive terms at once and
   a TERM kernel gives one, for both series and any exponent p (1/n^p:
   p = 1 is Oresme's, p > 1 converges and the grouping bound fails); the
   page adds full blocks, then single terms, so every partial sum is a
   raster number. A COMPARE kernel gives ln N + gamma and the bounds."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(def block "Terms per call of the block kernel." 16)

(def gamma "Euler's constant." 0.5772156649015329)

(defn- harmonic-term [p n] (e/exp (e/* (e/- p) (e/log n))))
(defn- oresme-term [n] (e/* n (e/exp (e/* (e/- n) (e/log 2)))))

(defn term
  "Kernel: params [p], state [n]. Out [1/n^p n/2^n]."
  [p]
  (fn [[n]] [(harmonic-term p n) (oresme-term n)]))

(defn terms
  "Kernel: params [p], state [start]. Out [Σ 1/n^p, Σ n/2^n] over
   n = start .. start + block - 1."
  [p]
  (fn [[start]]
    (let [ns (map #(e/+ start %) (range block))]
      [(reduce e/+ (map #(harmonic-term p %) ns))
       (reduce e/+ (map oresme-term ns))])))

(defn compare-kernel
  "Kernel: params [N k p]. Out [ln N + gamma, 1 + k/2, log2 N, group-floor,
   tail]: the asymptotic value, Oresme's bound after k groups, the number
   of whole groups in N, Oresme's estimate for group k of 1/n^p (2^(k-1)
   terms of at least 2^(-kp)), and 2 - S(N) for the convergent series by
   its closed form (N+2)/2^N."
  [N k p]
  (fn [_]
    [(e/+ (e/log N) gamma)
     (e/+ 1 (e// k 2))
     (e// (e/log N) (e/log 2))
     (e/exp (e/* (e/log 2) (e/- (e/- k 1) (e/* k p))))
     (e/* (e/+ N 2) (e/exp (e/* (e/- N) (e/log 2))))]))

(def compare-index {:asymptotic 0 :bound 1 :log2 2 :group-floor 3 :tail 4})

(defn bar-figure
  "Figure kernel of the stepped player: params [p sx sy], state [n].
   Out [x0 h]: the bar of term n starts at sx (n - 1) and is sy/n^p tall."
  [p sx sy]
  (fn [[n]] [(e/* sx (e/- n 1)) (e/* sy (harmonic-term p n))]))

(defn stack-figure
  "Oresme's figure of the convergent series (De configurationibus III.8):
   the degree n held over the n-th part of the extension [0, 1], the parts
   halving (1/2, 1/4, 1/8, ...). Params [s] (a scale, 1), state [n].
   Out [x0 x1 h area]: the part [1 - 2^-(n-1), 1 - 2^-n], its degree n and
   its area n/2^n."
  [s]
  (fn [[n]]
    (let [w (e/exp (e/* (e/- n) (e/log 2)))]
      [(e/* s (e/- 1 (e/* 2 w))) (e/* s (e/- 1 w)) n (e/* s (oresme-term n))])))

(def figures
  "The player's moving figures."
  {:bar {:f bar-figure :params [1 1 4] :state [1] :opts {:simplify? false}}})

;; ---------------------------------------------------------------------------
;; JVM side: the same sums by raster (tests, grades, the stepped figure)

(def ^:private block-k
  (delay (binding [raster/*simplify?* false]
           (raster/kernels (fn [p s] ((terms p) [s])) 2))))

(def ^:private term-k
  (delay (binding [raster/*simplify?* false]
           (raster/kernels (fn [p n] ((term p) [n])) 2))))

(defn- call2 [ks p x] (mapv #(% (double p) (double x)) ks))

(defn partial-sum
  "Sum over n in [a, b] by the block and term kernels: [Σ 1/n^p, Σ n/2^n]."
  ([b] (partial-sum 1 1 b))
  ([p a b]
   (let [full (quot (inc (- b a)) block)
         starts (map #(+ a (* block %)) (range full))
         tail (range (+ a (* block full)) (inc b))
         add (fn [[h o] [h' o']] [(+ h h') (+ o o')])]
     (as-> [0.0 0.0] acc
       (reduce (fn [acc s] (add acc (call2 @block-k p s))) acc starts)
       (reduce (fn [acc n] (add acc (call2 @term-k p n))) acc tail)))))

(defn group-sum
  "Oresme's group k (k >= 1): the terms 1/n^p for n = 2^(k-1) + 1 .. 2^k."
  ([k] (group-sum 1 k))
  ([p k] (first (partial-sum p (inc (bit-shift-left 1 (dec k))) (bit-shift-left 1 k)))))

(defn compare-values [N k p]
  (raster/value (fn [x y z] ((compare-kernel x y z) 0)) N k p))

(defn case-of
  "The name of the series 1/n^p the reader has set."
  [p]
  (cond (< (Math/abs (- (double p) 1)) 1e-9) :harmonic-diverges
        (< p 1) :diverges-faster
        :else :converges))

(defn player-data
  "The numbers the stepped grouping shows, by raster: each group's sum,
   H(2^k) for k = 1..4, and H(2^20)."
  []
  {:groups (mapv group-sum (range 1 5))
   :H (mapv #(first (partial-sum (bit-shift-left 1 %))) (range 1 5))
   :big (first (partial-sum (bit-shift-left 1 20)))})

;; ---------------------------------------------------------------------------
;; Grades

(def identities
  "[label difference] pairs reduced to 0 by Emmy; m stands for 2^(k-1) and
   x for 2^(-n)."
  [["group k has m = 2^(k−1) terms, each at least 1/(2m) = 1/2^k: together at least m · 1/(2m) = 1/2"
    (e/- (e/* 'm (e// 1 (e/* 2 'm))) (e// 1 2))]
   ["Oresme's bound 1 + k/2 grows by 1/2 with each doubling of the number of terms"
    (e/- (e/- (e/+ 1 (e// (e/+ 'k 1) 2)) (e/+ 1 (e// 'k 2))) (e// 1 2))]
   ["the convergent series: S(n) = 2 − (n + 2)/2ⁿ has S(n) − S(n − 1) = n/2ⁿ (x = 2⁻ⁿ)"
    (let [S (fn [n x] (e/- 2 (e/* (e/+ n 2) x)))]
      (e/- (e/- (S 'n 'x) (S (e/- 'n 1) (e/* 2 'x))) (e/* 'n 'x)))]
   ["and S(1) = 1/2, its first term"
    (e/- (e/- 2 (e/* 3 (e// 1 2))) (e// 1 2))]])

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded
  "[{:label :grade}]."
  []
  (let [ks (range 1 15)
        H (fn [k] (first (partial-sum (bit-shift-left 1 k))))
        bound-gaps (map (fn [k] (max 0.0 (- (+ 1 (/ k 2.0)) (H k)))) ks)
        group-gaps (map (fn [k] (max 0.0 (- 0.5 (group-sum k)))) ks)
        N 4096
        [hN] (partial-sum N)
        [asym] (compare-values N 12 1)
        [_ o] (partial-sum 60)]
    (-> (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))}) identities)
        (conj {:label "every one of Oresme's groups 1 … 14 sums to at least 1/2 (raster)"
               :grade (variant (grade/grade :numeric group-gaps 0)) :engine :raster}
              {:label "H(2^k) ≥ 1 + k/2 for k = 1 … 14 (raster)"
               :grade (variant (grade/grade :numeric bound-gaps 0)) :engine :raster}
              {:label "H(4096) − (ln 4096 + γ) is Euler's 1/(2N) to 1e-7 (raster)"
               :grade (variant (grade/grade :numeric [(- hN asym (/ 1.0 (* 2 N)))] 1e-7)) :engine :raster}
              {:label "Σ n/2ⁿ to 60 terms is 2 to 1e-12 (raster)"
               :grade (variant (grade/grade :numeric [(- o 2)] 1e-12)) :engine :raster}))))
