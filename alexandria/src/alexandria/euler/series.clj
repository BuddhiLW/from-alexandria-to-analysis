(ns alexandria.euler.series
  "Euler's infinite polynomials: the Basel problem (De summis serierum
   reciprocarum, E41, 1734/35) and e^{ix} = cos x + i sin x (Introductio in
   analysin infinitorum, E101, 1748, cap. VIII, par. 138).

     Viete's move  a polynomial with constant term 1 and roots r_n factors as
                   prod (1 - x/r_n); its coefficients are then the elementary
                   symmetric functions of the 1/r_n. Euler applies this to
                   sin x / x, whose roots are +-n pi, in u = x^2.
     coefficients  -e1 at u, e2 at u^2: proved by Emmy on the truncated
                   product of N symbolic factors, for each N asked
     Newton        p2 = e1^2 - 2 e2 (Euler's Q = P alpha - 2 beta, par. 10):
                   the same identity on the same truncations; with
                   e1 = 1/6 and e2 = 1/120 it gives sum 1/n^4 = pi^4/90
     partial sums  exact rationals; pi^2/6 - S_N lies below 1/N, graded
                   numeric with tolerance the tail bound
     e^{ix}        the exponential series truncated at order N equals the
                   cosine and sine series truncated at N, term by term:
                   proved for each N asked (a statement about every
                   truncation, not about the limit); e^{i pi} + 1 = 0 from
                   cos pi = -1, sin pi = 0
     figures       Emmy functions for media: the partial products of
                   sin x / x, the spiral of partial sums of e^{ix}

   Reuse searched: emmy.series (sin-series, exp-series, cos-series) gives the
   coefficients; emmy.env D takes the coefficients of the truncated product;
   emmy.complex the unit i. Nothing here re-implements them."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [emmy.series :as s]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]
            [alexandria.raster.series :as rs]))

(defn- variant
  ([kind diffs] (variant kind diffs grade/default-tolerance))
  ([kind diffs tol]
   (let [res (grade/grade kind diffs tol)]
     (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))))

(defn coefficient
  "The k-th coefficient of Emmy's power series `series`."
  [series k]
  (nth (seq series) k))

;; ---------------------------------------------------------------------------
;; sin x / x as a polynomial in u = x^2

(defn sinc-coefficient
  "Coefficient of u^k = x^(2k) in sin x / x: (-1)^k / (2k+1)!, read from
   emmy.series/sin-series."
  [k]
  (coefficient s/sin-series (inc (* 2 k))))

(defn- letters [n] (mapv #(symbol (str "a" %)) (range 1 (inc n))))

(defn truncated-product
  "u -> prod_{n <= N} (1 - a_n u), for symbols a_1..a_N."
  [as]
  (fn [u] (reduce e/* 1 (map #(e/- 1 (e/* % u)) as))))

(defn- e1 [as] (reduce e/+ 0 as))
(defn- e2 [as] (reduce e/+ 0 (for [i (range (count as)) j (range (count as)) :when (< i j)]
                               (e/* (as i) (as j)))))
(defn- p2 [as] (reduce e/+ 0 (map e/square as)))

(defn- taylor-coefficient
  "The coefficient of u^k of f at 0, by Emmy's derivative."
  [f k]
  (e/divide (((e/expt e/D k) f) 0) (e/factorial k)))

(defn viete-identities
  "[label difference] pairs for the product of N factors (1 - a_n u):
   the u and u^2 coefficients are -e1 and e2, and p2 = e1^2 - 2 e2."
  [N]
  (let [as (letters N) P (truncated-product as)]
    [[(str "N = " N ": the coefficient of u in prod (1 - a_n u) is -(a_1 + ... + a_N)")
      (e/+ (taylor-coefficient P 1) (e1 as))]
     [(str "N = " N ": the coefficient of u^2 is the sum of the products a_i a_j, i < j")
      (e/- (taylor-coefficient P 2) (e2 as))]
     [(str "N = " N ": the sum of the squares a_n^2 is e1^2 - 2 e2 (Euler's Q = P alpha - 2 beta)")
      (e/- (p2 as) (e/- (e/square (e1 as)) (e/* 2 (e2 as))))]]))

(def basel
  "What the matched coefficients give, as exact values in units of pi:
   with a_n = 1/(n^2 pi^2), e1 = 1/6 and e2 = 1/120, so
   sum 1/n^2 = pi^2 e1 and sum 1/n^4 = pi^4 (e1^2 - 2 e2)."
  (let [e1 (- (sinc-coefficient 1))
        e2 (sinc-coefficient 2)]
    {:e1 e1 :e2 e2
     :zeta-2 e1                                 ; times pi^2
     :zeta-4 (- (* e1 e1) (* 2 e2))}))          ; times pi^4

(defn partial-sum
  "Exact sum_{n=1}^{N} 1/n^power."
  [power N]
  (reduce + (map #(/ 1 (long (Math/pow % power))) (range 1 (inc N)))))

(defn tail-bound
  "sum_{n > N} 1/n^power < integral_N^inf dx / x^power = 1/((power-1) N^(power-1))."
  [power N]
  (/ 1 (* (dec power) (long (Math/pow N (dec power))))))

(defn tail
  "sum_{N < n <= N + m} 1/n^power by a raster loop kernel
   (alexandria.raster.series/sum): the part of the tail a computer adds."
  [power N m]
  (:value (rs/sum (fn [n] (e// 1 (e/expt n power))) (inc N) (+ N m))))

(defn basel-gap
  "pi^power-value - S_N with S_N the exact partial sum and the value
   (pi^2/6 or pi^4/90) evaluated by a raster kernel, e.g. (e// (e/square
   'pi) 6) lowered with pi raster's constant."
  [power N]
  (let [target (raster/value (fn [p] (if (= 2 power) (e// (e/square p) 6) (e// (e/expt p 4) 90)))
                             Math/PI)]
    (- target (double (partial-sum power N)))))

(defn graded-basel
  "[{:label :grade}]: the Viete identities for N = 1..6 (proved), the values
   1/6, 1/120 and 1/90 (proved, exact rationals), and the partial sums
   inside their tail bounds (numeric, tolerance the bound; the numbers by
   raster kernels, :source :raster)."
  []
  (let [{:keys [e1 e2 zeta-2 zeta-4]} basel
        N 2000]
    (concat
     (for [n (range 1 7) [label diff] (viete-identities n)]
       {:label label :grade (variant :symbolic [diff])})
     [{:label "sin x / x = 1 - x^2/6 + x^4/120 - ...: the u coefficient is -1/6, so e1 = sum 1/(n^2 pi^2) = 1/6"
       :grade (variant :symbolic [(- e1 1/6)])}
      {:label "the u^2 coefficient is 1/120 = e2"
       :grade (variant :symbolic [(- e2 1/120)])}
      {:label "sum 1/n^2 = pi^2/6"
       :grade (variant :symbolic [(e/- (e/* zeta-2 (e/square 'pi)) (e/divide (e/square 'pi) 6))])}
      {:label "the same move one coefficient on: sum 1/n^4 = pi^4 (1/36 - 2/120) = pi^4/90"
       :grade (variant :symbolic [(- zeta-4 1/90)])}
      {:label (str "pi^2/6 - (1 + 1/4 + ... + 1/" N "^2) is below the tail bound 1/" N)
       :grade (variant :numeric [(basel-gap 2 N)] (double (tail-bound 2 N)))
       :source raster/source}
      {:label (str "pi^4/90 - (1 + 1/16 + ... + 1/" N "^4) is below the tail bound 1/(3 " N "^3)")
       :grade (variant :numeric [(basel-gap 4 N)] (double (tail-bound 4 N)))
       :source raster/source}
      {:label (str "the gap is the tail: 1/" (inc N) "^2 + ... + 1/" (+ N 1000000) "^2 (a raster loop) is pi^2/6 - S_" N " to within 1e-6")
       :grade (variant :numeric [(- (basel-gap 2 N) (tail 2 N 1000000))] 1e-6)
       :source raster/source}])))

(defn newton-sums
  "E41 par. 8: Euler's rule for the sums of powers P, Q, R, S, ... of a
   series whose sum is alpha, sum of products of pairs beta, of triples
   gamma, ...: P = alpha, Q = P alpha - 2 beta, R = Q alpha - P beta + 3
   gamma, ... From es = [alpha beta gamma ...] returns [P Q R ...], k terms.
   Works on numbers and on Emmy expressions."
  [es k]
  (let [e (fn [j] (get es (dec j) 0))]
    (reduce (fn [ps m]
              (conj ps (e/+ (reduce e/+ 0 (for [j (range 1 m)]
                                           (e/* (if (odd? j) 1 -1) (e j) (ps (dec (- m j))))))
                            (e/* (if (odd? m) 1 -1) m (e m)))))
            [] (range 1 (inc k)))))

(def euler-par-17
  "E41 par. 17-18: the products of the terms 1/p^2, 1/4p^2, 1/9p^2, ... taken
   one, two, three, ... at a time are the coefficients 1/6, 1/120, 1/5040,
   ... of sin s / s up to sign, read from emmy.series; by par. 8 their power
   sums P, Q, R, S, T, V are the sums of 1/n^2, 1/n^4, ..., 1/n^12 in units
   of p^2, p^4, ... (par. 18 prints all six)."
  (let [es (mapv #(e/* (if (odd? %) -1 1) (sinc-coefficient %)) (range 1 7))]
    {:es es :sums (newton-sums es 6)}))

(defn euler-par-9
  "E41 par. 9-12 with y = 1. The equation 0 = 1 - s + s^3/1.2.3 -
   s^5/1.2.3.4.5 + ... has the roots q, q, -3q, -3q, 5q, 5q, ... (q the
   quarter circumference, par. 10); the sums of the powers of their
   reciprocals come from alpha = 1, beta = 0, gamma = -1/6, delta = 0,
   epsilon = 1/120 by par. 8: P, Q, R, S, T = 1, 1, 1/2, 1/3, 5/24, Euler's
   own values (par. 12). P = 1 is Leibniz's 1 - 1/3 + 1/5 - ... = q/2;
   Q = 1 gives 1 + 1/9 + 1/25 + ... = q^2/2 (par. 11)."
  []
  (newton-sums [1 0 -1/6 0 1/120] 5))

(defn partial-sums-raster
  "Euler's partial sums 1 + 1/2^power + ... + 1/N^power for each N in Ns,
   each a raster loop kernel (alexandria.raster.series/sum)."
  [power Ns]
  (mapv (fn [N] (:value (rs/sum (fn [n] (e// 1 (e/expt n power))) 1 N))) Ns))

(defn graded-e41
  "[{:label :grade}]: E41 paragraph by paragraph.
   par. 8   Euler's rule P = alpha, Q = P alpha - 2 beta, R = Q alpha - P beta
            + 3 gamma, S = ...: proved by Emmy for the k-th power sums of N
            symbolic roots, N = k = 1..4, alpha, beta, ... read off the
            truncated product prod (1 - a_n u) by Emmy's derivative
   par. 10-12 with y = 1 (roots q, q, -3q, -3q, 5q, 5q, ...): P, Q, R, S, T
            = 1, 1, 1/2, 1/3, 5/24; P = 1 is Leibniz's series = q/2 = p/4
            (the firmamentum); Q = 1 gives 1 + 1/9 + 1/25 + ... = p^2/8;
            adding a quarter of the whole gives 1 + 1/4 + 1/9 + ... = p^2/6
   par. 17-18 the sums of 1/n^2, ..., 1/n^12 (exact rationals times p^k)
   partial  Euler's partial sums 1, 1 + 1/4, ... by raster loop kernels,
            each below pi^2/6 by less than the tail bound 1/N (numeric,
            :source :raster)"
  []
  (let [sym-rule (fn [n]
                   (let [as (letters n)
                         P (truncated-product as)
                         es (mapv #(e/* (if (odd? %) -1 1) (taylor-coefficient P %)) (range 1 (inc n)))
                         ps (newton-sums es n)]
                     (for [k (range 1 (inc n))]
                       (e/- (ps (dec k)) (reduce e/+ 0 (map #(e/expt % k) as))))))
        [P Q] (euler-par-9)
        ;; the roots come in pairs: P = (2/q)(1 - 1/3 + 1/5 - ...),
        ;; Q = (2/q^2)(1 + 1/9 + 1/25 + ...), and q = p/2
        leibniz-in-p (* (/ P 2) 1/2)
        odd-squares-in-p2 (* (/ Q 2) 1/4)
        all-squares-in-p2 (* 4/3 odd-squares-in-p2)
        target (raster/value (fn [p] (e// (e/square p) 6)) Math/PI)
        Ns [1 2 3 4 10 100 1000]]
    (concat
     (for [n (range 1 5)]
       {:label (str "par. 8, N = " n " roots: P, Q, R, S by Euler's rule from alpha, beta, gamma, delta are the sums of the powers 1 to " n)
        :grade (variant :symbolic (sym-rule n))})
     [{:label "par. 10-12 (sin s = 1): P, Q, R, S, T = 1, 1, 1/2, 1/3, 5/24"
       :grade (variant :symbolic (map - (euler-par-9) [1 1 1/2 1/3 5/24]))}
      {:label "par. 10: P = 1 gives 1 - 1/3 + 1/5 - 1/7 + ... = q/2 = p/4, Leibniz's series (the firmamentum)"
       :grade (variant :symbolic [(- leibniz-in-p 1/4)])}
      {:label "par. 11: Q = 1 gives 1 + 1/9 + 1/25 + ... = q^2/2 = p^2/8"
       :grade (variant :symbolic [(- odd-squares-in-p2 1/8)])}
      {:label "par. 11: 1 + 1/4 + 1/9 + ... is the odd squares and a quarter of itself, so 4/3 of p^2/8 = p^2/6"
       :grade (variant :symbolic [(- all-squares-in-p2 1/6)])}
      {:label "par. 17-18: 1/n^2, 1/n^4, ..., 1/n^12 sum to p^2/6, p^4/90, p^6/945, p^8/9450, p^10/93555, 691 p^12/638512875"
       :grade (variant :symbolic (map e/- (:sums euler-par-17) [1/6 1/90 1/945 1/9450 1/93555 691/638512875]))}]
     (for [[N S] (map vector Ns (partial-sums-raster 2 Ns))]
       {:label (str "1 + 1/4 + ... + 1/" N "^2 = " (format "%.6f" S) " (raster), below pi^2/6 by less than 1/" N)
        :grade (variant :numeric [(- target S)] (/ 1.0 N))
        :source raster/source}))))

;; ---------------------------------------------------------------------------
;; e^{ix} = cos x + i sin x, on truncations

(def i (e/complex 0 1))

(defn- truncate [series N x]
  (reduce e/+ 0 (for [k (range (inc N))] (e/* (coefficient series k) (e/expt x k)))))

(defn euler-formula-difference
  "exp truncated at order N at i x, minus (cos truncated at N) + i (sin
   truncated at N), at the symbol x."
  [N]
  (let [x 'x]
    (e/- (truncate s/exp-series N (e/* i x))
         (e/+ (truncate s/cos-series N x) (e/* i (truncate s/sin-series N x))))))

(defn graded-euler-formula
  "[{:label :grade}]: the truncations to orders 1..max-order (proved), and
   e^{i pi} + 1 = 0 (proved from cos pi = -1 and sin pi = 0)."
  ([] (graded-euler-formula 16))
  ([max-order]
   (conj (vec (for [N (range 1 (inc max-order))]
                {:label (str "order " N ": sum_{k<=" N "} (ix)^k/k! = (cos x)_" N " + i (sin x)_" N)
                 :grade (variant :symbolic [(euler-formula-difference N)])}))
         {:label "x = pi: cos pi + i sin pi + 1 = 0, that is e^{i pi} + 1 = 0"
          :grade (variant :symbolic [(e/+ (e/cos 'pi) (e/* i (e/sin 'pi)) 1)])})))

(declare spiral)

(defn spiral-partial-sums
  "The first n+1 partial sums of e^{ix} as plane points [re im]: the
   spiral figure at k = 0 .. n, evaluated by its raster kernels
   (alexandria.raster/sample), n <= max-terms."
  [x n]
  (raster/sample (fn [k] ((spiral x) [k])) (mapv vector (range (inc n)))))

;; ---------------------------------------------------------------------------
;; Figures, for media

(def max-terms
  "Terms a figure carries; its parameter n turns them on one by one."
  16)

(defn- clamp01 [x] (e/divide (e/+ 1 (e/- (e/abs x) (e/abs (e/- x 1)))) 2))

(defn spiral
  "Point k (real, 0 <= k <= max-terms) along the partial sums of e^{ix}:
   at integer k the k-th partial sum 1 + ix + (ix)^2/2! + ... + (ix)^k/k!,
   between integers the next segment drawn to that fraction."
  [x]
  (fn [[k]]
    (let [terms (for [j (range 1 (inc max-terms))
                      :let [w (clamp01 (e/- k (dec j)))
                            t (e/divide (e/expt x j) (reduce * 1 (range 1 (inc j))))
                            [re im] ([[1 0] [0 1] [-1 0] [0 -1]] (mod j 4))]]
                  [(e/* w re t) (e/* w im t)])]
      [(reduce e/+ 1 (map first terms)) (reduce e/+ 0 (map second terms))])))

(defn partial-product
  "The graph of prod_{n <= m} (1 - x^2/(n^2 pi^2)) at x, factor n turned on
   by the weight clamp01(m - n + 1), so m may move continuously."
  [m]
  (fn [[x]]
    [x (reduce e/* 1 (for [n (range 1 (inc max-terms))
                           :let [w (clamp01 (e/- m (dec n)))]]
                       (e/- 1 (e/* w (e/divide (e/square x) (e/square (e/* n Math/PI)))))))]))

(defn sinc
  "The graph of sin x / x."
  []
  (fn [[x]] [x (e/divide (e/sin x) (e/+ x 1e-12))]))

(def figures
  "Every moving figure, by name: {:f figure :params initial-params :state
   initial-state :opts}. Both compile unsimplified: expanding sixteen
   factors through Emmy's polynomial simplifier overflows the stack and
   gains nothing for evaluation."
  {:spiral {:f spiral :params [Math/PI] :state [0] :opts {:simplify? false}}
   :product {:f partial-product :params [1] :state [0.5] :opts {:simplify? false}}})

;; ---------------------------------------------------------------------------
;; The proofs as data

(def proofs-resource "alexandria/euler/opera.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
