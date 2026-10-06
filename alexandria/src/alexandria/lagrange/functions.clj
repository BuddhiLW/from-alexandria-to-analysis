(ns alexandria.lagrange.functions
  "Lagrange, Theorie des fonctions analytiques (Paris 1797), art. 52: every
   function develops as f(x) = f(0) + x f'(u), = f(0) + x f'(0) + x^2/2 f''(u),
   ..., with u some quantity between 0 and x. The first line is the mean
   value theorem (the secant and the parallel tangent); the others are the
   Taylor series with Lagrange's remainder.

   Reuse searched before writing: Emmy's D gives the derivatives and the
   Taylor coefficients (exact); raster's Brent (alexandria.raster/root)
   finds the mean point numerically and raster kernels evaluate every
   number (alexandria.raster/value). Nothing here re-implements them.

     secant-slope, tangent-point   Lagrange's u for f = x^3 on [a, b], exact
     remainder-u                   the u of the Lagrange remainder of exp,
                                   a raster kernel of Emmy's formula"
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The mean value theorem, f(b) - f(a) = (b - a) f'(u)

(defn cube [x] (e/cube x))

(defn secant-slope [f a b] (e// (e/- (f b) (f a)) (e/- b a)))

(defn cube-mean-point
  "For f = x^3 on [a, b] the point where the tangent is parallel to the
   secant: u = sqrt((a^2 + ab + b^2)/3), which lies between a and b when
   0 <= a < b."
  [a b]
  (e/sqrt (e// (e/+ (e/square a) (e/* a b) (e/square b)) 3)))

(defn cube-mean-identity
  "f'(u) minus the secant slope for f = x^3: zero for every a, b."
  []
  (e/- ((e/D cube) (cube-mean-point 'a 'b)) (secant-slope cube 'a 'b)))

(defn cube-between
  "a^2 <= u^2 <= b^2 differences for 0 <= a < b: u^2 - a^2 = (b - a)(b + 2a)/3
   and b^2 - u^2 = (b - a)(2b + a)/3, both products of positives."
  []
  (let [u2 (e/square (cube-mean-point 'a 'b))]
    [(e/- (e/- u2 (e/square 'a)) (e// (e/* (e/- 'b 'a) (e/+ 'b (e/* 2 'a))) 3))
     (e/- (e/- (e/square 'b) u2) (e// (e/* (e/- 'b 'a) (e/+ (e/* 2 'b) 'a)) 3))]))

(defn mean-point
  "Numerically, a u in (a, b) where f'(u) equals the secant slope: Emmy
   writes f'(u) - slope (its D), raster's Brent finds the zero
   (alexandria.raster/root). a and b must bracket it."
  [f a b]
  (let [slope (secant-slope f a b)
        df (e/D f)]
    (:value (raster/root (fn [u] (e/- (df u) slope)) a b))))

;; ---------------------------------------------------------------------------
;; The Lagrange remainder

(defn taylor-polynomial
  "The first n terms of the Taylor series of f at 0, as an Emmy function:
   sum_{k < n} f^(k)(0) / k! x^k, the coefficients exact (Emmy's D)."
  [f n]
  (let [cs (mapv (fn [k] (e// (((e/expt e/D k) f) 0) (reduce * 1 (range 1 (inc k))))) (range n))]
    (fn [x] (reduce e/+ 0 (map-indexed (fn [k c] (e/* c (e/expt x k))) cs)))))

(defn taylor-partial
  "The sum of the first n terms of the Taylor series of f at 0, at x: the
   polynomial by Emmy, its value by a raster kernel."
  [f n x]
  (raster/value (taylor-polynomial f n) x))

(defn remainder-u
  "Lagrange's u for f = exp after n terms at x: the u in (0, x) with
   f(x) - (first n terms) = x^n / n! f^(n)(u). For exp f^(n) = exp, so
   u = log(n! R / x^n), and the claim is 0 < u < x. Emmy writes u as a
   function of x; a raster kernel evaluates it."
  [n x]
  (let [p (taylor-polynomial e/exp n)
        nf (reduce * 1 (range 1 (inc n)))]
    (raster/value (fn [x] (e/log (e// (e/* nf (e/- (e/exp x) (p x))) (e/expt x n)))) x)))

;; ---------------------------------------------------------------------------
;; Moving figure

(defn cubic-figure
  "Figure for media: the cubic y = x^3/2 - x + 1/2 (plain numbers), state [x]."
  []
  (fn [[x]] [x (e/+ (e// (e/cube x) 2) (e/- x) 1/2)]))

(defn exp-partial-figure
  "Figure for media: half the first n terms of exp's Taylor series (n = 0
   gives exp itself), state [x] -> [x, p_n(x)/2]; the polynomial is Emmy's
   (taylor-polynomial), the numbers a raster kernel's."
  [n]
  (let [p (if (zero? n) e/exp (taylor-polynomial e/exp n))]
    (fn [] (fn [[x]] [x (e/* 1/2 (p x))]))))

(def figures
  (merge {:cubic {:f cubic-figure :params [] :state [0]}}
         (into {} (for [n (range 5)]
                    [(keyword (str "exp-" n)) {:f (exp-partial-figure n) :params [] :state [0]}]))))

;; ---------------------------------------------------------------------------
;; Graded

(defn- components [x]
  (let [v (e/simplify x)] (if (e/structure? v) (flatten (seq v)) [v])))

(defn- g [kind diffs]
  (let [res (grade/grade kind (mapcat components diffs))]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn graded
  "[{:id :label :grade}] for art. 52; the numeric grade carries :engine
   :raster."
  []
  (let [us (for [n [1 2 3 4] x [0.5 1.0 2.0]] [x (remainder-u n x)])]
    [{:id :mean-value :label "for x^3 on [a, b]: f'(u) = (f(b) - f(a))/(b - a) at u^2 = (a^2 + ab + b^2)/3"
      :grade (g :symbolic [(cube-mean-identity)])}
     {:id :mean-value :label "that u lies between a and b: u^2 - a^2 and b^2 - u^2 factor with (b - a)"
      :grade (g :symbolic (cube-between))}
     {:id :remainder :label "exp: Lagrange's u lies strictly between 0 and x (n = 1..4, x = 1/2, 1, 2), computed by raster"
      :grade (if (every? (fn [[x u]] (< 0 u x)) us) :grade/numeric :grade/fails) :engine :raster}]))

(def proofs-resource "alexandria/lagrange/fonctions_analytiques.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
