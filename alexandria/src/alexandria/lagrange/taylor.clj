(ns alexandria.lagrange.taylor
  "Theorie des fonctions analytiques (1797), no 40 (Oeuvres IX; art. 52 of
   the 1797 numbering in alexandria.lagrange.functions): f(x) = f + x f' +
   ... + x^n/n! f^(n)(u), u between 0 and x, so the rest of the series lies
   between the least and greatest values of its first term. For f = sin
   every derived function is sin, cos, -sin or -cos, of size at most 1,
   so after n terms the rest is at most B = x^n / n! (x > 0): the order n
   and the bound B are tied through x.

   Emmy derives everything below; nothing is typed in as a result:

     the cycle         D sin = cos, D^2 sin = -sin, D^3 sin = -cos,
                       D^4 sin = sin (so |f^(n)| <= 1)
     the polynomial    the first n terms, coefficients by Emmy's D at 0
                       (alexandria.lagrange.functions/taylor-polynomial);
                       the widget's branch-free form (one polynomial in x
                       with the order n a parameter) equals it, n = 1..12
     the bound         log B = n log x - log n!; one more term multiplies
                       B by x/(n + 1)
     solved back       x = exp((log n! + log B)/n) (Emmy solves
                       n log x = log n! + log B, linear in log x)
     the tie           x -> B -> x and B -> x -> B compose to the
                       identity, :grade/proved

   The order n is a number on a slider, so the widget's kernels take it as
   a parameter: [k <= n] is the branch-free `step` (max(0, n - k + 1) -
   max(0, n - k), exact 0 or 1 for integers), log n! the sum of
   [k <= n] log k, and P_n the sum of the series' terms with [k < n]. All
   three are linear in their abs atoms, so Emmy's simplify (which raster's
   JVM side runs) and the browser compile stay small."
  (:require [alexandria.apollonius.projection :as pj]
            [alexandria.grade :as grade]
            [alexandria.lagrange.functions :as lf]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(def max-order "The widget's largest order." 12)

(defn- pos-part [x] (e/* 1/2 (e/+ x (e/abs x))))

(defn step
  "[k <= n] for integers n, k: max(0, n - k + 1) - max(0, n - k)."
  [n k]
  (e/- (pos-part (e/+ (e/- n k) 1)) (pos-part (e/- n k))))

(defn log-factorial
  "log n! = sum_{k=2}^{max-order} [k <= n] log k, n an integer in
   [0, max-order] (a number or a symbol)."
  [n]
  (reduce e/+ 0 (for [k (range 2 (inc max-order))] (e/* (step n k) (e/log k)))))

(defn log-bound
  "log B = n log x - log n!, x > 0 (sin is odd: the bound at -x is the
   same). lnN is log n!."
  [x n lnN]
  (e/- (e/* n (e/log x)) lnN))

(defn x-of-log-bound
  "x > 0 from log B (n held): the root L of n L - (log n! + log B) = 0 by
   Emmy's solve-linear, x = exp L."
  [lnB n lnN]
  (e/exp (:root (pj/solve-linear-in (fn [L] (e/- (e/* n L) (e/+ lnN lnB)))))))

(def sin-coefficients
  "The Taylor coefficients of sin at 0, k = 0 .. max-order - 1, by Emmy's D
   (exact rationals)."
  (delay (mapv (fn [k] (e// (((e/expt e/D k) e/sin) 0) (reduce * 1 (range 1 (inc k))))) (range max-order))))

(defn partial-sum
  "The first n terms of sin's series at t, one polynomial with the order n
   a parameter: the term of t^k counts where k + 1 <= n."
  [n t]
  (reduce e/+ 0 (for [[k c] (map-indexed vector @sin-coefficients) :when (not= 0 c)]
                  (e/* c (step n (inc k)) (e/expt t k)))))

(def derivation
  (delay
    {:cycle (mapv (fn [k] (e/simplify (((e/expt e/D k) e/sin) 'x))) (range 5))
     :log-bound (log-bound 'x 'n 'lnN)
     :x-of-bound (e/simplify (x-of-log-bound 'lnB 'n 'lnN))
     :p6 (e/simplify ((lf/taylor-polynomial e/sin 6) 'x))}))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [label diffs]
  {:label label :grade (variant (grade/grade :symbolic (vec diffs)))})

(defn grades
  "[{:label :grade}]: the derivation and the tie (x > 0)."
  []
  (let [d @derivation
        orders (range 0 (inc max-order))]
    [(proved "sin's derived functions cycle: D sin = cos, D² sin = −sin, D³ sin = −cos, D⁴ sin = sin"
             (map e/- (:cycle d) [(e/sin 'x) (e/cos 'x) (e/- (e/sin 'x)) (e/- (e/cos 'x)) (e/sin 'x)]))
     (proved "the widget's polynomial with the order as a parameter is Emmy's Taylor polynomial, n = 1..12"
             (for [n (range 1 (inc max-order))]
               (e/- (partial-sum n 'x) ((lf/taylor-polynomial e/sin n) 'x))))
     (proved "the branch-free step is [k ≤ n], so log n! = Σ [k ≤ n] log k, n, k = 0..12"
             (for [n orders k orders] (e/- (step n k) (if (<= k n) 1 0))))
     (proved "one more term multiplies the bound by x/(n + 1): log B grows by log x − log(n + 1)"
             [(e/- (log-bound 'x (e/+ 'n 1) (e/+ 'lnN (e/log (e/+ 'n 1))))
                   (e/+ (log-bound 'x 'n 'lnN) (e/log 'x) (e/- (e/log (e/+ 'n 1)))))])
     (proved "solved back: x = exp((log n! + log B)/n) has the bound B"
             [(e/- (log-bound (:x-of-bound d) 'n 'lnN) 'lnB)])
     (proved "forward then inverse: x from the bound of x (x > 0)"
             [(e/- (x-of-log-bound (log-bound 'x 'n 'lnN) 'n 'lnN) 'x)])
     (proved "inverse then forward: the bound of x(B) is B"
             [(e/- (log-bound (x-of-log-bound 'lnB 'n 'lnN) 'n 'lnN) 'lnB)])]))

(defn- eq [lhs rhs]
  (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn equations
  "[{:label :eqs}] for emmy.clerk/->TeX."
  []
  (let [d @derivation]
    [{:label "the derived functions of sin (Emmy's D), each at most 1 in size"
      :eqs (map-indexed (fn [k v] (eq (list (list 'expt 'D k) (list 'sin 'x)) v)) (:cycle d))}
     {:label "the first six terms, their coefficients by D at 0"
      :eqs [(eq 'P_6 (:p6 d))]}
     {:label "Lagrange's limits: after n terms the rest is at most B = xⁿ/n!, that is"
      :eqs [(eq (list 'log 'B) (:log-bound d))]}
     {:label "solved back for x (n log x = log n! + log B, linear in log x)"
      :eqs [(eq 'x (:x-of-bound d))]}]))
