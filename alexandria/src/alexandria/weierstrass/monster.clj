(ns alexandria.weierstrass.monster
  "Weierstrass: uniform convergence, the M-test, and the continuous function
   with no derivative anywhere (address to the Berlin Academy, 18 July
   1872; Mathematische Werke II, pp. 71-74).

     series      partial sums of the series the notebook compares: the
                 geometric series on [-1/2 1/2] (uniform, M_n = 2^-n) and
                 Abel's sin x - sin 2x/2 + sin 3x/3 - ... (not uniform)
     monster     f(x) = sum b^n cos(a^n x pi) in Weierstrass' letters:
                 a odd, 0 < b < 1, ab > 1 + 3pi/2. Here `a` and `b` follow
                 his paper; the notebook warns that today's books swap them.
                 Evaluated with exact rational reduction of the angle, so
                 b^... large frequencies stay accurate
     estimate    his x', x'' around x0 at stage m and the bound
                 |difference quotient| >= (ab)^m (2/3 - pi/(ab - 1))
     figures     Emmy functions for media (geometric, abel, monster window)
     identities  graded by alexandria.grade

   Reuse searched (carto, Emmy, desargues, raster): Emmy's series
   (emmy.series) builds power series lazily but has no uniform-norm or
   M-test, no nowhere-differentiable function, no exact cos(pi r) for
   rational r; desargues has none. The trigonometric steps are Emmy's
   (emmy.simplify.rules/trig:product->sum), the rest is written here."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [emmy.expression :as x]
            [emmy.simplify :as simp]
            [emmy.simplify.rules :as rules]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]
            [alexandria.raster.series :as rs]))

;; ---------------------------------------------------------------------------
;; Exact angles

(defn- floor*
  "The greatest integer <= q, exact for integers and rationals of any size."
  [q]
  (cond (integer? q) q
        (ratio? q) (let [n (quot (numerator q) (denominator q))]
                     (if (and (neg? q) (not= q n)) (dec n) n))
        :else (long (Math/floor (double q)))))

(defn- pow* "base^n, exact." [base n] (reduce * 1 (repeat n base)))

(defn cos-pi
  "cos(pi q) for a rational q: the angle reduced mod 2 EXACTLY first (a^n x
   is a big rational, never a float), so the cosine a raster kernel then
   evaluates has its argument in [0, 2 pi): no large factor reaches the
   trig (principle 20261004085255-241845ea), and the error is raster's libm
   error alone, not multiplied by a^n."
  [q]
  (let [q (rationalize q)
        m (- q (* 2 (floor* (/ q 2))))]
    (cond (zero? m) 1.0 (= 1 m) -1.0 :else (raster/value (fn [t] (e/cos (e/* Math/PI t))) m))))

;; ---------------------------------------------------------------------------
;; The monster, in Weierstrass' letters: a odd, 0 < b < 1

(def a "Weierstrass' odd integer." 13)
(def b "Weierstrass' constant below 1." 1/2)

(defn threshold "1 + 3 pi/2: ab must exceed it." [] (+ 1 (* 3/2 Math/PI)))

(defn monster
  "f(x) = sum_{n < terms} b^n cos(a^n x pi), x rational. a^n x is reduced
   mod 2 exactly (big rationals), b^n is an exact rational, and each
   cosine of the reduced angle is a raster kernel (cos-pi). So the error
   of f is at most libm-error * sum b^n <= 2 libm-error (raster's cos is
   good to ~5e-6): it does NOT grow with a^n, which would have made the
   naive cos(a^n x pi) worthless at n = 60."
  ([x] (monster x 60))
  ([x terms]
   (let [x (rationalize x)]
     (reduce + (map (fn [n] (* (double (pow* b n)) (cos-pi (* (pow* (bigint a) n) x))))
                    (range terms))))))

(defn stage
  "Weierstrass' construction at x0 and m: alpha_m the integer with
   x_{m+1} = a^m x0 - alpha_m in (-1/2, 1/2], x' = (alpha_m - 1)/a^m,
   x'' = (alpha_m + 1)/a^m. Exact rationals."
  [x0 m]
  (let [x0 (rationalize x0)
        am (pow* a m)
        t (* am x0)
        alpha (let [c (floor* (+ t 1/2))] (if (= (- t c) -1/2) (dec c) c))]
    {:x0 x0 :m m :alpha alpha :x-next (- t alpha)
     :x' (/ (dec alpha) am) :x'' (/ (inc alpha) am)}))

(defn quotients
  "{:m :left :right :bound}: the difference quotients (f(x') - f(x0))/(x' -
   x0) and (f(x'') - f(x0))/(x'' - x0) at stage m, and Weierstrass' lower
   bound (ab)^m (2/3 - pi/(ab - 1)) on their size."
  [x0 m]
  (let [{:keys [x0 x' x''] :as st} (stage x0 m)
        terms (+ m 60)
        f0 (monster x0 terms)
        q (fn [x1] (/ (- (monster x1 terms) f0) (double (- x1 x0))))
        ab (* a (double b))]
    (assoc st :left (q x') :right (q x'')
           :bound (* (Math/pow ab m) (- 2/3 (/ Math/PI (- ab 1)))))))

;; ---------------------------------------------------------------------------
;; The two series of the uniform-convergence comparison

(defn geometric-partial "1 + x + ... + x^(N-1)." [n-terms x] (reduce + (map #(Math/pow x %) (range n-terms))))

(defn abel-partial
  "sin x - sin 2x/2 + ... (N terms); its sum is x/2 on (-pi pi), 0 at pi."
  [n-terms x]
  (reduce + (map (fn [k] (* (if (odd? k) 1 -1) (/ (Math/sin (* k x)) k))) (range 1 (inc n-terms)))))

(defn sup-error
  "max over the grid xs of |S_N(x) - S(x)|."
  [partial limit n-terms xs]
  (apply max (map #(Math/abs (- (partial n-terms %) (limit %))) xs)))

(defn- grid [lo hi k] (map #(+ lo (* (- hi lo) (/ % (double k)))) (range (inc k))))

(def abel-grid "Points of [0 pi): the error near pi decides." (grid 0 (* Math/PI (- 1 1e-9)) 4000))

;; ---------------------------------------------------------------------------
;; Figures (Emmy functions of one point, for media)

(defn- clamp01 [x] (e// (e/+ 1 (e/- (e/abs x) (e/abs (e/- x 1)))) 2))

(def geometric-terms 24)
(def abel-terms 40)
(def monster-terms 9)

(defn geometric-graph
  "[x S_N(x)] of the geometric series, terms fading in as N grows."
  [n]
  (fn [[x]]
    [x (reduce e/+ (for [k (range geometric-terms)]
                     (e/* (clamp01 (e/- n k)) (e/expt x k))))]))

(defn abel-graph
  "[x S_N(x)] of Abel's series, terms fading in as N grows."
  [n]
  (fn [[x]]
    [x (reduce e/+ (for [k (range 1 (inc abel-terms))]
                     (e/* (clamp01 (e/- n (dec k))) (if (odd? k) 1 -1) (e// (e/sin (e/* k x)) k))))]))

(defn monster-window
  "The monster around x0 in a window of half-width w, its rise divided by
   h, with the first k terms (k may be fractional: term n fades in as k
   passes from n to n + 1). State [s], s in [-1 1] across the window; the
   point [s (f(x) - f(x0))/h] with x = x0 + s w."
  [x0 w h k]
  (fn [[s]]
    (let [xx (e/+ x0 (e/* s w))
          f (fn [t] (reduce e/+ (for [n (range monster-terms)]
                                  (e/* (clamp01 (e/- k n)) (Math/pow (double b) n)
                                       (e/cos (e/* (Math/pow a n) Math/PI t))))))]
      [s (e// (e/- (f xx) (f x0)) h)])))

(def figures
  "Every moving figure: {:f :params :state :opts}."
  {:geometric {:f geometric-graph :params [1] :state [0.1] :opts {:simplify? false}}
   :abel {:f abel-graph :params [1] :state [0.1] :opts {:simplify? false}}
   :monster {:f monster-window :params [0.3 1 1 9] :state [0.1] :opts {:simplify? false}}})

(def roughness
  "-ln b / ln a: scaling the rise by w^roughness when the window shrinks to
   w keeps the picture equally rough at every magnification."
  (/ (- (Math/log (double b))) (Math/log a)))

;; ---------------------------------------------------------------------------
;; What the address says, graded

(defn- product->sum [expr]
  (-> expr x/expression-of rules/trig:product->sum simp/simplify-expression simp/simplify-expression))

(def identities
  "[label difference], each zero. Emmy's simplifier does not telescope a
   symbolic exponent, so the finite geometric sum is checked for m = 1..8
   with r symbolic."
  (let [n 'n r 'r q 'q K 'K]
    [["M-test, geometric majorant: b^N + ... + b^(N+K) = b^N (1 - b^(K+1))/(1 - b)"
      (e/- (e// (e/- (e/expt q n) (e/expt q (e/+ n K 1))) (e/- 1 q))
           (e/* (e/expt q n) (e// (e/- 1 (e/expt q (e/+ K 1))) (e/- 1 q))))]
     ["M-test, M_n = 1/n^2: 1/(n(n-1)) = 1/(n-1) - 1/n, so the tail after N is below 1/N"
      (e/- (e// 1 (e/* n (e/- n 1))) (e/- (e// 1 (e/- n 1)) (e// 1 n)))]
     ["cos A - cos B = -2 sin((A+B)/2) sin((A-B)/2), Weierstrass' first step"
      (let [A (e/* 'pi (e/expt 'a n) 'x1) B (e/* 'pi (e/expt 'a n) 'x0)]
        (product->sum (e/- (e/- (e/cos A) (e/cos B))
                           (e/* -2 (e/sin (e// (e/+ A B) 2)) (e/sin (e// (e/- A B) 2))))))]
     ["the first m terms: 1 + ab + ... + (ab)^(m-1) = ((ab)^m - 1)/(ab - 1) < (ab)^m/(ab - 1), m = 1..8"
      (reduce e/+ (for [m (range 1 9)]
                    (e/square (e/- (reduce e/+ (map #(e/expt r %) (range m)))
                                   (e// (e/- (e/expt r m) 1) (e/- r 1))))))]
     ["ab > 1 + 3pi/2 exactly when 2/3 - pi/(ab - 1) > 0: (2/3)(ab - 1) - pi = (2/3)(ab - 1 - 3pi/2)"
      (e/- (e/- (e/* 2/3 (e/- r 1)) 'pi) (e/* 2/3 (e/- r (e/+ 1 (e/* 3/2 'pi)))))]]))

(defn- variant
  ([kind diffs] (variant kind diffs grade/default-tolerance))
  ([kind diffs tol]
   (let [res (grade/grade kind diffs tol)]
     (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))))

(def sample-points "Where the estimate is tried: rationals and 0." [0 3/10 1/7 5/11 -2/9])

(defn exact-checks
  "Decided exactly in rationals: [{:label :holds?}]."
  []
  [{:label (str "a = " a " is odd, b = " b ", ab = " (* a b) " > 1 + 3pi/2")
    :holds? (and (odd? a) (< 0 b 1) (> (* a b) (threshold)))}
   {:label "x' < x0 <= x'', both within 3/(2 a^m) of x0 (m = 1..6, five points)"
    :holds? (every? (fn [[x0 m]] (let [{:keys [x0 x' x'']} (stage x0 m) am (pow* a m)]
                                   (and (< x' x0) (< x0 x'') (<= (- x'' x') (/ 2 am)) (<= (- x0 x') (/ 3 (* 2 am))))))
                    (for [x0 sample-points m (range 1 7)] [x0 m]))}
   {:label "a odd: cos(a^(m+n) x' pi) = -(-1)^alpha_m for every later term (m = 3, n = 1..4)"
    :holds? (every? (fn [[x0 n]] (let [{:keys [x' alpha]} (stage x0 3)]
                                   (== (cos-pi (* (pow* a (+ 3 n)) x')) (if (even? alpha) -1 1))))
                    (for [x0 sample-points n (range 1 5)] [x0 n]))}])

(def libm-error
  "The absolute error budget of one raster cosine (wasm libm measured
   < 5e-6; the JVM kernel is at least as good), and of f: sum b^n of it."
  5e-6)

(defn quotient-error-bound
  "What raster's libm can do to a difference quotient at stage m: two
   values of f each off by at most 2 libm-error, divided by |x' - x0| >=
   1/(2 a^m) (x_{m+1} in (-1/2, 1/2]): 8 libm-error a^m."
  [m]
  (* 8 libm-error (Math/pow a m)))

(defn numeric-checks
  "Measured, not derived: [{:label :grade :rows :source}]. The quotients
   use f from raster cosines of exactly reduced angles; Weierstrass'
   bound is required to hold with the libm budget subtracted, so a raster
   rounding cannot pass a false claim. The sup errors are raster kernels:
   the geometric S_N written out (literal powers, which raster lowers to
   products) inside one grid sweep, Abel's terms nested in the grid loop."
  []
  (let [rows (for [x0 sample-points m (range 1 7)] (assoc (quotients x0 m) :libm-margin (quotient-error-bound m)))
        ok? (every? (fn [{:keys [left right bound libm-margin]}]
                      (and (>= (- (abs left) libm-margin) bound) (>= (- (abs right) libm-margin) bound)
                           (neg? (* left right))))
                    rows)
        geo (map (fn [N] (:value (rs/sup (fn [x] (e/abs (e/- (reduce e/+ (map #(e/expt x %) (range N)))
                                                              (e// 1 (e/- 1 x)))))
                                         -0.5 0.5 401)))
                 [5 10 20 40])
        abel (map (fn [N] (:value (rs/sup-of-sum (fn [k x] (e// (e/sin (e/* k x)) k)) (fn [x] (e// x 2))
                                                 0 (* Math/PI (- 1 1e-9)) 4001 {:abs? true :alternating? true :to N})))
                  [5 10 20 40])]
    [{:label "Weierstrass' estimate: both quotients at least (ab)^m (2/3 - pi/(ab-1)) beyond raster's libm margin 8e-6 a^m, of opposite signs (m = 1..6, five points)"
      :grade (if ok? :grade/numeric :grade/fails) :rows (vec rows) :source raster/source}
     {:label "geometric series on [-1/2 1/2]: sup |S_N - S| <= 2^(1-N) for N = 5, 10, 20, 40"
      :grade (if (every? true? (map (fn [err nn] (<= err (* 2 (Math/pow 0.5 nn)))) geo [5 10 20 40])) :grade/numeric :grade/fails)
      :errors (vec geo) :source raster/source}
     {:label "Abel's series on [0 pi): sup |S_N - x/2| stays near pi/2 for N = 5, 10, 20, 40"
      :grade (if (every? #(> % 1.4) abel) :grade/numeric :grade/fails)
      :errors (vec abel) :source raster/source}]))

(defn graded
  "[{:label :grade}] of every identity and check of the address."
  []
  (vec
   (concat
    (map (fn [[label diff]] {:label label :grade (variant :symbolic [diff])}) identities)
    (map (fn [{:keys [label holds?]}] {:label label :grade (if holds? :grade/proved :grade/fails)}) (exact-checks))
    (map #(select-keys % [:label :grade]) (numeric-checks)))))

;; ---------------------------------------------------------------------------
;; The address as data

(def proofs-resource "alexandria/weierstrass/monster.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
