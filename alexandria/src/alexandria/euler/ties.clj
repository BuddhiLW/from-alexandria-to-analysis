(ns alexandria.euler.ties
  "The equations behind the Euler notebook's widgets, derived and solved by
   Emmy, and their forward and inverse maps (widget brief, section 5).

     Basel (E41)     the tail of sum 1/n^2 after n terms lies between
                     1/(n+1) and b = 1/n: Emmy differentiates the
                     antiderivative F(x) = -1/x back to 1/x^2, reads the
                     bound b = F(inf) - F(n), and solves b n = 1 for n
                     (emmy.generic/solve-linear). Tie: n <-> b.
     e (E101 VII)    (1 + 1/n)^n = exp(n log(1 + 1/n)); with u = 1/n Emmy
                     expands n log(1 + 1/n) = 1 - u/2 + u^2/3 - ... and
                     composes it with exp: the error eps = e - (1 + 1/n)^n
                     is e (u/2 - 11u^2/24 + 7u^3/16 - ...). Tie: n <-> omega
                     = 1/n (Euler's infinitely small omega, par. 114-116)
                     <-> eps; the inverse eps -> n starts at e/(2 eps), the
                     leading term solved, and takes Newton steps with
                     Emmy's derivative of eps.
     e^{ix} (VIII)   f(x) = e^{-ix} (cos x + i sin x) has f' = 0 (Emmy's D)
                     and f(0) = 1, so e^{ix} = cos x + i sin x. Tie: x <->
                     (cos x, sin x), the inverse by atan2 keeping the other
                     coordinate's sign.
     V - E + F       (E231) cutting off k solid angles of degrees d_1..d_k
                     adds sum (d_j - 1) vertices, sum d_j edges and k faces;
                     Emmy simplifies the change of V - E + F to 0 for each k.

   Every map is an Emmy function of numbers or symbols: alexandria.raster
   compiles it to kernels for the page, and `grades` checks it
   symbolically (:grade/proved) or by raster (:grade/numeric)."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [emmy.series :as s]
            [hive-dsl.result :as r]
            [alexandria.solids :as solids]))

;; ---------------------------------------------------------------------------
;; Shared helpers

(defn- variant
  ([kind diffs] (variant kind diffs grade/default-tolerance))
  ([kind diffs tol]
   (let [res (grade/grade kind diffs tol)]
     (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))))

(defn solve-linear-in
  "The root of f(u) = 0 for f linear in u: f = a u + b with a = f'(0) and
   b = f(0) by Emmy's D, the root by emmy.generic/solve-linear (a x = -b),
   and :second = f''(u), which Emmy must simplify to 0."
  [f]
  (let [a ((e/D f) 0) b (f 0)]
    {:a a :b b :root (e/solve-linear a (e/- b)) :second ((e/D (e/D f)) 'u)}))

(defn at-least "max(x, lo), branch-free (raster)." [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn at-most "min(x, hi), branch-free (raster)." [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn clamp "x held in [lo hi], branch-free." [x lo hi] (at-most (at-least x lo) hi))

(defn- eq
  "lhs = rhs as one Emmy literal (->TeX renders it whole)."
  [lhs rhs]
  (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

;; ---------------------------------------------------------------------------
;; Basel: the tail bound

(defn antiderivative "F(x) = -1/x." [x] (e/- (e/invert x)))

(defn basel-forward "b of n: the tail bound F(inf) - F(n) = 1/n." [n] (e/- 0 (antiderivative n)))

(defn basel-inverse
  "n of b: the root of b n - 1 = 0, by solve-linear (linear in n)."
  [b]
  (:root (solve-linear-in (fn [n] (e/- (e/* b n) 1)))))

(defn basel-lower "The tail is above F(inf) - F(n + 1) = 1/(n + 1)." [n] (basel-forward (e/+ n 1)))

;; ---------------------------------------------------------------------------
;; e: the error of (1 + 1/n)^n

(defn power-n "(1 + 1/n)^n as exp(n log(1 + 1/n))." [n] (e/exp (e/* n (e/log (e/+ 1 (e/invert n))))))

(defn e-error "eps = e - (1 + 1/n)^n." [n] (e/- (e/exp 1) (power-n n)))

(def ^:private d-error (e/D e-error))

(def log-series
  "n log(1 + 1/n) = log(1 + u)/u in u = 1/n: Emmy's Taylor series of
   log(1 + u), shifted down one power."
  (delay (s/power-series* (rest (seq (e/taylor-series (fn [u] (e/log (e/+ 1 u))) 0))))))

(def error-coefficients
  "eps / e = 1 - exp(log(1 + u)/u - 1) as a series in u: Emmy composes
   exp-series with the log series. The first four coefficients."
  (delay
    (let [inner (e/- @log-series 1)
          composed (s/compose s/exp-series inner)]
      (mapv e/- (take 4 (cons 0 (rest (seq composed))))))))

(def e-ranges {:n [1 400] :eps [(double (e-error 400)) (double (e-error 1))]})

(defn e-start
  "The start of the inverse eps -> n: the leading term eps = e / (2n)
   solved for n (solve-linear), held at 1/2 or above."
  [eps]
  (at-least (:root (solve-linear-in (fn [n] (e/- (e/* 2 eps n) (e/exp 1))))) 0.5))

(defn e-slope
  "eps'(n) = -(1 + 1/n)^n (log(1 + 1/n) - 1/(n + 1)): the compact form of
   Emmy's D of e-error (graded equal to it), small enough for one kernel."
  [n]
  (e/- 0 (e/* (power-n n) (e/- (e/log (e/+ 1 (e/invert n))) (e/invert (e/+ n 1))))))

(defn e-newton
  "One Newton step towards the n whose error is eps: n - (eps(n) - eps) /
   eps'(n), held in [1/2, 1000]. One kernel; the caller iterates it."
  [n eps]
  (clamp (e/- n (e/divide (e/- (e-error n) eps) (e-slope n))) 0.5 1000))

(defn slope-grade
  "e-slope against Emmy's D of e-error: the difference simplified to 0."
  []
  {:label "ε′(n) by Emmy's D equals −(1 + 1/n)ⁿ (log(1 + 1/n) − 1/(n + 1)), the slope Newton's step uses"
   :grade (variant :symbolic [(e/- (d-error 'n) (e-slope 'n))])})

(def newton-steps "Newton steps the page and the grades take." 8)

(defn e-inverse
  "n of eps on the JVM: e-start, then `newton-steps` Newton steps, eps(n)
   and eps'(n) each by its raster kernel (alexandria.raster simplifies
   before it lowers, and the whole step simplified is too large for one
   JVM method; the page compiles e-newton unsimplified as one kernel).
   The iterate is held in [1/2, 1000]."
  [eps]
  (reduce (fn [n _] (min 1000.0 (max 0.5 (- n (/ (- (raster/value e-error n) eps) (raster/value e-slope n))))))
          (min 1000.0 (raster/value e-start eps)) (range newton-steps)))

(defn omega "Euler's omega = 1/n." [n] (e/invert n))

(defn omega-inverse "n of omega: the root of omega n - 1 = 0." [w] (basel-inverse w))

(defn k-of-a
  "E101 par. 114: a^omega = 1 + k omega for omega infinitely small; k is
   the slope of a^x at 0, which Emmy's D gives as log a."
  [a]
  ((e/D (fn [x] (e/expt a x))) 0))

(defn a-of-k
  "The base whose k is given: the root of log a = k, a = exp k (par. 122:
   k = 1 gives e)."
  [k]
  (e/exp k))

(defn base-grades
  "E101 par. 114-122 on the a <-> k tie."
  []
  [{:label "par. 114: the slope of aˣ at 0 is k = log a (Emmy's D)"
    :grade (variant :symbolic [(e/- (k-of-a 'a) (e/log 'a))])}
   {:label "inverse then forward: k(a(k)) = log(exp k) = k"
    :grade (variant :symbolic [(e/- (k-of-a (a-of-k 'k)) 'k)])}
   {:label "forward then inverse: a(k(a)) = exp(log a) = a"
    :grade (variant :symbolic [(e/- (a-of-k (k-of-a 'a)) 'a)])}
   {:label "par. 122: k = 1 gives the base e, whose slope at 0 is 1"
    :grade (variant :symbolic [(e/- (k-of-a (a-of-k 1)) 1)])}])

;; ---------------------------------------------------------------------------
;; e^{ix}

(def i (e/complex 0 1))

(defn cis "cos x + i sin x." [x] (e/+ (e/cos x) (e/* i (e/sin x))))

(defn- sgn [q] (e/divide q (at-least (e/abs q) 1e-300)))

(defn x-of-cos
  "x of c = cos x, keeping the sign of s (the point stays on its half)."
  [c s]
  (let [c (clamp c -1 1)] (e/atan (e/* (sgn s) (e/sqrt (e/- 1 (e/square c)))) c)))

(defn x-of-sin
  "x of s = sin x, keeping the sign of c."
  [s c]
  (let [s (clamp s -1 1)] (e/atan s (e/* (sgn c) (e/sqrt (e/- 1 (e/square s)))))))

;; ---------------------------------------------------------------------------
;; V - E + F under cutting off solid angles (E231)

(defn truncated-counts
  "{:V :E :F} after cutting off the solid angles of degrees ds (each cut
   shallow, so the cuts do not meet)."
  [{:keys [V E F]} ds]
  {:V (e/+ V (reduce e/+ 0 (map #(e/- % 1) ds)))
   :E (e/+ E (reduce e/+ 0 ds))
   :F (e/+ F (count ds))})

(defn- unit01 "clamp to [0 1], branch-free." [x] (clamp x 0 1))

(defn- neighbours
  "v's neighbours in the cyclic order of the faces round v: consecutive
   neighbours share a face (index sets of a convex solid)."
  [edges faces v]
  (let [nb (set (keep (fn [[a b]] (cond (= a v) b (= b v) a)) edges))
        share? (fn [a b] (some #(and (% v) (% a) (% b)) faces))]
    (loop [order [(first (sort nb))]]
      (if (= (count order) (count nb))
        order
        (recur (conj order (first (filter #(and (not (some #{%} order)) (share? (peek order) %)) (sort nb)))))))))

(defn cut-solid
  "Euler's E231 move on a solid {:vertices :faces :edges} (faces as index
   sets): cut off its solid angles one at a time. Returns
     :points   [[v w] ...] the cut points: on edge vw, near v
     :segments [[p q] ...] indices into :points (the shortened edges, then
               the new face round each cut vertex)
     :degree   the degree of each vertex, in cutting order
     :counts   {:V :E :F} of the solid
     :f        (fn [t k] -> flat [x y z ...]) the cut points: vertex v cut
               (weight clamp(k - v) in [0 1]) at depth t along each edge,
               an Emmy function the page compiles to one raster kernel.
   A vertex not yet cut collapses its cut points onto itself."
  [{:keys [vertices faces edges] :as s}]
  (let [vs (mapv (fn [v] (mapv (fn [x] (if (number? x) (double x) (raster/value (fn [_] x) 0.0))) v)) vertices)
        fsets (map set faces)
        es (map vec edges)
        orders (mapv #(neighbours es fsets %) (range (count vs)))
        points (vec (for [v (range (count vs)) w (orders v)] [v w]))
        idx (into {} (map-indexed (fn [i p] [p i]) points))
        segments (vec (concat (for [[a b] es] [(idx [a b]) (idx [b a])])
                              (for [v (range (count vs)) :let [o (orders v)]
                                    [w1 w2] (map vector o (concat (rest o) [(first o)]))]
                                [(idx [v w1]) (idx [v w2])])))]
    {:points points :segments segments :degree (mapv count orders)
     :counts (solids/counts s)
     :f (fn [t k]
          (vec (mapcat (fn [[v w]]
                         (let [a (vs v) b (vs w) wt (e/* (unit01 (e/- k v)) t 0.5)]
                           (mapv (fn [ai bi] (e/+ ai (e/* wt (- bi ai)))) a b)))
                       points)))}))

(defn- chi [{:keys [V E F]}] (e/+ (e/- V E) F))

;; ---------------------------------------------------------------------------
;; Grades

(defn grades
  "{:basel :e :cis :cut} -> [{:label :grade}]."
  []
  (let [ds (fn [k] (mapv #(symbol (str "d" %)) (range 1 (inc k))))
        [c0 c1 c2 c3] @error-coefficients
        euler-digits 271828182845904523536028N
        s30 (reduce + (map #(/ 1 (reduce * 1N (range 1 (inc %)))) (range 31)))]
    {:basel
     [{:label "D(−1/x) = 1/x²: F(x) = −1/x is an antiderivative of 1/n²"
       :grade (variant :symbolic [(e/- ((e/D antiderivative) 'x) (e/invert (e/square 'x)))])}
      {:label "b = F(∞) − F(n) = 1/n: the tail after n terms is below b"
       :grade (variant :symbolic [(e/- (basel-forward 'n) (e/invert 'n))])}
      {:label "b n − 1 is linear in n, solved: n = 1/b"
       :grade (variant :symbolic [(:second (solve-linear-in (fn [n] (e/- (e/* 'b n) 1))))
                                  (e/- (basel-inverse 'b) (e/invert 'b))])}
      {:label "forward then inverse: n(b(n)) = n" :grade (variant :symbolic [(e/- (basel-inverse (basel-forward 'n)) 'n)])}
      {:label "inverse then forward: b(n(b)) = b" :grade (variant :symbolic [(e/- (basel-forward (basel-inverse 'b)) 'b)])}]
     :e
     [{:label "n log(1 + 1/n) = 1 − u/2 + u²/3 − u³/4 + …, u = 1/n (Emmy's series of log(1 + u))"
       :grade (variant :symbolic (map e/- (take 4 (seq @log-series)) [1 -1/2 1/3 -1/4]))}
      {:label "e − (1 + 1/n)ⁿ = e (u/2 − 11u²/24 + 7u³/16 − …): exp composed with that series"
       :grade (variant :symbolic (map e/- [c0 c1 c2 c3] [0 1/2 -11/24 7/16]))}
      {:label "ω n − 1 is linear in n: n = 1/ω, and ω(n(ω)) = ω, n(ω(n)) = n"
       :grade (variant :symbolic [(e/- (omega-inverse 'omega) (e/invert 'omega))
                                  (e/- (omega (omega-inverse 'omega)) 'omega)
                                  (e/- (omega-inverse (omega 'n)) 'n)])}
      {:label "a Newton step from the root stays there: n − (ε(n) − ε(n))/ε′(n) = n"
       :grade (variant :symbolic [(e/- (e/- 'n (e/divide (e/- (e-error 'n) (e-error 'n)) (d-error 'n))) 'n)])}
      {:label (str "forward then inverse, by raster kernels: n(ε(n)) = n for n = 1, 2, 5, 10, 50, 100, 200 (e-start, then "
                   newton-steps " Newton steps)")
       :grade (variant :numeric (for [n [1 2 5 10 30 60 100]] (/ (- (e-inverse (raster/value e-error (double n))) n) n))
                       1e-7)
       :source raster/source}
      {:label "Euler's 23 decimals (par. 122), 2.71828182845904523536028: 1 + 1 + 1/2! + … + 1/30! truncated to 23 places, exact rationals"
       :grade (variant :symbolic [(- (quot (* s30 (bigint (.pow (biginteger 10) 23))) 1) euler-digits)])}]
     :cis
     [{:label "f(x) = e^{−ix}(cos x + i sin x): f′(x) = 0 (Emmy's D)"
       :grade (variant :symbolic [((e/D (fn [x] (e/* (e/exp (e/* (e/- i) x)) (cis x)))) 'x)])}
      {:label "f(0) = 1, so e^{ix} = cos x + i sin x"
       :grade (variant :symbolic [(e/- (e/* (e/exp 0) (cis 0)) 1)])}
      {:label "inverse then forward: sin(x(s)) = s, x(s) = atan2(s, √(1 − s²)) for c > 0"
       :grade (variant :symbolic [(e/- (e/sin (e/atan 's (e/sqrt (e/- 1 (e/square 's))))) 's)])}
      {:label "inverse then forward: cos(x(c)) = c, x(c) = atan2(√(1 − c²), c) for s > 0"
       :grade (variant :symbolic [(e/- (e/cos (e/atan (e/sqrt (e/- 1 (e/square 'c))) 'c)) 'c)])}
      {:label "forward then inverse, by raster kernels: x(cos x, sin x) = x on 25 angles in (−π, π)"
       :grade (variant :numeric (for [k (range 25) :let [x (- (* (/ (+ k 0.5) 25) 2 Math/PI) Math/PI)]]
                                  (- (raster/value x-of-cos (raster/value e/cos x) (raster/value e/sin x)) x))
                       1e-9)
       :source raster/source}]
     :cut
     (vec (for [k (range 1 5)]
            {:label (str "cut off " k " solid angle" (when (> k 1) "s") " of degrees " (apply str (interpose ", " (ds k)))
                         ": V − E + F unchanged")
             :grade (variant :symbolic [(e/- (chi (truncated-counts {:V 'V :E 'E :F 'F} (ds k))) (chi {:V 'V :E 'E :F 'F}))])}))}))

;; ---------------------------------------------------------------------------
;; The derivations, for ->TeX on the page

(defn equations
  "{:basel :e :cis :cut} -> [{:label :eqs}], every right-hand side as Emmy
   derived it."
  []
  (let [[_ c1 c2 c3] @error-coefficients
        u 'u]
    {:basel
     [{:label "the antiderivative, differentiated back by Emmy"
       :eqs [(eq '(F x) (antiderivative 'x)) (eq '(D F x) ((e/D antiderivative) 'x))]}
      {:label "the bound: the tail after n terms is below the area beyond n, and above the area beyond n + 1"
       :eqs [(eq 'b (basel-forward 'n)) (eq 'b_lower (basel-lower 'n))]}
      {:label "solve-linear: b n − 1 = 0" :eqs [(eq 'n (basel-inverse 'b))]}]
     :e
     [{:label "the power as an exponential" :eqs [(eq '(expt (+ 1 (/ 1 n)) n) (power-n 'n))]}
      {:label "Emmy's series of n log(1 + 1/n) in u = 1/n"
       :eqs [(eq '(* n (log (+ 1 (/ 1 n)))) (e/+ 1 (reduce e/+ (map #(e/* (nth (seq @log-series) %) (e/expt u %)) (range 1 4)))))]}
      {:label "composed with exp: the error"
       :eqs [(eq 'epsilon (e/* 'e (e/+ (e/* c1 u) (e/* c2 (e/square u)) (e/* c3 (e/cube u)))))]}
      {:label "the leading term solved for n starts Newton's steps; Euler's ω = 1/n"
       :eqs [(eq 'n_0 (:root (solve-linear-in (fn [n] (e/- (e/* 2 'epsilon n) 'e)))))
             (eq 'n_next (e/literal-number '(- n (/ (- (epsilon n) epsilon) ((D epsilon) n)))))
             (eq 'n (omega-inverse 'omega))]}]
     :cis
     [{:label "Emmy's derivative" :eqs [(eq '(D f x) ((e/D (fn [x] (e/* (e/exp (e/* (e/- i) x)) (cis x)))) 'x))]}
      {:label "so f is constant, f(0) = 1" :eqs [(eq '(exp (* i x)) (e/literal-number '(+ (cos x) (* i (sin x)))))]}
      {:label "solved back from the point" :eqs [(eq 'x (e/atan 's 'c))]}]
     :cut
     [{:label "cutting off solid angles of degrees d₁, d₂, d₃"
       :eqs (let [{:keys [V E F]} (truncated-counts {:V 'V :E 'E :F 'F} '[d1 d2 d3])]
              [(eq 'V' V) (eq 'E' E) (eq 'F' F)
               (eq '(+ (- V' E') F') (e/simplify (e/+ (e/- V E) F)))])}]}))
