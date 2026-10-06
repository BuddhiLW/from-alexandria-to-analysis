(ns alexandria.cauchy.analysis
  "Cauchy's rigour, 1821-1823: limits, continuity, convergence, the definite
   integral, as figures (Emmy functions) and graded checks.

     continuity   the epsilon-delta game on concrete functions: a delta for
                  each epsilon, closed-form where one exists (graded proved),
                  found by bisection otherwise (graded numeric)
     series       Cauchy's criterion on the geometric progression, the
                  harmonic series that fails it, the ratio test on 1/n!
     sum theorem  Cours d'analyse I.6.1 Theorem 1 and Abel's exception
                  sum (-1)^(k+1) sin(kx)/k: a jump and the Gibbs overshoot
     integral     Resume 1823, Lesson 21: S = sum (x_i - x_(i-1)) f(x_(i-1)),
                  closed-form sums for polynomials, quadrature otherwise;
                  Lesson 7's mean value theorem; Lesson 26's derivative of
                  the integral
     flat         exp(-1/x^2), the end of Lesson 38: every derivative at 0
                  vanishes, so its Taylor series sums to 0, not to it

   Reuse searched: Emmy's derivative D, simplify, bisection
   (emmy.numerical.roots.bisect) and quadrature (emmy.numerical.quadrature)
   do the work; alexandria.grade grades it. Emmy has no symbolic limit and
   no symbolic indefinite sum, so the closed forms of the sums are stated
   here and proved by their recurrence (F(n+1) - F(n) = term).

   Emmy simplifier gotcha: (- (expt x (+ n 1)) (* x (expt x n))) simplifies
   to 2 x^(n+1), not 0; the other order gives 0. The identities below are
   written so that they do not need that rewrite."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [alexandria.raster :as raster]
            [alexandria.raster.series :as rs]
            [hive-dsl.result :as r]
            [emmy.simplify.rules :as rules]))

;; ---------------------------------------------------------------------------
;; Grading helpers

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn proved
  "{:label :grade} of the symbolic difference diff (zero when the claim holds)."
  [label diff]
  {:label label :grade (variant (grade/grade :symbolic [diff]))})

(defn numeric
  "{:label :grade :source} of a numeric difference, within tolerance tol.
   Every number graded this way comes out of a raster kernel or solver
   (alexandria.raster, alexandria.raster.series), so :source is :raster."
  ([label diff] (numeric label diff grade/default-tolerance))
  ([label diff tol] {:label label :grade (variant (grade/grade :numeric [diff] tol)) :source raster/source}))

(defn decided
  "{:label :grade} of a finite claim decided exactly in rationals: proved
   when it holds, fails when it does not."
  [label holds?]
  {:label label :grade (if holds? :grade/proved :grade/fails)})

(defn- clamp01 [x] (e// (e/+ 1 (e/- (e/abs x) (e/abs (e/- x 1)))) 2))

;; ---------------------------------------------------------------------------
;; Continuity: the epsilon-delta game

(def games
  "The functions the game is played on. :delta is a closed form in a and
   eps where there is one; :worst the side of a where the increment first
   reaches eps (there it equals eps exactly at that delta)."
  {:linear {:f (fn [x] (e/+ (e/* 3 x) 1)) :label "f(x) = 3x + 1"
            :delta (fn [_a eps] (e// eps 3)) :worst :right}
   :square {:f (fn [x] (e/square x)) :label "f(x) = x^2, a > 0"
            :delta (fn [a eps] (e/- (e/sqrt (e/+ (e/square a) eps)) a)) :worst :right}
   :reciprocal {:f (fn [x] (e// 1 x)) :label "f(x) = 1/x, a > 0"
                :delta (fn [a eps] (e// (e/* eps a a) (e/+ 1 (e/* eps a)))) :worst :left}
   :sine {:f (fn [x] (e/sin x)) :label "f(x) = sin x"}})

(defn increment
  "f(a + h) - f(a): what an increment h of the variable makes of the function."
  [f a h]
  (e/- (f (e/+ a h)) (f a)))

(defn closed-form-delta-identity
  "For a game with a closed-form delta: the increment on the worst side, at
   that delta, minus eps. It is zero: the function's increment reaches the
   edge of the band exactly there (each of these functions increases away
   from a on its worst side)."
  [{:keys [f delta worst]}]
  (let [d (delta 'a 'eps)]
    (e/- (increment f 'a (if (= worst :left) (e/- d) d)) 'eps)))

(defn worst-increment
  "The largest |f(a + h) - f(a)| over |h| <= delta, sampled at 2n + 1
   points of [-delta, delta] inside one raster kernel
   (alexandria.raster.series/sup); f an Emmy function."
  ([f a delta] (worst-increment f a delta 400))
  ([f a delta n]
   (:value (rs/sup (fn [s a d] (e/abs (increment f a (e/* s d)))) -1 1 (inc (* 2 n)) [a delta]))))

(defn find-delta
  "The largest delta (up to delta-max) with every |f(a + h) - f(a)| <= eps
   for |h| <= delta: the root of worst-increment - eps by raster's Brent
   (alexandria.raster.series/root-of), each worst increment a raster
   kernel."
  ([f a eps] (find-delta f a eps 1.0))
  ([f a eps delta-max]
   (if (<= (worst-increment f a delta-max) eps)
     delta-max
     (:value (rs/root-of (fn [d] (- (worst-increment f a d) eps)) 1e-12 delta-max)))))

(defn continuity-graded
  "The game, graded: the closed-form deltas proved for every a and eps; the
   sine's delta found numerically at a = 1, eps = 1/10."
  []
  (let [sine (:f (games :sine))
        d (find-delta sine 1.0 0.1)]
    (conj (mapv (fn [k] (proved (str (get-in games [k :label]) ": delta = "
                                     (case k :linear "eps/3" :square "sqrt(a^2 + eps) - a"
                                           :reciprocal "eps a^2/(1 + eps a)")
                                     " reaches the band exactly at its edge")
                                (closed-form-delta-identity (games k))))
                [:linear :square :reciprocal])
          (numeric (str "f(x) = sin x at a = 1, eps = 1/10: delta = " (format "%.6f" d)
                        " found by bisection; the worst increment equals eps")
                   (- (worst-increment sine 1.0 d 400) 0.1) 1e-6))))

;; ---------------------------------------------------------------------------
;; Note III, Theorem 1: the root between two values of opposite sign
;;
;; Cauchy's proof, in his letters: h = X - x0; divide h into m parts and
;; form f(x0), f(x0 + h/m), ..., f(X); two consecutive terms of opposite
;; sign, f(x1) and f(X'), bound a root with X' - x1 = h/m; divide again,
;; x2 and X'' with X'' - x2 = h/m^2, and so on. The x's increase, the X's
;; decrease, their gap h/m^n tends to 0, so both tend to one limit a, and
;; f(a), approached by values of both signs, is 0. Scholie 1: the half-sum
;; of x_n and X^(n) is the root within half their difference, h/(2 m^n).

(defn ivt-example
  "The equation of the search: x^3 - 2x - 5 = 0 between 2 and 3 (Newton's
   example, which Lagrange took up in his Resolution des equations
   numeriques)."
  [x]
  (e/- (e/* x x x) (e/* 2 x) 5))

(defn tenths-search
  "Note III, Theorem 1, as a raster search: n rounds dividing [x0 X] into m
   parts (m = 10: tenths), the values f(x0 + k h/m) of each round sampled
   by f's raster kernel (alexandria.raster/sample), the first consecutive
   pair of contrary signs kept. The bounds stay exact rationals. One map
   per round: {:x x_k :X X^(k) :xs points :values f at them :j kept part}."
  [f x0 X m n]
  (loop [k 0 a x0 b X acc []]
    (let [h (/ (- b a) m)
          xs (mapv #(+ a (* % h)) (range (inc m)))
          ys (raster/sample f (mapv (fn [x] [(double x)]) xs))
          j (first (filter #(<= (* (nth ys %) (nth ys (inc %))) 0) (range m)))
          round {:x a :X b :xs xs :values ys :j j}]
      (if (or (= k n) (nil? j))
        (conj acc round)
        (recur (inc k) (nth xs j) (nth xs (inc j)) (conj acc round))))))

(defn ivt-graded
  "The search on x^3 - 2x - 5 between 2 and 3, graded: the bracket widths
   are Cauchy's h/m^n exactly; the half-sum is within his bound of the root
   raster's Brent finds; f(2) and f(3) of contrary signs proved exactly."
  []
  (let [rounds (tenths-search ivt-example 2 3 10 6)
        widths (mapv #(- (:X %) (:x %)) rounds)
        {:keys [x X]} (peek rounds)
        a (:value (raster/root ivt-example 2 3))]
    [(proved "f(2) = -1 and f(3) = 16: f(x0) and f(X) of contrary signs, so Theorem 1 applies"
             (e/+ (e/- (ivt-example 2) -1) (e/- (ivt-example 3) 16)))
     (decided "the brackets x_n, X^(n) have width h/m^n = 1/10^n exactly, n = 0..6"
              (= widths (mapv #(/ 1 (long (Math/pow 10 %))) (range 7))))
     (decided "the x_n increase, the X^(n) decrease, and each kept pair has contrary signs (values by f's raster kernel)"
              (and (apply <= (map :x rounds)) (apply >= (map :X rounds))
                   (every? (fn [{:keys [values j]}] (or (nil? j) (<= (* (nth values j) (nth values (inc j))) 0))) rounds)))
     (numeric (str "Scholie 1: the half-sum (x_6 + X^(6))/2 = " (format "%.7f" (double (/ (+ x X) 2)))
                   " is within h/(2 m^6) = 5e-7 of the root a = " (format "%.10f" a) " (raster's Brent)")
              (max 0.0 (- (Math/abs (- (double (/ (+ x X) 2)) a)) 5e-7)) 1e-12)]))

;; ---------------------------------------------------------------------------
;; Resume 1823, Lesson 3: the derivative, Delta y / Delta x with Delta x = i

(defn difference-quotient
  "Cauchy's rapport aux differences (f(x + i) - f(x))/i."
  [f x i]
  (e// (e/- (f (e/+ x i)) (f x)) i))

(defn power-quotient-expansion
  "His expansion of the quotient for f(x) = x^5, a function of i:
   5x^4 + 10x^3 i + 10x^2 i^2 + 5x i^3 + i^4."
  [x i]
  (e/+ (e/* 5 (e/expt x 4)) (e/* 10 (e/expt x 3) i) (e/* 10 (e/expt x 2) (e/expt i 2))
       (e/* 5 x (e/expt i 3)) (e/expt i 4)))

(defn derivative-graded
  "Lesson 3, graded: his x^m quotient written out (m = 5), his sin x
   quotient as a product (proved with Emmy's product-to-sum rule switched
   on, emmy.simplify.rules/*trig-product-to-sum-simplify?*); Emmy's
   derivative agrees with the value at i = 0; the quotients of sin at x = 1
   for i = 1/10 .. 1/10^6 by a raster kernel, tending to cos 1."
  []
  (let [qs (mapv (fn [k] (raster/value (fn [i] (difference-quotient e/sin 1 i)) (Math/pow 10.0 (- k)))) (range 1 7))
        c1 (raster/value e/cos 1)]
    [(proved "((x + i)^m - x^m)/i = m x^(m-1) + m(m-1)/2 x^(m-2) i + ... + i^(m-1), m = 5"
             (e/- (difference-quotient #(e/expt % 5) 'x 'i) (power-quotient-expansion 'x 'i)))
     (proved "every term after the first carries i, so the limit is its value at i = 0, m x^(m-1) = 5x^4: Emmy's D(x^5)"
             (e/- (power-quotient-expansion 'x 0) ((e/D #(e/expt % 5)) 'x)))
     (binding [rules/*trig-product-to-sum-simplify?* true]
       (proved "for y = sin x: sin(x + i) - sin x = 2 sin(i/2) cos(x + i/2), so Delta y/Delta x = [sin(i/2)/(i/2)] cos(x + i/2), whose limit is cos x"
               (e/- (e/- (e/sin (e/+ 'u 'j)) (e/sin (e/- 'u 'j))) (e/* 2 (e/sin 'j) (e/cos 'u)))))
     (numeric (str "the quotients of sin at x = 1, i = 1/10 .. 1/10^6 (raster kernel): "
                   (apply str (interpose ", " (map #(format "%.7f" %) qs))) " -> cos 1 = " (format "%.7f" c1))
              (- (peek qs) c1) 1e-6)
     (decided "the error shrinks with i: each quotient is nearer cos 1 than the one before"
              (apply > (map #(Math/abs (- % c1)) qs)))]))

;; ---------------------------------------------------------------------------
;; Series: Cauchy's criterion, chapter VI

(defn geometric-partial
  "s_n = 1 + x + ... + x^(n-1)."
  [x n]
  (e// (e/- 1 (e/expt x n)) (e/- 1 x)))

(defn harmonic-block
  "1/(n+1) + ... + 1/(2n), exactly."
  [n]
  (reduce + (map #(/ 1 %) (range (inc n) (inc (* 2 n))))))

(defn factorial [n] (reduce *' 1 (range 1 (inc n))))

(defn e-partial
  "1 + 1/1! + ... + 1/(n-1)!, exactly (n terms)."
  [n]
  (reduce + (map #(/ 1 (factorial %)) (range n))))

(defn series-graded
  "Cauchy's criterion on his examples. The partial sums are exact
   rationals; the one floating number, e, is exp(1) by a raster kernel."
  []
  (let [s (fn [n] (geometric-partial 'x n))]
    [(proved "geometric progression: s(n+p) - s(n) = x^n (1 - x^p)/(1 - x), as small as we like for every p once n is large (|x| < 1)"
             (e/- (e/- (s (e/+ 'n 'p)) (s 'n))
                  (e// (e/* (e/expt 'x 'n) (e/- 1 (e/expt 'x 'p))) (e/- 1 'x))))
     (proved "geometric progression: the remainder 1/(1 - x) - s(n) = x^n/(1 - x)"
             (e/- (e/- (e// 1 (e/- 1 'x)) (s 'n)) (e// (e/expt 'x 'n) (e/- 1 'x))))
     (decided "harmonic series: 1/(n+1) + ... + 1/(2n) >= 1/2 for n = 1..40, so the criterion fails although 1/n -> 0"
              (every? #(>= (harmonic-block %) 1/2) (range 1 41)))
     (proved "ratio test on 1/n!: u(n+1)/u(n) = 1/(n+1), whose limit 0 is below 1"
             (e/- (e// (e// 1 (e/factorial 7)) (e// 1 (e/factorial 6))) (e// 1 7)))
     (decided "1 + 1/1! + ... + 1/9!: every later partial sum stays inside the tail band of width 1/(9! 9), checked exactly to 30 terms"
              (let [s10 (e-partial 10)]
                (every? #(< 0 (- (e-partial %) s10) (/ 1 (* (factorial 9) 9))) (range 11 31))))
     (numeric "the partial sums of 1/n! converge to e (exp 1 by a raster kernel)"
              (- (double (e-partial 25)) (raster/value e/exp 1)) 1e-12)]))

(defn e-partials
  "[n s_n] for n = 1..k, as doubles, for the scene."
  [k]
  (mapv (fn [n] [n (double (e-partial n))]) (range 1 (inc k))))

(defn expt-rational
  "q^n exactly, q a rational, n a natural number."
  [q n]
  (reduce *' 1 (repeat n q)))

(defn root-test-term
  "u_n = n^2/2^n, the series of the root-test demonstration: (u_n)^(1/n)
   tends to k = 1/2."
  [n]
  (/ (* n n) (expt-rational 2 n)))

(defn root-test-graded
  "Chapter VI, section 2, Theorem 1, Cauchy's demonstration followed on
   u_n = n^2/2^n: k = 1/2 < 1; choose U between k and 1 (U = 3/4); past
   some n, (u_n)^(1/n) < U, that is u_n < U^n, so the terms end below those
   of the geometric progression 1, U, U^2, ..., which converges. The roots
   (u_n)^(1/n) are a raster kernel of the Emmy expression; the comparison
   u_n < U^n is decided exactly in rationals; the sum is a raster loop.
   2^-n is written exp(-n c) with c = log 2 passed as a kernel parameter
   (itself a raster value): raster lowers only literal exponents, and a
   constant (log 2) inside the kernel does not lower."
  []
  (let [U 3/4
        ln2 (raster/value e/log 2)
        below? (fn [n] (< (root-test-term n) (expt-rational U n)))
        roots (mapv (fn [n] (raster/value (fn [n c] (e/exp (e// (e/- (e/* 2 (e/log n)) (e/* n c)) n))) n ln2))
                    [10 100 1000 10000])
        N (first (filter (fn [n] (every? below? (range n 200))) (range 1 200)))
        total (:value (rs/sum (fn [k c] (e/* k k (e/exp (e/- (e/* k c))))) 1 200 [ln2]))]
    [(numeric (str "(u_n)^(1/n) at n = 10, 100, 1000, 10000 (raster kernel): "
                   (apply str (interpose ", " (map #(format "%.5f" %) roots))) ": its limit k = 1/2 < 1")
              (- (peek roots) 0.5) 1e-3)
     (decided (str "his U between k and 1, U = 3/4: from n = " N " on, u_n < U^n exactly (checked to n = 199): the terms end below the progression 1, U, U^2, ...")
              (and (some? N) (every? below? (range N 200))))
     (proved "the progression converges because U < 1: 1 + U + ... + U^(n-1) = (1 - U^n)/(1 - U) < 1/(1 - U) = 4"
             (e/- (geometric-partial 3/4 12) (e// (e/- 1 (e/expt 3/4 12)) 1/4)))
     (numeric "so the series converges; its sum, 200 terms in a raster loop, is 6 = sum n^2 x^n at x = 1/2, x(1 + x)/(1 - x)^3"
              (- total 6.0) 1e-9)
     (proved "the closed form x(1 + x)/(1 - x)^3 at x = 1/2 is 6"
             (e/- (e// (e/* 1/2 (e/+ 1 1/2)) (e/cube (e/- 1 1/2))) 6))]))

;; ---------------------------------------------------------------------------
;; The sum theorem and Abel's exception

(def abel-terms
  "How many terms the moving figure carries."
  48)

(defn abel-term [k x] (e// (e/* (if (odd? k) 1 -1) (e/sin (e/* k x))) k))

(defn abel-partial
  "sin x - sin 2x/2 + sin 3x/3 - ... to n terms."
  [n x]
  (reduce e/+ 0 (map #(abel-term % x) (range 1 (inc n)))))

(defn abel
  "The figure: S at a continuous number of terms n (term k fades in as n
   passes from k - 1 to k), state [x] -> [x S]."
  [n]
  (fn [[x]]
    [x (reduce e/+ 0 (map (fn [k] (e/* (clamp01 (e/- n (dec k))) (abel-term k x)))
                          (range 1 (inc abel-terms))))]))

(defn sawtooth
  "The sum of Abel's series: x/2 on (-pi, pi), 0 at odd multiples of pi,
   periodic."
  [x]
  (let [y (- x (* 2 Math/PI (Math/floor (/ (+ x Math/PI) (* 2 Math/PI)))))]
    (if (< (Math/abs (- (Math/abs y) Math/PI)) 1e-12) 0.0 (/ y 2))))

(defn abel-sum
  "The partial sum of n terms of Abel's series at x, a number: one raster
   loop kernel over the Emmy term sin(kx)/k, signs alternating."
  ^double [n x]
  (:value (rs/sum (fn [k x] (e// (e/sin (e/* k x)) k)) 1 n [x] {:alternating? true})))

(defn gibbs-peak
  "max of S_n on (0, pi), over the 3999 inner points of a 4000-part grid:
   one raster kernel, the grid and the n terms as nested loops."
  [n]
  (:value (rs/sup-of-sum (fn [k x] (e// (e/sin (e/* k x)) k)) (fn [_] 0)
                         (/ Math/PI 4000) (* Math/PI (/ 3999 4000.0)) 3999
                         {:alternating? true :to n})))

(defn error-near-jump
  "|S_n(x) - x/2| at x = pi - 1/n: one point for each n, always as far
   from the sum. It tends to pi/2 - Si(1), so sup |S_n - x/2| does not tend
   to 0."
  [n]
  (let [x (- Math/PI (/ 1.0 n))] (Math/abs (- (abel-sum n x) (/ x 2)))))

(defn- sinc-integral
  "The integral of sin t / t from 0 to b by raster's Gauss-Kronrod; the
   integrand never meets t = 0 (Kronrod nodes are interior)."
  [b]
  (:value (raster/integral (fn [t] (e// (e/sin t) t)) 0 b)))

(def si-1
  "Si(1), the integral of sin t / t from 0 to 1 (raster quadrature)."
  (sinc-integral 1))

(def si-pi
  "Si(pi), the integral of sin t / t from 0 to pi (raster quadrature): the
   height the peaks of S_n tend to."
  (sinc-integral Math/PI))

(defn sum-theorem-graded
  "Abel's exception, graded. The numbers: raster quadrature (Fourier's
   coefficient, Si), raster loop kernels (the partial sums and their peak)."
  []
  (vec
   (concat
    (for [k (range 1 7)]
      (proved (str "term " k " is continuous and vanishes at x = pi: sin(" k " pi) = 0")
              (e/sin (e/* k 'pi))))
    [(proved "so every partial sum, and the sum, is 0 at x = pi" (abel-partial 12 'pi))
     (numeric "Fourier's coefficient: (1/pi) times the integral of (x/2) sin 3x over (-pi, pi) is 1/3"
              (- (/ (:value (raster/integral (fn [x] (e/* (e// x 2) (e/sin (e/* 3 x)))) (- Math/PI) Math/PI)) Math/PI) 1/3) 1e-8)
     (numeric "left of pi, at x = pi - 1/10, 100000 terms give x/2 = 1.5208...: near pi the sum is near pi/2, at pi it is 0"
              (- (abel-sum 100000 (- Math/PI 0.1)) (/ (- Math/PI 0.1) 2)) 1e-3)
     (numeric "the Gibbs overshoot: the peak of S_n left of pi tends to Si(pi) = 1.8519..., not to pi/2 = 1.5708 (n = 2000)"
              (- (gibbs-peak 2000) si-pi) 1e-3)
     (numeric "not uniform: at x = pi - 1/n the error |S_n - x/2| stays at pi/2 - Si(1) = 0.6247... for every n (n = 2000)"
              (- (error-near-jump 2000) (- (/ Math/PI 2) si-1)) 1e-3)])))

;; ---------------------------------------------------------------------------
;; The definite integral, Resume 1823

(defn left-sum
  "Cauchy's S = (x1 - x0) f(x0) + ... + (X - x_(n-1)) f(x_(n-1)) for the
   points xs = [x0 ... X]."
  [f xs]
  (reduce e/+ 0 (map (fn [a b] (e/* (e/- b a) (f a))) xs (rest xs))))

(defn equal-points
  "x0, x0 + i, ..., X: n equal elements of X - x0."
  [x0 X n]
  (mapv #(e/+ x0 (e/* % (e// (e/- X x0) n))) (range (inc n))))

(defn sum-i "0 + 1 + ... + (n - 1)." [n] (e// (e/* n (e/- n 1)) 2))

(defn sum-i2 "0 + 1 + 4 + ... + (n - 1)^2." [n] (e// (e/* (e/- n 1) n (e/- (e/* 2 n) 1)) 6))

(defn square-sum-closed
  "S for f(x) = x^2 on [x0, X] in n equal elements of width i = (X - x0)/n,
   in closed form: i (n x0^2 + 2 x0 i (0 + ... + (n-1)) + i^2 (0 + ... + (n-1)^2))."
  [x0 X n]
  (let [i (e// (e/- X x0) n)]
    (e/* i (e/+ (e/* n (e/square x0)) (e/* 2 x0 i (sum-i n)) (e/* (e/square i) (sum-i2 n))))))

(defn geometric-power-sum
  "Cauchy's formula (12) of Lesson 22: with x_k = x0 (1 + alpha)^k up to
   X = x0 (1 + alpha)^n, S for f(x) = x^a is
   alpha (X^(a+1) - x0^(a+1)) / ((1 + alpha)^(a+1) - 1)."
  [x0 alpha a n]
  (let [X (e/* x0 (e/expt (e/+ 1 alpha) n))]
    (e// (e/* alpha (e/- (e/expt X (+ a 1)) (e/expt x0 (+ a 1))))
         (e/- (e/expt (e/+ 1 alpha) (+ a 1)) 1))))

(defn integral-graded
  "The sums converge to the difference of the antiderivative. The numeric
   sums are raster loop kernels (alexandria.raster.series/left-sum), the
   quadrature raster's Gauss-Kronrod."
  []
  (let [n 'n x0 'x_0 X 'X]
    [(proved "0 + 1 + ... + (n-1) = n(n-1)/2: the closed form satisfies its recurrence"
             (e/- (e/- (sum-i (e/+ n 1)) (sum-i n)) n))
     (proved "0 + 1 + ... + (n-1)^2 = (n-1)n(2n-1)/6: the closed form satisfies its recurrence"
             (e/- (e/- (sum-i2 (e/+ n 1)) (sum-i2 n)) (e/square n)))
     (proved "the closed form agrees with S added term by term, for n = 7 elements"
             (e/- (left-sum e/square (equal-points x0 X 7)) (square-sum-closed x0 X 7)))
     (proved "f(x) = x^2: S = (X^3 - x0^3)/3 - (X - x0)^2 (X + x0)/(2n) + (X - x0)^3/(6n^2), so S -> (X^3 - x0^3)/3"
             (e/- (square-sum-closed x0 X n)
                  (e/+ (e// (e/- (e/cube X) (e/cube x0)) 3)
                       (e/- (e// (e/* (e/square (e/- X x0)) (e/+ X x0)) (e/* 2 n)))
                       (e// (e/cube (e/- X x0)) (e/* 6 (e/square n))))))
     (proved "Cauchy's geometric division, a = 2: alpha/((1 + alpha)^3 - 1) = 1/(3 + 3 alpha + alpha^2), which tends to 1/3"
             (e/- (e// 'alpha (e/- (e/cube (e/+ 1 'alpha)) 1)) (e// 1 (e/+ 3 (e/* 3 'alpha) (e/square 'alpha)))))
     (proved "his formula (12) for a = 2 agrees with S added term by term, for n = 5"
             (e/- (left-sum e/square (mapv #(e/* x0 (e/expt (e/+ 1 'alpha) %)) (range 6)))
                  (geometric-power-sum x0 'alpha 2 5)))
     (proved "Lesson 26: the integral from x0 to x of t^2, as a function of x, has derivative x^2"
             (e/- ((e/D (fn [x] (e// (e/- (e/cube x) (e/cube x0)) 3))) 'x) (e/square 'x)))
     (proved "Lesson 7, mean value: (f(x0 + h) - f(x0))/h = f'(x0 + theta h) with theta = 1/2 for f(x) = x^2"
             (e/- (e// (increment e/square x0 'h) 'h) ((e/D e/square) (e/+ x0 (e/* 1/2 'h)))))
     (proved "for f(x) = x^3 at x0 = 0 the two sides differ by h^2 (1 - 3 theta^2), so theta = 1/sqrt 3"
             (e/- (e/- (e// (increment e/cube 0 'h) 'h) ((e/D e/cube) (e/* 'theta 'h)))
                  (e/* (e/square 'h) (e/- 1 (e/* 3 (e/square 'theta))))))
     (numeric "f = sin on [0, pi], 2000 equal elements: S is within 1e-5 of cos 0 - cos pi = 2"
              (- (:value (rs/left-sum e/sin 0 Math/PI 2000)) 2.0) 1e-5)
     (numeric "f = exp on [0, 1], 4000 equal elements: S is within 1e-3 of e - 1"
              (- (:value (rs/left-sum e/exp 0 1 4000)) (- Math/E 1)) 1e-3)
     (numeric "raster's quadrature of sin over [0, pi] is 2"
              (- (:value (raster/integral e/sin 0 Math/PI)) 2.0) 1e-10)]))

(defn rectangles
  "The rectangles of S for f on [x0 X] in n equal elements:
   [[x_(i-1) x_i f(x_(i-1))] ...], as doubles."
  [f x0 X n]
  (let [xs (mapv double (equal-points x0 X n))]
    (mapv (fn [a b] [a b (double (f a))]) xs (rest xs))))

(defn cauchy-integrand
  "The integrand of the refinement and fundamental-theorem checks:
   f(x) = 1 + sin x, on [x0 X] = [0 3], |f'| <= 1."
  [x]
  (e/+ 1 (e/sin x)))

(defn refinement-graded
  "Lesson 21, equations (2) to (7), on f(x) = 1 + sin x over [0 3]. Cauchy
   subdivides each element: the new S differs from the old by
   +-e0 (x1 - x0) +- ... +- e(n-1) (X - x(n-1)), each e_k the change of f
   inside the k-th element, so |S' - S| <= (X - x0) times the largest
   change of f over an element, here at most i = (X - x0)/n since |f'| <= 1.
   Every S is a raster loop kernel (alexandria.raster.series/left-sum); the
   limit is raster's Gauss-Kronrod."
  []
  (let [f cauchy-integrand x0 0 X 3
        S (fn [n] (:value (rs/left-sum f x0 X n)))
        I (:value (raster/integral f x0 X))
        ns [4 8 16 32 64 128 256]
        pairs (map (fn [n] [n (Math/abs (- (S (* 2 n)) (S n))) (/ (- X x0) (double n))]) ns)]
    [(proved "equation (4) for one element: (X - x0) f(x0 + theta (X - x0)) at theta = 0 is equation (3), (X - x0) f(x0)"
             (e/- (e/* (e/- 'X 'x_0) ((fn [t] (cauchy-integrand (e/+ 'x_0 (e/* t (e/- 'X 'x_0))))) 0))
                  (e/* (e/- 'X 'x_0) (cauchy-integrand 'x_0))))
     (proved "equation (7): halving every element changes the sum on [x_k, x_k + 2j] by j (f(x_k + j) - f(x_k)), an element times a change of f"
             (e/- (e/- (e/+ (e/* 'j (cauchy-integrand 'x_k)) (e/* 'j (cauchy-integrand (e/+ 'x_k 'j))))
                       (e/* 2 'j (cauchy-integrand 'x_k)))
                  (e/* 'j (e/- (cauchy-integrand (e/+ 'x_k 'j)) (cauchy-integrand 'x_k)))))
     (decided (str "|S(2n) - S(n)| <= (X - x0) * (X - x0)/n for n = 4 .. 256 (raster sums): "
                   (apply str (interpose ", " (map (fn [[n d]] (str n ": " (format "%.2e" d))) pairs))))
              (every? (fn [[_ d bound]] (<= d (* (- X x0) bound))) pairs))
     (numeric (str "two different modes, 1000 and 1001 equal elements, give sums within 1e-2 of each other: the mode of division ends by having no sensible influence")
              (- (S 1000) (S 1001)) 1e-2)
     (numeric (str "the limit: S(4096) is within 1e-3 of raster's quadrature, the integral = " (format "%.8f" I))
              (- (S 4096) I) 1e-3)]))

(defn integral-function
  "Lesson 26, equation (1): the integral from x0 = 0 to x of 1 + sin t, by
   raster's Gauss-Kronrod."
  [x]
  (:value (raster/integral cauchy-integrand 0 x)))

(defn fundamental-graded
  "Lesson 26, equations (3) and (4): F(x + alpha) - F(x) = alpha f(x + theta
   alpha), theta between 0 and 1 (found by raster's Brent), and the quotient
   tends to f(x) as alpha decreases. F by raster quadrature."
  []
  (let [x 1.0
        fx (raster/value cauchy-integrand x)
        alphas [0.5 0.1 0.01 0.001]
        rows (mapv (fn [alpha]
                     (let [q (/ (- (integral-function (+ x alpha)) (integral-function x)) alpha)
                           theta (/ (- (:value (raster/root (fn [t] (e/- (cauchy-integrand t) q)) x (+ x alpha))) x) alpha)]
                       {:alpha alpha :quotient q :theta theta}))
                   alphas)]
    [(proved "equation (3) for f(x) = x^2, F(x) = (x^3 - x0^3)/3: (F(x + alpha) - F(x))/alpha = x^2 + x alpha + alpha^2/3; every term after x^2 carries alpha"
             (e/- (e// (e/- (e/cube (e/+ 'x 'alpha)) (e/cube 'x)) (e/* 3 'alpha))
                  (e/+ (e/square 'x) (e/* 'x 'alpha) (e// (e/square 'alpha) 3))))
     (decided (str "equation (3) for f(x) = 1 + sin x at x = 1: theta lies between 0 and 1 for alpha = 0.5, 0.1, 0.01, 0.001 (raster quadrature and Brent): "
                   (apply str (interpose ", " (map #(format "%.4f" (:theta %)) rows))))
              (every? #(< 0 (:theta %) 1) rows))
     (numeric (str "equation (4): the quotient " (format "%.6f" (:quotient (peek rows))) " at alpha = 0.001 tends to f(1) = 1 + sin 1 = " (format "%.6f" fx))
              (- (:quotient (peek rows)) fx) 1e-3)
     (proved "and passing to the limit for x^2: Emmy's D of (x^3 - x0^3)/3 is x^2, the integrand"
             (e/- ((e/D (fn [x] (e// (e/- (e/cube x) (e/cube 'x_0)) 3))) 'x) (e/square 'x)))]))

;; ---------------------------------------------------------------------------
;; The flat function, Lesson 38

(defn flat [x] (e/exp (e// -1 (e/square x))))

(defn flat-derivative-factor
  "P_k with f^(k)(x) = P_k(1/x) exp(-1/x^2), as a function of t: Emmy's
   k-th derivative of f divided by f, at x = 1/t."
  [k]
  (fn [t] (e// (((e/expt e/D k) flat) (e// 1 t)) (flat (e// 1 t)))))

(defn flat-graded
  "Every derivative of exp(-1/x^2) vanishes at 0. The two small numbers are
   Emmy expressions evaluated by raster kernels (alexandria.raster/value)."
  []
  (vec
   (concat
    [(proved "f'(x) = (2/x^3) exp(-1/x^2): P1(t) = 2t^3"
             (e/- ((flat-derivative-factor 1) 't) (e/* 2 (e/cube 't))))]
    (for [k (range 1 5)]
      (proved (str "P" (inc k) "(t) = 2t^3 P" k "(t) - t^2 P" k "'(t): every derivative is a polynomial in 1/x times exp(-1/x^2)")
              (let [pk (flat-derivative-factor k)]
                (e/- ((flat-derivative-factor (inc k)) 't)
                     (e/- (e/* 2 (e/cube 't) (pk 't)) (e/* (e/square 't) ((e/D pk) 't)))))))
    [(numeric "f'(0) from the definition: (f(h) - f(0))/h = exp(-1/h^2)/h, already below 1e-100 at h = 1/20"
              (raster/value (fn [h] (e// (flat h) h)) 0.05) 1e-100)
     (numeric "t^9 exp(-t^2) -> 0, faster than any power grows: so P(1/x) exp(-1/x^2) -> 0 as x -> 0 (t = 30)"
              (raster/value (fn [t] (e/* (e/expt t 9) (e/exp (e/- (e/square t))))) 30) 1e-100)
     (proved "yet f is not 0: f(1) = 1/e = 0.3679..., f(1) - 1/e = 0 to machine precision" (e/- (flat 1) (e/exp -1)))])))

;; ---------------------------------------------------------------------------
;; Everything, by name

(def figures
  "Every moving figure, by name: {:f :params :state :opts}."
  {:abel {:f abel :params [1] :state [0] :opts {:simplify? false}}})

(defn graded
  "Every graded check, by part: {part [{:label :grade}]}, in Cauchy's
   order (Cours: continuity, the root search of Note III, series, the root
   test, the sum theorem; Resume: the derivative, the integral and its
   refinement, the fundamental theorem, the flat function)."
  []
  {:continuity (continuity-graded)
   :ivt (ivt-graded)
   :series (series-graded)
   :root-test (root-test-graded)
   :sum-theorem (sum-theorem-graded)
   :derivative (derivative-graded)
   :integral (integral-graded)
   :refinement (refinement-graded)
   :fundamental (fundamental-graded)
   :flat (flat-graded)})

(def proofs-resource "alexandria/cauchy/analysis.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
