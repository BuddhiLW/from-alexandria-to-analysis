(ns alexandria.riemann.integral
  "Riemann, Ueber die Darstellbarkeit einer Function durch eine
   trigonometrische Reihe (Habilitationsschrift 1854, printed 1868),
   sections 4 to 6: the definite integral as the limit of tagged sums, the
   oscillation criterion, and his function that jumps on a dense set and is
   still integrable.

     sums        Riemann's S = sum delta_i f(x_{i-1} + eps_i delta_i), the
                 upper and lower sums, the oscillation sum sum delta_i D_i
     bracket     Riemann's (x): the excess of x over the nearest integer,
                 0 half way between two integers
     pathology   f(x) = sum (nx)/n^2: its partial sums S_N exactly, in
                 rationals; their one-sided limits, jumps, and exact upper
                 and lower sums (S_N rises with slope H_N between jumps)
     figures     Emmy functions for media: the graph of S_N with terms
                 faded in one by one, (x) written atan(tan(pi x))/pi
     identities  graded by alexandria.grade

   Reuse searched (carto, Emmy, desargues, raster): Emmy has quadrature
   (emmy.numerical.quadrature, used by alexandria.vocab segment-area) but no
   tagged Riemann sums, no upper/lower sums and no exact sup/inf of a
   piecewise-linear function; desargues has none either. They are written
   here, over Clojure rationals so Riemann's numbers stay exact."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]
            [alexandria.raster.series :as rs]))

;; ---------------------------------------------------------------------------
;; Tagged sums (section 4)

(defn partition-points
  "a = x_0 < x_1 < ... < x_n = b, n equal intervals, exact when a, b are."
  [a b n]
  (mapv #(+ a (* (- b a) (/ % n))) (range (inc n))))

(defn riemann-sum
  "Riemann's S = delta_1 f(a + eps_1 delta_1) + ... + delta_n f(x_{n-1} +
   eps_n delta_n) over the points xs, eps a seq of proper fractions (one per
   interval), f any function of a number."
  [f xs epss]
  (reduce + (map (fn [x0 x1 eps] (let [d (- x1 x0)] (* d (f (+ x0 (* eps d))))))
                 xs (rest xs) epss)))

(defn darboux-sums
  "{:upper :lower :oscillation} over the points xs, given (sup-inf x0 x1) ->
   [sup inf] of f on [x0 x1]. oscillation is Riemann's delta_1 D_1 + ... +
   delta_n D_n, D_i the greatest oscillation on the i-th interval."
  [sup-inf xs]
  (let [cells (map (fn [x0 x1] (let [[s i] (sup-inf x0 x1)] [(- x1 x0) s i])) xs (rest xs))
        upper (reduce + (map (fn [[d s _]] (* d s)) cells))
        lower (reduce + (map (fn [[d _ i]] (* d i)) cells))]
    {:upper upper :lower lower :oscillation (- upper lower)}))

(defn monotone-sup-inf
  "sup-inf of an increasing f: [f(x1) f(x0)]."
  [f]
  (fn [x0 x1] [(f x1) (f x0)]))

;; ---------------------------------------------------------------------------
;; Riemann's (x) and his function (section 6)

(defn- floor*
  "The greatest integer <= x, exact for rationals."
  [x]
  (if (ratio? x)
    (let [q (quot (numerator x) (denominator x))]
      (if (and (neg? x) (not= x q)) (dec q) q))
    (long (Math/floor (double x)))))

(defn bracket
  "Riemann's (x): the excess of x over the nearest integer; where x lies
   half way between two integers, the mean of 1/2 and -1/2, that is 0.
   Exact for rationals."
  [x]
  (let [r (- x (floor* x))]
    (cond (< r 1/2) r
          (> r 1/2) (- r 1)
          :else 0)))

(defn- half-integer? [y] (= 1/2 (- y (floor* y))))

(defn bracket-left
  "(y - 0), the limit of (t) as t rises to y."
  [y]
  (if (half-integer? y) 1/2 (bracket y)))

(defn bracket-right
  "(y + 0), the limit of (t) as t falls to y."
  [y]
  (if (half-integer? y) -1/2 (bracket y)))

(defn- sum-terms [g n-terms x] (reduce + (map (fn [k] (/ (g (* k x)) (* k k))) (range 1 (inc n-terms)))))

(defn partial-sum
  "S_N(x) = (x)/1 + (2x)/4 + ... + (Nx)/N^2, exact."
  [n-terms x]
  (sum-terms bracket n-terms x))

(defn partial-sum-left "S_N(x - 0)." [n-terms x] (sum-terms bracket-left n-terms x))
(defn partial-sum-right "S_N(x + 0)." [n-terms x] (sum-terms bracket-right n-terms x))

(defn jump
  "S_N(x + 0) - S_N(x - 0)."
  [n-terms x]
  (- (partial-sum-right n-terms x) (partial-sum-left n-terms x)))

(defn harmonic "H_N = 1 + 1/2 + ... + 1/N." [n] (reduce + (map #(/ 1 %) (range 1 (inc n)))))

(defn breakpoints
  "The points of the open interval (a b) where S_N jumps: x = (2m+1)/(2k),
   k <= N, sorted."
  [n-terms a b]
  (->> (for [k (range 1 (inc n-terms))
             m (range (floor* (- (* k a) 1)) (inc (floor* (* k b))))
             :let [x (/ (+ (* 2 m) 1) (* 2 k))]
             :when (< a x b)]
         x)
       distinct sort vec))

(defn partial-sum-sup-inf
  "[sup inf] of S_N on [x0 x1], exact. Between jumps S_N rises with slope
   H_N, so the extremes are among the values and one-sided limits at x0, x1
   and at the jumps inside."
  [n-terms]
  (fn [x0 x1]
    (let [inside (breakpoints n-terms x0 x1)
          vals (concat [(partial-sum n-terms x0) (partial-sum-right n-terms x0)
                        (partial-sum n-terms x1) (partial-sum-left n-terms x1)]
                       (mapcat (fn [x] [(partial-sum-left n-terms x) (partial-sum n-terms x)
                                        (partial-sum-right n-terms x)])
                               inside))]
      [(apply max vals) (apply min vals)])))

(defn sums-of-partial
  "{:n :upper :lower :oscillation} of S_N on [0 1] with n equal intervals."
  [n-terms n]
  (assoc (darboux-sums (partial-sum-sup-inf n-terms) (partition-points 0 1 n)) :n n))

(defn odd-denominator-jump
  "The jump of the whole series at x = p/(2n), p odd and prime to n, as
   Riemann states it: -(1/n^2)(1 + 1/9 + 1/25 + ...) = -pi^2/(8 n^2); here
   with the first `odd-terms` terms, an exact rational."
  [n odd-terms]
  (- (/ (reduce + (map #(/ 1 (* % %)) (take odd-terms (iterate #(+ % 2) 1)))) (* n n))))

(defn big-jumps
  "The points p/(2n) of (0 1) where the whole series jumps by at least sigma
   (pi^2/(8 n^2) >= sigma): finitely many, as Riemann says."
  [sigma]
  (let [n-max (long (Math/floor (Math/sqrt (/ (* Math/PI Math/PI) (* 8 sigma)))))]
    (->> (for [n (range 1 (inc n-max)) p (range 1 (* 2 n) 2)
               :when (= 1 (.gcd (biginteger p) (biginteger n)))]
           (/ p (* 2 n)))
         sort vec)))

;; ---------------------------------------------------------------------------
;; Figures (Emmy functions of one point, for media)

(defn- clamp01 [x] (e// (e/+ 1 (e/- (e/abs x) (e/abs (e/- x 1)))) 2))

(def max-terms "Terms a graph figure carries." 12)

(defn graph
  "The point of the graph of S_N over state [x]; N may be fractional, term k
   fading in as N passes from k - 1 to k. (x) is written atan(tan(pi x))/pi."
  [n]
  (fn [[x]]
    [x (reduce e/+ (for [k (range 1 (inc max-terms))]
                     (e/* (clamp01 (e/- n (dec k)))
                          (e// (e/atan (e/tan (e/* Math/PI k x))) (e/* Math/PI k k)))))]))

(def figures
  "Every moving figure: {:f :params :state :opts}."
  {:graph {:f graph :params [1] :state [0.1] :opts {:simplify? false}}})

;; ---------------------------------------------------------------------------
;; What the sections say, graded

(def x-squared-identities
  "f = x^2 on [0 1], n equal intervals: the closed forms of the sums, and
   the shape of Riemann's argument."
  (let [n 'n
        lower (e// (e/* (e/- n 1) (e/- (e/* 2 n) 1)) (e/* 6 n n))
        upper (e// (e/* (e/+ n 1) (e/+ (e/* 2 n) 1)) (e/* 6 n n))]
    [["x^2 on [0 1]: upper - lower = sum delta_i D_i = 1/n" (e/- (e/- upper lower) (e// 1 n))]
     ["x^2 on [0 1]: upper + lower = 2/3 + 1/(3 n^2), so both close on 1/3"
      (e/- (e/+ upper lower) (e/+ 2/3 (e// 1 (e/* 3 n n))))]
     ["an increasing f on n equal parts: sum delta_i D_i = (f(b) - f(a)) (b - a)/n"
      (let [fa 'fa fb 'fb a 'a b 'b]
        (e/- (e/* (e// (e/- b a) n) (e/- fb fa)) (e// (e/* (e/- fb fa) (e/- b a)) n)))]]))

(def jump-identities
  "Riemann's jump at x = p/(2n)."
  [["f(x + 0) - f(x) = -(1/(2 n^2))(1 + 1/9 + 1/25 + ...) = -pi^2/(16 n^2)"
    (e/- (e/* (e// -1 (e/* 2 'n 'n)) (e// (e/square 'pi) 8)) (e// (e/- (e/square 'pi)) (e/* 16 'n 'n)))]
   ["the jump f(x + 0) - f(x - 0) is twice it, -pi^2/(8 n^2)"
    (e/- (e/* 2 (e// (e/- (e/square 'pi)) (e/* 16 'n 'n))) (e// (e/- (e/square 'pi)) (e/* 8 'n 'n)))]])

(defn- variant
  ([kind diffs] (variant kind diffs grade/default-tolerance))
  ([kind diffs tol]
   (let [res (grade/grade kind diffs tol)]
     (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))))

(defn exact-checks
  "Riemann's statements decided exactly in rationals: [{:label :holds?}]."
  []
  (let [n-terms 8]
    (concat
     [{:label "(x) is 0 half way between integers: (7/2) = 0" :holds? (= 0 (bracket 7/2))}
      {:label "(x) is the excess over the nearest integer: (13/10) = 3/10, (17/10) = -3/10"
       :holds? (and (= 3/10 (bracket 13/10)) (= -3/10 (bracket 17/10)))}
      {:label "S_8 is continuous at 1/3 (odd denominator) and jumps at 3/10 = 3/(2 x 5)"
       :holds? (and (zero? (jump n-terms 1/3)) (= (jump n-terms 3/10) (odd-denominator-jump 5 1)))}
      {:label "S_8 at 1/2 (n = 1): S(x+0) = S(x) + j/2, S(x-0) = S(x) - j/2, j = -(1 + 1/9 + 1/25 + 1/49)"
       :holds? (let [j (jump n-terms 1/2) fx (partial-sum n-terms 1/2)]
                 (and (= j (odd-denominator-jump 1 4))
                      (= (partial-sum-right n-terms 1/2) (+ fx (/ j 2)))
                      (= (partial-sum-left n-terms 1/2) (- fx (/ j 2)))))}
      {:label "S_12 at 1/6 (n = 3): j = -(1/9)(1 + 1/9)" :holds? (= (jump 12 1/6) (odd-denominator-jump 3 2))}
      {:label "jumps of at least 1/5 in (0 1): finitely many, at 1/4, 1/2, 3/4"
       :holds? (= [1/4 1/2 3/4] (big-jumps 1/5))}
      {:label "jumps of at least 1/10: five, adding 1/6 and 5/6 (pi^2/72 > 1/10)"
       :holds? (= [1/6 1/4 1/2 3/4 5/6] (big-jumps 1/10))}]
     (for [[n0 n1] [[4 8] [8 16] [16 32]]
           :let [s0 (sums-of-partial n-terms n0) s1 (sums-of-partial n-terms n1)]]
       {:label (str "S_8, " n0 " to " n1 " intervals: lower rises, upper falls, sum delta_i D_i shrinks")
        :holds? (and (<= (:lower s1) (:upper s1))
                     (<= (:lower s0) (:lower s1)) (<= (:upper s1) (:upper s0))
                     (< (:oscillation s1) (:oscillation s0)))}))))

(defn numeric-checks
  "Where only a numeric value exists: [{:label :grade :source}]. The sum
   1 + 1/9 + 1/25 + ... is one raster loop kernel over the Emmy term
   1/(2k-1)^2; pi^2/8 is evaluated by a raster kernel too."
  []
  (let [odd-sum (:value (rs/sum (fn [k] (e// 1 (e/square (e/- (e/* 2 k) 1)))) 1 200000))
        target (raster/value (fn [p] (e// (e/square p) 8)) Math/PI)]
    [{:label "1 + 1/9 + 1/25 + ... = pi^2/8 (200000 terms, within 1e-5)"
      :grade (variant :numeric [(- odd-sum target)] 1e-5)
      :source raster/source}]))

(defn graded
  "[{:label :grade}] of every identity and check of the sections; the
   numeric rows keep their :source (:raster)."
  []
  (vec
   (concat
    (map (fn [[label diff]] {:label label :grade (variant :symbolic [diff])})
         (concat x-squared-identities jump-identities))
    (map (fn [{:keys [label holds?]}] {:label label :grade (if holds? :grade/proved :grade/fails)})
         (exact-checks))
    (numeric-checks))))

;; ---------------------------------------------------------------------------
;; The sections as data

(def proofs-resource "alexandria/riemann/trigonometrische_reihe.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
