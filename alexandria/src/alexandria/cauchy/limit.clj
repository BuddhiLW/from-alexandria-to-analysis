(ns alexandria.cauchy.limit
  "Cauchy's limit and continuity (Cours d'analyse 1821, Preliminaires and
   ch. II sec. 2) as a tie between two tolerances.

   The doubter names eps, a tolerance on the value of f; the answer is
   delta, a tolerance on the variable, such that |h| <= delta keeps
   |f(a + h) - f(a)| <= eps. The largest such delta and eps are tied by

     eps = W(a, delta),   W(a, delta) = sup |f(a + h) - f(a)| over |h| <= delta

   the worst increment in the window. W is the inverse map (delta -> eps);
   the forward map (eps -> delta) solves W(a, delta) = eps for delta.

   Where W has a closed form on its worst side, Emmy derives the forward
   map on the page: for f = x^2 (worst side right) the equation
   (a + delta)^2 - a^2 = eps is quadratic in delta, and Emmy reads its
   coefficients off by differentiation and takes the positive root,
   delta = sqrt(a^2 + eps) - a; for f = 1/x (worst side left)
   1/(a - delta) - 1/a = eps cleared of denominators is linear in delta,
   delta = eps a^2 / (1 + eps a). Both maps are graded forward-then-inverse
   = identity, :grade/proved.

   Where there is no formula (sin x, a jump) the forward map is a search:
   bisection on delta, each trial W(a, mid) one raster kernel call (the 33
   increments of the window). At a jump the search closes on delta = 0:
   no window works."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The functions of the game

(defn- heaviside
  "0 left of 0, 1 right of it, 1/2 at 0; branch-free (raster lowers it)."
  [x]
  (e/* 0.5 (e/+ 1 (e/divide x (e/+ (e/abs x) 1e-12)))))

(def jump-at "Where the jump function jumps." 1.0)
(def jump-size "How far it jumps." 1.2)

(def functions
  "The functions the reader can pick: id -> {:f :label :tex :worst}. :worst
   names the side where the increment first reaches eps when a closed form
   exists (the function increases away from a on that side)."
  {:square {:f e/square :label "x²" :tex "f(x) = x^2" :worst :right}
   :reciprocal {:f (fn [x] (e/divide 1 x)) :label "1/x" :tex "f(x) = 1/x" :worst :left}
   :sine {:f e/sin :label "sin x" :tex "f(x) = \\sin x"}
   :jump {:f (fn [x] (e/+ 1 (e/* jump-size (heaviside (e/- x jump-at)))))
          :label "a jump at 1" :tex "f(x) = 1 + 1.2\\,H(x - 1)"}})

(def order "The picker's order." [:square :reciprocal :sine :jump])

(def a-range "The slider range of a: positive, so 1/x is defined in every window." [0.25 2.5])

(defn delta-max
  "The largest window the search tries: 1, and for 1/x at most 0.9 a so the
   window stays right of 0."
  [id a]
  (if (= id :reciprocal) (* 0.9 a) 1.0))

;; ---------------------------------------------------------------------------
;; The inverse map: the worst increment in the window

(def grid
  "Points s of [-1 1]; the window's points are a + s delta. 33 points,
   both edges included, so a function monotone on each side of a has its
   sup exactly at a grid point."
  (mapv #(- (/ % 16) 1) (range 33)))

(defn increments
  "The 33 increments |f(a + s delta) - f(a)|, s over the grid: one raster
   kernel returning a vector. W(a, delta) is their largest; the largest is
   taken outside the kernel (a branch-free max tree of 33 leaves has no
   common subexpressions and overflows a JVM method)."
  [f a delta]
  (let [fa (f a)]
    (mapv (fn [s] (e/abs (e/- (f (e/+ a (e/* s delta))) fa))) grid)))

(defn increments-value
  "The increments of function id at a and delta, by its raster kernel."
  [id a delta]
  (raster/value (fn [a d] (increments (:f (functions id)) a d)) a delta))

(defn worst-value
  "W(a, delta) for function id: the largest raster increment."
  [id a delta]
  (reduce max (increments-value id a delta)))

;; ---------------------------------------------------------------------------
;; The forward map: the search, and the closed forms by Emmy

(def search-steps "Halvings per solve: 1 / 2^48 of the bracket." 48)

(defn search-step
  "One halving of the bracket [lo hi] of delta for W(a, delta) = eps, with
   w = W at the midpoint: a window too wide (w > eps) moves hi down to the
   midpoint, otherwise lo up to it. The browser runs the same step."
  [[lo hi] w eps]
  (let [mid (* 0.5 (+ lo hi))]
    (if (> w eps) [lo mid] [mid hi])))

(defn search
  "The forward map by the search, on the JVM: each trial W(a, mid) one call
   of the increments' raster kernel. {:delta :trials [[mid W] ...]}."
  [id a eps]
  (loop [i 0 br [0.0 (double (delta-max id a))] trials []]
    (if (= i search-steps)
      {:delta (first br) :trials trials}
      (let [mid (* 0.5 (apply + br))
            w (worst-value id a mid)]
        (recur (inc i) (search-step br w eps) (conj trials [mid w]))))))

(defn edge-equation
  "F(delta) = 0 on the worst side: f(a + delta) - f(a) - eps (right) or
   f(a - delta) - f(a) - eps (left), cleared of denominators for 1/x."
  [id]
  (case id
    :square (fn [d] (e/- (e/square (e/+ 'a d)) (e/square 'a) 'eps))
    :reciprocal (fn [d] (e/* (e/- (e/divide 1 (e/- 'a d)) (e/divide 1 'a) 'eps) 'a (e/- 'a d)))))

(defn- coefficients
  "{:c0 :c1 :c2} of F as a polynomial of degree <= 2 in delta, read off by
   Emmy's derivatives at 0; :third, F''', must vanish."
  [F]
  {:c0 (e/simplify (F 0))
   :c1 (e/simplify ((e/D F) 0))
   :c2 (e/simplify (e/* 1/2 (((e/expt e/D 2) F) 0)))
   :third (e/simplify (((e/expt e/D 3) F) 'delta))})

(defn forward
  "delta(a, eps) for :square or :reciprocal: the positive root of the
   quadratic, or the root of the linear equation."
  [id a eps]
  (case id
    :square (e/- (e/sqrt (e/+ (e/square a) eps)) a)
    :reciprocal (e/divide (e/* eps a a) (e/+ 1 (e/* eps a)))))

(defn inverse
  "eps(a, delta), the increment on the worst side at delta."
  [id a delta]
  (case id
    :square (e/- (e/square (e/+ a delta)) (e/square a))
    :reciprocal (e/- (e/divide 1 (e/- a delta)) (e/divide 1 a))))

(def derivation
  "Emmy's derivation for each closed-form game: the edge equation, its
   coefficients, the solved root. Quadratic: divided by c2 to
   d^2 + 2p d + q = 0, p = c1/(2 c2), q = c0/c2, the positive root
   -p + sqrt(p^2 - q). Linear (c2 = 0): -c0/c1."
  (delay
    (into {}
          (for [id [:square :reciprocal]]
            (let [F (edge-equation id)
                  {:keys [c0 c1 c2] :as cs} (coefficients F)
                  linear? (e/zero? (e/simplify c2))
                  p (when-not linear? (e/simplify (e/divide c1 (e/* 2 c2))))
                  q (when-not linear? (e/simplify (e/divide c0 c2)))
                  root (e/simplify
                        (if linear?
                          (e/- (e/divide c0 c1))
                          (e/+ (e/- p) (e/sqrt (e/- (e/square p) q)))))]
              [id (assoc cs :F (e/simplify (F 'delta)) :root root :linear? linear? :p p :q q)])))))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [label diff]
  {:label label :grade (variant (grade/grade :symbolic [diff]))})

(defn- numeric [label diff tol]
  {:label label :grade (variant (grade/grade :numeric [diff] tol)) :source raster/source})

(defn grades
  "The tie, graded: Emmy's root solves the edge equation and is the
   forward map; forward-then-inverse and inverse-then-forward are the
   identity (proved); the raster search agrees with the formula, finds
   the sine's delta, and closes on 0 at the jump (numeric)."
  []
  (let [d @derivation]
    (vec
     (concat
      (mapcat (fn [id]
                (let [{:keys [root third]} (d id)
                      F (edge-equation id)
                      lbl (get-in functions [id :label])]
                  [(proved (str lbl ": the edge equation is " (if (= id :square) "quadratic" "linear") " in delta (F''' = 0)") third)
                   (proved (str lbl ": Emmy's root solves it, F(delta) = 0") (F root))
                   (proved (str lbl ": the root is the forward map " (if (= id :square) "sqrt(a^2 + eps) - a" "eps a^2/(1 + eps a)"))
                           (e/- root (forward id 'a 'eps)))
                   (proved (str lbl ": forward then inverse is the identity, eps -> delta -> eps")
                           (e/- (inverse id 'a (forward id 'a 'eps)) 'eps))
                   (proved (str lbl ": inverse then forward is the identity, delta -> eps -> delta")
                           (e/- (forward id 'a (inverse id 'a 'delta)) 'delta))]))
              [:square :reciprocal])
      [(numeric "x², a = 1, eps = 0.2: the raster search finds the formula's delta = sqrt(1.2) - 1"
                (- (:delta (search :square 1.0 0.2)) (- (Math/sqrt 1.2) 1)) 1e-9)
       (numeric "1/x, a = 0.8, eps = 0.5: the raster search finds eps a^2/(1 + eps a)"
                (- (:delta (search :reciprocal 0.8 0.5)) (/ (* 0.5 0.64) 1.4)) 1e-9)
       (numeric "sin x, a = 1, eps = 0.1: no formula; the search's delta has worst increment eps"
                (- (worst-value :sine 1.0 (:delta (search :sine 1.0 0.1))) 0.1) 1e-6)
       (numeric "the jump at a = 1, eps = 0.3: every window holds the jump, the search closes on delta = 0"
                (:delta (search :jump 1.0 0.3)) 1e-12)]))))

(defn equations
  "[{:label :eqs}] for emmy.clerk/->TeX: per closed-form game, the edge
   equation, its coefficients and the root."
  []
  (let [d @derivation
        eq (fn [lhs rhs] (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))]
    (vec
     (for [id [:square :reciprocal]
           :let [{:keys [F c0 c1 c2 root]} (d id)]]
       {:label (str (get-in functions [id :label]) ", worst side "
                    (name (get-in functions [id :worst])) ": the increment reaches eps at the edge of the window")
        :eqs [(eq (list 'F 'delta) F)
              (eq 'c_0 c0) (eq 'c_1 c1) (eq 'c_2 c2)
              (eq 'delta root)]}))))
