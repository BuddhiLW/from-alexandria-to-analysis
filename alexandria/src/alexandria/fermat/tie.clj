(ns alexandria.fermat.tie
  "The equations behind the Descartes-Fermat widgets, derived and solved by
   Emmy, with their grades.

   1. The point B on Fermat's parabola y^2 = p x (vertex D at the origin,
      diameter DC along x), B = (d, b) with b^2 = p d, so CD = d.
        - Fermat (adequality): the tangent meets the diameter at E with
          CE = 2 CD. Emmy redoes his steps on the chord through B and the
          point of ordinate b + e (CE of the chord = 2 d + b e / p) and
          strikes e.
        - Descartes (double root): the circle about P = (v, 0) through B
          meets the curve where Y = y^2 solves
            Y^2 / p^2 + (1 - 2 v / p) Y + v^2 - s^2 = 0,  s^2 = (d - v)^2 + p d.
          Its discriminant is linear in v (the v^2 cancel); Emmy solves it:
          v = d + p/2, so the normal meets the diameter at subnormal CP = p/2.
      The tied quantities of the widget: p, d = CD, ce = CE and np = DP
      (the foot of the normal). Forward maps ce = 2 d, np = d + p/2;
      inverse maps d = ce/2, d = np - p/2; Emmy grades forward-then-inverse
      as the identity.

   2. The loci of the Isagoge as general equations of the second degree,
        alpha A^2 + beta A E + gamma E^2 + delta A + epsilon E + zeta = 0,
      named by beta^2 - 4 alpha gamma (and alpha = gamma, beta = 0 for the
      circle at a right angle): `general-form`, `invariants`, `kind`.

   Reuse searched: emmy simplify, D (derivative) for the linear solve
   (the coefficient of v), alexandria.grade; nothing in emmy names a scalar
   linear solve, so `solve-linear` reads the two coefficients off by D."
  (:require [alexandria.grade :as grade]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; 1. Tangent and normal at B

(defn ordinate
  "b = sqrt(p d): the ordinate BC of the point at abscissa CD = d."
  [p d]
  (e/sqrt (e/* p d)))

(defn chord-ce
  "CE of the chord from B (ordinate b) to the point of ordinate b + e on
   y^2 = p x: E of the chord sits at x = -b (b + e) / p, C at x = b^2 / p."
  [p b ex]
  (e/+ (e// (e/square b) p) (e// (e/* b (e/+ b ex)) p)))

(defn circle-quadratic
  "Descartes' equation for Y = y^2: the circle about (v, 0) of radius^2 s2
   meets y^2 = p x where Y^2/p^2 + (1 - 2v/p) Y + v^2 - s2 = 0. Returns the
   coefficients [a2 a1 a0]."
  [p v s2]
  [(e// 1 (e/square p)) (e/- 1 (e// (e/* 2 v) p)) (e/- (e/square v) s2)])

(defn discriminant
  "a1^2 - 4 a2 a0 of the circle about (v, 0) through B = (d, sqrt(p d))."
  [p d v]
  (let [[a2 a1 a0] (circle-quadratic p v (e/+ (e/square (e/- d v)) (e/* p d)))]
    (e/- (e/square a1) (e/* 4 a2 a0))))

(defn quadratic-coefficients
  "[a b c] of f(v) = a v^2 + b v + c, read off by Emmy: a = f''/2,
   b = f'(0), c = f(0)."
  [f]
  [(e/simplify (e// ((e/D (e/D f)) 'v) 2)) (e/simplify ((e/D f) 0)) (e/simplify (f 0))])

(defn solve-double
  "The root of f(v) = 0 for f quadratic in v with a double root:
   v = -b / 2a (its own discriminant b^2 - 4ac is graded zero)."
  [f]
  (let [[a b] (quadratic-coefficients f)]
    (e/simplify (e/- 0 (e// b (e/* 2 a))))))

(defn forward
  "[ce np] from p and d: CE = 2 CD, DP = CD + p/2."
  [p d]
  [(e/* 2 d) (e/+ d (e// p 2))])

(defn d-from-ce [ce] (e// ce 2))
(defn d-from-np [p np] (e/- np (e// p 2)))

(defn derivation
  "The steps the notebook shows, each {:label :eqs}, eqs as [left right]
   Emmy expressions, already simplified."
  []
  (let [p 'p d 'd b 'b ex 'e v 'v
        delta (fn [v] (discriminant p d v))]
    [{:label "Fermat: the chord from B to the point of ordinate b + E meets the diameter at"
      :eqs [['CE (e/simplify (chord-ce p b ex))]]}
     {:label "with b² = p d, CE − 2 CD is"
      :eqs [[(e/- 'CE (e/* 2 'CD)) (e/simplify (e/- (chord-ce p b ex) (e/* 2 (e// (e/square b) p))))]]}
     {:label "strike out E: CE = 2 CD"
      :eqs [['CE (e/simplify (e/* 2 (e// (e/square b) p)))]]}
     {:label "Descartes: the circle about P = (v, 0) through B meets the curve where (Y = y²)"
      :eqs [[(let [[a2 a1 a0] (circle-quadratic p v (e/+ (e/square (e/- d v)) (e/* p d)))]
               (e/simplify (e/+ (e/* a2 (e/square 'Y)) (e/* a1 'Y) a0)))
             0]]}
     {:label "the two roots are one when the discriminant vanishes"
      :eqs [['Delta (e/simplify (delta v))]]}
     {:label "Emmy reads Δ's coefficients in v, and finds it a perfect square (its own discriminant is 0), so"
      :eqs [['v (solve-double delta)]]}
     {:label "so the tie, and its inverse"
      :eqs [['CE (e/* 2 d)] ['DP (e/+ d (e// p 2))] ['d (e// 'CE 2)] ['d (e/- 'DP (e// p 2))]]}]))

(defn identities
  "[label difference] pairs, each zero."
  []
  (let [p 'p d 'd b 'b ex 'e ce 'ce np 'np
        [ce' np'] (forward p d)
        delta (fn [v] (discriminant p d v))
        [qa qb qc] (quadratic-coefficients delta)
        v* (solve-double delta)]
    [["Fermat: CE of the chord - 2 CD = b E / p (b^2 = p d)"
      (e/- (e/- (chord-ce p b ex) (e/* 2 (e// (e/square b) p))) (e// (e/* b ex) p))]
     ["E struck out: CE = 2 CD" (e/- (chord-ce p b 0) (e/* 2 (e// (e/square b) p)))]
     ["Descartes: Delta is quadratic in v, read off exactly"
      (e/- (delta 'v) (e/+ (e/* qa (e/square 'v)) (e/* qb 'v) qc))]
     ["Delta is a perfect square: its own discriminant is 0" (e/- (e/square qb) (e/* 4 qa qc))]
     ["Descartes: Delta = 0 at v = d + p/2" (e/- v* (e/+ d (e// p 2)))]
     ["at v = d + p/2 the double root is Y = b^2 = p d"
      (let [[a2 a1] (circle-quadratic p v* 0)] (e/- (e// (e/- 0 a1) (e/* 2 a2)) (e/* p d)))]
     ["the normal BP is at right angles to the tangent EB: EC.CP = BC^2"
      (e/- (e/* (e/- d (e/- 0 d)) (e/- v* d)) (e/* p d))]
     ["tie: d -> ce -> d is the identity" (e/- (d-from-ce ce') d)]
     ["tie: d -> np -> d is the identity" (e/- (d-from-np p np') d)]
     ["tie: ce -> d -> ce is the identity" (e/- (first (forward p (d-from-ce ce))) ce)]
     ["tie: np -> d -> np is the identity" (e/- (second (forward p (d-from-np p np))) np)]]))

(defn- grade-of [kind diffs]
  (let [res (grade/grade kind diffs)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn graded
  "[{:label :grade}] of the identities."
  []
  (mapv (fn [[label diff]] {:label label :grade (grade-of :symbolic [diff])}) (identities)))

;; ---------------------------------------------------------------------------
;; 1b. The same tie on every conic: Apollonius' symptom y^2 = p x + k x^2
;;
;; k = 0 is the parabola, k < 0 the ellipse, k > 0 the hyperbola (k = -p/q
;; or p/q, q the transverse side; alexandria.apollonius.conics/names). B =
;; (d, b), b^2 = p d + k d^2. Fermat: adequate f(d + E) with f(d), divide by
;; E, strike E; the subtangent is CE = 2 b^2 / (p + 2 k d). Descartes: the
;; circle about (v, 0) through B meets the conic where
;;   (1 + k) x^2 + (p - 2 v) x + v^2 - s^2 = 0,
;; a double root at x = d exactly when v = d (1 + k) + p/2. At k = -r/q that
;; is his own rule for the ellipse, v = e - (r/q) e + r/2.

(defn symptom
  "f(x) = p x + k x^2: the square on the ordinate at abscissa x."
  [p k x]
  (e/+ (e/* p x) (e/* k x x)))

(defn conic-forward
  "[ce np] from p, k and d: CE = 2 d (p + k d) / (p + 2 k d),
   DP = d (1 + k) + p/2."
  [p k d]
  [(e// (e/* 2 (symptom p k d)) (e/+ p (e/* 2 k d)))
   (e/+ (e/* d (e/+ 1 k)) (e// p 2))])

(defn conic-d-from-ce
  "The abscissa whose subtangent is ce: the root of k d^2 + (p - k ce) d
   - p ce / 2 = 0 that is ce/2 at k = 0, written without a division by k:
   d = ce p / (p - k ce + sqrt(p^2 + k^2 ce^2))."
  [p k ce]
  (e// (e/* ce p) (e/+ (e/- p (e/* k ce)) (e/sqrt (e/+ (e/square p) (e/* k k ce ce))))))

(defn conic-d-from-np
  "d = (DP - p/2) / (1 + k)."
  [p k np]
  (e// (e/- np (e// p 2)) (e/+ 1 k)))

(defn conic-circle-quadratic
  "[a2 a1 a0] of the circle about (v, 0), radius^2 s2, met with the conic:
   (1 + k) x^2 + (p - 2v) x + v^2 - s2."
  [p k v s2]
  [(e/+ 1 k) (e/- p (e/* 2 v)) (e/- (e/square v) s2)])

(defn conic-discriminant
  "a1^2 - 4 a2 a0 for the circle about (v, 0) through B at abscissa d."
  [p k d v]
  (let [[a2 a1 a0] (conic-circle-quadratic p k v (e/+ (e/square (e/- d v)) (symptom p k d)))]
    (e/- (e/square a1) (e/* 4 a2 a0))))

(defn conic-chord-ce
  "CE of Fermat's chord from B (ordinate b) to the point at abscissa d + E
   (ordinate b2): b E / (b2 - b)."
  [b b2 ex]
  (e// (e/* b ex) (e/- b2 b)))

(defn conic-derivation
  "The steps on the page, {:label :eqs}, for the symptom y^2 = p x + k x^2."
  []
  (let [p 'p k 'k d 'd ex 'E v 'v
        f (fn [x] (symptom p k x))
        delta (fn [v] (conic-discriminant p k d v))
        [ce np] (conic-forward p k d)]
    [{:label "Apollonius' symptom (k = 0 parabola, k < 0 ellipse, k > 0 hyperbola)"
      :eqs [[(e/square 'y) (f 'x)]]}
     {:label "Fermat: adequate f(d + E) with f(d), take away what is common, divide by E"
      :eqs [['Q (e/simplify (e// (e/- (f (e/+ d ex)) (f d)) ex))]]}
     {:label "strike out E (Q = p + 2kd); the chord's CE = bE/(b' − b) = b(b' + b)/Q turns into"
      :eqs [['CE (e/simplify ce)]]}
     {:label "Descartes: the circle about P = (v, 0) through B meets the conic where"
      :eqs [[(let [[a2 a1 a0] (conic-circle-quadratic p k v (e/+ (e/square (e/- d v)) (f d)))]
               (e/simplify (e/+ (e/* a2 (e/square 'x)) (e/* a1 'x) a0)))
             0]]}
     {:label "its discriminant, a perfect square in v"
      :eqs [['Delta (e/simplify (delta v))]]}
     {:label "one double root, so"
      :eqs [['v (solve-double delta)]]}
     {:label "the tie, and its inverse"
      :eqs [['CE (e/simplify ce)] ['DP (e/simplify np)]
            ['d (conic-d-from-ce p k 'CE)] ['d (conic-d-from-np p k 'DP)]]}]))

(defn conic-identities
  "[label difference] pairs, each zero, for every p, k, d."
  []
  (let [p 'p k 'k d 'd ex 'E b 'b b2 'b2 ce 'ce np 'np r 'r q 'q
        f (fn [x] (symptom p k x))
        delta (fn [v] (conic-discriminant p k d v))
        [qa qb qc] (quadratic-coefficients delta)
        v* (solve-double delta)
        [ce' np'] (conic-forward p k d)]
    [["Fermat: (f(d + E) - f(d)) / E = p + 2kd + kE"
      (e/- (e// (e/- (f (e/+ d ex)) (f d)) ex) (e/+ p (e/* 2 k d) (e/* k ex)))]
     ["the chord: bE / (b' - b) = b (b' + b) E / (b'^2 - b^2)"
      (e/- (conic-chord-ce b b2 ex) (e// (e/* b (e/+ b2 b) ex) (e/- (e/square b2) (e/square b))))]
     ["E struck out: CE = 2 b^2 / (p + 2kd) with b^2 = f(d)"
      (e/- ce' (e// (e/* 2 (f d)) (e/+ p (e/* 2 k d))))]
     ["Descartes: Delta is quadratic in v, read off exactly"
      (e/- (delta 'v) (e/+ (e/* qa (e/square 'v)) (e/* qb 'v) qc))]
     ["Delta is a perfect square: its own discriminant is 0" (e/- (e/square qb) (e/* 4 qa qc))]
     ["Delta = 4 (v - DP)^2" (e/- (delta 'v) (e/* 4 (e/square (e/- 'v np'))))]
     ["Delta = 0 at v = d (1 + k) + p/2 = DP" (e/- v* np')]
     ["at that v the double root is x = d"
      (let [[a2 a1] (conic-circle-quadratic p k v* 0)] (e/- (e// (e/- 0 a1) (e/* 2 a2)) d))]
     ["the normal BP is at right angles to the tangent EB: EC.CP = BC^2"
      (e/- (e/* ce' (e/- np' d)) (f d))]
     ["k = 0: Fermat's CE = 2 CD and DP = CD + p/2"
      (e/+ (e/- (first (conic-forward p 0 d)) (e/* 2 d)) (e/- (second (conic-forward p 0 d)) (e/+ d (e// p 2))))]
     ["k = -r/q, p = r: Descartes' own ellipse rule v = e - (r/q) e + r/2"
      (e/- (second (conic-forward r (e/- 0 (e// r q)) d)) (e/+ (e/- d (e/* (e// r q) d)) (e// r 2)))]
     ["tie: d -> ce -> d is the identity" (e/- (conic-d-from-ce p k ce') d)]
     ["tie: ce -> d -> ce is the identity" (e/- (first (conic-forward p k (conic-d-from-ce p k ce))) ce)]
     ["tie: d -> np -> d is the identity" (e/- (conic-d-from-np p k np') d)]
     ["tie: np -> d -> np is the identity" (e/- (second (conic-forward p k (conic-d-from-np p k np))) np)]]))

(defn conic-graded
  "[{:label :grade}] of the conic identities."
  []
  (mapv (fn [[label diff]] {:label label :grade (grade-of :symbolic [diff])}) (conic-identities)))

;; ---------------------------------------------------------------------------
;; 2. The loci as general second-degree equations

(def loci-types
  "The Isagoge's equations, in the order of the widget's menu."
  [:line :hyperbola :parabola :circle :ellipse :hyperbola-ratio])

(defn general-form
  "[alpha beta gamma delta epsilon zeta] of each Isagoge equation, with the
   given D, B, Z and ratio k (the right angle at Z):
     :line             D A - B E = 0
     :hyperbola        A E - Z^2 = 0
     :parabola         A^2 - D E = 0
     :circle           B^2 - A^2 - E^2 = 0
     :ellipse          (B^2 - A^2) - k E^2 = 0        ((B^2 - A^2) : E^2 = k)
     :hyperbola-ratio  (B^2 + A^2) - k E^2 = 0        ((B^2 + A^2) : E^2 = k)"
  [kind D B Z k]
  (case kind
    :line [0 0 0 D (e/- 0 B) 0]
    :hyperbola [0 1 0 0 0 (e/- 0 (e/square Z))]
    :parabola [1 0 0 0 (e/- 0 D) 0]
    :circle [-1 0 -1 0 0 (e/square B)]
    :ellipse [-1 0 (e/- 0 k) 0 0 (e/square B)]
    :hyperbola-ratio [1 0 (e/- 0 k) 0 0 (e/square B)]))

(defn evaluate
  "The general form at the point [A E]."
  [[al be ga de ep ze] [A E]]
  (e/+ (e/* al (e/square A)) (e/* be A E) (e/* ga (e/square E)) (e/* de A) (e/* ep E) ze))

(defn invariants
  "[beta^2 - 4 alpha gamma, alpha - gamma, beta, alpha^2 + beta^2 + gamma^2]:
   what names the curve."
  [[al be ga]]
  [(e/- (e/square be) (e/* 4 al ga)) (e/- al ga) be (e/+ (e/square al) (e/square be) (e/square ga))])

(defn kind
  "The name from the invariants (numbers): no square terms, a line; else
   by the sign of beta^2 - 4 alpha gamma, with the circle where the squares
   are equal and there is no rectangle."
  [[disc diff beta quad] & [tol]]
  (let [tol (or tol 1e-9)]
    (cond (< (Math/abs (double quad)) tol) :line
          (< (Math/abs (double disc)) tol) :parabola
          (pos? disc) :hyperbola
          (and (< (Math/abs (double diff)) tol) (< (Math/abs (double beta)) tol)) :circle
          :else :ellipse)))

(defn locus-point
  "Figure: the point [A E] of each locus at parameter t."
  [kind D B Z k]
  (fn [t]
    (case kind
      :line [t (e// (e/* D t) B)]
      :hyperbola [t (e// (e/square Z) t)]
      :parabola [t (e// (e/square t) D)]
      :circle [(e/* B (e/cos t)) (e/* B (e/sin t))]
      :ellipse [(e/* B (e/cos t)) (e// (e/* B (e/sin t)) (e/sqrt k))]
      :hyperbola-ratio [(e/* B (e/tan t)) (e// B (e/* (e/sqrt k) (e/cos t)))])))

(defn loci-identities
  "[label difference]: each figure lies on its general form for every t,
   and the names come out of the invariants."
  []
  (let [D 'D B 'B Z 'Z k 'k t 't]
    (concat
     (for [kd loci-types]
       [(str (name kd) ": the point at t satisfies the general equation")
        (evaluate (general-form kd D B Z k) ((locus-point kd D B Z k) t))])
     [["ellipse: beta^2 - 4 alpha gamma = -4k, negative for every given ratio"
       (e/+ (first (invariants (general-form :ellipse D B Z k))) (e/* 4 k))]
      ["ellipse with k = 1 is the circle: alpha - gamma = 0"
       (second (invariants (general-form :ellipse D B Z 1)))]
      ["(B^2 + A^2) : E^2 = k: beta^2 - 4 alpha gamma = 4k, a hyperbola"
       (e/- (first (invariants (general-form :hyperbola-ratio D B Z k))) (e/* 4 k))]
      ["A E = Z^2: beta^2 - 4 alpha gamma = 1, a hyperbola"
       (e/- (first (invariants (general-form :hyperbola D B Z k))) 1)]
      ["A^2 = D E: beta^2 - 4 alpha gamma = 0, a parabola"
       (first (invariants (general-form :parabola D B Z k)))]])))

(defn loci-graded
  "[{:label :grade}] of the loci identities."
  []
  (mapv (fn [[label diff]] {:label label :grade (grade-of :symbolic [diff])}) (loci-identities)))
