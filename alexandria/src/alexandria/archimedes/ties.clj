(ns alexandria.archimedes.ties
  "The equations behind the Archimedes widgets, derived and graded by Emmy.
   Every quantity a widget shows is a control; these are the maps that tie
   them, as Emmy functions of numbers or symbols. alexandria.raster (JVM)
   and emmy-viewers' :raster backend (browser) compile them to kernels;
   `grades` checks them symbolically (alexandria.grade, :grade/proved).

     Measurement of a Circle, Prop. 3   n, d, p (inscribed perimeter),
                                        P (circumscribed), eps = (P - p)/d
     Quadrature of the Parabola 21-24   a (half chord), T (first triangle),
                                        k (generations), S (polygon), R (rest)
     Sphere and Cylinder I.34, Method 2 r, V (sphere), C (cylinder), K (cone)

   Inverses that have a closed form are exact (Emmy proves forward-then-
   inverse = identity). The two that have none (n from the error bound,
   n from p/d) are Newton's method on Emmy's own derivative D, three steps
   from the leading term of the series; their residual is measured, not
   assumed (tests)."
  (:require [alexandria.grade :as grade]
            [emmy.env :as e]
            [emmy.expression :as x]
            [emmy.simplify :as simp]
            [emmy.simplify.rules :as rules]
            [hive-dsl.result :as r]))

(def ^:private PI Math/PI)

;; ---------------------------------------------------------------------------
;; Measurement of a Circle, Prop. 3: the regular n-gons in and about the circle

(defn inscribed-perimeter
  "The perimeter of the regular n-gon in the circle of diameter d: n sides,
   each the chord d sin(pi/n)."
  [n d]
  (e/* n d (e/sin (e// PI n))))

(defn circumscribed-perimeter
  "The perimeter of the regular n-gon about the circle of diameter d: n
   sides, each the tangent segment d tan(pi/n)."
  [n d]
  (e/* n d (e/tan (e// PI n))))

(defn gap
  "The error bound of the n-gons, (P - p)/d: pi lies between p/d and P/d, so
   each misses pi by less than this."
  [n]
  (e/* n (e/- (e/tan (e// PI n)) (e/sin (e// PI n)))))

(defn newton-step
  "One Newton step on f(u) = 0 from u, the derivative by Emmy's D."
  [f u]
  (e/- u (e// (f u) ((e/D f) u))))

(def newton-steps
  "How many Newton steps the inverses take from the series' leading term:
   three bring n back within 1e-9 for n >= 6 (tests)."
  3)

(defn gap-start "The leading term: gap ~ pi^3 / (2 n^2)." [eps] (e/sqrt (e// (* PI PI PI) (e/* 2 eps))))
(defn gap-step "One Newton step on gap(n) = eps." [u eps] (newton-step (fn [n] (e/- (gap n) eps)) u))
(defn n-of-gap
  "The n whose error bound is eps: newton-steps steps of gap-step from
   gap-start. One expression (the browser compiles it unsimplified); on
   the JVM iterate the gap-step kernel instead."
  [eps]
  (nth (iterate #(gap-step % eps) (gap-start eps)) newton-steps))

(defn d-of-p
  "The diameter whose inscribed n-gon has perimeter p (n held)."
  [n p]
  (e// p (e/* n (e/sin (e// PI n)))))

(defn P-of-p
  "The circumscribed perimeter from the inscribed one, n held: each tangent
   side is the chord over cos(pi/n)."
  [n p]
  (e// p (e/cos (e// PI n))))

(defn p-of-P
  "The inscribed perimeter from the circumscribed one, n held."
  [n P]
  (e/* P (e/cos (e// PI n))))

;; ---------------------------------------------------------------------------
;; Quadrature of the Parabola, Props 21-24, on y = 1 - x^2 cut at y = 1 - a^2

(defn first-triangle
  "The first triangle on the chord of half-width a: base 2a, height a^2."
  [a]
  (e/cube a))

(defn half-chord
  "The half chord of the segment whose first triangle is T."
  [T]
  (e/expt T (/ 1 3)))

(defn- power
  "q^u: Emmy's expt when q is a symbol (the grades), exp(u log q) when q
   is a number (raster lowers a power only with a numeric exponent)."
  [q u]
  (if (number? q) (e/exp (e/* u (Math/log q))) (e/expt q u)))

(defn polygon
  "The inscribed polygon through generation k: T (1 + q + ... + q^k) with
   q = 1/4 (Prop. 21: each generation a quarter of the one before), summed
   by Prop. 23: T (1 - q^(k+1)) / (1 - q) = (4/3) T (1 - 4^-(k+1)). The
   ratio q is a parameter so Emmy can grade the sum for every q."
  ([T k] (polygon T k 1/4))
  ([T k q] (e// (e/* T (e/- 1 (power q (e/+ k 1)))) (e/- 1 q))))

(defn remainder
  "What the polygon through generation k leaves of the segment T/(1 - q) =
   (4/3) T: T q^(k+1) / (1 - q), Prop. 23's 'one third of the last'."
  ([T k] (remainder T k 1/4))
  ([T k q] (e// (e/* T (power q (e/+ k 1))) (e/- 1 q))))

(defn k-of-remainder
  "The generation k whose remainder is R (T held): q^(k+1) = R (1 - q) / T."
  ([T R] (k-of-remainder T R 1/4))
  ([T R q] (e/- (e// (e/log (e// (e/* R (e/- 1 q)) T)) (if (number? q) (Math/log q) (e/log q))) 1)))

;; ---------------------------------------------------------------------------
;; On the Sphere and Cylinder I.34 and The Method, Prop. 2

(defn sphere
  "The sphere of radius r; pi is a parameter so Emmy grades with a symbol."
  ([r] (sphere r PI))
  ([r pi] (e/* 4/3 pi (e/cube r))))
(defn cylinder
  "The cylinder about the sphere: base a great circle, height the diameter."
  ([r] (cylinder r PI))
  ([r pi] (e/* 2 pi (e/cube r))))
(defn cone
  "The cone of I.34: base a great circle, height the radius."
  ([r] (cone r PI))
  ([r pi] (e/* 1/3 pi (e/cube r))))
(defn radius-of-sphere
  ([V] (radius-of-sphere V PI))
  ([V pi] (e/expt (e// (e/* 3 V) (e/* 4 pi)) (/ 1 3))))
(defn radius-of-cylinder
  ([C] (radius-of-cylinder C PI))
  ([C pi] (e/expt (e// C (e/* 2 pi)) (/ 1 3))))
(defn radius-of-cone
  ([K] (radius-of-cone K PI))
  ([K pi] (e/expt (e// (e/* 3 K) pi) (/ 1 3))))

(defn method-slices
  "The Method, Prop. 2, at distance x from A along the diameter AC = 2r:
   the circles cut from the sphere (radius^2 = x (2r - x)), from the cone
   AEF (radius x) and from the cylinder (radius 2r), as areas over pi."
  [r x]
  {:sphere (e/* x (e/- (e/* 2 r) x)) :cone (e/square x) :cylinder (e/square (e/* 2 r))})

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [label diff]
  {:label label :grade (variant (grade/grade :symbolic [diff]))})

(defn- expand
  "Multiple angles expanded, tangents written as sine over cosine, then
   simplified twice (Emmy)."
  [expr]
  (-> expr x/expression-of rules/expand-multiangle simp/simplify-expression simp/simplify-expression))

(defn circle-grades
  "Prop. 3: the perimeters, Archimedes' doubling (Euclid VI.3 gives the
   harmonic mean, then the geometric mean), and the tie's exact inverses."
  []
  (let [n 'n d 'd th 'theta
        ;; with theta = pi/(2n): the n-gon's half angle is 2 theta
        pn (e/* n d (e/sin (e/* 2 th)))
        Pn (e/* n d (e// (e/sin (e/* 2 th)) (e/cos (e/* 2 th))))
        p2 (e/* 2 n d (e/sin th))
        P2 (e/* 2 n d (e// (e/sin th) (e/cos th)))]
    [(proved "doubling, circumscribed: 2/P₂ₙ = 1/pₙ + 1/Pₙ (the harmonic mean; VI.3 at the bisected angle)"
             (expand (e/- (e/* P2 (e/+ pn Pn)) (e/* 2 pn Pn))))
     (proved "doubling, inscribed: p₂ₙ² = pₙ·P₂ₙ (the geometric mean)"
             (expand (e/- (e/square p2) (e/* pn P2))))
     (proved "n held: P = p / cos(π/n)" (e/- (P-of-p 'n (inscribed-perimeter 'n 'd)) (circumscribed-perimeter 'n 'd)))
     (proved "forward then inverse: d → p → d" (e/- (d-of-p 'n (inscribed-perimeter 'n 'd)) 'd))
     (proved "inverse then forward: p → d → p" (e/- (inscribed-perimeter 'n (d-of-p 'n 'p)) 'p))
     (proved "forward then inverse: p → P → p" (e/- (p-of-P 'n (P-of-p 'n 'p)) 'p))
     (proved "inverse then forward: P → p → P" (e/- (P-of-p 'n (p-of-P 'n 'P)) 'P))]))

(defn parabola-grades
  "Props 21-24: the first triangle on the chord, Prop. 23's finite sum, and
   the tie's inverses, for the ratio q as a symbol (Archimedes' q = 1/4
   is one value of it)."
  []
  (let [T 'T k 'k a 'a q 'q w 'w
        ;; S with q^k written as the symbol w (Emmy does not combine
        ;; symbolic powers q^(k+1) = q q^k on its own)
        S (fn [qk] (e// (e/* T (e/- 1 (e/* q qk))) (e/- 1 q)))]
    [(proved "the first triangle: base 2a, height a², area a³"
             (e/- (e/* 1/2 (e/* 2 a) (e/square a)) (first-triangle a)))
     (proved "Prop. 23, Archimedes' numbers: T + T/4 + … + T/4ᵏ + (1/3)(T/4ᵏ) = (4/3) T, for every k"
             (let [v (e/expt 1/4 k)] (e/- (e/+ (e// (e/* T (e/- 1 (e/* 1/4 v))) 3/4) (e/* 1/3 T v)) (e/* 4/3 T))))
     (proved "polygon + remainder = the segment T/(1 − q) = (4/3) T"
             (e/- (e/+ (polygon T k q) (remainder T k q)) (e// T (e/- 1 q))))
     (proved "generation k adds T qᵏ (w = qᵏ): Sₖ − Sₖ₋₁ = T w"
             (e/- (e/- (S w) (S (e// w q))) (e/* T w)))
     (proved "forward then inverse: a → T → a" (e/- (half-chord (first-triangle a)) a))
     (proved "inverse then forward: T → a → T" (e/- (first-triangle (half-chord T)) T))
     (proved "forward then inverse: k → R → k" (e/- (k-of-remainder T (remainder T k q) q) k))]))

(defn sphere-grades
  "I.34 and the corollary (the cylinder is half as large again as the
   sphere), The Method's balance at every slice, and the tie's inverses,
   with pi a symbol."
  []
  (let [r 'r x 'x pi 'pi
        sl (method-slices r x)
        ;; the slices summed from x = 0 to 2r, by antiderivatives Emmy checks
        Fs (fn [u] (e/- (e/* r (e/square u)) (e// (e/cube u) 3)))]
    [(proved "The Method: (sphere slice + cone slice) at H, 2r from A, balance the cylinder slice where it is, x from A"
             (e/- (e/* (e/+ (:sphere sl) (:cone sl)) (e/* 2 r)) (e/* (:cylinder sl) x)))
     (proved "the sphere's slices summed: d/dx [r x² − x³/3] = x(2r − x)" (e/- ((e/D Fs) x) (:sphere sl)))
     (proved "so the sphere is π [r x² − x³/3] from 0 to 2r = (4/3) π r³"
             (e/- (e/* pi (e/- (Fs (e/* 2 r)) (Fs 0))) (sphere r pi)))
     (proved "I.34: the sphere is four times the cone (base a great circle, height the radius)"
             (e/- (sphere r pi) (e/* 4 (cone r pi))))
     (proved "the cylinder is half as large again as the sphere: 2C = 3V" (e/- (e/* 2 (cylinder r pi)) (e/* 3 (sphere r pi))))
     (proved "forward then inverse: r → V → r" (e/- (radius-of-sphere (sphere r pi) pi) r))
     (proved "forward then inverse: r → C → r" (e/- (radius-of-cylinder (cylinder r pi) pi) r))
     (proved "forward then inverse: r → K → r" (e/- (radius-of-cone (cone r pi) pi) r))
     (proved "inverse then forward: V → r → V" (e/- (sphere (radius-of-sphere 'V pi) pi) 'V))]))

(defn- eq [lhs rhs] (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn circle-equations
  "The derivation of the n-gon tie for emmy.clerk/->TeX: [{:label :eqs}]."
  []
  [{:label "the inscribed n-gon: n chords, each d sin(π/n)"
    :eqs [(eq 'p (inscribed-perimeter 'n 'd))]}
   {:label "the circumscribed n-gon: n tangent segments, each d tan(π/n)"
    :eqs [(eq 'P (circumscribed-perimeter 'n 'd))]}
   {:label "the error bound: π lies between p/d and P/d"
    :eqs [(eq 'epsilon (gap 'n))]}
   {:label "solved back, n held (exact)"
    :eqs [(eq 'd (d-of-p 'n 'p)) (eq 'P (P-of-p 'n 'p))]}
   {:label "n from ε: Newton on Emmy's D, from ε ≈ π³/(2n²)"
    :eqs [(eq (list 'Dε 'n) (e/simplify ((e/D gap) 'n)))]}])

(defn parabola-equations []
  [{:label "the first triangle, on the chord of half-width a" :eqs [(eq 'T (first-triangle 'a))]}
   {:label "the polygon through generation k (Prop. 23)" :eqs [(eq 'S (polygon 'T 'k))]}
   {:label "what is left of the segment (4/3)T" :eqs [(eq 'R (remainder 'T 'k))]}
   {:label "solved back (exact)" :eqs [(eq 'a (half-chord 'T)) (eq 'k (k-of-remainder 'T 'R))]}])

(defn sphere-equations []
  [{:label "the sphere, the cylinder about it, the cone of I.34"
    :eqs [(eq 'V (sphere 'r)) (eq 'C (cylinder 'r)) (eq 'K (cone 'r))]}
   {:label "solved back (exact)" :eqs [(eq 'r (radius-of-sphere 'V))]}
   {:label "The Method: the three slices at x from A, over π"
    :eqs (let [sl (method-slices 'r 'x)]
           [(eq 's (:sphere sl)) (eq 'c (:cone sl)) (eq 'z (:cylinder sl))])}])
