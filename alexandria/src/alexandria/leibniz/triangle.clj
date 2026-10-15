(ns alexandria.leibniz.triangle
  "The characteristic triangle of Nova Methodus (1684) on one curve, as a
   tie of live quantities (widget brief, section 5).

   The curve is Apollonius' parabola with parameter a, x^2 = a y (the
   curve of the series' conics notebook). At P = (x, y) Leibniz takes a
   line dx \"pro arbitrio\" (at will); Q is the point of the curve over
   x + dx and dy its rise. The triangle with sides dx, dy and the chord PQ
   is the characteristic triangle; the ratio dy : dx is the chord's slope.

   Emmy derives everything below from y(x) = x^2 / a; nothing is typed in
   as a result:

     the difference   dy = y(x + dx) - y(x), simplified: (2x dx + dx^2)/a
     the ratio        dy/dx = (2x + dx)/a
     the tangent      at dx = 0 the ratio is Emmy's D y at x, 2x/a: the
                      chord falls on the tangent; the gap dy - y'(x) dx is
                      dx^2/a, which Leibniz drops (d xx = 2x dx)
     the subtangent   y / y'(x) = x/2: Leibniz's dv : dx = v : VB
     solved back      from the ratio s, dx = a s - 2x (Emmy's solve-linear);
                      from dy, dx = sqrt(x^2 + a dy) - x, the positive root
                      of the quadratic dx^2 + 2x dx - a dy = 0
     the tie          forward (x, dx) -> (dy, s), inverse s -> dx and
                      dy -> dx, composed both ways to the identity

   `forward`, `dx-of-ratio` and `dx-of-dy` are Emmy functions of numbers or
   symbols: raster compiles them to the widget's kernels, `grades` checks
   them symbolically (alexandria.grade, :grade/proved)."
  (:require [alexandria.apollonius.projection :as pj]
            [alexandria.grade :as grade]
            [emmy.env :as e]
            [emmy.expression :as x]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The curve and the triangle

(defn curve
  "Apollonius' parabola x^2 = a y as y(x)."
  [a]
  (fn [x] (e// (e/square x) a)))

(defn difference
  "dy: the rise of the curve from x to x + dx."
  [a x dx]
  (e/- ((curve a) (e/+ x dx)) ((curve a) x)))

(defn ratio
  "dy/dx, the slope of the chord PQ, as the quotient."
  [a x dx]
  (e// (difference a x dx) dx))

(defn forward
  "[dy s] of the triangle at x with side dx: the rise, and the ratio in the
   form Emmy simplified it to, (2x + dx)/a, which is defined at dx = 0
   (graded equal to the quotient in `grades`)."
  [a x dx]
  [(difference a x dx) (e// (e/+ (e/* 2 x) dx) a)])

(defn dx-of-ratio
  "dx from the ratio s (x, a held): the root of a s - (2x + dx) = 0 by
   Emmy's solve-linear."
  [a x s]
  (:root (pj/solve-linear-in (fn [dx] (e/- (e/* a s) (e/+ (e/* 2 x) dx))))))

(defn dx-of-dy
  "dx from the rise dy (x, a held): the positive root of
   dx^2 + 2x dx - a dy = 0."
  [a x dy]
  (e/- (e/sqrt (e/+ (e/square x) (e/* a dy))) x))

(defn tangent-slope
  "y'(x), by Emmy's D."
  [a x]
  ((e/D (curve a)) x))

(defn subtangent
  "VB = y / y'(x): where the tangent at P meets the axis, measured back
   from the foot of the ordinate."
  [a x]
  (e// ((curve a) x) (tangent-slope a x)))

;; ---------------------------------------------------------------------------
;; The derivation, with symbols

(def derivation
  (delay
    (let [rat (e/simplify (ratio 'a 'x 'dx))]
      {:curve ((curve 'a) 'x)
       :dy (e/simplify (difference 'a 'x 'dx))
       :ratio rat
       :ratio-at-0 (e/simplify (x/substitute rat 'dx 0))
       :tangent (e/simplify (tangent-slope 'a 'x))
       :gap (e/simplify (e/- (difference 'a 'x 'dx) (e/* (tangent-slope 'a 'x) 'dx)))
       :subtangent (e/simplify (subtangent 'a 'x))
       :dx-of-ratio (e/simplify (dx-of-ratio 'a 'x 's))
       :dx-of-dy (dx-of-dy 'a 'x 'dy)})))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [label & diffs]
  {:label label :grade (variant (grade/grade :symbolic (vec diffs)))})

(defn grades
  "[{:label :grade}]: the derivation and the tie, each a difference Emmy
   simplifies to exact 0."
  []
  (let [d @derivation
        [dy1 s1] (forward 'a 'x 'dx)]
    [(proved "dy = y(x + dx) − y(x) = (2x·dx + dx²)/a"
             (e/- (:dy d) (e// (e/+ (e/* 2 'x 'dx) (e/square 'dx)) 'a)))
     (proved "dy/dx = (2x + dx)/a, the ratio the widget computes"
             (e/- (:ratio d) s1))
     (proved "at dx = 0 the ratio is the tangent's slope y′(x) = 2x/a (Emmy's D)"
             (e/- (:ratio-at-0 d) (:tangent d)) (e/- (:tangent d) (e// (e/* 2 'x) 'a)))
     (proved "the chord leaves the tangent by dx²/a, the term Leibniz drops: d(xx) = 2x dx"
             (e/- (:gap d) (e// (e/square 'dx) 'a)))
     (proved "the subtangent VB = y/y′ = x/2 (dv : dx = v : VB)" (e/- (:subtangent d) (e// 'x 2)))
     (proved "solved back from the ratio: dx = a·s − 2x (solve-linear)"
             (e/- (:dx-of-ratio d) (e/- (e/* 'a 's) (e/* 2 'x))))
     (proved "solved back from dy: dx = √(x² + a·dy) − x is a root of dx² + 2x·dx − a·dy"
             (let [r (:dx-of-dy d)] (e/- (e/+ (e/square r) (e/* 2 'x r)) (e/* 'a 'dy))))
     (proved "forward then inverse: dx from the ratio of dx" (e/- (dx-of-ratio 'a 'x s1) 'dx))
     (proved "inverse then forward: the ratio of dx(s) is s" (e/- (second (forward 'a 'x (dx-of-ratio 'a 'x 's))) 's))
     (proved "inverse then forward: the rise of dx(dy) is dy" (e/- (difference 'a 'x (dx-of-dy 'a 'x 'dy)) 'dy))
     (proved "forward then inverse: dx from the rise of dx (where x + dx ≥ 0)"
             (e/- (dx-of-dy 'a 'x dy1) 'dx))]))

;; ---------------------------------------------------------------------------
;; Equations for the page (emmy.clerk/->TeX)

(defn- eq [lhs rhs]
  (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn equations
  "[{:label :eqs}]: the derivation as Emmy equations, each right-hand side
   as Emmy derived and simplified it."
  []
  (let [d @derivation]
    [{:label "the curve: Apollonius' parabola with parameter a"
      :eqs [(eq 'y (:curve d))]}
     {:label "the difference of y, from x to x + dx"
      :eqs [(eq 'dy (:dy d))]}
     {:label "the ratio, the slope of the chord PQ"
      :eqs [(eq (list '/ 'dy 'dx) (:ratio d))]}
     {:label "dx = 0: the ratio is the tangent's slope (Emmy's D); the chord leaves the tangent by"
      :eqs [(eq (list 'Dy 'x) (:tangent d)) (eq (list '- 'dy (list '* (list 'Dy 'x) 'dx)) (:gap d))]}
     {:label "the subtangent VB = y / y′"
      :eqs [(eq 'VB (:subtangent d))]}
     {:label "solved back for dx, from the ratio s and from dy"
      :eqs [(eq 'dx (:dx-of-ratio d)) (eq 'dx (:dx-of-dy d))]}]))
