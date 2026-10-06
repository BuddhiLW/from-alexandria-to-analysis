(ns alexandria.leibniz.calculus
  "Leibniz's calculus: the characteristic triangle and the transmutation
   theorem (Paris, 1673), which give the arithmetical quadrature of the
   circle pi/4 = 1 - 1/3 + 1/5 - ..., and the rules of Nova Methodus pro
   Maximis et Minimis (Acta Eruditorum, October 1684).

     rules            the rules of 1684 as frozen formulas (alexandria.formula)
                      over the vocabulary's `differential`, each proved by Emmy
                      and rendered in Leibniz's d and in Newton's fluxions
     characteristic   the triangle of a curve: the chord PQ, the sides dx, dy,
                      and the triangle OPQ from the origin, whose area is
                      half of z dx (z where the tangent at P meets the y-axis)
     circle           Leibniz's circle y = sqrt(2x - x^2) and its rational
                      transmuted figure x = 2z^2 / (1 + z^2)
     series           the partial sums, exact rationals, and their remainder
     figures          Emmy functions of one point, for media to compile

   Reuse searched: Emmy's D (derivatives of literal functions), simplify and
   definite-integral; alexandria.vocab for the claims (`differential`, `area`)
   and alexandria.notation for the two notations. Nothing here re-derives a
   derivative or a TeX printer."
  (:require [alexandria.formula :as formula]
            [alexandria.grade :as grade]
            [alexandria.notation :as notation]
            [alexandria.proofs :as proofs]
            [alexandria.vocab :as vocab]
            [desargues.board.construction :as c]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

(defn- variant
  "The grade keyword of a Result of alexandria.grade/grade."
  [res]
  (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [diffs] (variant (grade/grade :symbolic diffs)))

;; ---------------------------------------------------------------------------
;; Nova Methodus: the rules, one value in two notations

(def ^:private acta "Leibniz, Nova Methodus pro Maximis et Minimis, Acta Eruditorum, Oct. 1684, pp. 467-473")

(def rules
  "The rules of the calculus as frozen formulas. :env carries the constants
   (Leibniz's quantitas data constans) of a rule."
  [{:name 'constant
    :expr '(= (differential (* a x)) (* a dx))
    :env {:constants '[a]}
    :bindings {}
    :passage {:author :leibniz :work :nova-methodus :locus "p. 467"
              :quote "Sit a quantitas data constans, erit da aequalis 0, & d ax erit aequ. a dx"}}
   {:name 'addition
    :expr '(= (differential (+ (- z y) w x)) (+ (- dz dy) dw dx))
    :env {}
    :bindings {}
    :passage {:author :leibniz :work :nova-methodus :locus "p. 467"
              :quote "Jam Additio & Subtractio: si sit z - y + w + x aequ. v, erit d z - y + w + x seu dv, aequ. dz - dy + dw + dx."}}
   {:name 'product
    :expr '(= (differential (* x v)) (+ (* x dv) (* v dx)))
    :env {}
    :bindings {}
    :passage {:author :leibniz :work :nova-methodus :locus "p. 467"
              :quote "Multiplicatio, d xv aequ. x dv + v dx, seu posito y aequ. xv, fiet dy aequ. x dv + v dx."}}
   {:name 'quotient
    :expr '(= (differential (/ v y)) (/ (- (* y dv) (* v dy)) (* y y)))
    :env {}
    :bindings {}
    :passage {:author :leibniz :work :nova-methodus :locus "p. 468"
              :quote "Porro Divisio, d v/y vel (posito z aequ. v/y) dz aequ. (±v dy ∓ y dv)/yy."}}
   {:name 'power
    :expr '(= (differential (expt x a)) (* a (expt x (- a 1)) dx))
    :env {:constants '[a]}
    :bindings {}
    :passage {:author :leibniz :work :nova-methodus :locus "p. 469"
              :quote "Potentia d x^a = a x^(a-1) dx, exempli gratia d x^3 = 3 x^2 dx"}}])

(defn rule [nm] (first (filter #(= nm (:name %)) rules)))

(defn frozen
  "The rule as a frozen formula (alexandria.formula), its claim the value."
  [{:keys [name expr bindings passage]}]
  (formula/freeze {:name name :expr expr :bindings bindings :passage passage}))

(defn rule-grade
  "The grade of a rule: its claim checked as a vocabulary claim
   (alexandria.vocab realizes `differential` through Emmy's D)."
  [{:keys [expr env]}]
  (proved [(c/check {:claim expr} (or env {}))]))

(defn renderings
  "One frozen formula in each notation: {:tex :leibniz :newton}."
  [r]
  (let [f (frozen r)]
    (into {} (map (fn [n] [n (formula/render f n)])) [:tex :leibniz :newton])))

;; ---------------------------------------------------------------------------
;; The characteristic triangle and the transmutation

(defn tangent-intercept
  "z of curve y at x: where the tangent at (x, y(x)) meets the y-axis,
   z = y - x dy/dx."
  [y]
  (fn [x] (e/- (y x) (e/* x ((e/D y) x)))))

(defn- cross [[ax ay] [bx by]] (e/- (e/* ax by) (e/* ay bx)))

(defn characteristic
  "The characteristic triangle of curve y at x with side dx = h: P, Q on the
   curve, R the corner (Q's abscissa, P's ordinate), T on the tangent at
   P above Q, and Z where the tangent at P meets the y-axis."
  [y x h]
  (let [x2 (e/+ x h)
        slope ((e/D y) x)]
    {:P [x (y x)] :Q [x2 (y x2)] :R [x2 (y x)]
     :T [x2 (e/+ (y x) (e/* slope h))]
     :Z [0 ((tangent-intercept y) x)]}))

(def identities
  "[label difference] pairs, each zero, for any curve y (an Emmy literal
   function): the transmutation theorem and its integrated form."
  (let [y (e/literal-function 'y)
        z (tangent-intercept y)
        sector (fn [x] (fn [h] (e/* 1/2 (cross [x (y x)] [(e/+ x h) (y (e/+ x h))]))))]
    [["the triangle OPQ is ultimately half of z dx: d(OPQ)/dx = z/2"
      (e/- ((e/D (sector 'x)) 0) (e/* -1/2 (z 'x)))]
     ["the tangent's triangle OPT has exactly half of z dx"
      (let [{:keys [P T]} (characteristic y 'x 'h)]
        (e/+ (e/* 1/2 (cross P T)) (e/* 1/2 (z 'x) 'h)))]
     ["transmutation: y dx = d(xy)/2 + z dx/2"
      (e/- (y 'x) (e/* 1/2 ((e/D (fn [x] (e/* x (y x)))) 'x)) (e/* 1/2 (z 'x)))]]))

;; Leibniz's circle: diameter 2 on the x-axis from the origin

(defn circle-y [x] (e/sqrt (e/- (e/* 2 x) (e/square x))))

(def circle-z (tangent-intercept circle-y))

(defn rational-x
  "The transmuted figure: the abscissa x as a rational function of z."
  [z]
  (e/divide (e/* 2 (e/square z)) (e/+ 1 (e/square z))))

(def circle-identities
  [["for the circle, z^2 = x/(2 - x)"
    (e/- (e/square (circle-z 'x)) (e/divide 'x (e/- 2 'x)))]
   ["so x = 2 z^2/(1 + z^2): a rational figure"
    (e/- 'x (rational-x (circle-z 'x)))]
   ["and z^2/(1 + z^2) = 1 - 1/(1 + z^2)"
    (e/- (e/divide (e/square 'z) (e/+ 1 (e/square 'z))) (e/- 1 (e/divide 1 (e/+ 1 (e/square 'z)))))]])

(defn geometric-remainder
  "1/(1 + z^2) minus its first n terms 1 - z^2 + z^4 - ... minus the
   remainder (-z^2)^n/(1 + z^2): zero for every n."
  [n]
  (let [q (e/- (e/square 'z))]
    (e/- (e/divide 1 (e/+ 1 (e/square 'z)))
         (e/+ (reduce e/+ 0 (map #(e/expt q %) (range n)))
              (e/divide (e/expt q n) (e/+ 1 (e/square 'z)))))))

;; ---------------------------------------------------------------------------
;; The series, in exact rationals

(defn partial-sum
  "1 - 1/3 + 1/5 - ... to n terms, an exact rational."
  [n]
  (reduce + 0 (map (fn [k] (/ (if (even? k) 1 -1) (inc (* 2 k)))) (range n))))

(defn remainder
  "pi/4 - S_n = (-1)^n times the area under z^(2n)/(1 + z^2) from 0 to 1:
   Emmy writes the integrand, raster's Gauss-Kronrod quadrature
   (alexandria.raster/integral) computes it."
  [n]
  (* (if (even? n) 1 -1)
     (:value (raster/integral (fn [z] (e// (e/expt z (* 2 n)) (e/+ 1 (e/square z)))) 0 1))))

(defn quarter-pi
  "pi/4 as the area under 1/(1 + z^2) from 0 to 1, Leibniz's own integral,
   by raster's quadrature."
  []
  (:value (raster/integral (fn [z] (e// 1 (e/+ 1 (e/square z)))) 0 1)))

(defn series-table
  "Rows {:n :sum :error :bound} for n in ns: the exact partial sum, its
   distance from pi/4 (pi/4 by raster's quadrature), and Leibniz's bound
   1/(2n + 1)."
  [ns]
  (let [q (quarter-pi)]
    (mapv (fn [n] (let [s (partial-sum n)]
                    {:n n :sum s :error (abs (- q (double s)))
                     :bound (/ 1 (inc (* 2 n)))}))
          ns)))

(defn graded
  "[{:label :grade}] of everything above. The series is graded numeric: pi
   has no closed form to compare an exact partial sum with, so its error is
   checked against raster's quadrature of the remainder and against
   Leibniz's bound 1/(2n + 1). Numeric rows carry :source :raster: every
   floating number in them came out of a raster kernel."
  []
  (concat
   (map (fn [r] {:label (str "d rule: " (name (:name r))) :grade (rule-grade r)}) rules)
   (map (fn [[label d]] {:label label :grade (proved [d])}) identities)
   (map (fn [[label d]] {:label label :grade (proved [d])}) circle-identities)
   [{:label "1/(1+z^2) = 1 - z^2 + z^4 - ... with its remainder, n = 1..8"
     :grade (proved (map geometric-remainder (range 1 9)))}
    {:label "pi/4 - S_n equals the remainder integral, n = 1..20 (both integrals by raster's quadrature)"
     :grade (let [q (quarter-pi)]
              (variant (grade/grade :numeric
                                    (map (fn [n] (- (- q (double (partial-sum n))) (remainder n)))
                                         (range 1 21))
                                    1e-8)))
     :source raster/source}
    {:label "raster's quadrature of 1/(1+z^2) over [0 1] is pi/4"
     :grade (variant (grade/grade :numeric [(- (quarter-pi) (/ Math/PI 4))] 1e-12))
     :source raster/source}
    {:label "|pi/4 - S_n| < 1/(2n+1), n = 1..200"
     :grade (if (every? (fn [{:keys [error bound]}] (< error bound)) (series-table (range 1 201)))
              :grade/numeric :grade/fails)
     :source raster/source}]))

;; ---------------------------------------------------------------------------
;; Figures (Emmy functions of one point)

(defn pick
  "The value at node k of vals, for a numeric k in 0 .. n-1: Lagrange's
   interpolation through the nodes, so a figure can choose a corner by a
   number and still be one polynomial expression Emmy compiles."
  [k vals]
  (let [n (count vals)]
    (reduce e/+ 0
            (for [j (range n)]
              (e/* (nth vals j)
                   (reduce e/* 1 (for [m (range n) :when (not= m j)]
                                   (e// (e/- k m) (- j m)))))))))

(defn triangle
  "The characteristic triangle of Leibniz's circle at abscissa x0 with side
   h, as one figure: state [a b] is the point x0 + a h, y(x0) + b dy, so
   [0 0] is P, [1 0] the corner R, [1 1] Q; the tangent at P is
   [u slope], its point above x0 + u h."
  [x0 h]
  (fn [[a b]]
    (let [y0 (circle-y x0)
          dy (e/- (circle-y (e/+ x0 h)) y0)]
      [(e/+ x0 (e/* a h)) (e/+ y0 (e/* b dy))])))

(defn tangent-line
  "The tangent of Leibniz's circle at x0: state [u] is its point of
   abscissa u, so [0] is Z on the y-axis."
  [x0]
  (fn [[u]]
    [u (e/+ (circle-y x0) (e/* ((e/D circle-y) x0) (e/- u x0)))]))

(defn transmute
  "Sector i of n (the triangle from the origin O to the chord P_i P_i+1 of
   the quarter arc from x = 0 to 1) carried at s = 0 .. 1 into its strip of
   the figure of z: from x_i to x_i+1 under height z(x_i), which has twice
   the triangle's area in the limit. State [i c], c the corner 0 .. 3: the
   triangle O, P_i, P_i+1, O and the strip foot, top, top, foot."
  [n s]
  (fn [[i c]]
    (let [x0 (e// i n)
          x1 (e// (e/+ i 1) n)
          ;; start half way into the arc so z stays finite (z -> oo at x = 0)
          u0 (e/+ 1/2 (e// x0 2))
          u1 (e/+ 1/2 (e// x1 2))
          z0 (circle-z u0)
          tri [[0 0] [u0 (circle-y u0)] [u1 (circle-y u1)] [0 0]]
          strip [[u0 0] [u0 z0] [u1 z0] [u1 0]]
          at (fn [k] (pick c (mapv #(nth % k) tri)))
          to (fn [k] (pick c (mapv #(nth % k) strip)))]
      [(e/+ (at 0) (e/* s (e/- (to 0) (at 0))))
       (e/+ (at 1) (e/* s (e/- (to 1) (at 1))))])))

(def figures
  "Every moving figure, by name: {:f figure :params initial-params :state initial-state}."
  {:triangle {:f triangle :params [0.4 0.3] :state [0 0]}
   :tangent {:f tangent-line :params [0.4] :state [0]}
   :transmute {:f transmute :params [8 0] :state [0 0]}})

;; ---------------------------------------------------------------------------
;; The propositions as data

(def proofs-resource "alexandria/leibniz/calculus.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
