(ns alexandria.bernoulli.brachistochrone
  "Johann Bernoulli's brachistochrone (Acta Eruditorum, June 1696) for any
   two points: the variational problem, its first integral (Beltrami's
   identity), the cycloid through A and B, and the race of the cycloid, the
   straight line and Galileo's circle arc.

   A is the origin, B = (X, Y) with Y the depth below A (y downward), g the
   acceleration of gravity. A bead starting from rest at A has speed
   sqrt(2 g y), so the time along y(x) is

     T[y] = integral sqrt((1 + y'^2) / (2 g y)) dx.

   The integrand does not contain x, so Emmy's Lagrange equation has the
   first integral H = y' dL/dy' - L (Beltrami); Emmy simplifies H to
   -1 / sqrt(2 g y (1 + y'^2)), so y (1 + y'^2) = 2 r is constant, and the
   cycloid x = r (theta - sin theta), y = r (1 - cos theta) satisfies it.

   Reuse: alexandria.lagrange.variations (the descent Lagrangian along a
   parametrised path, the cycloid's Euler-Lagrange residual, Bernoulli's
   quote) for B = (pi a, 2a); this namespace generalises B. Numbers are
   raster's: the end angle theta by Brent (alexandria.raster/root), the
   times by Gauss-Kronrod (alexandria.raster/integral); the closed forms the
   widget compiles are graded against them."
  (:require [alexandria.grade :as grade]
            [alexandria.lagrange.variations :as variations]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [emmy.mechanics.lagrange :as L]
            [hive-dsl.result :as r]))

(def g
  "m/s^2, exact (981/100): a float here makes Emmy's simplifier fail on
   quotients (gcd of non-integers) inside the tie's kernels."
  981/100)

;; ---------------------------------------------------------------------------
;; The variational problem and its first integral

(defn L-time
  "The time element of y(x), as a Lagrangian of the local tuple
   (x, y, y'): sqrt((1 + y'^2) / (2 g y))."
  [g]
  (fn [[_ y yp]] (e/sqrt (e// (e/+ 1 (e/square yp)) (e/* 2 g y)))))

(defn beltrami-H
  "Beltrami's first integral H = y' dL/dy' - L, as a function of the local
   tuple."
  [Lf]
  (fn [local]
    (e/- (e/* (nth local 2) (((e/partial 2) Lf) local)) (Lf local))))

(defn beltrami-identity
  "d/dx H(Gamma[y]) - y' E[y](x) for a literal y and a literal Lagrangian
   with no x in it: zero, so H is constant on every solution."
  []
  (let [Lf (fn [[_ y yp]] ((e/literal-function 'F '(-> (X Real Real) Real)) y yp))]
    (e/with-literal-functions [y]
      (let [H (e/compose (beltrami-H Lf) (L/Gamma y))
            E ((L/Lagrange-equations Lf) y)]
        ((e/- (e/D H) (e/* (e/D y) E)) 'x)))))

(defn H-closed
  "H for the time element, minus its closed form -1/sqrt(2 g y (1 + y'^2)):
   zero."
  []
  (let [loc (e/up 'x 'y 'yp)]
    (e/- ((beltrami-H (L-time 'g)) loc)
         (e/- (e/invert (e/sqrt (e/* 2 'g 'y (e/+ 1 (e/square 'yp)))))))))

(defn cycloid-first-integral
  "Along x = r(theta - sin theta), y = r(1 - cos theta): y (1 + y'^2) - 2r,
   with y' = (dy/dtheta) / (dx/dtheta). Zero: the cycloid keeps H constant."
  []
  (let [x (fn [th] (e/* 'r (e/- th (e/sin th))))
        y (fn [th] (e/* 'r (e/- 1 (e/cos th))))
        yp (e// ((e/D y) 'theta) ((e/D x) 'theta))]
    (e/- (e/* (y 'theta) (e/+ 1 (e/square yp))) (e/* 2 'r))))

(defn equations
  "The derivation for the page: [{:label :eqs}] of Emmy expressions."
  []
  (let [loc (e/up 'x 'y 'yp)]
    [{:label "the time along y(x): T = ∫ L dx, with L ="
      :eqs [((L-time 'g) loc)]}
     {:label "Beltrami: L has no x, so H = y′ ∂L/∂y′ − L is constant on a solution; Emmy simplifies H to"
      :eqs [((beltrami-H (L-time 'g)) loc)]}
     {:label "the cycloid x = r(θ − sin θ), y = r(1 − cos θ): y (1 + y′²) =  (constant 2r)"
      :eqs [(e/+ (cycloid-first-integral) (e/* 2 'r))]}]))

;; ---------------------------------------------------------------------------
;; The cycloid through A and B: forward, inverse

(defn cycloid-point
  "B on the cycloid of radius r at end angle theta: [X Y]."
  [r th]
  [(e/* r (e/- th (e/sin th))) (e/* r (e/- 1 (e/cos th)))])

(defn descent-time
  "The time from A to the cycloid's angle theta: theta sqrt(r/g)."
  [r th]
  (e/* th (e/sqrt (e// r g))))

(defn end-residual
  "Y (theta - sin theta) - X (1 - cos theta): zero at the end angle of the
   cycloid through (X, Y); negative near 0, positive at 2 pi."
  [X Y th]
  (e/- (e/* Y (e/- th (e/sin th))) (e/* X (e/- 1 (e/cos th)))))

(defn radius-of
  "r from Y and theta."
  [Y th]
  (e// Y (e/- 1 (e/cos th))))

(defn through
  "The cycloid through A and B = (X, Y), by raster: theta by Brent on
   end-residual over (1e-6, 2 pi - 1e-6), then r = Y / (1 - cos theta).
   {:theta :r :T :residual :source :raster}."
  [X Y]
  (let [{th :value res :residual} (raster/root (fn [t] (end-residual X Y t)) 1e-6 (- (* 2 Math/PI) 1e-6))
        rr (/ Y (- 1 (Math/cos th)))]
    {:theta th :r rr :T (* th (Math/sqrt (/ rr g))) :residual res :source :raster}))

;; ---------------------------------------------------------------------------
;; The race: line, circle arc, cycloid, any B

(defn line-time
  "The straight chord from A to (X, Y): sqrt(2 (X^2 + Y^2) / (g Y))."
  [X Y]
  (e/sqrt (e// (e/* 2 (e/+ (e/square X) (e/square Y))) (e/* g Y))))

(defn arc-centre
  "Galileo's circle arc through A tangent to the vertical there: centre
   (c, 0), c = (X^2 + Y^2) / 2X."
  [X Y]
  (e// (e/+ (e/square X) (e/square Y)) (e/* 2 X)))

(defn arc-angle
  "The angle at the centre from A to B: atan2(Y, c - X)."
  [X Y]
  (e/atan Y (e/- (arc-centre X Y) X)))

(defn arc-point
  "The arc at angle t: (c - c cos t, c sin t)."
  [c t]
  [(e/- c (e/* c (e/cos t))) (e/* c (e/sin t))])

(defn arc-integrand
  "dT/dw on the arc of centre c with t = w^2: 2 w sqrt(c / (2 g sin w^2)),
   smooth at w = 0 (the substitution of alexandria.lagrange.variations)."
  [c]
  (fn [w] (e/* 2 w (e/sqrt (e// c (e/* 2 g (e/sin (e/square w))))))))

(def ^:private gl-nodes
  "Gauss-Legendre, 16 nodes on [-1 1] (Golub-Welsch, double precision)."
  (let [n 16
        legendre (fn [x] (loop [k 1 p0 1.0 p1 x]
                           (if (= k n) [p1 p0]
                               (recur (inc k) p1 (/ (- (* (+ (* 2 k) 1) x p1) (* k p0)) (inc k))))))]
    (vec (for [i (range 1 (inc n))]
           (loop [x (Math/cos (/ (* Math/PI (- i 0.25)) (+ n 0.5))) it 0]
             (let [[pn pn1] (legendre x)
                   dp (/ (* n (- (* x pn) pn1)) (- (* x x) 1))
                   x' (- x (/ pn dp))]
               (if (< it 50) (recur x' (inc it))
                   [x' (/ 2 (* (- 1 (* x' x')) dp dp))])))))))

(defn arc-time-to
  "The time on the arc of centre c from A to the angle tu. On the arc the
   depth is c sin t and ds = c dt, so T = sqrt(c / 2g) F(tu) with
   F(tu) = integral dt / sqrt(sin t) over [0, tu]; with t = w^2 the
   integrand 2w / sqrt(sin w^2) is smooth, and 16-point Gauss-Legendre on
   [0, sqrt tu] gives F. An Emmy expression the widget compiles into a
   raster kernel (simplify off); graded against raster's quadgk."
  [c tu]
  (let [b (e/sqrt tu)
        F (apply e/+ (for [[x wt] gl-nodes]
                       (let [w (e/* 0.5 b (e/+ x 1))]
                         (e/* wt b (e// w (e/sqrt (e/sin (e/square w))))))))]
    (e/* (e/sqrt (e// c (e/* 2 g))) F)))

(defn arc-time
  "Galileo's arc from A to (X, Y), the Gauss-Legendre expression (what the
   widget's kernel computes)."
  [X Y]
  (arc-time-to (arc-centre X Y) (arc-angle X Y)))

(defn arc-time-gl
  "The same Gauss-Legendre sum on the JVM by raster: the integrand's kernel
   sampled at the 16 nodes (one small kernel instead of the whole sum)."
  [X Y]
  (let [c (raster/value arc-centre X Y)
        tb (raster/value arc-angle X Y)
        b (Math/sqrt tb)
        ws (mapv (fn [[x _]] [(* 0.5 b (+ x 1))]) gl-nodes)
        fs (raster/sample (fn [w] (e// w (e/sqrt (e/sin (e/square w))))) ws)]
    (* (Math/sqrt (/ c (* 2 g)))
       (reduce + (map (fn [[_ wt] f] (* wt b (double (if (sequential? f) (first f) f)))) gl-nodes fs)))))

(defn arc-time-quadgk
  "The same time by raster's adaptive Gauss-Kronrod."
  [X Y]
  (let [c (arc-centre X Y) tb (arc-angle X Y)]
    (:value (raster/integral (arc-integrand c) 0 (Math/sqrt tb)))))

(defn times
  "{:cycloid :arc :line} seconds from A to (X, Y), by raster (Brent for
   theta, quadgk for the arc)."
  [X Y]
  (let [{:keys [T]} (through X Y)]
    {:cycloid T
     :arc (arc-time-quadgk X Y)
     :line (raster/value line-time X Y)}))

(defn finishing-order
  "The tracks sorted by their raster time."
  [ts]
  (mapv key (sort-by val ts)))

(defn samples
  "Each track from A to B as n + 1 points [x depth], by raster kernels:
   {:cycloid :arc :line}."
  [X Y n]
  (let [{th :theta rr :r} (through X Y)
        c (raster/value arc-centre X Y)
        tb (raster/value arc-angle X Y)
        us (mapv #(vector (/ (double %) n)) (range (inc n)))]
    {:cycloid (mapv vec (raster/sample (fn [u] (cycloid-point rr (e/* u th))) us))
     :arc (mapv vec (raster/sample (fn [u] (arc-point c (e/* u tb))) us))
     :line (mapv vec (raster/sample (fn [u] [(e/* u X) (e/* u Y)]) us))}))

;; ---------------------------------------------------------------------------
;; Graded

(defn- components [x]
  (let [v (e/simplify x)] (if (e/structure? v) (flatten (seq v)) [v])))

(defn- gr [kind diffs]
  (let [res (grade/grade kind (mapcat components diffs))]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(def example "B for the page's race: 2 m across, 1 m down." [2.0 1.0])

(defn cycloid-time-quadgk
  "The cycloid's time by raster's quadgk of ds / sqrt(2 g y) along it, with
   theta = th w^2 (the substitution of alexandria.lagrange.variations):
   |P'(theta)| = r sqrt((1 - cos)^2 + sin^2), y = r (1 - cos)."
  [rr th]
  (:value (raster/integral
           (fn [w] (let [t (e/* th (e/square w))
                         speed (e/* rr (e/sqrt (e/+ (e/square (e/- 1 (e/cos t))) (e/square (e/sin t)))))]
                     (e/* 2 th w (e// speed (e/sqrt (e/* 2 g rr (e/- 1 (e/cos t))))))))
           0 1)))

(defn graded
  "[{:id :label :grade}] for the derivation, the cycloid through two
   points, the tie's inverses and the race; numeric grades by raster."
  ([] (graded example))
  ([[X Y]]
   (let [{th :theta rr :r T :T res :residual} (through X Y)
         [X' Y'] (raster/value (fn [r t] (cycloid-point r t)) rr th)
         ts (times X Y)
         gl (arc-time-gl X Y)
         cyc-quad (cycloid-time-quadgk rr th)]
     [{:id :beltrami :label "d/dx H = y′·E[y] for every L(y, y′): H is constant on a solution (Beltrami)"
       :grade (gr :symbolic [(beltrami-identity)])}
      {:id :beltrami :label "H = −1/√(2gy(1 + y′²)) for the time element"
       :grade (gr :symbolic [(H-closed)])}
      {:id :beltrami :label "along the cycloid y(1 + y′²) = 2r: H is constant"
       :grade (gr :symbolic [(cycloid-first-integral)])}
      {:id :beltrami :label "the cycloid satisfies the Euler–Lagrange equation of the descent time"
       :grade (gr :symbolic [(variations/cycloid-residual)])}
      {:id :tie :label "r ↦ Y(r, θ) ↦ Y/(1 − cos θ) = r (inverse of the radius)"
       :grade (gr :symbolic [(e/- (radius-of (second (cycloid-point 'r 'theta)) 'theta) 'r)])}
      {:id :tie :label "r ↦ T = θ√(r/g) ↦ g T²/θ² = r (inverse of the time)"
       :grade (gr :symbolic [(e/- (e/* g (e/square (e// (descent-time 'r 'theta) 'theta))) 'r)])}
      {:id :through :label (format "Brent: θ = %.6f, r = %.6f m through B = (%.1f, %.1f)" th rr X Y)
       :grade (gr :numeric [res (- X' X) (- Y' Y)]) :engine :raster}
      {:id :race :label (format "cycloid time θ√(r/g) = quadgk of the time element along it (%.6f s)" T)
       :grade (gr :numeric [(- T cyc-quad)]) :engine :raster}
      {:id :race :label "the arc's 16-point Gauss–Legendre sum (the widget's kernel) = raster's quadgk"
       :grade (gr :numeric [(- gl (:arc ts))]) :engine :raster}
      {:id :race :label (format "cycloid %.4f s < arc %.4f s < line %.4f s" (:cycloid ts) (:arc ts) (:line ts))
       :grade (if (= [:cycloid :arc :line] (finishing-order ts)) :grade/numeric :grade/fails) :engine :raster}])))
