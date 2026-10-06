(ns alexandria.lagrange.variations
  "The calculus of variations from Bernoulli's challenge (1696) to Lagrange's
   delta (1755), on Emmy.

   Reuse searched before writing: Emmy's emmy.mechanics.lagrange
   (Lagrange-equations, Gamma, L-harmonic, ->local) does the mechanics; the
   variation operator is the one of SICM p. 28 (Emmy's own test
   emmy.sicm.ch1-test/delta, private there, so its three lines are restated
   here as `delta`). Every number is raster's: the descent times are
   raster's Gauss-Kronrod quadrature (alexandria.raster/integral) of a time
   element Emmy writes, the tracks are sampled by raster kernels. This
   namespace only writes down the historical objects:

     descent-L     the integrand of the descent time, ds / sqrt(2 g y), as a
                   Lagrangian of the path (x(s), y(s)), y measured downward
     cycloid       the curve of quickest descent, a(t - sin t), a(1 - cos t)
     race          the bead on the line, Galileo's circle arc and the cycloid
     tautochrone   the same cycloid in its arc length s: a harmonic oscillator
     delta         Lagrange's variation of a functional along eta
     graded        every claim, graded by alexandria.grade"
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [alexandria.raster :as raster]
            [emmy.abstract.function :as af]
            [emmy.env :as e]
            [emmy.mechanics.lagrange :as L]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The brachistochrone, 1696

(defn descent-L
  "The time element ds / v of a bead falling from rest, v = sqrt(2 g y),
   as a Lagrangian of the local tuple (t, (x y), (x' y')) of any
   parametrisation of the path; y is the depth below the start."
  [g]
  (fn [[_ [_ y] [xd yd]]]
    (e// (e/sqrt (e/+ (e/square xd) (e/square yd)))
         (e/sqrt (e/* 2 g y)))))

(defn cycloid
  "The cycloid of a rolling circle of radius a, cusp at the start, y downward:
   theta -> (a(theta - sin theta), a(1 - cos theta))."
  [a]
  (fn [theta]
    (e/up (e/* a (e/- theta (e/sin theta)))
          (e/* a (e/- 1 (e/cos theta))))))

(defn cycloid-residual
  "The Euler-Lagrange expression of the descent time along the cycloid."
  []
  (((L/Lagrange-equations (descent-L 'g)) (cycloid 'a)) 'theta))

(defn cycloid-time-element
  "(dt/dtheta)^2 - a/g along the cycloid: zero, so the bead's time is
   proportional to the turning angle and the descent to the bottom, theta =
   pi, takes pi sqrt(a/g)."
  []
  (let [path (cycloid 'a)]
    (e/- (e/square ((descent-L 'g) (L/->local 'theta (path 'theta) ((e/D path) 'theta))))
         (e// 'a 'g))))

(def race-params
  "Bernoulli's two points: A at the origin, B = (pi a, 2 a) below it, a = 1 m,
   g = 9.81 m/s^2. The cycloid through A and B has its lowest point at B."
  {:a 1.0 :g 9.81})

(defn- arc-centre
  "Galileo's circle arc from A to B, tangent to the vertical at A: centre on
   the horizontal through A at distance c with c^2 = (bx - c)^2 + by^2."
  [bx by]
  (/ (+ (* bx bx) (* by by)) (* 2 bx)))

(defn curves
  "The three tracks from A to B, each an Emmy function of a parameter u in
   [0 1] to (up x depth)."
  ([] (curves race-params))
  ([{:keys [a]}]
   (let [bx (* a Math/PI) by (* 2 a)
         c (arc-centre bx by)
         t-b (e/atan by (- c bx))]
     {:line (fn [u] (e/up (e/* u bx) (e/* u by)))
      :arc (fn [u] (let [t (e/* u t-b)] (e/up (e/- c (e/* c (e/cos t))) (e/* c (e/sin t)))))
      :cycloid (fn [u] (let [th (e/* u Math/PI)] (e/up (e/* a (e/- th (e/sin th))) (e/* a (e/- 1 (e/cos th))))))})))

(defn time-element
  "dt/dw along the track f, written by Emmy: the descent Lagrangian
   ds / sqrt(2 g y) at u = w^2, times du/dw = 2w. The substitution takes the
   1/sqrt(u) singularity at the start out of the integrand, so raster's
   quadrature sees a smooth function."
  [g f]
  (fn [w] (let [u (e/square w)]
            (e/* 2 w ((descent-L g) (L/->local u (f u) ((e/D f) u)))))))

(defn- time-to
  "Descent time along curve f from u = 0 to u = u1: the integral of the
   time element over w in [0, sqrt u1], by raster's Gauss-Kronrod
   (alexandria.raster/integral)."
  [g f u1]
  (if (zero? u1)
    0.0
    (:value (raster/integral (time-element g f) 0 (Math/sqrt (double u1))))))

(defn descent-times
  "{:line :arc :cycloid} seconds from A to B, each by raster's quadrature."
  ([] (descent-times race-params))
  ([{:keys [g] :as ps}]
   (update-vals (curves ps) (fn [f] (time-to g f 1.0)))))

(defn race
  "Each bead's track as samples [t x depth], u from 0 to 1 in n steps: the
   times by raster's quadrature, the points by the track's raster kernels."
  ([] (race race-params 48))
  ([{:keys [g] :as ps} n]
   (update-vals (curves ps)
                (fn [f] (let [us (mapv #(/ % n) (range (inc n)))
                              pts (raster/sample (fn [u] (vec (f u))) (mapv vector us))]
                          (mapv (fn [u [x y]] [(time-to g f u) x y]) us pts))))))
;; ---------------------------------------------------------------------------
;; The tautochrone (Huygens 1673) as the same cycloid

(defn bowl
  "The cycloid of the brachistochrone turned over, as a bowl with its lowest
   point at u = pi/2: u -> (2a(u - sin u cos u), 2a sin^2 u), y the height
   above the cusp line; the arc length from the lowest point is
   s = -4 a cos u."
  [a]
  (fn [u] (e/up (e/* 2 a (e/- u (e/* (e/sin u) (e/cos u))))
                (e/* 2 a (e/square (e/sin u))))))

(defn tautochrone-identities
  "[label difference] pairs, each zero: in the arc length s the bowl's height
   above its lowest point is s^2 / 8a, and s is a true arc length."
  []
  (let [a 'a u 'u
        s (fn [u] (e/* -4 a (e/cos u)))
        h (fn [u] (e/- (e/* 2 a) (nth ((bowl a) u) 1)))
        v ((e/D (bowl a)) u)]
    [["s measures arc length: |dP/du|^2 = (ds/du)^2"
      (e/- (e/+ (e/square (nth v 0)) (e/square (nth v 1))) (e/square ((e/D s) u)))]
     ["height above the bottom = s^2 / 8a"
      (e/- (h u) (e// (e/square (s u)) (e/* 8 a)))]]))

(defn L-bowl
  "The bead in the bowl with coordinate s: T = m s'^2 / 2, V = m g s^2 / 8a."
  [m g a]
  (fn [[_ s sd]]
    (e/- (e/* 1/2 m (e/square sd)) (e// (e/* m g (e/square s)) (e/* 8 a)))))

(defn tautochrone-pattern
  "The pattern, not a new derivation: the bowl's Lagrangian IS Emmy's
   harmonic oscillator with k = m g / 4a, so its Lagrange equations agree on
   every path and the period 2 pi sqrt(4a/g) does not depend on where the
   bead starts."
  []
  (e/with-literal-functions [s]
    (e/- (((L/Lagrange-equations (L-bowl 'm 'g 'a)) s) 't)
         (((L/Lagrange-equations (L/L-harmonic 'm (e// (e/* 'm 'g) (e/* 4 'a)))) s) 't))))

(defn tautochrone-period
  "The period of the bowl, sqrt(4a/g) 2 pi; the descent from any height to
   the bottom is a quarter of it."
  [a g]
  (* 2 Math/PI (Math/sqrt (/ (* 4 a) g))))

;; ---------------------------------------------------------------------------
;; Lagrange's delta, 1755

(defn delta
  "The variation along eta of a functional F of paths: d/de F[q + e eta] at
   e = 0 (Lagrange's delta; SICM p. 28)."
  [eta]
  (fn [F] (fn [q] (let [g (fn [eps] (F (e/+ q (e/* eps eta))))] ((e/D g) 0)))))

(def literal-L
  "A Lagrangian about which nothing is known: L(t, q, q')."
  (af/literal-function 'L '(-> (UP Real Real Real) Real)))

(defn first-variation
  "delta_eta of the integrand L o Gamma[q] minus Lagrange's integration by
   parts: d/dt (eta dL/dq') - eta (d/dt dL/dq' - dL/dq). Zero for every L, q,
   eta; integrated between ends where eta vanishes, the boundary term drops
   and delta S = - integral of eta times the Euler-Lagrange expression."
  []
  (e/with-literal-functions [q eta]
    (let [density (fn [q] (e/compose literal-L (L/Gamma q)))
          lhs (((delta eta) density) q)
          E ((L/Lagrange-equations literal-L) q)
          rhs (e/- (e/D (e/* eta (e/compose ((e/partial 2) literal-L) (L/Gamma q))))
                   (e/* eta E))]
      ((e/- lhs rhs) 't))))

(defn euler-equation
  "The equation the delta method gives, for the literal L: Emmy's
   Lagrange-equations, d/dt dL/dq' - dL/dq. Euler's N - dP/dx = 0 up to sign."
  []
  (e/with-literal-functions [q]
    (e/simplify (((L/Lagrange-equations literal-L) q) 't))))

(defn varied-path
  "The path the scene varies: q(t) = sin(pi t), eta(t) = sin(2 pi t) sin(pi t)
   vanishing at both ends, at size eps."
  [eps]
  (fn [t] (e/+ (e/sin (e/* Math/PI t))
               (e/* eps (e/sin (e/* 2 Math/PI t)) (e/sin (e/* Math/PI t))))))

;; ---------------------------------------------------------------------------
;; Moving figures

(defn cycloid-figure
  "Figure for media: the cycloid of radius a drawn y-up below its cusp,
   state [theta]."
  [a]
  (fn [[theta]]
    [(e/* a (e/- theta (e/sin theta))) (e/- (e/* a (e/- 1 (e/cos theta))))]))

(defn rolling-figure
  "Figure for media: the circle of radius 1 that traces the cycloid, rolled
   through angle s under the line y = 0; state [phi] runs over the circle."
  [s]
  (fn [[phi]]
    [(e/+ s (e/cos phi)) (e/- (e/sin phi) 1)]))

(defn bowl-figure
  "Figure for media: a bead in the tautochrone bowl of radius r (the bowl
   function above with a = r) after phase tau of the harmonic motion,
   started at rest from u0; params [r tau], state [u0]. In the arc length
   s = -4 r cos u the motion is s0 cos tau, so cos u = cos u0 cos tau; with
   tau = 0 the states draw the bowl itself.

   Written for raster's polynomial libm: sin^2 u = sin^2 u0 + cos^2 u0
   sin^2 tau (a sum of squares, never negative, where 1 - cos^2 u would dip
   below 0 at the bottom and its sqrt be NaN), and u = atan2(sin u, cos u),
   not acos. Compile it with {:simplify? false} so Emmy does not fold the
   sum back into 1 - cos^2 u."
  [r tau]
  (fn [[u0]]
    (let [c (e/* (e/cos u0) (e/cos tau))
          s2 (e/+ (e/square (e/sin u0)) (e/* (e/square (e/cos u0)) (e/square (e/sin tau))))
          s (e/sqrt s2)
          u (e/atan s c)]
      [(e/* 2 r (e/- u (e/* s c))) (e/* 2 r s2)])))

(defn varied-figure
  "Figure for media: the varied path of `varied-path` drawn over [0 3],
   params [eps], state [t], t in [0 1]."
  [eps]
  (fn [[t]] [(e/* 3 t) ((varied-path eps) t)]))

(def figures
  "Every moving figure, by name: {:f figure :params initial-params :state initial-state}."
  {:cycloid {:f cycloid-figure :params [1] :state [0]}
   :wheel {:f rolling-figure :params [0] :state [0]}
   :bowl {:f bowl-figure :params [0.5 0] :state [1] :opts {:simplify? false}}
   :varied {:f varied-figure :params [0] :state [0]}})

;; ---------------------------------------------------------------------------
;; Graded

(defn- components [x]
  (let [v (e/simplify x)] (if (e/structure? v) (flatten (seq v)) [v])))

(defn- g [kind diffs]
  (let [res (grade/grade kind (mapcat components diffs))]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn graded
  "[{:id :label :grade}] for the brachistochrone, the tautochrone and the
   delta; the numeric grades carry :engine :raster."
  []
  (let [{:keys [a] grav :g :as ps} race-params
        {:keys [line arc cycloid]} (descent-times ps)
        closed (* Math/PI (Math/sqrt (/ a grav)))]
    (vec
     (concat
      [{:id :brachistochrone :label "the cycloid satisfies the Euler-Lagrange equation of the descent time"
        :grade (g :symbolic [(cycloid-residual)])}
       {:id :brachistochrone :label "along the cycloid dt = sqrt(a/g) dtheta"
        :grade (g :symbolic [(cycloid-time-element)])}
       {:id :brachistochrone :label "cycloid time by raster's quadrature = pi sqrt(a/g)"
        :grade (g :numeric [(- cycloid closed)]) :engine :raster}
       {:id :brachistochrone :label "cycloid < circle arc < straight line (raster's quadrature)"
        :grade (if (< cycloid arc line) :grade/numeric :grade/fails) :engine :raster}]
      (map (fn [[label d]] {:id :tautochrone :label label :grade (g :symbolic [d])})
           (tautochrone-identities))
      [{:id :tautochrone :label "the bowl's Lagrange equation is the harmonic oscillator's, k = mg/4a"
        :grade (g :symbolic [(tautochrone-pattern)])}
       {:id :delta :label "delta of L o Gamma[q] = d/dt(eta dL/dq') - eta E[L]"
        :grade (g :symbolic [(first-variation)])}]))))

;; ---------------------------------------------------------------------------
;; The proofs as data

(def brachistochrone-resource "alexandria/bernoulli/brachistochrone.proofs.edn")
(def delta-resource "alexandria/lagrange/variations.proofs.edn")

(defn brachistochrone-proof [] (proofs/read-proofs brachistochrone-resource))
(defn delta-proof [] (proofs/read-proofs delta-resource))
